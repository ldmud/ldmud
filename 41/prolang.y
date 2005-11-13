%{
%line
/* The above line is to give proper line number references. Please mail me
 * if your compiler complains about it.
 */
/*
 * This is the grammar definition of LPC. The token table is built
 * automatically by make_func. The lang.y is constructed from this file and
 * the generated token list. The reason of this is that there is no
 * #include-statment that yacc recognizes.
 */
#define LANG
#include "driver.h"

#include "my-alloca.h"
#include <stdio.h>
#ifdef __STDC__
#include <stdarg.h>
#endif

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
#include "stralloc.h"
#include "swap.h"
#include "switch.h"
#include "wiz_list.h"

#define YYMAXDEPTH        600

/* NUMPAREAS areas are saved with the program code after compilation.
 */
#define A_PROGRAM                0
#define A_STRINGS                1
#define A_VARIABLES                2
#define A_VIRTUAL_VAR                3
#define A_LINENUMBERS                4
#define A_INHERITS                5
#define A_ARGUMENT_TYPES        6
#define A_ARGUMENT_INDEX        7
#define NUMPAREAS                8
#define A_FUNCTIONS                8
%ifndef INITIALIZATION_BY___INIT
#define A_VARIABLE_VALUES        9
#define A_VIRTUAL_VAR_VALUES   10
%endif /* INITIALIZATION_BY___INIT */
#define A_STRING_NEXT               11
#define A_INCLUDE_NAMES        12
#define A_INHERIT_FLAG         13 
  /* TRUE if corresponding inherit is a duplicate due to virtual inheritance */
#define NUMAREAS               14

#define CURRENT_PROGRAM_SIZE (mem_block[A_PROGRAM].current_size)

#define BREAK_ON_STACK                0x4000000
#define BREAK_FROM_SWITCH        0x8000000
#define CASE_LABELS_ENABLED    0x10000000
#define BREAK_DELIMITER       -0x20000000

#define CONTINUE_ADDRESS_MASK   0x3ffff
#define SWITCH_DEPTH_UNIT        0x40000
#define SWITCH_DEPTH_MASK    0x3ffc0000
#define CONTINUE_DELIMITER  -0x40000000

struct const_list { struct const_list *next; struct svalue val; };
struct const_list_svalue {
    struct svalue head;
    struct const_list list;
};

struct efun_shadow {
    struct ident *shadow;
    struct efun_shadow *next;
};

/*
 * Information for allocating a block that can grow dynamically
 * using realloc. That means that no pointers should be kept into such
 * an area, as it might be moved.
 */

struct mem_block {
    char *block;
    mp_uint current_size;
    mp_uint max_size;
};

#define START_BLOCK_SIZE        2048

static struct mem_block mem_block[NUMAREAS];

/*
 * Some good macros to have.
 */

#define BASIC_TYPE(e,t) ((e) == TYPE_ANY ||\
                         (e) == (t) ||\
                         (t) == TYPE_ANY)

#define TYPE(e,t) (BASIC_TYPE((e) & TYPE_MOD_MASK, (t) & TYPE_MOD_MASK) ||\
                   (((e) & TYPE_MOD_POINTER) && ((t) & TYPE_MOD_POINTER) &&\
                    BASIC_TYPE((e) & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER),\
                               (t) & (TYPE_MOD_MASK & ~TYPE_MOD_POINTER))))

#define MASKED_TYPE(e,t) (BASIC_TYPE( (e) , (t) ) ||\
          ( (e) == (TYPE_MOD_POINTER|TYPE_ANY) && (t) & TYPE_MOD_POINTER ) ||\
          ( (t) == (TYPE_MOD_POINTER|TYPE_ANY) && (e) & TYPE_MOD_POINTER )    )

#define NON_VIRTUAL_OFFSET_TAG 0x4000
#define FUNCTION(n) ((struct function *)mem_block[A_FUNCTIONS].block + (n))
#define NV_VARIABLE(n) ((struct variable *)mem_block[A_VARIABLES].block + (n))
#define V_VARIABLE(n)  ((struct variable *)mem_block[A_VIRTUAL_VAR].block + \
                        (n) - VIRTUAL_VAR_TAG)
#define VARIABLE(n) ((n) & VIRTUAL_VAR_TAG ? V_VARIABLE(n) : NV_VARIABLE(n))

#define align(x) (((x) + (sizeof(char*)-1) ) & ~(sizeof(char*)-1) )

/*
 * If the type of the function is given, then strict types are
 * checked and required.
 */
static int exact_types;
int approved_object;                /* How I hate all these global variables */
int num_virtual_variables;

static int heart_beat;                /* Number of the heart beat function */

static p_int stored_bytes;        /* used by store_line_number_info to */
static p_int stored_lines;        /* keep track of the stored info     */
static p_uint last_include_start;
static p_int switch_pc; /* to ease relative addressing */
static p_int current_break_address;
static p_int current_continue_address;
static int current_type;

static p_uint last_expression;

static char *last_string_constant = 0;

static struct program NULL_program; /* marion - clean neat empty struct */

int yyparse PROT((void));
static char *get_two_types PROT((int type1, int type2));
static void add_local_name PROT((struct ident *, int));
static int verify_declared PROT((struct ident *));
%ifdef INITIALIZATION_BY___INIT
static void copy_variables PROT((struct program *, int));
static int copy_functions PROT((struct program *, int type));
%else
static void copy_variables PROT((struct program *, int, struct svalue *));
static void copy_functions PROT((struct program *, int type));
%endif
static void fix_function_inherit_indices PROT((struct program *));
static void fix_variable_index_offsets PROT((struct program *));
static void type_error PROT((char *, int));
static void argument_type_error PROT((int, int));

/*
 * The names and types of arguments and auto variables.
 */
static unsigned short type_of_locals[MAX_LOCAL];
static unsigned long full_type_of_locals[MAX_LOCAL];
static int current_number_of_locals = 0;
static int current_break_stack_need = 0  ,max_break_stack_need = 0;

/*
 * The types of arguments when calling functions must be saved,
 * to be used afterwards for checking. And because function calls
 * can be done as an argument to a function calls,
 * a stack of argument types is needed. This stack does not need to
 * be freed between compilations, but will be reused.
 */
static struct mem_block type_of_arguments;

struct program *compiled_prog;        /* Is returned to the caller of yyparse */
%ifndef INITIALIZATION_BY___INIT
struct svalue *prog_variable_values; /* this one too */
%endif /* INITIALIZATION_BY___INIT */

static struct ident *all_globals = 0;
static struct ident *all_locals = 0;
static struct efun_shadow *all_efun_shadows = 0;

#ifdef __STDC__
void yyerrorf(char *format, ...)
{
    va_list va;
    char buff[512];
    char fixed_fmt[200];

    format = limit_error_format(fixed_fmt, format);
    va_start(va, format);
    vsprintf(buff, format, va);
    va_end(va);
    yyerror(buff);
}
#else
/*VARARGS1*/
void yyerrorf(format, a1, a2, a3)
char *format;
int a1, a2, a3;
{
    char buff[512];
    char fixed_fmt[200];

    format = limit_error_format(fixed_fmt, format);
    sprintf(buff, format, a1, a2, a3);
    yyerror(buff);
}
#endif

/*
 * Compare two types, and return true if they are compatible.
 */
static int compatible_types(t1, t2)
    int t1, t2;
{
    if (t1 == TYPE_UNKNOWN || t2 == TYPE_UNKNOWN)
        return 0;
    if (t1 == t2)
        return 1;
    if (t1 == TYPE_ANY || t2 == TYPE_ANY)
        return 1;
    if ((t1 & TYPE_MOD_POINTER) && (t2 & TYPE_MOD_POINTER)) {
        if ((t1 & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER) ||
            (t2 & TYPE_MOD_MASK) == (TYPE_ANY|TYPE_MOD_POINTER))
            return 1;
    }
    return 0;
}

/*
 * Add another argument type to the argument type stack
 */
static INLINE void
add_arg_type(type)
    unsigned short type;
{
    struct mem_block *mbp = &type_of_arguments;
    if (mbp->current_size + sizeof type > mbp->max_size) {
        mbp->max_size <<= 1;
        mbp->block = rexalloc((char *)mbp->block, mbp->max_size);
    }
    *(short*)(mbp->block + mbp->current_size)  = type;
    mbp->current_size += sizeof type;
}

/*
 * Pop the argument type stack 'n' elements.
 */
static INLINE void
pop_arg_stack(n)
    int n;
{
    type_of_arguments.current_size -= sizeof (unsigned short) * n;
}

/*
 * Get type of argument number 'arg', where there are
 * 'n' arguments in total in this function call. Argument
 * 0 is the first argument.
 */
#if 0 /* not used */
static INLINE int
get_argument_type(arg, n)
    int arg, n;
{
    return
        ((unsigned short *)
         (type_of_arguments.block + type_of_arguments.current_size))[arg - n];
}
#endif

static INLINE unsigned short *
get_argument_types_start(n)
    int n;
{
    return
        &((unsigned short *)
         (type_of_arguments.block + type_of_arguments.current_size))[ - n];
}

static INLINE void
check_aggregate_types(n)
    int n;
{
    unsigned short *argp, mask;

    argp = (unsigned short *)
        (type_of_arguments.block +
          (type_of_arguments.current_size -= sizeof (unsigned short) * n) );
    for(mask = ~TYPE_MOD_REFERENCE; --n >= 0; ) {
        mask |= *argp++;
    }
    if (!(~mask & 0xffff))
        yyerror("Can't trace reference assignments.");
}

static char *realloc_mem_block(mbp, size)
    struct mem_block *mbp;
    mp_int size;
{
    mp_int max_size;
    char *p;

    max_size = mbp->max_size;
    do {
        max_size <<= 1;
    } while (size > max_size);
    p = rexalloc((char *)mbp->block, max_size);
    if (!p) {
        lex_close("Out of memory");
        return 0;
    }
    mbp->block = p;
    mbp->max_size = max_size;
    return p;
}

/* add_to_mem_block must not be called with length zero, because the length
 * is passed to memcpy .
 */
static INLINE void
add_to_mem_block(n, data, size)
    int n, size;
    char *data;
{
    struct mem_block *mbp = &mem_block[n];
    if (mbp->current_size + size > mbp->max_size) {
        if (!realloc_mem_block(mbp, mbp->current_size + size))
            return;
    }
    memcpy(mbp->block + mbp->current_size, data, size);
    mbp->current_size += size;
}

static char *realloc_a_program() {
    return realloc_mem_block(&mem_block[A_PROGRAM], 0);
}

#define byte_to_mem_block(n, b) ((void)(\
    (mem_block[n].current_size == mem_block[n].max_size ?\
      !!realloc_mem_block(&mem_block[n],0):1)?\
     (mem_block[n].block[mem_block[n].current_size++] = (b)) : 0))

#define ins_byte(b) byte_to_mem_block(A_PROGRAM, b)
#ifndef ins_byte
INLINE
static void ins_byte(b)
    char b;
{
    if (mem_block[A_PROGRAM].current_size == mem_block[A_PROGRAM].max_size ) {
        if (!realloc_a_program())
            return;
    }
    mem_block[A_PROGRAM].block[mem_block[A_PROGRAM].current_size++] = b;
}
#endif

/*
 * Store a 2 byte number. It is stored in such a way as to be sure
 * that correct byte order is used, regardless of machine architecture.
 * Also beware that some machines can't write a word to odd addresses.
 */
static void ins_short(l)
    short l;
{
    mp_uint current_size;
    char *dest;

    current_size = CURRENT_PROGRAM_SIZE;
    CURRENT_PROGRAM_SIZE = current_size + 2;
    if (current_size +1 >= mem_block[A_PROGRAM].max_size &&
        !realloc_a_program())
    {
        return;
    }
    dest = mem_block[A_PROGRAM].block + current_size;
    *dest++ = ((char *)&l)[0];
    *dest   = ((char *)&l)[1];
}

static void upd_short(offset, l)
    int offset;
    short l;
{
    char *dest;

    dest = mem_block[A_PROGRAM].block + offset;
    *dest++ = ((char *)&l)[0];
    *dest   = ((char *)&l)[1];
}

static short read_short(offset)
    int offset;
{
    short l[2];
    char *dest;

    dest = mem_block[A_PROGRAM].block + offset;
    ((char *)l)[0] = *dest++;
    ((char *)l)[1] = *dest;
    return l[0];
}

/*
 * Store a 4 byte number. It is stored in such a way as to be sure
 * that correct byte order is used, regardless of machine architecture.
 */
static void ins_long(l)
    int32 l;
{
    mp_uint current_size;
    char *dest;

    current_size = CURRENT_PROGRAM_SIZE;
    CURRENT_PROGRAM_SIZE = current_size + 4;
    if (current_size +3 >= mem_block[A_PROGRAM].max_size
        && !realloc_a_program())
    {
        return;
    }
    dest = mem_block[A_PROGRAM].block + current_size;
    *dest++ = ((char *)&l)[0];
    *dest++ = ((char *)&l)[1];
    *dest++ = ((char *)&l)[2];
    *dest   = ((char *)&l)[3];
}

#define ins_f_byte(b) (ins_byte((char)((b) - F_OFFSET)))
#ifndef ins_f_byte
static void ins_f_byte(b)
    unsigned int b;
{
    ins_byte((char)(b - F_OFFSET));
}
#endif

static void _ins_f_code PROT((unsigned int b));
#define ins_f_code(b) (_ins_f_code((b) - F_OFFSET))

#define PREPARE_INSERT(n) \
    char *__PREPARE_INSERT__p = (\
      (\
        CURRENT_PROGRAM_SIZE+(n) > mem_block[A_PROGRAM].max_size ?\
          realloc_a_program()\
        :\
          0\
      ),\
      mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE);

#define PREPARE_S_INSERT(n) \
    short __ADD_SHORT__s[2];\
    PREPARE_INSERT(n)

#define add_byte(b) (void)(*__PREPARE_INSERT__p++ = (b))

#define add_short(s) \
{\
    char *__ADD_SHORT__p = (char *)__ADD_SHORT__s;\
    *((short*)__ADD_SHORT__p) = (s);\
    *__PREPARE_INSERT__p++ = __ADD_SHORT__p[0];\
    *__PREPARE_INSERT__p++ = __ADD_SHORT__p[1];\
}

#define add_f_byte(b) (add_byte((char)((b) - F_OFFSET)))

#define defined_function(s) \
    ((s)->type == I_TYPE_GLOBAL ? (s)->u.global.function : -1)
#if 0
/*
 * Return the index of the function found, otherwise -1.
 */
static int defined_function(s)
    char *s;
{
    int offset;
    struct function *funp;

    for (offset = 0; offset < mem_block[A_FUNCTIONS].current_size;
         offset += sizeof (struct function)) {
        funp = (struct function *)&mem_block[A_FUNCTIONS].block[offset];
        if (funp->flags & NAME_HIDDEN)
            continue;
        if (strcmp(funp->name, s) == 0)
            return offset / sizeof (struct function);
    }
    return -1;
}
#endif

/* special allocate/free subroutines to be able to free intermediate results
 * that were thrown away due to an error.
 */
static char *last_yalloced = 0;

static char *yalloc(size)
unsigned long size;
{
    char **p;

    p = (char **)xalloc(size+sizeof(char*));
    if (!p) return 0;
    *p++ = last_yalloced;
    last_yalloced = (char *)p;
    return (char *)p;
}

#if 1 || defined(DEBUG)
static void yfree(block)
char *block;
{
    char **p;

    p = (char **)block;
    if (p != (char **)last_yalloced) {
        debug_message("Block mismatch");
        return;
    }
    last_yalloced = *--p;
    xfree((char*)p);
}
#else
#define yfree(block) _yfree()
static void _yfree()
{
    char **p;

    p = (char **)last_yalloced;
    last_yalloced = *--p;
    xfree((char*)p);
}
#endif

/*
 * A mechanism to remember addresses on a stack. The size of the stack is
 * defined in config.h.
 */
static int comp_stackp;
static p_int comp_stack[COMPILER_STACK_SIZE];

static void push_address() {
    if (comp_stackp >= COMPILER_STACK_SIZE) {
        yyerror("Compiler stack overflow");
        comp_stackp++;
        return;
    }
    comp_stack[comp_stackp++] = mem_block[A_PROGRAM].current_size;
}

static void push_explicit(address)
    p_int address;
{
    if (comp_stackp >= COMPILER_STACK_SIZE) {
        yyerror("Compiler stack overflow");
        comp_stackp++;
        return;
    }
    comp_stack[comp_stackp++] = address;
}

static int pop_address() {
    if (comp_stackp == 0)
        fatal("Compiler stack underflow.\n");
    if (comp_stackp > COMPILER_STACK_SIZE) {
        --comp_stackp;
        return 0;
    }
    return comp_stack[--comp_stackp];
}

%ifdef INITIALIZATION_BY___INIT
/*
 * If there is any initialization of a global variable, a function which will
 * execute the initialization code. This code is spread all over the program,
 * with jumps to next initializer. The next variable keeps track of
 * the previous jump. After the last initializer, the jump will be changed
 * into a return(0) statement instead.
 *
 * A function named '__INIT' will be defined, which will contain the
 * initialization code. If there was no initialization code, then the
 * function will not be defined.
 *
 * When inheriting from another object, a call will automatically be made
 * to call __INIT in that code from the current __INIT.
 */
static int last_initializer_end;
static int first_initializer_start;
static int variables_initialized;

/*
 * Arrange a jump to the current position for the initialization code
 * to continue.
 */
static void transfer_init_control() {
    if (last_initializer_end < 0) {
#ifdef ALIGN_FUNCTIONS
        CURRENT_PROGRAM_SIZE =
        (CURRENT_PROGRAM_SIZE + sizeof(char *) - 1) & -sizeof(char *);
#endif
        {
            char *name;
            PREPARE_INSERT(sizeof name + 3);

            name = make_shared_string(STR_VARINIT);
            memcpy(__PREPARE_INSERT__p , (char *)&name, sizeof name);
            __PREPARE_INSERT__p += sizeof(name);
            add_byte(TYPE_ANY);
            add_byte(0); /* num_arg */
            add_byte(0); /* num_local */
            first_initializer_start =
              (CURRENT_PROGRAM_SIZE += sizeof name + 3) - 2;
        }
    } else if ((int)(CURRENT_PROGRAM_SIZE - 2) == last_initializer_end) {
        mem_block[A_PROGRAM].current_size -= 3;
    } else {
        /*
         * Change the address of the last jump after the last
         * initializer to this point.
         */
        upd_short(last_initializer_end,
                  mem_block[A_PROGRAM].current_size);
    }
}

static void add_new_init_jump();

%endif /* INITIALIZATION_BY___INIT */

static char prog_string_tags[32];

/*
 * Initialize the environment that the compiler needs.
 */

static void prolog() {
    int i;

    if (type_of_arguments.block == 0) {
        type_of_arguments.max_size = 100;
        type_of_arguments.block = xalloc(type_of_arguments.max_size);
    }
    type_of_arguments.current_size = 0;
    approved_object = 0;
    last_expression = -1;
    compiled_prog = 0;                /* 0 means fail to load. */
    heart_beat = -1;
    comp_stackp = 0;        /* Local temp stack used by compiler */
    current_continue_address = 0;
    current_break_address = 0;
    num_parse_error = 0;
    free_all_local_names();        /* In case of earlier error */
    /* Initialize memory blocks where the result of the compilation
     * will be stored.
     */
    for (i=0; i < NUMAREAS; i++) {
        mem_block[i].block = xalloc(START_BLOCK_SIZE);
        mem_block[i].current_size = 0;
        mem_block[i].max_size = START_BLOCK_SIZE;
    }
    stored_lines = 0;
    stored_bytes = 0;
    last_include_start = -1;
    bzero(prog_string_tags, sizeof prog_string_tags);
    num_virtual_variables = 0;
    case_blocks = 0;
    case_state.free_block = (struct case_list_entry *)(
      ((PTRTYPE)(&case_blocks))-
      ((PTRTYPE)(&((struct case_list_entry*)0)->next)-(PTRTYPE) 0)
    );
    case_state.next_free = case_state.free_block + 1;
%ifdef INITIALIZATION_BY___INIT
    last_initializer_end = -3;
    variables_initialized = 0;
%endif
}

static int
insert_inherited
    PROT((char *,char *, struct program **, struct function *, int, char *));

/*
 * The program has been compiled. Prepare a 'struct program' to be returned.
 */
int32 current_id_number = 0;

static int define_new_function PROT(( struct ident *, int,int,int,int,int));

/*
 * Define a new function. Note that this function is called at least twice
 * for all function definitions. First as a prototype, then as the real
 * function. Thus, there are tests to avoid generating error messages more
 * than once by looking at (flags & NAME_PROTOTYPE).
 */
static int define_new_function(p, num_arg, num_local, offset, flags, type)
    struct ident *p;
    int num_arg, num_local;
    int offset, flags, type;
{
    int num;
    struct function fun;
    unsigned short argument_start_index;

    flags |= type & ~TYPE_MOD_MASK;
    if (p->type == I_TYPE_GLOBAL && (num = p->u.global.function) >= 0) {
        struct function *funp;

        /*
         * The function was already defined. It may be one of several reasons:
         *
         * 1.        There has been a prototype.
         * 2.        There was the same function defined by inheritance.
         * 3.        This function has been called, but not yet defined.
         * 4.        The function is defined twice.
         * 5.        A "late" prototype has been encountered.
         */
        funp = FUNCTION(num);
        if (funp->flags & TYPE_MOD_NO_MASK &&
          !((funp->flags|flags) & (NAME_PROTOTYPE|NAME_UNDEFINED)) )
            yyerrorf("Illegal to redefine 'nomask' function \"%s\"", p->name);
        if (!(funp->flags & (NAME_UNDEFINED|NAME_PROTOTYPE|NAME_INHERITED) ) )
        {
            yyerrorf("Redeclaration of function %s.", p->name);
            if ( !(flags & NAME_PROTOTYPE) )
                free_string(p->name);
            return num;
        }
        /*
         * It was either an undefined but used funtion, or an inherited
         * function. In both cases, we now consider this to be THE new
         * definition. It might also have been a prototype to an already
         * defined function.
         *
         * Check arguments only when types are supposed to be tested,
         * and if this function really has been defined already.
         *
         * 'nomask' functions may not be redefined.
         */
        if (exact_types && funp->type != TYPE_UNKNOWN) {
            int i;
            if (funp->num_arg != num_arg && !(funp->flags & TYPE_MOD_VARARGS))
                yyerror("Incorrect number of arguments.");
            else if (funp->num_arg == num_arg &&
                     ((funp->flags ^ flags) & TYPE_MOD_XVARARGS) &&
                     !(funp->flags & TYPE_MOD_VARARGS))
                yyerror("Incorrect number of arguments.");
            else {
                unsigned short first_arg;

                first_arg =
                  ((unsigned short *)mem_block[A_ARGUMENT_INDEX].block)[num];
                if (first_arg == INDEX_START_NONE) {
                    if (num_arg && !(funp->flags & NAME_TYPES_LOST) )
                        yyerror(
                          "Called function not compiled with type testing."
                        );
                } else {
                    /* Now check that argument types weren't changed. */
                    for (i=0; i < num_arg; i++) {
                    }
                }
            }
        }
        /* If it was yet another prototype, then simply return. */
        if (flags & NAME_PROTOTYPE) {
            return num;
        }
        if (funp->num_arg != num_arg) {
            funp->num_arg = num_arg;
            ((unsigned short *)mem_block[A_ARGUMENT_INDEX].block)[num] =
              INDEX_START_NONE;
        }
        funp->num_local = num_local;
        funp->flags = flags;
        funp->offset.pc = offset;
#if 0
        funp->function_index_offset = 0;
#endif
        funp->type = type;
        return num;
    }
    if (strcmp(p->name, "heart_beat") == 0)
        heart_beat = mem_block[A_FUNCTIONS].current_size /
            sizeof (struct function);
    fun.name = p->name;
    fun.offset.pc = offset;
    fun.flags = flags;
    fun.num_arg = num_arg;
    fun.num_local = num_local;
#if 0
    fun.function_index_offset = 0;
#endif
    fun.type = type;
    num = mem_block[A_FUNCTIONS].current_size / sizeof fun;
    if (p->type != I_TYPE_GLOBAL) {
        if (p->type != I_TYPE_UNKNOWN) {
            p = make_shared_identifier(p->name, I_TYPE_GLOBAL);
        }
        /* should be I_TYPE_UNKNOWN now. */
        p->type = I_TYPE_GLOBAL;
        p->u.global.variable = -1;
        p->u.global.efun     = -1;
        p->u.global.sim_efun = -1;
        p->next_all = all_globals;
        all_globals = p;
    } else if (p->u.global.variable == -2) {
        struct efun_shadow *q;

#if 0
        fprintf(stderr,"define efun shadow function '%s'\n",p->name);
#endif
        q = (struct efun_shadow *)xalloc(sizeof(struct efun_shadow));
        q->shadow = p;
#if 0
        fprintf(stderr,"all_efun_shadows: %x\n",all_efun_shadows);
        if (all_efun_shadows) {
            fprintf(stderr,"last shadow pnt: %x\n",all_efun_shadows->shadow);
            fprintf(stderr,"last name: '%s'\n",all_efun_shadows->shadow->name);
        }
#endif
        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }
    p->u.global.function = num;
    /* Number of local variables will be updated later */
    add_to_mem_block(A_FUNCTIONS, (char *)&fun, sizeof fun);

    if (exact_types == 0) {
        argument_start_index = INDEX_START_NONE;
    } else {
        int i;

        /*
         * Save the start of argument types.
         */
        argument_start_index =
            mem_block[A_ARGUMENT_TYPES].current_size /
                sizeof (unsigned short);
        for (i=0; i < num_arg; i++) {
            add_to_mem_block(A_ARGUMENT_TYPES, (char *)&type_of_locals[i],
                             sizeof type_of_locals[i]);
        }
    }
    add_to_mem_block(A_ARGUMENT_INDEX, (char *)&argument_start_index,
                     sizeof argument_start_index);
    return num;
}

%ifdef INITIALIZATION_BY___INIT
static void define_variable(name, flags)
    struct ident *name;
    int32 flags;
%else /* INITIALIZATION_BY___INIT */
static void define_variable(name, flags, svp)
    struct ident *name;
    int32 flags;
    struct svalue *svp;
%endif /* INITIALIZATION_BY___INIT */
{
    struct variable dummy;
    int n;

    if (name->type != I_TYPE_GLOBAL) {
        if (name->type != I_TYPE_UNKNOWN) {
            name = make_shared_identifier(name->name, I_TYPE_GLOBAL);
        }
        name->type = I_TYPE_GLOBAL;
        name->u.global.function = -1;
        name->u.global.variable = -1;
        name->u.global.efun     = -1;
        name->u.global.sim_efun = -1;
        name->next_all = all_globals;
        all_globals = name;
    } else if (name->u.global.function == -2) {
        struct efun_shadow *q;

        q = (struct efun_shadow *)xalloc(sizeof(struct efun_shadow));
        q->shadow = name;
        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }
    if ( (n = name->u.global.variable) >= 0) {
        if ( VARIABLE(n)->flags & TYPE_MOD_NO_MASK && !(flags & NAME_HIDDEN))
            yyerrorf("Illegal to redefine 'nomask' variable \"%s\"",
              name->name);
        if (flags & NAME_INHERITED) {
            flags |= ~(VARIABLE(n)->flags) & TYPE_MOD_STATIC;
        } else {
            VARIABLE(n)->flags |=   ~flags & TYPE_MOD_STATIC;
        }
    }
    dummy.name = name->name;
    increment_string_ref(dummy.name);
    dummy.flags = flags;
    if (flags & TYPE_MOD_VIRTUAL) {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = VIRTUAL_VAR_TAG |
                (mem_block[A_VIRTUAL_VAR].current_size / sizeof dummy);
        add_to_mem_block(A_VIRTUAL_VAR, (char *)&dummy, sizeof dummy);
%ifndef INITIALIZATION_BY___INIT
        add_to_mem_block(A_VIRTUAL_VAR_VALUES, (char*)svp, sizeof *svp);
%endif /* INITIALIZATION_BY___INIT */
    } else {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable =
                mem_block[A_VARIABLES].current_size / sizeof dummy;
        add_to_mem_block(A_VARIABLES, (char *)&dummy, sizeof dummy);
%ifndef INITIALIZATION_BY___INIT
        add_to_mem_block(A_VARIABLE_VALUES, (char*)svp, sizeof *svp);
%endif /* INITIALIZATION_BY___INIT */
    }
}

static void redeclare_variable(name, flags, n)
    struct ident *name;
    int32 flags;
    int n;
{
    if (name->type != I_TYPE_GLOBAL) {
        /* I_TYPE_UNKNOWN */
        name->type = I_TYPE_GLOBAL;
        name->u.global.function = -1;
        name->u.global.variable = -1;
        name->u.global.efun     = -1;
        name->u.global.sim_efun = -1;
        name->next_all = all_globals;
        all_globals = name;
    } else if (name->u.global.function == -2) {
        struct efun_shadow *q;

        q = (struct efun_shadow *)xalloc(sizeof(struct efun_shadow));
        q->shadow = name;
        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }
    if (flags & NAME_HIDDEN)
        return;
    if (name->u.global.variable >= 0 && name->u.global.variable != n) {
        if (VARIABLE(name->u.global.variable)->flags & TYPE_MOD_NO_MASK )
            yyerrorf("Illegal to redefine 'nomask' variable \"%s\"",
                name->name);
    } else if ( V_VARIABLE(n)->flags & TYPE_MOD_NO_MASK &&
         !(V_VARIABLE(n)->flags & NAME_HIDDEN) &&
         (V_VARIABLE(n)->flags ^ flags) & TYPE_MOD_STATIC )
    {
        yyerrorf("Illegal to redefine 'nomask' variable \"%s\"", name->name);
    }
    name->u.global.variable = n;
    V_VARIABLE(n)->flags = flags;
}

static int last_string_is_new;

static int prog_string_indizes[0x100];

static short store_prog_string(str)
    char *str;
{
    int size;
    long hash;
    char mask, *tagp;
    int i, *indexp;

    hash = (long)str ^ (long)str >> 16;
    hash = (hash ^ hash >> 8);
    mask = 1 << (hash & 7);
    hash = hash & 0xff;
    indexp = &prog_string_indizes[hash];
    tagp = &prog_string_tags[hash >> 3];
    if (*tagp & mask) {
        i = *indexp;
        for(;;) {
            if ( ((char**)(mem_block[A_STRINGS].block))[i] == str ) {
                free_string(str); /* Needed as string is only free'ed once. */
                last_string_is_new = 0;
                return i;
            }
            if (
              (i=*((int*)(&((char**)(mem_block[A_STRING_NEXT].block))[i]))) < 0
            )
                break;
        }
        i = *indexp;
    } else {
        *tagp |= mask;
        i = -1;
    }
    size = mem_block[A_STRINGS].current_size;
    if (size + sizeof(char *) > mem_block[A_STRINGS].max_size) {
        if (!realloc_mem_block(&mem_block[A_STRINGS], 0) ||
            !realloc_mem_block(&mem_block[A_STRING_NEXT], 0))
        {
            if (i < 0)
                *tagp &= ~mask;
            last_string_is_new = 0;
            return 0;
        }
    }
    mem_block[A_STRING_NEXT].current_size =
      mem_block[A_STRINGS].current_size = size + sizeof str;
    *((char **)(mem_block[A_STRINGS].block+size)) = str;
    *((int *)(mem_block[A_STRING_NEXT].block+size)) = i;
    last_string_is_new = 1;
    return *indexp = size / sizeof str;
}

static void delete_prog_string()
{
    char *str;
    int size;
    long hash;
    char mask, *tagp;
    int *indexp;

    size = mem_block[A_STRINGS].current_size - sizeof(char *);
    free_string(
      str = *(char**)(mem_block[A_STRINGS].block+size)
    );
    mem_block[A_STRING_NEXT].current_size =
      mem_block[A_STRINGS].current_size = size;
    hash = (long)str ^ (long)str >> 16;
    hash = (hash ^ hash >> 8);
    mask = 1 << (hash & 7);
    hash = hash & 0xff;
    indexp = &prog_string_indizes[hash];
    tagp = &prog_string_tags[hash >> 3];
    if ( ( *indexp = *((int *)(mem_block[A_STRING_NEXT].block+size)) ) < 0)
        *tagp &= ~mask;
}

%ifndef INITIALIZATION_BY___INIT
/* convert an svalue how mt is used at run-time to it's compile-time type */
INLINE static
int type_rtoc(svp)
    struct svalue *svp;
{
    switch (svp->type) {
      case T_NUMBER:                if (!svp->u.number)
                                    return TYPE_ANY;
                                return TYPE_NUMBER;
      case T_STRING:                return TYPE_STRING;
      case T_POINTER:                return TYPE_MOD_POINTER | TYPE_ANY;
      case T_FLOAT:                return TYPE_FLOAT;
      case T_CLOSURE:                return TYPE_CLOSURE;
      case T_SYMBOL:                return TYPE_SYMBOL;
      case T_QUOTED_ARRAY:        return TYPE_QUOTED_ARRAY;
      case T_MAPPING:                return TYPE_MAPPING;
      default:                        fatal("Bad svalue type at compile time.\n");
        return TYPE_ANY; /* gag the compiler warning ... */
    }
}

INLINE static
struct svalue *copy_svalue(svp)
    struct svalue *svp;
{
    switch (svp->type) {
      case T_NUMBER:
      case T_FLOAT:
        break;
      case T_STRING:
        if (svp->x.string_type != STRING_SHARED)
            return &const0;
      case T_SYMBOL:
        increment_string_ref(svp->u.string);
        break;
      case T_POINTER:
      case T_QUOTED_ARRAY:
        svp->u.vec->ref++;
        break;
      case T_MAPPING:
        svp->u.map->ref++;
        break;
      case T_CLOSURE:
        if (CLOSURE_MALLOCED(svp->x.closure_type))
            svp->u.lambda->ref++;
        else
            add_ref(svp->u.ob, "ass to var");
        break;
      default:
        return &const0;
    }
    return svp;
}
%endif /* INITIALIZATION_BY___INIT */

static void insert_pop_value();

#define FIX_BRANCH(lfcode, destination, location) fix_branch(\
  (lfcode)-F_OFFSET, destination, location)

static int fix_branch(ltoken, dest, loc)
    int ltoken, dest, loc;
{
    int offset;

    offset =  dest - (loc +1);
    if (offset > 0xff) {
        int i,j;
        char *p;

#ifdef DEBUG
        if (d_flag > 2) {
            debug_message("fix_branch handles long offset.\n");
        }
#endif
        if ( current_break_address > loc &&
            !(current_break_address & (BREAK_ON_STACK|BREAK_DELIMITER) ) )
        {
            for(i = current_break_address; (j=read_short(i)) > loc; )
            {
                upd_short(i, j+1);
                i = j;
            }
            current_break_address++;
        }
        if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > loc &&
            !(current_continue_address & CONTINUE_DELIMITER ) )
        {
            for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
              (j=read_short(i)) > loc; )
            {
                upd_short(i, j+1);
                i = j;
            }
            current_continue_address++;
        }
        ins_byte(0);
        p = mem_block[A_PROGRAM].block +
            mem_block[A_PROGRAM].current_size-1;
        i = mem_block[A_PROGRAM].current_size - loc;
        for( ; --i >= 0; --p ) *p = p[-1];
        *p = ltoken;
        upd_short(loc, offset+2);
        if (offset > 0x7ffd)
            yyerror("offset overflow");
        return 1;
    } else {
        mem_block[A_PROGRAM].block[loc] = offset;
        return 0;
    }
}

static char *ystring_copy(str)
char *str;
{
    char *p;

    p = yalloc(strlen(str)+1);
    (void)strcpy(p, str);
    return p;
}

static void add_string_constant() {
    mp_int len1;
    char *tmp;

    len1 = strlen(last_string_constant);
    tmp = alloca(len1 + strlen(last_lex_string) + 1);
    strcpy(tmp, last_string_constant);
    strcpy(tmp + len1, last_lex_string);
    free_string(last_string_constant);
    free_string(last_lex_string);
    last_string_constant = make_shared_string(tmp);
    last_lex_string = 0;
}

static char *yyget_space(size)
    p_int size;
{
    while (CURRENT_PROGRAM_SIZE + size > mem_block[A_PROGRAM].max_size)
        realloc_a_program();
    CURRENT_PROGRAM_SIZE += size;
    return mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE - size;
}

static void yymove_switch_instructions(len, blocklen)
    int len;
    p_int blocklen;
{
    mp_int i, j;

    if ( (CURRENT_PROGRAM_SIZE += len) > mem_block[A_PROGRAM].max_size )
        (void)realloc_a_program();
    if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > switch_pc &&
        !(current_continue_address & CONTINUE_DELIMITER ) )
    {
        for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
          (j=read_short(i)) > switch_pc; )
        {
                upd_short(i, j+len);
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

static void yycerrorl(s1, s2, line1, line2)
    char *s1, *s2;
    int line1, line2;
{
    char buff[100];

    sprintf(buff, s2, line1, line2);
    yyerrorf(s1, buff);
}


%ifndef INITIALIZATION_BY___INIT
static struct vector *list_to_vector(length, initialized)
    int length;
    struct svalue *initialized;
{
    struct const_list *list;
    struct vector *vec;
    struct svalue *svp;
    char *block;
    struct const_list_svalue *clsv;
%line
    vec = allocate_array(length);
    if (length) {
        svp = initialized->u.lvalue;
        clsv = initialized->u.const_list;
        list = &clsv->list;
        block = (char *)clsv;
        svp = vec->item;
        do {
            *svp++ = list->val;
            list = list->next;
            xfree(block);
        } while ( (block = (char *)list) );
    }
    initialized->type = T_POINTER;
    initialized->u.vec = vec;
    return vec;
}

static void free_const_list_svalue(svp)
    struct svalue *svp;
{
    struct const_list *list;
    char *block;
%line
        list = &((struct const_list_svalue *)svp)->list;
        block = (char *)svp;
        do {
            free_svalue(&list->val);
            list = list->next;
            xfree(block);
        } while ( (block = (char *)list) );
}
%endif

LOCAL_INLINE int proxy_efun PROT((int, int));

static void arrange_protected_lvalue PROT((int, int, int, int));

struct s_lrvalue {
    short type;
    uint32 start;
    short code;
    uint32 end;
};

static struct s_lrvalue indexing_argument, indexing_index1, indexing_index2;
static int indexing_code;
#ifndef INITIALIZATION_BY___INIT
static struct svalue *currently_initialized;
#endif

%}

%efuns /* Include the tokens */

%union
{
%line
        p_int number;
        p_uint address;        /* Address of an instruction */
        char *string;
        char *shared_string;
        short type;
        struct { p_int key; int numeric; } case_label;
        struct { int simul_efun; short start; } function_call_head;
        struct svalue svalue;
%if 0
        struct vector *array;
%endif
        struct svalue *initialized;
        struct {
            int function;
            struct svalue *initialized;
        } const_call_head;
        p_int numbers[2];
        struct {
            p_int length;
            struct const_list * l;
        } const_list;
        struct {
            char *p;
            unsigned short length;
            unsigned short line;
        } expression;
        struct {
            union {
                char *p, simple[2];
            } u;
            unsigned short length;
            short type;
        } lvalue;
        struct {
            char *super;
            struct ident *real;
        } function_name;
        struct ident *ident;
        double float_number;
        struct {
            p_int number;
%if 0
            int type;
%endif
        } closure;
        struct {
            char *name;
            int quotes;
        } symbol;
        struct s_lrvalue lrvalue;
}

%type <number> F_ASSIGN F_NUMBER constant F_LOCAL expr_list expr_list3
%type <number> lvalue_list argument type basic_type optional_star expr_list2
%type <float_number> F_FLOAT
%type <closure> F_CLOSURE
%type <symbol> F_SYMBOL
%type <number> F_QUOTED_AGGREGATE

%ifndef INITIALIZATION_BY___INIT
%type <svalue> float_constant
%type <const_list> const_expr_list const_expr_list2 const_expr_list3
%endif /* INITIALIZATION_BY___INIT */

%type <number> inheritance_modifier inheritance_modifier_list
%type <numbers> inheritance_qualifier inheritance_qualifiers
%type <number> type_modifier type_modifier_list opt_basic_type function_body
%type <number> argument_list optional_else pre_inc_dec

%ifdef MAPPINGS
%type <numbers> m_expr_list m_expr_list2
%type <number> m_expr_values
%endif

%type <numbers> condStart
%type <ident> F_IDENTIFIER
%type <function_name> function_name
%type <string> anchestor

%type <case_label> case_label

/* The following symbols return type information */

%type <type> decl_cast cast
%type <lvalue> lvalue
%type <lrvalue> function_call expr4
%type <lrvalue> catch sscanf
%ifdef SUPPLY_PARSE_COMMAND
%type <lrvalue> parse_command
%endif

%type <lrvalue> expr0 comma_expr

%type <lrvalue> note_start

%right F_ASSIGN
%right '?'
%left F_LOR
%left F_LAND
%left '|'
%left '^'
%left '&'
%left F_EQ F_NE
%left '<' F_LE '>' F_GE
%left F_LSH F_RSH
%left '+' '-'
%left '*' '/' '%'
%right '~' F_NOT
%nonassoc F_INC F_DEC
%left F_ARROW '['
%%

all: program;

program: program def possible_semi_colon
       |         /* empty */ ;

possible_semi_colon: /* empty */
                   | ';' { yyerror("Extra ';'. Ignored."); };

inheritance: inheritance_qualifiers F_INHERIT string_constant ';'
                {
%line
/* copy_variables might add extra inherits for virtual inheritance.
 * For this reason, copy_functions() can't know the actual index
 * of the new inherit, so it sets it to NEW_INHERITED_INDEX instead.
 * This is changed later to the actual value by
 * fix_function_inherit_indices() .
 */
                    struct object *ob;
                    struct inherit inherit;
%ifdef INITIALIZATION_BY___INIT
                    int initializer;
%endif /* INITIALIZATION_BY___INIT */

                    if (CURRENT_PROGRAM_SIZE)
%ifdef INITIALIZATION_BY___INIT
                      if (!(((struct function *)(mem_block[A_FUNCTIONS].block+
                         mem_block[A_FUNCTIONS].current_size))[-1].flags &
                         NAME_INHERITED))
%endif /* INITIALIZATION_BY___INIT */
                        yyerror(
                          "illegal to inherit after defining functions"
                        );
                    ob = find_object(last_string_constant);
                    if (ob == 0) {
                        inherit_file = last_string_constant;
                        last_string_constant = 0;
                        /* Return back to load_object() */
                        YYACCEPT;
                    }
                    /* We want to refer to the program;
                       variables are needed too if they are initialized. */
                    if (ob->flags & O_SWAPPED && load_ob_from_swap(ob) < 0) {
                        free_string(last_string_constant);
                        last_string_constant = 0;
                        yyerror("Out of memory");
                        YYACCEPT;
                    }
                    free_string(last_string_constant);
                    last_string_constant = 0;
                    if (ob->flags & O_APPROVED)
                        approved_object = 1;
                    inherit.prog = ob->prog;
                    inherit.function_index_offset =
                        mem_block[A_FUNCTIONS].current_size /
                          sizeof(struct function);
%ifdef INITIALIZATION_BY___INIT
                    initializer = copy_functions(ob->prog, $1[0]);
                    if (initializer > 0) {
                        transfer_init_control();
                        ins_f_byte(F_CALL_EXPLICIT_INHERITED);
                        ins_short(mem_block[A_INHERITS].current_size /
                            sizeof inherit);
                        ins_short(initializer);
                        ins_byte(0);        /* Actual number of arguments */
                        ins_f_byte(F_POP_VALUE);
                        add_new_init_jump();
                    }
                    copy_variables(ob->prog, $1[1]);
%else  /* INITIALIZATION_BY___INIT */
                    copy_functions(ob->prog, $1[0]);
                    copy_variables(ob->prog, $1[1], ob->variables);
%endif /* INITIALIZATION_BY___INIT */
                    fix_function_inherit_indices(ob->prog);
                    inherit.variable_index_offset =
                      $1[1] & TYPE_MOD_VIRTUAL ?
                        mem_block[A_VIRTUAL_VAR].current_size /
                          sizeof(struct variable) - ob->prog->num_variables
                      :
                        (mem_block[A_VARIABLES].current_size /
                          sizeof(struct variable) - ob->prog->num_variables)
                        | NON_VIRTUAL_OFFSET_TAG;
                    add_to_mem_block(
                      A_INHERITS,
                      (char *)&inherit,
                      sizeof inherit
                    );
                    byte_to_mem_block(A_INHERIT_FLAG, 0);
                    num_virtual_variables =
                      mem_block[A_VIRTUAL_VAR].current_size /
                        sizeof (struct variable);
                }
optional_star: /* empty */ { $$ = 0; } | '*' { $$ = TYPE_MOD_POINTER; } ;

function_body:
        {
%line

#ifdef ALIGN_FUNCTIONS
            CURRENT_PROGRAM_SIZE =
              (CURRENT_PROGRAM_SIZE + sizeof(char *) - 1) & -sizeof(char *);
#endif
            $<number>$ = CURRENT_PROGRAM_SIZE;
            if (
              (CURRENT_PROGRAM_SIZE += sizeof(char *) + 3) >
              mem_block[A_PROGRAM].max_size )
            {
                (void)realloc_a_program();
            }
        } block
%ifdef YACC_CANNOT_MIX_ANONYMOUS_WITH_DEFAULT
        { $$ = $<number>1; }
%endif
    |   ';' { $$ = -1; } ;

def: type optional_star F_IDENTIFIER
        {
            $2 |= $1;

            if ($1 & TYPE_MOD_MASK) {
                exact_types = $2;
            } else {
                if (pragma_strict_types != PRAGMA_WEAK_TYPES)
                    yyerror("\"#pragma strict_types\" requires type of function");
                exact_types = 0;
            }
            if ($3->type == I_TYPE_UNKNOWN) {
                /* prevent freeing by exotic name clashes */
                struct ident *p = $3;
                p->type = I_TYPE_GLOBAL;
                p->u.global.variable = -1;
                p->u.global.efun     = -1;
                p->u.global.sim_efun = -1;
                p->u.global.function = -1;
                p->next_all = all_globals;
                all_globals = p;
            }
        }
        '(' argument ')'
        {
            if (current_number_of_locals &&
                  (full_type_of_locals[current_number_of_locals-1] &
                    TYPE_MOD_VARARGS))
            {
%line
                /* The last argument has to allow an array. */
                unsigned short *t;
#ifdef NO_XVARARGS
                yyerror ("variadic function supporty disabled");
#endif
                $2 |= TYPE_MOD_XVARARGS;
                t = type_of_locals + (current_number_of_locals-1);
                if (!(*t & TYPE_MOD_POINTER) && (*t & TYPE_MOD_RMASK)!=TYPE_ANY)
                {
                    if ((*t & TYPE_MOD_RMASK) != TYPE_UNKNOWN)
                        yyerror(
                          "varargs parameter must be declared array or mixed");
                    *t &= ~TYPE_MOD_RMASK;
                    *t |= TYPE_ANY;
                }
            }
            /*
             * Define a prototype. If it is a real function, then the
             * prototype will be replaced below.
             */
            define_new_function($3, $6, 0, 0,
                                NAME_UNDEFINED|NAME_PROTOTYPE, $2);
        }
        function_body
        {
            int start;
            char *p;
%line
            if ( (start = $9) < 0) {
                /* function_body was a ';' ==> prototype */
                uint32 *flagp;

                flagp = &FUNCTION($3->u.global.function)->flags;
                *flagp |= $1 &
                  (*flagp & TYPE_MOD_PUBLIC ?
                    (TYPE_MOD_NO_MASK) :
                    (TYPE_MOD_NO_MASK|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|
                     TYPE_MOD_PROTECTED|TYPE_MOD_PUBLIC) );
            } else {
                /* function_body was a block */
                p = &mem_block[A_PROGRAM].block[start];
                memcpy(p, (char *)&$3->name, sizeof $3->name);
                p += sizeof $3->name;
                *p++ = $2;
                if ($2 & TYPE_MOD_XVARARGS)
                  *p++ = $6 | ~0x7f;
                else
                  *p++ = $6;
                *p   = current_number_of_locals - $6+ max_break_stack_need;
                define_new_function($3, $6, current_number_of_locals - $6+
                        max_break_stack_need,
                        start + sizeof $3->name + 1, 0, $2);
                increment_string_ref($3->name);
                ins_f_byte(F_RETURN0);
            }
            free_all_local_names();
        }
   | type name_list ';' { if ($1 == 0) yyerror("Missing type"); }
   | inheritance ;

new_arg_name: type optional_star F_IDENTIFIER
        {
            if (exact_types && $1 == 0) {
                yyerror("Missing type for argument");
                add_local_name($3, TYPE_ANY);        /* Supress more errors */
            } else {
                add_local_name($3, $1 | $2);
            }
        }
        | type optional_star F_LOCAL
        {
            yyerror("Illegal to redeclare local name");
        } ;

argument: /* empty */ { $$ = 0; }
          | argument_list ;

argument_list: new_arg_name { $$ = 1; }
             | argument_list ',' new_arg_name { $$ = $1 + 1; } ;

type_modifier: F_NO_MASK { $$ = TYPE_MOD_NO_MASK; }
             | F_STATIC { $$ = TYPE_MOD_STATIC; }
             | F_PRIVATE { $$ = TYPE_MOD_PRIVATE; }
             | F_PUBLIC { $$ = TYPE_MOD_PUBLIC; }
             | F_VARARGS { $$ = TYPE_MOD_VARARGS; }
             | F_PROTECTED { $$ = TYPE_MOD_PROTECTED; } ;

inheritance_modifier:
               F_VIRTUAL { $$ = TYPE_MOD_VIRTUAL; } ;

type_modifier_list: /* empty */ { $$ = 0; }
        | type_modifier_list type_modifier { $$ = $1 | $2; } ;

inheritance_modifier_list: type_modifier_list
        | inheritance_modifier_list inheritance_modifier type_modifier_list
          { $$ = $1 | $2 | $3; } ;

inheritance_qualifier: type optional_star F_IDENTIFIER
        {
            static struct ident *last_identifier;
            static uint32 last_modifier;
%line
            if ($1 & TYPE_MOD_MASK) {
                yyerror("syntax error");
            }
            if ( !($1 & ~TYPE_MOD_MASK) ) {
                /* take lookahead into account */
                if ($3 == last_identifier) {
                    last_identifier = 0;
                    $$[0] = $$[1] = 0;
                    break;
                }
            } else {
                last_modifier = $1 & ~TYPE_MOD_MASK;
            }
            last_identifier = $3;
            if ($2) {
                yyerror("syntax error");
            }
            if (strcmp(last_identifier->name, "functions") == 0) {
                $$[0] = last_modifier;
                $$[1] = 0;
            } else if (strcmp(last_identifier->name, "variables") == 0) {
                $$[0] = 0;
                $$[1] = last_modifier;
            } else {
                yyerror("Unrecognized inheritance modifier qualifier");
                $$[0] = $$[1] = 0;
            }
            if (last_identifier->type == I_TYPE_UNKNOWN)
                free_shared_identifier(last_identifier);
        } ;

inheritance_qualifiers: inheritance_modifier_list
        {
            $$[0] = $$[1] = $1;
        }
        | inheritance_qualifier inheritance_qualifiers
        {
            $$[0] = $1[0] | $2[0];
            $$[1] = $1[1] | $2[1];
        } ;

type: type_modifier_list opt_basic_type { $$ = $1 | $2; current_type = $$; } ;

cast: '(' basic_type optional_star ')'
        {
            $$ = $2 | $3;
        } ;

decl_cast: '(' '{' basic_type optional_star '}' ')'
        {
            $$ = $3 | $4;
        } ;

opt_basic_type: basic_type | /* empty */ { $$ = TYPE_UNKNOWN; } ;

basic_type: F_STATUS { $$ = TYPE_NUMBER; current_type = $$; }
        | F_INT { $$ = TYPE_NUMBER; current_type = $$; }
        | F_STRING_DECL { $$ = TYPE_STRING; current_type = $$; }
        | F_OBJECT { $$ = TYPE_OBJECT; current_type = $$; }
        | F_CLOSURE_DECL { $$ = TYPE_CLOSURE; current_type = $$; }
        | F_SYMBOL_DECL { $$ = TYPE_SYMBOL; current_type = $$; }
%ifdef FLOATS
        | F_FLOAT_DECL { $$ = TYPE_FLOAT; current_type = $$; };
%endif
%ifdef MAPPINGS
        | F_MAPPING { $$ = TYPE_MAPPING; current_type = $$; };
%endif
        | F_VOID {$$ = TYPE_VOID; current_type = $$; }
        | F_MIXED { $$ = TYPE_ANY; current_type = $$; } ;

name_list: new_name
         | name_list ',' new_name;

new_name: optional_star F_IDENTIFIER
        {
%line
            if (current_type & TYPE_MOD_VARARGS) {
                yyerror("can't declare a variable as varargs");
                current_type &= ~TYPE_MOD_VARARGS;
            }
%ifdef INITIALIZATION_BY___INIT
            define_variable($2, current_type | $1);
        }
| optional_star F_IDENTIFIER
        {
            define_variable($2, current_type | $1);
            $<number>$ = verify_declared($2);
            transfer_init_control();
        }
        F_ASSIGN expr0
        {
            int i = $<number>3;
            PREPARE_S_INSERT(4)

#ifdef DEBUG
            if (i & VIRTUAL_VAR_TAG) {
                /* When we want to allow 'late' initializers for
                 * inherited variables, it must have a distinct syntax,
                 * lest name clashs remain undetected, making LPC code
                 * hard to debug.
                 */
                fatal("Newly declared variable is virtual\n");
            }
#endif
            variables_initialized = 1;
            if (i + num_virtual_variables > 0xff) {
                add_f_byte(F_PUSH_IDENTIFIER16_LVALUE);
                add_short(i + num_virtual_variables);
                CURRENT_PROGRAM_SIZE += 1;
            } else {
                add_f_byte(F_PUSH_IDENTIFIER_LVALUE);
                add_byte(i + num_virtual_variables);
            }
            if ($4 != F_ASSIGN-F_OFFSET) yyerror("Illegal initialization");
            if (!compatible_types((current_type | $1) & TYPE_MOD_MASK, $5.type)){
                yyerrorf("Type mismatch %s when initializing %s",
                        get_two_types(current_type | $1, $5.type), $2->name);
            }
            add_f_byte(F_VOID_ASSIGN);
            CURRENT_PROGRAM_SIZE += 3;
            add_new_init_jump();
        } ;
%else /* INITIALIZATION_BY___INIT */
            define_variable($2, current_type | $1, &const0);
        }
        | optional_star F_IDENTIFIER
        {
            /* svalue_constant can contain identifiers, so define the variable
             * now, lest the identifier could get freed by a name clash.
             */
            int n;
%line
            define_variable($2, current_type | $1 | NAME_INITIALIZED, &const0);
            n = $2->u.global.variable;
            $<initialized>$ = currently_initialized =
              n & VIRTUAL_VAR_TAG ?
                &((struct svalue *)mem_block[A_VIRTUAL_VAR_VALUES].block)
                  [n & ~VIRTUAL_VAR_TAG] :
                &((struct svalue *)mem_block[A_VARIABLE_VALUES].block)[n];
        }
        F_ASSIGN svalue_constant
        {
%line
            if ($4 != F_ASSIGN-F_OFFSET) yyerror("Illegal initialization");
            if (exact_types)
                if (!TYPE( current_type | $1 , type_rtoc($<initialized>3)) ) {
                    yyerror("Bad initializer type");
                }
        };
%endif /* INITIALIZATION_BY___INIT */

block: '{' local_declarations statements '}';

local_declarations: /* empty */
                  | local_declarations basic_type local_name_list ';' ;

new_local_name: optional_star F_IDENTIFIER
        {
            add_local_name($2, current_type | $1);
        } ;

local_name_list: new_local_name
        | local_name_list ',' new_local_name ;

statements: /* empty */
          | statements statement

statement: comma_expr ';'
        {
            insert_pop_value();
%ifdef F_BREAK_POINT
            if (d_flag)
                ins_f_byte(F_BREAK_POINT);
%endif /* F_BREAK_POINT */
            /* if (exact_types && !BASIC_TYPE($1.type, TYPE_VOID))
                yyerror("Value thrown away"); */
        }
         | error ';' ;
         | cond | while | do | for | switch | case | default | return ';'
         | block
           | /* empty */ ';'
         | F_BREAK ';'        /* This code is a jump */
                {
                    if (current_break_address == 0)
                        yyerror("break statement outside loop");
                    if (current_break_address & BREAK_ON_STACK) {
                        ins_f_byte(F_BREAK);
                    } else {
                        ins_f_byte(F_LBRANCH);
                        ins_short(current_break_address);
                        current_break_address =
                          mem_block[A_PROGRAM].current_size - 2;
                    }
                }
         | F_CONTINUE ';'        /* This code is a jump */
                {
                    int depth;
%line
                    if (current_continue_address == 0)
                        yyerror("continue statement outside loop");
                    if ( 0 != (depth = (current_continue_address & SWITCH_DEPTH_MASK)) )
                    {
                        while(depth > SWITCH_DEPTH_UNIT*256) {
                            ins_f_code(F_BREAKN_CONTINUE);
                            ins_byte(255);
                            ins_short(2);
                            depth -= SWITCH_DEPTH_UNIT*256;
                        }
                        if (depth > SWITCH_DEPTH_UNIT) {
                            depth /= SWITCH_DEPTH_UNIT;
                            ins_f_code(F_BREAKN_CONTINUE);
                            ins_byte(depth-1);
                        } else {
                            ins_f_code(F_BREAK_CONTINUE);
                        }
                    } else {
                        ins_f_byte(F_LBRANCH);
                    }
                    ins_short(current_continue_address);
                    current_continue_address =
                        ( current_continue_address & SWITCH_DEPTH_MASK ) |
                        ( mem_block[A_PROGRAM].current_size - 2 );
                }
         ;

while:  {   $<numbers>$[0] = current_continue_address;
            $<numbers>$[1] = current_break_address;
            push_address();
        } F_WHILE '(' comma_expr ')'
        {
%line
            int addr = pop_address();
            int length = CURRENT_PROGRAM_SIZE - addr;
            char *expression;

            expression = yalloc(length+2);
            memcpy(expression, mem_block[A_PROGRAM].block+addr, length);
            if (last_expression == CURRENT_PROGRAM_SIZE - 1 &&
                expression[length-1] == F_NOT - F_OFFSET        )
            {
                length--;
                expression[length] = F_BBRANCH_WHEN_ZERO - F_OFFSET;
            } else {
                expression[length] = F_BBRANCH_WHEN_NON_ZERO - F_OFFSET;
            }
            $<expression>$.p = expression;
            $<expression>$.length = length;
            $<expression>$.line = current_line;
            CURRENT_PROGRAM_SIZE = addr;
            last_expression = -1;
            ins_f_byte(F_BRANCH); /* to expression */
            push_address();
            ins_byte(0);
            current_continue_address = CONTINUE_DELIMITER;
            current_break_address = BREAK_DELIMITER;
        }
       statement
        {
%line
          int offset;
          int next_addr;
          int addr = pop_address();

          for(;current_continue_address > 0;
            current_continue_address = next_addr) {
              next_addr = read_short(current_continue_address);
              upd_short(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }
          offset = FIX_BRANCH( F_LBRANCH, CURRENT_PROGRAM_SIZE, addr);
          if ($<expression>6.line != current_line)
              store_line_number_info();
          add_to_mem_block(A_PROGRAM, $<expression>6.p, $<expression>6.length+2);
          yfree($<expression>6.p);
          offset += addr + 1 - ( CURRENT_PROGRAM_SIZE - 1 );
          if (offset < -0xff) {
              char * codep;

              if (offset < -0x8000)
                  yyerror("offset overflow");
              codep = mem_block[A_PROGRAM].block + --CURRENT_PROGRAM_SIZE - 1;
              *codep = *codep == F_BBRANCH_WHEN_NON_ZERO - F_OFFSET ?
                F_LBRANCH_WHEN_NON_ZERO - F_OFFSET :
                F_LBRANCH_WHEN_ZERO - F_OFFSET
              ;
              ins_short(offset);
          } else {
              mem_block[A_PROGRAM].block[CURRENT_PROGRAM_SIZE-1] = -offset;
          }
          if ($<expression>6.line != current_line)
              store_line_number_relocation($<expression>6.line);
          for(;current_break_address > 0;current_break_address = next_addr) {
              next_addr = read_short(current_break_address);
              upd_short(current_break_address,
                  CURRENT_PROGRAM_SIZE - current_break_address);
          }
          current_continue_address = $<numbers>1[0];
          current_break_address    = $<numbers>1[1];
        }

do: {
        $<numbers>$[0] = current_continue_address;
        $<numbers>$[1] = current_break_address;
        current_break_address = BREAK_DELIMITER;
        current_continue_address = CONTINUE_DELIMITER;
        push_address();
    } F_DO statement F_WHILE
    {
        int next_addr;
        int current;
%line
        current = CURRENT_PROGRAM_SIZE;
        for(;current_continue_address > 0;
          current_continue_address = next_addr) {
            next_addr = read_short(current_continue_address);
            upd_short(current_continue_address,
                current - current_continue_address);
        }
    } '(' comma_expr ')' ';'
    {
%line
        int offset;
        int next_addr;
        int addr = pop_address();
        mp_uint current;
        char *dest;
        char tmp_short[2];

        current = CURRENT_PROGRAM_SIZE;
        if (current + 3 > mem_block[A_PROGRAM].max_size)
            realloc_a_program();
        dest = mem_block[A_PROGRAM].block + current;
        if (current == last_expression + 1 && dest[-1] == F_NOT - F_OFFSET) {
            offset = addr - current;
            if (offset < -0xff) {
                if (offset < -0x8000)
                    yyerror("offset overflow");
                *((short *)tmp_short) = offset;
                dest[-1] = F_LBRANCH_WHEN_ZERO - F_OFFSET;
                *dest++ = tmp_short[0];
                *dest   = tmp_short[1];
                current += 2;
            } else {
                dest[-1] = F_BBRANCH_WHEN_ZERO - F_OFFSET;
                *dest = -offset;
                current++;
            }
        } else {
            offset = addr - ( current + 1 );
            if (offset < -0xff) {
                if (offset < -0x8000)
                    yyerror("offset overflow");
                *((short *)tmp_short) = offset;
                *dest++ = F_LBRANCH_WHEN_NON_ZERO - F_OFFSET;
                *dest++ = tmp_short[0];
                *dest   = tmp_short[1];
                current += 3;
            } else {
                *dest++ = F_BBRANCH_WHEN_NON_ZERO - F_OFFSET;
                *dest = -offset;
                current += 2;
            }
        }
        CURRENT_PROGRAM_SIZE = current;
        for(;current_break_address > 0;current_break_address = next_addr) {
            next_addr = read_short(current_break_address);
            upd_short(current_break_address,
                current - current_break_address);
        }
        current_continue_address = $<numbers>1[0];
        current_break_address    = $<numbers>1[1];
    }

for: F_FOR '('          { $<numbers>$[0] = current_continue_address;
                    $<numbers>$[1] = current_break_address; }
     for_expr ';'
        {   insert_pop_value();
            current_continue_address = CONTINUE_DELIMITER;
            $<number>$ = CURRENT_PROGRAM_SIZE;
        }
     for_expr ';'
        {
%line
            int start, length;
            char *expression;

            start = $<number>6;
            length = CURRENT_PROGRAM_SIZE - start;
            expression = yalloc(length+2);
            memcpy(expression,
              mem_block[A_PROGRAM].block + start, length );
            if (last_expression == CURRENT_PROGRAM_SIZE - 1 &&
                expression[length-1] == F_NOT - F_OFFSET        )
            {
                length--;
                expression[length] = F_BBRANCH_WHEN_ZERO - F_OFFSET;
            } else {
                expression[length] = F_BBRANCH_WHEN_NON_ZERO - F_OFFSET;
            }
            $<expression>$.p = expression;
            $<expression>$.length = length;
            $<expression>$.line = current_line;
            CURRENT_PROGRAM_SIZE = start;
            last_expression = -1;
        }
     for_expr ')'
        {
%line
            int length;

            insert_pop_value();
            length = CURRENT_PROGRAM_SIZE - $<number>6;
            $<expression>$.p = yalloc(length);
            if (length)
                memcpy($<expression>$.p,
                  mem_block[A_PROGRAM].block + $<number>6, length );
            $<expression>$.length = length;
            $<expression>$.line = current_line;
            CURRENT_PROGRAM_SIZE = $<number>6;
            last_expression = -1;
            ins_f_byte(F_BRANCH); /* to expression */
            ins_byte(0);
            current_break_address = BREAK_DELIMITER;
        }
     statement
        {
%line
          int offset;
          int next_addr;

          for(;current_continue_address > 0;
            current_continue_address = next_addr) {
              next_addr = read_short(current_continue_address);
              upd_short(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }
          if ( $<expression>9.line != current_line ||
               ($<expression>12.line != current_line &&
                $<expression>12.length) )
              store_line_number_info();
          if ($<expression>12.length) {
              add_to_mem_block(A_PROGRAM,
                $<expression>12.p, $<expression>12.length);
              if ($<expression>12.line != $<expression>9.line)
                  store_line_number_relocation($<expression>12.line);
          }
          yfree($<expression>12.p);
          offset =
            FIX_BRANCH( F_LBRANCH, CURRENT_PROGRAM_SIZE, $<number>6 + 1);
          add_to_mem_block(A_PROGRAM, $<expression>9.p, $<expression>9.length+2);
          yfree($<expression>9.p);
          offset += $<number>6 + 2 - ( CURRENT_PROGRAM_SIZE - 1 );
          if (offset < -0xff) {
              char * codep;

              if (offset < -0x8000)
                  yyerror("offset overflow");
              codep = mem_block[A_PROGRAM].block + --CURRENT_PROGRAM_SIZE - 1;
              *codep = *codep == F_BBRANCH_WHEN_NON_ZERO - F_OFFSET ?
                F_LBRANCH_WHEN_NON_ZERO - F_OFFSET :
                F_LBRANCH_WHEN_ZERO - F_OFFSET
              ;
              ins_short(offset);
          } else {
              mem_block[A_PROGRAM].block[CURRENT_PROGRAM_SIZE-1] = -offset;
          }
          if ($<expression>9.line != current_line)
              store_line_number_relocation($<expression>9.line);
          for(;current_break_address > 0;current_break_address = next_addr) {
              next_addr = read_short(current_break_address);
              upd_short(current_break_address,
                  CURRENT_PROGRAM_SIZE - current_break_address);
          }
       current_continue_address = $<numbers>3[0];
       current_break_address        = $<numbers>3[1];
   }

for_expr: /* EMPTY */
        {
            last_expression = mem_block[A_PROGRAM].current_size;
            ins_f_byte(F_CONST1);
        }
        | comma_expr;

switch: F_SWITCH '(' comma_expr ')'
    {
        struct s_case_state *statep;
%line
        current_break_stack_need++;
        if ( current_break_stack_need > max_break_stack_need )
            max_break_stack_need = current_break_stack_need;
        if ( !(statep = (struct s_case_state *)
                                yalloc(sizeof(struct s_case_state))) )
        {
            yyerror("Out of memory");
            YYACCEPT;
        }
        *statep = case_state;
        case_state.previous = statep;
        push_explicit(current_break_address);
        push_explicit(switch_pc);
        ins_f_byte(F_SWITCH);
        switch_pc = mem_block[A_PROGRAM].current_size;
        case_state.list0 = case_state.list1 = 0;
        case_state.zero = 0;
        case_state.no_string_labels = 1;
        case_state.some_numeric_labels = 0;
        case_state.default_addr = 0;
        ins_short(0);
        current_break_address =
                BREAK_ON_STACK | BREAK_FROM_SWITCH | CASE_LABELS_ENABLED ;
        if (current_continue_address)
            current_continue_address += SWITCH_DEPTH_UNIT;
    }
      statement
    {
%line
        struct s_case_state *statep;

        current_break_address &=
            ~(BREAK_ON_STACK|BREAK_FROM_SWITCH|CASE_LABELS_ENABLED);

        if (!case_state.default_addr) {
            /* no default given -> create one */
            case_state.default_addr = CURRENT_PROGRAM_SIZE-switch_pc;
        }
        /* it isn't unusual that the last case/default has no break */
        ins_f_byte(F_BREAK);
        store_case_labels(
          CURRENT_PROGRAM_SIZE-switch_pc,
          case_state.default_addr,
          case_state.no_string_labels | case_state.some_numeric_labels,
          case_state.zero,
          yyget_space, yymove_switch_instructions, yyerror, yycerrorl
        );
        switch_pc = pop_address();
        current_break_address = pop_address();
        statep = case_state.previous;
        case_state = *statep;
        yfree((char *)statep);
        if (current_continue_address)
            current_continue_address -= SWITCH_DEPTH_UNIT;
        current_break_stack_need--;
    } ;

case: F_CASE case_label ':'
    {
%line
        struct case_list_entry *temp;

        if ( !( current_break_address & CASE_LABELS_ENABLED ) ) {
            yyerror("Case outside switch");
            break;
        }
        if ( !(temp = new_case_entry()) ) {
            yyerror("Out of memory");
            break;
        }
        if ( !(temp->key = $2.key) ) {
            case_state.zero = temp;
        }
        temp->addr = mem_block[A_PROGRAM].current_size - switch_pc;
        temp->line = current_line;
    }
    | F_CASE case_label F_RANGE case_label ':'
    {
%line
        struct case_list_entry *temp;

        if ( !$2.numeric || !$4.numeric )
            yyerror("String case labels not allowed as range bounds");
        if ( !( current_break_address & CASE_LABELS_ENABLED ) ) {
            yyerror("Case range outside switch");
            break;
        }
        if ($2.key >= $4.key) {
            if ($2.key > $4.key)
                break;
            if ( !(temp = new_case_entry()) ) {
                yyerror("Out of memory");
                break;
            }
            temp->key = $2.key;
            temp->addr = mem_block[A_PROGRAM].current_size - switch_pc;
            temp->line = current_line;
        }
        if ( !(temp = new_case_entry()) ) {
            yyerror("Out of memory");
            break;
        }
        temp->key = $2.key;
        temp->addr = 1;
        temp->line = current_line;
        if ( !(temp = new_case_entry()) ) {
            yyerror("Out of memory");
            break;
        }
        temp->key = $4.key;
        temp->addr = mem_block[A_PROGRAM].current_size - switch_pc;
        temp->line = 0;
    } ;

case_label: constant
        {
%line
            if ( 0 != ($$.key = $1) ) {
                if ( !(case_state.no_string_labels) )
                    yyerror("Mixed case label list not allowed");
                case_state.some_numeric_labels = 1;
            }
            $$.numeric = 1;
        }
          | string_constant
        {
%line
            if ( case_state.some_numeric_labels )
                yyerror("Mixed case label list not allowed");
            case_state.no_string_labels = 0;
            store_prog_string(last_string_constant);
            $$.key = (p_int)last_string_constant;
            $$.numeric = 0;
            last_string_constant = 0;
        }
          ;

constant:
        constant '|'        constant { $$ = $1 |  $3; }
      | constant '^'        constant { $$ = $1 ^  $3; }
      | constant '&'        constant { $$ = $1 &  $3; }
      | constant F_EQ        constant { $$ = $1 == $3; }
      | constant F_NE        constant { $$ = $1 != $3; }
      | constant '>'        constant { $$ = $1 >  $3; }
      | constant F_GE        constant { $$ = $1 >= $3; }
      | constant '<'        constant { $$ = $1 <  $3; }
      | constant F_LE        constant { $$ = $1 <= $3; }
      | constant F_LSH        constant { $$ = (p_uint)$3 > MAX_SHIFT ? 0 : $1 << $3; }
      | constant F_RSH        constant { $$ = $1 >> ((p_uint)$3 > MAX_SHIFT ? MAX_SHIFT : $3); }
      | constant '+'        constant { $$ = $1 +  $3; }
      | constant '-'        constant { $$ = $1 -  $3; }
      | constant '*'        constant { $$ = $1 *  $3; }
      | constant '%'        constant
      {
        if ($3) {
            $$ = $1 % $3;
        } else {
            yyerror("division by zero");
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
      | '(' constant ')' { $$ = $2; } ;
      | '-'   constant %prec '~' { $$ = -$2; }
      | F_NOT constant { $$ = !$2; }
      | '~'   constant { $$ = ~$2; } ;
      | F_NUMBER
      ;

default: F_DEFAULT ':'
    {
%line
        if ( !( current_break_address & CASE_LABELS_ENABLED ) ) {
            yyerror("Default outside switch");
            break;
        }
        if (case_state.default_addr)
            yyerror("Duplicate default");
        case_state.default_addr =
          mem_block[A_PROGRAM].current_size - switch_pc;
    } ;


comma_expr: expr0
        | comma_expr
        {
            insert_pop_value();
        }
        ',' expr0
        { $$.type = $4.type; } ;

expr0:
       lvalue F_ASSIGN expr0 %prec F_ASSIGN
        {
            int length;
            int type2;
%line
            type2 = $3.type;
            if (exact_types && !compatible_types($1.type, type2) &&
                !( $1.type == TYPE_STRING &&
                   (type2 == TYPE_NUMBER || type2 == TYPE_FLOAT) &&
                   $2 == F_ADD_EQ-F_OFFSET))
            {
                type_error("Bad assignment. Rhs", $3.type);
            }
            if (type2 & TYPE_MOD_REFERENCE)
                yyerror("Can't trace reference assignments.");
            length = $1.length;
            if (length) {
                add_to_mem_block
                  (A_PROGRAM, $1.u.p, length+1);
                yfree($1.u.p);
                mem_block[A_PROGRAM].block[
                  last_expression = CURRENT_PROGRAM_SIZE-1
                ] = $2;
            } else {
                char *source, *dest;
                mp_uint current_size;

                source = $1.u.simple;
                current_size = CURRENT_PROGRAM_SIZE;
                CURRENT_PROGRAM_SIZE = (last_expression = current_size + 2) + 1;
                if (current_size + 3 > mem_block[A_PROGRAM].max_size )
                    if (!realloc_a_program()) {
                        yyerror("Out of memory");
                        YYACCEPT;
                    }
                dest = mem_block[A_PROGRAM].block + current_size;
                *dest++ = *source++;
                *dest++ = *source;
                *dest = $2;
            }
            $$.type = type2;
        }
     | error F_ASSIGN expr0  %prec F_ASSIGN
        {   yyerror("Illegal LHS"); $$.type = TYPE_ANY; };

     | expr0 '?' %prec '?'
        {
            ins_f_byte(F_BRANCH_WHEN_ZERO);
            $<number>$ = CURRENT_PROGRAM_SIZE;
            ins_byte(0);
        }
      expr0
        {
            int address, offset;

            address = $<number>3;
            ins_f_byte(F_BRANCH);
            $<number>$ = CURRENT_PROGRAM_SIZE;
            ins_byte(0);
            offset = CURRENT_PROGRAM_SIZE - ( address + 1);
            if (offset > 0xff - 1) {
                int i;
                char *p;

                $<number>$ = CURRENT_PROGRAM_SIZE;
                ins_byte(0);
                p = mem_block[A_PROGRAM].block +
                    mem_block[A_PROGRAM].current_size-1;
                for( i = offset; --i >= 0; --p ) *p = p[-1];
                p[-2] = F_LBRANCH_WHEN_ZERO-F_OFFSET;
                upd_short(address, offset+2);
                if (offset > 0x7ffd)
                    yyerror("offset overflow");
            } else {
                mem_block[A_PROGRAM].block[address] = offset;
            }
        }
      ':' expr0 %prec '?'
        {
            int address, old_address;
            int offset;
            int type1, type2;

            last_expression = -1;
            old_address = $<number>3;
            address = $<number>5;
            offset = mem_block[A_PROGRAM].current_size - ( address + 1);
            if (offset > 0xff) {
                int i;
                char *p;

                ins_byte(0);
                p = mem_block[A_PROGRAM].block +
                    mem_block[A_PROGRAM].current_size-1;
                for( i = offset; --i >= 0; --p ) *p = p[-1];
                p[-2] = F_LBRANCH-F_OFFSET;
                upd_short(address, offset+2);
                if (offset > 0x7ffd)
                    yyerror("offset overflow");
                if ( mem_block[A_PROGRAM].block[old_address-1] ==
                    F_BRANCH_WHEN_ZERO-F_OFFSET )
                    mem_block[A_PROGRAM].block[old_address]++;
                else
                    upd_short(old_address,read_short(old_address)+1);
            } else {
                mem_block[A_PROGRAM].block[address] = offset;
            }
            type1 = $4.type;
            type2 = $7.type;
            if (exact_types && !compatible_types(type1, type2) &&
                instrs[F_CALL_OTHER-F_OFFSET].ret_type != TYPE_ANY)
            {
                type_error("Different types in ?: expr", type1);
                type_error("                      and ", type2);
            }
            if (type1 == TYPE_ANY) $$.type = type2;
            else if (type2 == TYPE_ANY) $$.type = type1;
            else if (type1 == (TYPE_MOD_POINTER|TYPE_ANY) )
                $$.type = type2;
            else if (type2 == (TYPE_MOD_POINTER|TYPE_ANY) )
                $$.type = type1;
            else
                $$.type = type1;
        }

     | expr0 F_LOR %prec F_LOR
        {
            ins_f_byte(F_LOR);
            $<number>$ = CURRENT_PROGRAM_SIZE;
            ins_byte(0);
        }
       expr0
        {
            int address, offset;

            last_expression = -1;
            address = $<number>3;
            offset = mem_block[A_PROGRAM].current_size - ( address + 1);
            if (offset > 0xff) {
                int i;
                char *p;

                ins_short(0);
                ins_byte(0);
                p = mem_block[A_PROGRAM].block +
                    mem_block[A_PROGRAM].current_size-1;
                for( i = offset; --i >= 0; --p ) *p = p[-3];
                p[-4] = F_DUP-F_OFFSET;
                p[-3] = F_LBRANCH_WHEN_NON_ZERO-F_OFFSET;
                upd_short(address+1, offset+3);
                if (offset > 0x7ffc)
                    yyerror("offset overflow");
                p[0]  = F_POP_VALUE-F_OFFSET;
            } else {
                mem_block[A_PROGRAM].block[address] = offset;
            }
            if ($1.type == $4.type)
                $$.type = $1.type;
            else
                $$.type = TYPE_ANY;        /* Return type can't be known */
        }

     | expr0 F_LAND %prec F_LAND
        {
            ins_f_byte(F_LAND);
            $<number>$ = CURRENT_PROGRAM_SIZE;
            ins_byte(0);
        }
       expr0
        {
            int address, offset;

            last_expression = -1;
            address = $<number>3;
            offset = mem_block[A_PROGRAM].current_size - ( address + 1);
            if (offset > 0xff) {
                int i;
                char *p;

                ins_short(0);
                ins_byte(0);
                p = mem_block[A_PROGRAM].block +
                    mem_block[A_PROGRAM].current_size-1;
                for( i = offset; --i >= 0; --p ) *p = p[-3];
                p[-4] = F_DUP-F_OFFSET;
                p[-3] = F_LBRANCH_WHEN_ZERO-F_OFFSET;
                upd_short(address+1, offset+3);
                if (offset > 0x7ffc)
                    yyerror("offset overflow");
                p[0]  = F_POP_VALUE-F_OFFSET;
            } else {
                mem_block[A_PROGRAM].block[address] = offset;
            }
            if ($1.type == $4.type)
                $$.type = $1.type;
            else
                $$.type = TYPE_ANY;        /* Return type can't be known */
        } ;

       | expr0 '|' expr0
          {
              if (exact_types && !BASIC_TYPE($1.type,TYPE_NUMBER))
                  type_error("Bad argument 1 to |", $1.type);
              if (exact_types && !BASIC_TYPE($3.type,TYPE_NUMBER))
                  type_error("Bad argument 2 to |", $3.type);
              $$.type = TYPE_NUMBER;
              ins_f_byte(F_OR);
          }

       | expr0 '^' expr0
          {
              if (exact_types && !BASIC_TYPE($1.type,TYPE_NUMBER))
                  type_error("Bad argument 1 to ^", $1.type);
              if (exact_types && !BASIC_TYPE($3.type,TYPE_NUMBER))
                  type_error("Bad argument 2 to ^", $3.type);
              $$.type = TYPE_NUMBER;
              ins_f_byte(F_XOR);
          }

        | expr0 '&' expr0
        {
            ins_f_byte(F_AND);
            $$.type = TYPE_ANY;
            if (exact_types) {
                short first_type  = $1.type;
                short second_type = $3.type;

                if ( first_type == TYPE_ANY &&
                    second_type == TYPE_ANY )
                {
                    /* $$ == TYPE_ANY is correct */
                } else if ( (first_type | second_type) & TYPE_MOD_POINTER) {
                    if (first_type  == TYPE_NUMBER ||
                        second_type == TYPE_NUMBER)
                    {
                        yyerrorf("Incompatible types for arguments to & %s"
                          ,get_two_types(first_type, second_type));
                    } else if ( ( !( first_type  & TYPE_MOD_POINTER ) ||
                                  first_type  & TYPE_MOD_REFERENCE) &&
                                first_type  != TYPE_ANY              ) {
                        type_error("Bad argument 1 to &", first_type );
                    } else if ( ( !( second_type & TYPE_MOD_POINTER ) ||
                                  second_type & TYPE_MOD_REFERENCE) &&
                                second_type != TYPE_ANY               ) {
                        type_error("Bad argument 2 to &", first_type );
                    } else if ( !BASIC_TYPE(first_type &~TYPE_MOD_POINTER,
                                           second_type &~TYPE_MOD_POINTER) )
                    {
                        yyerrorf("Incompatible types for arguments to & %s"
                          ,get_two_types(first_type, second_type));
                    } else {
                        $$.type = TYPE_ANY | TYPE_MOD_POINTER;
                    }
                } else {
                    if ( !BASIC_TYPE(first_type ,TYPE_NUMBER) )
                        type_error("Bad argument 1 to &", first_type );
                    if ( !BASIC_TYPE(second_type,TYPE_NUMBER) )
                        type_error("Bad argument 2 to &", second_type);
                    $$.type = TYPE_NUMBER;
                }
            } /* end of exact_types code */
        } /* end of '&' code */

      | expr0 F_EQ expr0
        {
            int t1 = $1.type, t2 = $3.type;
            if (exact_types && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY) {
                type_error("== always false because of different types",
                  $1.type);
                type_error("                               compared to",
                  $3.type);
            }
            ins_f_byte(F_EQ);
            $$.type = TYPE_NUMBER;
        }
      | expr0 F_NE expr0
        {
            int t1 = $1.type, t2 = $3.type;
            if (exact_types && t1 != t2 && t1 != TYPE_ANY && t2 != TYPE_ANY) {
                type_error("!= always true because of different types",
                  $1.type);
                type_error("                              compared to",
                  $3.type);
            }
            ins_f_byte(F_NE);
            $$.type = TYPE_NUMBER;
        }

      | expr0 '>' expr0
        { $$.type = TYPE_NUMBER; ins_f_byte(F_GT); }
      | expr0 F_GE expr0
        { $$.type = TYPE_NUMBER; ins_f_byte(F_GE); }
      | expr0 '<' expr0
        { $$.type = TYPE_NUMBER; ins_f_byte(F_LT); }
      | expr0 F_LE expr0
        { $$.type = TYPE_NUMBER; ins_f_byte(F_LE); }

      | expr0 F_LSH expr0
        {
            ins_f_byte(F_LSH);
            $$.type = TYPE_NUMBER;
            if (exact_types) {
                if (!BASIC_TYPE($1.type, TYPE_NUMBER))
                    type_error("Bad argument number 1 to '<<'", $1.type);
                if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                    type_error("Bad argument number 2 to '<<'", $3.type);
            }
        }
      | expr0 F_RSH expr0
        {
            ins_f_byte(F_RSH);
            $$.type = TYPE_NUMBER;
            if (exact_types) {
                if (!BASIC_TYPE($1.type, TYPE_NUMBER))
                    type_error("Bad argument number 1 to '>>'", $1.type);
                if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                    type_error("Bad argument number 2 to '>>'", $3.type);
            }
        };

      | expr0 '+'
        {
%line
            $<numbers>$[0] = last_expression;
            $<numbers>$[1] = last_string_is_new;
        }
        expr0
        {
            /* Type checks of this case are complicated */
            mp_uint current_size;
            unsigned char *p;
%line
            if (pragma_combine_strings &&
                last_expression + 2 == (current_size = CURRENT_PROGRAM_SIZE) &&
                $<numbers>3[0] + 4 == (mp_int)current_size &&
                (((p = &mem_block[A_PROGRAM].block[current_size])[-2] -
                  (F_CSTRING0 - F_OFFSET)) & ~3) == 0 &&
                ((p[-4] - (F_CSTRING0 - F_OFFSET)) & ~3) == 0
            ) {
                char *str1, *str2, *sum;
                int i;

                str1 = ((char**)(mem_block[A_STRINGS].block))
                  [p[-3] | (p[-4]-(F_CSTRING0-F_OFFSET))<<8 ];
                str2 = ((char**)(mem_block[A_STRINGS].block))
                  [p[-1] | (p[-2]-(F_CSTRING0-F_OFFSET))<<8 ];
                sum = xalloc(strlen(str1) + strlen(str2) + 1);
                strcpy(sum, str1);
                strcat(sum, str2);
                if (last_string_is_new) {
                    delete_prog_string();
                }
                if ($<numbers>3[1]) {
                    delete_prog_string();
                }
                i = store_prog_string(make_shared_string(sum));
                xfree(sum);
                last_expression = current_size - 4;
                if (i < 0x400) {
                    p[-4] = F_CSTRING0 - F_OFFSET + (i>>8);
                    p[-3] = i;
                    CURRENT_PROGRAM_SIZE = current_size - 2;
                } else {
                    p[-4] = F_STRING-F_OFFSET;
                    upd_short(current_size - 3, i);
                    CURRENT_PROGRAM_SIZE = current_size - 1;
                }
                $$.type = TYPE_STRING;
            } else {
                ins_f_byte(F_ADD);
                $$.type = TYPE_ANY;
            }
        };
      | expr0 '-' expr0
        {
%line
            $$.type = TYPE_ANY;
            if (exact_types) {
                int type1 = $1.type;
                int type2 = $3.type;

                if (type1 == type2) {
                    static char matchok[] =
%typemap TYPE_ANY:1,TYPE_NUMBER:1,TYPE_FLOAT:1,TYPE_MAPPING:1

                    if ( type1 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE) ?
                           (type1 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                             TYPE_MOD_POINTER :
                           matchok[type1]
                       )
                    {
                        $$.type = type1;
                    } else {
                        type_error("Bad arguments to '-'", type1);
                    }
                } else if ( (type1 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                    TYPE_MOD_POINTER)
                {

                    if ( (type2 | TYPE_MOD_POINTER) ==
                         (TYPE_MOD_POINTER|TYPE_ANY)   ||
                         (type2 & TYPE_MOD_POINTER &&
                         type1 == (TYPE_MOD_POINTER|TYPE_ANY))
                       )
                    {
                        $$.type = type1;
                    } else {
                        yyerror("Arguments to '-' don't match");
                    }
                } else switch (type1) {
                  case TYPE_ANY:
                    switch (type2) {
                      case TYPE_NUMBER:
                        /* number or float -> TYPE_ANY */
                        break;
                      case TYPE_MAPPING:
                      case TYPE_FLOAT:
                        $$.type = type2;
                        break;
                      default:
                        if ( (type2 & (TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                             TYPE_MOD_POINTER)
                        {
                            $$.type = TYPE_ANY | TYPE_MOD_POINTER;
                            break;
                        } else {
                            type_error("Bad argument number 2 to '-'", type2);
                            break;
                        }
                    }
                    break;
                  case TYPE_NUMBER:
                    if (type2 == TYPE_FLOAT || type2 == TYPE_ANY) {
                        $$.type = type2;
                    } else {
                        yyerror("Arguments to '-' don't match");
                    }
                    break;
                  case TYPE_FLOAT:
                    if (type2 == TYPE_NUMBER || type2 == TYPE_ANY) {
                        $$.type = TYPE_FLOAT;
                    } else {
                        yyerror("Arguments to '-' don't match");
                    }
                    break;
                  case TYPE_MAPPING:
                    if (type2 == TYPE_ANY) {
                        $$.type = type1;
                    } else {
                        yyerror("Arguments to '-' don't match");
                    }
                    break;
                  default:
                    type_error("Bad argument number 1 to '-'", type1);
                    break;
                }
            }
            ins_f_byte(F_SUBTRACT);
        };

      | expr0 '*' expr0
        {
            int type1, type2;

            type1 = $1.type;
            type2 = $3.type;
            if (exact_types) {
                if ( !BASIC_TYPE(type1, TYPE_NUMBER) && type1 != TYPE_FLOAT)
                    type_error("Bad argument number 1 to '*'", type1);
                if ( !BASIC_TYPE(type2, TYPE_NUMBER) && type2 != TYPE_FLOAT)
                    type_error("Bad argument number 2 to '*'", type2);
            }
            ins_f_byte(F_MULTIPLY);
            if (type1 == TYPE_FLOAT || type2 == TYPE_FLOAT )
            {
                $$.type = TYPE_FLOAT;
            } else {
                $$.type = TYPE_NUMBER;
            }
        };
      | expr0 '%' expr0
        {
            if (exact_types) {
                if (!BASIC_TYPE($1.type, TYPE_NUMBER))
                    type_error("Bad argument number 1 to '%'", $1.type);
                if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                    type_error("Bad argument number 2 to '%'", $3.type);
            }
            ins_f_byte(F_MOD);
            $$.type = TYPE_NUMBER;
        };
      | expr0 '/' expr0
        {
            int type1, type2;

            type1 = $1.type;
            type2 = $3.type;
            if (exact_types) {
                if ( !BASIC_TYPE(type1, TYPE_NUMBER) && type1 != TYPE_FLOAT)
                    type_error("Bad argument number 1 to '/'", type1);
                if ( !BASIC_TYPE(type2, TYPE_NUMBER) && type2 != TYPE_FLOAT)
                    type_error("Bad argument number 2 to '/'", type2);
            }
            ins_f_byte(F_DIVIDE);
            if (type1 == TYPE_FLOAT || type2 == TYPE_FLOAT )
            {
                $$.type = TYPE_FLOAT;
            } else {
                $$.type = TYPE_NUMBER;
            }
        };

      | decl_cast expr0 %prec '~'
              {
                  $$.type = $1;
                  if (exact_types && $2.type != TYPE_ANY &&
                      $2.type != TYPE_UNKNOWN && $1 != TYPE_VOID)
                      type_error("Casts are only legal for type mixed, or when unknown", $2.type);
              } ;

      | cast expr0 %prec '~'
              {
                  $$.type = $1;
                  if ($2.type != TYPE_ANY &&
                      $2.type != TYPE_UNKNOWN && $1 != TYPE_VOID) {
                      switch($1) {
                        default:
                          type_error("Illegal cast", $1);
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
              } ;

      | pre_inc_dec F_IDENTIFIER %prec F_INC
        {
            int i;
            PREPARE_S_INSERT(4)
%line
            i = verify_declared($2);
            if (i != -1) {
                if (i & VIRTUAL_VAR_TAG) {
                    add_f_byte(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
                    add_byte(i);
                    i = V_VARIABLE(i)->flags & TYPE_MOD_MASK;
                } else {
                    if ((i + num_virtual_variables) & ~0xff) {
                        add_f_byte(F_PUSH_IDENTIFIER16_LVALUE);
                        add_short(i + num_virtual_variables);
                    } else {
                        add_f_byte(F_PUSH_IDENTIFIER_LVALUE);
                        add_byte(i + num_virtual_variables);
                    }
                    i = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
                }
                if (exact_types && !BASIC_TYPE(i, TYPE_NUMBER)) {
                    argument_type_error($1, i);
                }
            }
            CURRENT_PROGRAM_SIZE =
              (last_expression = CURRENT_PROGRAM_SIZE + 2) + 1;
            add_byte($1);
            $$.type = TYPE_NUMBER;
        }
      | pre_inc_dec F_LOCAL %prec F_INC
        {
            int i;
            PREPARE_INSERT(3)
%line
            add_f_byte(F_PUSH_LOCAL_VARIABLE_LVALUE);
            add_byte($2);
            CURRENT_PROGRAM_SIZE =
              (last_expression = CURRENT_PROGRAM_SIZE + 2) + 1;
            add_byte($1);
            i = type_of_locals[$2];
            if (exact_types && !BASIC_TYPE(i, TYPE_NUMBER)) {
                argument_type_error($1, i);
            }
            $$.type = TYPE_NUMBER;
        }
      | pre_inc_dec expr4 '[' expr0 ']' %prec '['
        {
            /* the ',' operator is reserved for indexing on multi-valued
             * mappings and other multi-dimensional data
             */
            mp_uint current;
            char *p;
            int start;
%line
            if ($4.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $2.type;
                switch (type) {
                  default:
                    if (type & TYPE_MOD_POINTER)
                        argument_type_error($1, type);
                    else
                        type_error("Bad type to indexed lvalue", type);
                    break;
                  case TYPE_ANY:
#ifdef MAPPINGS
                  case TYPE_MAPPING:
                    break;
#endif
                  case TYPE_STRING:
                  case TYPE_MOD_POINTER|TYPE_ANY:
                  case TYPE_MOD_POINTER|TYPE_NUMBER:
                    if (!BASIC_TYPE($4.type, TYPE_NUMBER))
                        type_error("Bad type of index", $4.type);
                  ;
                }
            }
            current = CURRENT_PROGRAM_SIZE;
            start = $2.start;
            if ($2.code >= 0) {
                if ($2.end) {
                    int length;
                    char *q;

                    length = $2.end - start + 1;
                    if (current+length > mem_block[A_PROGRAM].max_size)
                        realloc_a_program();
                    p = mem_block[A_PROGRAM].block;
                    memcpy(
                        p + current,
                        p + start,
                        length
                    );
                    p += start;
                    q = p + length;
                    length = current - start;
                    for( ; --length >= 0; ) *p++ = *q++;
                    if ($2.code == F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET)
                        p[-3] = $2.code;
                    else
                        p[-1] = $2.code;
                    *p++ = F_INDEX_LVALUE - F_OFFSET;
                } else {
                    int i;
                    int length;

                    if (current + 2 > mem_block[A_PROGRAM].max_size)
                        realloc_a_program();
                    p = mem_block[A_PROGRAM].block + start;
                    i = p[1];
                    length = current - start - 2;
                    for( ; --length >= 0; p++) *p = p[2];
                    *p++ = $2.code;
                    *p++ = i;
                    *p++ = F_INDEX_LVALUE - F_OFFSET;
                }
            } else {
                if (current + 2 > mem_block[A_PROGRAM].max_size)
                    realloc_a_program();
                p = mem_block[A_PROGRAM].block + start;
                *p++ = F_PUSH_INDEXED_LVALUE - F_OFFSET;
            }
            *p = $1;
            last_expression = current + 1;
            CURRENT_PROGRAM_SIZE = current + 2;
            $$.type = TYPE_NUMBER;
        };
      | pre_inc_dec expr4 '[' '<' expr0 ']' %prec '['
        {
            /* the ',' operator is reserved for indexing on multi-valued
             * mappings and other multi-dimensional data
             */
            mp_uint current;
            char *p;
            int start;
%line
            if (exact_types) {
                int type;

                type = $2.type;
                if (type & TYPE_MOD_POINTER) {
                    if (type != (TYPE_MOD_POINTER|TYPE_ANY) &&
                        type != (TYPE_MOD_POINTER|TYPE_NUMBER) )
                        argument_type_error($1, type);
                } else switch (type) {
                  default:
                    type_error("Bad type to indexed lvalue", type);
                  case TYPE_STRING:
                  case TYPE_ANY:
                    if (!BASIC_TYPE($5.type, TYPE_NUMBER))
                        type_error("Bad type of index", $5.type);
                  ;
                }
            }
            current = CURRENT_PROGRAM_SIZE;
            start = $2.start;
            if ($2.code >= 0) {
                if ($2.end) {
                    int length;
                    char *q;

                    length = $2.end - start + 1;
                    if (current+length > mem_block[A_PROGRAM].max_size)
                        realloc_a_program();
                    p = mem_block[A_PROGRAM].block;
                    memcpy(
                        p + current,
                        p + start,
                        length
                    );
                    p += start;
                    q = p + length;
                    length = current - start;
                    for( ; --length >= 0; ) *p++ = *q++;
                    if ($2.code == F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET)
                        p[-3] = $2.code;
                    else
                        p[-1] = $2.code;
                    *p++ = F_RINDEX_LVALUE - F_OFFSET;
                } else {
                    int i;
                    int length;

                    if (current + 2 > mem_block[A_PROGRAM].max_size)
                        realloc_a_program();
                    p = mem_block[A_PROGRAM].block + start;
                    i = p[1];
                    length = current - start - 2;
                    for( ; --length >= 0; p++) *p = p[2];
                    *p++ = $2.code;
                    *p++ = i;
                    *p++ = F_RINDEX_LVALUE - F_OFFSET;
                }
            } else {
                if (current + 2 > mem_block[A_PROGRAM].max_size)
                    realloc_a_program();
                p = mem_block[A_PROGRAM].block + start;
                *p++ = F_PUSH_RINDEXED_LVALUE - F_OFFSET;
            }
            *p = $1;
            last_expression = current + 1;
            CURRENT_PROGRAM_SIZE = current + 2;
            $$.type = TYPE_NUMBER;
        };
      | F_NOT expr0
        {
            last_expression = CURRENT_PROGRAM_SIZE;
            ins_f_byte(F_NOT);        /* Any type is valid here. */
            $$.type = TYPE_NUMBER;
        };
      | '~' expr0
        {
            ins_f_byte(F_COMPL);
            if (exact_types && !BASIC_TYPE($2.type, TYPE_NUMBER))
                type_error("Bad argument to ~", $2.type);
            $$.type = TYPE_NUMBER;
        };
      | '-' expr0 %prec '~'
        {
            int type;

            if (CURRENT_PROGRAM_SIZE - last_expression == 2 &&
                mem_block[A_PROGRAM].block[last_expression] ==
                  F_CLIT - F_OFFSET )
            {
                mem_block[A_PROGRAM].block[last_expression] =
                  F_NCLIT - F_OFFSET;
            } else {
                ins_f_byte(F_NEGATE);
            }
            type = $2.type;
            $$.type = type;
            if (exact_types && !BASIC_TYPE(type, TYPE_NUMBER) &&
                type != TYPE_FLOAT )
                type_error("Bad argument to unary '-'", $2.type);
        };

      | lvalue F_INC %prec F_INC
        {
            if ($1.length) {
                add_to_mem_block(A_PROGRAM, $1.u.p, $1.length);
                yfree($1.u.p);
                last_expression = CURRENT_PROGRAM_SIZE;
                ins_f_byte(F_POST_INC);
            } else {
                PREPARE_INSERT(3)
                char *source;

                CURRENT_PROGRAM_SIZE =
                  (last_expression = CURRENT_PROGRAM_SIZE+2) + 1;
                source = $1.u.simple;
                add_byte(*source++);
                add_byte(*source);
                add_f_byte(F_POST_INC);
            }
            if (exact_types && !BASIC_TYPE($1.type, TYPE_NUMBER))
                type_error("Bad argument to ++", $1.type);
            $$.type = TYPE_NUMBER;
        };
      | lvalue F_DEC %prec F_DEC
        {
            if ($1.length) {
                add_to_mem_block(A_PROGRAM, $1.u.p, $1.length+1);
                yfree($1.u.p);
                mem_block[A_PROGRAM].block[
                  last_expression = CURRENT_PROGRAM_SIZE-1
                ] = F_POST_DEC - F_OFFSET;
            } else {
                PREPARE_INSERT(3)
                char *source;

                CURRENT_PROGRAM_SIZE =
                  (last_expression = CURRENT_PROGRAM_SIZE+2) + 1;
                source = $1.u.simple;
                add_byte(*source++);
                add_byte(*source);
                add_f_byte(F_POST_DEC);
            }
            if (exact_types && !BASIC_TYPE($1.type, TYPE_NUMBER))
                type_error("Bad argument to --", $1.type);
            $$.type = TYPE_NUMBER;
        };

     | expr4 ;

note_start: { $$.start = CURRENT_PROGRAM_SIZE; }

expr4: function_call %prec '~'
%//  | F_STRING F_STRING
%//        { fatal("presence of rule should prevent its reduction"); }
     | F_STRING
        {
            int string_number;
            PREPARE_S_INSERT(3)
            char *p;
%line
            p = last_lex_string;
            last_lex_string = 0;
            $$.start = last_expression = CURRENT_PROGRAM_SIZE;
            $$.type = TYPE_STRING;
            $$.code = -1;
            string_number = store_prog_string(p);
            if ( string_number <= 0xff ) {
                add_f_byte(F_CSTRING0);
                add_byte(string_number);
            } else if ( string_number <= 0x1ff ) {
                add_f_byte(F_CSTRING1);
                add_byte(string_number);
            } else if ( string_number <= 0x2ff ) {
                add_f_byte(F_CSTRING2);
                add_byte(string_number);
            } else if ( string_number <= 0x3ff ) {
                add_f_byte(F_CSTRING3);
                add_byte(string_number);
            } else {
                add_f_byte(F_STRING);
                add_short(string_number);
                CURRENT_PROGRAM_SIZE++;
            }
            CURRENT_PROGRAM_SIZE += 2;
        };
     | F_NUMBER
        {
            int current;
            int number;
            PREPARE_INSERT(1 + sizeof (p_int))
%line
            $$.start = last_expression = current = CURRENT_PROGRAM_SIZE;
            $$.code = -1;
            number = $1;
            if ( number == 0 ) {
                current++;
                add_f_byte(F_CONST0);
                $$.type = TYPE_ANY;
            } else if ( number == 1 ) {
                add_f_byte(F_CONST1);
                current++;
                $$.type = TYPE_NUMBER;
            } else if ( number >= 0 && number <= 0xff ) {
                add_f_byte(F_CLIT);
                add_byte(number);
                current += 2;
                $$.type = TYPE_NUMBER;
            } else {
                add_f_byte(F_NUMBER);
                memcpy(__PREPARE_INSERT__p, (char*)&$1, sizeof $1);
                current += 1 + sizeof (p_int);
                $$.type = TYPE_NUMBER;
            }
            CURRENT_PROGRAM_SIZE = current;
        } ;
     | F_CLOSURE
        {
            int ix;

            $$.start = CURRENT_PROGRAM_SIZE;
            $$.code = -1;
            ix = $1.number;
            ins_f_byte(F_CLOSURE);
            ins_short(ix);
            $$.type = TYPE_CLOSURE;
        } ;
     | F_SYMBOL
        {
            int string_number;
            int quotes;

            $$.start = CURRENT_PROGRAM_SIZE;
            $$.code = -1;
            quotes = $1.quotes;
            string_number = store_prog_string($1.name);
            if (quotes == 1 && string_number < 0x100) {
                ins_f_byte(F_CSTRING0);
                ins_byte(string_number);
                ins_f_byte(F_QUOTE);
            } else {
                ins_f_byte(F_SYMBOL);
                ins_short(string_number);
                ins_byte(quotes);
            }
            $$.type = TYPE_SYMBOL;
        } ;
%ifdef FLOATS
     | F_FLOAT
        {
            int exponent;

            $$.start = CURRENT_PROGRAM_SIZE;
            $$.code = -1;
            ins_f_byte(F_FLOAT);
            ins_long ( SPLIT_DOUBLE( $1, &exponent) );
            ins_short( exponent );
            $$.type = TYPE_FLOAT;
        } ;
%endif /* FLOATS */
     | '(' note_start comma_expr ')'         %prec '~'
        {
            $$.type = $3.type;
            $$.start = $2.start;
            $$.code = -1;
        }
     | catch                         %prec '~'
     | sscanf                         %prec '~'
%ifdef SUPPLY_PARSE_COMMAND
     | parse_command                 %prec '~'
%endif /* SUPPLY_PARSE_COMMAND */
     | '(' '{' note_start expr_list '}' ')' %prec '~'
        {
            check_aggregate_types($4);        /* We don't care about these types,
                                         * unless a reference appears */
            ins_f_byte(F_AGGREGATE);
            ins_short($4);
            if ($4 > MAX_ARRAY_SIZE)
                yyerror("Illegal array size");
            $$.type = TYPE_MOD_POINTER | TYPE_ANY;
            $$.start = $3.start;
            $$.code = -1;
        }
     | F_QUOTED_AGGREGATE note_start expr_list '}' ')' %prec '~'
        {
            int quotes;

            check_aggregate_types($3);        /* We don't care about these types,
                                         * unless a reference appears */
            ins_f_byte(F_AGGREGATE);
            ins_short($3);
            if ($3 > MAX_ARRAY_SIZE)
                yyerror("Illegal array size");
            $$.type = TYPE_QUOTED_ARRAY;
            $$.start = $2.start;
            $$.code = -1;
            quotes = $1;
            do {
                ins_f_byte(F_QUOTE);
            } while (--quotes);
        }
%ifdef MAPPINGS
     | '(' '[' note_start m_expr_list ']' ')'
        {
            mp_int num_keys;

            check_aggregate_types($4[0]);
            num_keys = $4[0] / ($4[1]+1);
            if ((num_keys|$4[1]) & ~0xffff)
                yyerror("cannot handle more than 65525 keys/values "
                        "in mapping aggregate");
            if ( (num_keys | $4[1]) &~0xff) {
                ins_f_byte(F_M_AGGREGATE);
                ins_short(num_keys);
                ins_short($4[1]);
            } else {
                ins_f_byte(F_M_CAGGREGATE);
                ins_byte(num_keys);
                ins_byte($4[1]);
            }
            $$.type = TYPE_MAPPING;
            $$.start = $3.start;
            $$.code = -1;
        }
%endif
     | expr4 '[' expr0 F_RANGE expr0 ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.code = -1;
            ins_f_byte(F_RANGE);
            if (exact_types) {
                int type;

                $$.type = type = $1.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $3.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
     | expr4 '[' expr0 F_RANGE '<' expr0 ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.code = -1;
            ins_f_code(F_NR_RANGE);
            if (exact_types) {
                int type;

                $$.type = type = $1.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $3.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $6.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
     | expr4 '[' '<' expr0 F_RANGE expr0 ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.code = -1;
            ins_f_code(F_RN_RANGE);
            if (exact_types) {
                int type;

                $$.type = type = $1.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $4.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $6.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
     | expr4 '[' '<' expr0 F_RANGE '<' expr0 ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.code = -1;
            ins_f_code(F_RR_RANGE);
            if (exact_types) {
                int type;

                $$.type = type = $1.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $4.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $7.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
     | expr4 '[' expr0 F_RANGE ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.code = -1;
            ins_f_byte(F_EXTRACT2);
            if (exact_types) {
                int type;

                $$.type = type = $1.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $3.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
     | expr4 '[' '<' expr0 F_RANGE ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.code = -1;
            if (CURRENT_PROGRAM_SIZE - last_expression == 2 &&
                mem_block[A_PROGRAM].block[last_expression] ==
                  F_CLIT - F_OFFSET )
            {
                mem_block[A_PROGRAM].block[last_expression] =
                  F_NCLIT - F_OFFSET;
            } else {
                ins_f_byte(F_NEGATE);
            }
            ins_f_byte(F_EXTRACT2);
            if (exact_types) {
                int type;

                $$.type = type = $1.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $4.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
     | '&' F_IDENTIFIER                        %prec '~'
        {
            int i;
            mp_uint current;
            char *p;
%line
            i = verify_declared($2);
            $$.start = current = CURRENT_PROGRAM_SIZE;
            $$.code = -1;
            if (current + 3 > mem_block[A_PROGRAM].max_size)
                realloc_a_program();
            p = mem_block[A_PROGRAM].block + current;
            if (i & VIRTUAL_VAR_TAG) {
                *p++ = F_PUSH_VIRTUAL_VARIABLE_LVALUE - F_OFFSET;
                *p = i;
            } else {
                if ((i + num_virtual_variables) & ~0xff) {
                    *p = F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET;
                    upd_short(++current, i + num_virtual_variables);
                } else {
                    *p++ = F_PUSH_IDENTIFIER_LVALUE - F_OFFSET;
                    *p = i + num_virtual_variables;
                }
            }
            CURRENT_PROGRAM_SIZE = current + 2;
            if (i == -1)
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            else
                $$.type = (VARIABLE(i)->flags & TYPE_MOD_MASK) |
                  TYPE_MOD_REFERENCE;
        };
     | '&' F_LOCAL                        %prec '~'
        {
            mp_uint current;
            char *p;
%line
            $$.start = current = CURRENT_PROGRAM_SIZE;
            $$.code = -1;
            if (current + 2 > mem_block[A_PROGRAM].max_size)
                realloc_a_program();
            p = mem_block[A_PROGRAM].block + current;
            *p++ = F_PUSH_LOCAL_VARIABLE_LVALUE - F_OFFSET;
            *p = $2;
            CURRENT_PROGRAM_SIZE = current + 2;
            $$.type = type_of_locals[$2] | TYPE_MOD_REFERENCE;
        };
     | '&' '(' expr4 '[' expr0 ']' ')'        %prec '~'
        {
%line
            /* the ',' operator is reserved for indexing on multi-valued
             * mappings and other multi-dimensional data
             */
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_INDEX_LVALUE - F_OFFSET
            );
            $$.start = $3.start;
            $$.code = -1;
            if ($5.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (!exact_types) {
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            } else {
                int type;

                type = $3.type;
                if (type & TYPE_MOD_POINTER) {
                    $$.type = type & ~TYPE_MOD_POINTER;
                } else switch (type) {
                  default:
                    type_error("Bad type to indexed reference", type);
                  case TYPE_ANY:
#ifdef MAPPINGS
                  case TYPE_MAPPING:
                    $5.type = TYPE_ANY;
#endif
                    $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
                    break;
                  case TYPE_STRING:
                    $$.type = TYPE_NUMBER | TYPE_MOD_REFERENCE;
                    break;
                }
                if (!BASIC_TYPE($5.type, TYPE_NUMBER))
                    type_error("Bad type of index", $5.type);
            }
        };
     | '&' '(' expr4 '[' '<' expr0 ']' ')'        %prec '~'
        {
%line
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RINDEX_LVALUE - F_OFFSET
            );
            $$.start = $3.start;
            $$.code = -1;
            if (!exact_types) {
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            } else {
                int type;

                type = $3.type;
                if (type & TYPE_MOD_POINTER) {
                    $$.type = type & ~TYPE_MOD_POINTER;
                } else switch (type) {
                  default:
                    type_error("Bad type to indexed reference", type);
                  case TYPE_ANY:
                    $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
                    break;
                  case TYPE_STRING:
                    $$.type = TYPE_NUMBER | TYPE_MOD_REFERENCE;
                    break;
                }
                if (!BASIC_TYPE($6.type, TYPE_NUMBER))
                    type_error("Bad type of index", $6.type);
            }
        };
%ifdef MAPPINGS
        | '&' '(' expr4 '[' expr0 ',' expr0 ']' ')'
        {
%line
            $$.start = $3.start;
            $$.code = -1;
            $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            ins_f_code(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);
            if ($5.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $3.type;
                if (type != TYPE_ANY && type != TYPE_MAPPING)
                {
                    type_error("Bad type to indexed value", type);
                }
                type = $7.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
%endif /* MAPPINGS */
     | '&' '(' expr4 '[' expr0 F_RANGE expr0 ']' ')'        %prec '~'
        {
%line
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RANGE_LVALUE - F_OFFSET
            );
            $$.start = $3.start;
            $$.code = -1;
            if (!exact_types) {
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            } else {
                int type;

                $$.type = type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $7.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
     | '&' '(' expr4 '[' expr0 F_RANGE '<' expr0 ']' ')'        %prec '~'
        {
%line
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_NR_RANGE_LVALUE - F_OFFSET
            );
            $$.start = $3.start;
            $$.code = -1;
            if (!exact_types) {
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            } else {
                int type;

                $$.type = type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $8.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
     | '&' '(' expr4 '[' '<' expr0 F_RANGE expr0 ']' ')'        %prec '~'
        {
%line
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RN_RANGE_LVALUE - F_OFFSET
            );
            $$.start = $3.start;
            $$.code = -1;
            if (!exact_types) {
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            } else {
                int type;

                $$.type = type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $6.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $8.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
     | '&' '(' expr4 '[' '<' expr0 F_RANGE '<' expr0 ']' ')'        %prec '~'
        {
%line
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RR_RANGE_LVALUE - F_OFFSET
            );
            $$.start = $3.start;
            $$.code = -1;
            if (!exact_types) {
                $$.type = TYPE_ANY | TYPE_MOD_REFERENCE;
            } else {
                int type;

                $$.type = type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = $6.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $9.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
%// The following expressions can be patched to lvalues for use in index_lvalue.
     | F_IDENTIFIER
        {
            int i;
            mp_uint current;
            char *p;
%line
            i = verify_declared($1);
            $$.start = current = CURRENT_PROGRAM_SIZE;
            $$.end = 0;
            if (current + 3 > mem_block[A_PROGRAM].max_size)
                realloc_a_program();
            p = mem_block[A_PROGRAM].block + current;
            if (i & VIRTUAL_VAR_TAG) {
                $$.code = F_PUSH_VIRTUAL_VARIABLE_LVALUE - F_OFFSET;
                *p++ = F_VIRTUAL_VARIABLE - F_OFFSET;
                *p = i;
                $$.type = V_VARIABLE(i)->flags & TYPE_MOD_MASK;
            } else {
                if ((i + num_virtual_variables) & ~0xff) {
                    $$.code = F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET;
                    *p = F_IDENTIFIER16 - F_OFFSET;
                    upd_short(++current, i + num_virtual_variables);
                    $$.end = current+1;
                } else {
                    $$.code = F_PUSH_IDENTIFIER_LVALUE - F_OFFSET;
                    *p++ = F_IDENTIFIER - F_OFFSET;
                    *p = i + num_virtual_variables;
                }
                $$.type = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
            }
            CURRENT_PROGRAM_SIZE = current + 2;
            if (i == -1)
                $$.type = TYPE_ANY;
        }
     | F_LOCAL
        {
            mp_uint current;
            char *p;
%line
            $$.start = current = CURRENT_PROGRAM_SIZE;
            $$.code = F_PUSH_LOCAL_VARIABLE_LVALUE - F_OFFSET;
            $$.end = 0;
            if (current + 2 > mem_block[A_PROGRAM].max_size)
                realloc_a_program();
            p = mem_block[A_PROGRAM].block + current;
            *p++ = F_LOCAL - F_OFFSET;
            *p = $1;
            CURRENT_PROGRAM_SIZE = current + 2;
            $$.type = type_of_locals[$1];
        }
     | expr4 '[' expr0 ']' %prec '['
        {
%line
            /* the ',' operator is reserved for indexing on multi-valued
             * mappings and other multi-dimensional data
             */
            $$.start = $1.start;
            $$.end = CURRENT_PROGRAM_SIZE;
            $$.code = F_PUSH_INDEXED_LVALUE - F_OFFSET;
            ins_f_byte(F_INDEX);
            if ($3.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $1.type;
                if (type & TYPE_MOD_POINTER) {
                    $$.type = type & ~TYPE_MOD_POINTER;
                } else switch (type) {
                  default:
                    type_error("Bad type to indexed value", type);
#ifdef MAPPINGS
                  case TYPE_MAPPING:
#endif
                  case TYPE_ANY:
                    $3.type = TYPE_ANY;
                    $$.type = TYPE_ANY;
                    break;
                  case TYPE_STRING:
                    $$.type = TYPE_NUMBER;
                    break;
                }
                if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                    type_error("Bad type of index", $3.type);
            }
        };
     | expr4 '[' '<' expr0 ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.end = CURRENT_PROGRAM_SIZE;
            $$.code = F_PUSH_RINDEXED_LVALUE - F_OFFSET;
            ins_f_byte(F_RINDEX);
            if (exact_types) {
                int type;

                type = $1.type;
                if (type & TYPE_MOD_POINTER) {
                    $$.type = type & ~TYPE_MOD_POINTER;
                } else switch (type) {
                  default:
                    type_error("Bad type to indexed value", type);
                  case TYPE_ANY:
                    $$.type = TYPE_ANY;
                    break;
                  case TYPE_STRING:
                    $$.type = TYPE_NUMBER;
                    break;
                }
                if (!BASIC_TYPE($4.type, TYPE_NUMBER))
                    type_error("Bad type of index", $4.type);
            }
        };
%ifdef MAPPINGS
     | expr4 '[' expr0 ',' expr0 ']' %prec '['
        {
%line
            $$.start = $1.start;
            $$.end = CURRENT_PROGRAM_SIZE;
            $$.code = F_PUSH_INDEXED_MAP_LVALUE - F_OFFSET;
            $$.type = TYPE_ANY;
            ins_f_byte(F_MAP_INDEX);
            if ($3.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $1.type;
                if (type != TYPE_ANY && type != TYPE_MAPPING)
                {
                    type_error("Bad type to indexed value", type);
                }
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
%endif /* MAPPINGS */

pre_inc_dec:
       F_INC { $$ = F_PRE_INC-F_OFFSET; }
     | F_DEC { $$ = F_PRE_DEC-F_OFFSET; }
;

return: F_RETURN
        {
            if (exact_types &&
                !BASIC_TYPE(exact_types & TYPE_MOD_MASK, TYPE_VOID))
                type_error("Must return a value for a function declared",
                           exact_types);
            ins_f_byte(F_RETURN0);
        }
      | F_RETURN comma_expr
        {
%line
            if (exact_types) {
                if (!MASKED_TYPE($2.type, exact_types & TYPE_MOD_MASK))
                    type_error("Return type not matching", exact_types);
                /* if (exact_types & ~TYPE_MOD_POINTER) == TYPE_ANY ,
                 * a reference in $2.type remains undetected.
                 */
            }
            if ($2.type & TYPE_MOD_REFERENCE) {
                yyerror("May not return a reference");
            }
            if (last_expression == CURRENT_PROGRAM_SIZE - 1 &&
                mem_block[A_PROGRAM].block[last_expression] ==
                    F_CONST0 - F_OFFSET )
            {
                mem_block[A_PROGRAM].block[last_expression] =
                      F_RETURN0 - F_OFFSET;
                last_expression = -1;
            } else ins_f_byte(F_RETURN);
        };

expr_list: /* empty */                { $$ = 0; }
         | expr_list2                { $$ = $1; }
         | expr_list2 ','        { $$ = $1; } ; /* Allow a terminating comma */

expr_list2: expr0                { $$ = 1; add_arg_type($1.type); }
         | expr_list2 ',' expr0        { $$ = $1 + 1; add_arg_type($3.type); } ;

expr_list3: /* empty */                { $$ = 0; }
         | expr0                { $$ = 1; add_arg_type($1.type); }
         | expr_list2 ',' expr0        { $$ = $1 + 1; add_arg_type($3.type); } ;

%ifdef MAPPINGS
m_expr_list: /* empty */        { $$[0] = 0; $$[1]= 1; }
         | m_expr_list2                /* { $$ = $1; } */
         | m_expr_list2 ','        /* { $$ = $1; } Allow a terminating comma */
         | expr_list2                { $$[0] = $1; $$[1] = 0; } ;
         | expr_list2 ','        { $$[0] = $1; $$[1] = 0; } ;

m_expr_list2: expr0  m_expr_values
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
m_expr_values: ':' expr0                { $$ = 1; add_arg_type($2.type); }
        | m_expr_values ';' expr0        { $$ = $1 + 1; add_arg_type($3.type); }
%endif /* MAPPINGS */

%ifndef INITIALIZATION_BY___INIT
const_expr_list: /* empty */ { $$.length = 0; };
        | const_expr_list2        { $$ = $1; }
        | const_expr_list2 ','        { $$ = $1; } ;

const_expr_list2:
        {
            struct svalue *svp;
            struct const_list_svalue *clsv;
%line
            clsv = (struct const_list_svalue *)xalloc(sizeof *clsv);
            svp = currently_initialized;
            svp->type = T_LVALUE;
            svp->u.lvalue = &clsv->head;
            clsv->head.type = T_ERROR_HANDLER;
            clsv->head.u.error_handler = free_const_list_svalue;
            clsv->list.next = 0;
            clsv->list.val.type = T_INVALID;
            currently_initialized = &clsv->list.val;
            $<const_list>$.l = &clsv->list;
            $<const_list>$.length = 1;
        } svalue_constant
%ifdef YACC_CANNOT_MIX_ANONYMOUS_WITH_DEFAULT
        { $$ = $<const_list>1; }
%endif
        | const_expr_list2 ','
        {
            struct const_list *l;
%line
            l = (struct const_list *)xalloc(sizeof (struct const_list));
            l->next = 0;
            l->val.type = T_INVALID;
            currently_initialized = &l->val;
            $1.l->next = l;
        } svalue_constant {
            $$.l = $1.l->next;
            $$.length = $1.length+1;
        }

const_expr_list3: /* empty */ { $$.length =0; $$.l = (struct const_list *)0; };
        | const_expr_list2        { $$ = $1; } ;

svalue_constant: constant
        {
            struct svalue *svp = currently_initialized;
%line
            svp->type = T_NUMBER;
            svp->u.number = $1;
        }
        | string_constant
        {
            struct svalue *svp = currently_initialized;
%line
            svp->type = T_STRING;
            svp->x.string_type = STRING_SHARED;
            svp->u.string = last_string_constant;
            last_string_constant = 0;
        }
        | F_SYMBOL
        {
            struct svalue *svp = currently_initialized;
%line
            svp->type = T_SYMBOL;
            svp->x.quotes = $1.quotes;
            svp->u.string = $1.name;
        }
        | array_constant
        | F_QUOTED_AGGREGATE
        { $<initialized>$ = currently_initialized; }
        const_expr_list '}' ')'
        {
            struct svalue *svp = $<initialized>2;
%line
            list_to_vector($3.length, svp);
            svp->type = T_QUOTED_ARRAY;
            svp->x.quotes = $1;
        }
        | float_constant
        {
            *currently_initialized = $1;
        }
        | constant_function_call
        | F_CLOSURE
        {
            int ix;
            struct svalue *svp = currently_initialized;
%line
            ix = $1.number;
            svp->type = T_CLOSURE;
            if (ix < 0xf000) {
                struct lambda *l;

                l = (struct lambda *)xalloc(
                   sizeof *l - sizeof l->function + sizeof l->function.index
                );
                l->ref = 1;
                l->ob = current_object;
                l->function.index = ix;
                svp->u.lambda = l;
                svp->x.closure_type = CLOSURE_PRELIMINARY;
            } else {
                svp->x.closure_type = ix >= CLOSURE_SIMUL_EFUN_OFFS ? ix :
                  (instrs[ix - CLOSURE_EFUN_OFFS].Default == -1 ?
                    ix + CLOSURE_OPERATOR-CLOSURE_EFUN :
                    ix);
                svp->u.ob = current_object;
            }
            add_ref(current_object, "closure");
        }
%ifdef MAPPINGS
        | '(' '[' ']' ')'
        {
            struct svalue *svp = currently_initialized;
%line
            svp->type = T_MAPPING;
            svp->u.map = allocate_mapping(0, 1);
        }
        | '(' '[' ':' constant ']' ')'
        {
            struct svalue *svp = currently_initialized;
%line
            svp->type = T_MAPPING;
            svp->u.map = allocate_mapping(0, $4);
        }
%endif
        ;

array_constant: '(' '{'
        { $<initialized>$ = currently_initialized; }
        const_expr_list '}' ')'
        {
%line
            list_to_vector($4.length, $<initialized>3);
        };

float_constant: F_FLOAT
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
        };

constant_function_call: F_IDENTIFIER
        {
            /* I_TYPE_UNKNOWN must not be overrun by annother one, so
             * evaluate the identifier now.
             */
            /* we rely on the fact that $1.real->type is either
             * I_TYPE_UNKNOWN or I_TYPE_GLOBAL here. All others are filtered
             * by the lexical analysis.
             */
            $<const_call_head>$.function = $1->u.global.efun;
            $<const_call_head>$.initialized = currently_initialized;
            if ($1->type == I_TYPE_UNKNOWN) {
                free_shared_identifier($1);
                $<number>$ = -1;
            }
        }
        '(' const_expr_list3 ')'
        {
            struct svalue *svp;
            struct const_list_svalue *list;
%line
            svp = $<const_call_head>2.initialized;
            list = svp->u.const_list;
            switch($<const_call_head>2.function) {
              case F_ORDER_ALIST-F_OFFSET:
              {
                size_t i, listsize;
                struct vector *vec;

                if ($4.length == 1 &&
                    list->list.val.type == T_POINTER &&
                    VEC_SIZE(vec = list->list.val.u.vec) &&
                    vec->item[0].type == T_POINTER        )
                {
                    xfree((char *)list);
                } else {
                    vec = list_to_vector($4.length, svp);
                }
                if ((listsize = VEC_SIZE(vec)) &&
                    vec->item[0].type == T_POINTER)
                {
                    size_t keynum = VEC_SIZE(vec->item[0].u.vec);
                    for (i = 0; i < VEC_SIZE(vec); i++) {
                        if (vec->item[i].type != T_POINTER ||
                            VEC_SIZE(vec->item[i].u.vec) != keynum)
                        {
                            yyerrorf("bad data array %d for alist", i);
                            free_vector(vec);
                            *svp = const0;
                            break;
                        }
                    }
                } else {
                    yyerror("missing argument for order_alist");
                }
                if (listsize) {
                    svp->type = T_POINTER;
                    svp->u.vec = order_alist(vec->item, listsize, 1);
                } else {
                    *svp = const0;
                }
                free_vector(vec);
                break;
              }
              default:
                yyerror("Illegal function call in initialization");
                free_svalue(svp);
                *svp = const0;
            }
        };
%endif /* INITIALIZATION_BY___INIT */

catch: F_CATCH
        {
            $<number>$ = CURRENT_PROGRAM_SIZE;
            ins_f_byte(F_CATCH);
            ins_byte(0);
        }
       '(' comma_expr ')'
        {
%line
            int start, offset;
#if 1
            ins_f_code(F_END_CATCH);
#else
            ins_f_byte(F_POP_VALUE);
            ins_f_byte(F_RETURN);
#endif
            start = $<number>2;
            offset = CURRENT_PROGRAM_SIZE - (start + 2);
            if (offset >= 0x100) {
                /* should happen seldom, but better handle it without error...
                 */
                int i;
                char *p;

                if (CURRENT_PROGRAM_SIZE + 5 > mem_block[A_PROGRAM].max_size)
                    realloc_a_program();
                CURRENT_PROGRAM_SIZE += 5;
                p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE - 1;
                for( i = offset; --i >= 0; --p ) *p = p[-5];
                p[-5] = 2;
                p[-4] = F_BRANCH  - F_OFFSET;
                p[-3] = 3;
                p[-2] = F_LBRANCH - F_OFFSET;
                upd_short(start + 5, offset+2);
                if (offset > 0x7ffd)
                    yyerror("offset overflow");
            } else {
                mem_block[A_PROGRAM].block[start+1] = offset;
            }
            $$.start = start;
            $$.type  = TYPE_ANY;
            $$.code = -1;
        };

sscanf: F_SSCANF note_start '(' expr0 ',' expr0 lvalue_list ')'
        {
            ins_f_byte(F_SSCANF);
            ins_byte($7 + 2);
            $$.start = $2.start;
            $$.type = TYPE_NUMBER;
            $$.code = -1;
        } ;

%ifdef SUPPLY_PARSE_COMMAND
parse_command: F_PARSE_COMMAND note_start
               '(' expr0 ',' expr0 ',' expr0 lvalue_list ')'
        {
            ins_f_byte(F_PARSE_COMMAND);
            ins_byte($9 + 3);
            $$.start = $2.start;
            $$.type = TYPE_NUMBER;
            $$.code = -1;
        } ;
%endif /* SUPPLY_PARSE_COMMAND */

lvalue_list: /* empty */ { $$ = 0; }
        | lvalue_list ',' F_IDENTIFIER
        {
            int i;
%line
            $$ = 1 + $1;
            i = verify_declared($3);
            if (i & VIRTUAL_VAR_TAG) {
                ins_f_byte(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
                ins_byte(i);
            } else {
                if ((i + num_virtual_variables) & ~0xff) {
                    ins_f_byte(F_PUSH_IDENTIFIER16_LVALUE);
                    ins_short(i + num_virtual_variables);
                } else {
                    ins_f_byte(F_PUSH_IDENTIFIER_LVALUE);
                    ins_byte(i + num_virtual_variables);
                }
            }
        }
        | lvalue_list ',' F_LOCAL
        {
%line
            $$ = 1 + $1;
            ins_f_byte(F_PUSH_LOCAL_VARIABLE_LVALUE);
            ins_byte($3);
        };
        | lvalue_list ',' expr4 '[' expr0 ']'
        {
%line
            $$ = 1 + $1;
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_INDEX_LVALUE - F_OFFSET
            );
            if ($5.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $3.type;
                if ( !(type & TYPE_MOD_POINTER) )
                 switch (type) {
                  default:
                    type_error("Bad type to indexed lvalue", type);
                  case TYPE_ANY:
#ifdef MAPPINGS
                  case TYPE_MAPPING:
                    $5.type = TYPE_ANY;
#endif
                    break;
                  case TYPE_STRING:
                    break;
                }
                if (!BASIC_TYPE($5.type, TYPE_NUMBER))
                    type_error("Bad type of index", $5.type);
            }
        };
        | lvalue_list ',' expr4 '[' '<' expr0 ']'
        {
%line
            $$ = 1 + $1;
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RINDEX_LVALUE - F_OFFSET
            );
            if (exact_types) {
                int type;

                type = $3.type;
                if ( !(type & TYPE_MOD_POINTER) )
                 switch (type) {
                  default:
                    type_error("Bad type to indexed lvalue", type);
                  case TYPE_ANY:
                    break;
                  case TYPE_STRING:
                    break;
                }
                if (!BASIC_TYPE($6.type, TYPE_NUMBER))
                    type_error("Bad type of index", $6.type);
            }
        };
%ifdef MAPPINGS
        | lvalue_list ',' expr4 '[' expr0 ',' expr0 ']'
        {
%line
            $$ = 1 + $1;
            ins_f_code(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);
            if ($5.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $3.type;
                if (type != TYPE_ANY && type != TYPE_MAPPING)
                {
                    type_error("Bad type to indexed value", type);
                }
                type = $7.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
%endif /* MAPPINGS */
        | lvalue_list ',' expr4 '[' expr0 F_RANGE ']'
        {
%line
            $$ = 1 + $1;
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_EXTRACT_LVALUE - F_OFFSET
            );
            if (exact_types) {
                int type;

                type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                }
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
        | lvalue_list ',' expr4 '[' expr0 F_RANGE expr0 ']'
        {
%line
            $$ = 1 + $1;
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RANGE_LVALUE - F_OFFSET
            );
            if (exact_types) {
                int type;

                type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                }
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $7.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
        | lvalue_list ',' expr4 '[' expr0 F_RANGE '<' expr0 ']'
        {
%line
            $$ = 1 + $1;
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_NR_RANGE_LVALUE - F_OFFSET
            );
            if (exact_types) {
                int type;

                type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                }
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $8.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
        | lvalue_list ',' expr4 '[' '<' expr0 F_RANGE expr0 ']'
        {
%line
            $$ = 1 + $1;
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RN_RANGE_LVALUE - F_OFFSET
            );
            if (exact_types) {
                int type;

                type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                }
                type = $6.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $8.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
        | lvalue_list ',' expr4 '[' '<' expr0 F_RANGE '<' expr0 ']'
        {
%line
            $$ = 1 + $1;
            arrange_protected_lvalue($3.start, $3.code, $3.end,
              F_PROTECTED_RR_RANGE_LVALUE - F_OFFSET
            );
            if (exact_types) {
                int type;

                type = $3.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                }
                type = $6.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = $9.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };

lvalue: F_IDENTIFIER
        {
            int i;
%line
            $$.length = 0;
            i = verify_declared($1);
            if (i & VIRTUAL_VAR_TAG) {
                $$.u.simple[0] = F_PUSH_VIRTUAL_VARIABLE_LVALUE - F_OFFSET;
                $$.u.simple[1] = i;
                $$.type = V_VARIABLE(i)->flags & TYPE_MOD_MASK;
                if (i == -1)
                    $$.type = TYPE_ANY;
            } else {
                if ((i + num_virtual_variables) & ~0xff) {
                    char *q;
                    short var_index[2];

                    q = yalloc(4); /* assign uses an extra byte */
                    $$.length = 3;
                    $$.u.p = q;
                    q[0] = F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET;
                    var_index[0] = i + num_virtual_variables;
                    memcpy(q+1, (char*)var_index, 2);
                    $$.type = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
                } else {
                    $$.u.simple[0] = F_PUSH_IDENTIFIER_LVALUE - F_OFFSET;
                    $$.u.simple[1] = i + num_virtual_variables;
                }
                $$.type = NV_VARIABLE(i)->flags & TYPE_MOD_MASK;
            }
        }
        | F_LOCAL
        {
%line
            $$.u.simple[0] = F_PUSH_LOCAL_VARIABLE_LVALUE - F_OFFSET;
            $$.u.simple[1] = $1;
            $$.length = 0;
            $$.type = type_of_locals[$1];
        }
        | expr4 '[' expr0 ']' %prec '['
%// the ',' operator is reserved for indexing on multi-valued
%// mappings and other multi-dimensional data
        {
            char *p, *q;
            int start, current;
%line
            start = $1.start;
            current = CURRENT_PROGRAM_SIZE;
            p = mem_block[A_PROGRAM].block;
            q = yalloc(current-start+2); /* assign uses an extra byte */
            if ($1.code >= 0) {
                int end, start2;

                if ( 0 != (end = $1.end) ) {
                    start2 = end+1;
                    if ($1.code == F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET)
                        p[start] = $1.code;
                    else
                        p[end] = $1.code;
                    memcpy(q, p + start2, current - start2);
                    memcpy(q + current - start2, p + start, start2 - start);
                    q[current - start] = F_INDEX_LVALUE - F_OFFSET;
                } else {
                    char c;

                    start2 = start + 2;
                    c = p[start+1];
                    memcpy(q, p + start2, current - start2);
                    p = q + current - start2;
                    *p++ = $1.code;
                    *p++ = c;
                    *p = F_INDEX_LVALUE - F_OFFSET;
                }
            } else {
                memcpy(q, p + start, current - start);
                q[current - start] = F_PUSH_INDEXED_LVALUE - F_OFFSET;
            }
            $$.length = current + 1 - start;
            $$.u.p = q;
            CURRENT_PROGRAM_SIZE = start;
            last_expression = -1;
            if ($3.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $1.type;
                if (type & TYPE_MOD_POINTER) {
                    $$.type = type & ~TYPE_MOD_POINTER;
                } else switch (type) {
                  default:
                    type_error("Bad type to indexed lvalue", type);
#ifdef MAPPINGS
                  case TYPE_MAPPING:
#endif
                  case TYPE_ANY:
                    $3.type = TYPE_ANY;
                    $$.type = TYPE_ANY;
                    break;
                  case TYPE_STRING:
                    $$.type = TYPE_NUMBER;
                    break;
                }
                if (!BASIC_TYPE($3.type, TYPE_NUMBER))
                    type_error("Bad type of index", $3.type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
        | expr4 '[' '<' expr0 ']' %prec '['
        {
            char *p, *q;
            int start, current;
%line
            start = $1.start;
            current = CURRENT_PROGRAM_SIZE;
            p = mem_block[A_PROGRAM].block;
            q = yalloc(current-start+2); /* assign uses an extra byte */
            if ($1.code >= 0) {
                int end, start2;

                if ( 0 != (end = $1.end) ) {
                    start2 = end+1;
                    if ($1.code == F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET)
                        p[start] = $1.code;
                    else
                        p[end] = $1.code;
                    memcpy(q, p + start2, current - start2);
                    memcpy(q + current - start2, p + start, start2 - start);
                    q[current - start] = F_RINDEX_LVALUE - F_OFFSET;
                } else {
                    char c;

                    start2 = start + 2;
                    c = p[start+1];
                    memcpy(q, p + start2, current - start2);
                    p = q + current - start2;
                    *p++ = $1.code;
                    *p++ = c;
                    *p = F_RINDEX_LVALUE - F_OFFSET;
                }
            } else {
                memcpy(q, p + start, current - start);
                q[current - start] = F_PUSH_RINDEXED_LVALUE - F_OFFSET;
            }
            $$.length = current + 1 - start;
            $$.u.p = q;
            CURRENT_PROGRAM_SIZE = start;
            last_expression = -1;
            if (exact_types) {
                int type;

                type = $1.type;
                if (type & TYPE_MOD_POINTER) {
                    $$.type = type & ~TYPE_MOD_POINTER;
                } else switch (type) {
                  default:
                    type_error("Bad type to indexed lvalue", type);
                  case TYPE_ANY:
                    $$.type = TYPE_ANY;
                    break;
                  case TYPE_STRING:
                    $$.type = TYPE_NUMBER;
                    break;
                }
                if (!BASIC_TYPE($4.type, TYPE_NUMBER))
                    type_error("Bad type of index", $4.type);
            } else {
                $$.type = TYPE_ANY;
            }
        };
%ifdef MAPPINGS
        | expr4 '[' expr0 ',' expr0 ']' %prec '['
        {
            char *p, *q;
            int start, current;
%line
            start = $1.start;
            current = CURRENT_PROGRAM_SIZE;
            p = mem_block[A_PROGRAM].block;
            q = yalloc(current-start+2); /* assign uses an extra byte */
            memcpy(q, p + start, current - start);
            q[current - start] = F_PUSH_INDEXED_MAP_LVALUE - F_OFFSET;
            $$.length = current + 1 - start;
            $$.u.p = q;
            $$.type = TYPE_ANY;
            CURRENT_PROGRAM_SIZE = start;
            last_expression = -1;

            if ($3.type & TYPE_MOD_REFERENCE)
                yyerror("Reference used as index");
            if (exact_types) {
                int type;

                type = $1.type;
                if (type != TYPE_ANY && type != TYPE_MAPPING)
                {
                    type_error("Bad type to indexed value", type);
                }
                type = $5.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
%endif /* MAPPINGS */
        | expr4 '[' expr0 F_RANGE ']' %prec '['
        {
%line
            indexing_argument = $1;
            indexing_index1 = $3;
            indexing_index2.type = TYPE_ANY;
            indexing_code = F_EXTRACT_LVALUE - F_OFFSET;
            goto range_lvalue_indexing;
        };
        | expr4 '[' '<' expr0 F_RANGE ']' %prec '['
        {
%line
            if (CURRENT_PROGRAM_SIZE - last_expression == 2 &&
                mem_block[A_PROGRAM].block[last_expression] ==
                  F_CLIT - F_OFFSET )
            {
                mem_block[A_PROGRAM].block[last_expression] =
                  F_NCLIT - F_OFFSET;
            } else {
                ins_f_byte(F_NEGATE);
            }
            indexing_argument = $1;
            indexing_index1 = $4;
            indexing_index2.type = TYPE_ANY;
            indexing_code = F_EXTRACT_LVALUE - F_OFFSET;
            goto range_lvalue_indexing;
        };
        | expr4 '[' expr0 F_RANGE expr0 ']' %prec '['
        {
            char *p, *q;
            int start, current;
%line
            indexing_argument = $1;
            indexing_index1 = $3;
            indexing_index2 = $5;
            indexing_code = F_RANGE_LVALUE - F_OFFSET;
range_lvalue_indexing:
            start = indexing_argument.start;
            current = CURRENT_PROGRAM_SIZE;
            p = mem_block[A_PROGRAM].block;
            q = yalloc(current-start+3);
            if (indexing_argument.code < 0) {
                yyerror("Need lvalue for range lvalue.");
            } else {
                int end, start2;

                if ( 0 != (end = indexing_argument.end) ) {
                    start2 = end+1;
                    if (indexing_argument.code ==
                        F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET)
                    {
                        p[start] = indexing_argument.code;
                    } else {
                        p[end] = indexing_argument.code;
                    }
                } else {
                    start2 = start+2;
                    p[start] = indexing_argument.code;
                }
                memcpy(q, p + start2, current - start2);
                memcpy(q + current - start2, p + start, start2 - start);
                current -= start;
                if (indexing_code > 0xff) {
                    q[current++] = indexing_code >> F_ESCAPE_BITS;
                }
                q[current] = indexing_code;
            }
            $$.length = current + 1;
            $$.u.p = q;
            CURRENT_PROGRAM_SIZE = start;
            last_expression = -1;
            if (exact_types) {
                int type;

                $$.type = type = indexing_argument.type;
                if ((type & TYPE_MOD_POINTER) == 0 &&
                    type != TYPE_ANY && type != TYPE_STRING)
                {
                    type_error("Bad type of argument used for range", type);
                    $$.type = TYPE_ANY;
                }
                type = indexing_index1.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
                type = indexing_index2.type;
                if (type != TYPE_ANY && type != TYPE_NUMBER)
                    type_error("Bad type of index", type);
            }
        };
        | expr4 '[' expr0 F_RANGE '<' expr0 ']' %prec '['
        {
%line
            indexing_argument = $1;
            indexing_index1 = $3;
            indexing_index2 = $6;
            indexing_code = F_NR_RANGE_LVALUE - F_OFFSET;
            goto range_lvalue_indexing;
        };
        | expr4 '[' '<' expr0 F_RANGE expr0 ']' %prec '['
        {
%line
            indexing_argument = $1;
            indexing_index1 = $4;
            indexing_index2 = $6;
            indexing_code = F_RN_RANGE_LVALUE - F_OFFSET;
            goto range_lvalue_indexing;
        };
        | expr4 '[' '<' expr0 F_RANGE '<' expr0 ']' %prec '['
        {
%line
            indexing_argument = $1;
            indexing_index1 = $4;
            indexing_index2 = $7;
            indexing_code = F_RR_RANGE_LVALUE - F_OFFSET;
            goto range_lvalue_indexing;
        };

string_constant: F_STRING
        {
            last_string_constant = last_lex_string;
            last_lex_string = 0;
        }
        | string_constant '+' F_STRING
        {
            add_string_constant();
        }
        | F_STRING F_STRING
        { fatal("presence of rule should prevent its reduction"); }
        | string_constant '+' F_STRING F_STRING
        { fatal("presence of rule should prevent its reduction"); }
        | '(' string_constant ')' ;

function_call: function_name
    {
%line
        /* This seems to be an ordinary function call. But, if the function
         * is not defined, then it might be a call to a simul_efun.
         * If it is, then we make it a call_other(), which requires the
         * function name as argument.
         * We have to remember until after parsing the arguments if it was
         * a simulated efun or not, which means that the pointer has to be
         * pushed on a stack. Use the internal yacc stack for this purpose.
         */
         struct ident *real_name;

         $<function_call_head>$.start = CURRENT_PROGRAM_SIZE;
         $<function_call_head>$.simul_efun = -1;
         real_name = $1.real;
        /* we rely on the fact that $1.real->type is either
         * I_TYPE_UNKNOWN or I_TYPE_GLOBAL here. All others are filtered
         * by the lexical analysis.
         */
        if (real_name->type == I_TYPE_UNKNOWN) {
            /* prevent freeing by exotic name clashes */
            /* also makes life easyer below */
            real_name->type = I_TYPE_GLOBAL;
            real_name->u.global.function = -1;
            real_name->u.global.variable = -1;
            real_name->u.global.efun     = -1;
            real_name->u.global.sim_efun = -1;
            real_name->next_all = all_globals;
            all_globals = real_name;
        } else  if (!$1.super && real_name->u.global.function < 0 &&
          real_name->u.global.sim_efun >= 0) {
            if ( ($<function_call_head>$.simul_efun =
                                real_name->u.global.sim_efun) & ~0xff)
            {
                PREPARE_S_INSERT(6)
                char *p = real_name->name;
                increment_string_ref(p);
                add_f_byte(F_STRING);
                add_short(store_prog_string(
                  make_shared_string(query_simul_efun_file_name())));
                add_f_byte(F_STRING);
                add_short(store_prog_string(p));
                CURRENT_PROGRAM_SIZE += 6;
            }
        }
    }
        '(' expr_list3 ')'
    {
%line
        PREPARE_S_INSERT(6)
        int f = 0;
        unsigned short *arg_types = NULL;
        int first_arg;
        int efun_override = $1.super && strcmp($1.super, "efun") == 0;
        int simul_efun;

        $$.start = $<function_call_head>2.start;
        $$.code = -1;
        if ( (simul_efun = $<function_call_head>2.simul_efun) >= 0) {
            struct function *funp;

            funp = &simul_efunp[simul_efun];
            if ($4 > funp->num_arg)
                yyerrorf("Too many arguments to simul_efun %s", funp->name);
            if (simul_efun & ~0xff) {
                add_f_byte(F_CALL_OTHER);
                add_byte($4 + 2);
                CURRENT_PROGRAM_SIZE += 2;
            } else {
                if (funp->num_arg != 0xff) {
                    int i;

                    i = funp->num_arg - $4;
                    if (i > 4 && CURRENT_PROGRAM_SIZE + i + 2 >
                                mem_block[A_PROGRAM].max_size)
                    {
                        realloc_a_program();
                        __PREPARE_INSERT__p = mem_block[A_PROGRAM].block
                                              + CURRENT_PROGRAM_SIZE;
                    }
                    CURRENT_PROGRAM_SIZE += i;
                    while ( --i >= 0 ) {
                        add_f_byte(F_CONST0);
                    }
                }
                add_f_byte(F_SIMUL_EFUN);
                add_byte(simul_efun);
                if (funp->num_arg == 0xff) {
                    add_byte($4);
                    CURRENT_PROGRAM_SIZE += 3;
                } else
                    CURRENT_PROGRAM_SIZE += 2;
            }
            $$.type = funp->type & TYPE_MOD_MASK;
        } else if ($1.super?!efun_override:(f=defined_function($1.real)) >= 0)
        {
            struct function *funp;
            if ($1.super) {
                struct program *super_prog;
                int ix;
                static struct function dummy;

                ix = insert_inherited(
                  $1.super, $1.real->name,
                  &super_prog, &dummy, $4, __PREPARE_INSERT__p
                );
                if ($1.real->type == I_TYPE_UNKNOWN) {
                    free_shared_identifier($1.real);
                }
                if (ix < 0) {
                    yyerror("function not defined by inheritance as specified");
                    $$.type = TYPE_ANY;
                    if ($1.super) yfree($1.super);
                    pop_arg_stack($4);        /* Argument types not needed more */
                    break;
                }
                if (super_prog && NULL != (arg_types = super_prog->argument_types)) {
                    first_arg = super_prog->type_start[ix];
                } else {
                    first_arg = INDEX_START_NONE;
                }
                funp = &dummy;
            } else {
                add_f_byte(F_CALL_FUNCTION_BY_ADDRESS); add_short(f);
                funp = FUNCTION(f);
                arg_types = (unsigned short *)
                    mem_block[A_ARGUMENT_TYPES].block;
                first_arg =
                  ((unsigned short *)mem_block[A_ARGUMENT_INDEX].block)[f];
                add_byte($4);        /* Actual number of arguments */
                CURRENT_PROGRAM_SIZE += 4;
            }
            /*
             * Verify that the function has been defined already.
             */
            if (funp->flags & (NAME_UNDEFINED|NAME_HIDDEN)) {
                if ( !(funp->flags & (NAME_PROTOTYPE|NAME_INHERITED)) &&
                     exact_types )
                {
                    yyerrorf("Function %.50s undefined", funp->name);
                } else if ((funp->flags &
                            (NAME_UNDEFINED|NAME_PROTOTYPE|NAME_HIDDEN))
                           == NAME_HIDDEN)
                {
                    yyerrorf("Function %.50s is private", funp->name);
                }
            }
            $$.type = funp->type & TYPE_MOD_MASK;
            /*
             * Check number of arguments.
             */
            if (funp->num_arg != $4 && !(funp->flags & TYPE_MOD_VARARGS) &&
                (first_arg != INDEX_START_NONE) && exact_types)
            {
                if (funp->num_arg-1 > $4 || !(funp->flags & TYPE_MOD_XVARARGS))
                  yyerrorf("Wrong number of arguments to %.60s", $1.real->name);
            }
            /*
             * Check the argument types.
             */
            if (exact_types && first_arg != INDEX_START_NONE)
            {
                int i;
                unsigned short *argp;
                int num_arg, anum_arg;

                if ( 0 != (num_arg = funp->num_arg) ) {
                    if (funp->flags & TYPE_MOD_XVARARGS)
                      num_arg--; /* last argument is checked separately */
                    if (num_arg > (anum_arg = $4) )
                        num_arg = anum_arg;
                    arg_types += first_arg;
                    argp = get_argument_types_start(anum_arg);
                    for (i = num_arg; --i >= 0; ) {
                        int tmp1, tmp2;

                        tmp1 = *argp++ & TYPE_MOD_RMASK;
                        tmp2 = *arg_types++ & TYPE_MOD_MASK;
                        if (!MASKED_TYPE(tmp1, tmp2)) {
                            yyerrorf("Bad type for argument %d of %s %s",
                                anum_arg - i,
                                funp->name,
                                get_two_types(tmp2, tmp1));
                        }
                    }
                    if (funp->flags & TYPE_MOD_XVARARGS)
                    {
                        int tmp1, tmp2;
                        /* varargs argument is either a pointer type or mixed */
                        tmp2 = *arg_types & TYPE_MOD_MASK;
                        tmp2 &= ~TYPE_MOD_POINTER;
                        for (i = anum_arg - num_arg; --i >=0; )
                        {
                            tmp1 = *argp++ & TYPE_MOD_RMASK;
                            if (!MASKED_TYPE(tmp1,tmp2)) {
                                yyerrorf("Bad type for argument %d of %s %s",
                                    anum_arg - i,
                                    funp->name,
                                    get_two_types(tmp2, tmp1));
                            }
                        }
                    }
                }
            }
        } else if ( (f = lookup_predef($1.real)) != -1 ) {
            int min, max, def, *argp, num_arg;
            {
                int f2;

                min = instrs[f].min_arg;
                max = instrs[f].max_arg;
                def = instrs[f].Default;
                $$.type = instrs[f].ret_type;
                argp = &efun_arg_types[instrs[f].arg_index];
                num_arg = $4;
                if (def && num_arg == min-1) {
                    add_f_byte(def);
                    CURRENT_PROGRAM_SIZE++;
                    max--;
                    min--;
                } else if (num_arg < min &&
                    ( (f2 = proxy_efun(f, num_arg)) < 0 || (f = f2,MY_FALSE) )  )
                {
                    yyerrorf("Too few arguments to %s",
                            instrs[f].name);
                } else if (num_arg > max && max != -1) {
                    yyerrorf("Too many arguments to %s",
                            instrs[f].name);
                    num_arg = max;
                }
                if (max != -1 && exact_types && num_arg) {
                    /*
                     * Now check all types of the arguments to efuns.
                     */
                    int argn;
                    unsigned short *aargp;

                    aargp = get_argument_types_start(num_arg);
                    for (argn=0; argn < num_arg; argn++) {
                        int tmp1, tmp2;
                        tmp1 = *aargp++ & TYPE_MOD_MASK;
                        for (;;) {
                            if ( !(tmp2 = *argp) ) {
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
                                if (tmp1 & TYPE_MOD_POINTER & ~tmp2) {
                                    if ((tmp2 & ~TYPE_MOD_REFERENCE) !=
                                        TYPE_ANY)
                                    {
                                        continue;
                                    }
                                }
                                if ( !( (tmp1 ^ tmp2) & TYPE_MOD_REFERENCE) )
                                    break;
                            } else if (
                                (tmp2 &
                                 ~(TYPE_MOD_POINTER|TYPE_MOD_REFERENCE)) ==
                                TYPE_ANY)
                            {
                                if (tmp2 & TYPE_MOD_POINTER & ~tmp1)
                                    continue;
                                if ( !( (tmp1 ^ tmp2) & TYPE_MOD_REFERENCE) )
                                    break;
                            }
                        } /* end for */
                        while(*argp++) NOOP;
                    }
                }
                if (f > 255) {
                    switch(0) { default:
                        if (f > LAST_INSTRUCTION_CODE) {
                            f = efun_aliases[f-LAST_INSTRUCTION_CODE-1];
                            if (f <= 255)
                                break;
                        }
                        add_byte(f >> F_ESCAPE_BITS);
                        CURRENT_PROGRAM_SIZE++;
                    }
                }
                add_byte(f);
                CURRENT_PROGRAM_SIZE++;
                /* Only store number of arguments for instructions
                 * that allowed a variable number.
                 */
                if (max != min) {
                    add_byte($4);/* Number of actual arguments */
                    CURRENT_PROGRAM_SIZE++;
                }
                if ( instrs[f].ret_type == TYPE_VOID ) {
                    last_expression = mem_block[A_PROGRAM].current_size;
                    add_f_byte(F_CONST0);
                    CURRENT_PROGRAM_SIZE++;
                }
            }
        } else if (efun_override) {
            yyerrorf("Unknown efun: %s", $1.real->name);
            $$.type = TYPE_ANY;
        } else {
            struct function *funp;

            f = define_new_function(
                $1.real, 0, 0, 0, NAME_UNDEFINED, TYPE_UNKNOWN
            );
            add_f_byte(F_CALL_FUNCTION_BY_ADDRESS);
            add_short(f);
            add_byte($4);        /* Number of actual arguments */
            CURRENT_PROGRAM_SIZE += 4;
            funp = FUNCTION(f);
            if (exact_types) {
                yyerrorf("Undefined function %.50s", $1.real->name);
            }
            $$.type = TYPE_ANY;        /* Just a guess */
        }
        if ($1.super) yfree($1.super);
        pop_arg_stack($4);        /* Argument types not needed more */
    }
| expr4 F_ARROW function_name %prec F_ARROW
    {
%line
        int string_number;
        char *p = $3.real->name;
        increment_string_ref(p);
        if ($3.real->type == I_TYPE_UNKNOWN)
            free_shared_identifier($3.real);
        if ($3.super) {
            yfree($3.super);
            yyerror("inherited function may not be called by call_other");
        }
        string_number = store_prog_string(p);
        if        ( string_number <= 0x0ff ) {
            ins_f_byte(F_CSTRING0);
            ins_byte(string_number);
        } else if ( string_number <= 0x1ff ) {
            ins_f_byte(F_CSTRING1);
            ins_byte(string_number);
        } else if ( string_number <= 0x2ff ) {
            ins_f_byte(F_CSTRING2);
            ins_byte(string_number);
        } else if ( string_number <= 0x3ff ) {
            ins_f_byte(F_CSTRING3);
            ins_byte(string_number);
        } else {
            ins_f_byte(F_STRING);
            ins_short(string_number);
        }
    }
'(' expr_list3 ')'
    {
        ins_f_byte(F_CALL_OTHER);
        ins_byte($6 + 2);
        $$.type = instrs[F_CALL_OTHER-F_OFFSET].ret_type;
        $$.code = -1;
        $$.start = $1.start;
        pop_arg_stack($6);        /* No good need of these arguments */
    };

anchestor: F_IDENTIFIER
        {
            $$ = ystring_copy($1->name);
            if ($1->type == I_TYPE_UNKNOWN)
                free_shared_identifier($1);
        }
        | F_STRING F_STRING
        { fatal("presence of rule should prevent its reduction"); }
        | F_STRING
        {
            $$ = ystring_copy(last_lex_string);
            free_string(last_lex_string);
            last_lex_string = 0;
        };

function_name: F_IDENTIFIER
                {
                    $$.super = 0;
                    $$.real  = $1;
                }
              | F_COLON_COLON F_IDENTIFIER
                {
                    *($$.super = yalloc(1)) = '\0';
                    $$.real  = $2;
                }
              | anchestor F_COLON_COLON F_IDENTIFIER
                {
%line
                    if ( !strcmp($1, "efun") &&
                      $3->type == I_TYPE_GLOBAL &&
                      $3->u.global.sim_efun >= 0 &&
                      simul_efunp[$3->u.global.sim_efun].flags &
                        TYPE_MOD_NO_MASK &&
                      master_ob
                    )
                    {
                        struct svalue *res;

                        push_constant_string("nomask simul_efun");
                        push_volatile_string(current_file);
                        push_volatile_string($3->name);
                        res = apply_master_ob(STR_PRIVILEGE, 3);
                        if (!res || res->type != T_NUMBER || res->u.number < 0)
                        {
                            yyerrorf(
                              "Privilege violation: nomask simul_efun %s",
                              $3->name
                            );
                            yfree($1);
                            $$.super = 0;
                        } else if (!res->u.number) {
                            yfree($1);
                            $$.super = 0;
                        } else {
                            $$.super = $1;
                        }
                    } else $$.super = $1;
                    $$.real  = $3;
                };

condStart: F_IF '(' comma_expr ')'
        {
            mp_uint current;
            char *current_code;

            $$[0] = current_break_address;
            /* don't place labels to enter the inner part of an if */
            current_break_address &= ~CASE_LABELS_ENABLED;
            current = CURRENT_PROGRAM_SIZE;
            if (current + 2 > mem_block[A_PROGRAM].max_size)
                realloc_a_program();
            current_code = mem_block[A_PROGRAM].block + current;
            if (last_expression == current - 1 &&
                current_code[-1] == F_NOT - F_OFFSET)
            {
                current_code[-1] = F_BRANCH_WHEN_NON_ZERO - F_OFFSET;
            } else {
                *current_code = F_BRANCH_WHEN_ZERO - F_OFFSET;
                current++;
            }
            $$[1] = current;
            CURRENT_PROGRAM_SIZE = current + 1;
        } ;

cond: condStart
      statement
      optional_else
        {
            int destination, location, offset;

            destination = $3;
            location = $1[1];
            if ( (offset = destination - location) > 0x100) {
                FIX_BRANCH(
                  mem_block[A_PROGRAM].block[location-1] ==
                   F_BRANCH_WHEN_ZERO - F_OFFSET ?
                    F_LBRANCH_WHEN_ZERO :
                    F_LBRANCH_WHEN_NON_ZERO
                  ,
                  destination, location
                );
            } else {
                mem_block[A_PROGRAM].block[location] = offset - 1;
            }
            current_break_address |= $1[0] & CASE_LABELS_ENABLED;
        };
optional_else: /* empty */
    {
        $$=mem_block[A_PROGRAM].current_size;
    }
    | F_ELSE
    {
        ins_f_byte(F_BRANCH);
        $<number>$ = CURRENT_PROGRAM_SIZE;
        ins_byte(0);
    }
    statement
    {
        $$ = FIX_BRANCH( F_LBRANCH, CURRENT_PROGRAM_SIZE, $<number>2);
        $$ += $<number>2 + 1;
    };

%%
%line

%ifdef INITIALIZATION_BY___INIT
/*
 * Add a trailing jump after the last initialization code.
 */
static void add_new_init_jump() {
    /*
     * Add a new jump.
     */
    ins_f_byte(F_JUMP);
    last_initializer_end = mem_block[A_PROGRAM].current_size;
    ins_short(0);
}
%endif /* INITIALIZATION_BY___INIT */

static void arrange_protected_lvalue(start, code, end, newcode)
    int start, code, end, newcode;
{
    mp_uint current;
    char *p;

    current = CURRENT_PROGRAM_SIZE;
    if (code >= 0) {
        if (end) {
            int length;
            char *q;

            length = end - start + 1;
            if (current+length >
                mem_block[A_PROGRAM].max_size)
                realloc_a_program();
            p = mem_block[A_PROGRAM].block;
            memcpy(p + current, p + start, length);
            p += start;
            q = p + length;
            length = current - start;
            do *p++ = *q++; while (--length);
            switch(code) {
              case F_PUSH_INDEXED_LVALUE - F_OFFSET:
                code = F_PUSH_PROTECTED_INDEXED_LVALUE - F_OFFSET;
                break;
              case F_PUSH_RINDEXED_LVALUE - F_OFFSET:
                code = F_PUSH_PROTECTED_RINDEXED_LVALUE - F_OFFSET;
                break;
              case F_PUSH_INDEXED_MAP_LVALUE - F_OFFSET:
                code = F_PUSH_PROTECTED_INDEXED_MAP_LVALUE - F_OFFSET;
                break;
              case F_PUSH_IDENTIFIER16_LVALUE - F_OFFSET:
                p[-3] = code;
                goto code_stored;
#ifdef DEBUG
              default:
                fatal("Unexpected lvalue code\n");
#endif
            }
            p[-1] = code >> F_ESCAPE_BITS;
            *p++ = code;
            current++;
        code_stored:
            *p++ = newcode >> F_ESCAPE_BITS;
            *p = newcode;
        } else {
            int i;
            int length;

            if (current + 2 > mem_block[A_PROGRAM].max_size)
                realloc_a_program();
            p = mem_block[A_PROGRAM].block + start;
            i = p[1];
            length = current - start - 2;
            for( ; --length >= 0; p++) *p = p[2];
            *p++ = code;
            *p++ = i;
            *p++ = newcode >> F_ESCAPE_BITS;
            *p = newcode;
        }
    } else {
        switch(newcode) {
          case F_PROTECTED_INDEX_LVALUE - F_OFFSET:
            newcode = F_PUSH_PROTECTED_INDEXED_LVALUE - F_OFFSET;
            break;
          case F_PROTECTED_RINDEX_LVALUE - F_OFFSET:
            newcode = F_PUSH_PROTECTED_RINDEXED_LVALUE - F_OFFSET;
            break;
          default:
            yyerror("Need lvalue for range lvalue.");
        }
        if (current + 2 > mem_block[A_PROGRAM].max_size)
            realloc_a_program();
        p = mem_block[A_PROGRAM].block + start;
        *p++ = newcode >> F_ESCAPE_BITS;
        *p = newcode;
    }
    CURRENT_PROGRAM_SIZE = current + 2;
}

static void epilog() {
    int size, i;
    mp_int num_functions, num_strings, num_variables;
    char *p;
    struct ident *g,*q;
    struct function *f;
    mp_int num_function_names;
    struct function *funname_start1, *funname_start2, **link1, **link2;
    struct program *prog;

#ifdef DEBUG
    if (num_parse_error == 0 && type_of_arguments.current_size != 0)
        fatal("Failed to deallocate argument type stack\n");
#endif
    if (last_string_constant) {
        free_string(last_string_constant);
        last_string_constant = 0;
    }
    while (case_blocks) {
        struct case_list_entry *tmp;

        tmp = case_blocks;
        case_blocks = tmp->next;
        xfree(tmp);
    }
    if (mem_block[A_VIRTUAL_VAR].current_size / sizeof (struct variable) >
        0x100)
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
    add_to_mem_block(
        A_VIRTUAL_VAR_VALUES,
        mem_block[A_VARIABLE_VALUES].block,
        mem_block[A_VARIABLE_VALUES].current_size
    );
    mem_block[A_VARIABLE_VALUES].current_size = 0;
%else
    /*
     * Define the __INIT function, but only if there was any code
     * to initialize.
     */
    if (last_initializer_end > 0) {
        struct ident *ip;

        ip = make_shared_identifier("__INIT", I_TYPE_UNKNOWN);
        switch (0) { default:
            if (!ip) {
                yyerror("Out of memory");
                break;
            }
            if (ip->type > I_TYPE_GLOBAL) {
                /* sigh. can't people keep from such #defines? */
                do {
                    q = ip;
                    ip = ip->inferior;
                } while (ip && ip->type > I_TYPE_GLOBAL);
                if (!ip) {
                    ip = (struct ident *)xalloc(sizeof(struct ident));
                    if (!ip) {
                        yyerror("Out of memory");
                        break;
                    }
                    ip->name = q->name;
                    ip->type = I_TYPE_UNKNOWN;
                    ip->inferior = 0;
                    ip->hash = q->hash;
                    q->inferior = ip;
                }
            }
            define_new_function(ip, 0, 0, first_initializer_start, 0, 0);
        }
        /* ref count for ip->name was incremented by transfer_init_control() */
        /*
         * Change the last jump after the last initializer into a
         * return(1) statement.
         */
        mem_block[A_PROGRAM].block[last_initializer_end-1] =
            F_CONST1 - F_OFFSET;
        mem_block[A_PROGRAM].block[last_initializer_end-0] =
            F_RETURN - F_OFFSET;
    }

%endif /* INITIALIZATION_BY___INIT */
    if (mem_block[A_STRINGS].current_size > 0x10000 * sizeof (char *))
        yyerror("Too many strings");
    while(mem_block[A_INCLUDE_NAMES].current_size) {
        add_to_mem_block(
          A_STRINGS,
          mem_block[A_INCLUDE_NAMES].block +
            (mem_block[A_INCLUDE_NAMES].current_size -= sizeof(char *)),
          sizeof(char*)
        );
    }
    num_functions = mem_block[A_FUNCTIONS].current_size /
        sizeof (struct function);
    if (num_functions > 0x10000) {
        yyerror("Too many functions");
    }
    num_strings = mem_block[A_STRINGS].current_size /
        sizeof (char *);
    num_variables = mem_block[A_VIRTUAL_VAR].current_size /
        sizeof (struct variable);
    if (num_variables >= VIRTUAL_VAR_TAG) {
        yyerror("Too many variables");
    }
    f = (struct function *)mem_block[A_FUNCTIONS].block;
    link1 = &funname_start2;
    link2 = &funname_start1;
    num_function_names = 0;
    /* Save the time to fill in undefined functions & to sort functions
     * if there was an error or unresolved inheritance.
     */
    if (!num_parse_error && !inherit_file) {
        for (i = num_functions; --i >= 0; f++) {
            uint32 flags;

            if ( f->flags & NAME_CROSS_DEFINED ) {
                int32 offset;

                offset = f->offset.func - ( (INHERIT_MASK + 1) >> 1);
                while (f[offset].flags & NAME_CROSS_DEFINED) {
                    offset =
                      (f->offset.func = offset + f[offset].offset.func) -
                      ( (INHERIT_MASK + 1) >> 1);
                }
            }
            if ((f->flags & (NAME_UNDEFINED|NAME_INHERITED)) == NAME_UNDEFINED)
            {
                if (CURRENT_PROGRAM_SIZE + sizeof f->name + 5 >
                    mem_block[A_PROGRAM].max_size)
                {
                    realloc_a_program();
                }
                increment_string_ref(f->name);
                f->offset.pc = CURRENT_PROGRAM_SIZE + sizeof f->name + 1;
                p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE;
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
                    *p++ = F_CONST1 - F_OFFSET;
                    *p   = F_RETURN - F_OFFSET;
                } else {
%endif
                    *p++ = F_ESCAPE-F_OFFSET;
                    *p   = F_UNDEF-F_OFFSET-0x100;
%ifdef INITIALIZATION_BY___INIT
                }
%endif
                CURRENT_PROGRAM_SIZE += sizeof f->name + 5;
            }
            flags = f->flags;
            f->flags = flags & NAME_INHERITED ?
              (flags & ~INHERIT_MASK)  | (f->offset.inherit & INHERIT_MASK) :
              (flags & ~FUNSTART_MASK) | (f->offset.pc & FUNSTART_MASK);
            if ( !(flags & (NAME_HIDDEN|NAME_UNDEFINED|TYPE_MOD_PRIVATE) ) ) {
                *link1 = f;
                link1 = link2;
                link2 = &f->offset.next;
                num_function_names++;
            }
        }
        *link1 = 0;
        *link2 = 0;

        /* Store line number info for undefined functions */
        store_line_number_info();

        if (num_function_names <= 1) {
            funname_start1 = funname_start2;
        } else {
          int runlength;

          runlength = 1;
          do {
            struct function *out_start1, *out_start2, **out1, **out2;
            int count1, count2;

            count1 = num_function_names & (runlength-1);
            count2 = num_function_names & runlength;
            if (!count1) {
                out2 = &out_start1;
                *out2 = funname_start2;
                while (--count2 >= 0) {
                    out2 = &(*out2)->offset.next;
                }
                funname_start2 = *out2;
                count1 = count2 = runlength;
                out1 = &out_start2;
            } else if (!count2) {
                out2 = &out_start1;
                *out2 = funname_start1;
                do {
                    out2 = &(*out2)->offset.next;
                } while (--count1);
                funname_start1 = *out2;
                count1 = count2 = runlength;
                out1 = &out_start2;
            } else {
                out1 = &out_start1;
                out2 = &out_start2;
            }
            while (funname_start1) {
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
                        if (!--count2) {
                            *out1 = funname_start1;
                            do {
                                out1 = &(*out1)->offset.next;
                            } while (--count1);
                            funname_start1 = *out1;
                            break;
                        }
                    } else {
                        *out1 = funname_start1;
                        out1 = &funname_start1->offset.next;
                        funname_start1 = *out1;
                        if (!--count1) {
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
                    struct function **temp;

                    temp = out1;
                    out1 = out2;
                    out2 = temp;
                }
                count1 = count2 = runlength;
            }
            *out1 = 0;
            *out2 = 0;
            funname_start1 = out_start1;
            funname_start2 = out_start2;

            runlength <<= 1;
          } while (runlength < num_function_names);
        }
        if (CURRENT_PROGRAM_SIZE > FUNSTART_MASK) {
            struct function *functions;

            yyerror("Program too large");
            functions = (struct function *)mem_block[A_FUNCTIONS].block;
            for (i = num_functions; --i >= 0; functions++) {
                if ( !(functions->flags & (NAME_UNDEFINED|NAME_INHERITED)) ==
                      NAME_UNDEFINED)
                {
                    free_string(functions->name);
                }
            }
        }
    }
%ifndef INITIALIZATION_BY___INIT
    prog_variable_values =
      (struct svalue *)mem_block[A_VIRTUAL_VAR_VALUES].block;
%endif /* INITIALIZATION_BY___INIT */
    free_all_local_names();
    for (q=all_globals; NULL != (g = q); ) {
         q=g->next_all;
         free_shared_identifier(g);
    }
    while(last_yalloced) { yfree(last_yalloced); debug_message("freeing lost block\n"); }
#if 0
    fprintf(stderr,"freeing efun shadows\n");
#endif
    if (all_efun_shadows) {
        struct efun_shadow *s, *t;
            for (t=all_efun_shadows; NULL != (s = t); ) {
#if 0
    fprintf(stderr,"freeing efun shadow '%s'\n",s->shadow->name);
#endif
                s->shadow->u.global.function = -2;
                s->shadow->u.global.variable = -2;
                t=s->next;
                xfree((char *)s);
            }
            all_efun_shadows = 0;
    }
    all_globals = 0;
    switch (0) { default:
        if (num_parse_error > 0 || inherit_file)
            break;
        size = align(sizeof (struct program));
        if (!pragma_save_types) {
            mem_block[A_ARGUMENT_TYPES].current_size = 0;
            mem_block[A_ARGUMENT_INDEX].current_size = 0;
        }
        for (i=0; i<NUMPAREAS; i++)
            size += align(mem_block[i].current_size);
        size += align(num_function_names * sizeof *prog->function_names);
        size += align(num_functions * sizeof *prog->functions);
        if ( !(p = (char *)xalloc(size)) ) {
            yyerror("Out of memory");
            break;
        }
        prog = (struct program *)p;
        *prog = NULL_program;
        if ( !(prog->name = string_copy(current_file)) ) {
            xfree((char*)prog);
            yyerror("Out of memory");
            break;
        }
        prog->total_size = size;
        prog->ref = 0;
        prog->heart_beat = heart_beat;
        prog->id_number =
          ++current_id_number ? current_id_number : renumber_programs();
        prog->load_time = current_time;
        total_prog_block_size += prog->total_size;
        total_num_prog_blocks += 1;

        p += align(sizeof (struct program));
        prog->program = p;
        if (mem_block[A_PROGRAM].current_size)
            memcpy(p, mem_block[A_PROGRAM].block,
                   mem_block[A_PROGRAM].current_size);

        p += align(mem_block[A_PROGRAM].current_size);
        prog->num_function_names = num_function_names;
        prog->function_names = (unsigned short *)p;
        {
            unsigned short *namep;

            namep = (unsigned short *)p;
            if ( NULL != (f = funname_start1) || NULL != (f = funname_start2) ) {
                do {
                    *namep++ =
                      f - (struct function *)mem_block[A_FUNCTIONS].block;
                } while ( NULL != (f = f->offset.next) );
            }
        }
        p += align(num_function_names * sizeof *prog->function_names);
        prog->num_functions = num_functions;
        prog->functions = (uint32 *)p;
        {
            uint32 *flagp;

            f = (struct function *)mem_block[A_FUNCTIONS].block;
            flagp = (uint32 *)p;
            for (i = num_functions; --i >= 0; f++)
                *flagp++ = f->flags;
        }
        p += align(num_functions * sizeof *prog->functions);
        prog->strings = (char **)p;
        prog->num_strings = num_strings;
        if (mem_block[A_STRINGS].current_size)
            memcpy(p, mem_block[A_STRINGS].block,
                   mem_block[A_STRINGS].current_size);

        p += align(mem_block[A_STRINGS].current_size);
        prog->variable_names = (struct variable *)p;
        prog->num_variables = num_variables;
        if (mem_block[A_VIRTUAL_VAR].current_size)
            memcpy(p, mem_block[A_VIRTUAL_VAR].block,
                   mem_block[A_VIRTUAL_VAR].current_size);

        p += align(mem_block[A_VIRTUAL_VAR].current_size);
        prog->num_inherited = mem_block[A_INHERITS].current_size /
            sizeof (struct inherit);
        if (prog->num_inherited) {
            memcpy(p, mem_block[A_INHERITS].block,
                   mem_block[A_INHERITS].current_size);
            prog->inherit = (struct inherit *)p;
        } else {
            prog->inherit = 0;
        }
        p += align(mem_block[A_INHERITS].current_size);
        if (pragma_save_types) {
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
        } else {
            prog->argument_types = 0;
            prog->type_start = 0;
        }
        prog->line_numbers = p;
        if (mem_block[A_LINENUMBERS].current_size)
            memcpy(p, mem_block[A_LINENUMBERS].block,
                   mem_block[A_LINENUMBERS].current_size);
        p += align(mem_block[A_LINENUMBERS].current_size);
        fix_variable_index_offsets(prog);
        prog->swap_num = -1;
        for (i=0; i<NUMAREAS; i++) {
%ifndef INITIALIZATION_BY___INIT
            if (i == A_VIRTUAL_VAR_VALUES) /* Don't free now */
                continue;
%endif /* INITIALIZATION_BY___INIT */
            xfree((char *)mem_block[i].block);
        }

        /*  marion
            Do referencing here - avoid multiple referencing when an object
            inherits more than one object and one of the inherited is already
            loaded and not the last inherited
        */
        reference_prog (prog, "epilog");
        for (i = 0; i < prog->num_inherited; i++) {
            reference_prog (prog->inherit[i].prog, "inheritance");
        }
        compiled_prog = prog;
        return;
    }
    {
        struct function *functions;

%ifndef INITIALIZATION_BY___INIT
        for (i=0; i<num_variables; i++)
            free_svalue(&prog_variable_values[i]);
%endif /* INITIALIZATION_BY___INIT */

        /* Free all function names. */
        functions = (struct function *)mem_block[A_FUNCTIONS].block;
        for (i = num_functions; --i >= 0; functions++)
            if ( !(functions->flags & (NAME_INHERITED|NAME_UNDEFINED)) &&
                 functions->name )
            {
                free_string(functions->name);
            }
        do_free_sub_strings(
          num_strings,
          (char **)mem_block[A_STRINGS].block,
          num_variables,
          (struct variable *)mem_block[A_VIRTUAL_VAR].block );
        compiled_prog = 0;
        for (i=0; i<NUMAREAS; i++)
            xfree(mem_block[i].block);
        return;
    }
}

LOCAL_INLINE int proxy_efun(function, num_arg)
int function, num_arg;
{
    if (function == F_EXTRACT-F_OFFSET) {
        if (num_arg == 2) {
            return F_EXTRACT2-F_OFFSET;
        }
        if (num_arg == 1) {
            return F_EXTRACT1-F_OFFSET;
        }
    }
    if (function == F_PREVIOUS_OBJECT-F_OFFSET) {
        /* num_arg == 0 */
        return F_PREVIOUS_OBJECT0-F_OFFSET;
    }
    return -1;
}

static void _ins_f_code(b)
unsigned int b;
{
    if (b > 0x100)
        ins_byte(b >> F_ESCAPE_BITS);
    ins_byte(b);
}

/*
 * Patch a function definition of an inherited function, to what it really
 * should be.
 * The name of the function can be one of:
 *    object::name
 *    ::name
 *    name
 * Where 'object' is the name of the superclass.
 */
/* this function could be inline, but it is not called very often, and
   making it inline would damage the variable-register mapping in the
   calling block of yyparse.
 */
static int
insert_inherited(super_name, real_name,
                super_p, fun_p, num_arg, __prepare_insert__p)
    char *super_name;
    char *real_name;
    struct program **super_p;
    struct function *fun_p;
    int num_arg;
    char *__prepare_insert__p;
{
    struct inherit *ip;
    int num_inherits, super_length;
    int ix;

    while(*super_name == '/')
        super_name++;
    super_length = strlen(super_name);
    num_inherits = mem_block[A_INHERITS].current_size /
        sizeof (struct inherit);
    real_name = findstring(real_name);
    ip = (struct inherit *)mem_block[A_INHERITS].block;
    for (ix = 0; num_inherits > 0; ip++, ix++, num_inherits--) {
        short i;
        uint32 flags;
        char *__PREPARE_INSERT__p = __prepare_insert__p;
        short __ADD_SHORT__s[2];

        if (mem_block[A_INHERIT_FLAG].block[ix] != 0)
            /* this is a duplicate inherit */
            continue;

        if (*super_name) {
            /* ip->prog->name includes .c */
            int l = strlen(ip->prog->name + 2);

            if (l < super_length)
                continue;
            if (l > super_length && ip->prog->name[l-super_length-1] != '/')
                continue;
            if (strncmp(super_name, ip->prog->name + l - super_length,
                        super_length) != 0)
                continue;
        }
        if ( (i = find_function(real_name, ip->prog)) < 0)
            continue;
        flags = ip->prog->functions[i];
        if (flags & NAME_INHERITED) {
            struct inherit *ip2;
            struct program *prog1, *prog2;
            int numvar2;

            prog1 = ip->prog;
            ip2 = &prog1->inherit[flags & INHERIT_MASK];
            prog2 = ip2->prog;
            if ( 0 != (numvar2 = prog2->num_variables) &&
              prog1->variable_names[ip2->variable_index_offset+numvar2-1].flags&
                TYPE_MOD_VIRTUAL &&
              !(prog2->variable_names[numvar2-1].flags & TYPE_MOD_VIRTUAL) )
            {
                /* inherited from a virtually inherited program */
                do --ip; while (ip->prog != prog2);
                i -= ip2->function_index_offset;
            }
        }
        add_f_byte(F_CALL_EXPLICIT_INHERITED);
        add_short(ip - (struct inherit *)mem_block[A_INHERITS].block);
        add_short(i);
        add_byte(num_arg);
        *super_p = ip->prog;
        fun_p->name = real_name;
        fun_p->flags = flags & ~INHERIT_MASK;
        {
            int i2 = i;
            char *funstart;

            while ( (flags = ip->prog->functions[i2]) & NAME_INHERITED) {
                ip = &ip->prog->inherit[flags & INHERIT_MASK];
                i2 -= ip->function_index_offset;
            }
            funstart = &ip->prog->program[flags & FUNSTART_MASK];
            fun_p->type = funstart[-1];
            fun_p->num_arg = (funstart[0] & 0x7f);
            if (funstart[0] & ~0x7f)
              fun_p->type |= TYPE_MOD_XVARARGS;
        }
        CURRENT_PROGRAM_SIZE += 6;
        return i;
    }
    if (strpbrk(super_name, "*?") && !num_arg) {
        int calls = 0;
        short i = -1;
        int ix;

        *super_p = 0;
        num_inherits = mem_block[A_INHERITS].current_size /
            sizeof (struct inherit);
        ip = (struct inherit *)mem_block[A_INHERITS].block;
        for (ix = 0; num_inherits > 0; ip++, ix++, num_inherits--) {
            uint32 flags;
            PREPARE_S_INSERT(6)

            /* ip->prog->name includes .c */
            int l = strlen(ip->prog->name + 2);

            if (mem_block[A_INHERIT_FLAG].block[ix] != 0)
                /* duplicate inherit */
                continue;
            if ( !match_string(super_name, ip->prog->name, l) )
                continue;
            if ( (i = find_function(real_name, ip->prog)) < 0)
                continue;
            flags = ip->prog->functions[i];
            if (flags & NAME_INHERITED) {
                struct inherit *ip2;
                struct program *prog1, *prog2;
                int numvar2;

                prog1 = ip->prog;
                ip2 = &prog1->inherit[flags & INHERIT_MASK];
                prog2 = ip2->prog;
                if ( 0 != (numvar2 = prog2->num_variables) &&
                  prog1->variable_names[ip2->variable_index_offset+numvar2-1].flags&
                    TYPE_MOD_VIRTUAL &&
                  !(prog2->variable_names[numvar2-1].flags & TYPE_MOD_VIRTUAL) )
                {
                    /* Inherited from a virtually inherited program */
#if 0
                    /* The call for the virtually program itself should
                     * be sufficent. */
                    continue;
#else
                    /* The above code attempted to avoid repeated calls to
                     * the same virtually inherited function. With the
                     * test for A_INHERIT_FLAG this would result in no calls
                     * the function at all; without the test we again have
                     * the problem that *::f() ignores redefinition of
                     * such functions. For now, we keep the test and simply
                     * accept that virtually inherited functions might
                     * be called several times.
                     * TODO: One day, solve this problem properly.
                     */
                    do --ip; while (ip->prog != prog2);
                    i -= ip2->function_index_offset;
#endif
                }
            }
            add_f_byte(F_CALL_EXPLICIT_INHERITED);
            add_short(ip - (struct inherit *)mem_block[A_INHERITS].block);
            add_short(i);
            add_byte(num_arg);
            *super_p = ip->prog;
            fun_p->name = real_name;
            fun_p->flags = flags & ~INHERIT_MASK;
            {
                struct inherit *ip2 = ip;
                int i2 = i;
                char *funstart;

                while ( (flags = ip2->prog->functions[i2]) & NAME_INHERITED) {
                    ip2 = &ip2->prog->inherit[flags & INHERIT_MASK];
                    i2 -= ip2->function_index_offset;
                }
                funstart = &ip2->prog->program[flags & FUNSTART_MASK];
                fun_p->type = funstart[-1];
                fun_p->num_arg = funstart[0];
            }
            calls++;
            CURRENT_PROGRAM_SIZE += 6;
        }
        {
            PREPARE_S_INSERT(3)
            add_f_byte(F_AGGREGATE);
            add_short(calls);
            CURRENT_PROGRAM_SIZE += 3;
        }
        return i;
    }
    return -1;
}

void yyerror(str)
char *str;
{
    char *context;

    if (num_parse_error > 5)
        return;
    context = lex_error_context();
    (void)fprintf(stderr, "%s: %s line %d %s\n", current_file, str,
                  current_line, context);
    fflush(stderr);
    smart_log(current_file, current_line, str, context);
    if (num_parse_error == 0)
        save_error(str, current_file, current_line);
    num_parse_error++;
}

#if 0
static int check_declared(varname)
    struct ident *varname;
{
    struct variable *vp;
    int offset;

    for (offset=0;
         offset < mem_block[A_VARIABLES].current_size;
         offset += sizeof (struct variable)) {
        vp = (struct variable *)&mem_block[A_VARIABLES].block[offset];
        if (vp->flags & NAME_HIDDEN)
            continue;
        if (strcmp(vp->name, str) == 0)
            return offset / sizeof (struct variable);
    }
    return -1;
}
#endif

static int verify_declared(p)
    struct ident *p;
{
    int r;

    if (p->type != I_TYPE_GLOBAL || (r = p->u.global.variable) < 0) {
        yyerrorf("Variable %s not declared !", p->name);
        return -1;
    }
    return r;
}

void free_all_local_names()
{
    struct ident *p,*q;
    for (q=all_locals; NULL != (p = q);) {
        q = p->next_all;
        free_shared_identifier(p);
    }
    all_locals = 0;
    current_number_of_locals = 0;
    current_break_stack_need = 0;
    max_break_stack_need = 0;
}

static void add_local_name(ident, type)
    struct ident *ident;
    int type;
{
    if (current_number_of_locals == MAX_LOCAL)
        yyerror("Too many local variables");
    else {
        if (ident->type != I_TYPE_UNKNOWN) {
            ident = make_shared_identifier(ident->name, I_TYPE_LOCAL);
        }
        ident->type = I_TYPE_LOCAL;
        ident->u.local = current_number_of_locals;
        ident->next_all = all_locals;
        all_locals = ident;
        type_of_locals[current_number_of_locals] = type;
        full_type_of_locals[current_number_of_locals++] = type;
    }
}

static void cross_define(from, to, offset)
struct function *from, *to;
int32 offset;
{
    short nomask;

    to->flags = (to->flags & ~NAME_UNDEFINED) |
               (from->flags & (NAME_UNDEFINED|NAME_PROTOTYPE)) |
               NAME_CROSS_DEFINED | NAME_HIDDEN | NAME_INHERITED;
    to->offset.func = offset + ( (INHERIT_MASK + 1) >> 1);
    nomask = (from->flags|to->flags) & TYPE_MOD_NO_MASK;
    from->flags |= nomask;
    to  ->flags |= nomask;
}

static uint32 *get_function_id(progp, fx)
    struct program *progp;
    int fx;
{
    uint32 flags;

    flags = progp->functions[fx];
    if (flags & NAME_CROSS_DEFINED) {
        fx += (flags & INHERIT_MASK) - ( (INHERIT_MASK + 1) >> 1);
        flags = progp->functions[fx];
    }
    while(flags & NAME_INHERITED) {
        struct inherit *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        progp = inheritp->prog;
        fx -= inheritp->function_index_offset;
        flags = progp->functions[fx];
    }
    return &progp->functions[fx];
}

#define NEW_INHERITED_INDEX (0xfffff)

/*
 * Copy all function definitions from an inherited object. They are added
 * as undefined, so that they can be redefined by a local definition.
 * If they are not redefined, then they will be updated, so that they
 * point to the inherited definition. See epilog(). Types will be copied
 * at that moment (if available).
 *
 * An explicit call to an inherited function will not be
 * done through this entry (because this entry can be replaced by a new
 * definition). If an function defined by inheritance is called,
 * this is done with F_CALL_EXPLICIT_INHERITED
 */
%ifdef INITIALIZATION_BY___INIT
static int copy_functions(from, type)
    struct program *from;
    int type;
{
    int i, initializer = -1;
    uint32 first_func_index, current_func_index;
%else
static void copy_functions(from, type)
    struct program *from;
    int type;
{
    int i;
    uint32 first_func_index, current_func_index;
%endif
    struct function *fun_p;
    unsigned short *ixp;

    if (mem_block[A_FUNCTIONS].max_size <
        mem_block[A_FUNCTIONS].current_size +
          from->num_functions * sizeof(struct function) )
    {
        if (!realloc_mem_block(&mem_block[A_FUNCTIONS],
                          mem_block[A_FUNCTIONS].current_size +
                            from->num_functions * sizeof(struct function)))
            return
%ifdef INITIALIZATION_BY___INIT
                    0
%endif
                        ;
    }
    fun_p = (struct function *)
        (mem_block[A_FUNCTIONS].block + mem_block[A_FUNCTIONS].current_size);
    for (i=0; i < from->num_functions; i++,fun_p++) {
        struct program *defprog;
        struct inherit *ip;
        unsigned char *funstart;
        int i2;
        uint32 flags;

        flags = from->functions[i];
        fun_p->offset.inherit = NEW_INHERITED_INDEX;
        i2 = i;
        if (flags & NAME_INHERITED) {
            fun_p->flags =
                (flags & ~INHERIT_MASK) | NAME_INHERITED | NAME_HIDDEN;
            if (flags & NAME_CROSS_DEFINED) {
                i2 +=
                  (fun_p->offset.func = (flags & INHERIT_MASK)) -
                  ( (INHERIT_MASK + 1) >> 1);
            }
        } else {
            fun_p->flags =
                (flags & ~FUNSTART_MASK) | NAME_INHERITED | NAME_HIDDEN;
        }
        defprog = from;
        while ( (flags = defprog->functions[i2]) & NAME_INHERITED) {
            ip = &defprog->inherit[flags & INHERIT_MASK];
            i2 -= ip->function_index_offset;
            defprog = ip->prog;
        }
        funstart = &defprog->program[flags & FUNSTART_MASK];
        memcpy(
            (char *)&fun_p->name,
            funstart - 1 - sizeof fun_p->name,
            sizeof fun_p->name
        );
        fun_p->type = funstart[-1];
        fun_p->num_arg = (funstart[0] & 0x7f);
        if (funstart[0] & ~0x7f)
          fun_p->type |= TYPE_MOD_XVARARGS;
        if (funstart[2] == F_ESCAPE - F_OFFSET &&
            funstart[3] == F_UNDEF  - F_OFFSET -0x100)
        {
            fun_p->flags |= NAME_UNDEFINED;
        }
    }
    fun_p = (struct function *)
        (mem_block[A_FUNCTIONS].block + mem_block[A_FUNCTIONS].current_size);
    ixp = from->function_names;
    for (i = from->num_function_names; --i >= 0; ) {
        fun_p[*ixp++].flags &= ~NAME_HIDDEN;
    }
    first_func_index = current_func_index =
      mem_block[A_FUNCTIONS].current_size / sizeof (struct function);
    mem_block[A_FUNCTIONS].current_size += sizeof *fun_p * from->num_functions;
    for (i=0; i < from->num_functions; i++,current_func_index++) {
        /* Do not call define_new_function() from here, as duplicates would
         * be removed.
         */
        struct function fun;
        int new_type;
        unsigned short tmp_short;

        struct ident* p;

        fun = fun_p[i];        /* Make a copy */
        /* Prepare some data to be used if this function will not be
         * redefined.
         */
        /* fun.name has already it's ref as a newly defined function in from */
        fun.flags |= type & TYPE_MOD_NO_MASK;
        switch (0) { default:
            if ( (fun.flags & (NAME_HIDDEN|TYPE_MOD_NO_MASK|NAME_UNDEFINED) ) ==
                 (NAME_HIDDEN|TYPE_MOD_NO_MASK) )
                break;
            /* this function is either visible or subject to redefinition */
            p = make_shared_identifier(fun.name, I_TYPE_GLOBAL);
            if (!p) {
                yyerror("Out of memory");
                break;
            }
            if (p->type > I_TYPE_GLOBAL) {
                /* sigh. can't people keep from such #defines? */
                struct ident *q;

                do {
                    q = p;
                    p = p->inferior;
                }
                while (p && p->type > I_TYPE_GLOBAL);
                if (!p) {
                    p = (struct ident *)xalloc(sizeof(struct ident));
                    if (!p) {
                        yyerror("Out of memory");
                        break;
                    }
                    p->name = q->name;
                    p->type = I_TYPE_UNKNOWN;
                    p->inferior = 0;
                    p->hash = q->hash;
                    q->inferior = p;
                }
            }
            if (p->type != I_TYPE_UNKNOWN) {
                int32 n;

                if ( (n = p->u.global.function) >= 0) {
                  /* already inherited from somewhere else */
                  /* Don't try to resolve cross-references inside the
                   * currently inherited program; not only is this superflous,
                   * but it can also lead to circular cross-inheritance
                   * when there was a misplaced prototype or an explicit
                   * directive to inherit a multiply inherited function
                   * from a particular base class (the latter is not
                   * implemented). In these cases, the information that lead
                   * to the non-standard preference would be very hard to
                   * reconstruct.
                   */
                  if ((uint32)n < first_func_index) {
                    struct function *OldFunction = FUNCTION(n);

                    if ( !(OldFunction->flags & NAME_INHERITED) ) {
                        debug_message(
                          "Misplaced prototype for %s in %s\n",
                          fun.name, current_file
                        );
                        cross_define( &fun, OldFunction,
                          current_func_index - n );
                        p->u.global.function = current_func_index;
                    } else if (fun.flags & TYPE_MOD_VIRTUAL &&
                        OldFunction->flags & TYPE_MOD_VIRTUAL &&
                        !((fun.flags | OldFunction->flags) & NAME_HIDDEN) &&
                        get_function_id(from, i) ==
                        get_function_id(
                          ((struct inherit *)mem_block[A_INHERITS].block)
                            [OldFunction->offset.inherit].prog,
                          n - ((struct inherit *)mem_block[A_INHERITS].block)
                           [OldFunction->offset.inherit].function_index_offset)
                    ) {
                        /* Entries denote the same function. We have to use
                         * cross_define nonetheless, to get consistant
                         * redefinition.
                         */
                        /* prefer the first one */
                        OldFunction->flags |= fun.flags &
                            (TYPE_MOD_PUBLIC|TYPE_MOD_NO_MASK);
                        OldFunction->flags &= fun.flags | ~TYPE_MOD_STATIC;
                        cross_define( OldFunction, &fun,
                          n - current_func_index );
                    } else if
                      ( (fun.flags & OldFunction->flags & TYPE_MOD_NO_MASK) &&
                      /* both are nomask... */
                      !( (fun.flags|OldFunction->flags) & NAME_UNDEFINED ) )
                    {
                        yyerrorf(
                          "Illegal to inherit 'nomask' function '%s' twice",
                          fun.name);
                    }
                    else if ((fun.flags & TYPE_MOD_NO_MASK ||
                        OldFunction->flags & (NAME_HIDDEN|NAME_UNDEFINED)) &&
                      !(         fun.flags & (NAME_HIDDEN|NAME_UNDEFINED)) ) {
                        /* silently prefer this one */
                        cross_define( &fun, OldFunction,
                          current_func_index - n );
                        p->u.global.function = current_func_index;
                    } else {
                        /* prefer the first one */
                        cross_define( OldFunction, &fun,
                          n - current_func_index );
                    }
                  } else if ( !(fun.flags & NAME_CROSS_DEFINED) ) {
                    /* This is the dominant definition in the superclass,
                     * inherit this one.
                     */
#ifdef DEBUG
                    /* The definition we picked before should be cross-defined
                     * to the definition we have now.
                     */
                    if ( !(FUNCTION(n)->flags & NAME_CROSS_DEFINED) ||
                         FUNCTION(n)->offset.func !=
                           ((int32)current_func_index) - n + ( (INHERIT_MASK + 1) >> 1) )
                    {
                        fatal(
                          "inconsistent function definition in superclass\n"
                        );
                    }
#endif
                    p->u.global.function = current_func_index;
                  }
                } else {
                    if (n == -2) {
                        struct efun_shadow *q;

                        q =(struct efun_shadow *)
                           xalloc(sizeof(struct efun_shadow));
                        if (!q) {
                            yyerror("Out of memory");
                            break;
                        }
                        q->shadow = p;
                        q->next = all_efun_shadows;
                        all_efun_shadows = q;
                    }
                    p->u.global.function = current_func_index;
                }
            } else {
                /* was I_TYPE_UNKNOWN */
                p->type = I_TYPE_GLOBAL;
                p->u.global.variable = -1;
                p->u.global.efun     = -1;
                p->u.global.sim_efun = -1;
                p->u.global.function = current_func_index;
                p->next_all = all_globals;
                all_globals = p;
            }
            /*
             * public functions should not become private when inherited
             * 'private'
             */
            new_type = type;
            if (fun.flags & TYPE_MOD_PUBLIC)
                new_type &= ~(TYPE_MOD_PRIVATE|TYPE_MOD_STATIC);
            fun.flags |= new_type;
            /* marion
             * this should make possible to inherit a heart beat function, and
             * thus to mask it if wanted.
             */
            if ((heart_beat == -1) && fun.name[0] == 'h' &&
              (strcmp(fun.name, "heart_beat") == 0))
                heart_beat = current_func_index;
%ifdef INITIALIZATION_BY___INIT
            else if (fun.name[0] == '_' && strcmp(fun.name+1, "_INIT") == 0) {
                initializer = i;
                fun.flags |= NAME_UNDEFINED;
            }
%endif
        } /* end of visibility/redifinability - only code */
        /*
         * Copy information about the types of the arguments, if it is
         * available.
         */
        tmp_short = INDEX_START_NONE;        /* Presume not available. */
        if (from->type_start != 0) {
          if (from->type_start[i] != INDEX_START_NONE)
          {
            /*
             * They are available for function number 'i'. Copy types of
             * all arguments, and remember where they started.
             */
            tmp_short = mem_block[A_ARGUMENT_TYPES].current_size /
                sizeof from->argument_types[0];
            if (fun.num_arg)
                add_to_mem_block(
                  A_ARGUMENT_TYPES,
                  (char *)&from->argument_types[from->type_start[i]],
                  (sizeof (unsigned short)) * fun.num_arg
                );
          }
        } else
            fun.flags |= NAME_TYPES_LOST;
        /*
         * Save the index where they started. Every function will have an
         * index where the type info of arguments starts.
         */
        add_to_mem_block(A_ARGUMENT_INDEX, (char*)&tmp_short, sizeof tmp_short);
        fun_p[i] = fun;
    }
%ifdef INITIALIZATION_BY___INIT
    return initializer;
%endif
}

static void fix_function_inherit_indices(from)
    struct program *from;
{
    int i, inherit_index;
    struct function *funp;

    inherit_index =
      mem_block[A_INHERITS].current_size / sizeof (struct inherit);
    funp =
      (struct function *)
        (mem_block[A_FUNCTIONS].block+mem_block[A_FUNCTIONS].current_size) -
      from->num_functions;
    for(i = from->num_functions; --i>=0; funp++) {
        if ( funp->offset.inherit == NEW_INHERITED_INDEX &&
             !(funp->flags & NAME_CROSS_DEFINED) )
        {
            funp->offset.inherit = inherit_index;
        }
    }
}

static void fix_variable_index_offsets(new_prog)
    struct program *new_prog;
{
    int i;
    struct inherit *inheritp;

    i = new_prog->num_inherited;
    for (inheritp = new_prog->inherit; --i >= 0; inheritp++) {
        if (inheritp->variable_index_offset & NON_VIRTUAL_OFFSET_TAG) {
            inheritp->variable_index_offset += num_virtual_variables;
            inheritp->variable_index_offset &= ~NON_VIRTUAL_OFFSET_TAG;
        }
    }
}

/*
 * Copy all variable names from the object that is inherited from.
 * It is very important that they are stored in the same order with the
 * same index.
 */
%ifndef INITIALIZATION_BY___INIT
static void copy_variables(from, type, initializers)
    struct program *from;
    int type;
    struct svalue *initializers;
%else
static void copy_variables(from, type)
    struct program *from;
    int type;
%endif
{
    int i, j;
    int new_bound, last_bound;
    int variable_index_offset, fun_index_offset;
    uint inheritc;
    struct inherit *inheritp;
    int from_variable_index_offset = -1;
    int previous_variable_index_offset;

    type &= ~TYPE_MOD_VARARGS;
    if (type & TYPE_MOD_VIRTUAL) {
        inheritp = (struct inherit *)(mem_block[A_INHERITS].block);
        j = mem_block[A_INHERITS].current_size;
        for (; (j -= sizeof(struct inherit)) >= 0; inheritp++) {
            if (inheritp->prog == from &&
                !(inheritp->variable_index_offset & NON_VIRTUAL_OFFSET_TAG) )
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
    fun_index_offset =
      mem_block[A_FUNCTIONS].current_size / sizeof(struct function) -
      from->num_functions;
    variable_index_offset =
        mem_block[A_VIRTUAL_VAR].current_size / sizeof(struct variable);
    i = from->num_inherited;
    last_bound = 0;
    for (inheritc = 0, inheritp = from->inherit; ; inheritc++, inheritp++) {
        if (--i >= 0) {
            struct program *progp;

            progp = inheritp->prog;
            new_bound =
              inheritp->variable_index_offset + progp->num_variables;

            /* has a new virtual variable been introduced in this program? */
            if (progp->num_variables &&
                from->variable_names[new_bound-1].flags & TYPE_MOD_VIRTUAL &&
                !(progp->variable_names[progp->num_variables-1].flags &
                  TYPE_MOD_VIRTUAL) )
            {
                struct inherit inherit, *inheritp2;
                int k, inherit_index;
                uint32 *flagp;
                struct function *funp;

#ifdef INITIALIZATION_BY___INIT
                if (variables_initialized)
                    yyerror(
"illegal to inherit virtually after initializing variables\n"
                    );
#endif
                inherit = *inheritp;
                inheritp2 = (struct inherit *)(mem_block[A_INHERITS].block);
                j = mem_block[A_INHERITS].current_size;
                for (; (j -= sizeof(struct inherit)) >= 0; inheritp2++) {
                    if (inheritp2->prog == inherit.prog &&
                        !(inheritp2->variable_index_offset &
                          NON_VIRTUAL_OFFSET_TAG) )
                    {
                        inherit.variable_index_offset =
                          inheritp2->variable_index_offset;
                        break;
                    }
                }
                if (j < 0) {
                    variable_index_offset += new_bound - last_bound;
                    inherit.variable_index_offset =
                      variable_index_offset - progp->num_variables;
                }
                inherit_index = (mem_block[A_INHERITS].current_size - j) /
                   sizeof(struct inherit) - 1;
                inherit.function_index_offset += fun_index_offset;
                add_to_mem_block(A_INHERITS, (char *)&inherit, sizeof inherit);
                byte_to_mem_block(A_INHERIT_FLAG, 1);
                /* If a function is directly inherited from a program that
                 * introduces a virtual variable, the code therein is not
                 * aware of virtual inheritance. For this reason, there are
                 * the extra struct inherits with an appropriate
                 * variable_index_offset; we have to redirect inheritance
                 * to these struct inherits.
                 */
                flagp = from->functions + inheritp->function_index_offset;
                funp = (struct function *)mem_block[A_FUNCTIONS].block +
                    inherit.function_index_offset;
                for (k = inherit.prog->num_functions; --k >= 0; funp++) {
                    if ( !(funp->flags & NAME_CROSS_DEFINED) &&
                           (*flagp & (NAME_INHERITED|NAME_CROSS_DEFINED)) ==
                           NAME_INHERITED &&
                         (*flagp & INHERIT_MASK) == inheritc )
                    {
                        funp->offset.inherit = inherit_index;
                    }
                    flagp++;
                }
                if (j >= 0) {
                    /* There has been another instance
                     * of this virtual superclass before
                     */
                    if (new_bound > last_bound)
                        last_bound = new_bound;
                    continue;
                }
                previous_variable_index_offset = -1;
            } else {
                continue;        /* Not a virtual superclass */
            }
        } else {
            previous_variable_index_offset = from_variable_index_offset;
            new_bound = from->num_variables;
            if (new_bound == last_bound)
                break;
        }
        for (j = last_bound; j < new_bound; j++) {
            struct ident *p;
            int new_type;

            p = make_shared_identifier(from->variable_names[j].name,
                I_TYPE_GLOBAL);
            if (!p) {
                yyerror("Out of memory");
                return;
            }
            if (p->type > I_TYPE_GLOBAL) {
                /* sigh. can't people keep from such #defines? */
                struct ident *q;

                do {
                        q = p;
                        p = p->inferior;
                } while (p && p->type > I_TYPE_GLOBAL);
                if (!p) {
                    p = (struct ident *)xalloc(sizeof(struct ident));
                    if (!p) {
                        yyerror("Out of memory");
                        return;
                    }
                    p->name = q->name;
                    p->type = I_TYPE_UNKNOWN;
                    p->inferior = 0;
                    p->hash = q->hash;
                    q->inferior = p;
                }
            }
            new_type = type;
            /*
             * 'public' variables should not become private when inherited
             * 'private'.
             */
            if (from->variable_names[j].flags & TYPE_MOD_PUBLIC)
                new_type &= ~TYPE_MOD_PRIVATE;
            /* define_variable checks for previous 'nomask' definition. */
            if (previous_variable_index_offset >= 0) {
                if ( !(from->variable_names[j].flags & TYPE_MOD_PRIVATE) )
                    redeclare_variable(p,
                      new_type | from->variable_names[j].flags | NAME_INHERITED,
                      previous_variable_index_offset + j
                    );
            } else {
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
        } /* end loop through functions */
        last_bound = new_bound;
    } /* end of loop through inherits */
}

/*
 * This function is called from lex.c for every new line read from the
 * "top" file (means not included files). Some new lines are missed,
 * as with #include statements, so it is compensated for.
 */
void store_line_number_info()
{
    char c;
    short offset = mem_block[A_PROGRAM].current_size - stored_bytes;
    if (offset <= 0) return;
    stored_bytes = mem_block[A_PROGRAM].current_size;

    if (offset <= 8 &&
      current_line - stored_lines >= 2 && current_line - stored_lines <= 9) {
        c = offset + 8*(current_line - stored_lines) + 47;
        byte_to_mem_block(A_LINENUMBERS, c);
        stored_lines = current_line;
        return;
    }
    stored_lines++;
    while(stored_lines < current_line)
    {
        int lines;

        lines = current_line - stored_lines;
        if (lines > LI_MAXEMPTY) lines = LI_MAXEMPTY;
        stored_lines += lines;
        c = 256 - lines;
        byte_to_mem_block(A_LINENUMBERS, c);
    }
    while(offset >= LI_MAXOFFSET) {
        byte_to_mem_block(A_LINENUMBERS, LI_MAXOFFSET);
        offset -= LI_MAXOFFSET;
    }
    byte_to_mem_block(A_LINENUMBERS, offset);
}

static void store_line_number_relocation(relocated_from)
    int relocated_from;
{
    int save_current, offset;

    save_current = current_line;
    stored_lines -= 2;
    current_line = stored_lines+1;
    offset = current_line - relocated_from;
    if (offset >= LI_SMALL_REL) {
        byte_to_mem_block(A_LINENUMBERS, LI_L_RELOCATED);
        byte_to_mem_block(A_LINENUMBERS, offset >> 8);
        byte_to_mem_block(A_LINENUMBERS, offset);
        /* trailing LI_L_RELOCATED allows bidirectional traversal */
        byte_to_mem_block(A_LINENUMBERS, LI_L_RELOCATED);
    } else {
        byte_to_mem_block(A_LINENUMBERS, LI_RELOCATED + offset);
    }
    store_line_number_info();
    current_line = save_current;
}

static int simple_includes;

void store_include_info(name)
    char *name;
{
    if (last_include_start == mem_block[A_LINENUMBERS].current_size) {
        simple_includes++;
    } else {
        simple_includes = 0;
    }
    stored_lines++;
    while(stored_lines < current_line) {
        int lines;

        lines = current_line - stored_lines;
        if (lines > LI_MAXEMPTY) lines = LI_MAXEMPTY;
        stored_lines += lines;
        byte_to_mem_block(A_LINENUMBERS, 256 - lines);
    }
    byte_to_mem_block(A_LINENUMBERS, LI_INCLUDE);
    last_include_start = mem_block[A_LINENUMBERS].current_size;
    name = make_shared_string(name);
    if (!name) {
        increment_string_ref(name = STR_DEFAULT);
        yyerror("Out of memory");
    }
    add_to_mem_block(A_INCLUDE_NAMES, (char *)&name, sizeof name);
    stored_lines = 0;
}

void store_include_end() {
    unsigned char c;

    stored_lines = current_line-1;
    if (last_include_start == mem_block[A_LINENUMBERS].current_size) {
        last_include_start = mem_block[A_LINENUMBERS].current_size - 1;
        stored_lines--;
        while(last_include_start &&
          (c = mem_block[A_LINENUMBERS].block[last_include_start - 1]) >=
          0x100 - LI_MAXEMPTY)
        {
            stored_lines += c - 0x100;
            last_include_start--;
        }
        mem_block[A_LINENUMBERS].current_size = last_include_start;
        if (--simple_includes < 0) {
            last_include_start--;
        }
        free_string( *(char **)
          (mem_block[A_INCLUDE_NAMES].block +
           (mem_block[A_INCLUDE_NAMES].current_size -= sizeof(char *)))
        );
        /* If we return to the auto_include_string, current_line has been
         * negative, and hence stored_lines became negative.  However,
         * actually, the line number information has been unwinded to 0.  */
        if (stored_lines < 0)
            stored_lines = 0;
    } else {
        byte_to_mem_block(A_LINENUMBERS, LI_INCLUDE_END);
        /* Since LI_INCLUDE advances the line number by one, this is even more
         * tricky to accomodate correctly in the auto_include string.
         * By using a fake line number relocation, we decrease the line number
         * by two, then we advance it by one empty line.  */
        if (stored_lines < 1) {
            stored_lines = 0;
            byte_to_mem_block(A_LINENUMBERS, LI_RELOCATED + 1);
            byte_to_mem_block(A_LINENUMBERS, 256 - 1);
        }
    }
}

static char *get_type_name(type)
    int type;
{
    static char buff[100];
    static char *type_name[] = { "unknown", "int", "string", "void", "object",
                                 "mapping", "float", "mixed", 0, "closure",
                                 "symbol", "quoted_array", };
    int pointer = 0, reference = 0;

    buff[0] = 0;
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
    if (type & TYPE_MOD_POINTER) {
        pointer = 1;
        type &= ~TYPE_MOD_POINTER;
    }
    if (type & TYPE_MOD_REFERENCE) {
        reference = 1;
        type &= ~TYPE_MOD_REFERENCE;
    }
    if ((size_t)type >= sizeof type_name / sizeof type_name[0])
        fatal("Bad type\n");
    strcat(buff, type_name[type]);
    strcat(buff," ");
    if (pointer)
        strcat(buff, "* ");
    if (reference)
        strcat(buff, "& ");
    return buff;
}

static void type_error(str, type)
    char *str;
    int type;
{
    char *p;

    p = get_type_name(type);
    yyerrorf("%s: \"%s\"", str, p);
}

static void argument_type_error(i, type)
    int i, type;
{
    char *p;

    p = get_type_name(type);
    yyerrorf("Bad argument to %s: \"%s\"", instrs[i].name, p);
}

/*
 * Compile an LPC file.
 */
void compile_file() {
    prolog();
    yyparse();
    epilog();
}

static char *get_two_types(type1, type2)
    int type1, type2;
{
    static char buff[100];

    strcpy(buff, "( ");
    strcat(buff, get_type_name(type1));
    strcat(buff, "vs ");
    strcat(buff, get_type_name(type2));
    strcat(buff, ")");
    return buff;
}

static void insert_pop_value()
{
    if (last_expression == mem_block[A_PROGRAM].current_size-1) {
        switch ( mem_block[A_PROGRAM].block[last_expression]+F_OFFSET ) {
        case F_ASSIGN:
            mem_block[A_PROGRAM].block[last_expression] =
                F_VOID_ASSIGN - F_OFFSET;
            break;
        case F_ADD_EQ:
            mem_block[A_PROGRAM].block[last_expression] =
                F_VOID_ADD_EQ - F_OFFSET;
            break;
        case F_PRE_INC:
        case F_POST_INC:
            mem_block[A_PROGRAM].block[last_expression] =
                F_INC - F_OFFSET;
            break;
        case F_PRE_DEC:
        case F_POST_DEC:
            mem_block[A_PROGRAM].block[last_expression] =
                F_DEC - F_OFFSET;
            break;
        case F_CONST0:
        case F_CONST1:
            mem_block[A_PROGRAM].current_size = last_expression;
            break;
        default: ins_f_byte(F_POP_VALUE);
        }
        last_expression = -1;
    } else ins_f_byte(F_POP_VALUE);
}

#define SH(x) - -(1 << (x))
short hook_type_map[NUM_CLOSURE_HOOKS] =
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

#if defined( DEBUG ) && defined ( TRACE_CODE )

static int code_window_offset = -1;

void set_code_window() {
    code_window_offset = CURRENT_PROGRAM_SIZE;
}

void show_code_window() {
    int i;
    unsigned char *p;

    if (code_window_offset < 0) return;
    p = (unsigned char *)mem_block[A_PROGRAM].block + code_window_offset;
    for (i=0; i<16; i++) {
        printf("%3d ", p[i]);
    }
    printf("\n");
    fflush(stdout);
}

#endif

#ifdef MALLOC_smalloc
void count_compiler_refs() {
    if (type_of_arguments.block) {
        note_malloced_block_ref(type_of_arguments.block);
    }
}
#endif
