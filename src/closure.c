/*---------------------------------------------------------------------------
 * Closure compiler and functions; including the switch() code generation.
 *
 *---------------------------------------------------------------------------
 * Closures implement the possibility to treat code as data. This means
 * both 'function pointers' (efun, simul-efun and lfun closures) as well
 * as functions compiled from data at runtime (lambda closures).
 *
 * The data for closures are stored in two places: in T_CLOSURE-svalues and
 * in additional lambda structures. The exact type of the closure is
 * stored in the secondary type of the svalue, the x.closure_type.
 * Depending on this closure type, the data part of the svalue (union u)
 * holds additional data or not.
 *
 * operator closure:   type = CLOSURE_OPERATOR (-0x1800) + instr
 *   These are the closures for the LPC operators (e.g. #'&& or #'?).
 *   The 'instr' is usually the machine instruction implementing
 *   the operator (see lex::symbol_operator() for the exact mapping).
 *   u.ob is the object this closure is bound to.
 *
 * efun closure:       type = CLOSURE_EFUN (-0x1000) + instr
 *   These are the closures for the LPC efuns (e.g. #'atan or #'call_other).
 *   The 'instr' is usually the machine instruction implementing
 *   the efun (see lex::symbol_efun() for the exact mapping).
 *   u.ob is the object this closure is bound to.
 *
 * operator and efun closure could be implemented using the same number
 * range because the use of unique machine instructions guarantees no
 * value collision. This way however makes distinguishing the two cases
 * easier.
 *
 * simul-efun closure: type = CLOSURE_SIMUL_EFUN (-0x0800) + index
 *   These are the closures for the simul-efuns. The 'index' is the
 *   the index of the simul-efun in the function table of the simul-efun
 *   object.
 *   u.ob is the object this closure is bound to.
 *
 * lfun closure:           type = CLOSURE_LFUN (0)
 *   Reference to a lfun in an object.
 *   u.lambda points to the lambda structure with the detailed data.
 *
 * identifier closure:     type = CLOSURE_IDENTIFIER (1)
 *   Reference to a variable in this object.
 *   u.lambda points to the lambda structure with the detailed data.
 *
 * preliminary closure:    type = CLOSURE_PRELIMINARY (2)
 *   TODO: ???
 *
 * bound lambda closure:   type = CLOSURE_BOUND_LAMBDA (3)
 *   This is an unbound lambda closure which was bound to an object.
 *   To allow binding the same unbound lambda to different objects
 *   at the same time, this construct uses a double indirection:
 *   u.lambda points to a lambda structure with the binding information,
 *   which then points to the actual unbound lambda structure.
 *
 * lambda closure:         type = CLOSURE_LAMBDA (4)
 *   Lambda closure bound to an object at compilation time.
 *   u.lambda points to the lambda structure with the compiled function.
 *
 * unbound lambda closure: type = CLOSURE_UNBOUND_LAMBDA (5)
 *   Unbound lambda closure, which is not bound to any object at
 *   compile time.
 *   u.lambda points to the lambda structure with the compiled function.
 *
 *
 * The additional information for closure are stored in structures
 * of type lambda_s, which are refcounted. For lambda closures the lambda
 * structure is in fact embedded in the middle of a larger memory block:
 * it is prepended by an array of the svalues used as constants in
 * the function, and followed by the actual function code.
 *
 *   struct lambda_s
 *   {
 *       svalue_t values[] (lambda closures only)
 *           For lambda closures, the constant values used by the function
 *           which are indexed from the end ((svalue_t*)lambda_t).
 *       p_int ref;
 *       object_t *ob;
 *           Object the closure is bound to (for bound UNBOUND_LAMBDAs just
 *           during the execution of the lambda).
 *       union --- Closure information ---
 *       {
 *           unsigned short var_index;
 *               _IDENTIFIER: index in the variable table
 *               Function indices are lower than CLOSURE_IDENTIFIER_OFFS
 *               (0xe800), variable indices are higher.
 *               The special value VANISHED_VARCLOSURE_INDEX (-1) is
 *               used to mark vanished variables.
 *
 *           struct -- CLOSURE_LFUN
 *           {
 *               object_t *ob;
 *                   Originating object
 *               unsigned short  index
 *                   Index in the objects function table
 *           } lfun;
 *
 *           struct
 *           {
 *               mp_int num_values;         -- Number of svalues.
 *               unsigned char num_locals;  -- Number of local variables
 *               unsigned char num_arg;     -- Number of arguments needed
 *               bytecode_t program[1];
 *           } code;
 *               LAMBDA and UNBOUND_LAMBDA closures: the function code.
 *               'num_values' is the number of constants store before
 *               the lambda structure.
 *
 *           lambda_t *lambda;
 *               BOUND_LAMBDA: pointer to the UNBOUND_LAMBDA structure.
 *
 *       } function;
 *
 *       svalue_t context[.lfun.context_size];
 *          lfun-closure context variables, if any.
 *          Putting this array into the function.lfun somehow causes memory
 *          corruption because some lambda structures won't be allocated large
 *          enough.
 *   }
 *
 *
 * If a lambda() is compiled while replace_program() is scheduled, the
 * construction information is stored in the protector and the lambda
 * is recompiled when the program replacement is put into place.
 *
 *
 * To handle lambda closures, two more svalue types are needed:
 *
 * Symbols (T_SYMBOL svalue)
 *     Symbols are names to be used as variable names.
 *     The name is stored as shared string in u.string, the number
 *     of quotes is stored in x.quotes.
 *     If the number of quotes is reduced to 0, the lambda compiler
 *     will find/create a local variable with this name.
 *
 * Quoted Arrays (T_QUOTED_ARRAY svalue)
 *     Quoted arrays are needed to put array literals into lambda
 *     closures which usually treat arrays as code.
 *     u.vec is the reference to the array, x.quotes the number
 *     of quotes.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>

#include "closure.h"
#include "array.h"
#include "backend.h"
#include "exec.h"
#include "instrs.h"
#include "interpret.h"
#include "lex.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "prolang.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "structs.h"
#include "svalue.h"
#include "swap.h"
#include "switch.h"
#include "xalloc.h"
#include "pkg-python.h"
#include "i-svalue_cmp.h"

/*-------------------------------------------------------------------------*/

#define MAX_LAMBDA_LEVELS 0x8000;
  /* Maximum recursion depth for compile_value.
   */

#define SYMTAB_START_SIZE       16
  /* Initial number of entries in the work_area.symbols table.
   */

#define CODE_BUFFER_START_SIZE  1024
  /* Initial size of the work_area codebuffer.
   */

#define VALUE_START_MAX         0x20
  /* Initial number of value entries in the work_area.values table.
   */

/* Flags passed to resp. returned by compile_value().
 * They must fit into one bytecode_t (the #'? needs that)
 */
enum compile_value_input_flags
{
    ZERO_ACCEPTED      = 0x01, /* a return value of zero need not be coded */
    VOID_ACCEPTED      = 0x02, /* any return value can be left out */
    NEGATE_ACCEPTED    = 0x04, /* Caller accepts a reversed logic result */
    REF_ACCEPTED       = 0x08, /* lvalue references may be produced */
    LEAVE_LVALUE       = 0x10, /* For function calls, leave lvalue references. */
};

enum compile_value_output_flags
{
    VOID_GIVEN         = 0x01, /* no return value given */
    NEGATE_GIVEN       = 0x02, /* Result is in reversed logic */
    REF_GIVEN          = 0x04, /* Result will be an lvalue reference */
};

#define VOID_WANTED (ZERO_ACCEPTED | VOID_ACCEPTED | NEGATE_ACCEPTED)
  /* all "don't care for the result" flags.
   */


/* Flags passed to compile_lvalue()
 */
enum compile_lvalue_input_flags
{
    PROTECT_LVALUE     = 0x01, /* Protect the generated lvalue */
    MAKE_VAR_LVALUE    = 0x02, /* Make an variable lvalue */
                               /* (ignore any lvalues in the variable) */
    RESEATING_ACCEPTED = 0x04, /* Allow reseating references (&var). */
    ALLOW_FUNCTION_CALL= 0x08, /* Allow function calls (maybe-lvalues). */
};

#define UNIMPLEMENTED \
            lambda_error("Unimplemented - contact the maintainer\n");
  /* Guess :-)
   */

/*-------------------------------------------------------------------------*/
/* Types */

typedef struct symbol_s    symbol_t;
typedef struct work_area_s work_area_t;


/* --- struct lambda_replace_program_protecter ---
 *
 * If closures are bound to objects for which a program replacement
 * has been scheduled, a list of these structures, one for each closure,
 * is kept in replace_ob_s.lambda_rpp to hold the necessary information
 * to adjust the closures after the program has been replaced.
 *
 * The list is created by calls to lambda_ref_replace_program() and evaluated
 * in lambda_replace_program_adjust().
 */

struct lambda_replace_program_protector
{
    struct lambda_replace_program_protector *next;  /* The list link */
    svalue_t  l;      /* The closure bound, counted as reference */
    p_int     size;   /* 0, or for lambda()s the number of parameters in .args */
    vector_t *args;   /* If .size != 0: the parameter array of a lambda() */
    svalue_t  block;  /* If .size != 0: the lambda() body */
};


/* --- struct symbol_s: Symbolentry ---
 *
 * All encountered local symbols (arguments and variables) are stored using
 * this structure in a hashed symbol table in the work_area.
 */

struct symbol_s
{
    string_t *name;       /* Tabled name of the symbol (not counted as ref) */
    symbol_t *next;        /* Next symbol structure in hash list */
    symbol_t *next_local;
    int index;             /* Index number of this symbol, -1 if unassigned */
};


/* --- struct work_area_s: Information for the lambda compilation ---
 *
 * This structure holds all the information for a lambda compilation.
 * In theory this allows nested compilations, but this feature hasn't been
 * implemented yet.
 */

struct work_area_s
{
    symbol_t   **symbols;         /* Dynamic ashtable of lists of symbols */
    mp_int       symbol_max;      /* Allocated size of .symbols in byte */
    mp_int       symbol_mask;     /* Mask hashvalue -> .symbols index */
    mp_int       symbols_left;
      /* Size(!) of symbols left to enter into .symbols before the table
       * has to be enlarged.
       */
    bytecode_p   code;
      /* Memory block for the generated code, filled from the beginning */
    bytecode_p   codep;           /* First free bytecode in .code */
    mp_int       code_max;        /* Size of .code in byte */
    mp_int       code_left;       /* Unused .code left in byte */
    svalue_t    *values;
      /* Memory block for the values, filled from the end */
    svalue_t    *valuep;          /* Last value assigned in .values */
    mp_int       value_max;       /* Size of *.values in entries */
    mp_int       values_left;     /* Number of unused values */
    mp_int       num_locals;      /* Number of local vars, including args */
    mp_int       levels_left;
    object_t    *lambda_origin;   /* Object the lambda will be bound to */
    int          break_stack;     /* Current size of the break stack */
    int          max_break_stack; /* Max size of the break stack */
};
/* TODO: Use a pooled allocator for the memory held by the work_area (or
 * TODO:: all workareas?)
 */

/*-------------------------------------------------------------------------*/
/* Variables for the switch() code generator */

static Bool switch_initialized;
  /* TRUE if the case_blocks/case_state variables are valid.
   * Set to FALSE whenever a new lambda is compiled.
   */

case_state_t case_state;
  /* State of the current switch generation.
   */

static case_list_entry_t *case_blocks = NULL;
static case_list_entry_t *case_blocks_last = NULL;
  /* List of allocated case_list_entry_t blocks, freed in free_symbols().
   * The blocks are arrays [CASE_BLOCKING_FACTOR] of case_list_entry_t's,
   * and the first entry is used to link the blocks.
   * The blocks are used from the beginning, case_state.free_block points
   * the currently used block. There may be blocks following .free_block
   * which were generated during the compilation of nested switch()es.
   */

static case_list_entry_t *save_case_free_block;
static case_list_entry_t *save_case_next_free;
static case_list_entry_t *save_case_list0;
static case_list_entry_t *save_case_list1;
  /* Saved case_state entries .free_block, .next_free, .list0, .list1
   * from a LPC compilation. This happens when lambda() is called during
   * a compile, e.g. from an LPC error handler.
   * These values are too restored in free_symbols().
   */

/*-------------------------------------------------------------------------*/
/* Lambda compiler variables */

static work_area_t current
  = { 0, 0, 0, 0, 0, 0 };
  /* The current (and in this implementation only) work area.
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void lambda_error VARPROT((const char *error_str, ...), printf, 1, 2) NORETURN;
static void lambda_cerror(const char *s) FORMATDEBUG(printf,1,0) NORETURN;
static void lambda_cerrorl(const char *s1, const char *s2 UNUSED, int line1 UNUSED, 
                           int line2 UNUSED) NORETURN FORMATDEBUG(printf,1,0);
static void free_symbols(void);
static int compile_efun_call(ph_int type, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags);
static int compile_sefun_call(ph_int type, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags);
#ifdef USE_PYTHON
static int compile_python_efun_call(ph_int type, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags);
#endif
static int compile_lambda_call(ph_int type, svalue_t* closure, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags);
static Bool is_lvalue (svalue_t *argp, int flags);
static void compile_lvalue(svalue_t *, int);
static lambda_t * lambda (vector_t *args, svalue_t *block, object_t *origin);

/*-------------------------------------------------------------------------*/
static INLINE int
function_cmp (const string_t *name, const program_t *prog, int ix)

/* Compare <name> with the name of function number <ix> in <prog>ram.
 * Result:  0: <name> is equal to the indexed name
 *         >0: <name> is smaller
 *         <0: <name> is bigger
 *
 * Note that both names are directly tabled strings, so only the pointers are
 * compared.
 */

{
    function_t *header = get_function_header(prog, prog->function_names[ix]);

    /* Return the result of the comparison */
    /* Compare the two pointers.
     * The comparison operation has to match the one in prolang.y:epilog().
     */
    return memcmp( &name, &(header->name), sizeof name);
} /* function_cmp() */

/*-------------------------------------------------------------------------*/
long
find_function (const string_t *name, const program_t *prog)

/* Find the function <name> (a shared string) in the <prog>ram.
 * Result is the index of the function in the functions[] table,
 * or -1 if the function hasn't been found.
 */
{
    int i, o, d;  /* Testindex, Partitionsize, Comparisonresult */
    int size;     /* Number of functions */

    if ( !(size = prog->num_function_names) )
        return -1;

    /* A simple binary search */
    i = size >> 1;
    o = (i+2) >> 1;
    for (;;)
    {
        d = function_cmp(name, prog, i);
        if (d<0)
        {
            i -= o;
            if (i < 0)
            {
                i = 0;
            }
        }
        else if (d > 0)
        {
            i += o;
            if (i >= size)
            {
                i = size-1;
            }
        }
        else
        {
            return prog->function_names[i];
        }

        if (o <= 1)
        {
            if (function_cmp(name, prog, i))
                return -1;
            return prog->function_names[i];
        }
        o = (o+1) >> 1;
    }

    /* NOTREACHED */
    return -1;
} /* find_function() */

/*-------------------------------------------------------------------------*/
Bool
closure_eq (svalue_t * left, svalue_t * right)

/* Compare the two closure svalues <left> and <right> and return TRUE if
 * they refer to the same closure.
 */

{
    int i;

    /* Operator, Efun and Simul efun closures don't have a .u.lambda
     * part.
     */
    i = left->x.generic == right->x.generic;

    if (i
     && (   left->x.closure_type >= 0
         || right->x.closure_type >= 0)
       )
        i = left->u.lambda  == right->u.lambda;

    /* Lfun- and identifier closure can be equal even if
     * their pointers differ.
     */
    if (!i
     && left->x.closure_type == right->x.closure_type
     && (   left->x.closure_type == CLOSURE_LFUN
         || left->x.closure_type == CLOSURE_IDENTIFIER
        )
     && left->u.lambda->ob == right->u.lambda->ob
       )
    {
        if (left->x.closure_type == CLOSURE_LFUN)
        {
            i =    (   left->u.lambda->function.lfun.ob
                    == right->u.lambda->function.lfun.ob)
                && (   left->u.lambda->function.lfun.index
                    == right->u.lambda->function.lfun.index)
                && (   left->u.lambda->function.lfun.inhProg
                    == right->u.lambda->function.lfun.inhProg)
                && (    left->u.lambda->function.lfun.context_size
                     == right->u.lambda->function.lfun.context_size)
                ;

            if (i)
            {
                unsigned int context_size, ix;

                /* There might be a difference is in the context svalues.
                 * To prevent recursion, hide them while comparing them.
                 */

                context_size = left->u.lambda->function.lfun.context_size;
                left->u.lambda->function.lfun.context_size = 0;
                right->u.lambda->function.lfun.context_size = 0;

                for (ix = 0; i && ix < context_size; ix++)
                {
                    i = svalue_eq( &(left->u.lambda->context[ix])
                                 , &(right->u.lambda->context[ix])
                                 );
                }

                /* Restore the context size.
                 */
                left->u.lambda->function.lfun.context_size = context_size;
                right->u.lambda->function.lfun.context_size = context_size;
            }
        }
        else /* CLOSURE_IDENTIFIER */
        {
            i =    left->u.lambda->function.var_index
                == right->u.lambda->function.var_index;
        }
    }

    return (Bool)i;
} /* closure_eq() */

/*-------------------------------------------------------------------------*/
int
closure_cmp (svalue_t * left, svalue_t * right)

/* Compare the two closure svalues <left> and <right> and return a value
 * describing their relation:
 *  -1: <left> is 'smaller' than <right>
 *   0: the closures are equal
 *   1: <left> is 'greater' than <right>
 */

{
    if (closure_eq(left, right)) return 0;

    /* First comparison criterium is the closure_type */
    if (left->x.closure_type != right->x.closure_type)
    {
        return (left->x.closure_type < right->x.closure_type) ? -1 : 1;
    }

    /* The types are identical and determine the next comparison.
     * For lfun/identifier closure, we compare the actual closure data,
     * for other closures a comparison of the lambda pointer is sufficient.
     */
    if (left->x.closure_type == CLOSURE_IDENTIFIER
     || left->x.closure_type == CLOSURE_LFUN
       )
    {
        if (left->u.lambda->ob != right->u.lambda->ob)
        {
            return (left->u.lambda->ob < right->u.lambda->ob) ? -1 : 1;
        }

        if (left->x.closure_type == CLOSURE_LFUN)
        {
            unsigned context_size, i;
            int d;

            if ( left->u.lambda->function.lfun.ob
              != right->u.lambda->function.lfun.ob)
            {
                return (  left->u.lambda->function.lfun.ob
                        < right->u.lambda->function.lfun.ob)
                       ? -1 : 1;
            }

            if (   left->u.lambda->function.lfun.index
                != right->u.lambda->function.lfun.index
               )
                return (  left->u.lambda->function.lfun.index
                        < right->u.lambda->function.lfun.index)
                       ? -1 : 1;

            if (   left->u.lambda->function.lfun.inhProg
                != right->u.lambda->function.lfun.inhProg
               )
                return (  left->u.lambda->function.lfun.inhProg
                        < right->u.lambda->function.lfun.inhProg)
                       ? -1 : 1;

            /* The difference is in the context svalues.
             * To prevent recursion, hide them while comparing them.
             */
            if (   left->u.lambda->function.lfun.context_size
                != right->u.lambda->function.lfun.context_size
               )
                return (  left->u.lambda->function.lfun.context_size
                        < right->u.lambda->function.lfun.context_size)
                       ? -1 : 1;

            context_size = left->u.lambda->function.lfun.context_size;
            left->u.lambda->function.lfun.context_size = 0;
            right->u.lambda->function.lfun.context_size = 0;

            for (i = 0, d = 0; d == 0 && i < context_size; i++)
            {
                d = svalue_cmp( &(left->u.lambda->context[i])
                              , &(right->u.lambda->context[i])
                              );
            }

            /* Restore the context size, the return the comparison
             * result in d.
             */
            left->u.lambda->function.lfun.context_size = context_size;
            right->u.lambda->function.lfun.context_size = context_size;

            return d;
        }
        else /* CLOSURE_IDENTIFIER */
        {
            /* This is the only field left, so it is guaranteed to differ */
            return (  left->u.lambda->function.var_index
                    < right->u.lambda->function.var_index)
                   ? -1 : 1;
        }
    }

    /* Normal closure: compare the lambda pointers */
    return (left->u.lambda < right->u.lambda) ? -1 : 1;
} /* closure_cmp() */

/*-------------------------------------------------------------------------*/
Bool
lambda_ref_replace_program( object_t * curobj, lambda_t *l, int type
                          , p_int size, vector_t *args, svalue_t *block)

/* The lambda <l> of type <type> is about to be bound to the object <curobj>
 * which might be scheduled for program replacement.
 * If that is the case, a(nother) protector is added to replace_ob_s.lambda_rpp
 * and the function returns TRUE. Otherwise the function just returns FALSE.
 *
 * If <size> is not zero, it is the size of <args>, a vector with parameter
 * descriptions for a lambda(), and <block> holds the body of the lambda().
 * If <size> is zero, both <args> and <block> are undetermined.
 */

{
    replace_ob_t *r_ob;

    /* Search for a program replacement scheduled for the current
     * object.
     */
    for (r_ob = obj_list_replace; r_ob; r_ob = r_ob->next)
    {
        if (r_ob->ob == curobj)
        {
            /* Replacement found: add the protector */

            struct lambda_replace_program_protector *lrpp;

            l->ref++;
            lrpp = xalloc(sizeof *lrpp);
            lrpp->l.u.lambda = l;
            lrpp->l.x.closure_type = (short)type;
            lrpp->next = r_ob->lambda_rpp;
            r_ob->lambda_rpp = lrpp;
            if (size)
            {
                lrpp->size = size;
                lrpp->args = ref_array(args);
                assign_svalue_no_free(&lrpp->block, block);
            }

            return MY_TRUE;
        }
    } /* for() */

    /* No replacement found: return false */
    return MY_FALSE;
} /* lambda_ref_replace_program() */

/*-------------------------------------------------------------------------*/
void
set_closure_user (svalue_t *svp, object_t *owner)

/* Set <owner> as the new user of the closure stored in <svp> if the closure
 * is an operator-, sefun- or efun-closure, or if the closure is under
 * construction ("preliminary"). Finished lambda closures can't be rebound.
 *
 * Sideeffect: for preliminary closures, the function also determines the
 * proper svp->x.closure_type and updates the closures .function.index.
 */

{
    int type;        /* Type of the closure */

    if ( !CLOSURE_MALLOCED(type = svp->x.closure_type) )
    {
    	/* Operator-, sefun-, efun-closure: just rebind */

        free_object(svp->u.ob, "set_closure_user");
        svp->u.ob = ref_object(owner, "set_closure_user");
    }
    else if (type == CLOSURE_PRELIMINARY)
    {
    	/* lambda closure under construction: rebind, but take care
    	 * of possible program replacement
    	 */
    	
        int ix;
        lambda_t *l;
        funflag_t flags;
        program_t *prog;

        prog = owner->prog;
        l = svp->u.lambda;
        ix = l->function.lfun.index;

        /* If the program is scheduled for replacement (or has been replaced),
         * create the protector for the closure, otherwise mark the object
         * as referenced by a lambda.
         */
        if ( !(prog->flags & P_REPLACE_ACTIVE)
         || !lambda_ref_replace_program( owner, l
                                       , ix >= CLOSURE_IDENTIFIER_OFFS
                                         ? CLOSURE_IDENTIFIER
                                         : CLOSURE_LFUN
                                       , 0, NULL, NULL)
           )
        {
            owner->flags |= O_LAMBDA_REFERENCED;
        }

        /* Set the svp->x.closure_type to the type of the closure. */

        if (ix >= CLOSURE_IDENTIFIER_OFFS)
        {
            /* Identifier closure */
            ix -= CLOSURE_IDENTIFIER_OFFS;
            svp->x.closure_type = CLOSURE_IDENTIFIER;

            /* Update the closure index */
            l->function.var_index = (unsigned short)ix;
        }
        else
        {
            /* lfun closure. Be careful to handle cross-defined lfuns
             * correctly.
             */

            flags = prog->functions[ix];
            if (flags & NAME_CROSS_DEFINED)
            {
                ix += CROSSDEF_NAME_OFFSET(flags);
            }
            svp->x.closure_type = CLOSURE_LFUN;

            /* Update the closure index */
            l->function.lfun.ob = ref_object(owner, "closure");
            l->function.lfun.index = (unsigned short)ix;
            l->function.lfun.context_size = 0;
        }

        /* (Re)Bind the closure */
        free_object(l->ob, "closure");
        l->ob = ref_object(owner, "set_closure_user");
    }
} /* set_closure_user() */

/*-------------------------------------------------------------------------*/
void
free_replace_program_protector (replace_ob_t *r_ob)

/* In case a replace program is aborted, free the lambda adjustment
 * structure.
 */

{
    struct lambda_replace_program_protector *lrpp = r_ob->lambda_rpp;
    struct lambda_replace_program_protector *next_lrpp;

    for (; lrpp; lrpp = next_lrpp)
    {
        next_lrpp = lrpp->next;

        if (CLOSURE_HAS_CODE(lrpp->l.x.closure_type))
        {
            free_array(lrpp->args);
            free_svalue(&lrpp->block);
        }

        free_closure(&lrpp->l);
        xfree(lrpp);
    }
} /* free_replace_program_protector() */

/*-------------------------------------------------------------------------*/
int
replace_program_function_adjust (replace_ob_t *r_ob, int fun_idx)

/* Determines the new index of function <fun_idx> after program replacement.
 * If the function is not found in the new program, -1 is returned.
 */

{
    if (fun_idx >= r_ob->fun_offset
     && fun_idx < r_ob->fun_offset + r_ob->new_prog->num_functions)
    {
        /* Easy case, the lfun is within the new program's
         * block of the function table.
         */
        return fun_idx - r_ob->fun_offset;
    }

    {
        /* Let's see if there's a function in the new program's
         * block that is cross-defined to our lfun.
         */
        funflag_t* oldflags = r_ob->ob->prog->functions + r_ob->fun_offset;
        funflag_t* newflags = r_ob->new_prog->functions;

        for (int newlfunidx = 0; newlfunidx < r_ob->new_prog->num_functions; newlfunidx++)
        {
            /* It must be cross-defined in the old program to the lfun. */
            if (!(oldflags[newlfunidx] & NAME_CROSS_DEFINED)
             || newlfunidx + CROSSDEF_NAME_OFFSET(oldflags[newlfunidx]) != fun_idx)
                continue;

            /* And must not be cross-defined in the new program. */
            if (newflags[newlfunidx] & NAME_CROSS_DEFINED)
                continue;

            return newlfunidx;
        }
    }

    {
        /* Last try, let's look at virtual inherits.
         * Look at whether our lfun is in an virtual inherit
         * and if the target program has it, too.
         */

        int oldinhcount = r_ob->ob->prog->num_inherited;
        for (inherit_t* oldinh = r_ob->ob->prog->inherit; oldinhcount-- > 0; oldinh++)
        {
            if (oldinh->inherit_type == INHERIT_TYPE_NORMAL)
                continue;

            if (fun_idx >= oldinh->function_index_offset
             && fun_idx < oldinh->function_index_offset + oldinh->prog->num_functions)
            {
                int newinhcount = r_ob->new_prog->num_inherited;
                for (inherit_t* newinh = r_ob->new_prog->inherit; newinhcount-- > 0; newinh++)
                {
                    if (newinh->inherit_type == INHERIT_TYPE_NORMAL)
                        continue;
                    if (newinh->inherit_type & (INHERIT_TYPE_DUPLICATE|INHERIT_TYPE_MAPPED))
                        continue;
                    if (newinh->prog != oldinh->prog)
                        continue;

                    /* Yeah, we found it. */
                    return fun_idx - oldinh->function_index_offset + newinh->function_index_offset;
                }

                break;
            }
        }
    }

    return -1;

} /* replace_program_function_adjust() */

/*-------------------------------------------------------------------------*/
int
replace_program_variable_adjust (replace_ob_t *r_ob, int var_idx)

/* Determines the new index of variable <var_idx> after program replacement.
 * If the variable is not found in the new program, -1 is returned.
 */

{
    program_t *oldprog = r_ob->ob->prog;
    program_t *newprog = r_ob->new_prog;

    if (var_idx >= r_ob->var_offset
     && var_idx < r_ob->var_offset + newprog->num_variables - newprog->num_virtual_variables)
    {
        /* This is (at least in the new program) a regular variable, just adjust the index of the identifier.
         */
        return var_idx - r_ob->var_offset + newprog->num_virtual_variables;
    }
    else if (var_idx < oldprog->num_virtual_variables)
    {
        /* This is/was a virtual variable, we need to find the
         * corresponding inherit in the new program.
         */
        int oldinhcount = oldprog->num_inherited;
        for (inherit_t *oldinh = oldprog->inherit; oldinhcount-- > 0; oldinh++)
        {
            if (oldinh->inherit_type == INHERIT_TYPE_NORMAL)
                continue;
            if (oldinh->inherit_type & INHERIT_TYPE_DUPLICATE)
                continue;
            if (oldinh->inherit_type & INHERIT_TYPE_MAPPED)
            {
                int newinhcount;

                /* Obsolete virtual inherit. */
                if (var_idx < oldinh->variable_index_offset
                 || var_idx >= oldinh->variable_index_offset + oldinh->num_additional_variables)
                    continue;

                /* And this is an obsoleted variable. */
                newinhcount = newprog->num_inherited;
                for (inherit_t *newinh = newprog->inherit; newinhcount-- > 0; newinh++)
                {
                    program_t* oldupdprog;
                    int oldvaridx, origvaridx;

                    if (newinh->inherit_type == INHERIT_TYPE_NORMAL)
                        continue;
                    if (newinh->inherit_type & INHERIT_TYPE_DUPLICATE)
                        continue;
                    if (newinh->prog != oldinh->prog)
                        continue;

                    /* Determine the original variable index. */
                    oldupdprog = oldprog->inherit[oldinh->updated_inherit].prog;
                    oldvaridx = var_idx - oldinh->variable_index_offset + oldupdprog->num_variables - oldupdprog->num_virtual_variables;

                    for (origvaridx = oldinh->prog->num_variables - oldinh->prog->num_virtual_variables; origvaridx-- > 0;)
                    {
                        if (oldprog->update_index_map[oldinh->variable_map_offset + origvaridx] == oldvaridx)
                        {
                            if (newinh->inherit_type & INHERIT_TYPE_MAPPED)
                            {
                                /* It's obsolete in the new program as well. */
                                int updvaridx = newprog->update_index_map[newinh->variable_map_offset + origvaridx];
                                program_t * newupdprog = newprog->inherit[newinh->updated_inherit].prog;

                                if (updvaridx >= newupdprog->num_variables - newupdprog->num_virtual_variables)
                                    return newinh->variable_index_offset + updvaridx - (newupdprog->num_variables - newupdprog->num_virtual_variables);
                                else
                                    return newprog->inherit[newinh->updated_inherit].variable_index_offset + updvaridx;
                            }
                            else
                            {
                                return newinh->variable_index_offset + origvaridx;
                            }

                            /* NOTREACHED */
                        }
                    }

                    /* We did not find the original variable. That should not happen. */
                    return -1;
                }
            }
            else
            {
                int newinhcount, updinhcount;

                /* Regular virtual inherit. */
                if (var_idx < oldinh->variable_index_offset
                 || var_idx >= oldinh->variable_index_offset + oldinh->prog->num_variables - oldinh->prog->num_virtual_variables)
                    continue;

                /* We have found the entry, now search for the entry in the new program. */
                if (oldinh->prog == newprog)
                {
                    /* Oh, we are the virtual program. Then the variable is
                     * now in the block after our virtual variables.
                     */
                    return var_idx - oldinh->variable_index_offset + newprog->num_virtual_variables;
                }

                newinhcount = newprog->num_inherited;
                for (inherit_t *newinh = newprog->inherit; newinhcount-- > 0; newinh++)
                {
                    if (newinh->inherit_type == INHERIT_TYPE_NORMAL)
                        continue;
                    if (newinh->inherit_type & INHERIT_TYPE_DUPLICATE)
                        continue;
                    if (newinh->prog != oldinh->prog)
                        continue;

                    return var_idx - oldinh->variable_index_offset + newinh->variable_index_offset;
                }

                /* If we have come here, then we haven't found the exact program.
                 * Now we need to look for updated (obsolete) programs.
                 */
                updinhcount = oldprog->num_inherited;
                for (inherit_t *updinh = oldprog->inherit; updinhcount-- > 0; updinh++)
                {
                    if (updinh->inherit_type & INHERIT_TYPE_DUPLICATE)
                        continue;
                    if (!(updinh->inherit_type & INHERIT_TYPE_MAPPED))
                        continue;
                    if (updinh->updated_inherit != oldinh - oldprog->inherit)
                        continue;

                    newinhcount = newprog->num_inherited;
                    for (inherit_t *newinh = newprog->inherit; newinhcount-- > 0; newinh++)
                    {
                        int oldvaridx, newvaridx;

                        if (newinh->inherit_type == INHERIT_TYPE_NORMAL)
                            continue;
                        if (newinh->inherit_type & INHERIT_TYPE_DUPLICATE)
                            continue;
                        if (newinh->prog != updinh->prog)
                            continue;

                        /* Okay, we have the obsolete inherit in the new program.
                         * Get the original variable index.
                         */
                        oldvaridx = var_idx - oldinh->variable_index_offset;
                        for (newvaridx = newinh->prog->num_variables - newinh->prog->num_virtual_variables; newvaridx-- > 0;)
                        {
                            if (oldprog->update_index_map[updinh->variable_map_offset + newvaridx] == oldvaridx)
                                return newinh->variable_index_offset + newvaridx;
                        }

                        break;
                    }
                    break;

                }
            }

            /* We did not found the virtual inherit in the new program. */
            return -1;
        }
    }

    return -1;

} /* replace_program_variable_adjust() */

/*-------------------------------------------------------------------------*/
void
replace_program_lfun_closure_adjust (replace_ob_t *r_ob)

/* This function is called as one of the last steps during the replacement
 * of an object's program, but only if the object has been marked to hold
 * closure references.
 *
 * This function will handle lfun and variable closures. This will be
 * done just before the program is replaced.
 */

{
    struct lambda_replace_program_protector *lrpp;
      /* Current protector */

    /* Loop through the list of lambda protectors, adjusting
     * the lfun closures. Vanished lfun closures are replaced by
     * references to master::dangling_lfun_closure() if existing.
     * Vanished identifier closures are marked with a special value
     * and just vanish.
     * TODO: Store the name somehow for error messages/sprintf/to_string?
     *
     * This is done first because these are possible building blocks.
     */
    lrpp = r_ob->lambda_rpp;
    do {
        if ( !CLOSURE_HAS_CODE(lrpp->l.x.closure_type) )
        {
            /* Yup, it's an lfun or identifier */

            if (lrpp->l.x.closure_type == CLOSURE_LFUN)
            {
                lambda_t *l = lrpp->l.u.lambda;

                /* Adjust the index of the lfun
                 * If the lfun closure is a reference to an inherited
                 * program we need to check if the inheritance relation
                 * changes.
                 */

                int newidx = replace_program_function_adjust(r_ob, l->function.lfun.index);
                if (newidx < 0)
                {
                    /* If the function vanished, replace it with a default */
                    assert_master_ob_loaded();
                    free_object( l->function.lfun.ob, "replace_program_lambda_adjust");
                    if(l->function.lfun.inhProg)
                        free_prog(l->function.lfun.inhProg, MY_TRUE);

                    l->function.lfun.ob = ref_object(master_ob, "replace_program_lambda_adjust");
                    newidx = find_function( STR_DANGLING_LFUN, master_ob->prog);
                    l->function.lfun.index = (unsigned short)(newidx < 0 ? 0 : newidx);
                    l->function.lfun.inhProg = NULL;
                }
                else
                {
                    l->function.lfun.index = newidx;
                }

                /* For inherited lfuns we might have to adjust the program. */
                if (l->function.lfun.inhProg == r_ob->new_prog)
                {
                    /* Easy case: The new program is the same one the closure
                     * is pointing to. Just concert the closure into a
                     * straight lfun closure.
                     */
                     free_prog(l->function.lfun.inhProg, MY_TRUE);
                     l->function.lfun.inhProg = NULL;
                }
                else if (l->function.lfun.inhProg)
                {
                    program_t *prog = r_ob->new_prog;
                    int fx = l->function.lfun.index;

                    /* Checkt hat inhProg is still in the inherit chain.
                     * If not, convert the closure into a straight
                     * lfun closure.
                     */

                    while(prog != l->function.lfun.inhProg)
                    {
                        inherit_t *inheritp = prog->inherit;
                        int inhcount = prog->num_inherited;

                        for (; inhcount-- > 0; inheritp++)
                        {
                            if (fx >= inheritp->function_index_offset
                             && fx <  inheritp->function_index_offset + inheritp->prog->num_functions)
                                break;
                        }

                        if (inhcount < 0)
                        {
                            /* Didn't find it. */
                            free_prog(l->function.lfun.inhProg, MY_TRUE);
                            l->function.lfun.inhProg = NULL;
                            break;
                        }

                        fx -= inheritp->function_index_offset;
                        prog = inheritp->prog;
                    }
                }
            }
            else /* CLOSURE_IDENTIFIER */
            {
                lambda_t *l = lrpp->l.u.lambda;
                int newidx = replace_program_variable_adjust(r_ob, l->function.var_index);
                if (newidx < 0)
                {
                    l->function.var_index = VANISHED_VARCLOSURE_INDEX;
                    /* TODO: This value should be properly publicized and
                     * TODO:: tested.
                     */
                }
                else
                    l->function.var_index = newidx;
            }
        } /* if (!CLOSURE_HAS_CODE()) */
    } while ( NULL != (lrpp = lrpp->next) );

} /* replace_program_lfun_closure_adjust() */

/*-------------------------------------------------------------------------*/
void
replace_program_lambda_adjust (replace_ob_t *r_ob)

/* This function is called as one of the last steps during the replacement
 * of an object's program, but only if the object has been marked to hold
 * closure references.
 *
 * This function will handle lambda closures. It will be called just after
 * the program has been replaced.
 *
 * The function is called in the backend context and catches errors during
 * its execution.
 */

{
    struct lambda_replace_program_protector *lrpp;
      /* Current protector */

    struct lambda_replace_program_protector *next_lrpp;
      /* Next protector */

    static struct lambda_replace_program_protector *current_lrpp;
      /* Copy of lrpp, static to survive errors */

    struct error_recovery_info error_recovery_info;

    /* So, now adjust the lambda closures.
     * This is done by recompilation of every closure and comparison
     * with the original one. If the two closures differ, the closure
     * references now-vanished entities and has to be abandoned.
     *
     * In such a case, an error is generated and also caught: the code
     * for the closure is replaced by the instruction "undef" so that
     * accidental executions are caught.
     */

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_BACKEND;
    rt_context = (rt_context_t *)&error_recovery_info.rt;
    if (setjmp(error_recovery_info.con.text))
    {
        lrpp = current_lrpp;

        /* Replace the function with "undef" */
        lrpp->l.u.lambda->function.code.program[0] = F_UNDEF;

        /* Free the protector and all held values */
        free_array(lrpp->args);
        free_svalue(&lrpp->block);
        free_closure(&lrpp->l);

        next_lrpp = lrpp->next;
        xfree(lrpp);

        /* Restart the loop */
        lrpp = next_lrpp;
    }
    else
        /* Set lrpp to the first lambda to process.
         * (Doing it here makes gcc happy).
         */
        lrpp = r_ob->lambda_rpp;


    /* lrpp here is the next protector to handle, or NULL */

    if (lrpp) do
    {

    	/* If it's a lambda, adjust it */
        if (lrpp->l.x.closure_type == CLOSURE_LAMBDA)
        {
            lambda_t *l, *l2;      /* Original and recompiled closure */
            svalue_t *svp, *svp2;  /* Pointer to the two closure's values */
            mp_int num_values, num_values2, code_size2;

            current_lrpp = lrpp; /* in case an error occurs */

            /* Remember the original lambda, and also recompile it */
            l = lrpp->l.u.lambda;
            l2 = lambda(lrpp->args, &lrpp->block, l->ob);

            svp = (svalue_t *)l;
            num_values = l->function.code.num_values;

            svp2 = (svalue_t *)l2;
            num_values2 = l2->function.code.num_values;

            code_size2 = current.code_max - current.code_left;

            /* If the recompiled lambda differs from the original one, we
             * lose it.
             */
            if (num_values != num_values2 || lrpp->size != code_size2)
            {
                free_svalue(&lrpp->block);

                /* lrpp->block will be freed after the error, so lets fake
                 * a closure svalue and put the just-compiled closure in
                 * there.
                 */
                lrpp->block.type = T_CLOSURE;
                lrpp->block.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
                lrpp->block.u.lambda = l2;

                errorf("Cannot adjust lambda closure after replace_program(), "
                      "object %s\n", get_txt(r_ob->ob->name));
            }

            /* The recompiled lambda can be used (and has to: think changed
             * indices), so replace the original by the new one.
             * We have to keep the memory of the original one as other
             * code might already reference it.
             */
            while (--num_values >= 0)
                transfer_svalue(--svp, --svp2);
            memcpy(&l->function.code, &l2->function.code, sizeof(l->function.code) - 1 + (size_t)code_size2);

            /* Free the (now empty) memory */
            if  (l2->ob)
                free_object(l2->ob, "replace_program_lambda_adjust");
            if  (l2->prog_ob)
                free_object(l2->prog_ob, "replace_program_lambda_adjust");
            xfree(svp2);
            free_array(lrpp->args);
            free_svalue(&lrpp->block);
        }

        /* lambda or not, the protector is no longer needed */
        free_closure(&lrpp->l);
        next_lrpp = lrpp->next;
        xfree(lrpp);

    } while ( NULL != (lrpp = next_lrpp) );

    /* Restore the old error recovery info */
    rt_context = error_recovery_info.rt.last;

} /* replace_lambda_program_adjust() */

/*-------------------------------------------------------------------------*/
void
closure_init_lambda (lambda_t * l, object_t * obj)

/* Initialize the freshly created lambda <l> to be bound to object <obj>
 * (if given), and set the other generic fields (.ref, .prog_ob, .prog_pc).
 */

{
    l->ref = 1;
    if (current_prog)
    {
        l->prog_ob = ref_valid_object(current_prog->blueprint, "lambda creator");
        l->prog_pc = inter_pc - current_prog->program;
    }
    else
    {
        l->prog_ob = NULL;
        l->prog_pc = 0;
    }

    if (obj)
        l->ob = ref_object(obj, "lambda object");
    else
        l->ob = NULL;
} /* closure_init_lambda() */

/*-------------------------------------------------------------------------*/
lambda_t *
closure_new_lambda ( object_t * obj,  unsigned short context_size
                   , Bool raise_error)
/* Create a basic lambda closure structure, suitable to hold <context_size>
 * context values, and bound to <obj>. The structure has the generic
 * fields (.ref, .ob, .prog_ob, .prog_pc) initialized.
 *
 * The function may raise an error on out of memory if <raise_error> is TRUE,
 * or just return NULL.
 */

{
    lambda_t *l;

    /* Allocate a new lambda structure */
    l = xalloc(SIZEOF_LAMBDA(context_size));
    if (!l)
    {
        if (raise_error)
        {
            outofmem(SIZEOF_LAMBDA(context_size)
                    , "closure literal");
            /* NOTREACHED */
        }
    	return NULL;
    }

    closure_init_lambda(l, obj);

    return l;
} /* closure_new_lambda() */

/*-------------------------------------------------------------------------*/
void
closure_identifier (svalue_t *dest, object_t * obj, int ix, Bool raise_error)

/* Create a literal variable closure, bound to <obj> and with variable
 * index <ix>. The caller has to account for any variable offsets before
 * calling this  function.
 *
 * The created closure is stored as new svalue into *<dest>.
 *
 * The function may raise an error on out of memory if <raise_error> is TRUE,
 * or set *<dest> to svalue 0 else.
 */

{
    lambda_t *l;

    /* Allocate an initialise a new lambda structure */
    l = closure_new_lambda(obj, 0, raise_error);
    if (!l)
    {
        put_number(dest, 0);
        return;
    }

    /* If the object's program will be replaced, store the closure
     * in lambda protector, otherwise mark the object as referenced by
     * a closure.
     */
    if ( !(obj->prog->flags & P_REPLACE_ACTIVE)
     || !lambda_ref_replace_program( obj, l, CLOSURE_IDENTIFIER
                                   , 0, NULL, NULL)
       )
    {
        obj->flags |= O_LAMBDA_REFERENCED;
    }

    dest->x.closure_type = CLOSURE_IDENTIFIER;
    l->function.var_index = (unsigned short)ix;

    /* Fill in the rest of the lambda and of the result svalue */

    dest->type = T_CLOSURE;
    dest->u.lambda = l;
} /* closure_identifier() */

/*-------------------------------------------------------------------------*/
void
closure_lfun ( svalue_t *dest, object_t *obj, program_t *prog, int ix
             , unsigned short num
             , Bool raise_error)
/* Create a literal lfun closure, bound to the object <obj>. The resulting
 * svalue is stored in *<dest>.
 *
 * The closure is defined by the function index <ix>, for which the caller
 * has to make sure that all function offsets are applied before calling
 * this function. <ix> is relative to the object's program. <prog> is
 * the program used for the lookup of this function (but <ix> is nevertheless
 * the index into the object's function table, not neccessarily into the
 * function table of <prog>). <num> indicates the number of context variables
 * which are initialized to svalue-0.
 *
 * The function may raise an error on out of memory if <raise_error> is TRUE,
 * or set *<dest> to svalue 0 else.
 */

{
    lambda_t *l;

    /* Allocate and initialise a new lambda structure */
    l = closure_new_lambda(obj, num, raise_error);
    if (!l)
    {
        put_number(dest, 0);
        return;
    }

    /* If the object's program will be replaced, store the closure
     * in lambda protector, otherwise mark the object as referenced by
     * a closure.
     */
    if ( !(obj->prog->flags & P_REPLACE_ACTIVE)
     || !lambda_ref_replace_program( obj, l, CLOSURE_LFUN
                                   , 0, NULL, NULL)
       )
    {
        obj->flags |= O_LAMBDA_REFERENCED;
    }

    dest->x.closure_type = CLOSURE_LFUN;

    l->function.lfun.ob = ref_object(obj, "closure");
    l->function.lfun.index = (unsigned short)ix;
    l->function.lfun.inhProg = prog;
    if (prog)
        reference_prog(prog, "closure_lfun");
    l->function.lfun.context_size = num;

    /* Init the context variables */
    while (num > 0)
    {
        num--;
        put_number(&(l->context[num]), 0);
    }

    /* Fill in the rest of the lambda and of the result svalue */

    dest->type = T_CLOSURE;
    dest->u.lambda = l;
} /* closure_lfun() */

/*-------------------------------------------------------------------------*/
void
closure_literal ( svalue_t *dest
                , int ix, unsigned short inhIndex, unsigned short num)
/* Create a literal closure (lfun or variable closure), bound to the
 * current object. The resulting svalue is stored in *<dest>. The function
 * implements the instruction F_CLOSURE.
 *
 * The closure is defined by the index <ix>/<inhIndex>, which is to be
 * interpreted in the context of the current, possibly inherited, program:
 * values < CLOSURE_IDENTIFIER_OFFS are lfun indices, values above are
 * variable indices. For closures referencing inherited lfuns <inhIndex>
 * is the index+1 in the inherit list of <prog>. For lfun closures, <num>
 * indicates the number context variables which are initialized to svalue-0.
 *
 * The function may raise an error on out of memory.
 */

{

    if (ix >= CLOSURE_IDENTIFIER_OFFS)
    {
        ix += - CLOSURE_IDENTIFIER_OFFS
              /* The added difference takes into account that the
               * index is specified relative to the program which might
               * have been inherited.
               */
              + (current_variables - current_object->variables)
              /* But current_variables points to the non-virtual
               * variables, so adjusting for that...
               * (The lexer forbids closures to virtual variables,
               * so we don't have to look for them here. But
               * the variable index takes them into account.)
               */
              - current_prog->num_virtual_variables;

        closure_identifier(dest, current_object, ix, MY_TRUE);
    }
    else /* lfun closure */
    {
        funflag_t flags;
        program_t *prog;

        if (inhIndex)
        {
            /* inherited lfun closure */
            inherit_t *inh, *vinh;
            
            inh = &current_prog->inherit[inhIndex-1];
            /* Normalize pointers to functions of virtual inherits.
             * This is just for comparability of the closures.
             */
            vinh = adjust_variable_offsets(inh, current_prog, current_object);
            if (vinh)
                inh = vinh;
            
            prog = inh->prog;
            
            flags = prog->functions[ix];
            if (!vinh)
                ix += function_index_offset;
            ix += inh->function_index_offset;
        }
        else
        {
            ix += function_index_offset;
            flags = current_object->prog->functions[ix];
            prog = NULL;
        }

        if (flags & NAME_CROSS_DEFINED)
        {
            ix += CROSSDEF_NAME_OFFSET(flags);
        }
        closure_lfun(dest, current_object, prog, ix, num, MY_TRUE);
    }
} /* closure_literal() */

/*-------------------------------------------------------------------------*/
static void
realloc_values (void)

/* Double the size of the value block in the current workspace.
 * The function is called only when all values in the current block
 * have been assigned.
 *
 * Raise an error when out of memory.
 */

{
    mp_int new_max;
    svalue_t *new_values;

    new_max = current.value_max * 2;

    new_values = xalloc(new_max * sizeof(*new_values));
    if (!new_values)
        lambda_error("Out of memory (%"PRIdMPINT
                     " bytes) for %"PRIdMPINT" new values\n",
                     new_max, new_max * sizeof(*new_values));

    current.values_left += current.value_max;
    memcpy( (current.valuep = new_values + current.value_max)
          , current.values
          , current.value_max * sizeof(*new_values)
          );
    xfree(current.values);

    current.values = new_values;
    current.value_max = new_max;
} /* realloc_values() */

/*-------------------------------------------------------------------------*/
static void
realloc_code (void)

/* Double the size of the code block in the current workspace.
 *
 * Raise an error when out of memory.
 */

{
    mp_int new_max;
    bytecode_p new_code;
    ptrdiff_t curr_offset;

    curr_offset = current.codep - current.code;

    new_max = current.code_max * 2;
    new_code = rexalloc(current.code, (size_t)new_max);
    if (!new_code)
        lambda_error("Out of memory (%"PRIdMPINT" bytes) for new code\n", 
                     new_max);

    current.code_left += current.code_max;
    current.code_max = new_max;
    current.code = new_code;
    current.codep = current.code + curr_offset;
} /* realloc_code() */

/*-------------------------------------------------------------------------*/
static void
lambda_error(const char *error_str, ...)

/* Raise an errorf(error_str, ...) with 0 or 1 extra argument from within
 * the lambda compiler.
 *
 * The function takes care that all memory is deallocated.
 */

{
    va_list va;

    /* Deallocate all memory held in the work_areas */
    free_symbols();
    if (current.code)
        xfree(current.code);

    if (current.values)
    {
        mp_int num_values = current.value_max - current.values_left;
        svalue_t *svp;

        for (svp = current.valuep; --num_values >= 0; )
            free_svalue(svp++);
        xfree(current.values);
    }

    /* Now raise the error */
    va_start(va, error_str);
    errorf(error_str, va_arg(va, char *)); /* One arg or nothing :-) */
    /* TODO: a verror() would be handy here */
    va_end(va);
} /* lambda_error() */

/*-------------------------------------------------------------------------*/
static void
lambda_cerror (const char *s)

/* Callback for store_case_labels: raise an errorf(s) from within the
 * lambda compiler.
 *
 * The function takes care that all memory is deallocated.
 */

{
    lambda_error("%s\n", s);
} /* lambda_cerror() */

/*-------------------------------------------------------------------------*/
static void
lambda_cerrorl ( const char *s1, const char *s2 UNUSED
               , int line1 UNUSED, int line2 UNUSED)

/* Callback for store_case_labels(): Raise an errorf(s1) from within the lambda
 * compiler. store_case_labels() also passes line numbers and filename, but
 * when compiling a lambda that information is not very useful.
 *
 * The function takes care that all memory is deallocated.
 */

{
#ifdef __MWERKS__
#    pragma unused(s2,line1,line2)
#endif
    lambda_error(s1, "\n");
} /* lambda_errorl() */

/*-------------------------------------------------------------------------*/
static bytecode_p
lambda_get_space (p_int size)

/* Callback for store_case_labels(): Make space for <size> bytes in the
 * current code space and return the pointer to the first byte.
 *
 * Internally this function reallocates the code space when necessary.
 */

{
    while (current.code_left < size)
        realloc_code();
    current.code_left -= size;
    current.codep += size;
    return current.codep - size;
} /* lambda_get_space() */

/*-------------------------------------------------------------------------*/
static void
lambda_move_switch_instructions (int len, p_int blocklen)

/* Callback from store_case_labels(): Move the last instructions
 * of <blocklen> bytes forward by <len> bytes.
 */

{
    while (current.code_left < len)
        realloc_code();
    current.code_left -= len;
    current.codep += len;
    memmove( current.codep - blocklen
               , current.codep - blocklen - len
               , (size_t)blocklen
               );
} /* lambda_move_switch_instructions() */

/*-------------------------------------------------------------------------*/
static void
free_symbols (void)

/* Free the symbols in the current workarea, and also the memory allocated
 * for the case blocks.
 */

{
    p_int i;
    symbol_t **symp, *sym, *next;

    /* Free the symbols */
    i = current.symbol_max;
    symp = current.symbols;
    do {
        for (sym = *symp++; sym; sym = next)
        {
            next = sym->next;
            xfree(sym);
        }
    } while (i -= sizeof sym);

    xfree(current.symbols);

    /* Clean up the memory for the case blocks */
    if (switch_initialized)
    {
        if (current_loc.file)
        {
            case_state.free_block = save_case_free_block;
            case_state.next_free  = save_case_next_free;
            case_state.list0 = save_case_list0;
            case_state.list1 = save_case_list1;
        }
        else
        {
            free_case_blocks();
        }
    }
} /* free_symbols() */

/*-------------------------------------------------------------------------*/
static symbol_t *
make_symbol (string_t *name)

/* Look up the symbol <name> in the current symbol table and return the
 * pointer to the symbol_t structure. If <name> is not yet in the table,
 * a new structure is generated, linked in, and returned.
 *
 * If necessary, the symbol table is enlarged.
 */

{
    p_int h;
    symbol_t *sym, **symp;

    /* Hash the <name> pointer and look it up in the table.
     * TODO: This assumes 32-Bit ints.
     */
    h = (p_int)name;
    h ^= h >> 16;
    h ^= h >> 8;
    h ^= h >> 4;

    h &= current.symbol_mask;
    symp = (symbol_t **)((char *)current.symbols + h);
    for (sym = *symp; sym; sym = sym->next)
    {
        if (sym->name == name)
            return sym;
    }

    /* Not found: generate a new symbol entry and link it in */
    sym = xalloc(sizeof *sym);
    if (!sym)
        lambda_error("Out of memory (%lu bytes) for symbol\n"
                    , (unsigned long) sizeof(*sym));
    sym->name = name;
    sym->index = -1;
    sym->next = *symp;
    *symp = sym;

    /* Does the table has to be enlarged now? */
    if ( !(current.symbols_left -= sizeof sym) )
    {
    	/* Yup. Double the size of the hashtable and re-hash all
    	 * existing entries.
    	 */
    	
        symbol_t **newtab, *sym2;
        p_int i;

        sym2 = sym; /* Save the new entry */

        /* Allocate the new table and initialize it */
        current.symbols_left = current.symbol_max;
        current.symbol_max *= 2;
        symp = newtab = xalloc((size_t)current.symbol_max);
        if (!symp) {
            current.symbol_max /= 2;
            xfree(sym);
            lambda_error("Out of memory (%"PRIdMPINT" bytes) for symbol table\n"
                        , current.symbol_max);
        }
        current.symbol_mask = i = current.symbol_max - (long)sizeof sym;
        do {
            *symp++ = NULL;
        } while ((i -= sizeof sym) >= 0);

        /* Loop over the old table and all entries and rehash them
         * into the new table.
         * TODO: Again the hash assumes 32-Bit-ints.
         */
        i = current.symbols_left - (long)sizeof sym;
        do {
            symbol_t *next;

            for ( sym = *(symbol_t **)((char *)current.symbols+i)
                ; sym; sym = next)
            {
                next = sym->next;
                h = (p_int)sym->name;
                h ^= h >> 16;
                h ^= h >> 8;
                h ^= h >> 4;
                h &= current.symbol_mask;
                symp = (symbol_t **)((char *)newtab + h);
                sym->next = *symp;
                *symp = sym;
            }
        } while ((i -= sizeof sym) >= 0);

        /* Put the new table in place of the old one */
        xfree(current.symbols);
        current.symbols = newtab;

        sym = sym2; /* Restore the pointer to the new entry */
    }

    /* Return the new entry */
    return sym;
} /* make_symbol() */

/*-------------------------------------------------------------------------*/
static void
insert_value_push (svalue_t *value)

/* Add the <value> to the value block of the closure, and insert
 * the appropriate F_LAMBDA_(C)CONSTANT instruction to the compiled
 * code.
 */

{
    mp_int offset;  /* Index of the value in the value block */

    if (current.code_left < 3)
        realloc_code();

    offset = current.value_max - current.values_left;
    if (offset < 0xff)
    {
    	/* Less than 255 values: the short instruction */
    	
        current.code_left -= 2;
        STORE_CODE(current.codep, F_LAMBDA_CCONSTANT);
        STORE_UINT8(current.codep, (unsigned char)offset);
    }
    else
    {
    	/* More than 254 values: the long instruction */
    	
        if (offset == 0xff)
        {
            /* Offset #0xff will be used to hold the actual
             * number of values.
             */
            current.values_left--;
            offset++;
            (--current.valuep)->type = T_INVALID;
        }
        current.code_left -= 3;
        STORE_CODE(current.codep, F_LAMBDA_CONSTANT);
        STORE_SHORT(current.codep, offset);
    }

    if (--current.values_left < 0)
        realloc_values();

    /* Don't forget to copy the value itself */
    assign_svalue_no_free(--current.valuep, value);
} /* insert_value_push() */

/*-------------------------------------------------------------------------*/
static int
compile_value (svalue_t *value, enum compile_value_input_flags opt_flags)

/* Compile the <value> into a closure in the context of the current
 * work_area. <opt_flags> gives additional instructions about what
 * to accept or reject. The generated code is appended to the code
 * buffer in the work_area, the function itself returns a flag whether
 * the code leaves a result on the stack or not.
 *
 * The function calls itself recursively for nested code sequences.
 *
 * <value> can be of these types:
 *    array: a block of instructions in lisp-ish array notation.
 *    quoted array: inserted as is with one quote less
 *    symbol, 1 quote:   resolved as local variable/argument
 *    symbol, > 1 quote: inserted as symbol with one quote less
 *    other: inserted as is
 *
 * The return value may contain the following flags:
 *    VOID_GIVEN:
 *        The generated code will not leave any result on the stack.
 *        This will be only the case when VOID_ACCEPTED was in
 *        <opt_flags> (otherwise there will be code emittet to
 *        put something on the stack).
 *
 *    NEGATE_GIVEN:
 *        The value on the stack must be negated. The needed F_NOT
 *        was not emitted. This will be only the case when
 *        NEGATE_ACCEPTED or VOID_ACCEPTED was set in <opt_flags>.
 *        In the former case it is expected, the caller will negate
 *        the value (used to optimize to bytecodes into one), in
 *        the later case the value on the stack will be discarded
 *        anyway.
 *
 *    REF_GIVEN:
 *       The result on the stack will be a freshly created
 *       lvalue reference (&var).
 */

{
    enum compile_value_output_flags result_flags = 0;
    svalue_t *rvalue = get_rvalue(value, NULL);
    struct protected_range_lvalue* r = NULL;

    if (rvalue == NULL)
    {
        r = value->u.protected_range_lvalue;
        if (r->vec.type == T_POINTER)
            rvalue = &(r->vec);
        else
            rvalue = value;
    }

    if (!--current.levels_left)
        lambda_error("Too deep recursion inside lambda()\n");

    switch(rvalue->type)
    {
    case T_LVALUE:                             /* ----- T_LVALUE ----- */
      {
        /* Can only be a string/bytes range, otherwise get_rvalue
         * would have returned another type. And array ranges
         * were handled before.
         */
        svalue_t tmp;

        assert(value->x.lvalue_type == LVALUE_PROTECTED_RANGE);
        assert(r->vec.type == T_STRING || r->vec.type == T_BYTES);

        assign_rvalue_no_free(&tmp, value);
        insert_value_push(value);
        free_svalue(&tmp);
        break;
      }

    case T_POINTER:                            /* ----- T_POINTER ----- */
      {
        svalue_t *block;  /* The block of svalues to compile */
        svalue_t *argp;   /* Pointer to the current svalue */
        svalue_t *item;   /* The rvalue of argp. */
        ph_int type;      /* Various types */
        mp_int block_size;/* Number of elements in block .*/

        block = rvalue->u.vec->item;
        if (r != NULL)
        {
            argp = block + r->index1;
            block_size = (mp_int)(r->index2 - r->index1);
        }
        else
        {
            argp = block;
            block_size = (mp_int)VEC_SIZE(rvalue->u.vec);
        }

        /* The first value must be a closure */
        if (block_size == 0)
            lambda_error("Missing function\n");

        item = get_rvalue(argp, NULL);
        if (item == NULL || item->type != T_CLOSURE)
            lambda_error("Missing function\n");

        if ( (type = item->x.closure_type) < (ph_int)CLOSURE_SIMUL_EFUN)
        {
            /* Most common case: closure is an efun or an operator */

#ifdef USE_PYTHON
            if (type < (ph_int)CLOSURE_PYTHON_EFUN)
#else
            if (type < (ph_int)CLOSURE_EFUN)
#endif
            {
                /* Closure is an operator */

                switch (type - CLOSURE_OPERATOR)
                {
                default:
                    lambda_error("Unimplemented operator %s for lambda()\n",
                      instrs[type - CLOSURE_OPERATOR].name);

                /* ({ #'||, arg1, ...,  argn })
                 * ({ #'&&, arg1, ...,  argn })
                 */
                case F_LOR:
                case F_LAND:
                  {
                    /* For #'|| his is compiled into
                     *      <arg 1>
                     *      F_LAND end
                     *      <arg 2>
                     *      F_LAND end
                     *      ...
                     *      <arg n>
                     * end:
                     *
                     * If only the logical result is needed (VOID_ACCEPTED),
                     * F_LAND are replaced by F_BRANCH_ZERO. If the distance
                     * to end is too big, the F_LANDs are compiled as:
                     *
                     *      <arg i>
                     *      DUP
                     *      LBRANCH_ZERO end
                     *      POP
                     *      <arg i+1>
                     *
                     * respectively for the logical result:
                     *
                     *      <arg i>
                     *      LBRANCH_ZERO end
                     *      <arg i+1>
                     *
                     * Analog for F_LOR, here the branches are on _NON_ZERO.
                     */
                    mp_int *branchp;
                      /* Table storing the position of the branch/operator
                       * instruction after every compiled argument.
                       */
                    mp_int i;        /* most of the time: number of values left */
                    mp_int start;
                    mp_int end;      /* first free code byte */
                    Bool is_and;     /* TRUE if the operator is F_LAND */
                    int code;        /* Compiled instruction */
                    enum compile_value_output_flags void_given;

                    code = type - CLOSURE_OPERATOR;
                    is_and = code == (F_LAND);

                    /* If the caller doesn't need a return value,
                     * compile the operator as branches (much faster).
                     */
                    if (opt_flags & VOID_ACCEPTED)
                    {
                        code = is_and ? F_BRANCH_WHEN_ZERO
                                      : F_BRANCH_WHEN_NON_ZERO;
                        result_flags |= VOID_GIVEN;
                    }

                    /* Generate the code for the arguments but the last one.
                     * After every compiled argument, insert <code> and
                     * an empty byte and store the position of the inserted
                     * byte in the branchp table.
                     */
                    i = block_size - 1;
                    branchp = alloca(i * sizeof *branchp);
                    while (--i > 0)
                    {
                        compile_value(++argp, opt_flags & REF_ACCEPTED);
                        if (current.code_left < 2)
                            realloc_code();
                        *branchp++ = current.code_max - current.code_left;
                        current.code_left -= 2;
                        PUT_CODE(current.codep, (bytecode_t)code);
                        current.codep += 2;
                    }

                    /* If i is != 0 here, no arguments were given.
                     * In that case, fake a result, otherwise compile the
                     * last argument.
                     */
                    if (i)
                        void_given = compile_value(is_and ? &const1 : &const0
                                         , opt_flags & (VOID_ACCEPTED|REF_ACCEPTED)
                                     );
                    else
                        void_given = compile_value(++argp
                                         , opt_flags & (VOID_ACCEPTED|REF_ACCEPTED)
                                     );

                    /* If the caller accepts void, but we compiled a result,
                     * remove it from the stack.
                     */
                    if (opt_flags & VOID_ACCEPTED && !(void_given & VOID_GIVEN))
                    {
                        if (current.code_left < 1)
                            realloc_code();
                        current.code_left--;
                        STORE_CODE(current.codep, F_POP_VALUE);
                    }

                    /* Walk backwards through the generated code segments
                     * and store the correct offsets for the operator/branch
                     * instructions. If necessary, the short branches are
                     * converted into long ones.
                     */
                    i = block_size - 1;
                    end = current.code_max - current.code_left;
                      /* The target to jump to */
                    while (--i > 0)
                    {
                        mp_int offset;

                        start = *--branchp;
                        offset = end - start - 2;
                        if (offset <= 0xff)
                        {
                            PUT_UINT8(current.code+start+1, (unsigned char)offset);
                            continue;
                        }
                        else
                        {
                            /* We exceeded the limit of the short offsets.
                             * Prepare the extension of the remaining offsets
                             * to long offsets.
                             */

                            mp_int growth;        /* Additional bytes needed */
                            int    growth_factor; /* Additional byte per branch */
                            mp_int j;
                            bytecode_p p, q;      /* Src/Dest for code copying */

                            if (opt_flags & VOID_ACCEPTED)
                            {
                            	/* We don't need a result: just change the
                            	 * short into long branches.
                            	 */
                                growth = i;
                                growth_factor = 1;
                                code = is_and ? F_LBRANCH_WHEN_ZERO
                                              : F_LBRANCH_WHEN_NON_ZERO;
                            }
                            else
                            {
                            	/* We need a result: change the OP instructions
                            	 * into OP/LBRANCH combinations.
                            	 */
                                growth = i * 3;
                                growth_factor = 3;
                                code = is_and ? F_LBRANCH_WHEN_ZERO
                                              : F_LBRANCH_WHEN_NON_ZERO;
                            }

                            /* Prepare the copying of the code */
                            if (current.code_left < growth)
                                realloc_code();
                            current.code_left -= growth;
                            current.codep += growth;
                            p = current.code + end;
                            q = p + growth;
                            end += growth_factor - 1;
                                /* - 1 is precompensation for leading branch code */
                            if ( !(opt_flags & VOID_ACCEPTED) )
                                /* offset precompensation for leading F_DUP */
                                end--;

                            /* Restart the walk through the branchpoints */
                            branchp++;
                            do {
                                start = *--branchp;
                                offset = end - start;
                                end += growth_factor;
                                if (offset > 0x7fff)
                                    UNIMPLEMENTED

                                /* Move the code from here back to the branch
                                 * point.
                                 */
                                j = p - (bytecode_p)&current.code[start+2];
                                do {
                                    *--q = *--p;
                                } while (--j);

                                /* Generate the new branch instructions instead
                                 * of copying the old.
                                 */
                                p -= 2;
                                if (opt_flags & VOID_ACCEPTED)
                                {
                                    RSTORE_SHORT(q, offset);
                                    RSTORE_CODE(q, (bytecode_t)code);
                                }
                                else
                                {
                                    RSTORE_CODE(q, F_POP_VALUE);
                                    RSTORE_SHORT(q, offset);
                                    RSTORE_CODE(q, (bytecode_t)code);
                                    RSTORE_CODE(q, F_DUP);
                                }
                            } while (--i > 0);
                            break; /* outer while(), it's finished anyway */
                        }
                    } /* while(--i > 0); */
                    break;
                  }

                /* ({#'?, expr, cond-part, ..., default-part })
                 * ({#'?!, expr, cond-part, ..., default-part })
                 */
                case F_BRANCH_WHEN_ZERO:
                case F_BRANCH_WHEN_NON_ZERO:
                  {
                    /* For #'? is compiled into:
                     *
                     *   result required:           no result required:
                     *
                     *       <expr1>                    <expr1>
                     *       BRANCH_ZERO l1             BRANCH_ZERO l1
                     *       <cond1>                    <cond1>
                     *       BRANCH vd/nvd              BRANCH vd/nvd
                     * l1:   <expr2>              l1:   <expr2>
                     *       BRANCH_ZERO l2             BRANCH_ZERO l2
                     *       <cond2>                    <cond2>
                     *       BRANCH vd/nvd              BRANCH vd/nvd
                     * l2:   <expr3>              l2:   <expr3>
                     *       ...                        ...
                     * ln-1: <exprn>              ln-1: <exprn>
                     *       BRANCH_ZERO ln             BRANCH_ZERO ln
                     *       <condn>                    <condn>
                     *       BRANCH vd/nvd              BRANCH vd/nvd
                     * ln:   <default>            ln:   <default>
                     * nvd:  BRANCH +1            nvd:  POP
                     * vd:   CONST0               vd:
                     *
                     * (vd: void_dest, nvd: non_void_dest)
                     *
                     * The branch conditions after every <expr> are reversed
                     * for #'?! and/or if the <expr> returns a result in
                     * reverse logic. And of course the F_BRANCHes are converted
                     * into F_LBRANCHes where necessary.
                     *
                     * If <default> is required but not given, CONST0 is
                     * inserted in its place. In that case, the branches from
                     * <cond>s without a result are directed to that one CONST0
                     * as well.
                     *
                     * There are few other ways to compile the end of the
                     * sequence if no <default> is required and/or not given,
                     * or if no result is required - they are explained
                     * below.
                     */

                    mp_int *branchp;
                      /* Table storing two values for every argument pair: the
                       * position after the cond-part and the position after
                       * the cond. Yes, in reverse order.
                       */
                    mp_int i;
                    mp_int start;
                    mp_int end;
                    mp_int void_dest;     /* branch dest with no result */
                    mp_int non_void_dest; /* branch dest with a result */
                    Bool is_notif;        /* TRUE if this is #'?! */
                    int code;             /* The instruction to compile to */
                    enum compile_value_output_flags opt_used;
                                          /* Current compile() result */
                    enum compile_value_output_flags all_void;
                                          /* !0 if  cond-parts returns a value */
                    mp_int last_branch;   /* Position of branch after cond */

                    non_void_dest = 0;
                    code = type - CLOSURE_OPERATOR;
                    is_notif = (code == F_BRANCH_WHEN_NON_ZERO);

                    /* If the default part exists, is the number 0 or at least
                     * has no side-effects, and if the caller accepts void/0
                     * for an answer, it is not compiled as it won't have
                     * any effect anyway.
                     */
                    if (!(block_size & 1)
                      && (opt_flags & (VOID_ACCEPTED|ZERO_ACCEPTED)))
                    {
                       item = get_rvalue(argp + block_size - 1, NULL);
                       if (!item)
                           item = &(argp->u.protected_range_lvalue->vec);
                       if (( opt_flags & VOID_ACCEPTED )
                          ? item->type != T_POINTER /* no side effect */
                          : (item->type == T_NUMBER && !item->u.number))
                        {
                            /* Ignore the default-part by hiding it */
                            block_size--;
                        }
                    }

                    /* Generate the code for the (cond, cond-part) pairs,
                     * and add the necessary branch instructions.
                     * Also store the positions of the inserted code
                     * in the branchp table.
                     */
                    i = block_size;
                    branchp = alloca(i * sizeof *branchp);
                    all_void = VOID_GIVEN;
                    while ( (i -= 2) > 0)
                    {
                        mp_int offset;

                        /* Compile the condition and add the branch
                         * to skip the cond-part.
                         */
                        opt_used = compile_value(++argp, NEGATE_ACCEPTED);
                        if (current.code_left < 2)
                            realloc_code();
                        last_branch = current.code_max - current.code_left;
                        current.code_left -= 2;
                        if (opt_used & NEGATE_GIVEN)
                            STORE_CODE(current.codep
                              , (bytecode_t)
                                (is_notif ? F_BRANCH_WHEN_ZERO
                                          : F_BRANCH_WHEN_NON_ZERO)
                                       );
                        else
                            STORE_CODE(current.codep, (bytecode_t)code);
                        STORE_UINT8(current.codep, 0);

                        /* Compile the cond-part */
                        ++argp;
                        opt_used = compile_value(argp,
                            (i == 1 && !all_void) ?
                                opt_flags & REF_ACCEPTED :
                                opt_flags &
                                  (VOID_ACCEPTED|ZERO_ACCEPTED|REF_ACCEPTED)
                          );
                        all_void &= (opt_used&VOID_GIVEN);
                        if (current.code_left < 4)
                            realloc_code();

                        /* Now that we know the size of the cond-part, store
                         * the branch offset into the branch instruction
                         * before the cond-part.
                         */
                        offset =
                          current.code_max - current.code_left - last_branch;

                        /* Make sure that the offset won't overflow
                         * when incremented later during backpatching.
                         */
                        if (offset > 0xfe)
                        {
                            /* The cond-part was too big, we have to change
                             * the 2-Byte F_BRANCH_ into an 3-Byte F_LBRANCH.
                             */
                            bytecode_p p;
                            mp_int j;

                            /* Move the cond-part one byte forward */
                            p = current.codep++;
                            j = offset - 2;
                            if (offset > 0x7ffd)
                                UNIMPLEMENTED
                            do {
                                p--;
                                p[1] = *p;
                            } while (--j);

                            current.code_left--;
                            if (current.code[last_branch] == F_BRANCH_WHEN_ZERO)
                                PUT_CODE(current.code+last_branch
                                        , F_LBRANCH_WHEN_ZERO);
                            else
                                PUT_CODE(current.code+last_branch
                                        , F_LBRANCH_WHEN_NON_ZERO);
                            PUT_SHORT(current.code+last_branch+1, offset+2);
                        }
                        else
                        {
                            /* The offset fits, just store it */
                            PUT_UINT8(current.code+last_branch+1
                                     , (unsigned char)offset);
                        }

                        /* Store the two branch positions */
                        *branchp++ = current.code_max - current.code_left;
                        *branchp++ = last_branch;

                        /* Add the unconditional branch. In place of the
                         * offset we store the opt_used flags so that the
                         * later backpatching run knows exactly what the cond-part
                         * left on the stack.
                         */
                        current.code_left -= 2;
                        STORE_CODE(current.codep, F_BRANCH);
                        STORE_CODE(current.codep, (bytecode_t)opt_used);
                    } /* while() */
                    /* If i is not zero now, then there is no default part */

                    /* Now compile the default part.
                     * There are a few conditions to distinguish...
                     */
                    if ( i /* no default */
                     &&  (   opt_flags & VOID_ACCEPTED
                          || (all_void && opt_flags & ZERO_ACCEPTED)
                         ) )
                    {
                    	/* There is no default part, and the caller doesn't
                    	 * want a result or accepts a zero when we don't
                    	 * have one.
                    	 */
                    	
                        mp_int offset;  /* corrective offset for the branch after
                                         * the last cond
                                         */

                        result_flags |= VOID_GIVEN;
                        if (all_void)
                        {
                            /* No cond-part returned a result, just remove
                             * the last F_BRANCH.
                             * The code sequence is therefore:
                             *
                             *       <expr1>
                             *       BRANCH_ZERO l1
                             *       <cond1>
                             *       BRANCH end
                             * l1:   <expr2>
                             *       ...
                             * ln-1: <exprn>
                             *       BRANCH_ZERO ln
                             *       <condn>
                             * ln: end:
                             */
                            if (block_size < 2)
                            {
                            	/* empty statement: all done */
                                break; /* switch() */
                            }
                            offset = -2;
                            void_dest =
                              current.code_max - current.code_left - 2;
                        }
                        else
                        {
                            /* Some cond-parts returned a result: let them
                             * jump to a POP statement.
                             * The code sequence is therefore:
                             *
                             *       <expr1>
                             *       BRANCH_ZERO l1
                             *       <cond1>
                             *       BRANCH vd/nvd
                             * l1:   <expr2>
                             *       ...
                             * ln-1: <exprn>
                             *       BRANCH_ZERO ln
                             *       <condn>
                             * nvd:  POP
                             * ln: vd:
                             *
                             * TODO: Uhm what if <condn> is void?
                             */
                            /* Terminating void after non-void is avoided */
                            current.codep[-2] = F_POP_VALUE;
                            offset = -1;
                            non_void_dest =
                              current.code_max - current.code_left - 2;
                            void_dest = non_void_dest + 1;
                        }

                        /* Now rewrite the BRANCH_ZERO ln according to offset */

                        start = *--branchp;
                        code = GET_CODE(current.code+start);
                        if (code == F_LBRANCH_WHEN_ZERO
                         || code == F_LBRANCH_WHEN_NON_ZERO)
                        {
                            unsigned short old_offset;

                            GET_SHORT(old_offset, current.code+start+1);
                            PUT_SHORT(current.code+start+1, old_offset+offset);
                        }
                        else
                        {
                            put_uint8(current.code+start+1
                                     , (int8_t)get_uint8(current.code+start+1) + offset);
                        }

                        /* Prepare for the backpatching run */
                        current.codep += offset;
                        current.code_left -= offset;
                        branchp--;
                        i = block_size - 2;
                    }
                    else
                    {
                    	/* We may or may not have a default part, but
                    	 * the caller expects a result.
                    	 */
                    	
                        /* the following assignment is only valid if
                         *
                         *   ( !all_void && i "no default" &&
                         *   ( (opt_flags & (VOID_ACCEPTED|ZERO_ACCEPTED)) ==
                         *     ZERO_ACCEPTED) )
                         *
                         * is met, and it is only needed when there is at
                         * least one void expression, too.
                         * However, it's easier to do the assignment
                         * all the time, and it does no harm here.
                         * The effect is that the 'const0' default synthesized
                         * will be used as result from the cond-part, too.
                         */
                        void_dest = current.code_max - current.code_left;

                        /* Compile the default part */
                        opt_used = compile_value(
                          i ? &const0 : ++argp,
                          opt_flags &
                            ( all_void ?
                              (VOID_ACCEPTED|ZERO_ACCEPTED|REF_ACCEPTED) :
                              REF_ACCEPTED
                            )
                        );

                        /* <cond>s with a result of course want to branch
                         * after the <default>.
                         */
                        non_void_dest = current.code_max - current.code_left;

                        if (opt_used & VOID_GIVEN)
                        {
                            /* Whoops, <default> didn't return a result.
                             * Prepare to insert a default, and let the
                             * void-<cond>s branch here, too.
                             */
                            void_dest = non_void_dest;
                            result_flags |= VOID_GIVEN;
                        }
                        else if (opt_flags & VOID_ACCEPTED)
                        {
                            /* We have a result, but the caller doesn't want
                             * it: add the code sequence
                             *
                             *   nvd: POP
                             *   vd:
                             */
                            result_flags |= VOID_GIVEN;
                            if (current.code_left < 1)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_POP_VALUE);
                            opt_used = VOID_GIVEN;
                            void_dest = non_void_dest + 1;
                        }
                        else if (all_void && block_size > 2)
                        {
                            /* The caller wants a result, <default> has one,
                             * but none of the <cond>s does (and they exist).
                             */
                            if (current.code_left < 3)
                                realloc_code();
                            if (block_size > 4
                             || branchp[-2] - branchp[-1] > 0xfd)
                            {
                            	/* There is more than one <cond>, or the one
                            	 * <cond> alone needs a long branch: add
                            	 *
                            	 *    nvd: BRANCH +1
                            	 *    vd:  CONST0
                            	 */
                                void_dest = non_void_dest + 2;
                                current.code_left -= 3;
                                STORE_CODE(current.codep, F_BRANCH);
                                STORE_UINT8(current.codep, 1);
                                STORE_CODE(current.codep, F_CONST0);
                            }
                            else
                            {
                            	/* Just one <cond>: replace the 'BRANCH end'
                            	 * by 'CONST0; BRANCH end'.
                            	 */
                                bytecode_p p;

                            	/* Make space for the CONST0 */
                                current.code_left--;
                                start = branchp[-2];
                                memmove(
                                  &current.code[start+1],
                                  &current.code[start],
                                  (size_t)(non_void_dest - start)
                                );
                                current.codep++;

                                /* Add the CONST0 instruction */
                                PUT_CODE(current.code+start, F_CONST0);

                                /* Set the saved opt_flags to 'not void' */
                                PUT_UINT8(current.code+start+2, 0);

                                /* Increment the branch offset for the branch
                                 * skipping the <cond>
                                 */
                                p = current.code+branchp[-1]+1;
                                PUT_UINT8(p, GET_UINT8(p)+1);

                                /* Update the stored position */
                                branchp[-2] = start+1;

                                non_void_dest++;
                                /* void_dest = start; */
                                /* all_void isn't used any more, else we'd
                                 * need to zero it now.
                                 */
                            }
                        }
                        else if (!i && !all_void
                              && opt_flags & ZERO_ACCEPTED)
                        {
                            /* We had a real <default> with result, there are
                             * some <cond> which return a result, and the
                             * caller accepts a zero for void-<conds>: add
                             *   nvd: BRANCH +1
                             *   vd:  CONST0
                             * if there are void-<conds>.
                             */
                            mp_int *branchp2, j;

                            /* Check all <cond>s if there is a void one */
                            branchp2 = branchp;
                            for (j = block_size;  (j -= 2) > 0; )
                            {
                                start = *(branchp2 -= 2);
                                if (current.code[start+1] & VOID_GIVEN)
                                {
                                    /* Yup, we need the extra code.
                                     */
                                    void_dest = non_void_dest + 2;
                                    non_void_dest += 3;
                                    if (current.code_left < 3)
                                        realloc_code();
                                    current.code_left -= 3;
                                    STORE_CODE(current.codep, F_BRANCH);
                                    STORE_UINT8(current.codep, 1);
                                    STORE_CODE(current.codep, F_CONST0);
                                    break;
                                }
                            }
                        }

                        /* Prepare the backpatching run */
                        i = block_size;
                    }

                    /* Now walk backwards through all the <cond>branches, insert
                     * the proper offset and rewrite them to long branches where
                     * necessary.
                     */
                    end = current.code_max - current.code_left;
                    while ( (i -= 2) > 0)
                    {
                        mp_int offset;

                        /* Compute the distance to branch */
                        start = *(branchp -= 2);
                        offset = (GET_UINT8(current.code+start+1) & VOID_GIVEN)
                                 ? void_dest - start - 2
                                 : non_void_dest - start - 2;

                        if (offset <= 0xff)
                        {
                            /* A short branch is sufficient. */
                            PUT_UINT8(current.code+start+1, (bytecode_t)offset);
                            continue;
                        }
                        else
                        {
                            /* We have to rewrite this and all previous
                             * branches to long branches.
                             */
                            mp_int growth;  /* (Current) offset from old pos. */
                            mp_int j;
                            bytecode_p p, q;

                            /* Determine how much more is needed and allocate
                             * the memory
                             */
                            growth = (i+1) >> 1;
                            if (current.code_left < growth)
                                realloc_code();
                            current.code_left -= growth;
                            current.codep += growth;

                            /* Now move the code, starting from the end,
                             * and rewriting the branches when we encounter
                             * them.
                             * The first move will move all the code up to
                             * the end, the next move just the code up to
                             * the following <cond>.
                             * The offset from the old position is given
                             * by (q-p).
                             */
                            p = current.code + end;
                            q = p + growth;

                            branchp += 2; /* have to reconsider this one */
                            do {
                            	unsigned short dist;
                            	bytecode_p pstart;

                                /* First, increment the distance of the
                                 * branch skipping the previous <cond> (it might
                                 * already be a long branch).
                                 */
                                start = *--branchp;
                                pstart = current.code+start;
                                code = GET_CODE(pstart);
                                if (code == F_LBRANCH_WHEN_ZERO
                                 || code == F_LBRANCH_WHEN_NON_ZERO)
                                {
                                    GET_SHORT(dist, pstart+1);
                                    PUT_SHORT(pstart+1, dist+1);
                                }
                                else
                                {
                                    PUT_UINT8(pstart+1, GET_UINT8(pstart+1)+1);
                                }

                                /* Compute the distance for the <cond> branch */
                                start = *--branchp;
                                offset = (current.code[start+1] & VOID_GIVEN)
                                          ? void_dest - start - 1
                                          : non_void_dest - start - 1;

                                /* Count the extra byte we're going to insert */
                                end++;
                                void_dest++;
                                non_void_dest++;

                                if (offset > 0x7fff)
                                    UNIMPLEMENTED

                                /* Compute the distance to store while q and p
                                 * give the proper offset.
                                 */
                                dist = (unsigned short)(offset + (q-p));

                                /* Move the code after this branch.
                                 */
                                j = (p - (current.code + start)) - 2;
                                do {
                                    *--q = *--p;
                                } while (--j);

                                /* Store the new branch in place of the old one. */
                                RSTORE_SHORT(q, dist);

                                p -= 2;
                                code = GET_CODE(p);
                                if (code == F_BRANCH_WHEN_ZERO)
                                    RSTORE_CODE(q, F_LBRANCH_WHEN_ZERO);
                                else if (code == F_BRANCH_WHEN_NON_ZERO)
                                    RSTORE_CODE(q, F_LBRANCH_WHEN_NON_ZERO);
                                else if (code == F_BRANCH)
                                    RSTORE_CODE(q, F_LBRANCH);
                                else
                                    fatal("Can't rewrite %s (%02x) at %p\n"
                                         , get_f_name(code), code, p);
                            } while ( (i -= 2) > 0);
                            break; /* outer while() - it's finished anyway */
                        }
                    } /* while() backpatching */
                    break;
                  }

                /* ({#',, <expr1>, <expr2>, ..., <exprn> })
                 */
                case F_POP_VALUE:
                  {
                    /* This is compiled as:
                     *
                     *    <expr1>
                     *    POP
                     *    <expr2>
                     *    POP
                     *    ...
                     *    POP
                     *    <exprn>
                     *
                     * If an expression doesn't return a value, the following
                     * POP is omitted.
                     *
                     * If no expression is given, 'CONST0' is compiled.
                     */

                    mp_int i;
                    enum compile_value_output_flags void_given;

                    /* Compile the first n-1 expressions */
                    for (i = block_size - 1; --i > 0; )
                    {
                        void_given = compile_value(++argp, VOID_WANTED|REF_ACCEPTED);

                        /* If we got a result, pop it */
                        if ( !(void_given & VOID_GIVEN) )
                        {
                            if (current.code_left < 1)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_POP_VALUE);
                        }
                    }

                    /* Compile the last expression.
                     * If there is none (i != 0), use CONST0 instead.
                     */
                    result_flags = compile_value(i ? &const0 : ++argp, opt_flags);
                    break;
                  }

                /* ({#'=, <lvalue1>, <expr1>, ..., <lvaluen>, <exprn> })
                 */
                case F_ASSIGN:
                  {
                    /* This is compiled as:
                     *
                     *   <expr1>
                     *   <lvalue1>
                     *   VOID_ASSIGN
                     *   <expr2>
                     *   <lvalue2>
                     *   VOID_ASSIGN
                     *   ...
                     *   <exprn>
                     *   <lvaluen>
                     *   ASSIGN
                     *
                     * If the caller doesn't require a result, the last
                     * ASSIGN is compiled as VOID_ASSIGN.
                     */

                    mp_int i;

                    /* There must be at least one assignment in order to get
                     * a return value.
                     */
                    if ( !(i = block_size - 1) || (i & 1) )
                        lambda_error("Missing value in assignment\n");
                    argp++;
                    for (; (i -= 2) >= 0; argp+=2)
                    {
                        compile_value(argp+1, REF_ACCEPTED);
                        compile_lvalue(argp, RESEATING_ACCEPTED);
                        if (!i)
                        {
                            /* Last assignment: we might need to keep this value */
                            if (opt_flags & VOID_ACCEPTED)
                            {
                                result_flags = VOID_GIVEN;
                                STORE_CODE(current.codep, F_VOID_ASSIGN);
                            }
                            else
                            {
                                STORE_CODE(current.codep, F_ASSIGN);
                            }
                        }
                        else
                        {
                            /* First assignemnts: forget the value */
                            STORE_CODE(current.codep, F_VOID_ASSIGN);
                        }
                        current.code_left--;
                    }
                    break;
                  }

                /* ({#'+=, <lvalue>, <expr> })
                 */
                case F_ADD_EQ:
                    /* This is compiled as:
                     *
                     *   <expr>
                     *   <lvalue>
                     *   (VOID_)ADD_EQ
                     *
                     * For the special case <expr> == 1:
                     *
                     *   <lvalue>
                     *   (PRE_)INC
                     */

                    if (block_size != 3)
                        lambda_error(
                          "Bad number of arguments to #'%s\n",
                          instrs[type - CLOSURE_OPERATOR].name
                        );

                    item = get_rvalue(argp + 2, NULL);
                    if (item != NULL && item->type == T_NUMBER && item->u.number == 1)
                    {
                        compile_lvalue(argp+1, 0);
                        if (opt_flags & VOID_ACCEPTED)
                        {
                            result_flags = VOID_GIVEN;
                            STORE_CODE(current.codep, F_INC);
                        }
                        else
                        {
                            STORE_CODE(current.codep, F_PRE_INC);
                        }
                        current.code_left--;
                    }
                    else
                    {
                        compile_value(argp+2, 0);
                        compile_lvalue(argp+1, 0);
                        if (opt_flags & VOID_ACCEPTED)
                        {
                            result_flags = VOID_GIVEN;
                            STORE_CODE(current.codep, F_VOID_ADD_EQ);
                        }
                        else
                            STORE_CODE(current.codep, F_ADD_EQ);
                        current.code_left--;
                    }
                    break;

                /* ({#'-=, <lvalue>, <expr> })
                 */
                case F_SUB_EQ:
                    /* This is compiled as:
                     *
                     *   <expr>
                     *   <lvalue>
                     *   SUB_EQ
                     *
                     * For the special case <expr> == 1:
                     *
                     *   <lvalue>
                     *   (PRE_)DEC
                     */

                    if (block_size != 3)
                        lambda_error(
                          "Bad number of arguments to #'%s\n",
                          instrs[type - CLOSURE_OPERATOR].name
                        );

                    item = get_rvalue(argp + 2, NULL);
                    if (item != NULL && item->type == T_NUMBER && item->u.number == 1)
                    {
                        compile_lvalue(argp+1, 0);
                        if (opt_flags & VOID_ACCEPTED)
                        {
                            result_flags = VOID_GIVEN;
                            STORE_CODE(current.codep, F_DEC);
                        }
                        else
                        {
                            STORE_CODE(current.codep, F_PRE_DEC);
                        }
                        current.code_left--;
                    }
                    else
                    {
                        compile_value(argp+2, 0);
                        compile_lvalue(argp+1, 0);
                        STORE_CODE(current.codep, F_SUB_EQ);
                        current.code_left--;
                    }
                    break;

                /* ({#'op=, <lvalue>, <expr> })
                 * with op: *, &, |, ^, <<, >>, >>>, /, %, &&, ||
                 */
                case F_MULT_EQ:
                case F_AND_EQ:
                case F_OR_EQ:
                case F_XOR_EQ:
                case F_LSH_EQ:
                case F_RSH_EQ:
                case F_RSHL_EQ:
                case F_DIV_EQ:
                case F_MOD_EQ:
                    /* This is compiled as:
                     *
                     *   <expr>
                     *   <lvalue>
                     *   <op>_EQ
                     */

                    if (block_size != 3)
                    {
                        lambda_error(
                          "Bad number of arguments to #'%s\n",
                          instrs[type - CLOSURE_OPERATOR].name
                        );
                    }
                    compile_value(argp+2, 0);
                    compile_lvalue(argp+1, 0);
                    STORE_CODE(current.codep, (bytecode_t)(type - CLOSURE_OPERATOR));
                    current.code_left--;
                    break;

                /* ({#'op=, <lvalue>, <expr> })
                 * with op: &&, ||
                 */
                case F_LAND_EQ:
                case F_LOR_EQ:
                  {
                    /* This is compiled as:
                     *
                     *      <prot-lvalue>
                     *      LDUP
                     *      <op> l
                     *      <expr>
                     *   l: SWAP_VALUES
                     *      ASSIGN
                     *
                     * respectively for long branches:
                     *
                     *      <prot-lvalue>
                     *      LDUP
                     *      DUP
                     *      LBRANCH l
                     *      POP
                     *      <expr>
                     *   l: SWAP_VALUES
                     *      ASSIGN
                     */

                    mp_int branchp;
                      /* The position of the branch/operator instruction.
                       */
                    int code;        /* Compiled instruction */
                    Bool is_and;     /* TRUE if the operator is F_LAND_EQ */
                    mp_int end;      /* The branch target */
                    mp_int offset;   /* The branch offset */

                    if (type - CLOSURE_OPERATOR == F_LAND_EQ)
                    {
                        code = F_LAND;
                        is_and = MY_TRUE;
                    }
                    else
                    {
                        code = F_LOR;
                        is_and = MY_FALSE;
                    }

                    if (block_size != 3)
                    {
                        lambda_error(
                          "Bad number of arguments to #'%s\n",
                          instrs[type - CLOSURE_OPERATOR].name
                        );
                    }

                    compile_lvalue(argp+1, PROTECT_LVALUE);

                    if (current.code_left < 3)
                        realloc_code();

                    current.code_left--;
                    STORE_CODE(current.codep, (bytecode_t)F_LDUP);

                    branchp = current.code_max - current.code_left;
                    current.code_left -= 2;
                    STORE_CODE(current.codep, (bytecode_t)code);
                    STORE_CODE(current.codep, (bytecode_t)0);

                    compile_value(argp+2, REF_ACCEPTED);

                    /* Store the correct offsets for the operator/branch
                     * instruction. If necessary, the short branch is
                     * converted into long ones.
                     */
                    end = current.code_max - current.code_left;
                      /* The target to jump to */
                    offset = end - branchp - 2;
                    if (offset <= 0xff)
                    {
                        PUT_UINT8(current.code+branchp+1, (unsigned char)offset);
                    }
                    else
                    {
                        /* We exceeded the limit of the short offsets.
                         * Extend the offset into long branch.
                         */

                        mp_int i;
                        bytecode_p p;

                        code = is_and ? F_LBRANCH_WHEN_ZERO
                                      : F_LBRANCH_WHEN_NON_ZERO;

                        /* Prepare the copying of the code */
                        if (current.code_left < 3)
                            realloc_code();
                        current.code_left -= 3;
                        current.codep += 3;
                        p = current.code + end + 2;
                        for (i = offset; --i >= 0; --p )
                            *p = p[-3];
                        p[-4] = F_DUP;
                        p[-3] = code;
                        offset += 3;
                        PUT_SHORT((p-2), offset);
                        if (offset > 0x7fff)
                            UNIMPLEMENTED;
                        p[0]  = F_POP_VALUE;
                    }

                    if (current.code_left < 2)
                        realloc_code();
                    current.code_left -= 2;
                    STORE_CODE(current.codep, (bytecode_t)F_SWAP_VALUES);
                    STORE_CODE(current.codep, (bytecode_t)F_ASSIGN);
                    break;
                  }

                /* ({#'++, <lvalue> })
                 * ({#'--, <lvalue> })
                 */
                case F_POST_INC:
                case F_POST_DEC:
                    /* This is compiled as:
                     *
                     *   <lvalue>        <lvalue>
                     *   (POST_)INC      (POST_)DEC
                     */

                    if (block_size != 2)
                    {
                        lambda_error(
                          "Bad number of arguments to #'%s\n",
                          instrs[type - CLOSURE_OPERATOR].name
                        );
                    }

                    compile_lvalue(argp+1, 0);

                    if (opt_flags & VOID_ACCEPTED)
                    {
                        result_flags = VOID_GIVEN;
                        if (type-CLOSURE_OPERATOR == F_POST_INC)
                            STORE_CODE(current.codep, F_INC);
                        else
                            STORE_CODE(current.codep, F_DEC);
                    }
                    else
                        STORE_CODE(current.codep, (bytecode_t)type);
                    current.code_left--;

                    break;

                /* ({#'do, <body1>, ... <bodyn>, <cond>, <result> })
                 */
                case F_BBRANCH_WHEN_NON_ZERO:
                  {
                    /* This is compiled as:
                     *
                     *   l: <body>
                     *      POP
                     *      <body2>
                     *      ...
                     *      <bodyn>
                     *      POP
                     *      <cond>
                     *      BBRANCH_NON_ZERO l
                     *      <result>
                     *
                     * If a <body> doesn't return a value, the following POP
                     * is omitted.
                     *
                     * As usual, if the jump distance is too big, the BBRANCH
                     * is converted into a LBRANCH. Also, if the <cond>
                     * returns a result in reversed logic, the branch condition
                     * is reversed.
                     */

                    mp_int i;
                    enum compile_value_output_flags void_given;
                    mp_int offset;      /* Position of first <body> */

                    i = block_size - 3;
                    if (i < 0)
                        lambda_error("Missing argument(s) to #'do\n");

                    offset = current.code_left - current.code_max;

                    /* Compile all the bodys */
                    if (i) do
                    {
                        void_given = compile_value(++argp, VOID_WANTED|REF_ACCEPTED);
                        if ( !(void_given & VOID_GIVEN) )
                        {
                            /* POP the unwanted result */
                            if (current.code_left < 1)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_POP_VALUE);
                        }
                    } while(--i);

                    /* Compile the condition */
                    void_given = compile_value(++argp, NEGATE_ACCEPTED);
                    offset += current.code_max - current.code_left + 1;

                    if (current.code_left < 3)
                        realloc_code();
                    if (offset > 0xff)
                    {
                    	/* We need a long branch */
                        if (offset > 0x8000)
                            UNIMPLEMENTED
                        current.code_left -= 3;
                        if (void_given & NEGATE_GIVEN)
                            STORE_CODE(current.codep, F_LBRANCH_WHEN_ZERO);
                        else
                            STORE_CODE(current.codep, F_LBRANCH_WHEN_NON_ZERO);
                        STORE_SHORT(current.codep, -offset);
                    }
                    else
                    {
                        current.code_left -= 2;
                        if (void_given & NEGATE_GIVEN)
                            STORE_CODE(current.codep, F_BBRANCH_WHEN_ZERO);
                        else
                            STORE_CODE(current.codep, F_BBRANCH_WHEN_NON_ZERO);
                        STORE_UINT8(current.codep, offset);
                    }

                    /* Compile the result */
                    result_flags = compile_value(++argp, opt_flags);
                    break;
                  }

                /* ({#'while, <cond>, <result>, <body1>, ... <bodyn> })
                 */
                case F_BBRANCH_WHEN_ZERO:
                  {
                    /* This is compiled as:
                     *
                     *        BRANCH l1
                     *   l0:  <body>
                     *        POP
                     *        <body2>
                     *        ...
                     *        <bodyn>
                     *        POP
                     *   l1:  <cond>
                     *        BRANCH_NON_ZERO l0
                     *        <result>
                     *
                     * If a <body> doesn't return a value, the following POP
                     * is omitted.
                     *
                     * As usual, if the jump distances are too big, the (B)BRANCHes
                     * are converted into LBRANCHes. Also, if the <cond>
                     * returns a result in reversed logic, the branch condition
                     * is reversed.
                     */

                    mp_int i;
                    enum compile_value_output_flags void_given;
                    mp_int start_branch;
                    mp_int offset;

                    /* Store the initial branch, and remember its position
                     * for the backpatching.
                     */
                    if (current.code_left < 2)
                        realloc_code();
                    current.code_left -= 2;
                    start_branch = current.code_max - current.code_left;
                    STORE_CODE(current.codep, F_BRANCH);
                    STORE_UINT8(current.codep, 0);

                    i = block_size - 3;
                    if (i < 0)
                        lambda_error("Missing argument(s) to #'while\n");

                    /* Compile all bodies */
                    offset = current.code_left - current.code_max;
                    argp += 2;
                    if (i) do
                    {
                        void_given = compile_value(++argp, VOID_WANTED|REF_ACCEPTED);
                        if ( !(void_given & VOID_GIVEN) )
                        {
                            /* The body returned a result: POP it */
                            if (current.code_left < 2)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_POP_VALUE);
                        }
                    } while(--i);

                    /* Store the proper distance into the initial branch.
                     * Rewrite it to a long branch if necessary.
                     */
                    offset =
                      current.code_max - current.code_left - start_branch;
                    if (offset > 0xff)
                    {
                        bytecode_p p;

                        if (offset > 0x7ffd)
                            UNIMPLEMENTED
                        if (current.code_left < 1)
                            realloc_code();
                        current.code_left--;

                        /* Move the generated code */
                        p = (bytecode_p)current.codep++;
                        i = offset;
                        do {
                            p--;
                            p[1] = *p;
                        } while (--i);

                        /* Generate the LBRANCH */
                        p = current.code+start_branch-2;
                        PUT_CODE(p, F_LBRANCH);
                        PUT_SHORT(p+1, offset+2);
                        start_branch++;
                    }
                    else
                    {
                        PUT_UINT8(current.code+start_branch-1, (unsigned char)offset);
                    }

                    /* Compile the condition and generate the branch */
                    argp = block;
                    void_given = compile_value(++argp, NEGATE_ACCEPTED);

                    if (current.code_left < 3)
                        realloc_code();

                    offset =
                      current.code_max - current.code_left - start_branch + 1;
                    if (offset > 0xff)
                    {
                        if (offset > 0x8000)
                            UNIMPLEMENTED

                        current.code_left -= 3;
                        if (void_given & NEGATE_GIVEN)
                            STORE_CODE(current.codep, F_LBRANCH_WHEN_ZERO);
                        else
                            STORE_CODE(current.codep, F_LBRANCH_WHEN_NON_ZERO);
                        STORE_SHORT(current.codep, -offset);
                    }
                    else
                    {
                        current.code_left -= 2;
                        if (void_given & NEGATE_GIVEN)
                            STORE_CODE(current.codep, F_BBRANCH_WHEN_ZERO);
                        else
                            STORE_CODE(current.codep, F_BBRANCH_WHEN_NON_ZERO);
                        STORE_UINT8(current.codep, (bytecode_t)offset);
                    }

                    /* Compile the result */
                    result_flags = compile_value(++argp, opt_flags);
                    break;
                  }

                /* ({#'foreach, <sym>, <expr>, <body1>, ... <bodyn> })
                 * ({#'foreach, ({ <sym1>, ... <symn> }), <expr>, <body1>, ... <bodyn> })
                 */
                case F_FOREACH:
                  {
                    /* This is compiled as:
                     *
                     *       PUSH_(LOCAL_)LVALUE <var1>
                     *       ...
                     *       PUSH_(LOCAL_)LVALUE <varn>
                     *       <expr>
                     *       FOREACH <numargs> c
                     *    l: <body1>
                     *       POP
                     *       <body2>
                     *       ...
                     *       <bodyn>
                     *       POP
                     *    c: FOREACH_NEXT l
                     *    e: FOREACH_END
                     *       CONST0
                     *
                     * or if no bodies are given:
                     *
                     *       <expr>
                     *       POP_VALUE
                     *       CONST0
                     *
                     * If a <body> doesn't return a value, the following POP
                     * is omitted.
                     * If the caller doesn't require a result, the final CONST0
                     * is omitted.
                     */

                    mp_int i;
                    enum compile_value_output_flags void_given;
                    mp_int start;
                    mp_int offset;
                    int    vars_given;
                    int    body_count;
                    bool   foreach_ref = false;
                    svalue_t *svp;

                    body_count = block_size - 3;
                    if (body_count < 0)
                        lambda_error("Missing argument(s) to #'foreach\n");

                    if (!body_count)
                    {
                        /* Just create the code for the expression
                         * and pop the value
                         */
                        compile_value(argp+2, REF_ACCEPTED);
                        if (current.code_left < 2)
                            realloc_code();
                        current.code_left--;
                        STORE_CODE(current.codep, F_POP_VALUE);

                        /* If a result is required, compile a 0 */
                        if (opt_flags & VOID_ACCEPTED)
                            result_flags = VOID_GIVEN;
                        else
                        {
                            current.code_left--;
                            STORE_CODE(current.codep, F_CONST0);
                        }

                        break;
                    }

                    /* Create the code to push the variable lvalues
                     */
                    item = get_rvalue(++argp, NULL);
                    if (item == NULL)
                    {
                        struct protected_range_lvalue *vr = argp->u.protected_range_lvalue;

                        if (vr->vec.type != T_POINTER)
                            lambda_error("Missing variable lvalue to #'foreach\n");

                        svp = vr->vec.u.vec->item + vr->index1;
                        vars_given = i = vr->index2 - vr->index1;
                    }
                    else if (item->type != T_POINTER)
                    {
                        vars_given = 1;
                        svp = item;
                    }
                    else
                    {
                        svp = item->u.vec->item;
                        vars_given = i = (int)VEC_SIZE(item->u.vec);
                    }

                    if (!vars_given)
                        lambda_error("Missing variable lvalue to #'foreach\n");
                    if (vars_given > 0xFE)
                        lambda_error("Too many lvalues to #'foreach: %d\n", vars_given);
                    for (i = vars_given; i > 0; i--, svp++)
                    {
                        if (!is_lvalue(svp, MAKE_VAR_LVALUE))
                            lambda_error("Missing variable lvalue to #'foreach\n");
                        compile_lvalue(svp, MAKE_VAR_LVALUE);
                    }

                    /* Create the code for the expression */
                    foreach_ref = (compile_value(++argp, REF_ACCEPTED) & REF_GIVEN) != 0;

                    /* Create the FOREACH instruction and remember the position
                     */
                    if (current.code_left < 4)
                        realloc_code();
                    current.code_left -= 4;
                    STORE_CODE(current.codep, foreach_ref ? F_FOREACH_REF : F_FOREACH);
                    STORE_UINT8(current.codep, vars_given+1);
                    STORE_SHORT(current.codep, 0);
                    start = current.code_max - current.code_left;

                    /* Compile all bodies.
                     */
                    for (i = body_count; i > 0; i--)
                    {
                        void_given = compile_value(++argp, VOID_WANTED|REF_ACCEPTED);
                        if ( !(void_given & VOID_GIVEN) )
                        {
                            /* The body returned a result: POP it */
                            if (current.code_left < 2)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_POP_VALUE);
                        }
                    }

                    /* Store the proper distance into the initial offset.
                     */
                    offset = current.code_max - current.code_left - start;
                    PUT_SHORT(current.code+start-2, offset);

                    /* Generate the FOREACH_NEXT, followed by F_FOREACH_END.
                     */
                    if (current.code_left < 7)
                        realloc_code();
                    current.code_left -= 6;
                    STORE_CODE(current.codep, F_FOREACH_NEXT);
                    STORE_SHORT(current.codep, USHRT_MAX);
                    STORE_SHORT(current.codep, offset+5);
                    STORE_CODE(current.codep, F_FOREACH_END);

                    /* If a result is required, compile a 0 */
                    if (opt_flags & VOID_ACCEPTED)
                        result_flags = VOID_GIVEN;
                    else
                    {
                        current.code_left--;
                        STORE_CODE(current.codep, F_CONST0);
                    }
                    break;
                  }

                /* ({#'catch, <body> })
                 * ({#'catch, <body>, 'nolog })
                 * ({#'catch, <body>, 'publish })
                 * ({#'catch, <body>, 'nolog, 'publish })
                 * ({#'catch, <body>, 'nolog, 'publish, 'reserve, <expr> })
                 */
                case F_CATCH:
                  {
                    /* This is compiled as:
                     *
                     *      SAVE_ARG_FRAME
                     *      CATCH l / CATCH_NO_LOG l
                     *      <body>
                     *   l: END_CATCH
                     *      RESTORE_ARG_FRAME
                     */

                    mp_int start, offset;
                    int flags, i;

                    if (block_size < 2 || block_size > 6)
                        lambda_error("Wrong number of arguments to #'catch\n");

                    flags = 0;
                    for (i = 3; i <= block_size; i++)
                    {
                        item = get_rvalue(argp + i -1, NULL);
                        if (item == NULL)
                            item = argp + i - 1;

                        if (item->type == T_SYMBOL
                         && mstreq(item->u.str, STR_NOLOG))
                            flags |= CATCH_FLAG_NOLOG;
                        else if (item->type == T_SYMBOL
                         && mstreq(item->u.str, STR_PUBLISH))
                            flags |= CATCH_FLAG_PUBLISH;
                        else if (item->type == T_SYMBOL
                         && mstreq(item->u.str, STR_RESERVE)
                                 )
                        {
                            if (i > block_size)
                                lambda_error("Missing expression for 'reserve "
                                             "catch-modifier.\n");
                            flags |= CATCH_FLAG_RESERVE;
                            if (compile_value(argp+i, 0) & VOID_GIVEN)
                                lambda_error("Expression for 'reserve "
                                             "doesn't return a value.\n");
                            i++;
                        }
                        else
                            lambda_error("Expected 'nolog, 'publish or "
                                         "'reserve as catch-modifier.\n");
                    }

                    if (current.code_left < 4)
                        realloc_code();
                    current.code_left -= 4;

                    STORE_CODE(current.codep, F_SAVE_ARG_FRAME);
                    STORE_CODE(current.codep, F_CATCH);

                    STORE_UINT8(current.codep, flags);

                    STORE_UINT8(current.codep, 0);
                    start = current.code_max - current.code_left;

                    compile_value(++argp, REF_ACCEPTED);
                    if (current.code_left < 2)
                        realloc_code();

                    current.code_left -= 2;
                    STORE_CODE(current.codep, F_END_CATCH);
                    STORE_CODE(current.codep, F_RESTORE_ARG_FRAME);

                    offset = current.code_max - current.code_left - start - 1;
                    if (offset > 0xff)
                    {
                        UNIMPLEMENTED
                    }
                    PUT_UINT8(current.code+start-1, (bytecode_t)offset);
                    break;
                  }

                /* ({#'({, <expr1>, ..., <exprN> })
                 */
                case F_AGGREGATE:
                  {
                    /* This is compiled as:
                     *
                     *   <expr1>
                     *   ...
                     *   <exprN>
                     *   F_AGGREGATE N
                     */
                    int i, size;

                    size = i = block_size - 1;
                    while (--i >= 0)
                    {
                        compile_value(++argp, REF_ACCEPTED);
                    }
                    if (current.code_left < 3)
                        realloc_code();
                    current.code_left -= 3;
                    STORE_CODE(current.codep, F_AGGREGATE);
                    STORE_SHORT(current.codep, size);
                    break;
                  }

                /* ({#'([, <array1>, ..., <arrayN> })
                 */
                case F_M_CAGGREGATE:
                  {
                    /* This is compiled as:
                     *
                     *   <array1>[0]
                     *   ...
                     *   <array1>[M]
                     *   <array2>[0]
                     *   ...
                     *   <arrayN>[M]
                     *   M_(C)AGGREGATE N M
                     */
                    mp_int i, j;
                    mp_int num_keys;    /* Number of keys to add */
                    mp_int num_values;  /* Number of values per key */

                    num_values = 1;
                    i = block_size;
                    num_keys = i - 1;

                    /* Check and compile all mapping keys and values */
                    for (i = block_size; --i;)
                    {
                        svalue_t *element;
                        item = get_rvalue(++argp, NULL);

                        if (item == NULL)
                        {
                            struct protected_range_lvalue *ar = argp->u.protected_range_lvalue;
                            if (ar->vec.type != T_POINTER)
                                lambda_error("Bad argument to #'([\n");

                            element = ar->vec.u.vec->item + ar->index1;
                            j = ar->index2 - ar->index1;
                        }
                        else if (item->type == T_POINTER)
                        {
                            element = item->u.vec->item;
                            j = (mp_int)VEC_SIZE(item->u.vec);
                        }
                        else
                            lambda_error("Bad argument to #'([\n");


                        /* The first array determines the width */
                        if (j != num_values)
                        {
                            if (!j)
                                lambda_error("#'([ : Missing key.\n");
                            if (i != num_keys)
                                lambda_error(
                                  "#'([ : Inconsistent value count.\n");
                            num_values = j;
                        }

                        while (--j >= 0)
                        {
                            compile_value(element++, REF_ACCEPTED);
                        }
                    }

                    if (current.code_left < 5)
                        realloc_code();

                    num_values--; /* one item of each subarray is the key */
                    if ( (num_keys | num_values) & ~0xff)
                    {
                    	/* More than 255 keys or values: long instruction */
                        current.code_left -= 5;
                        STORE_CODE(current.codep, F_M_AGGREGATE);
                        STORE_SHORT(current.codep, num_keys);
                        STORE_SHORT(current.codep, num_values);
                    }
                    else
                    {
                    	/* Short instruction */
                        current.code_left -= 3;
                        STORE_CODE(current.codep, F_M_CAGGREGATE);
                        STORE_UINT8(current.codep, (unsigned char)num_keys);
                        STORE_UINT8(current.codep, (unsigned char)num_values);
                    }
                    break;
                  }

                /* ({#'(<, <template>, <expr1>, ..., <exprN> })
                 */
                case F_S_AGGREGATE:
                  {
                    /* This is compiled as:
                     *
                     *   <template>
                     *   <expr1>
                     *   ...
                     *   <exprN>
                     *   F_S_AGGREGATE -1 N
                     */
                    int i, size;

                    size = block_size - 2;
                    if (size > STRUCT_MAX_MEMBERS)
                    {
                        lambda_error("Too many elements for struct.\n");
                        size = STRUCT_MAX_MEMBERS;
                    }

                    item = get_rvalue(argp+1, NULL);
                    if (item != NULL && item->type == T_STRUCT
                     && struct_size(item->u.strct) < size)
                    {
                        lambda_error("Too many elements for struct %s.\n"
                                    , get_txt(struct_name(item->u.strct))
                                    );
                        size = struct_size(item->u.strct);
                    }
                    i = size+1;
                    while (--i >= 0)
                    {
                        compile_value(++argp, REF_ACCEPTED);
                    }
                    if (current.code_left < 4)
                        realloc_code();
                    current.code_left -= 4;
                    STORE_CODE(current.codep, F_S_AGGREGATE);
                    STORE_SHORT(current.codep, -1);
                    STORE_UINT8(current.codep, (unsigned char)size);
                    break;
                  }

                /* ({#'return })
                 * ({#'return, <expr> })
                 */
                case F_RETURN:
                  {
                    /* This is compiled as:
                     *
                     *   <expr>    or    RETURN0
                     *   RETURN
                     */

                    if (block_size != 2)
                    {
                        if (block_size > 1)
                            lambda_error("Too many arguments to #'return\n");
                        result_flags = VOID_GIVEN;
                    }
                    else
                    {
                        result_flags =
                          compile_value(++argp, ZERO_ACCEPTED|REF_ACCEPTED);
                    }

                    if (current.code_left < 1)
                        realloc_code();
                    current.code_left--;
                    if (result_flags & VOID_GIVEN)
                    {
                        STORE_CODE(current.codep, F_RETURN0);
                        result_flags ^= VOID_GIVEN;
                    }
                    else
                        STORE_CODE(current.codep, F_RETURN);

                    break;
                  }

                /* ({#'[.., <value>, <index> })
                 * ({#'[<.., <value>, <index> })
                 */
                case F_NX_RANGE:
                case F_RX_RANGE:
                case F_AX_RANGE:
                  {
                    /* This is compiled as:
                     *     <value>
                     *     <index>
                     *     CONST1
                     *     NR_RANGE/RR_RANGE/AR_RANGE
                     */

                    bytecode_t opcode = (type - CLOSURE_OPERATOR);
                    const char *opname;

                    switch (type - CLOSURE_OPERATOR)
                    {
                    case F_NX_RANGE:
                        opcode = F_NR_RANGE;
                        opname = "#'[..";
                        break;
                    case F_RX_RANGE:
                        opcode = F_RR_RANGE;
                        opname = "#'[<..";
                        break;
                    case F_AX_RANGE:
                        opcode = F_AR_RANGE;
                        opname = "#'[>..";
                        break;
                    default:
                        fatal("Illegal operator %d\n", type - CLOSURE_OPERATOR);
                        break;
                    }

                    if (block_size != 3)
                        lambda_error("Bad number of arguments to %s\n"
                                    , opname);

                    compile_value(++argp, 0);
                    compile_value(argp, 0);

                    if (current.code_left < 2)
                        realloc_code();
                    current.code_left -= 2;
                    STORE_CODE(current.codep, F_CONST1);
                    STORE_CODE(current.codep, opcode);
                    break;
                  }

                /* ({#'switch, <value>, {<case1>, <block1>, <delim>} })
                 *
                 * <case>s can be #'default, simple values, or arrays of values.
                 * <delim>s are #', or #'break.
                 */
                case F_SWITCH:
                  {
                    /* This is compiled as:
                     *
                     *   <value>
                     *   SWITCH switchargs
                     *   <block1>
                     *   POP/BREAK
                     *   <block2>
                     *   POP/BREAK
                     *   ...
                     *   <blockN>
                     *   POP/BREAK
                     *   switchargs
                     *
                     * If no default case is given, a default 'CONST0; BREAK'
                     * is inserted after the last block.
                     *
                     * See interpret.c for a detailed description of the SWITCH
                     * instruction.
                     */

                    mp_int num_blocks;        /* Number different cases */
                    mp_int i;
                    mp_int switch_pc;         /* Position of the SWITCH+1. */
                    mp_int default_addr = 0;  /* Pos of the default case */
                    Bool some_numeric = MY_FALSE;
                      /* TRUE if some cases are numeric */
                    Bool no_string    = MY_TRUE;
                      /* TRUE if the case list contains no strings (yet) */
                    case_list_entry_t *zero = NULL;
                      /* Case label with value 0. */
                    case_list_entry_t *save_free_block;
                    case_list_entry_t *save_next_free;
                    case_list_entry_t *save_list0;
                    case_list_entry_t *save_list1;
                      /* Save the vitals of the outer switch.
                       * We don't need an explicit list of case_states
                       * because compile_value() recurses.
                       */

#                   define REUSE_LIST_ENTRY \
                        case_state.list0 = case_state.list1; \
                        case_state.list1 = l->next; \
                        case_state.next_free++;

                    /* Initialize the globals if it didn't happen yet */
                    if (!switch_initialized)
                    {
                        switch_initialized = MY_TRUE;
                        if (current_loc.file)
                        {
                            /* lambda() is called while the LPC compiler
                             * was busy compiling a switch(), maybe from
                             * within the error handling.
                             */
                            save_case_free_block = case_state.free_block;
                            save_case_next_free  = case_state.next_free;
                            save_case_list0 = case_state.list0;
                            save_case_list1 = case_state.list1;
                        }
                        else
                        {
                            case_state.free_block = NULL;
                            case_state.next_free = NULL;
                        }
                    }

                    /* Let's begin */

                    num_blocks = (block_size) / 3;
                    if (block_size != 2 + num_blocks * 3)
                        lambda_error("Bad number of arguments to #'switch\n");

                    compile_value(++argp, 0);

                    if (current.code_left < 3)
                        realloc_code();
                    current.code_left -= 3;
                    STORE_CODE(current.codep, F_SWITCH);
                    current.codep += 2; /* Space for b1 a2 */

                    /* Save position and prepare to compile the switch() */
                    switch_pc = current.code_max - current.code_left - 2;

                    if (++current.break_stack > current.max_break_stack)
                        current.max_break_stack = current.break_stack;

                    save_free_block = case_state.free_block;
                    save_next_free  = case_state.next_free;
                    save_list0 = case_state.list0;
                    save_list1 = case_state.list1;
                    case_state.list0 = case_state.list1 = NULL;

                    /* Collect the cases and compile the associated
                     * blocks.
                     */
                    for (i = num_blocks; --i >= 0;)
                    {
                        svalue_t *labels;     /* Label value(s) */
                        mp_int j;             /* Number of (remaining) labels */
                        case_list_entry_t *l;
                        enum compile_value_output_flags opt_used;
                        svalue_t tmp_label = { T_NUMBER };  /* Temporary string label. */

                        /* Compile the case labels */

                        ++argp;
                        item = get_rvalue(argp, NULL);
                        if (item == NULL)
                        {
                            struct protected_range_lvalue *lr = argp->u.protected_range_lvalue;
                            if (lr->vec.type == T_POINTER)
                            {
                                labels = lr->vec.u.vec->item + lr->index1;
                                j = lr->index2 - lr->index1;
                            }
                            else
                            {
                                assign_rvalue_no_free(&tmp_label, argp);
                                labels = &tmp_label;
                                j = 1;
                            }
                        }
                        else if (item->type == T_POINTER)
                        {
                            labels = item->u.vec->item;
                            j = (mp_int)VEC_SIZE(item->u.vec);
                        }
                        else
                        {
                            labels = item;
                            j = 1;
                        }

                        for (; j--; labels++)
                        {
                            l = new_case_entry();
                            l->addr =
                              current.code_max - current.code_left - switch_pc;
                            l->line = 1;

                            /* Create the case_list_entry for this case label */
                            item = NULL;
                            if (j > 1)
                                item = get_rvalue(labels + 1, NULL);
                            if (item != NULL && item->type == T_CLOSURE
                                             && item->x.closure_type == F_RANGE +CLOSURE_EFUN )
                            {
                                /* It's a ({<low>, #'[..], <high>}) range */
                            	
                                if (j < 2) {
                                    lambda_error(
                                      "case label range lacks end\n"
                                    );
                                }

                                if (labels[0].type != T_NUMBER
                                 || labels[2].type != T_NUMBER )
                                {
                                    lambda_error(
                                      "case label range must be numeric\n"
                                    );
                                }

                                if (!no_string)
                                    lambda_error(
                                      "mixed case label lists not supported\n"
                                    );

                                some_numeric = MY_TRUE;
                                l->key = labels->u.number;

                                /* Get the upper end of the range */

                                j -= 2;
                                labels += 2;
                                if (labels[-2].u.number == labels->u.number)
                                    continue;
                                    /* Single entry sufficient */

                                if (labels[-2].u.number > labels->u.number)
                                {
                                    /* <low> > <high>: invalid case */
                                    REUSE_LIST_ENTRY
                                    continue;
                                }

                                l->addr = 1;
                                l = new_case_entry();
                                l->addr =
                                  current.code_max - current.code_left -
                                    switch_pc;
                                l->line = 0;
                                l->key = labels->u.number;
                            }
                            else if (labels->type == T_STRING || labels->type == T_BYTES)
                            {
                            	/* String label: we have to make the string shared
                            	 * and store it in the value table (to keep the
                            	 * reference).
                            	 */

                                svalue_t stmp;

                                if (some_numeric)
                                {
                                    free_svalue(&tmp_label);
                                    lambda_error(
                                      "mixed case label lists not supported\n"
                                    );
                                }

                                if (--current.values_left < 0)
                                    realloc_values();
                                no_string = MY_FALSE;
                                stmp.type = labels->type;
                                stmp.u.str = make_tabled_from(labels->u.str);
                                *--current.valuep = stmp;

                                l->key = (p_int)stmp.u.str;
                                free_svalue(&tmp_label);
                            }
                            else if (labels->type == T_NUMBER)
                            {
                            	/* Numeric label, with special treatment of
                            	 * the label 0.
                            	 */
                                if ( 0 != (l->key = labels->u.number) )
                                {
                                    if (!no_string)
                                        lambda_error(
                                         "mixed case label lists not supported\n"
                                        );
                                    some_numeric = MY_TRUE;
                                }
                                else
                                {
                                    zero = l;
                                }
                            }
                            else if (labels->type == T_CLOSURE
                                     && labels->x.closure_type == F_CSTRING0 +CLOSURE_OPERATOR)
                            {
                            	/* #'default label */
                            	
                                if (default_addr)
                                    lambda_error("duplicate default\n");
                                default_addr = l->addr;
                                REUSE_LIST_ENTRY
                                continue;
                            }
                            else
                            {
                            	/* Something else - bad wizard! */
                                lambda_error("bad type of case label\n");
                            }
                        } /* for(j over labels) */

                        /* Compile the code block for this case */
                        argp++;
                        opt_used = compile_value(
                          argp,
                          argp[1].x.closure_type ==
                          F_POP_VALUE+CLOSURE_OPERATOR ?
                            VOID_ACCEPTED|REF_ACCEPTED : REF_ACCEPTED
                        );

                        /* Check and compile the delimiter #', or #'break */
                        item = get_rvalue(++argp, NULL);
                        if (item == NULL)
                            item = argp;

                        if (item->type != T_CLOSURE
                         || (   item->x.closure_type !=
                                  F_BREAK+CLOSURE_OPERATOR
                             && (!i || item->x.closure_type !=
                                       F_POP_VALUE+CLOSURE_OPERATOR)) )
                        {
                            lambda_error("Bad delimiter in #'switch\n");
                        }

                        if ( !(opt_used & VOID_GIVEN) )
                        {
                            if (current.code_left < 1)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep
                                      , (bytecode_t) item->x.closure_type);
                        }
                    } /* for (i = num_blocks) */

                    /* If there was not default case, create one. */
                    if (!default_addr)
                    {
                        default_addr =
                          current.code_max - current.code_left - switch_pc;
                        if (current.code_left < 2)
                            realloc_code();
                        current.code_left -= 2;
                        STORE_CODE(current.codep, F_CONST0);
                        STORE_CODE(current.codep, F_BREAK);
                    }

                    /* Create the rest of the switch instruction, especially
                     * the lookup tables.
                     */
                    store_case_labels(
                      current.code_max - current.code_left - switch_pc,
                      default_addr,
                      some_numeric || no_string, zero,
                      lambda_get_space, lambda_move_switch_instructions,
                      lambda_cerror, lambda_cerrorl
                    );

                    /* That's it: restore the previous switch context if any.
                     */
                    case_state.free_block = save_free_block;
                    case_state.next_free  = save_next_free;
                    case_state.list0 = save_list0;
                    case_state.list1 = save_list1;
                    current.break_stack--;
                    break;

#                   undef REUSE_LIST_ENTRY
                  }
                } /* switch(type - CLOSURE_OPERATOR) */

            }
#ifdef USE_PYTHON
            else if (type < (ph_int)CLOSURE_EFUN)
            {
                /* Closure is an python-defined efun */

                result_flags = compile_python_efun_call(type, block_size - 1, argp+1, opt_flags);
            }
#endif
            else /* it's an EFUN closure */
            {
                switch (type - CLOSURE_EFUN)
                {
                /* ({#'&, <expr1>, ..., <exprn> })
                 * ({#'&, <lvalue> })
                 */
                case F_AND:
                  {
                    int i;

                    i = block_size - 2;

                    if ( i > 0 )
                    {
                    	/* This is compiled as:
                    	 *
                    	 *   <expr1>
                    	 *   <expr2>
                    	 *   AND
                    	 *   ...
                    	 *   <exprn>
                    	 *   AND
                    	 */
                        compile_value(++argp, 0);
                        do {
                            compile_value(++argp, 0);
                            if (current.code_left < 1)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_AND);
                        } while (--i);
                    }
                    else if (!i)
                    {
                    	/* This is compiled as:
                    	 *
                    	 *   <lvalue>
                    	 *   F_MAKE_PROTECTED
                    	 *
                    	 * (easy, isn't it?)
                    	 */
                        if (!(opt_flags & REF_ACCEPTED))
                            lambda_error("Reference value in bad position\n");
                        compile_lvalue(++argp, PROTECT_LVALUE|ALLOW_FUNCTION_CALL);
                        result_flags |= REF_GIVEN;
                    }
                    else
                    {
                        lambda_error("Missing argument(s) to #'&\n");
                    }
                    break;
                  }

                /* ({#'|, <expr1>, ..., <exprn> })
                 */
                case F_OR:
                  {
                    int i;

                    i = block_size - 2;

                    if ( i > 0 )
                    {
                    	/* This is compiled as:
                    	 *
                    	 *   <expr1>
                    	 *   <expr2>
                    	 *   OR
                    	 *   ...
                    	 *   <exprn>
                    	 *   OR
                    	 */
                        compile_value(++argp, 0);
                        do {
                            compile_value(++argp, 0);
                            if (current.code_left < 1)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_OR);
                        } while (--i);
                    }
                    else
                    {
                        lambda_error("Missing argument(s) to #'|\n");
                    }
                    break;
                  }

                /* ({#'^, <expr1>, ..., <exprn> })
                 */
                case F_XOR:
                  {
                    int i;

                    i = block_size - 2;

                    if ( i > 0 )
                    {
                    	/* This is compiled as:
                    	 *
                    	 *   <expr1>
                    	 *   <expr2>
                    	 *   OR
                    	 *   ...
                    	 *   <exprn>
                    	 *   OR
                    	 */
                        compile_value(++argp, 0);
                        do {
                            compile_value(++argp, 0);
                            if (current.code_left < 1)
                                realloc_code();
                            current.code_left--;
                            STORE_CODE(current.codep, F_XOR);
                        } while (--i);
                    }
                    else
                    {
                        lambda_error("Missing argument(s) to #'^\n");
                    }
                    break;
                  }

                /* ({#'!, <expr> })
                 */
                case F_NOT:
                  {
                    /* This is compiled as
                     *
                     *   <expr>
                     *   NOT
                     *
                     * If the caller accepts reversed logic, the NOT is
                     * omitted and the fact is stored in result_flags:NEGATE_GIVEN.
                     */

                    if (block_size != 2)
                        lambda_error("Wrong number of arguments to #'!\n");

                    result_flags = compile_value(++argp, opt_flags & ~(ZERO_ACCEPTED|REF_ACCEPTED));
                    if (opt_flags & NEGATE_ACCEPTED)
                    {
                        result_flags ^= NEGATE_GIVEN;
                    }
                    else
                    {
                        if (current.code_left < 1)
                            realloc_code();
                        current.code_left--;
                        STORE_CODE(current.codep, F_NOT);
                    }
                    break;
                  }

                default:
                    result_flags = compile_efun_call(type, block_size - 1, argp+1, opt_flags);
                    break;

                } /* switch */
                break;
            }
        } /* if (efun or operator closure) */
        else switch (type) /* type >= CLOSURE_SIMUL_EFUN */
        {
        default: /* SIMUL_EFUN closure */
            result_flags = compile_sefun_call(type, block_size - 1, argp+1, opt_flags);
            break;

        case CLOSURE_PRELIMINARY:
            lambda_error("Unimplemented closure type for lambda()\n");

        case CLOSURE_UNBOUND_LAMBDA:
        case CLOSURE_BOUND_LAMBDA:
        case CLOSURE_LAMBDA:
        case CLOSURE_LFUN:
            result_flags = compile_lambda_call(type, item, block_size - 1, argp+1, opt_flags);
            break;

        case CLOSURE_IDENTIFIER:
          {

            /* This is compiled as
             *   alien ident:             local ident:
             *
             *   <ident_closure>
             *   FUNCALL 1                IDENTIFIER <ident-index>
             *
             * The FUNCALL will call call_lambda() which in turn will
             * recognize the CLOSURE_IDENTIFIER and act accordingly.
             */

            lambda_t *l;

            l = item->u.lambda;
            if (block_size != 1)
                lambda_error("Argument to variable\n");

            if (l->ob != current.lambda_origin)
            {
            	/* We need the FUNCALL */
            	
                if (current.code_left < 1)
                    realloc_code();
                current.code_left -= 1;
                STORE_CODE(current.codep, instrs[F_SAVE_ARG_FRAME].opcode);

                insert_value_push(item);
                if (current.code_left < 3)
                    realloc_code();
                current.code_left -= 3;
                STORE_CODE(current.codep, instrs[F_FUNCALL].prefix);
                STORE_CODE(current.codep, instrs[F_FUNCALL].opcode);
                STORE_CODE(current.codep, instrs[F_RESTORE_ARG_FRAME].opcode);
            }
            else
            {
            	/* We can use the IDENTIFIER */
            	
                if (current.code_left < 2)
                    realloc_code();
                current.code_left -= 2;
                if ((short)l->function.var_index < 0)
                    lambda_error("Variable not inherited\n");
                STORE_CODE(current.codep, F_IDENTIFIER);
                STORE_CODE(current.codep, (bytecode_t)l->function.var_index);
            }
            break;
          } /* CLOSURE_IDENTIFIER */
        } /* switch(type) for type >= CLOSURE_SIMUL_EFUN */
        break;
      } /* end of case T_POINTER (block compiling code) */

    case T_QUOTED_ARRAY:                  /* ----- T_QUOTED_ARRAY ----- */
        /* This compiles into the value itself minus one quote.
         */

        insert_value_push(rvalue);
        if (!--current.valuep->x.quotes)
            current.valuep->type = T_POINTER;
        break;

    case T_SYMBOL:                              /* ----- T_SYMBOL ----- */
        /* Symbols with more than one quote compile into the value itself
         * minus one quote.
         * Symbols with just one quote compile into 'LOCAL <index>'. This may
         * create the local variable in the first place.
         */

        if (rvalue->x.quotes > 1)
        {
            insert_value_push(rvalue);
            --current.valuep->x.quotes;
        }
        else
        {
            /* Make/find the local variable to the symbol name and
             * compile the LOCAL instruction.
             */
            symbol_t *sym;

            sym = make_symbol(rvalue->u.str);
            if (sym->index < 0)
                lambda_error("Symbol '%s' not bound\n"
                            , get_txt(sym->name));
            if (current.code_left < 2)
                realloc_code();
            STORE_CODE(current.codep, F_LOCAL);
            STORE_CODE(current.codep, (bytecode_t)sym->index);
            current.code_left -= 2;
        }
        break;

    case T_NUMBER:                              /* ----- T_NUMBER ----- */
      {
      	/* Number are compiled as optimal as possible:
      	 *   num == 0: CONST0
      	 *       == 1: CONST1
      	 *   1 < num < 0x100: CLIT <num>
      	 *   -0x100 < num < 0: NCLIT -<num>
      	 *
      	 * Other numbers are compiled as normal values.
      	 */
      	
        mp_int i;

        i = rvalue->u.number;
        if (i <= -0x100 || i >= 0x100)
        {
            insert_value_push(value);
        }
        else if (i >= 0)
        {
            if (current.code_left < 2)
                realloc_code();
            if (!i)
            {
                if (opt_flags & (VOID_ACCEPTED|ZERO_ACCEPTED))
                {
                    /* The caller doesn't really need a value */
                    result_flags = VOID_GIVEN;
                    break;
                }
                STORE_CODE(current.codep, F_CONST0);
                current.code_left--;
                break;
            }
            else if (i == 1)
            {
                STORE_CODE(current.codep, F_CONST1);
                current.code_left--;
                break;
            }
            STORE_CODE(current.codep, F_CLIT);
            STORE_UINT8(current.codep, (unsigned char)i);
            current.code_left -= 2;
            break;
        }
        else /* -0x100 < i < 0 */
        {
            if (current.code_left < 2)
                realloc_code();
            STORE_CODE(current.codep, F_NCLIT);
            STORE_UINT8(current.codep, (unsigned char)(-i));
            current.code_left -= 2;
            break;
        }
        break;
      }

    default:                                 /* ----- other value ----- */
        /* Generate a LAMBDA_(C)CONSTANT for this value. */
        insert_value_push(rvalue);
        break;
    }

    /* Finish up */
    current.levels_left++;
    return result_flags;

} /* compile_value() */

/*-------------------------------------------------------------------------*/
static int
compile_efun_call (ph_int type, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags)

/* Compile a generic efun call.
 */

{
    /* This is compiled as:
     *
     *   optional <save_arg_frame>
     *   <arg1>
     *   <arg2>
     *   ...
     *   <argN>
     *   <efun>
     *   optional <restore_arg_frame>
     *   optional <make_rvalue>
     */

    enum compile_value_output_flags result_flags = 0;

    mp_int i;
    bytecode_p p;
    int f;
    Bool needs_ap;
    mp_int min;
    mp_int max;
    mp_int def;
    mp_int remaining;
    fulltype_t *argtypep;

    /* Get the instruction code */
    f = type - CLOSURE_EFUN;
    min = instrs[f].min_arg;
    max = instrs[f].max_arg;
    if (instrs[f].arg_index >= 0)
    {
        argtypep = efun_arg_types + instrs[f].arg_index;
        remaining = max;
    }
    else
    {
        argtypep = NULL;
        remaining = 0;
    }

    /* Handle the arg frame for varargs efuns */
    needs_ap = MY_FALSE;
    if (f >= EFUNV_OFFSET)
    {
        needs_ap = MY_TRUE;
        if (current.code_left < 1)
            realloc_code();
        current.code_left--;
        STORE_CODE(current.codep, F_SAVE_ARG_FRAME);
    }

    /* Compile the arguments */
    for (i = num_arg; --i >= 0; )
    {
        typeflags_t flags = 0;
        if (remaining != 0)
        {
            fulltype_t *last_argtypep = argtypep;
            while (argtypep->t_type != NULL)
            {
                flags |= argtypep->t_flags;
                argtypep++;
            }

            argtypep++;
            if (remaining > 1)
                remaining--;
            else if (argtypep->t_type == NULL)
                argtypep = last_argtypep;
        }

        if (flags & TYPE_MOD_LVALUE)
            compile_lvalue(argp++, PROTECT_LVALUE);
        else
            compile_value(argp++, REF_ACCEPTED);
    }

    /* Get the instruction and check if it received the
     * correct number of arguments.
     */
    if (current.code_left < 9)
        realloc_code();

    /* The 'efun' #'-> needs a hidden argument
     * for the struct type index.
     */
    if (f == F_S_INDEX || f == F_SX_INDEX)
    {
        current.code_left--;
        STORE_CODE(current.codep, (bytecode_t)F_NCONST1);
    }

    p = current.codep;
    if (num_arg < min)
    {
        /* Not enough arguments... probably */

        int g;

        if (num_arg == min-1 && 0 != (def = instrs[f].Default))
        {
            /* We have a default argument */
            if (instrs[def].prefix)
            {
                STORE_CODE(p, instrs[def].prefix);
                current.code_left--;
                max--;
                min--;
            }
            STORE_CODE(p, instrs[def].opcode);
            current.code_left--;
            max--;
            min--;
        }
        else
            /* Maybe there is a replacement efun */
             if ( (g = proxy_efun(f, num_arg)) < 0
         ||  (f = g, MY_FALSE) )
            /* No, there isn't */
            lambda_error("Too few arguments to %s\n", instrs[f].name);
    }
    else if (num_arg > max && max != -1)
    {
        /* More arguments than the efun can handle */
        if (f == F_INDEX && num_arg == 3)
        {
            /* Exception: indexing of wide mappings */
            f = F_MAP_INDEX;
        }
        else
        {
            lambda_error(
              "Too many arguments to %s\n",
              instrs[f].name
            );
        }
    }

    /* Store function bytecode. */
    if (instrs[f].prefix)
    {
        STORE_CODE(p, instrs[f].prefix);
        current.code_left--;
    }
    STORE_CODE(p, instrs[f].opcode);
    current.code_left--;

    /* Note the type of the result, and add a CONST0 if
     * the caller expects one from a void efun. Always
     * add the CONST0 for void varargs efuns.
     */
    if ( instrs[f].ret_type == lpctype_void )
    {
        if (f < EFUNV_OFFSET
         && (opt_flags & (ZERO_ACCEPTED|VOID_ACCEPTED)))
        {
            result_flags = VOID_GIVEN;
        }
        else
        {
            STORE_CODE(p, F_CONST0);
            current.code_left--;
        }
    }

    /* Handle the arg frame for varargs efuns */
    if (needs_ap)
    {
        current.code_left--;
        STORE_CODE(p, F_RESTORE_ARG_FRAME);
    }

    if (instrs[f].might_return_lvalue && !(opt_flags & LEAVE_LVALUE))
    {
        STORE_CODE(p, F_MAKE_RVALUE);
        current.code_left--;
    }

    current.codep = p;

    return result_flags;
} /* compile_efun_call */


#ifdef USE_PYTHON
/*-------------------------------------------------------------------------*/
static int
compile_python_efun_call (ph_int type, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags)

/* Compile a python efun call.
 */

{
    /* This is compiled as:
     *
     * <save_arg_frame>
     * <arg1>
     * <arg2>
     * ...
     * <argN>
     * <python_efun>
     * <restore_arg_frame>
     * optional <make_rvalue>
     */

    int f = type - CLOSURE_PYTHON_EFUN;

    if (current.code_left < 1)
        realloc_code();
    STORE_CODE(current.codep, F_SAVE_ARG_FRAME);
    current.code_left--;

    /* Compile the arguments */
    for (int i = num_arg; --i >= 0; )
        compile_value(argp++, REF_ACCEPTED);

    if (current.code_left < 5)
        realloc_code();
    STORE_CODE(current.codep, F_PYTHON_EFUN);
    STORE_SHORT(current.codep, (short)f);
    STORE_CODE(current.codep, F_RESTORE_ARG_FRAME);
    current.code_left -= 4;

    if (!(opt_flags & LEAVE_LVALUE))
    {
        STORE_CODE(current.codep, F_MAKE_RVALUE);
        current.code_left--;
    }

    return 0;
} /* compile_python_efun_call */
#endif

/*-------------------------------------------------------------------------*/
static int
compile_sefun_call (ph_int type, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags)

/* Compile a simul-efun call.
 */

{
    /* This is compiled as:
     *    sefun <= 0xffff           sefun > 0xffff
     *
     *    opt. SAVE_ARG_FRAME       SAVE_ARG_FRAME
     *                              <sefun_object_name>
     *                              <sefun_name>
     *    <arg1>                    <arg1>
     *    ...                       ...
     *    <argN>                    <argN>
     *    SIMUL_EFUN <sefun>        CALL_DIRECT
     *    opt. RESTORE_ARG_FRAME    RESTORE_ARG_FRAME
     */

    int simul_efun = type - CLOSURE_SIMUL_EFUN;
    function_t *funp = &simul_efunp[simul_efun];
    bool needs_ap = false;
    bool needs_call_direct = (simul_efun >= SEFUN_TABLE_SIZE);

    /* First check the arguments. */
    if (num_arg > funp->num_arg
      && !(funp->flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS))
       )
    {
        lambda_error(
          "Too many arguments to simul_efun %s\n"
         , get_txt(funp->name)
        );
        num_arg = funp->num_arg;
    }
    else
    {
        if (num_arg < funp->num_arg - funp->num_opt_arg - ((funp->flags & TYPE_MOD_XVARARGS)?1:0)
          && !(funp->flags & TYPE_MOD_VARARGS)
           )
        {
            lambda_error(
              "Missing arguments to simul_efun %s\n"
             , get_txt(funp->name)
            );
        }
    }

    if (needs_call_direct)
    {
    	/* We have to call the sefun by name */
        static svalue_t string_sv = { T_STRING };

        if (current.code_left < 1)
            realloc_code();
        current.code_left -= 1;
        STORE_CODE(current.codep, F_SAVE_ARG_FRAME);
        needs_ap = true;

        string_sv.u.str = query_simul_efun_file_name();
        compile_value(&string_sv, 0);
        string_sv.u.str = funp->name;
        compile_value(&string_sv, 0);
    }
    else if (0 != (funp->flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS))
          || funp->num_opt_arg > 0)
    {
        /* varargs efuns need the arg frame */

        if (current.code_left < 1)
            realloc_code();
        current.code_left -= 1;
        STORE_CODE(current.codep, F_SAVE_ARG_FRAME);
        needs_ap = true;
    }

    /* Compile the arguments */

    for (int i = num_arg; --i >= 0; )
    {
        compile_value(argp++, REF_ACCEPTED);
    }

    /* and the simul-efun instruction */

    if (current.code_left < 4)
        realloc_code();

    if (needs_call_direct)
    {
        /* We need the call_direct */
        if (instrs[F_CALL_DIRECT].prefix)
        {
            STORE_CODE(current.codep, instrs[F_CALL_DIRECT].prefix);
            current.code_left--;
        }
        STORE_CODE(current.codep, instrs[F_CALL_DIRECT].opcode);
        current.code_left--;

        if (num_arg + 1 > 0xff)
            lambda_error("Argument number overflow\n");
    }
    else
    {
    	/* We can call by index */
    	
        if (!needs_ap)
        {
            /* The function takes fixed number of args:
             * push 0s onto the stack for missing args
             */
            int i = funp->num_arg - num_arg;
            if (i > 1 && current.code_left < i + 4)
                realloc_code();
            current.code_left -= i;
            while ( --i >= 0 ) {
                STORE_CODE(current.codep, F_CONST0);
            }
        }

        STORE_CODE(current.codep, F_SIMUL_EFUN);
        STORE_SHORT(current.codep, (short)simul_efun);
        current.code_left -= 3;

        if (needs_ap)
        {
            STORE_UINT8(current.codep, F_RESTORE_ARG_FRAME);
            current.code_left--;
        }
    }

    if(!(opt_flags & LEAVE_LVALUE))
    {
        STORE_CODE(current.codep, F_MAKE_RVALUE);
        current.code_left--;
    }

    return 0;
} /* compile_sefun_call */

/*-------------------------------------------------------------------------*/
static int
compile_lambda_call (ph_int type, svalue_t* closure, mp_int num_arg, svalue_t *argp, enum compile_value_input_flags opt_flags)
{
    /* This is compiled as
     *   alien-lfun:             local lfun:
     *
     *   <lfun_closure>
     *   <arg1>                   <arg1>
     *   ...                      ...
     *   <argN>                   <argN>
     *   FUNCALL N+1              CALL_FUNCTION <lfun-index> N
     *   MAKE_RVALUE              MAKE_RVALUE
     *
     * alien-lfun: lambda->ob != lambda->function.lfun.ob
     *
     * Inherited lfun closures, context lfun closures and lambda
     * closures are compiled similar to alien lfuns using
     * F_CALL_CLOSURE.
     */

    lambda_t *l;

    l = closure->u.lambda;
    if ((type != CLOSURE_UNBOUND_LAMBDA && l->ob != current.lambda_origin)
     || (type == CLOSURE_LFUN && l->ob != l->function.lfun.ob)
       )
    {
        /* Compile it like an alien lfun */

        if (current.code_left < 1)
            realloc_code();
        current.code_left -= 1;
        STORE_CODE(current.codep, instrs[F_SAVE_ARG_FRAME].opcode);

        insert_value_push(closure); /* Push the closure */
        for (mp_int i = num_arg; i--; )
        {
            compile_value(argp++, REF_ACCEPTED);
        }
        if (current.code_left < 3)
            realloc_code();
        current.code_left -= 3;
        STORE_CODE(current.codep, instrs[F_FUNCALL].prefix);
        STORE_CODE(current.codep, instrs[F_FUNCALL].opcode);
        STORE_CODE(current.codep, instrs[F_RESTORE_ARG_FRAME].opcode);

        if (!(opt_flags & LEAVE_LVALUE))
        {
            current.code_left--;
            STORE_CODE(current.codep, F_MAKE_RVALUE);
        }
    }
    else if (type != CLOSURE_LFUN
     || l->function.lfun.inhProg
     || l->function.lfun.context_size
       )
    {
        /* Compile it using F_CALL_CLOSURE. */

        if (current.code_left < 1)
            realloc_code();
        current.code_left -= 1;
        STORE_CODE(current.codep, instrs[F_SAVE_ARG_FRAME].opcode);

        insert_value_push(closure); /* Push the closure */
        for (mp_int i = num_arg; i--; )
        {
            compile_value(argp++, REF_ACCEPTED);
        }
        if (current.code_left < 4)
            realloc_code();
        current.code_left -= 3;
        STORE_CODE(current.codep, instrs[F_CALL_CLOSURE].opcode);
        STORE_CODE(current.codep, instrs[F_POP_SECOND].opcode);
        STORE_CODE(current.codep, instrs[F_RESTORE_ARG_FRAME].opcode);
        if (!(opt_flags & LEAVE_LVALUE))
        {
            current.code_left--;
            STORE_CODE(current.codep, F_MAKE_RVALUE);
        }
    }
    else
    {
        /* Intra-object call: we can call by address */

        if (current.code_left < 1)
            realloc_code();
        current.code_left -= 1;
        STORE_CODE(current.codep, instrs[F_SAVE_ARG_FRAME].opcode);

        for (mp_int i = num_arg; i--; )
        {
            compile_value(argp++, REF_ACCEPTED);
        }

        if (current.code_left < 7)
            realloc_code();

        STORE_CODE(current.codep, F_CALL_FUNCTION);
        STORE_SHORT(current.codep, l->function.lfun.index);
        STORE_CODE(current.codep, instrs[F_RESTORE_ARG_FRAME].opcode);

        current.code_left -= 4;

        if (!(opt_flags & LEAVE_LVALUE))
        {
            STORE_CODE(current.codep, F_MAKE_RVALUE);
            current.code_left--;
        }

        if (num_arg >= 0x100)
            lambda_error("Too many arguments to lfun closure\n");
    }

    return 0;
} /* compile_lambda_call */

/*-------------------------------------------------------------------------*/
static Bool
is_lvalue (svalue_t *argp, int flags)

/* Test if the value <argp> can be compiled into a lvalue.
 * <flags> denote the same flags as compile_lvalue:
 *   MAKE_VAR_LVALUE: If set, then compiling indexed arrays/strings
 *                    are not allowed (only plain variables).
 */

{
    svalue_t *item = get_rvalue(argp, NULL);
    struct protected_range_lvalue* r = NULL;
    if (item == NULL)
    {
        r = argp->u.protected_range_lvalue;
        item = &(r->vec);
    }

    switch(item->type)
    {
    case T_SYMBOL:
        return item->x.quotes == 1;

    case T_POINTER:
      {
        vector_t *block;
        p_int size;

        block = item->u.vec;

        if (r != NULL)
        {
            size = r->index2 - r->index1;
            argp = block->item + r->index1;
        }
        else
        {
            size = VEC_SIZE(block);
            argp = block->item;
        }

        if (size < 1)
            break;

        argp = get_rvalue(argp, NULL);
        if (argp == NULL || argp->type != T_CLOSURE)
            break;

        switch (argp->x.closure_type)
        {
          case F_NX_RANGE +CLOSURE_EFUN:
          case F_RX_RANGE +CLOSURE_EFUN:
          case F_AX_RANGE +CLOSURE_EFUN:
            if (flags & MAKE_VAR_LVALUE)
               break;
            /* Fallthrough */
          case F_INDEX +CLOSURE_EFUN:
          case F_RINDEX+CLOSURE_EFUN:
          case F_AINDEX+CLOSURE_EFUN:
          case F_S_INDEX +CLOSURE_EFUN:
          case F_SX_INDEX +CLOSURE_EFUN:
          case CLOSURE_IDENTIFIER:
            if (size != 3)
                break;
            return MY_TRUE;

          case F_RANGE    +CLOSURE_EFUN:
          case F_NR_RANGE +CLOSURE_EFUN:
          case F_RN_RANGE +CLOSURE_EFUN:
          case F_RR_RANGE +CLOSURE_EFUN:
          case F_NA_RANGE +CLOSURE_EFUN:
          case F_AN_RANGE +CLOSURE_EFUN:
          case F_RA_RANGE +CLOSURE_EFUN:
          case F_AR_RANGE +CLOSURE_EFUN:
          case F_AA_RANGE +CLOSURE_EFUN:
            if (flags & MAKE_VAR_LVALUE)
               break;
            /* Fallthrough */
          case F_MAP_INDEX +CLOSURE_EFUN:
            if (size != 4)
                break;
            return MY_TRUE;
        }
        break;
      }

    case T_CLOSURE:
        if (item->x.closure_type == CLOSURE_IDENTIFIER)
            return MY_TRUE;
        break;
    }

     /* Default: it's not. */
    return MY_FALSE;
} /* is_lvalue() */

/*-------------------------------------------------------------------------*/
static void
compile_lvalue (svalue_t *argp, int flags)

/* Compile the <argp> into an lvalue, according to the <flags>. The function
 * allocates enough space in the code buffer to store the assignment code
 * (1 Byte) as well, so on return .code_left >= 1 holds.
 */

{
    svalue_t *item = get_rvalue(argp, NULL);
    struct protected_range_lvalue* r = NULL;
    if (item == NULL)
    {
        r = argp->u.protected_range_lvalue;
        item = &(r->vec);
    }

    /* These 3 flags are exclusive. */
    assert(((flags & PROTECT_LVALUE)?1:0) + ((flags & MAKE_VAR_LVALUE)?1:0) + ((flags & RESEATING_ACCEPTED)?1:0) <= 1);

    /* Function calls only allowed with PROTECT_LVALUE. */
    assert(!(flags & ALLOW_FUNCTION_CALL) || (flags & PROTECT_LVALUE));

    switch(item->type)
    {

    /* 'a: Symbol of a local variable.
     */
    case T_SYMBOL:
      /* This compiles to:
       *
       *   PUSH_LOCAL_VARIABLE_LVALUE <index>
       */

      {
        symbol_t *sym;

        if (item->x.quotes > 1)
            break;

        /* Find (or create) the variable for this symbol */
        sym = make_symbol(item->u.str);
        if (sym->index < 0)
        {
            sym->index = current.num_locals++;
            if (current.num_locals > MAX_LOCAL)
              lambda_error("Too many symbols.\n");
        }

        if (current.code_left < 4)
            realloc_code();
        current.code_left -= 2;
        STORE_CODE(current.codep, (flags & MAKE_VAR_LVALUE) ? F_PUSH_LOCAL_VARIABLE_VLVALUE : F_PUSH_LOCAL_VARIABLE_LVALUE);
        STORE_UINT8(current.codep, (bytecode_t)sym->index);

        if (flags & PROTECT_LVALUE)
        {
            current.code_left--;
            STORE_CODE(current.codep, F_MAKE_PROTECTED);
        }
        return;
      }

    /* ({ indexing operation })
     */
    case T_POINTER:
      {
        argp = item->u.vec->item;
        p_int size;

        if (r != NULL)
        {
            size = r->index2 - r->index1;
            argp += r->index1;
        }
        else
        {
            size = VEC_SIZE(item->u.vec);
        }

        if (size != 0)
        {
            bool is_struct = false;

            svalue_t *cl = get_rvalue(argp, NULL);
            if (cl == NULL || cl->type != T_CLOSURE)
                break;

            switch (cl->x.closure_type)
            {

            /* ({ #'[, map|array, index [, index] })
             * ({ #'[<, map|array, index })
             * ({ #'., struct, index })
             * ({ #'->, struct, index })
             */
            case F_S_INDEX +CLOSURE_EFUN:
            case F_SX_INDEX +CLOSURE_EFUN:
                is_struct = true;
                /* FALLTHROUGH */
            case F_INDEX +CLOSURE_EFUN:
            case F_RINDEX+CLOSURE_EFUN:
            case F_AINDEX+CLOSURE_EFUN:

                if (size == 3)
                {
                    /* Indexing of an array or normal mapping.
                     */

                    /* If it is a string, we need an lvalue,
                     * but we can't really differentiate here.
                     */
                    if (!is_struct && is_lvalue(argp+1, 0))
                        compile_lvalue(argp+1, PROTECT_LVALUE);
                    else
                        compile_value(argp+1, 0);

                    compile_value(argp+2, 0);

                    if (current.code_left < 3)
                        realloc_code();

                    current.code_left--;
                    switch (cl->x.closure_type)
                    {
                        case F_INDEX + CLOSURE_EFUN:
                            STORE_CODE(current.codep
                                      , (bytecode_t)
                                        ((flags & MAKE_VAR_LVALUE) ? F_INDEX_VLVALUE : F_INDEX_LVALUE));
                            break;

                        case F_RINDEX + CLOSURE_EFUN:
                            STORE_CODE(current.codep
                                      , (bytecode_t)
                                        ((flags & MAKE_VAR_LVALUE) ? F_RINDEX_VLVALUE : F_RINDEX_LVALUE));
                            break;

                        case F_AINDEX + CLOSURE_EFUN:
                            STORE_CODE(current.codep
                                      , (bytecode_t)
                                        ((flags & MAKE_VAR_LVALUE) ? F_AINDEX_VLVALUE : F_AINDEX_LVALUE));
                            break;

                        case F_S_INDEX + CLOSURE_EFUN:
                        case F_SX_INDEX + CLOSURE_EFUN:
                            current.code_left --;
                            STORE_CODE(current.codep, (bytecode_t) F_NCONST1);
                            STORE_CODE(current.codep
                                      , (bytecode_t)
                                        ((cl->x.closure_type ==  F_S_INDEX + CLOSURE_EFUN)
                                         ? ((flags & MAKE_VAR_LVALUE) ? F_S_INDEX_VLVALUE : F_S_INDEX_LVALUE)
                                         : ((flags & MAKE_VAR_LVALUE) ? F_SX_INDEX_VLVALUE : F_SX_INDEX_LVALUE)));
                            break;
                    }

                    if (flags & PROTECT_LVALUE)
                    {
                        current.code_left--;
                        STORE_CODE(current.codep, F_MAKE_PROTECTED);
                    }
                    return;
                } /* if (size == 3) */

                if (size == 4
                 && cl->x.closure_type == F_INDEX +CLOSURE_EFUN)
                {
                    /* Indexing of a wide mapping.
                     */
                    compile_value(argp+1, 0);
                    compile_value(argp+2, 0);
                    compile_value(argp+3, 0);

                    if (current.code_left < 2)
                        realloc_code();

                    current.code_left--;
                    STORE_CODE(current.codep, (flags & MAKE_VAR_LVALUE) ? F_MAP_INDEX_VLVALUE : F_MAP_INDEX_LVALUE);
                    if (flags & PROTECT_LVALUE)
                    {
                        current.code_left--;
                        STORE_CODE(current.codep, F_MAKE_PROTECTED);
                    }
                    return;
                } /* if (size == 4...) */

                /* Otherwise: raise an error */
                break;

            /* ({#'[..], map/array, index, index })
             * ({#'[..<], map/array, index, index })
             * ({#'[<..], map/array, index, index })
             * ({#'[<..<], map/array, index, index })
             * ({#'[..>], map/array, index, index })
             * ({#'[>..], map/array, index, index })
             * ({#'[<..>], map/array, index, index })
             * ({#'[>..<], map/array, index, index })
             * ({#'[>..>], map/array, index, index })
             */
            case F_RANGE    +CLOSURE_EFUN:
            case F_NR_RANGE +CLOSURE_EFUN:
            case F_RN_RANGE +CLOSURE_EFUN:
            case F_RR_RANGE +CLOSURE_EFUN:
            case F_NA_RANGE +CLOSURE_EFUN:
            case F_AN_RANGE +CLOSURE_EFUN:
            case F_RA_RANGE +CLOSURE_EFUN:
            case F_AR_RANGE +CLOSURE_EFUN:
            case F_AA_RANGE +CLOSURE_EFUN:
              {
                int code;

                if (flags & MAKE_VAR_LVALUE)
                    break;

                if (size != 4)
                    break;

                code = F_ILLEGAL;
                switch(cl->x.closure_type)
                {
                case F_RANGE+CLOSURE_EFUN:
                    code = F_RANGE_LVALUE;
                    break;
                case F_NR_RANGE+CLOSURE_EFUN:
                    code = F_NR_RANGE_LVALUE;
                    break;
                case F_RN_RANGE+CLOSURE_EFUN:
                    code = F_RN_RANGE_LVALUE;
                    break;
                case F_RR_RANGE+CLOSURE_EFUN:
                    code = F_RR_RANGE_LVALUE;
                    break;
                case F_NA_RANGE+CLOSURE_EFUN:
                    code = F_NA_RANGE_LVALUE;
                    break;
                case F_AN_RANGE+CLOSURE_EFUN:
                    code = F_AN_RANGE_LVALUE;
                    break;
                case F_RA_RANGE+CLOSURE_EFUN:
                    code = F_RA_RANGE_LVALUE;
                    break;
                case F_AR_RANGE+CLOSURE_EFUN:
                    code = F_AR_RANGE_LVALUE;
                    break;
                case F_AA_RANGE+CLOSURE_EFUN:
                    code = F_AA_RANGE_LVALUE;
                    break;
                }

                compile_lvalue(++argp, PROTECT_LVALUE);
                compile_value(++argp, 0);
                compile_value(++argp, 0);

                if (current.code_left < 2)
                    realloc_code();
                current.code_left--;
                STORE_CODE(current.codep, (bytecode_t)code);

                if (flags & PROTECT_LVALUE)
                {
                    current.code_left--;
                    STORE_CODE(current.codep, F_MAKE_PROTECTED);
                }
                return;
              }

            /* ({#'[.., array, index })
             * ({#'[<.., array, index })
             * ({#'[>.., array, index })
             */
            case F_NX_RANGE +CLOSURE_EFUN:
            case F_RX_RANGE +CLOSURE_EFUN:
            case F_AX_RANGE +CLOSURE_EFUN:
              {
                int code;

                if (flags & MAKE_VAR_LVALUE)
                    break;

                if (size != 3)
                    break;

                code = F_ILLEGAL;
                switch(cl->x.closure_type)
                {
                case F_NX_RANGE+CLOSURE_EFUN:
                    code = F_NX_RANGE_LVALUE;
                    break;
                case F_RX_RANGE+CLOSURE_EFUN:
                    code = F_RX_RANGE_LVALUE;
                    break;
                case F_AX_RANGE+CLOSURE_EFUN:
                    code = F_AX_RANGE_LVALUE;
                    break;
                }

                compile_lvalue(++argp, PROTECT_LVALUE);
                compile_value(++argp, 0);

                if (current.code_left < 2)
                    realloc_code();
                current.code_left--;
                STORE_CODE(current.codep, (bytecode_t)code);

                if (flags & PROTECT_LVALUE)
                {
                    current.code_left--;
                    STORE_CODE(current.codep, F_MAKE_PROTECTED);
                }
                return;
              }

            /* ({ #'[, mapping, index [,index] })
             */
            case F_MAP_INDEX +CLOSURE_EFUN:
                if (size != 4)
                    break;

                compile_value(++argp, 0);
                compile_value(++argp, 0);
                compile_value(++argp, 0);

                if (current.code_left < 2)
                    realloc_code();

                current.code_left--;
                STORE_CODE(current.codep, (flags & MAKE_VAR_LVALUE) ? F_MAP_INDEX_VLVALUE : F_MAP_INDEX_LVALUE);

                if (flags & PROTECT_LVALUE)
                {
                    current.code_left--;
                    STORE_CODE(current.codep, F_MAKE_PROTECTED);
                }
                return;

            /* ({ #'&, lvalue })
             */
            case F_AND +CLOSURE_EFUN:
                if (!(flags & RESEATING_ACCEPTED))
                    break;

                if (size != 2)
                    break;

                compile_lvalue(++argp, MAKE_VAR_LVALUE);
                return;

            /* ({ #'global_var })
             */
            case CLOSURE_IDENTIFIER:
              {
                if (size != 1)
                    break;

                compile_lvalue(cl, flags);
                return;
              }

            case CLOSURE_UNBOUND_LAMBDA:
            case CLOSURE_BOUND_LAMBDA:
            case CLOSURE_LAMBDA:
            case CLOSURE_LFUN:
                if (flags & ALLOW_FUNCTION_CALL)
                {
                    compile_lambda_call(cl->x.closure_type, cl, (mp_int)size - 1, argp+1, LEAVE_LVALUE);
                    return;
                }
                break;

            default:
                if (flags & ALLOW_FUNCTION_CALL)
                {
                    ph_int type = cl->x.closure_type;
#ifdef USE_PYTHON
                    if (type < (ph_int)CLOSURE_PYTHON_EFUN)     /* Operator closure. */
#else
                    if (type < (ph_int)CLOSURE_EFUN)            /* Operator closure. */
#endif
                        break;
#ifdef USE_PYTHON
                    else if (type < (ph_int)CLOSURE_EFUN)       /* Python Efun closure. */
                    {
                        compile_python_efun_call(type, (mp_int)size - 1, argp+1, LEAVE_LVALUE);
                        return;
                    }
#endif
                    else if (type < (ph_int)CLOSURE_SIMUL_EFUN) /* Efun closure. */
                    {
                        /* Only allow the function call efuns. */
                        if (!instrs[type - CLOSURE_EFUN].might_return_lvalue)
                            break;

                        compile_efun_call(type, (mp_int)size - 1, argp+1, LEAVE_LVALUE);
                        return;
                    }
                    else if (type < (ph_int)CLOSURE_LFUN)       /* Simul-Efun closure. */
                    {
                        compile_sefun_call(type, (mp_int)size - 1, argp+1, LEAVE_LVALUE);
                        return;
                    }
                }
                break;
            } /* switch(closure_type) */
        }
        break;
      } /* case T_POINTER */

    /* precomputed closure: only identifiers in this object are allowed
     */
    case T_CLOSURE:
      {
        switch (item->x.closure_type)
        {
        case CLOSURE_IDENTIFIER:
          {
            lambda_t *l;

            l = item->u.lambda;
            if (l->ob != current.lambda_origin)
                break;
            if (current.code_left < 4)
                realloc_code();
            current.code_left -= 2;
            if ((short)l->function.var_index < 0)
                lambda_error("Variable not inherited\n");
            STORE_CODE(current.codep, (flags & MAKE_VAR_LVALUE) ? F_PUSH_IDENTIFIER_VLVALUE : F_PUSH_IDENTIFIER_LVALUE);
            STORE_CODE(current.codep, (bytecode_t)(l->function.var_index));

            if (flags & PROTECT_LVALUE)
            {
                current.code_left--;
                STORE_CODE(current.codep, F_MAKE_PROTECTED);
            }
            return;
          }
        }
        break;
      }

    } /* switch(item->type) */

    lambda_error("Illegal lvalue\n");
} /* compile_lvalue() */

/*-------------------------------------------------------------------------*/
static lambda_t *
lambda (vector_t *args, svalue_t *block, object_t *origin)

/* Compile a lambda closure with the arguments <args>, an array with symbols,
 * and the body <block>. If <origin> is given, the created lambda is bound
 * that object (with proper respect of scheduled program replacements).
 *
 * Result is a pointer to the created lambda structure, the size of the code
 * generated can be determined as current.code_max - current.code_left before
 * lambda() is called again.
 */

{
    mp_int    i, j;
    svalue_t *argp;
    mp_int    num_values;   /* number of values needed */
    mp_int    values_size;  /* size of the value block */
    mp_int    code_size;    /* size of the generated code */
    char     *l0;           /* allocated memory for the lambda */
    lambda_t *l;            /* pointer to the lambda structure */
    int       void_given;   /* result flags from the compiler */

    /* Initialize the work area */

    current.symbols_left = current.symbol_max =
        sizeof current.symbols[0] * SYMTAB_START_SIZE;
    current.symbol_mask = (long)(current.symbol_max- sizeof(symbol_t *));
    current.code = NULL;
    current.values = NULL;
    current.symbols = xalloc((size_t)current.symbol_max);
    i = SYMTAB_START_SIZE - 1;
    do {
        current.symbols[i] = 0;
    } while (--i >= 0);

    switch_initialized = MY_FALSE;

    /* Evaluate the args array: check that all entries are symbols,
     * enter them into the symbol table and check for duplicates.
     */
    argp = args->item;
    j = (mp_int)VEC_SIZE(args);
    if (j >= MAX_LOCAL)
        lambda_error("Too many arguments to lambda()\n");

    for (i = 0; i < j; i++, argp++)
    {
        symbol_t *sym;
        svalue_t *item = get_rvalue(argp, NULL);

        if (item == NULL || item->type != T_SYMBOL)
        {
            lambda_error("Illegal argument type to lambda()\n");
        }
        sym = make_symbol(item->u.str);
        if (sym->index >= 0)
            lambda_error("Double symbol name in lambda arguments\n");
        sym->index = i;
    }

    current.num_locals = i;  /* Args count as locals, too */

    /* Continue initializing the work area */

    current.break_stack = current.max_break_stack = 0;

    current.code_max = CODE_BUFFER_START_SIZE;
    current.code_left = CODE_BUFFER_START_SIZE;
    current.levels_left = MAX_LAMBDA_LEVELS;
    if ( !(current.code = current.codep = xalloc((size_t)current.code_max)) )
       lambda_error("Out of memory (%"PRIdMPINT
                    " bytes) for initial codebuffer\n", current.code_max);

    current.value_max = current.values_left = VALUE_START_MAX;
    if ( !(current.values =
        xalloc(current.value_max * sizeof current.values[0])) )
    {
        lambda_error("Out of memory (%"PRIdMPINT
                     " bytes) for initial value buffer\n",
                     current.value_max * sizeof current.values[0]);
    }
    current.valuep = current.values + current.value_max;

    current.lambda_origin = origin;

    /* Now compile */

    void_given = compile_value(block, ZERO_ACCEPTED|REF_ACCEPTED);

    /* Add the final F_RETURN instruction */
    if (current.code_left < 1)
        realloc_code();
    current.code_left -= 1;
    STORE_CODE(current.codep, (bytecode_t)(void_given & VOID_GIVEN
                                            ? F_RETURN0
                                            : F_RETURN));

    /* Determine number and size of values needed */
    num_values = current.value_max - current.values_left;
    values_size = (long)(num_values * sizeof (svalue_t));
    code_size = current.code_max - current.code_left;

    /* Allocate the memory for values, lambda_t and code */
    l0 = xalloc(values_size + SIZEOF_LAMBDA(0) - sizeof(l->function) + sizeof(l->function.code) - 1 + code_size);

    /* Copy the data */
    memcpy(l0, current.valuep, (size_t)values_size);
    l0 += values_size;
    l = (lambda_t *)l0;
    closure_init_lambda(l, origin);

    memcpy(l->function.code.program, current.code, (size_t)code_size);
    l->function.code.num_arg = VEC_SIZE(args);
    l->function.code.num_locals = current.num_locals + current.max_break_stack - l->function.code.num_arg;
    l->function.code.num_values = num_values;

    /* Clean up */
    free_symbols();
    xfree(current.code);
    xfree(current.values);

    /* If the lambda is to be bound to an object, check if the object's program
     * is scheduled for replacement. If not, mark the object as referenced.
     */
    if (origin
     && (   !(origin->prog->flags & P_REPLACE_ACTIVE)
         || !lambda_ref_replace_program(origin,  l, CLOSURE_LAMBDA, code_size, args, block)
    ) )
    {
        origin->flags |= O_LAMBDA_REFERENCED;
    }

    /* Return the lambda */
    return l;
} /* lambda() */

/*-------------------------------------------------------------------------*/
void
free_closure (svalue_t *svp)

/* Free the closure value in <svp> and all references it holds.
 */

{
    lambda_t *l;
    int type;

    if (!CLOSURE_MALLOCED(type = svp->x.closure_type))
    {
    	/* Simple closure */
        free_object(svp->u.ob, "free_closure");
        return;
    }

    /* Lambda closure */

    l = svp->u.lambda;
    if (--l->ref)
        return;

    if (l->prog_ob)
        free_object(l->prog_ob, "free_closure: lambda creator");

    if (CLOSURE_HAS_CODE(type))
    {
    	/* Free all the values for this lambda, then the memory */
    	
        mp_int num_values;

        if (type != CLOSURE_UNBOUND_LAMBDA)
            free_object(l->ob, "free_closure");

        svp = (svalue_t *)l;
        num_values = l->function.code.num_values;

        while (--num_values >= 0)
            free_svalue(--svp);
        xfree(svp);
        return;
    }

    free_object(l->ob, "free_closure: lambda object");
    if (type == CLOSURE_BOUND_LAMBDA)
    {
    	/* BOUND_LAMBDAs are indirections to UNBOUND_LAMBDA structures.
    	 * Free the BOUND_LAMBDA and then deref/free the referenced
    	 * UNBOUND_LAMBDA.
    	 */
    	
        mp_int num_values;
        lambda_t *l2;

        l2 = l->function.lambda;
        xfree(l);

        if (--l2->ref)
            return;

        if (l2->prog_ob)
            free_object(l2->prog_ob, "free_closure: unbound lambda creator");

        svp = (svalue_t *)l2;
        num_values = l2->function.code.num_values;

        while (--num_values >= 0)
            free_svalue(--svp);
        xfree(svp);
        return;
    }

    if (type == CLOSURE_LFUN)
    {
        free_object(l->function.lfun.ob, "free_closure: lfun object");
        if(l->function.lfun.inhProg)
            free_prog(l->function.lfun.inhProg, MY_TRUE);
    }

    if (type == CLOSURE_LFUN)
    {
        unsigned short num = l->function.lfun.context_size;

        l->function.lfun.context_size = 0; /* ...just in case... */
        while (num > 0)
        {
            num--;
            free_svalue(&(l->context[num]));
        }
    }

    /* else CLOSURE_LFUN || CLOSURE_IDENTIFIER || CLOSURE_PRELIMINARY:
     * no further references held.
     */
    xfree(l);
} /* free_closure() */

/*-------------------------------------------------------------------------*/
Bool
is_undef_closure (svalue_t *sp)

/* Return TRUE if <sp> is an efun closure to F_UNDEF.
 * It's a simple function, but it reduces the couplings to instrs.h .
 *
 * With the current way closure svalues are handled, no closure should
 * be found 'undef', but this check is kept around just in case...
 *
 * Called by swap.c and efuns.c
 */

{
    return (sp->type == T_CLOSURE)
        && (sp->x.closure_type == F_UNDEF+CLOSURE_EFUN);
} /* is_undef_closure() */

/*-------------------------------------------------------------------------*/
void
closure_lookup_lfun_prog ( lambda_t * l
                         , program_t ** pProg
                         , string_t ** pName
                         , Bool * pIsInherited
                         )

/* For lfun/context closure <l>, lookup the defining program and
 * function name, and store the pointers (uncounted) in *<pProg>
 * and *<pName>. If the closure is defined in a program inherited
 * by <l>->function.lfun.ob, *<pIsInherited> is set to TRUE.
 *
 * The results are undefined if called for non-lfun/context closures.
 *
 * The function is used by closure_to_string() and the get_type_info()
 * efun.
 */

{
    object_t       *ob;
    int             ix;
    program_t      *prog;
    Bool            is_inherited;

    is_inherited = MY_FALSE;

    ob = l->function.lfun.ob;
    ix = l->function.lfun.index;

    /* Get the program resident */
    if (O_PROG_SWAPPED(ob)) {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            errorf("Out of memory\n");
    }

    /* Find the true definition of the function */
    prog = ob->prog;

    if (l->function.lfun.inhProg)
    {
        while (prog != l->function.lfun.inhProg)
        {
            inherit_t *inheritp;

#ifdef DEBUG
            if (!prog->num_inherited)
                errorf("(closure_lookup_lfun_prog): Couldn't find "
                       "program '%s' in object '%s' with function index %ld. "
                       "Found program '%s' instead.\n"
                     , get_txt(l->function.lfun.inhProg->name)
                     , get_txt(ob->name)
                     , (long) l->function.lfun.index
                     , get_txt(prog->name)
                     );
#endif

            inheritp = search_function_inherit(prog, ix);
            ix -= inheritp->function_index_offset;
            prog = inheritp->prog;

#ifdef DEBUG
            if (ix >= prog->num_functions)
                errorf("(closure_lookup_lfun_prog): ix %ld > number of "
                       "functions %ld in program '%s'\n"
                     , (long) ix
                     , (long) prog->num_functions
                     , get_txt(prog->name)
                     );
#endif
        }

        is_inherited = MY_TRUE;
    }

    /* Copy the function name pointer (a shared string) */
    *pName = get_function_header_extended(prog, ix, (const program_t**)pProg, NULL)->name;
    if (*pProg != prog)
        is_inherited = MY_TRUE;

    /* Copy the other result values */
    *pProg = prog;
    *pIsInherited = is_inherited;
} /* closure_lookup_lfun_prog() */

/*-------------------------------------------------------------------------*/
const char *
closure_operator_to_string (int type)

/* <type> is the code for a closure operator (the caller has to make sure
 * of that!).
 * If <type> denotes one of the non-efun operators, return its textual
 * presentation as a pointer to a constant string.
 * Otherwise return NULL.
 */

{
    const char *str = NULL;

    if ((type & -0x0800) == CLOSURE_OPERATOR)
    {
        switch(type - CLOSURE_OPERATOR)
        {
        case F_POP_VALUE:
            str = ",";
            break;

        case F_BBRANCH_WHEN_NON_ZERO:
            str = "do";
            break;

        case F_BBRANCH_WHEN_ZERO:
            str = "while";
            break;

        case F_BRANCH:
            str = "continue";
            break;

        case F_CSTRING0:
            str = "default";
            break;

        case F_BRANCH_WHEN_ZERO:
            str = "?";
            break;

        case F_BRANCH_WHEN_NON_ZERO:
            str = "?!";
            break;

        case F_POST_INC:
            str = "++";
            break;

        case F_POST_DEC:
            str = "--";
            break;

        } /* switch() */
    } /* if() */

    return str;
} /* closure_operator_to_string() */

/*-------------------------------------------------------------------------*/
const char *
closure_efun_to_string (int type)

/* <type> is the code for a closure efun (the caller has to make sure
 * of that!), result is the efun name.
 * If <type> denotes one of the non-efun operators, return its textual
 * presentation as a pointer to a constant string.
 *
 * Result is NULL if <type> is not an efun.
 */

{
    if ((type & -0x0800) == CLOSURE_EFUN)
        return instrs[type - CLOSURE_EFUN].name;

    return NULL;
} /* closure_operator_to_string() */

/*-------------------------------------------------------------------------*/
string_t *
closure_location (lambda_t *l)

/* Return the location the lambda structure <l> was created as
 * the string 'from <filename> line <number>".
 */

{
    string_t * rc = NULL;

    if (l && l->prog_ob && !(l->prog_ob->flags & O_DESTRUCTED))
    {

        if (l->prog_ob->flags & O_SWAPPED)
        {
            if (load_ob_from_swap(l->prog_ob) < 0)
                errorf("Out of memory\n");
        }

        do {
            int          lineno;
            char buf[20];
            string_t   * name = NULL;

            program_t  * prog    = l->prog_ob->prog;
            bytecode_p   prog_pc = prog->program + l->prog_pc;

            if (prog_pc <= prog->program || prog_pc >= PROGRAM_END(*prog))
                break;

            lineno = get_line_number( l->prog_ob->prog->program + l->prog_pc
                                    , l->prog_ob->prog
                                    , &name
                                    );

            sprintf(buf, "%d", lineno);

            rc = mstr_add(STR_FROM, name);
            free_mstring(name);
            rc = mstr_append(rc, STR_LINE);
            rc = mstr_append_txt(rc, buf, strlen(buf));
        } while(0);
    }

    if (!rc)
        rc = ref_mstring(STR_EMPTY);

    return rc;
} /* closure_location() */

/*-------------------------------------------------------------------------*/
string_t *
closure_to_string (svalue_t * sp, Bool compact)

/* Convert the closure <sp> into a printable string and return it.
 * If <compact> is true, a compact (spaceless) representation is used.
 */

{
    char buf[1024];
    string_t *rc;
    lambda_t *l;
    object_t *ob;
    size_t len;

    rc = NULL;
    buf[sizeof(buf)-1] = '\0';
    strcpy(buf, "#'");

    if (sp->type != T_CLOSURE)
    {
        fatal("closure_to_string() called for non-closure value %"PRIdPHINT":%"PRIdPHINT"\n"
             , sp->type, sp->x.generic
            );
        /* NOTREACHED */
        return NULL;
    }

    l = NULL;
      /* Will be set to valid pointer if the closure has a lambda_t structure.
       */

    switch(sp->x.closure_type)
    {

    case CLOSURE_IDENTIFIER: /* Variable Closure */
      {
        l = sp->u.lambda;
        if (l->ob->flags & O_DESTRUCTED)
        {
            strcat(buf, compact ? "<dest lvar>"
                                : "<local variable in destructed object>");
            break;
        }

        if (l->function.var_index == VANISHED_VARCLOSURE_INDEX)
        {
            strcat(buf, compact ? "<repl lvar>"
                                : "<local variable from replaced program>");
            break;
        }

        /* We need the program resident */
        if (O_PROG_SWAPPED(l->ob))
        {
            l->ob->time_of_ref = current_time;
            if (load_ob_from_swap(l->ob) < 0)
                errorf("Out of memory.\n");
        }

        sprintf(buf, "#'%s->%s"
                   , get_txt(l->ob->name)
                   , get_txt(l->ob->prog->variables[l->function.var_index].name)
              );
        break;
      }

    case CLOSURE_LFUN: /* Lfun closure */
      {
        program_t *prog;
        string_t  *function_name;
        Bool       is_inherited;

        l = sp->u.lambda;

        /* For alien lfun closures, prepend the object the closure
         * is bound to.
         */
        if (l->ob != l->function.lfun.ob)
        {
            ob = l->function.lfun.ob;

            if (ob->flags & O_DESTRUCTED)
            {
                strcat(buf, compact ? "[<dest obj>]" : "[<destructed object>]");
            }
            else
            {
                strcat(buf, "[");
                strcat(buf, get_txt(ob->name));
                strcat(buf, "]");
            }
        }

        ob = l->function.lfun.ob;

        closure_lookup_lfun_prog(l, &prog, &function_name, &is_inherited);

        if (ob->flags & O_DESTRUCTED)
            strcat(buf, compact ? "<dest lfun>" 
                                : "<local function in destructed object>");
        else
            strcat(buf, get_txt(ob->name));

        if (is_inherited)
        {
            strcat(buf, "(");
            strcat(buf, get_txt(prog->name));
            buf[strlen(buf)-2] = '\0'; /* Remove the '.c' after the program name */
            strcat(buf, ")");
        }
        strcat(buf, "->");
        strcat(buf, get_txt(function_name));
        strcat(buf, "()");
        break;
      }

    case CLOSURE_UNBOUND_LAMBDA: /* Unbound-Lambda Closure */
    case CLOSURE_PRELIMINARY:    /* Preliminary Lambda Closure */
      {
        l = sp->u.lambda;

        if (sp->x.closure_type == CLOSURE_PRELIMINARY)
            sprintf(buf, compact ? "<pre %p>" : "<prelim lambda %p>", l);
        else
            sprintf(buf, compact ? "<free %p>" : "<free lambda %p>", l);
        break;
      }

    case CLOSURE_LAMBDA:         /* Lambda Closure */
    case CLOSURE_BOUND_LAMBDA:   /* Bound-Lambda Closure */
      {
        l = sp->u.lambda;

        if (sp->x.closure_type == CLOSURE_BOUND_LAMBDA)
            sprintf(buf, compact ? "<bound %p:" : "<bound lambda %p:", l);
        else
            sprintf(buf, compact ? "<%p:" : "<lambda %p:", l);

        ob = l->ob;

        if (!ob)
        {
            strcat(buf, "{null}>");
        }
        else
        {
            if (ob->flags & O_DESTRUCTED)
                strcat(buf, "{dest}");
            strcat(buf, "/");
            strcat(buf, get_txt(ob->name));
            strcat(buf, ">");
        }
        break;
      }

    default:
      {
        int type = sp->x.closure_type;

        if (type >= 0)
            errorf("Bad arg 1 to to_string(): closure type %d.\n"
                 , sp->x.closure_type);
        else
        {
            switch(type & -0x0800)
            {
#ifdef USE_PYTHON
            case CLOSURE_PYTHON_EFUN:
                strcat(buf, closure_python_efun_to_string(type));
                break;
#endif
            case CLOSURE_OPERATOR:
              {
                const char *str = closure_operator_to_string(type);

                if (str)
                {
                    strcat(buf, str);
                    break;
                }

                type += CLOSURE_EFUN - CLOSURE_OPERATOR;
              }
            /* default action for operators: FALLTHROUGH */

            case CLOSURE_EFUN:
              {
                const char *str = closure_efun_to_string(type);

                if (str)
                {
                    strcat(buf, str);
                    break;
                }
              }
              /* Shouldn't happen: FALLTHROUGH */

            case CLOSURE_SIMUL_EFUN:
                strcat(buf, "sefun::");
                strcat(buf, get_txt(simul_efunp[type - CLOSURE_SIMUL_EFUN].name));
                break;
            }
            break;
        } /* if (type) */
      } /* case default */
    } /* switch(closure_type) */

    len = strlen(buf);
    memsafe(rc = new_n_unicode_mstring(buf, len), len, "converted lambda");

    /* If it's a closure with a lambda structure, we can determine
     * where it was created.
     */
    if (l && l->prog_ob && !(l->prog_ob->flags & O_DESTRUCTED))
    {
        string_t * rc2 = closure_location(l);
        string_t * rc3;

        /* Final step: append the created string rc2 to the original
         * string rc such that an out of memory condition won't
         * destroy rc itself.
         */
        rc3 = mstr_add(rc, rc2);
        if (rc3)
        {
            free_mstring(rc);
            rc = rc3;
        }

        free_mstring(rc2);
    }

    return rc;
} /* closure_to_string() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_bind_lambda (svalue_t *sp, int num_arg)

/* EFUN bind_lambda()
 *
 *     closure bind_lambda(closure cl [, object ob ])
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
 */

{
    object_t *ob;

    if (num_arg == 1)
    {
        /* this_object() is fine */
        ob = ref_object(current_object, "bind_lambda");
    }
    else /* (sp->type == T_OBJECT) */
    {
        /* If <ob> is given, check for a possible privilege breach */
        ob = sp->u.ob;
        if (ob != current_object
         && !privilege_violation(STR_BIND_LAMBDA, sp, sp))
        {
            free_object(ob, "bind_lambda");
            sp--;
            return sp;
        }

        sp--; /* points to closure now */
    }

    inter_sp = sp;

    switch(sp->x.closure_type)
    {
    case CLOSURE_LAMBDA:
    case CLOSURE_IDENTIFIER:
    case CLOSURE_PRELIMINARY:
        /* Unbindable closures. Free the ob reference and
         * throw an error (unless <ob> has been omitted)
         */
        free_object(ob, "bind_lambda");
        if (num_arg == 1)
            break;
        errorf("Bad arg 1 to bind_lambda(): unbindable closure\n");
        /* NOTREACHED */
        return sp;
        break;

    case CLOSURE_LFUN:
        /* Rebind an lfun to the given object */
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

        lambda_t *l;

        if ( (l = sp->u.lambda)->ref == 1)
        {
            /* We are the only user of the lambda: simply rebind it.
             */

            object_t **obp;

            obp = &l->ob;
            free_object(*obp, "bind_lambda");
            *obp = ob; /* Adopt the reference */
            break;
        }
        else
        {
            /* We share the closure with others: create our own
             * copy, bind it and put it onto the stack in place of
             * the original one.
             */
            lambda_t *l2;

            l->ref--;
            l2 = closure_new_lambda(ob, 0, /* raise_error: */ MY_TRUE);
            l2->function.lambda = l->function.lambda;
            l->function.lambda->ref++;
            free_object(ob, "bind_lambda"); /* We adopted the reference */
            sp->u.lambda = l2;
            break;
        }
      }

    case CLOSURE_UNBOUND_LAMBDA:
      {
        /* Whee, an unbound lambda: create the bound-lambda structure
         * and put it onto the stack in place of the unbound one.
         */

        lambda_t *l;
        l = closure_new_lambda(ob, 0, /* raise_error: */ MY_TRUE);
        free_object(ob, "bind_lambda"); /* We adopted the reference */
        l->function.lambda = sp->u.lambda;
          /* The ref to the unbound closure is just transferred from
           * sp to l->function.lambda.
           */
        sp->x.closure_type = CLOSURE_BOUND_LAMBDA;
        sp->u.lambda = l;
        break;
      }
    }

    return sp;
} /* v_bind_lambda() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_lambda (svalue_t *sp)

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

{
    lambda_t *l;
    vector_t *args;

    if (sp[-1].type != T_POINTER)
    {
        /* If '0' is given for the args array, replace it
         * with the null-vector.
         */
        if (sp[-1].type != T_NUMBER || sp[-1].u.number)
            efun_arg_error(1, T_POINTER, sp->type, sp);
        args = ref_array(&null_vector);
    }
    else
    {
        args = sp[-1].u.vec;
    }

    /* Create the lambda closure */
    l = lambda(args, sp, current_object);

    /* Clean up the stack and push the result */
    free_svalue(sp--);
    free_array(args);

    sp->type = T_CLOSURE;
    sp->x.closure_type = CLOSURE_LAMBDA;
    sp->u.lambda = l;

    return sp;
} /* f_lambda() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_symbol_function (svalue_t *sp)

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

{
    object_t *ob;
    program_t *prog;
    string_t *fun;
    int i;

    /* If 'arg' is not a symbol, make sure it's a shared string. */
    if (sp[-1].type != T_SYMBOL)
    {
        sp[-1].u.str = make_tabled(sp[-1].u.str);
    }

    /* If 'ob' is not of type object, it might be the name of
     * an object to load, or we need to make an efun symbol.
     */
    if (sp->type != T_OBJECT)
    {
        /* If it's the number 0, an efun symbol is desired */
        if (sp->type == T_NUMBER && sp->u.number == 0)
        {
            string_t *name;
            sp--;
            inter_sp = sp;
            name = sp->u.str;
            symbol_efun(name, sp);
            free_mstring(name);
            return sp;
        }

        /* Find resp. load the object by name */
        if (sp->type != T_STRING)
        {
            efun_exp_arg_error(2, TF_STRING|TF_OBJECT, sp->type, sp);
            /* NOTREACHED */
            return sp;
        }
        ob = get_object(sp->u.str);
        if (!ob)
            errorf("Object '%s' not found.\n", get_txt(sp->u.str));
        free_svalue(sp);
        put_ref_object(sp, ob, "symbol_function");
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
            errorf("Out of memory\n");
        }
    }

    /* Find the function in the program */
    prog = ob->prog;

    /* Remove any lfun:: prefix from the name. */
    fun = sp[-1].u.str;
    if (mstrsize(fun) >=6 && !strncmp(get_txt(fun), "lfun::", 6))
        fun = find_tabled_str_n(get_txt(fun)+6, mstrsize(fun)-6, fun->info.unicode);
    if (fun)
    {
        i = find_function(fun, prog);
        if (fun != sp[-1].u.str)
            free_mstring(fun);
    }
    else
        i = -1;

    /* If the function exists and is visible, create the closure
     */
    if ( i >= 0
      && ( !(prog->functions[i] & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|TYPE_MOD_PRIVATE) )
         || (    !(prog->functions[i] & TYPE_MOD_PRIVATE)
              && current_object == ob)
         )
       )
    {
        // check for deprecated functions.
        if (prog->functions[i] & TYPE_MOD_DEPRECATED)
        {
            warnf("Creating lfun closure to deprecated function \'%s\' in object %s (%s).\n",
                  get_txt(sp[-1].u.str),
                  get_txt(ob->name),
                  get_txt(ob->prog->name));
        }
        
        /* Clean up the stack */
        sp--;
        free_mstring(sp->u.str);
        inter_sp = sp-1;

        closure_lfun(sp, ob, NULL, (unsigned short)i, 0
                    , /* raise_error: */ MY_FALSE);
        if (sp->type != T_CLOSURE)
        {
            inter_sp = sp - 1;
            outofmem(SIZEOF_LAMBDA(0), "symbol_function");
        }

        /* The lambda was bound to the wrong object */
        free_object(sp->u.lambda->ob, "symbol_function");
        sp->u.lambda->ob = ref_object(current_object, "symbol_function");

        free_object(ob, "symbol_function"); /* We adopted the reference */

        return sp;
    }

    /* Symbol can't be created - free the stack and push 0 */
    free_object(ob, "symbol_function");
    sp--;
    free_mstring(sp->u.str);
    put_number(sp, 0);

    return sp;
} /* f_symbol_function() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_symbol_variable (svalue_t *sp)

/* EFUN symbol_variable()
 *
 *   closure symbol_variable(string arg)
 *   closure symbol_variable(symbol arg)
 *   closure symbol_variable(int arg)
 *
 * Constructs an identifier (lfun) closure from the global
 * variable arg of this_object(). The variable may be given as a
 * symbol, by name or by its ordinal number in the objects
 * variable table.
 * If there is no such variable, or if it is not visible outside
 * the object, 0 is returned.
 *
 * If the argument is an integer, and the variable is inherited
 * and private in the inherited object (i.e. hidden), then a
 * privilege violation ("symbol_variable", this_object(), arg)
 * will occur.
 *
 */

{
    object_t *ob;
    int n;         /* Index of the desired variable */

    ob = current_object;
    if (!current_variables
     || !ob->variables
     || current_variables < ob->variables
     || current_variables >= ob->variables + ob->prog->num_variables)
    {
        /* efun closures are called without changing current_prog nor
         * current_variables. This keeps the program scope for variables
         * for calls inside this_object(), but would give trouble with
         * calling from other ones if it were not for this test.
         */
        current_prog = ob->prog;
        current_variables = ob->variables;
    }

    /* Test and get the arguments; set n to the index of the desired
     * variable.
     */
    switch(sp->type)
    {
    default:
        fatal("Bad arg 1 to symbol_variable(): type %s\n", typename(sp->type));
        break;

    case T_NUMBER:  /* The index is given directly */
        n = sp->u.number;
        if (n < 0 || n >= current_prog->num_variables)
        {
            sp->u.number = 0;
            return sp;
        }

        if (current_prog->variables[n].type.t_flags & NAME_HIDDEN)
        {
            if (!privilege_violation(STR_SYMBOL_VARIABLE, sp, sp))
            {
                sp->u.number = 0;
                return sp;
            }
        }
        break;

    case T_STRING:  /* Name is given by string */
        if (!mstr_tabled(sp->u.str))
        {
            /* If the variable exists, it must exist as tabled
             * string.
             */
            string_t *str;

            str = find_tabled(sp->u.str);
            if (!str)
            {
                free_svalue(sp);
            	put_number(sp, 0);
            	return sp;
            }

            /* Make sp a tabled string value to continue processing */
            free_mstring(sp->u.str);
            sp->u.str = ref_mstring(str);
        }
        /* FALL THROUGH */

    case T_SYMBOL:  /* Name is given as shared string (symbol) */
      {
        string_t *str;
        variable_t *var;
        program_t *prog;
        int num_var;

        str = sp->u.str;
        prog = current_prog;
        var = prog->variables;
        num_var = prog->num_variables;
        for (n = num_var; --n >= 0; var++)
        {
            if (var->name == str && !(var->type.t_flags & NAME_HIDDEN))
                break;
        }
        free_mstring(str);
        if (n < 0)
        {
            put_number(sp, 0);
            return sp;
        }
        n = num_var - n - 1;
      }
    }
    // check for deprecated object / global variable.
    if (current_prog->variables[n].type.t_flags & TYPE_MOD_DEPRECATED)
    {
        warnf("Creating closure to deprecated global variable %s.\n",
              get_txt(current_prog->variables[n].name));
    }

    /* Check for virtual variables */
    if (current_prog == ob->prog)
    {
        /* When called from a lambda, variable_index_offset is 0,
         * even though there might be virtual variables.
         * But in these and all other cases, where current_prog
         * is the top program, no translation is necessary.
         */
    }
    else if (n < current_prog->num_virtual_variables)
    {
        /* Search for the virtual variable in the current_object's
         * variable block.
         */
        n = translate_virtual_variable_index(n);
    }
    else
    {
        /* Ordinary variables, translate the index into current_object's
         * variable block.
         */
        n = n - current_prog->num_virtual_variables
              + (current_variables - current_object->variables);
    }

    /* Create the result closure and put it onto the stack */
    closure_identifier( sp, current_object
                      , (unsigned short)(n)
                      , /* raise_error: */ MY_FALSE);
    if (sp->type != T_CLOSURE)
    {
        inter_sp = sp - 1;
        outofmem(SIZEOF_LAMBDA(0), "variable symbol");
    }

    return sp;
} /* f_symbol_variable() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_unbound_lambda (svalue_t *sp)

/* EFUN closure unbound_lambda()
 *
 *   closure unbound_lambda(mixed *args, mixed)
 *
 *
 * Constructs a lambda closure that is not bound to an object,
 * like lambda function in LISP.
 * The closure cannot contain references to global variables, and
 * all lfun closures are inserted as is, since there is no native
 * object for this closure. You need to bind it before it can be
 * called. Ordinary objects can only bind to themselves, binding
 * to other objects causes a privilege violation(). The point is
 * that previous_object for calls done from inside the closure
 * will reflect the object doing bind_lambda(), and all object /
 * uid based security will also refer to this object.
 *
 * The first argument is an array describing the arguments
 * (symbols) passed to the closure upon evaluation by funcall()
 * or apply(), the second arg forms the code of the closure.
 */

{
    lambda_t *l;
    vector_t *args;

    /* Get and test the arguments */
    if (sp[-1].type != T_POINTER)
    {
        if (sp[-1].type != T_NUMBER || sp[-1].u.number)
            efun_gen_arg_error(1, sp->type, sp);
        args = ref_array(&null_vector);
    }
    else
    {
        args = sp[-1].u.vec;
    }

    /* Compile the lambda */
    inter_sp = sp;
    l = lambda(args, sp, 0);
    l->ob = NULL;

    /* Clean up the stack and push the result */

    free_svalue(sp--);
    free_array(args);
    sp->type = T_CLOSURE;
    sp->x.closure_type = CLOSURE_UNBOUND_LAMBDA;
    sp->u.lambda = l;
    return sp;
} /* f_unbound_lambda() */

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
case_list_entry_t *
new_case_entry (void)

/* Allocate a new case_list_entry, insert it into the case list
 * and return the pointer.
 * The memory will be deallocated in the call to free_symbols().
 */

{
    case_list_entry_t *ret;

    /* Get a new case_list_entry from the free_block.
     * If the block is empty (or non-existing, the initial state),
     * get a new block.
     */
    if (!case_state.free_block
     || (ret = --case_state.next_free) == case_state.free_block)
    {
        case_list_entry_t *next;

        if ( !case_state.free_block
         || !(next = case_state.free_block->next)
           )
        {
            /* There was no following free block, so allocate a new
             * one and append it to the block list.
             */
            next = xalloc(sizeof(case_list_entry_t[CASE_BLOCKING_FACTOR]));
            next->next = NULL;
            if (!case_blocks)
            {
                /* Initialize the total block list */
                case_blocks = next;
                case_blocks_last = next;
            }
            else
            {
                /* Append the new block to the block list */
                case_blocks_last->next = next;
                case_blocks_last = next;
            }
        }

        /* Point .free_block to the new one and initialize .next_free */
        case_state.free_block = next;
        case_state.next_free = ret = next + CASE_BLOCKING_FACTOR - 1;
    }

    /* Add the new entry to the head of the entry list */
    /* DELETED: case_state.next_free->next = case_state.list1; */ /* TODO: ??? */
    ret->next = case_state.list1;
    case_state.list1 = case_state.list0;
    case_state.list0 = ret;

    return ret;
} /* new_case_entry() */

/*-------------------------------------------------------------------------*/
void
free_case_blocks (void)

/* Deallocate all block listed in case_blocks.
 */

{
    while (case_blocks)
    {
        case_list_entry_t *tmp;

        tmp = case_blocks;
        case_blocks = tmp->next;
        xfree(tmp);
    }
    case_blocks_last = NULL;
} /* free_case_blocks() */

/*-------------------------------------------------------------------------*/
void
store_case_labels( p_int total_length
                 , p_int default_addr
                 , Bool numeric
                 , case_list_entry_t *zero
                 , bytecode_p (*get_space)(p_int)
                 , void (*move_instructions)(int, p_int)
                 , void (*cerror)(const char *)
                 , void (*cerrorl)(const char *, const char *, int, int)
                 )

/* This function creates the lookup tables for a switch instruction.
 * It expects that 'SWITCH b1 a2 <instruction>' has already been created
 * (with dummies for b1 and a2). The position of 'b1' is the reference
 * point for all addresses and offsets in the arguments. Speaking of arguments:
 *
 *   total_length:      length of the generated code so far
 *   default_addr:      address of the default-case code
 *   numeric:           flag if the switch has numeric or string cases.
 *   zero:              the case_list_entry for 'case 0', or NULL if none.
 *   get_space:         function to allocate more code space
 *   move_instructions: function to move a block of bytecode
 *   cerror, cerrorl:   error functions.
 *
 * For more detailed information about the argument functions, look at the
 * lambda_... implementations in this file.
 *
 * The created switch is not aligned, because later code generation ops may
 * still move this piece of code.
 */

{
    case_list_entry_t *list0; /* (Sorted) list of case_entries */
    case_list_entry_t *list1; /* Current case_entry */
    int        type;          /* The type byte */
    mp_int     runlength;     /* Mergesort runlength */
    mp_int     key_num;       /* Number of keys */
    int        len;
    int        i;
    int        o;
    p_int      maxspan;       /* Max span of one 'l' lookup range */
    mp_int     current_key;   /* Current key */
    mp_int     last_key = 0;  /* Last key a table entry was generated for */
    mp_int     current_addr;  /* Adr of instructions for current_key */
    mp_int     last_addr;     /* Adr of instruction for last_key */
    bytecode_p p;
    mp_int     tablen;
    bytecode_t i0;

    list0 = case_state.list0;
    list1 = case_state.list1;

    /* Determine the type of the switch: numeric or string */
    if (numeric)
    {
        type = 0;
    }
    else
    {
        type = SWITCH_TYPE;
        if (zero)
        {
            /* 'case 0' in string-switches is special */
            zero->key = (p_int)ZERO_AS_STR_CASE_LABEL;
        }
    }

    /* length(list0) >= length(list1) */
    if (!list0)
        (*cerror)("switch without case not supported");

    /* Mergesort the list of case entries by their keys in ascending
     * order.
     *
     * The implementation combines the merge and split phase by
     * using two 'out' lists and switching them appropriately.
     */

    for (runlength = 1; list1; runlength *= 2)
    {
        case_list_entry_t *out_hook0, *out_hook1;
          /* The two out lists */
        case_list_entry_t **out0, **out1;
          /* Indirect access to the out lists, which also
           * helps in creating the single links of the lists.
           */
        mp_int count0, count1;
          /* Number of list elements left to merge in this run */

        out0 = &out_hook0;
        out1 = &out_hook1;
        while (list1)
        {
            /* Merge the next <runlength> elements from both lists */

            count0 = count1 = runlength;
            while (1)
            {
                if (list1->key < list0->key)
                {
                    /* Put element from list1 into out list */
                    *out0 = list1;
                    out0 = &list1->next;
                    list1 = *out0;

                    if (!--count1 || !list1)
                    {
                        /* All elements from list1 processed, now
                         * append the remaining ones from list0.
                         */
                        *out0 = list0;
                        do {
                            out0 = &list0->next;
                            list0 = *out0;
                        } while (--count0 && list0);
                        break;
                    }
                }
                else
                {
                    /* Put element from list0 into out list */
                    *out0 = list0;
                    out0 = &list0->next;
                    list0 = *out0;

                    if (!--count0 || !list0)
                    {
                        /* All elements from list0 processed, now
                         * append the remaining ones from list1.
                         */
                        *out0 = list1;
                        do {
                            out0 = &list1->next;
                            list1 = *out0;
                        } while (--count1 && list1);
                        break;
                    }
                }
            } /* while(1) */

            /* 2*runlength elements put into out0,
             * now switch the roles of out0 and out1.
             */
            {
                case_list_entry_t **temp;

                temp = out0;
                out0 = out1;
                out1 = temp;
            }
        } /* while (list1) */

        *out0 = list0;
        *out1 = NULL;
        list0 = out_hook0;
        list1 = out_hook1;
    } /* for (runlength, list1) */

    /* list0 now contains all entries, sorted.
     * Scan the list and determine the size of the switch, moving
     * the so far generated code if necessary.
     * For a numeric switch, also compute the range information and
     * generate the 'l' lookup table for sparse ranges.
     */
    key_num = 0;
    if (numeric)
    {
    	/* Numeric switch: scan the list for ranges
         * (which might have been separated during the sort).
         */
        case_list_entry_t *table_start;
          /* Begin of range to write a 'l' lookup range for */
        case_list_entry_t *max_gain_end = NULL;
          /* End of range to write a 'l' lookup range for */
        case_list_entry_t *previous = NULL;
        case_list_entry_t *range_start = NULL;
          /* Range currently build */
        int last_line = 0;  /* Source line of last_key */
        p_int keys;       /* Number of keys covered by this lookup table entry */
        p_int max_gain;     /* total gain so far */
        p_int cutoff;       /* Cutoff point during lookup table generation */

        /* Walk the list and join consecutive cases to ranges. Intermediate
         * entries are removed from the list, explicit range end entries
         * are left in the list.
         */
        for (last_addr = 0xffffff, list1=list0; list1; list1 = list1->next)
        {
            int curr_line;  /* Source line of current_key */

            key_num++;
            current_key = list1->key;
            curr_line = list1->line;
            current_addr = list1->addr;

            if (current_key == last_key && list1 != list0)
            {
                (*cerrorl)("Duplicate case%s", " in line %d and %d",
                    last_line, curr_line);
            }

            if (curr_line)
            {
            	/* Not a range end */
                if (last_addr == 1)
                {
                    (*cerrorl)(
                      "Discontinued case label list range%s",
                      ", line %d by line %d",
                      last_line, curr_line);
                }
                else if (current_key == last_key + 1)
                {
                    /* Consecutive keys: maybe a new case, maybe
                     * a continuation. Look at the code addresses
                     * to decide.
                     */
                    if (current_addr == last_addr)
                    {
                        /* range continuation with single value */
                        if (list1 != range_start->next)
                        {
                            range_start->addr = 1;
                              /* Mark the range start, in case it didn't
                               * happen already.
                               */
                            range_start->next = list1;
                              /* list1 becomes the new range end, replacing
                               * the old one.
                               */
                            list1->line = 0;
                              /* lookup table building uses !end->line */
                            key_num--;
                        }
                    }
                    else if (current_addr == 1
                          && list1->next->addr == last_addr)
                    {
                        /* range continuation with range start */

                        key_num -= 1 + (list1 != range_start->next);
                        range_start->addr = 1;
                          /* Mark the range start, in case it didn't
                           * happen already.
                           */
                        range_start->next = list1->next;
                          /* list1 becomes the new range end, replacing
                           * the old one.
                           */
                        /* list1->next was range end before, thus
                         * range_start->next->line == 0 already.
                         */
                        list1 = range_start;
                          /* list1 is now outside the list, therefore
                           * re-init list1 for proper continuation.
                           */
                    }
                    else
                    {
                        /* New range, or a single case */
                        range_start = list1;
                    }
                }
                else
                {
                    /* New range, or a single case */
                    range_start = list1;
                }
            }
            last_key = current_key;
            last_line = curr_line;
            last_addr = current_addr;
        } /* for() */
        /* The list contains now single cases, ranges, and some spurious
         * range ends. The following length computation is therefore a bit
         * on the big side.
         */

        /* Compute the needed offset size for the switch */
        if ( !( (total_length + key_num*(sizeof(p_int)+1)) & ~0xff) ) {
            len = 1;
            maxspan = PINT_MAX/len;
        }
        else if ( !( (total_length + key_num*(sizeof(p_int)+2) + 1) & ~0xffff) )
        {
            len = 2;
            maxspan = PINT_MAX/len;
        }
        else if ( !( (total_length + key_num*(sizeof(p_int)+3) + 2) & ~0xffffff) )
        {
            len = 3;
            maxspan = PINT_MAX/len;
        }
        else
        {
            (*cerror)("offset overflow");
            return;
        }

        /* For bigger offset sizes, move the instruction block to make
         * space for the additional bytes after the F_SWITCH.
         */
        if (len > 1)
        {
            (*move_instructions)(len-1, total_length);
            total_length += len-1;
            default_addr += len-1;
        }

        /* Now generate the 'l' lookup table for sparse ranges.
         * For every singular case count up how many bytes a range lookup
         * would save, and generate the next 'l' table entry when
         * the cutoff point has been reached (dynamic programming).
         * If the gain is negative, keep it singular.
         */
        cutoff =(long)(sizeof(p_int)*2 + len*2);
        list1 = list0;
        table_start = list1;
        for (max_gain = keys = 0; list1; list1 = list1->next)
        {
            p_int span, gain;

            keys++;
            if (list1->addr == 1)
            {
            	/* Range case - no gain possible here */
                previous = list1;
                continue;
            }

            /* Btw, adapt the .addr to the offset length */
            list1->addr += len-1;

            span = list1->key - table_start->key + 1;
            if ((p_uint)span >= (p_uint)maxspan) /* p_uint to catch span<0, too */
                gain = -1;
            else
                gain = (long)(keys * sizeof(p_int) - (span - keys)* len);

            /* If the gain is big enough, write the next l table entry
             * for the list from table_start to max_gain_end.
             */
            if (max_gain - gain > cutoff && max_gain >= cutoff)
            {
                case_list_entry_t *tmp;
                p_int key, addr, size;
                bytecode_p p0;

                span = max_gain_end->key - table_start->key + 1;
                size = span * len;
                p0 = (bytecode_p)(*get_space)(size);
                tmp = table_start;
                key = tmp->key;

                if (tmp->addr == 1)
                {
                    /* table_start is a range start: start with
                     * the associated end.
                     */
                    key_num--;
                    tmp = tmp->next;
                }

                /* Loop over the partial list, inserting the jump address
                 * for every singular case and range, and the default_addr
                 * for every other value without a case.
                 * The
                 */
                do {
                    if (tmp->key < key)
                    {
                    	/* key is beyond the current list entry - move
                    	 * on to the next.
                    	 */
                        key_num--;
                        tmp = tmp->next;
                        /* This next entry might be the begin of a new range.
                         * However, we don't want to move tmp to its end
                         * entry quite yet, since we still have to fill in
                         * the 'default' jump points for all interim values.
                         */
                    }
                    if (key == tmp->key && tmp->addr == 1)
                    {
                        /* tmp is the beginning of a range, and key (finally)
                         * caught up with it. We can now move tmp to the end
                         * entry for this range.
                         * It's .line is 0 which will force the code
                         * to insert all values for this range.
                         */
                        key_num--;
                        tmp = tmp->next;
                    }

                    /* Get the address to insert */
                    addr = default_addr;
                    if (key == tmp->key  || !tmp->line)
                        addr = tmp->addr;

                    /* Insert the address */
                    p0 += len;
                    PUT_UINT8(p0-1, (unsigned char)addr);
                    if (len >= 2)
                    {
                        PUT_UINT8(p0-2, (unsigned char)(addr >> 8));
                        if (len > 2)
                        {
                            PUT_UINT8(p0-3, (unsigned char)(addr >> 16));
                        }
                    }
                } while (++key <= max_gain_end->key);

                /* Replace the partial list with singular range, and mark
                 * it as sparse lookup range.
                 */
                key_num += 1;
                max_gain_end->addr = total_length;
                total_length += size;
                table_start->addr = 0;
                table_start->next = max_gain_end;

                /* Restart the gain search */
                gain = -1;
            }

            if (gain < 0)
            {
            	/* No gain with this entry - restart search
            	 * from here.
            	 */
                if (list1->line)
                {
                    /* Not a range end */
                    table_start = list1;
                    keys = 1;
                }
                else
                {
                    /* A range end: restart from the range start */
                    table_start = previous;
                    keys = 2;
                }
                max_gain = 0;
            }
            else if (gain > max_gain)
            {
            	/* We gained space: remember this position */
                max_gain = gain;
                max_gain_end = list1;
            }
        } /* for (write lookup table) */
    }
    else
    {
        /* String case: neither ordinary nor lookup table ranges are viable.
         * Thus, don't spend unnecesarily time with calculating them.
         * Also, a more accurate calculation of len is possible.
         */
        int last_line = 0;

        /* Compute the number of keys, and check that none are duplicate.
         */
        for (list1 = list0; list1; list1 = list1->next)
        {
            int curr_line;

            key_num++;
            current_key = list1->key ;
            curr_line = list1->line ;
            if ( current_key == last_key && list1 != list0) {
                (*cerrorl)("Duplicate case%s", " in line %d and %d",
                    last_line, curr_line);
            }
            last_key = current_key;
            last_line = curr_line;
        }

        if (        !( ( total_length   | key_num*sizeof(p_int)) & ~0xff) ) {
            len = 1;
        } else if ( !( ((total_length+1) | key_num*sizeof(p_int)) & ~0xffff) ) {
            len = 2;
        } else if ( !( ((total_length+2) | key_num*sizeof(p_int)) & ~0xffffff) ) {
            len = 3;
        } else {
            (*cerror)("offset overflow");
            return;
        }

        if (len > 1)
        {
            (*move_instructions)(len-1, total_length);
            total_length += len-1;
            default_addr += len-1;
            for (list1 = list0; list1; list1 = list1->next)
            {
                list1->addr += len-1;
            }
        }
    }

    /* calculate starting index for iterative search at execution time */
    for (i = 0, o = 2; o <= key_num; )
        i++, o<<=1;

    /* and store it */
    type |= (i & SWITCH_START) | (len << SWITCH_TYPE_VALUELEN_SHIFT);

    /* Store the 'type' byte and the table length */
    tablen = (long)(key_num * sizeof(p_int));
      /*   = key_num << SWITCH_TABLEN_SHIFT */
    p = (bytecode_p)get_space((long)
        (tablen + key_num * len + 2 + len + sizeof(p_int) - 4));

    PUT_UINT8(p-total_length, (unsigned char)tablen);
    PUT_UINT8(p-total_length+1, (unsigned char)type);

    /* Store the total length, and rescue the first instruction byte i0.
     */
    i0 = GET_UINT8(p-total_length+1+len);
    PUT_UINT8(p-total_length+2, (unsigned char)total_length);
    if (len >= 2)
    {
        STORE_UINT8(p, (bytecode_t)(tablen >> 8));
        PUT_UINT8(p-total_length+2, (unsigned char)(total_length >> 8));
        if (len > 2)
        {
            STORE_UINT8(p, (bytecode_t)(tablen >> 16));
            PUT_UINT8(p-total_length+2, (unsigned char)(total_length >> 16));
        }
    }

    /* Store the default address and the saved instruction byte i0.
     */
    STORE_SHORT(p, (short)default_addr);
    STORE_CODE(p, i0);

    /* Dummy bytes for aligning. */
    p += sizeof(p_int) - 4;

    /* Create the value table 'v' */
    for (list1 = list0; list1; list1 = list1->next)
    {
        memcpy(p, &list1->key, sizeof(list1->key));
        p += sizeof(list1->key);
    }

    /* Create the offset table 'o' */
    for (list1 = list0; list1; list1 = list1->next)
    {
        p += len;
        PUT_UINT8(p-1, (unsigned char)list1->addr);
        if (len >= 2)
        {
            PUT_UINT8(p-2, (unsigned char)(list1->addr >> 8));
            if (len > 2)
            {
                PUT_UINT8(p-3, (unsigned char)(list1->addr >> 16));
            }
        }
    }
    if (len > 2)
    {
        p = (*get_space)(1);
        PUT_UINT8(p, (unsigned char)(default_addr >> 16));
    }
} /* store_case_labels() */

/*-------------------------------------------------------------------------*/
void
align_switch (bytecode_p pc)

/* Align the switch instruction starting at <pc> with the byte 'b1'.
 * Called from interpret.c when an unaligned switch is encountered
 * the first time.
 */

{
    int len;
    int32 tablen, offset, size;
    unsigned char a2;  /* Alignment byte 2 */
    unsigned char abuf[sizeof(p_int)-1]; /* Buffer for the alignment bytes */
    bytecode_p     off_pc;  /* Pointer after the instruction block */
    unsigned char *startu;  /* Unaligned start address */
    unsigned char *starta;  /* Aligned start address */

    /* Get the valuelength from the 'type' byte and put it into
     * the 'b1' byte where it belongs.
     */
    tablen = GET_UINT8(pc);
    a2 = GET_UINT8(pc+1);
    len = a2 >> SWITCH_TYPE_VALUELEN_SHIFT;
    PUT_UINT8(pc, GET_UINT8(pc) | len);

    /* Get the offset, and move the first bytes */
    offset = GET_UINT8(pc+2);
    PUT_UINT8(pc+1, GET_UINT8(pc+2));
    if (len >=2)
    {
        PUT_UINT8(pc+2, GET_UINT8(pc+3));
        offset += GET_UINT8(pc+3) << 8;
        if (len > 2)
        {
            PUT_UINT8(pc+3, GET_UINT8(pc+4));
            offset += GET_UINT8(pc+4) << 16;
        }
    }

    if (len >=2)
    {
        tablen += GET_UINT8(pc+offset) << 8;
        if (len > 2)
        {
            tablen += GET_UINT8(pc+offset+1) << 16;
        }
    }

    /* Now align the tables, moving the alignment bytes around */
    off_pc = pc + offset + len;

    abuf[0] = off_pc[-1];
    abuf[1] = off_pc[0];
    abuf[2] = a2;
    PUT_UINT8(pc+len+1, GET_UINT8(off_pc+1));
    PUT_UINT8(off_pc+1, a2);
    startu = off_pc+2 + sizeof(p_int) - 4;
    starta = (unsigned char *)((p_int)startu & ~(sizeof(char *)-1));
    size = (long)(tablen + tablen / sizeof(char*) * len);
    if (starta != startu)
    {
        memmove(starta, startu, (size_t)size);
        memmove(starta+size, abuf + sizeof abuf - (startu-starta)
                   , (size_t)(startu-starta));
    }
} /* align_switch() */

/***************************************************************************/
