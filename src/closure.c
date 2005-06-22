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
 *   the operator (see symbol_operator() for the exact mapping).
 *   u.ob is the object this closure is bound to.
 *
 * efun closure:       type = CLOSURE_EFUN (-0x1000) + instr
 *   These are the closures for the LPC efuns (e.g. #'atan or #'call_other).
 *   The 'instr' is usually the machine instruction implementing
 *   the efun (see symbol_efun() for the exact mapping).
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
 *   Reference to a lfun in the same object.
 *   u.lambda points to the lambda structure with the detailed data.
 *
 * alien-lfun closure:     type = CLOSURE_ALIEN_LFUN (1)
 *   Reference to a lfun in an other object.
 *   u.lambda points to the lambda structure with the detailed data.
 *
 * identifier closure:     type = CLOSURE_IDENTIFIER (2)
 *   Reference to a variable in this object.
 *   u.lambda points to the lambda structure with the detailed data.
 *
 * preliminary closure:    type = CLOSURE_PRELIMINARY (3)
 *   TODO: ???
 *
 * bound lambda closure:   type = CLOSURE_BOUND_LAMBDA (4)
 *   This is an unbound lambda closure which was bound to an object.
 *   To allow binding the same unbound lambda to different objects
 *   at the same time, this construct uses a double indirection:
 *   u.lambda points to a lambda structure with the binding information,
 *   which then points to the actual unbound lambda structure.
 *
 * lambda closure:         type = CLOSURE_LAMBDA (5)
 *   Lambda closure bound to an object at compilation time.
 *   u.lambda points to the lambda structure with the compiled function.
 *
 * unbound lambda closure: type = CLOSURE_UNBOUND_LAMBDA (6)
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
 *           unsigned short index;
 *               _LFUN:       index in the function table
 *               _IDENTIFIER: index in the variable table
 *               Function indices are lower than CLOSURE_IDENTIFIER_OFFS
 *               (0xe800), variable indices are higher.
 *               The special value VANISHED_VARCLOSURE_INDEX (-1) is
 *               used to mark vanished variables.
 *
 *           bytecode_t code[1];
 *               LAMBDA and UNBOUND_LAMBDA closures: the function code.
 *               The first bytes are:
 *                 +0: uint8 num_values
 *                 +1: uint8 num_args
 *                 +2: uint8 num_vars
 *                 +3...: the function code
 *               'num_values' is the number of constants store before
 *               the lambda structure. If it is 0xff, the actual number
 *               is stored in .values[-0x100].u.number.
 *
 *           lambda_t *lambda;
 *               BOUND_LAMBDA: pointer to the UNBOUND_LAMBDA structure.
 *
 *           struct -- CLOSURE_ALIEN_LFUN
 *           {
 *               object_t *ob;
 *                   Originating object
 *               unsigned short  index
 *                   Index in the objects variable table
 *           } alien;
 *       } function;
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
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>

#include "closure.h"
#include "array.h"
#include "backend.h"
#include "exec.h"
#include "instrs.h"
#include "interpret.h"
#include "lang.h"     /* for the L_ tokens */
#include "lex.h"
#include "main.h"
#include "object.h"
#include "prolang.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "stralloc.h"
#include "svalue.h"
#include "swap.h"
#include "switch.h"
#include "xalloc.h"

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
#define ZERO_ACCEPTED     0x01  /* in:  a return value of zero need not be coded */
#define VOID_ACCEPTED     0x02  /* in:  any return value can be left out */
#define VOID_GIVEN        0x04  /* out: no return value given */
#define NEGATE_ACCEPTED   0x08  /* in:  Caller accepts a reversed logic result */
#define NEGATE_GIVEN      0x10  /* out: Result is in reversed logic */
#define REF_REJECTED      0x20  /* in:  lvalues not accepted */

#define VOID_WANTED (ZERO_ACCEPTED | VOID_ACCEPTED | NEGATE_ACCEPTED)
  /* all "don't care for the result" flags.
   */


/* Flags passed to compile_lvalue()
 */
#define USE_INDEX_LVALUE  0x1  /* Use INDEX_LVALUE instead of PUSH_INDEX_LVALUE */
#define PROTECT_LVALUE    0x2  /* Protect the generated lvalue */


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
    char *name;            /* Shared name of the symbol (not counted as ref) */
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

static void lambda_error VARPROT((char *error_str, ...), printf, 1, 2) NORETURN;
static void free_symbols(void);
static Bool is_lvalue (svalue_t *argp, int index_lvalue);
static void compile_lvalue(svalue_t *, int);

/*-------------------------------------------------------------------------*/
static INLINE int
function_cmp (const char *name, const program_t *prog, int ix)

/* Compare <name> with the name of function number <ix> in <prog>ram.
 * Result:  0: <name> is equal to the indexed name
 *         >0: <name> is smaller
 *         <0: <name> is bigger
 *
 * Note that both names are shared strings, so only the pointers are
 * compared.
 */

{
    funflag_t flags;

    /* Set ix to the memory offset for the (possibly inherited) function
     * function.
     */
    ix = prog->function_names[ix];
    flags = prog->functions[ix];
    while (flags & NAME_INHERITED)
    {
        inherit_t *inheritp;

        inheritp = &prog->inherit[flags & INHERIT_MASK];
        prog = inheritp->prog;
        ix -= inheritp->function_index_offset;
        flags = prog->functions[ix];
    }

    /* Return the result of the comparison */
#ifdef ALIGN_FUNCTIONS
    return (int)(*((char **)FUNCTION_NAMEP(prog->program + (flags & FUNSTART_MASK)))
                 - name);
#else
    return memcmp( &name, FUNCTION_NAMEP(prog->program + (flags & FUNSTART_MASK))
                 , sizeof name
    );
#endif
} /* function_cmp() */

/*-------------------------------------------------------------------------*/
long
find_function (const char *name, const program_t *prog)

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
         || left->x.closure_type == CLOSURE_ALIEN_LFUN
         || left->x.closure_type == CLOSURE_IDENTIFIER
        )
     && left->u.lambda->ob == right->u.lambda->ob
       )
    {
        if (left->x.closure_type == CLOSURE_ALIEN_LFUN)
            i =    (   left->u.lambda->function.alien.ob
                    == right->u.lambda->function.alien.ob)
                && (   left->u.lambda->function.alien.index
                    == right->u.lambda->function.alien.index);
        else
            i =    (   left->u.lambda->function.lfun.index
                    == right->u.lambda->function.lfun.index)
                && (   left->u.lambda->function.lfun.inhIndex
                    == right->u.lambda->function.lfun.inhIndex);
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
    if (left->x.closure_type == CLOSURE_LFUN
     || left->x.closure_type == CLOSURE_ALIEN_LFUN
     || left->x.closure_type == CLOSURE_IDENTIFIER
       )
    {
        if (left->u.lambda->ob != right->u.lambda->ob)
        {
            return (left->u.lambda->ob < right->u.lambda->ob) ? -1 : 1;
        }

        if (left->x.closure_type == CLOSURE_ALIEN_LFUN)
        {
            if ( left->u.lambda->function.alien.ob
              != right->u.lambda->function.alien.ob)
            {
                return (  left->u.lambda->function.alien.ob
                        < right->u.lambda->function.alien.ob)
                       ? -1 : 1;
            }

            /* This is the only field left, so it is guaranteed to differ */
            return (  left->u.lambda->function.alien.index
                    < right->u.lambda->function.alien.index)
                   ? -1 : 1;
        }
        else
        {
            if (   left->u.lambda->function.lfun.inhIndex
                != right->u.lambda->function.lfun.inhIndex
               )
                return (  left->u.lambda->function.lfun.inhIndex
                        < right->u.lambda->function.lfun.inhIndex)
                       ? -1 : 1;

            /* This is the only field left, so it is guaranteed to differ */
            return (  left->u.lambda->function.lfun.index
                    < right->u.lambda->function.lfun.index)
                   ? -1 : 1;
        }
    }

    /* Normal closure: compare the lambda pointers */
    return (left->u.lambda < right->u.lambda) ? -1 : 1;
} /* closure_cmp() */

/*-------------------------------------------------------------------------*/
mp_int
closure_hash (svalue_t *sp)
{
    /* Compute the hash value of this closure */
    mp_int i;
    
    if (sp->x.closure_type == CLOSURE_LFUN
     || sp->x.closure_type == CLOSURE_ALIEN_LFUN
     || sp->x.closure_type == CLOSURE_IDENTIFIER
       )
    {
	i = (mp_int) sp->u.lambda->ob;

        if (sp->x.closure_type == CLOSURE_ALIEN_LFUN)
        {
	    i = i ^ (mp_int) sp->u.lambda->function.alien.ob 
		  ^ sp->u.lambda->function.alien.index;
        }
        else
        {
	    i = i ^ sp->u.lambda->function.lfun.inhIndex
		  ^ sp->u.lambda->function.lfun.index;
        }
    }
    else
	i = (mp_int) sp->u.lambda;
	
    return i;
} /* closure_hash() */

/*-------------------------------------------------------------------------*/
Bool
lambda_ref_replace_program( lambda_t *l, int type
                          , p_int size, vector_t *args, svalue_t *block)

/* The lambda <l> of type <type> is about to be bound to the current object,
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
        if (r_ob->ob == current_object)
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
        object_t *curobj = current_object;

        prog = owner->prog;
        l = svp->u.lambda;
        ix = l->function.lfun.index;

        current_object = owner;
          /* Needed by lambda_ref_replace_program().
           * Will be restored from curobj below.
           */

        /* If the program is scheduled for replacement (or has been replaced),
         * create the protector for the closure, otherwise mark the object
         * as referenced by a lambda.
         */
        type = CLOSURE_LFUN;

        if ( !(prog->flags & P_REPLACE_ACTIVE) )
        {
            owner->flags |= O_LAMBDA_REFERENCED;
        }
        else if (!lambda_ref_replace_program( l
                                            , ix >= CLOSURE_IDENTIFIER_OFFS
                                              ? CLOSURE_IDENTIFIER
                                              : CLOSURE_ALIEN_LFUN
                                            , 0, NULL, NULL)
           )
        {
            owner->flags |= O_LAMBDA_REFERENCED;
        }
        else
        {
            type = CLOSURE_ALIEN_LFUN;
        }

        current_object = curobj;


        /* Set the svp->x.closure_type to the type of the closure. */

        if (ix >= CLOSURE_IDENTIFIER_OFFS)
        {
            /* Identifier closure */
            ix -= CLOSURE_IDENTIFIER_OFFS;
            svp->x.closure_type = CLOSURE_IDENTIFIER;

            l->function.lfun.index = (unsigned short)ix;
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
            svp->x.closure_type = type;

            if (type == CLOSURE_LFUN)
            {
                l->function.lfun.index = (unsigned short)ix;
            }
            else
            {
                l->function.alien.ob = ref_object(owner, "closure");
                l->function.alien.index = (unsigned short)ix;
            }
        }

        /* (Re)Bind the closure */
        free_object(l->ob, "closure");
        l->ob = ref_object(owner, "set_closure_user");
    }
} /* set_closure_user() */

/*-------------------------------------------------------------------------*/
void
replace_program_lambda_adjust (replace_ob_t *r_ob)

/* This function is called as the last step during the replacement of an
 * object's program, but only if the object has been marked to hold
 * closure references.
 *
 * The function is called in the backend context and catches errors during
 * its execution.
 */

{
    static struct lambda_replace_program_protector *current_lrpp;
      /* Copy of lrpp, static to survive errors */

    struct lambda_replace_program_protector *lrpp;
      /* Current protector */

    struct lambda_replace_program_protector *next_lrpp;
      /* Next protector */

    struct error_recovery_info error_recovery_info;

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
                lambda_t *l;
                int i;

                /* Adjust the index of the lfun
                 * If the lfun closure is a reference to an inherited
                 * program we need to check if the inheritance relation
                 * changes.
                 */
                l = lrpp->l.u.lambda;

                if (!l->function.lfun.inhIndex)
                    i = l->function.lfun.index -= r_ob->fun_offset;
                else
                {
                    i = -1; /* Default for 'not found' */

                    do {
                        int j;
                        inherit_t * oldInh;

                        oldInh = &r_ob->ob->prog->inherit[l->function.lfun.inhIndex-1];

                        /* First possibility: the new program is the same
                         * one the closure is pointing to.
                         * In that case, convert the closure into a straight
                         * lfun closure.
                         */
                        if (oldInh->prog == r_ob->new_prog)
                        {
                            i = l->function.lfun.index;
                            l->function.lfun.inhIndex = 0;
                            break;
                        }

                        /* Second possibility: the new program still
                         * inherits the program the closure is referencing.
                         * In that case, just update the inhIndex.
                         */
                        for (j = 0; j < r_ob->new_prog->num_inherited; ++j)
                        {
                            if (r_ob->new_prog->inherit[j].prog == oldInh->prog)
                            {
                                i = l->function.lfun.index;
                                l->function.lfun.inhIndex = j;
                                break;
                            }
                        }
                        if (i != -1)
                            break;

                        /* Lastly: the program is not found, so we fail
                         * and keep the indicator i at -1.
                         */
                    } while(0);
                }

                /* If the function vanished, replace it with a default,
                 * converting it into an alien-lfun closure
                 */
                if (i < 0 || i >= r_ob->new_prog->num_functions)
                {
                    assert_master_ob_loaded();
                    free_object(l->ob, "replace_program_lambda_adjust");
                    l->ob = ref_object( master_ob
                                      , "replace_program_lambda_adjust");
                    i = find_function( findstring("dangling_lfun_closure")
                                     , master_ob->prog);
                    l->function.lfun.index = (unsigned short)(i < 0 ? 0 : i);
                    l->function.lfun.inhIndex = 0;
                }
            }
            else if (lrpp->l.x.closure_type == CLOSURE_ALIEN_LFUN)
            {
                lambda_t *l;
                int i;

                /* Adjust the index of the lfun */
                l = lrpp->l.u.lambda;
                i = l->function.alien.index -= r_ob->fun_offset;

                /* If the function vanished, replace it with a default */
                if (i < 0 || i >= r_ob->new_prog->num_functions)
                {
                    assert_master_ob_loaded();
                    free_object( l->function.alien.ob
                               , "replace_program_lambda_adjust");
                    l->function.alien.ob
                        = ref_object(master_ob
                                    , "replace_program_lambda_adjust");
                    i = find_function( findstring("dangling_lfun_closure")
                                     , master_ob->prog);
                    l->function.alien.index = (unsigned short)(i < 0 ? 0 :i);
                }
            }
            else /* CLOSURE_IDENTIFIER */
            {
                lambda_t *l;
                int i;

                /* Adjust the index of the identifier */
                l = lrpp->l.u.lambda;
                i = l->function.lfun.index -= r_ob->var_offset;

                /* If it vanished, mark it as such */
                if (i >= r_ob->new_prog->num_variables)
                {
                    l->function.lfun.index = VANISHED_VARCLOSURE_INDEX;
                    /* TODO: This value should be properly publicized and
                     * TODO:: tested.
                     */
                }
            }
        } /* if (!CLOSURE_HAS_CODE()) */
    } while ( NULL != (lrpp = lrpp->next) );

    /* Second pass: now adjust the lambda closures.
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
    rt_context = (rt_context_t *)&error_recovery_info;
    if (setjmp(error_recovery_info.con.text))
    {
        bytecode_p p;

        lrpp = current_lrpp;

        /* Replace the function with "undef" */
        p = LAMBDA_CODE(lrpp->l.u.lambda->function.code);
        p[0] = F_ESCAPE;
        p[1] = F_UNDEF - 0x100;

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
            if ( (num_values = LAMBDA_NUM_VALUES(l->function.code)) == 0xff)
                num_values = svp[-0x100].u.number;

            svp2 = (svalue_t *)l2;
            if ( (num_values2 = LAMBDA_NUM_VALUES(l2->function.code)) == 0xff)
                num_values2 = svp2[-0x100].u.number;
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

                error("Cannot adjust lambda closure after replace_program(), "
                      "object %s\n", r_ob->ob->name);
            }

            /* The recompiled lambda can be used (and has to: think changed
             * indices), so replace the original by the new one.
             * We have to keep the memory of the original one as other
             * code might already reference it.
             */
            while (--num_values >= 0)
                transfer_svalue(--svp, --svp2);
            memcpy(l->function.code, l2->function.code, (size_t)code_size2);

            /* Free the (now empty) memory */
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
closure_literal (svalue_t *dest, int ix, unsigned short inhIndex)

/* Create a literal closure (lfun or variable closure), bound to the
 * current object. The resulting svalue is stored in *<dest>. The function
 * implements the instruction F_CLOSURE.
 *
 * The closure is defined by the index <ix>/<inhIndex>, which is to be
 * interpreted in the context of the current, possibly inherited, program:
 * values < CLOSURE_IDENTIFIER_OFFS are lfun indices, values above are
 * variable indices. For lfun closures, <num> indicates the number context
 * variables which are initialized to svalue-0.
 *
 * The function may raise an error on out of memory.
 */

{
    lambda_t *l;
    funflag_t flags;
    program_t *prog;
    int lfun_type = CLOSURE_LFUN;

    /* Allocate an initialise a new lambda structure */
    l = xalloc(sizeof *l);
    if (!l)
    {
    	put_number(dest, 0);
    	error("Out of memory (%lu bytes) for closure literal"
             , (unsigned long) sizeof(*l));
    	/* NOTREACHED */
    	return;
    }

    l->ref = 1;
    prog = current_object->prog;

    /* If the object's program will be replaced, store the closure
     * in lambda protector, otherwise mark the object as referenced by
     * a closure.
     */
    if ( !(prog->flags & P_REPLACE_ACTIVE) )
    {
        current_object->flags |= O_LAMBDA_REFERENCED;
    }
    else if (!lambda_ref_replace_program( l
                                        , ix >= CLOSURE_IDENTIFIER_OFFS
                                          ? CLOSURE_IDENTIFIER
                                          : CLOSURE_ALIEN_LFUN
                                        , 0, NULL, NULL)
       )
    {
        current_object->flags |= O_LAMBDA_REFERENCED;
    }
    else
    {
        lfun_type = CLOSURE_ALIEN_LFUN;
    }

    /* Set ix to the proper index, and dest->x.closure_type to the
     * proper type.
     */
    if (ix >= CLOSURE_IDENTIFIER_OFFS)
    {
        ix += - CLOSURE_IDENTIFIER_OFFS
              + (current_variables - current_object->variables);
              /* the added difference takes into account that the
               * index is specified relative to the program which might
               * have been inherited.
               */
        dest->x.closure_type = CLOSURE_IDENTIFIER;
        l->function.lfun.index = (unsigned short)ix;
    }
    else /* lfun closure */
    {
        ix += function_index_offset;
        flags = prog->functions[ix];
        if (flags & NAME_CROSS_DEFINED)
        {
            ix += CROSSDEF_NAME_OFFSET(flags);
        }
        dest->x.closure_type = lfun_type;

        if (lfun_type == CLOSURE_LFUN)
        {
            l->function.lfun.index = (unsigned short)ix;
            l->function.lfun.inhIndex = inhIndex;
        }
        else
        {
            l->function.alien.ob = ref_object(current_object, "closure");
            l->function.alien.index = (unsigned short)ix;
        }
    }

    /* Fill in the rest of the lambda and of the result svalue */
    l->ob = ref_object(current_object, "closure");

    dest->type = T_CLOSURE;
    dest->u.lambda = l;
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
        lambda_error("Out of memory (%lu bytes) for %ld new values\n"
                    , new_max, new_max * sizeof(*new_values));

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
        lambda_error("Out of memory (%ld bytes) for new code\n", new_max);

    current.code_left += current.code_max;
    current.code_max = new_max;
    current.code = new_code;
    current.codep = current.code + curr_offset;
} /* realloc_code() */

/*-------------------------------------------------------------------------*/
static void
lambda_error(char *error_str, ...)

/* Raise an error(error_str, ...) with 0 or 1 extra argument from within
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
    error(error_str, va_arg(va, char *)); /* One arg or nothing :-) */
    /* TODO: a verror() would be handy here */
    va_end(va);
} /* lambda_error() */

/*-------------------------------------------------------------------------*/
static void
lambda_cerror (char *s)

/* Callback for store_case_labels: raise an error(s) from within the
 * lambda compiler.
 *
 * The function takes care that all memory is deallocated.
 */

{
    lambda_error("%s\n", s);
} /* lambda_cerror() */

/*-------------------------------------------------------------------------*/
static void
lambda_cerrorl ( char *s1, char *s2 UNUSED
               , int line1 UNUSED, int line2 UNUSED)

/* Callback for store_case_labels(): Raise an error(s1) from within the lambda
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
    move_memory( current.codep - blocklen
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
        if (current_file)
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
make_symbol (char *name)

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
            lambda_error("Out of memory (%ld bytes) for symbol table\n"
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
compile_value (svalue_t *value, int opt_flags)

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
 */

{
    if (!--current.levels_left)
        lambda_error("Too deep recursion inside lambda()\n");

    switch(value->type)
    {
    case T_POINTER:                            /* ----- T_POINTER ----- */
      {
        vector_t *block;  /* The block of svalues to compile */
        svalue_t *argp;   /* Pointer to the current svalue */
        ph_int type;      /* Various types */

        block = value->u.vec;
        argp = block->item;
        /* The first value must be a closure */
        if (block == &null_vector || argp->type != T_CLOSURE)
        {
            lambda_error("Missing function\n");
        }

        if ( (type = argp->x.closure_type) < (ph_int)CLOSURE_SIMUL_EFUN)
        {
            /* Most common case: closure is an efun or an operator */

            if (type < (ph_int)CLOSURE_EFUN)
            {
                /* Closure is an operator */

                mp_int block_size;  /* Number of entries */

                block_size = (mp_int)VEC_SIZE(block);
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
                     * Analog for F_OR, here the branches are on _NON_ZERO.
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
                    int void_given;

                    code = type - CLOSURE_OPERATOR;
                    is_and = code == (F_LAND);

                    /* If the caller doesn't need a return value,
                     * compile the operator as branches (much faster).
                     */
                    if (opt_flags & VOID_ACCEPTED)
                    {
                        code = is_and ? F_BRANCH_WHEN_ZERO
                                      : F_BRANCH_WHEN_NON_ZERO;
                        opt_flags |= VOID_GIVEN;
                    }

                    /* Generate the code for the arguments but the last one.
                     * After every compiled argument, insert <code> and
                     * an empty byte and store the position of the inserted
                     * byte in the branchp table.
                     */
                    i = block_size - 1;
                    branchp = alloca(i * sizeof *branchp);
                    while (--i > 0) {
                        compile_value(++argp, REF_REJECTED);
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
                                         , opt_flags & (VOID_ACCEPTED|REF_REJECTED)
                                     );
                    else
                        void_given = compile_value(++argp
                                         , opt_flags & (VOID_ACCEPTED|REF_REJECTED)
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
                    int opt_used;         /* Current compile() result */
                    int all_void;         /* !0 if  cond-parts returns a value */
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
                     && (opt_flags & (VOID_ACCEPTED|ZERO_ACCEPTED))
                     && ( opt_flags & VOID_ACCEPTED
                          ? argp[block_size-1].type != T_POINTER /* no side effect */
                          :     argp[block_size-1].type == T_NUMBER
                            && !argp[block_size-1].u.number
                         ) )
                    {
                    	/* Ignore the default-part by hiding it */
                        block_size--;
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
                                opt_flags & REF_REJECTED :
                                opt_flags &
                                  (VOID_ACCEPTED|ZERO_ACCEPTED|REF_REJECTED)
                          );
                        all_void &= opt_used;
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

                        opt_flags |= VOID_GIVEN;
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
                            short old_offset;

                            GET_SHORT(old_offset, current.code+start+1);
                            PUT_SHORT(current.code+start+1, old_offset+offset);
                        }
                        else
                        {
                            PUT_INT8(current.code+start+1
                                     , GET_INT8(current.code+start+1) + offset);
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
                              (VOID_ACCEPTED|ZERO_ACCEPTED|REF_REJECTED) :
                              REF_REJECTED
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
                            opt_flags |= VOID_GIVEN;
                        }
                        else if (opt_flags & VOID_ACCEPTED)
                        {
                            /* We have a result, but the caller doesn't want
                             * it: add the code sequence
                             *
                             *   nvd: POP
                             *   vd:
                             */
                            opt_flags |= VOID_GIVEN;
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
                                move_memory(
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

                /* ({#', <expr1>, <expr2>, ..., <exprn> })
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
                    int void_given;

                    /* Compile the first n-1 expressions */
                    for (i = block_size - 1; --i > 0; )
                    {
                        void_given = compile_value(++argp, VOID_WANTED);

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
                    opt_flags = compile_value(i ? &const0 : ++argp, opt_flags);
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
                        compile_value(argp+1, REF_REJECTED);
                        compile_lvalue(argp, USE_INDEX_LVALUE);
                        if (!i)
                        {
                            /* Last assignment: we might need to keep this value */
                            if (opt_flags & VOID_ACCEPTED)
                            {
                                opt_flags = VOID_GIVEN;
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

                    if (argp[2].type == T_NUMBER && argp[2].u.number == 1)
                    {
                        compile_lvalue(argp+1, USE_INDEX_LVALUE);
                        if (opt_flags & VOID_ACCEPTED)
                        {
                            opt_flags = VOID_GIVEN;
                            STORE_CODE(current.codep, F_INC);
                        }
                        else
                        {
                            STORE_CODE(current.codep, F_PRE_INC);
                        }
                    }
                    else
                    {
                        compile_value(argp+2, REF_REJECTED);
                        compile_lvalue(argp+1, USE_INDEX_LVALUE);
                        if (opt_flags & VOID_ACCEPTED)
                        {
                            opt_flags = VOID_GIVEN;
                            STORE_CODE(current.codep, F_VOID_ADD_EQ);
                        }
                        else
                            STORE_CODE(current.codep, F_ADD_EQ);
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

                    if (argp[2].type == T_NUMBER && argp[2].u.number == 1)
                    {
                        compile_lvalue(argp+1, USE_INDEX_LVALUE);
                        if (opt_flags & VOID_ACCEPTED)
                        {
                            opt_flags = VOID_GIVEN;
                            STORE_CODE(current.codep, F_DEC);
                        }
                        else
                        {
                            STORE_CODE(current.codep, F_PRE_DEC);
                        }
                    }
                    else
                    {
                        compile_value(argp+2, REF_REJECTED);
                        compile_lvalue(argp+1, USE_INDEX_LVALUE);
                        STORE_CODE(current.codep, F_SUB_EQ);
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
                    compile_value(argp+2, REF_REJECTED);
                    compile_lvalue(argp+1, USE_INDEX_LVALUE);
                    STORE_CODE(current.codep, (bytecode_t)(type - CLOSURE_OPERATOR));
                    break;

                /* ({#'op=, <lvalue>, <expr> })
                 * with op: &&, ||
                 */
                case F_LAND_EQ:
                case F_LOR_EQ:
                  {
                    /* This is compiled as:
                     *
                     *      <lvalue>
                     *      LDUP
                     *      <op> l
                     *      <expr>
                     *   l: SWAP_VALUES
                     *      ASSIGN
                     *
                     * respectively for long branches:
                     *
                     *      <lvalue>
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

                    compile_lvalue(argp+1, USE_INDEX_LVALUE);
                    current.code_left++; /* Don't need that extra byte */

                    if (current.code_left < 3)
                        realloc_code();

                    current.code_left--;
                    STORE_CODE(current.codep, (bytecode_t)F_LDUP);

                    branchp = current.code_max - current.code_left;
                    current.code_left -= 2;
                    STORE_CODE(current.codep, (bytecode_t)code);
                    STORE_CODE(current.codep, (bytecode_t)0);

                    compile_value(argp+2, REF_REJECTED);

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

                    compile_lvalue(argp+1, USE_INDEX_LVALUE);

                    if (opt_flags & VOID_ACCEPTED)
                    {
                        opt_flags = VOID_GIVEN;
                        if (type-CLOSURE_OPERATOR == F_POST_INC)
                            STORE_CODE(current.codep, F_INC);
                        else
                            STORE_CODE(current.codep, F_DEC);
                    }
                    else
                        STORE_CODE(current.codep, (bytecode_t)type);

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
                    int    void_given;
                    mp_int offset;      /* Position of first <body> */

                    i = block_size - 3;
                    if (i < 0)
                        lambda_error("Missing argument(s) to #'do\n");

                    offset = current.code_left - current.code_max;

                    /* Compile all the bodys */
                    if (i) do
                    {
                        void_given = compile_value(++argp, VOID_WANTED);
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
                    opt_flags = compile_value(++argp, opt_flags);
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
                    int    void_given;
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
                        void_given = compile_value(++argp, VOID_WANTED);
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
                    argp = block->item;
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
                    opt_flags = compile_value(++argp, opt_flags);
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
                    int    void_given;
                    mp_int start;
                    mp_int offset;
                    int    vars_given;
                    int    body_count;

                    body_count = block_size - 3;
                    if (body_count < 0)
                        lambda_error("Missing argument(s) to #'foreach\n");

                    if (!body_count)
                    {
                        /* Just create the code for the expression
                         * and pop the value
                         */
                        compile_value(argp+2, 0);
                        if (current.code_left < 2)
                            realloc_code();
                        current.code_left--;
                        STORE_CODE(current.codep, F_POP_VALUE);

                        /* If a result is required, compile a 0 */
                        if (opt_flags & VOID_ACCEPTED)
                            opt_flags = VOID_GIVEN;
                        else
                        {
                            current.code_left--;
                            STORE_CODE(current.codep, F_CONST0);
                        }

                        break;
                    }

                    /* Create the code to push the variable lvalues
                     */
                    if ((++argp)->type != T_POINTER)
                    {
                        vars_given = 1;
                        if (!is_lvalue(argp, 0))
                            lambda_error("Missing variable lvalue to #'foreach\n");
                        compile_lvalue(argp, 0);
                        current.code_left++; /* Don't need that assign byte */
                    }
                    else
                    {
                        svalue_t * svp;

                        svp = argp->u.vec->item;
                        vars_given = i = (int)VEC_SIZE(argp->u.vec);

                        if (!vars_given)
                            lambda_error("Missing variable lvalue to #'foreach\n");
                        if (vars_given > 0xFE)
                            lambda_error("Too many lvalues to #'foreach: %d\n", vars_given);
                        for ( ; i > 0; i--, svp++)
                        {
                            if (!is_lvalue(svp, 0))
                                lambda_error("Missing variable lvalue to #'foreach\n");
                            compile_lvalue(svp, 0);
                            current.code_left++; /* Don't need that assign byte */
                        }
                    }

                    /* Create the code for the expression */
                    compile_value(++argp, 0);

                    /* Create the FOREACH instruction and remember the position
                     */
                    if (current.code_left < 4)
                        realloc_code();
                    current.code_left -= 4;
                    STORE_CODE(current.codep, F_FOREACH);
                    STORE_UINT8(current.codep, vars_given+1);
                    STORE_SHORT(current.codep, 0);
                    start = current.code_max - current.code_left;

                    /* Compile all bodies.
                     */
                    for (i = body_count; i > 0; i--)
                    {
                        void_given = compile_value(++argp, VOID_WANTED);
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
                    if (current.code_left < 5)
                        realloc_code();
                    current.code_left -= 4;
                    STORE_CODE(current.codep, F_FOREACH_NEXT);
                    STORE_SHORT(current.codep, offset+3);
                    STORE_CODE(current.codep, F_FOREACH_END);

                    /* If a result is required, compile a 0 */
                    if (opt_flags & VOID_ACCEPTED)
                        opt_flags = VOID_GIVEN;
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
                 */
                case F_CATCH:
                  {
                    /* This is compiled as:
                     *
                     *      CATCH l / CATCH_NO_LOG l
                     *      <body>
                     *   l: END_CATCH
                     */

                    mp_int start, offset;
                    int flags, i;
                    int void_given;

                    if (block_size != 2 && block_size != 4)
                        lambda_error("Wrong number of arguments to #'catch\n");

                    if (current.code_left < 3)
                        realloc_code();
                    current.code_left -= 3;

                    STORE_CODE(current.codep, F_CATCH);

                    flags = 0;
                    for (i = 3; i <= block_size; i++)
                    {
                        if (argp[i-1].type == T_SYMBOL
                         && !strcmp(argp[i-1].u.string, "nolog"))
                            flags |= CATCH_FLAG_NOLOG;
                        else if (argp[i-1].type == T_SYMBOL
                         && !strcmp(argp[i-1].u.string, "publish"))
                            flags |= CATCH_FLAG_PUBLISH;
                        else
                            lambda_error("Expected 'nolog or 'publish as "
                                         "catch-modifier.\n");
                    }

                    STORE_UINT8(current.codep, flags);

                    STORE_UINT8(current.codep, 0);
                    start = current.code_max - current.code_left;

                    void_given = compile_value(++argp, 0);
                    if (current.code_left < 2)
                        realloc_code();

                    current.code_left -= 2;
                    STORE_CODE(current.codep, F_ESCAPE);
                    STORE_CODE(current.codep, F_END_CATCH - 0x100);

                    offset = current.code_max - current.code_left - start;
                    if (offset > 0xff)
                    {
                        UNIMPLEMENTED
                    }
                    PUT_UINT8(current.code+start-1, (bytecode_t)offset);
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
                     * omitted and the fact is stored in opt_flags:NEGATE_GIVEN.
                     */

                    if (block_size != 2)
                        lambda_error("Wrong number of arguments to #'!\n");

                    opt_flags |= compile_value(++argp, opt_flags & ~ZERO_ACCEPTED);
                    if (opt_flags & (NEGATE_ACCEPTED|VOID_GIVEN) )
                    {
                        opt_flags ^= NEGATE_GIVEN;
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

                /* ({#'&, <expr1>, ..., <exprn> })
                 * ({#'&, <lvalue> })
                 */
                case F_AND:
                  {
                    int i;

                    if ( (i = block_size - 2) > 0)
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
                    	 *
                    	 * (easy, isn't it?)
                    	 */
                        if (opt_flags & REF_REJECTED)
                            lambda_error("Reference value in bad position\n");
                        compile_lvalue(++argp, PROTECT_LVALUE|USE_INDEX_LVALUE);
                        current.code_left++;
                    }
                    else
                    {
                        lambda_error("Missing argument(s) to #'&\n");
                    }
                    break;
                  }

                /* ({#'sscanf, <data>, <fmt>, <lvalue1>, ..., <lvalueN> })
                 */
                case F_SSCANF:
                  {
                    /* This is compiled as:
                     *
                     *   <data>
                     *   <fmt>
                     *   <lvalue1>
                     *   ...
                     *   <lvalueN>
                     *   SSCANF N+2
                     */

                    int lvalues;

                    if ( (lvalues = block_size - 3) < 0)
                        lambda_error("Missing argument(s) to #'sscanf\n");

                    if (lvalues > 0xff - 2)
                        lambda_error("Too many arguments to #'sscanf\n");

                    compile_value(++argp, 0);
                    compile_value(++argp, 0);

                    while (--lvalues >= 0)
                    {
                        compile_lvalue(++argp, PROTECT_LVALUE|USE_INDEX_LVALUE);
                        current.code_left++;
                    }

                    if (current.code_left < 2)
                        realloc_code();
                    current.code_left -= 2;

                    STORE_CODE(current.codep, F_SSCANF);
                    STORE_CODE(current.codep, (bytecode_t)(block_size - 1));
                    break;
                  }

#ifdef SUPPLY_PARSE_COMMAND
                /* ({#'parse_command, <data>, <fmt>, <data>, <lvalue1>, ..., <lvalueN> })
                 */
                case F_PARSE_COMMAND:
                  {
                    /* This is compiled as:
                     *
                     *   <data>
                     *   <fmt>
                     *   <data>
                     *   <lvalue1>
                     *   ...
                     *   <lvalueN>
                     *   SSCANF N+2
                     */

                    int lvalues;

                    if ( (lvalues = block_size - 3) < 0)
                        lambda_error("Missing argument(s) to #'sscanf\n");

                    if (lvalues > 0xff - 2)
                        lambda_error("Too many arguments to #'sscanf\n");

                    compile_value(++argp, 0);
                    compile_value(++argp, 0);
                    compile_value(++argp, 0);

                    while (--lvalues >= 0)
                    {
                        compile_lvalue(++argp, PROTECT_LVALUE|USE_INDEX_LVALUE);
                        current.code_left++;
                    }

                    if (current.code_left < 2)
                        realloc_code();
                    current.code_left -= 2;

                    STORE_CODE(current.codep, F_SSCANF);
                    STORE_CODE(current.codep, (bytecode_t)(block_size - 1));
                    break;
                  }
#endif

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
                        compile_value(++argp, REF_REJECTED);
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

                        if ( (++argp)->type != T_POINTER )
                            lambda_error("Bad argument to #'([\n");

                        element = argp->u.vec->item;

                        /* The first array determines the width */
                        j = (mp_int)VEC_SIZE(argp->u.vec);
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
                            compile_value(element++, REF_REJECTED);
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
                        opt_flags = VOID_GIVEN;
                    }
                    else
                    {
                        opt_flags =
                          compile_value(++argp, ZERO_ACCEPTED|REF_REJECTED);
                    }

                    if (current.code_left < 1)
                        realloc_code();
                    current.code_left--;
                    if (opt_flags & VOID_GIVEN)
                    {
                        STORE_CODE(current.codep, F_RETURN0);
                        opt_flags ^= VOID_GIVEN;
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
                  {
                    /* This is compiled as:
                     *     <value>
                     *     <index>
                     *     CONST1
                     *     NR_RANGE/RR_RANGE
                     */

                    Bool is_rindex;

                    is_rindex = ((type - CLOSURE_OPERATOR) == F_RX_RANGE);

                    if (block_size != 3)
                        lambda_error("Bad number of arguments to %s\n"
                                    , is_rindex ? "#'[<.." : "#'[..");

                    compile_value(++argp, REF_REJECTED);

                    /* A numeric index can be compiled directly */
                    if ((++argp)->type == T_NUMBER)
                        compile_value(argp, 0);
                    else
                    {
                        compile_value(argp, REF_REJECTED);
                    }

                    if (current.code_left < 2)
                        realloc_code();
                    current.code_left -= 2;
                    STORE_CODE(current.codep, F_CONST1);
                    STORE_CODE(current.codep, is_rindex ? F_RR_RANGE : F_NR_RANGE);
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
                       * We don't need an explicite list of case_states
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
                        if (current_file)
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

                    compile_value(++argp, REF_REJECTED);

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
                        int opt_used;

                        /* Compile the case labels */

                        ++argp;
                        if (argp->type == T_POINTER) {
                            labels = argp->u.vec->item;
                            j = (mp_int)VEC_SIZE(argp->u.vec);
                        } else {
                            labels = argp;
                            j = 1;
                        }

                        for (; j--; labels++)
                        {
#if defined(__linux__) && defined(__GNUC__)
                              /* Workaround for probable optimizer bug. */
                            ph_int label_type;
#define UNION_BUG
#endif
                            l = new_case_entry();
                            l->addr =
                              current.code_max - current.code_left - switch_pc;
                            l->line = 1;

                            /* Create the case_list_entry for this case label */
#ifndef UNION_BUG
                            if (j && labels[1].type == T_CLOSURE
#else
                            if (j && (label_type = labels[1].type) == T_CLOSURE
#endif
                              && labels[1].x.closure_type ==
                                  F_RANGE +CLOSURE_EFUN )
                            {
                                /* It's a ({#'.., <low>, <high>}) range */
                            	
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
                            else if (labels->type == T_STRING)
                            {
                            	/* String label: we have to make the string shared
                            	 * and store it in the value table (to keep the
                            	 * reference).
                            	 */

                                svalue_t stmp;

                                if (some_numeric)
                                    lambda_error(
                                      "mixed case label lists not supported\n"
                                    );

                                if (--current.values_left < 0)
                                    realloc_values();
                                no_string = MY_FALSE;
                                put_string(&stmp
                                          , make_shared_string(labels->u.string));
                                if (!stmp.u.string)
                                    lambda_error("Out of memory (%lu bytes) "
                                                 "for string\n"
                                                , (unsigned long) strlen(labels->u.string));
                                *--current.valuep = stmp;

                                l->key = (p_int)stmp.u.string;

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
#ifndef UNION_BUG
                            }
                            else if (labels->type == T_CLOSURE
#else
                            }
                            else if ((label_type = labels->type) == T_CLOSURE
#endif
                               && labels->x.closure_type ==
                                  F_CSTRING0 +CLOSURE_OPERATOR)
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
                            REF_REJECTED | VOID_ACCEPTED :
                            REF_REJECTED
                        );

                        /* Check and compile the delimiter #', or #'break */

                        if ((++argp)->type != T_CLOSURE
                         || (   argp->x.closure_type !=
                                  F_BREAK+CLOSURE_OPERATOR
                             && (!i || argp->x.closure_type !=
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
                                      , (bytecode_t) argp->x.closure_type);
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
            else /* it's an EFUN closure */
            {
            	/* This is compiled as:
            	 *
            	 *   <arg1>
            	 *   <arg2>
            	 *   ...
            	 *   <argN>
            	 *   <efun>
            	 */
            	
                mp_int i;
                bytecode_p p;
                int f;
                mp_int num_arg;
                mp_int min;
                mp_int max;
                mp_int def;

                /* Compile the arguments */
                num_arg = (mp_int)VEC_SIZE(block) - 1;
                for (i = num_arg; --i >= 0; )
                {
                    compile_value(++argp, 0);
                }

                /* Get the instruction and check if it received the
                 * correct number of arguments.
                 */
                argp = block->item;
                if (current.code_left < 5)
                    realloc_code();

                f = type - CLOSURE_EFUN;
                min = instrs[f].min_arg;
                max = instrs[f].max_arg;
                p = current.codep;
                if (num_arg < min)
                {
                    /* Not enough arguments... probably */

                    int g;

                    if (num_arg == min-1 && 0 != (def = instrs[f].Default))
                    {
                    	/* We have a default argument */
                        STORE_CODE(p, (bytecode_t)(def));
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
                if (f > 0xff)
                {
                    STORE_CODE(p, (bytecode_t)(f >> F_ESCAPE_BITS));
                    /* This relies on the known encoding of the prefix
                     * codes.
                     */
                    current.code_left--;
                }
                STORE_CODE(p, (bytecode_t)f);
                current.code_left--;

                /* Store the number of arguments if required */
                if (min != max)
                {
                    STORE_UINT8(p, (bytecode_t)num_arg);
                    if (num_arg > 0xff)
                        lambda_error("Too many arguments to efun closure\n");
                    current.code_left--;
                }

                /* Note the type of the result, and add a CONST0 if
                 * the caller expects one from a void efun.
                 */
                if ( instrs[f].ret_type == TYPE_VOID )
                {
                    if (opt_flags & (ZERO_ACCEPTED|VOID_ACCEPTED))
                    {
                        opt_flags = VOID_GIVEN;
                    }
                    else
                    {
                        STORE_CODE(p, F_CONST0);
                        current.code_left--;
                    }
                }
                current.codep = p;
                break;
            }
        } /* if (efun or operator closure) */
        else switch (type) /* type >= CLOSURE_SIMUL_EFUN */
        {
        default: /* SIMUL_EFUN closure */
          {
            /* This is compiled as:
             *    sefun <= 0xff             sefun > 0xff
             *
             *                              <sefun_object_name>
             *                              <sefun_name>
             *    <arg1>                    <arg1>
             *    ...                       ...
             *    <argN>                    <argN>
             *    SIMUL_EFUN <sefun> N+2    CALL_OTHER N+2
             */

            int simul_efun;
            mp_int num_arg;
            int i;

            simul_efun = type - CLOSURE_SIMUL_EFUN;

            if (simul_efun > 0xff)
            {
            	/* We have to call the sefun by name */
                static svalue_t string_sv = { T_STRING };

                string_sv.x.string_type = STRING_SHARED;
                string_sv.u.string = query_simul_efun_file_name();
                compile_value(&string_sv, 0);
                string_sv.u.string = simul_efunp[simul_efun].name;
                compile_value(&string_sv, 0);
            }

            /* Compile the arguments */

            num_arg = (mp_int)VEC_SIZE(block) - 1;
            for (i = num_arg; --i >= 0; )
            {
                compile_value(++argp, 0);
            }

            /* and the simul-efun instruction */

            if (current.code_left < 3)
                realloc_code();

            if (simul_efun > 0xff)
            {
            	/* We need the call_other */
                current.code_left -= 2;
                STORE_CODE(current.codep, F_CALL_OTHER);
                STORE_CODE(current.codep, (bytecode_t)(num_arg + 2));
                if (num_arg + 2 > 0xff)
                    lambda_error("Argument number overflow\n");
            }
            else
            {
            	/* We can call by index */
            	
                function_t *funp;
                Bool bVarargs = MY_FALSE;

                funp = &simul_efunp[simul_efun];
                if (funp->num_arg == SIMUL_EFUN_VARARGS
                 || (funp->flags & TYPE_MOD_XVARARGS)
                   )
                    bVarargs = MY_TRUE;

                if (!bVarargs)
                {
                    /* The function takes fixed number of args:
                     * push 0s onto the stack for missing args
                     */

                    if (num_arg > funp->num_arg)
                        lambda_error(
                          "Too many arguments to simul_efun %s\n", funp->name
                        );

                    i = funp->num_arg - num_arg;
                    if (i > 1 && current.code_left < i + 2)
                        realloc_code();
                    current.code_left -= i;
                    while ( --i >= 0 ) {
                        STORE_CODE(current.codep, F_CONST0);
                    }
                }

                STORE_CODE(current.codep, F_SIMUL_EFUN);
                STORE_UINT8(current.codep, (bytecode_t)simul_efun);

                /* For a varargs sefun, add the number of arguments */
                if (bVarargs)
                {
                    STORE_UINT8(current.codep, (bytecode_t)num_arg);
                    current.code_left -= 3;
                }
                else
                    current.code_left -= 2;
            }
            break;
          } /* CLOSURE_SIMUL_EFUN */

        case CLOSURE_UNBOUND_LAMBDA:
        case CLOSURE_BOUND_LAMBDA:
        case CLOSURE_LAMBDA:
        case CLOSURE_PRELIMINARY:
            lambda_error("Unimplemented closure type for lambda()\n");

        case CLOSURE_ALIEN_LFUN:
          {
            /* This is compiled as
             *
             *   <alien_lfun_closure>
             *   <arg1>
             *   ...
             *   <argN>
             *   FUNCALL N+1
             */

            mp_int i;
            lambda_t *l;
            mp_int block_size;

            block_size = (mp_int)VEC_SIZE(block);

            l = argp->u.lambda;
            insert_value_push(argp);

            for (i = block_size; --i; )
            {
                compile_value(++argp, 0);
            }

            if (current.code_left < 2)
                realloc_code();
            current.code_left -= 2;
            STORE_CODE(current.codep, F_FUNCALL);
            STORE_UINT8(current.codep, (bytecode_t)block_size);
            break;
          } /* CLOSURE_ALIEN_LFUN */

        case CLOSURE_LFUN:
          {
            /* This is compiled as
             *   alien-lfun:             local lfun:
             *
             *   <alien_lfun_closure>
             *   <arg1>                   <arg1>
             *   ...                      ...
             *   <argN>                   <argN>
             *   FUNCALL N+1              CALL_BY_ADDRESS <lfun-index> N
             */

            mp_int i;
            lambda_t *l;
            mp_int block_size;

            block_size = (mp_int)VEC_SIZE(block);
            l = argp->u.lambda;
            if (l->ob != current.lambda_origin)
            {
            	/* Compile it like an alien lfun */
            	
                insert_value_push(argp);
                for (i = block_size; --i; )
                {
                    compile_value(++argp, 0);
                }
                if (current.code_left < 2)
                    realloc_code();
                current.code_left -= 2;
                STORE_CODE(current.codep, F_FUNCALL);
                STORE_CODE(current.codep, (bytecode_t)block_size);
            }
            else
            {
            	/* Intra-object call: we can call by address */
            	
                for (i = block_size; --i; )
                {
                    compile_value(++argp, 0);
                }

                if (current.code_left < 6)
                    realloc_code();
                if (l->function.lfun.inhIndex)
                {
                    STORE_CODE(current.codep, F_CALL_EXPLICIT_INHERITED);
                    STORE_SHORT(current.codep, l->function.lfun.inhIndex-1);
                    STORE_SHORT(current.codep, l->function.lfun.index);
                    STORE_UINT8(current.codep, (bytecode_t)(block_size - 1));
                    current.code_left -= 6;
                }
                else
                {
                    STORE_CODE(current.codep, F_CALL_FUNCTION_BY_ADDRESS);
                    STORE_SHORT(current.codep, l->function.lfun.index);
                    STORE_UINT8(current.codep, (bytecode_t)(block_size - 1));
                    current.code_left -= 4;
                }
                if (block_size > 0x100)
                    lambda_error("Too many arguments to lfun closure\n");
            }
            break;
          } /* CLOSURE_LFUN */

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

            l = argp->u.lambda;
            if (VEC_SIZE(block) != 1)
                lambda_error("Argument to variable\n");

            if (l->ob != current.lambda_origin)
            {
            	/* We need the FUNCALL */
            	
                insert_value_push(argp);
                if (current.code_left < 2)
                    realloc_code();
                current.code_left -= 2;
                STORE_CODE(current.codep, F_FUNCALL);
                STORE_UINT8(current.codep, 1);
            }
            else
            {
            	/* We can use the IDENTIFIER */
            	
                if (current.code_left < 2)
                    realloc_code();
                current.code_left -= 2;
                if ((short)l->function.lfun.index < 0)
                    lambda_error("Variable not inherited\n");
                STORE_CODE(current.codep, F_IDENTIFIER);
                STORE_CODE(current.codep, (bytecode_t)l->function.lfun.index);
            }
            break;
          } /* CLOSURE_IDENTIFIER */
        } /* switch(type) for type >= CLOSURE_SIMUL_EFUN */
        break;
      } /* end of case T_POINTER (block compiling code) */

    case T_QUOTED_ARRAY:                  /* ----- T_QUOTED_ARRAY ----- */
        /* This compiles into the value itself minus one quote.
         */

        insert_value_push(value);
        if (!--current.valuep->x.quotes)
            current.valuep->type = T_POINTER;
        break;

    case T_SYMBOL:                              /* ----- T_SYMBOL ----- */
        /* Symbols with more than one quote compile into the value itself
         * minus one quote.
         * Symbols with just one quote compile into 'LOCAL <index>'. This may
         * create the local variable in the first place.
         */

        if (value->x.quotes > 1)
        {
            insert_value_push(value);
            --current.valuep->x.quotes;
        }
        else
        {
            /* Make/find the local variable to the symbol name and
             * compile the LOCAL instruction.
             */
            symbol_t *sym;

            sym = make_symbol(value->u.string);
            if (sym->index < 0)
                lambda_error("Symbol '%s' not bound\n", sym->name);
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

        i = value->u.number;
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
                    opt_flags = VOID_GIVEN;
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
        insert_value_push(value);
        break;
    }

    /* Finish up */
    current.levels_left++;
    return opt_flags;

#ifdef UNION_BUG
#undef UNION_BUG
#endif
} /* compile_value() */

/*-------------------------------------------------------------------------*/
static Bool
is_lvalue (svalue_t *argp, int index_lvalue)

/* Test if the value <argp> can be compiled into a lvalue.
 * If <index_lvalue> is not zero, arrays compiling into an indexing
 * instruction are accepted, too.
 */

{
    switch(argp->type)
    {
    case T_SYMBOL:
        return argp->x.quotes == 1;

    case T_POINTER:
        if (index_lvalue)
        {
            vector_t *block;

            block = argp->u.vec;
            if (VEC_SIZE(block) != 3)
                break;

            argp = block->item;
            if (argp->type != T_CLOSURE)
            {
                break;
            }

            switch (argp->x.closure_type)
            {
              case F_INDEX +CLOSURE_EFUN:
              case F_RINDEX+CLOSURE_EFUN:
              case CLOSURE_IDENTIFIER:
                return MY_TRUE;
            }
        }
        break;

    case T_CLOSURE:
        if (argp->x.closure_type == CLOSURE_IDENTIFIER)
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
 * (1 Byte) as well. This extra space will already be counted out of the
 * remaining current.code_left, even though the code hasn't been stored yet!
 */

{
    switch(argp->type) {

    /* 'a: Symbol of a local variable.
     */
    case T_SYMBOL:
      /* This compiles to:
       *
       *   PUSH_LOCAL_VARIABLE_LVALUE <index>
       */

      {
        symbol_t *sym;

        if (argp->x.quotes > 1)
            break;

        /* Find (or create) the variable for this symbol */
        sym = make_symbol(argp->u.string);
        if (sym->index < 0)
            sym->index = current.num_locals++;

        if (current.code_left < 3)
            realloc_code();
        current.code_left -= 3;
        STORE_CODE(current.codep, F_PUSH_LOCAL_VARIABLE_LVALUE);
        STORE_UINT8(current.codep, (bytecode_t)sym->index);
        return;
      }

    /* ({ indexing operation })
     */
    case T_POINTER:
      {
        vector_t *block;

        block = argp->u.vec;
        if (block != &null_vector && (argp = block->item)->type == T_CLOSURE)
        {
            switch (argp->x.closure_type)
            {

            /* ({ #'[, map|array, index [, index] })
             * ({ #'[<, map|array, index })
             */
            case F_INDEX +CLOSURE_EFUN:
            case F_RINDEX+CLOSURE_EFUN:
                if (VEC_SIZE(block) == 3)
                {
                    /* Indexing of an array or normal mapping.
                     */
                    if (is_lvalue(argp+1, flags & USE_INDEX_LVALUE))
                    {
                        compile_value(argp+2, 0);
                        compile_lvalue(argp+1, flags & PROTECT_LVALUE);
                        if (current.code_left < 2)
                            realloc_code();
                        if (flags & PROTECT_LVALUE)
                        {
                            current.code_left -= 2;
                            STORE_CODE(current.codep, F_ESCAPE);
                            STORE_CODE(current.codep, (bytecode_t)(
                              argp->x.closure_type == F_RINDEX +CLOSURE_EFUN ?
                                F_PROTECTED_RINDEX_LVALUE - 0x100 :
                                F_PROTECTED_INDEX_LVALUE  - 0x100));
                        } else {
                            current.code_left--;
                            STORE_CODE(current.codep, (bytecode_t)(
                              argp->x.closure_type == F_RINDEX + CLOSURE_EFUN ?
                                F_RINDEX_LVALUE :
                                F_INDEX_LVALUE));
                        }
                        return;
                    }

                    compile_value(argp+1, 0);
                    compile_value(argp+2, 0);
                    if (current.code_left < 3)
                        realloc_code();
                    if (flags & PROTECT_LVALUE) {
                        current.code_left -= 3;
                        STORE_CODE(current.codep, F_ESCAPE);
                        STORE_CODE(current.codep, (bytecode_t)(
                          argp->x.closure_type == F_RINDEX +CLOSURE_EFUN ?
                            F_PUSH_PROTECTED_RINDEXED_LVALUE - 0x100 :
                            F_PUSH_PROTECTED_INDEXED_LVALUE  - 0x100));
                    } else {
                        current.code_left -= 2;
                        STORE_CODE(current.codep, (bytecode_t)(
                          argp->x.closure_type == F_RINDEX +CLOSURE_EFUN ?
                            F_PUSH_RINDEXED_LVALUE :
                            F_PUSH_INDEXED_LVALUE));
                    }
                    return;
                } /* if (VEC_SIZE(block) == 3) */

                if (VEC_SIZE(block) == 4
                 && argp->x.closure_type == F_INDEX +CLOSURE_EFUN)
                {
                    /* Indexing of a wide mapping.
                     */
                    compile_value(argp+1, 0);
                    compile_value(argp+2, 0);
                    compile_value(argp+3, 0);
                    if (current.code_left < 3)
                        realloc_code();

                    if (flags & PROTECT_LVALUE)
                    {
                        current.code_left -= 3;
                        STORE_CODE(current.codep, F_ESCAPE);
                        STORE_CODE(current.codep,
                          F_PUSH_PROTECTED_INDEXED_MAP_LVALUE  -0x100);
                    }
                    else
                    {
                        current.code_left -= 2;
                        STORE_CODE(current.codep,
                                     F_PUSH_INDEXED_MAP_LVALUE);
                    }
                    return;
                } /* if (VEC_SIZE(block) == 4...) */

                /* Otherwise: raise an error */
                break;

            /* ({#'[..], map/array, index, index })
             */
            case F_RANGE +CLOSURE_EFUN:
                if (VEC_SIZE(block) != 4)
                    break;
                compile_value(argp += 2, 0);
                compile_value(++argp, 0);
                compile_lvalue(argp - 2, flags & PROTECT_LVALUE);

                if (current.code_left < 2)
                    realloc_code();
                if (flags & PROTECT_LVALUE)
                {
                    current.code_left -= 2;
                    STORE_CODE(current.codep, F_ESCAPE);
                    STORE_CODE(current.codep,
                                 F_PROTECTED_RANGE_LVALUE - 0x100);
                }
                else
                {
                    current.code_left--;
                    STORE_CODE(current.codep, F_RANGE_LVALUE);
                }
                return;

            /* ({#'[..<], map/array, index, index })
             * ({#'[<..], map/array, index, index })
             * ({#'[<..<], map/array, index, index })
             */
            case F_NR_RANGE +CLOSURE_EFUN:
            case F_RN_RANGE +CLOSURE_EFUN:
            case F_RR_RANGE +CLOSURE_EFUN:
              {
              	int code;
              	
                if (VEC_SIZE(block) != 4)
                    break;

                code = F_ILLEGAL;
                switch(argp->x.closure_type)
                {
                case F_NR_RANGE+CLOSURE_EFUN:
                    code = (flags & PROTECT_LVALUE) ? F_PROTECTED_NR_RANGE_LVALUE
                                                    : F_NR_RANGE_LVALUE;
                    break;
                case F_RN_RANGE+CLOSURE_EFUN:
                    code = (flags & PROTECT_LVALUE) ? F_PROTECTED_RN_RANGE_LVALUE
                                                    : F_RN_RANGE_LVALUE;
                    break;
                case F_RR_RANGE+CLOSURE_EFUN:
                    code = (flags & PROTECT_LVALUE) ? F_PROTECTED_RR_RANGE_LVALUE
                                                    : F_RR_RANGE_LVALUE;
                    break;
                }

                compile_value(argp += 2, 0);
                compile_value(++argp, 0);
                compile_lvalue(argp - 2, flags & PROTECT_LVALUE);

                if (current.code_left < 2)
                    realloc_code();
                current.code_left -= 2;
                STORE_CODE(current.codep, F_ESCAPE);
                STORE_CODE(current.codep, (bytecode_t)code);
                return;
              }

            /* ({ #'[, mapping, index [,index] })
             */
            case F_MAP_INDEX +CLOSURE_EFUN:
                if (VEC_SIZE(block) != 4)
                    break;

                compile_value(++argp, 0);
                compile_value(++argp, 0);
                compile_value(++argp, 0);

                if (current.code_left < 3)
                    realloc_code();
                if (flags & PROTECT_LVALUE)
                {
                    current.code_left -= 3;
                    STORE_CODE(current.codep, F_ESCAPE);
                    STORE_CODE(current.codep,
                      F_PUSH_PROTECTED_INDEXED_MAP_LVALUE - 0x100);
                }
                else
                {
                    current.code_left -= 2;
                    STORE_CODE(current.codep, F_PUSH_INDEXED_MAP_LVALUE);
                }
                return;

            /* ({ #'global_var })
             */
            case CLOSURE_IDENTIFIER:
              {
                lambda_t *l;

                if (VEC_SIZE(block) != 1)
                    break;
                l = argp->u.lambda;
                if (l->ob != current.lambda_origin)
                    break;
                if (current.code_left < 3)
                    realloc_code();
                current.code_left -= 3;
                if ((short)l->function.lfun.index < 0)
                    lambda_error("Variable not inherited\n");
                STORE_CODE(current.codep, F_PUSH_IDENTIFIER_LVALUE);
                STORE_UINT8(current.codep, (bytecode_t)l->function.lfun.index);
                return;
              }
            } /* switch(closure_type) */
        }
        break;
      } /* case T_POINTER */

    /* precomputed closure: only identifiers in this object are allowed
     */
    case T_CLOSURE:
      {
        switch (argp->x.closure_type)
        {
        case CLOSURE_IDENTIFIER:
          {
            lambda_t *l;

            l = argp->u.lambda;
            if (l->ob != current.lambda_origin)
                break;
            if (current.code_left < 3)
                realloc_code();
            current.code_left -= 3;
            if ((short)l->function.lfun.index < 0)
                lambda_error("Variable not inherited\n");
            STORE_CODE(current.codep, F_PUSH_IDENTIFIER_LVALUE);
            STORE_CODE(current.codep, (bytecode_t)(l->function.lfun.index));
            return;
          }
        }
        break;
      }

    } /* switch(argp->type) */

    lambda_error("Illegal lvalue\n");
} /* compile_lvalue() */

/*-------------------------------------------------------------------------*/
lambda_t *
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
    for (i = 0; i < j; i++, argp++)
    {
        symbol_t *sym;

        if (argp->type != T_SYMBOL)
        {
            lambda_error("Illegal argument type to lambda()\n");
        }
        sym = make_symbol(argp->u.string);
        if (sym->index >= 0)
            lambda_error("Double symbol name in lambda arguments\n");
        sym->index = i;
    }

    current.num_locals = i;  /* Args count as locals, too */

    /* Continue initializing the work area */

    current.break_stack = current.max_break_stack = 0;

    current.code_max = CODE_BUFFER_START_SIZE;
    current.code_left = CODE_BUFFER_START_SIZE-3;
    current.levels_left = MAX_LAMBDA_LEVELS;
    if ( !(current.code = current.codep = xalloc((size_t)current.code_max)) )
        lambda_error("Out of memory (%ld bytes) for initial codebuffer\n"
                    , current.code_max);

    /* Store the lambda code header */
    STORE_UINT8(current.codep, 0);          /* dummy for num values */
    STORE_INT8(current.codep, (char)current.num_locals); /* num arguments */
    STORE_UINT8(current.codep, 0);          /* dummy for num variables */

    current.value_max = current.values_left = VALUE_START_MAX;
    if ( !(current.values =
        xalloc(current.value_max * sizeof current.values[0])) )
    {
        lambda_error("Out of memory (%lu bytes) for initial value buffer\n"
                    , current.value_max * sizeof current.values[0]);
    }
    current.valuep = current.values + current.value_max;

    current.lambda_origin = origin;

    /* Now compile */

    void_given = compile_value(block, ZERO_ACCEPTED|REF_REJECTED);

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

    if (num_values == 0xff)
    {
        /* Special case: we have exactly 255 values, that means
         * we are going to use the indirect way of storing the number
         * of values. At the same time, the extra entry to store
         * the number of values has not been reserved yet.
         * Do it now.
         */
        num_values++;
        values_size += (long)sizeof(svalue_t);
        current.values_left--;
        (--current.valuep)->type = T_INVALID;
    }

    /* Allocate the memory for values, lambda_t and code */
    l0 = xalloc(values_size + sizeof *l - sizeof l->function + code_size);

    /* Copy the data */
    memcpy(l0, current.valuep, (size_t)values_size);
    l0 += values_size;
    l = (lambda_t *)l0;
    l->ref = 1;
    memcpy(l->function.code, current.code, (size_t)code_size);

    /* Fix number of constant values */
    if (num_values >= 0xff)
    {
    	/* The entry in the value block has been reserved for this */
        ((svalue_t *)l)[-0x100].u.number = num_values;
        PUT_UINT8(l->function.code, 0xff);
    }
    else
    {
        PUT_UINT8(l->function.code, (unsigned char)num_values);
    }

    /* Fix number of variables */
    PUT_UINT8( l->function.code+2
             , (unsigned char)(current.num_locals + current.max_break_stack));

    /* Clean up */
    free_symbols();
    xfree(current.code);
    xfree(current.values);

    /* If the lambda is to be bound to an object, check if the object's program
     * is scheduled for replacement. If not, mark the object as referenced.
     */
    if (origin
     && (   !(origin->prog->flags & P_REPLACE_ACTIVE)
         || !lambda_ref_replace_program(l, CLOSURE_LAMBDA, code_size, args, block)
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

    if (CLOSURE_HAS_CODE(type))
    {
    	/* Free all the values for this lambda, then the memory */
    	
        mp_int num_values;

        if (type != CLOSURE_UNBOUND_LAMBDA)
            free_object(l->ob, "free_closure");
        svp = (svalue_t *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
        {
            num_values = svp[-0x100].u.number;
        }
        while (--num_values >= 0)
            free_svalue(--svp);
        xfree(svp);
        return;
    }

    free_object(l->ob, "free_closure");
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
        svp = (svalue_t *)l2;
        if ( (num_values = EXTRACT_UCHAR(l2->function.code)) == 0xff)
            num_values = svp[-0x100].u.number;
        while (--num_values >= 0)
            free_svalue(--svp);
        xfree(svp);
        return;
    }

    if (type == CLOSURE_ALIEN_LFUN)
    {
        free_object(l->function.alien.ob, "free_closure");
    }

    /* else CLOSURE_LFUN || CLOSURE_IDENTIFIER || CLOSURE_PRELIMINARY:
     * no further references held.
     */
    xfree(l);
} /* free_closure() */

/*-------------------------------------------------------------------------*/
static void
closure_lookup_lfun_prog ( lambda_t * l
                         , Bool is_alien_lfun
                         , program_t ** pProg
                         , char ** pName
                         , Bool * pIsInherited
                         )

/* For lfun closure <l> (alien-lfun closure if <is_alien_lfun> is TRUE,
 * lookup the defining program and function name, and store the pointers
 * (uncounted) in *<pProg> and *<pName>. If the closure is defined in a
 * program inherited by <l>->function.lfun.ob, *<pIsInherited> is set to TRUE.
 *
 * The results are undefined if called for non-lfun closures.
 */

{
    object_t       *ob;
    int             ix;
    unsigned short  inhIndex;
    program_t      *prog;
    fun_hdr_p       fun;
    funflag_t       flags;
    inherit_t      *inheritp;
    Bool            is_inherited;

    is_inherited = MY_FALSE;

    if (is_alien_lfun)
    {
        ob = l->function.alien.ob;
        ix = l->function.alien.index;
        inhIndex = 0;
    }
    else
    {
        ob = l->ob;
        ix = l->function.lfun.index;
        inhIndex = l->function.lfun.inhIndex;
    }

    /* Get the program resident */
    if (O_PROG_SWAPPED(ob)) {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");
    }

    /* Find the true definition of the function */
    prog = ob->prog;

    if (inhIndex)
    {
        inheritp = &prog->inherit[inhIndex-1];
        prog = inheritp->prog;
        is_inherited = MY_TRUE;
    }

    flags = prog->functions[ix];
    while (flags & NAME_INHERITED)
    {
        is_inherited = MY_TRUE;
        inheritp = &prog->inherit[flags & INHERIT_MASK];
        ix -= inheritp->function_index_offset;
        prog = inheritp->prog;
        flags = prog->functions[ix];
    }

    /* Copy the function name pointer (a shared string) */
    fun = prog->program + (flags & FUNSTART_MASK);
    memcpy(pName, FUNCTION_NAMEP(fun) , sizeof *pName);

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
    const char *str = NULL;

    if ((type & -0x0800) == CLOSURE_EFUN)
    {
        switch(type - CLOSURE_EFUN)
        {
        case F_INDEX:
            str = "[";
            break;

        case F_RINDEX:
            str = "[<";
            break;

        case F_RANGE:
            str = "[..]";
            break;

        case F_NR_RANGE:
            str = "[..<]";
            break;

        case F_RR_RANGE:
            str = "[<..<]";
            break;

        case F_RN_RANGE:
            str = "[<..]";
            break;

        case F_MAP_INDEX:
            str = "[,]";
            break;

        case F_NX_RANGE:
            str = "[..";
            break;

        case F_RX_RANGE:
            str = "[<..";
            break;

        default:
            str = instrs[type - CLOSURE_EFUN].name;
            break;
        } /* switch() */
    } /* if() */

    return str;
} /* closure_operator_to_string() */

/*-------------------------------------------------------------------------*/
char *
closure_to_string (svalue_t * sp, Bool compact)

/* Convert the closure <sp> into a printable string and return it.
 * If <compact> is true, a compact (spaceless) representation is used.
 */

{
    char buf[1024];
    char     *rc;
    lambda_t *l;
    object_t *ob;

    rc = NULL;
    buf[sizeof(buf)-1] = '\0';
    strcpy(buf, "#'");

    if (sp->type != T_CLOSURE)
    {
        fatal("closure_to_string() called for non-closure value %hd:%hd\n"
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
        }

        /* We need the program resident */
        if (O_PROG_SWAPPED(l->ob))
        {
            l->ob->time_of_ref = current_time;
            if (load_ob_from_swap(l->ob) < 0)
                error("Out of memory.\n");
        }

        sprintf(buf, "#'%s->%s"
                   , l->ob->name
                   , l->ob->prog->variable_names[l->function.var_index].name
              );
        break;
      }

    case CLOSURE_LFUN: /* Lfun closure */
    case CLOSURE_ALIEN_LFUN: /* Lfun closure */
      {
        program_t *prog;
        char      *function_name;
        Bool       is_inherited;
        Bool       is_alien_lfun = (sp->x.closure_type == CLOSURE_ALIEN_LFUN);

        l = sp->u.lambda;

        /* For alien lfun closures, prepend the object the closure
         * is bound to.
         */
        if (is_alien_lfun)
        {
            ob = l->function.alien.ob;

            if (ob->flags & O_DESTRUCTED)
            {
                strcat(buf, compact ? "[<dest obj>]" : "[<destructed object>]");
            }
            else
            {
                strcat(buf, "[");
                strcat(buf, ob->name);
                strcat(buf, "]");
            }
        }
        else
            ob = l->ob; 

        closure_lookup_lfun_prog(l, is_alien_lfun, &prog, &function_name, &is_inherited);

        if (ob->flags & O_DESTRUCTED)
            strcat(buf, compact ? "<dest lfun>" 
                                : "<local function in destructed object>");
        else
            strcat(buf, ob->name);

        if (is_inherited)
        {
            strcat(buf, "(");
            strcat(buf, prog->name);
            buf[strlen(buf)-2] = '\0'; /* Remove the '.c' after the program name */
            strcat(buf, ")");
        }
        strcat(buf, "->");
        strcat(buf, function_name);
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
            strcat(buf, ob->name);
            strcat(buf, ">");
        }
        break;
      }

    default:
      {
        int type = sp->x.closure_type;

        if (type >= 0)
            error("Bad arg 1 to to_string(): closure type %d.\n"
                 , sp->x.closure_type);
        else
        {
            switch(type & -0x0800)
            {
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
                strcat(buf, simul_efunp[type - CLOSURE_SIMUL_EFUN].name);
                break;
            }
            break;
        } /* if (type) */
      } /* case default */
    } /* switch(closure_type) */

    rc = string_copy(buf);
    if (!rc)
        outofmem(strlen(buf)+1, "converted lambda");

    return rc;
} /* closure_to_string() */

/*-------------------------------------------------------------------------*/
int
symbol_operator (char *symbol, char **endp)

/* Analyse the text starting at <symbol> (which points to the first character
 * after the assumed "#'") if it describes a closure symbol. If yes, return
 * the operator code and set *<endp> to the first character after the
 * recognized operator.
 * If no operator can be recognized, return -1 and set *<endp> to <symbol>.
 *
 * The function is called from lex.c, ed.c and from symbol_efun().
 *
 * Recognized are the following operators:
 *
 *   #'+=     -> F_ADD_EQ
 *   #'++     -> F_POST_INC
 *   #'+      -> F_ADD
 *   #'-=     -> F_SUB_EQ
 *   #'--     -> F_POST_DEC
 *   #'-      -> F_SUBTRACT
 *   #'*=     -> F_MULT_EQ
 *   #'*      -> F_MULTIPLY
 *   #'/=     -> F_DIV_EQ
 *   #'/      -> F_DIVIDE
 *   #'%=     -> F_MOD_EQ
 *   #'%      -> F_MOD
 *   #',      -> F_POP_VALUE
 *   #'^=     -> F_XOR_EQ
 *   #'^      -> F_XOR
 *   #'||     -> F_LOR
 *   #'||=    -> F_LOR_EQ
 *   #'|=     -> F_OR_EQ
 *   #'|      -> F_OR
 *   #'&&     -> F_LAND
 *   #'&&=    -> F_LAND_EQ
 *   #'&=     -> F_AND_EQ
 *   #'&      -> F_AND
 *   #'~      -> F_COMPL
 *   #'<=     -> F_LE
 *   #'<<=    -> F_LSH_EQ
 *   #'<<     -> F_LSH
 *   #'<      -> F_LT
 *   #'>=     -> F_GE
 *   #'>>=    -> F_RSH_EQ
 *   #'>>>=   -> F_RSHL_EQ
 *   #'>>>    -> F_RSHL
 *   #'>>     -> F_RSH
 *   #'>      -> F_GT
 *   #'==     -> F_EQ
 *   #'=      -> F_ASSIGN
 *   #'!=     -> F_NE
 *   #'!      -> F_NOT
 *   #'?!     -> F_BRANCH_WHEN_NON_ZERO
 *   #'?      -> F_BRANCH_WHEN_ZERO
 *   #'[<..]  -> F_RN_RANGE
 *   #'[<..<] -> F_RR_RANGE
 *   #'[<..   -> F_RX_RANGE
 *   #'[<     -> F_RINDEX
 *   #'[..]   -> F_RANGE
 *   #'[..<]  -> F_NR_RANGE
 *   #'[..    -> F_NX_RANGE
 *   #'[,]    -> F_MAP_INDEX
 *   #'[      -> F_INDEX
 *   #'({     -> F_AGGREGATE
 *   #'([     -> F_M_CAGGREGATE
 *
 * Note that all operators must have a instrs[].Default value of '0'.
 * If necessary, update the lex::init_lexer()::binary_operators[] to
 * include the operator values.
 */

{
    char c;
    int ret;

    switch(*symbol)
    {
    case '+':
        c = symbol[1];
        if (c == '=')
        {
            symbol++;
            ret = F_ADD_EQ;
            break;
        }
        else if (c == '+')
        {
            symbol++;
            ret = F_POST_INC;
            break;
        }
        ret = F_ADD;
        break;

    case '-':
        c = symbol[1];
        if (c == '=')
        {
            symbol++;
            ret = F_SUB_EQ;
            break;
        }
        else if (c == '-')
        {
            symbol++;
            ret = F_POST_DEC;
            break;
        }
        ret = F_SUBTRACT;
        break;

    case '*':
        if (symbol[1] == '=')
        {
            symbol++;
            ret = F_MULT_EQ;
            break;
        }
        ret = F_MULTIPLY;
        break;

    case '/':
        if (symbol[1] == '=')
        {
            symbol++;
            ret = F_DIV_EQ;
            break;
        }
        ret = F_DIVIDE;
        break;

    case '%':
        if (symbol[1] == '=')
        {
            symbol++;
            ret = F_MOD_EQ;
            break;
        }
        ret = F_MOD;
        break;

    case ',':
        ret = F_POP_VALUE;
        break;

    case '^':
        if (symbol[1] == '=')
        {
            symbol++;
            ret = F_XOR_EQ;
            break;
        }
        ret = F_XOR;
        break;

    case '|':
        c = *++symbol;
        if (c == '|')
        {
            if (symbol[1] == '=')
            {
                symbol++;
                ret = F_LOR_EQ;
            }
            else
                ret = F_LOR;
            break;
        }
        else if (c == '=')
        {
            ret = F_OR_EQ;
            break;
        }
        symbol--;
        ret = F_OR;
        break;

    case '&':
        c = *++symbol;
        if (c == '&')
        {
            if (symbol[1] == '=')
            {
                symbol++;
                ret = F_LAND_EQ;
            }
            else
                ret = F_LAND;
            break;
        }
        else if (c == '=')
        {
            ret = F_AND_EQ;
            break;
        }
        symbol--;
        ret = F_AND;
        break;

    case '~':
        ret = F_COMPL;
        break;

    case '<':
        c = *++symbol;
        if (c == '=')
        {
            ret = F_LE;
            break;
        }
        else if (c == '<')
        {
            if (symbol[1] == '=')
            {
                symbol++;
                ret = F_LSH_EQ;
                break;
            }
            ret = F_LSH;
            break;
        }
        symbol--;
        ret = F_LT;
        break;

    case '>':
        c = *++symbol;
        if (c == '=')
        {
            ret = F_GE;
            break;
        }
        else if (c == '>')
        {
            if (symbol[1] == '=')
            {
                symbol++;
                ret = F_RSH_EQ;
                break;
            }
            if (symbol[1] == '>')
            {
                symbol++;
                if (symbol[1] == '=')
                {
                    symbol++;
                    ret = F_RSHL_EQ;
                    break;
                }
                ret = F_RSHL;
                break;
            }
            ret = F_RSH;
            break;
        }
        symbol--;
        ret = F_GT;
        break;

    case '=':
        if (symbol[1] == '=')
        {
            symbol++;
            ret = F_EQ;
            break;
        }
        ret = F_ASSIGN;
        break;

    case '!':
        if (symbol[1] == '=')
        {
            symbol++;
            ret = F_NE;
            break;
        }
        ret = F_NOT;
        break;

    case '?':
        if (symbol[1] == '!')
        {
            symbol++;
            ret = F_BRANCH_WHEN_NON_ZERO;
            break;
        }
        ret = F_BRANCH_WHEN_ZERO;
        break;

    case '[':
        c = *++symbol;
        if (c == '<')
        {
            if (symbol[1] == '.' && symbol[2] == '.')
            {
                c = *(symbol+=3);
                if (c == ']')
                {
                    ret = F_RN_RANGE;
                    break;
                }
                else if (c == '<' && symbol[1] == ']')
                {
                    symbol++;
                    ret = F_RR_RANGE;
                    break;
                }
                symbol--;
                ret = F_RX_RANGE;
                break;
            }
            ret = F_RINDEX;
            break;
        }
        else if (c == '.' && symbol[1] == '.')
        {
            c = *(symbol+=2);
            if (c == ']') {
                ret = F_RANGE;
                break;
            } else if (c == '<' && symbol[1] == ']') {
                symbol++;
                ret = F_NR_RANGE;
                break;
            }
            symbol--;
            ret = F_NX_RANGE;
            break;
        }
        else if (c == ',' && symbol[1] == ']')
        {
            symbol++;
            ret = F_MAP_INDEX;
            break;
        }
        symbol--;
        ret = F_INDEX;
        break;

    case '(':
        c = *++symbol;
        if (c == '{')
        {
            ret = F_AGGREGATE;
            break;
        }
        else if (c == '[')
        {
            ret = F_M_CAGGREGATE;
            break;
        }
        symbol--;
        /* FALL THROUGH */
    default:
        ret = -1;
        symbol--;
        break;
    }

    /* Symbol is not an operator */
    *endp = symbol+1;
    return ret;
} /* symbol_operator() */

/*-------------------------------------------------------------------------*/
void
symbol_efun (svalue_t *sp)

/* This function implements the efun symbol_efun(). It is also called by
 * parse_command to lookup the (simul)efuns find_living() and find_player()
 * at runtime.
 *
 * The function takes the string/symbol svalue <sp> and looks up the efun
 * or operator given by <sp>->u.string. If the efun/operator is found, the
 * value <sp> is turned into the proper closure value, otherwise it is set
 * to the numeric value 0.
 *
 * Accepted symbols are:
 *
 *   #'<operator>: see symbol_operator()
 *
 *   #'if          -> F_BRANCH_WHEN_ZERO       +CLOSURE_OPERATOR
 *   #'do          -> F_BBRANCH_WHEN_NON_ZERO  +CLOSURE_OPERATOR
 *   #'while       -> F_BBRANCH_WHEN_ZERO      +CLOSURE_OPERATOR
 *   #'foreach     -> F_FOREACH                +CLOSURE_OPERATOR
 *   #'continue    -> F_BRANCH                 +CLOSURE_OPERATOR
 *   #'default     -> F_CSTRING0               +CLOSURE_OPERATOR
 *   #'switch      -> F_SWITCH                 +CLOSURE_OPERATOR
 *   #'break       -> F_BREAK                  +CLOSURE_OPERATOR
 *   #'return      -> F_RETURN                 +CLOSURE_OPERATOR
 *   #'sscanf      -> F_SSCANF                 +CLOSURE_OPERATOR
 *   #'catch       -> F_CATCH                  +CLOSURE_OPERATOR
 *
 *   #'<efun>      -> F_<efun>                 +CLOSURE_EFUN
 *   #'<sefun>     -> <function-index>         +CLOSURE_SIMUL_EFUN
 */

{
    Bool efun_override = MY_FALSE;
    char *str;

    str = sp->u.string;

    /* If the first character is alphanumeric, the string names a function,
     * otherwise an operator.
     */
    if (isalunum(*str))
    {
    	/* It is a function or keyword.
    	 */

        ident_t *p;

        /* Take care of an leading efun override */

        if ( !strncmp(str, "efun::", 6) )
        {
            str += 6;
            efun_override = MY_TRUE;
        }

        /* Lookup the identifier in the string in the global table
         * of identifers.
         */
        if ( !(p = make_shared_identifier(str, I_TYPE_GLOBAL, 0)) )
        {
            inter_sp = sp;
            error("Out of memory (%lu bytes) for identifier\n"
                 , (unsigned long) strlen(str));
        }

        /* Loop through the possible multiple definitions.
         */
        while (p->type > I_TYPE_GLOBAL)
        {
            /* Is it a reserved word? */
            if (p->type == I_TYPE_RESWORD)
            {
                int code;

                switch(code = p->u.code)
                {
                default:
                    /* Unimplemented reserved word */
                    if ( NULL != (p = p->inferior) )
                        continue;
                    goto undefined_function;

                case L_IF:
                    code = F_BRANCH_WHEN_ZERO;
                    break;

                case L_DO:
                    code = F_BBRANCH_WHEN_NON_ZERO;
                    break;

                case L_WHILE:
                    /* the politically correct code   /
                    /  was already taken, see above. */
                    code = F_BBRANCH_WHEN_ZERO;
                    break;

                case L_FOREACH:
                    code = F_FOREACH;
                    break;

                case L_CONTINUE:
                    code = F_BRANCH;
                    break;

                case L_DEFAULT:
                    code = F_CSTRING0;
                    break;

                case L_SWITCH:
                    code = F_SWITCH;
                    break;
                case L_BREAK:
                    code = F_BREAK;
                    break;
                case L_RETURN:
                    code = F_RETURN;
                    break;
                case L_SSCANF:
                    code = F_SSCANF;
                    break;
#ifdef SUPPLY_PARSE_COMMAND
                case L_PARSE_COMMAND:
                    code = F_PARSE_COMMAND;
                    break;
#endif
                case L_CATCH:
                    code = F_CATCH;
                    break;
                }

                /* Got the reserved word: return the closure value */

                free_svalue(sp);
                sp->type = T_CLOSURE;
                sp->x.closure_type = (short)(code + CLOSURE_OPERATOR);
                sp->u.ob = ref_object(current_object, "symbol_efun");
                return;
            }
            if ( !(p = p->inferior) )
                break; /* Found a valid definition */
        }

        /* It is a real identifier */

        if (!p || p->type < I_TYPE_GLOBAL
         || (( efun_override || p->u.global.sim_efun < 0 )
             && p->u.global.efun < 0)
           )
        {
            /* But it's a (new) local identifier or a non-existing function */
            if (p && p->type == I_TYPE_UNKNOWN)
                free_shared_identifier(p);

undefined_function:
            free_svalue(sp);
            put_number(sp, 0);
            return;
        }

        /* Attempting to override a 'nomask' simul efun?
         * Check it with a privilege violation.
         */
        if (efun_override && p->u.global.sim_efun >= 0
         && simul_efunp[p->u.global.sim_efun].flags & TYPE_MOD_NO_MASK)
        {
            svalue_t *res;

            inter_sp = sp;
            push_volatile_string("nomask simul_efun");
            push_valid_ob(current_object);
            push_shared_string(p->name);
            res = apply_master(STR_PRIVILEGE, 3);

            if (!res || res->type != T_NUMBER || res->u.number < 0)
            {
            	/* Override attempt is fatal */
                error(
                  "Privilege violation: nomask simul_efun %s\n",
                  p->name
                );
            }
            else if (!res->u.number)
            {
            	/* Override attempt not fatal, but rejected nevertheless */
                efun_override = MY_FALSE;
            }
        }

        /* Symbol is ok - create the closure value */

        free_svalue(sp);
        sp->type = T_CLOSURE;
        if (!efun_override && p->u.global.sim_efun >= 0)
        {
            /* Handle non-overridden simul efuns */

            sp->x.closure_type = (short)(p->u.global.sim_efun + CLOSURE_SIMUL_EFUN);
            sp->u.ob = ref_object(current_object, "symbol_efun");
        }
        else
        {
            /* Handle efuns (possibly aliased).
             * We know that p->u.global.efun >= 0 here.
             */
            sp->x.closure_type = (short)(p->u.global.efun + CLOSURE_EFUN);
            if (sp->x.closure_type > LAST_INSTRUCTION_CODE + CLOSURE_EFUN)
                sp->x.closure_type = (short)(CLOSURE_EFUN +
                  efun_aliases[
                    sp->x.closure_type - CLOSURE_EFUN - LAST_INSTRUCTION_CODE - 1]);
            sp->u.ob = ref_object(current_object, "symbol_efun");
        }
    }
    else
    {
        int i;
        char *end;

        i = symbol_operator(str, &end);
        /* If there was a valid operator with trailing junk, *end, but i >= 0.
         * On the other hand, if we passed the empty string, i < 0, but !*end.
         * Thus, we have to test for (*end || i < 0) (not that end points
         * into sp->u.str).
         */

        if (*end || i < 0)
        {
            free_svalue(sp);
            put_number(sp, 0);
            return;
        }

        free_svalue(sp);
        sp->type = T_CLOSURE;
        if (instrs[i].Default == -1) {
            sp->x.closure_type = (short)(i + CLOSURE_OPERATOR);
        } else {
            sp->x.closure_type = (short)(i + CLOSURE_EFUN);
        }
        sp->u.ob = ref_object(current_object, "symbol_efun");
    }
} /* symbol_efun() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_unbound_lambda (svalue_t *sp)

/* TEFUN closure unbound_lambda()
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
            bad_xefun_arg(1, sp);
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

/*-------------------------------------------------------------------------*/
svalue_t *
f_symbol_variable (svalue_t *sp)

/* TEFUN symbol_variable()
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
    char *str;
    object_t *ob;
    int n;         /* Index of the desired variable */
    lambda_t *l;

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
        bad_xefun_arg(1, sp);

    case T_NUMBER:  /* The index is given directly */
        n = sp->u.number;
        if (n < 0 || n >= current_prog->num_variables)
        {
            sp->u.number = 0;
            return sp;
        }

        if (current_prog->variable_names[n].flags & NAME_HIDDEN)
        {
            if (!privilege_violation3("symbol_variable", sp, sp))
            {
                sp->u.number = 0;
                return sp;
            }
        }
        break;

    case T_STRING:  /* Name is given by string */
        if (sp->x.string_type != STRING_SHARED)
        {
            /* If the variable exists, it must exist as shared
             * string.
             */
            str = findstring(sp->u.string);
            if (sp->x.string_type == STRING_MALLOC)
                xfree(sp->u.string);
            if (!str)
            {
            	put_number(sp, 0);
            	return sp;
            }

            /* Fake a shared string value to continue processing */
            sp->x.string_type = STRING_SHARED;
            sp->u.string = ref_string(str);
        }
        /* FALL THROUGH */

    case T_SYMBOL:  /* Name is given as shared string (symbol) */
      {
        variable_t *var;
        program_t *prog;
        int num_var;

        str = sp->u.string;
        prog = current_prog;
        var = prog->variable_names;
        num_var = prog->num_variables;
        for (n = num_var; --n >= 0; var++)
        {
            if (var->name == str && !(var->flags & NAME_HIDDEN))
                break;
        }
        free_string(str);
        if (n < 0)
        {
            put_number(sp, 0);
            return sp;
        }
        n = num_var - n - 1;
      }
    }

    /* Create the result closure and put it onto the stack */
    l = xalloc(sizeof *l);
    if (!l)
    {
        inter_sp = sp - 1;
        error("Out of memory (%lu bytes) for variable symbol\n"
             , (unsigned long) sizeof(*l));
    }

    l->ob = ref_object(current_object, "symbol_variable");
    l->ref = 1;
    l->function.lfun.index = (unsigned short)(n + (current_variables - current_object->variables));
    sp->type = T_CLOSURE;
    sp->x.closure_type = CLOSURE_IDENTIFIER;
    sp->u.lambda = l;

    /* Handle replace_program() */
    if ( !(current_object->prog->flags & P_REPLACE_ACTIVE)
      || !lambda_ref_replace_program(l, sp->x.closure_type, 0, 0, 0) )
    {
        current_object->flags |= O_LAMBDA_REFERENCED;
    }

    return sp;
} /* f_symbol_variable() */

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
                 , void (*cerror)(char *)
                 , void (*cerrorl) (char *, char *, int, int)
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
         * entries are removed from the list, explicite range end entries
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
        if (        !( (total_length + key_num*(sizeof(p_int)+1))     & ~0xff) ) {
            len = 1;
            maxspan = MAXINT/len;
        }
        else if ( !( (total_length + key_num*(sizeof(p_int)+2) + 1) & ~0xffff) )
        {
            len = 2;
            maxspan = MAXINT/len;
        }
        else if ( !( (total_length + key_num*(sizeof(p_int)+3) + 2) & ~0xffffff) )
        {
            len = 3;
            maxspan = MAXINT/len;
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

    /* Create the value table 'v' */
    p += sizeof(p_int) - 4;
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
        move_memory(starta, startu, (size_t)size);
        move_memory(starta+size, abuf + sizeof abuf - (startu-starta)
                   , (size_t)(startu-starta));
    }
} /* align_switch() */

/***************************************************************************/
