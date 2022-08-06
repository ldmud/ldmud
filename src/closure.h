#ifndef CLOSURE_H__
#define CLOSURE_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "bytecode.h"
#include "svalue.h"
#include "exec.h"

/* In case offsetof() is not a compiler builtin include stddef.h which
 * supplies a define as fallback. Needed for LAMBDA_VALUE_OFFSET */
#include <stddef.h>

/* --- Types --- */

/* --- struct closure_base_s:  ---
 *
 * Base structure for all allocated closure types. All specialized
 * structures will have this as their first member (so in in the
 * svalue union this base can directly be accessed).
 */

struct closure_base_s
{
    p_int ref;
      /* ref count */
    svalue_t ob;
      /* Normal or lightweight object the closure is bound to.
       * (Refcounted except for CLOSURE_UNBOUND_LAMBDA.)
       */

    object_t   *prog_ob;
    ptrdiff_t   prog_pc;
      /* Blueprint object and relative for the program creating this closure,
       * or NULL. Since this information is error reporting purposes only,
       * the lambda keeps a reference to the blueprint and not the program
       * itself to not get in the way of swapping or other things.
       */
};

/* --- struct lambda_s:  ---
 *
 * Structure used for lambda closures, i.e. closures with
 * closure_type CLOSURE_(UNBOUND_)LAMBDA. They contain their
 * bytecode.
 *
 * If the closure uses constant values, they are stored in an svalue[]
 * right before the struct lambda and indexed from the end. Context
 * variables for lambdas (that emulate inline closures) are stored
 * at the other (lower) end of the variable block.
 */

struct lambda_s
{
    /* svalue_t values[]
     *
     * The constant values used by the function which are indexed from
     * the end. That means that value #x can be found at address
     * ((svalue_t *)lambda_t)[-x-1].
     *
     * Context variables are at the other end, so variable #x can be
     * found at ((svalue_t *)lambda_t)[x-.num_values].
     */

    closure_base_t base;        /* Base closure information.    */
    mp_int num_values;          /* Number of svalues.           */
    unsigned char num_locals;   /* Number of local variables    */
    unsigned char num_arg;      /* Number of arguments needed.  */
    unsigned char num_opt_arg;  /* Number of optional arguments *
                                 * (with default values).       */
    bool xvarargs: 1;           /* Whether to the last arg is   *
                                 * a varargs argument.          */

    bytecode_t program[];       /* The bytecode of the closure. */
};

#define LAMBDA_VALUE_OFFSET (sizeof(svalue_t) + offsetof(lambda_t, program))
  /* Offset from the fun_hdr_p of a lambda closure to the first
   * constant value (the one with index number 0).
   */

/* --- struct bound_lambda_s:  ---
 *
 * Structure used for bound lambda closures
 * (closure_type CLOSURE_BOUND_LAMBDA.)
 */

struct bound_lambda_s
{
    closure_base_t base;        /* Base closure information.                */
    lambda_t *lambda;           /* Pointer to the UNBOUND_LAMBDA structure. */
};

/* --- struct lfun_closure_s:  ---
 *
 * Structure for lfun and inline closures (closure_type CLOSURE_LFUN).
 */

struct lfun_closure_s
{
    closure_base_t base;        /* Base closure information.        */
    svalue_t        fun_ob;     /* Target object with the function. */
    program_t      *inhProg;
      /* NULL or the program containing the function for closure
       * referencing inherited functions (even if overloaded).
       */

    unsigned short fun_index;    /* Index in the object's function table */
    unsigned short context_size; /* Number of context vars */

    svalue_t context[ /* .context_size */ ];
      /* inline closure context variables, if any.
       */
};

#define SIZEOF_LFUN_CLOSURE(num) (sizeof(lambda_t) + ((int)num) * sizeof(svalue_t))
  /* size_t SIZEOF_LFUN_CLOSURE(int num)
   *   Size of a lambda closure with <num> context variables.
   */

/* --- struct identifier_closure_s:  ---
 *
 * Structure for identifier closures (closure_type CLOSURE_IDENTIFIER).
 */

struct identifier_closure_s
{
    closure_base_t base;        /* Base closure information.    */
    unsigned short var_index;   /* Index in the variable table. */
#   define VANISHED_VARCLOSURE_INDEX ((unsigned short)-1)
      /* Special value for vanished variable closures.
       * TODO: it's tested with >=0 at places :-(,
       */
};


/* --- Prototypes --- */

extern long      find_function(const string_t *name, const program_t *prog);
extern Bool      closure_eq (svalue_t * left, svalue_t * right);
extern int       closure_cmp (svalue_t * left, svalue_t * right);
extern void      free_replace_program_protector (replace_ob_t *r_ob);
extern int       replace_program_function_adjust(replace_ob_t *r_ob, int fun_idx);
extern int       replace_program_variable_adjust(replace_ob_t *r_ob, int var_idx);
extern void      replace_program_lfun_closure_adjust(replace_ob_t *r_ob);
extern void      replace_program_lambda_adjust(replace_ob_t *r_ob);
extern void      closure_init_base(closure_base_t * cl, svalue_t obj);
extern lambda_t *closure_new_lambda (svalue_t obj, unsigned short context_size, Bool raise_error);
extern void      closure_lfun (svalue_t *dest, svalue_t obj, program_t *prog, int ix, unsigned short num, Bool raise_error);
extern void      closure_literal(svalue_t *dest, int ix, unsigned short inhIndex, unsigned short num);
extern void      closure_identifier (svalue_t *dest, svalue_t obj, int ix, Bool raise_error);
extern void      free_closure(svalue_t *svp);
extern Bool      is_undef_closure (svalue_t *sp);
extern void      closure_lookup_lfun_prog (lfun_closure_t *l , program_t ** pProg , string_t ** pName , Bool * pIsInherited);
extern const char * closure_operator_to_string (int type);
extern const char * closure_efun_to_string (int type);
extern string_t * closure_location (closure_base_t *l);
extern string_t *closure_to_string (svalue_t * sp, Bool compact);
extern svalue_t *v_bind_lambda(svalue_t *sp, int num_arg);
extern svalue_t *f_lambda(svalue_t *sp);
extern svalue_t *f_symbol_function(svalue_t *sp);
extern svalue_t *f_symbol_variable(svalue_t *sp);
extern svalue_t *f_unbound_lambda(svalue_t *sp);
extern svalue_t *v_compile_string(svalue_t *sp, int num_arg);
extern void      align_switch(bytecode_p pc);

/* --- helper functions --- */

static INLINE svalue_t get_bound_object(const svalue_t cl)
/* Return the object, the closure is bound to.
 */
{
    // TODO
    return CLOSURE_MALLOCED(cl.x.closure_type)
            ? cl.u.closure->ob
            : cl.x.closure_type < CLOSURE_LWO
            ? svalue_lwobject(cl.u.lwob)
            : svalue_object(cl.u.ob);
} /* get_bound_object() */

#endif /* CLOSURE_H__ */
