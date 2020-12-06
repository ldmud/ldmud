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

/* --- struct lambda_s:  ---
 *
 * If the closure uses constant values, they are stored in an svalue[]
 * right before the struct lambda and indexed from the end.
 * For lfun closures, the context variables are stored after the
 * lambda_s. If the lfun closure has no context variables, or for
 * other kinds of closures the context[] part is not allocated at all.
 */

struct lambda_s
{
    /* svalue_t values[]
     *
     * For lambda closures, the constant values used by the function
     * which are indexed from the end. That means that value #x can
     * be found at address ((svalue_t *)lambda_t)[-x-1].
     */

    p_int ref;          /* ref count */
    svalue_t ob;
      /* Normal or lightweight object the closure is bound to
       * (for bound UNBOUND_LAMBDAs just during the execution of the lambda).
       */

    object_t   *prog_ob;
    ptrdiff_t   prog_pc;
      /* Blueprint object and relative for the program creating this closure,
       * or NULL. Since this information is error reporting purposes only,
       * the lambda keeps a reference to the blueprint and not the program
       * itself to not get in the way of swapping or other things.
       */

    union               /* Closure information: */
    {
        unsigned short var_index;
          /* _IDENTIFIER: index in the variable table
           */
#       define VANISHED_VARCLOSURE_INDEX ((unsigned short)-1)
          /*              Special value for vanished variable closures.
           *              TODO: it's tested with >=0 at places :-(,
           */

        struct {
            /* CLOSURE_LFUN */
            svalue_t        ob;          /* Originating object */
            program_t      *inhProg;     /* NULL or the program containing
                                          * the function for closure
                                          * referencing inherited functions
                                          * (even if overloaded).
                                          */
            unsigned short index;        /* Index in the object's function
                                          * table */
            unsigned short context_size; /* Number of context vars */
        } lfun;

        struct
        {
            mp_int num_values;        /* Number of svalues.          */
            unsigned char num_locals; /* Number of local variables   */
            unsigned char num_arg;    /* Number of arguments needed. */
            bytecode_t program[1];
        } code;
          /* LAMBDA and UNBOUND_LAMBDA closures: the compiled function code.
           * 'num_values' is the size of the svalue[] preceeding the lambda.
           */

        lambda_t *lambda;
          /* BOUND_LAMBDA: pointer to the UNBOUND_LAMBDA structure.
           */
    } function;

    svalue_t context[ /* .lfun.context_size */ ];
      /* lfun-closure context variables, if any.
       * Putting this array into the function.lfun somehow causes memory
       * corruption because some lambda structures won't be allocated large
       * enough.
       */
};

#define LAMBDA_VALUE_OFFSET \
  (sizeof(svalue_t) + offsetof(lambda_t, function.code.program[0]))
  /* Offset from the fun_hdr_p of a lambda closure to the first
   * constant value (the one with index number 0).
   */

#define SIZEOF_LAMBDA(num) (sizeof(struct lambda_s) + ((int)num) * sizeof(svalue_t))
  /* size_t SIZEOF_LAMBDA(int num)
   *   Size of a lambda closure with <num> context variables.
   */

/* --- Prototypes --- */

extern long      find_function(const string_t *name, const program_t *prog);
extern Bool      closure_eq (svalue_t * left, svalue_t * right);
extern int       closure_cmp (svalue_t * left, svalue_t * right);
extern void      free_replace_program_protector (replace_ob_t *r_ob);
extern int       replace_program_function_adjust(replace_ob_t *r_ob, int fun_idx);
extern int       replace_program_variable_adjust(replace_ob_t *r_ob, int var_idx);
extern void      replace_program_lfun_closure_adjust(replace_ob_t *r_ob);
extern void      replace_program_lambda_adjust(replace_ob_t *r_ob);
extern void      closure_init_lambda (lambda_t * l, svalue_t obj);
extern lambda_t *closure_new_lambda (svalue_t obj, unsigned short context_size, Bool raise_error);
extern void      closure_lfun (svalue_t *dest, svalue_t obj, program_t *prog, int ix, unsigned short num, Bool raise_error);
extern void      closure_literal(svalue_t *dest, int ix, unsigned short inhIndex, unsigned short num);
extern void      closure_identifier (svalue_t *dest, svalue_t obj, int ix, Bool raise_error);
extern void      free_closure(svalue_t *svp);
extern Bool      is_undef_closure (svalue_t *sp);
extern void      closure_lookup_lfun_prog ( lambda_t * l , program_t ** pProg , string_t ** pName , Bool * pIsInherited);
extern const char * closure_operator_to_string (int type);
extern const char * closure_efun_to_string (int type);
extern string_t * closure_location (lambda_t *l);
extern string_t *closure_to_string (svalue_t * sp, Bool compact);
extern svalue_t *v_bind_lambda(svalue_t *sp, int num_arg);
extern svalue_t *f_lambda(svalue_t *sp);
extern svalue_t *f_symbol_function(svalue_t *sp);
extern svalue_t *f_symbol_variable(svalue_t *sp);
extern svalue_t *f_unbound_lambda(svalue_t *sp);
extern void      align_switch(bytecode_p pc);

/* --- helper functions --- */

static INLINE svalue_t get_bound_object(const svalue_t cl)
/* Return the object, the closure is bound to.
 */
{
    return CLOSURE_MALLOCED(cl.x.closure_type)
            ? cl.u.lambda->ob
            : cl.x.closure_type < CLOSURE_LWO
            ? svalue_lwobject(cl.u.lwob)
            : svalue_object(cl.u.ob);
} /* get_bound_object() */

#endif /* CLOSURE_H__ */
