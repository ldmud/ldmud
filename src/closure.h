#ifndef CLOSURE_H__
#define CLOSURE_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "bytecode.h"
#ifdef USE_NEW_INLINES
#include "svalue.h"
#endif /* USE_NEW_INLINES */

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
    object_t *ob;
      /* Object the closure is bound to (for bound UNBOUND_LAMBDAs just
       * during the execution of the lambda).
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
            object_t       *ob;          /* Originating object */
            unsigned short index;        /* Index in the object's function
                                          * table */
            program_t      *inhProg;     /* NULL or the program containing
                                          * the function for closure
                                          * referencing inherited functions
                                          * (even if overloaded).
                                          */
            unsigned short context_size; /* Number of context vars */
        } lfun;

        bytecode_t code[1];
          /* LAMBDA and UNBOUND_LAMBDA closures: the function code, starting
           * with uint8 'num_values' and continuing with FUNCTION_NUM_ARGS
           * (which is where the fun_hdr_p will point to).
           * 'num_values' is the size of the svalue[] preceeding the lambda;
           * if it is 0xff, the actual size is stored in
           * svalue[-0xff].u.number.
           */
#       define LAMBDA_NUM_VALUES(p)  EXTRACT_UCHAR((char *)p)
#       define LAMBDA_NUM_ARGS(p)    EXTRACT_SCHAR((char *)p + sizeof(char))
#       define LAMBDA_NUM_VARS(p)    (*((unsigned char *)((char *)p + 2*sizeof(char))))
#       define LAMBDA_CODE(p)        ((bytecode_p)((unsigned char *)p + 3*sizeof(char)))

        lambda_t *lambda;
          /* BOUND_LAMBDA: pointer to the UNBOUND_LAMBDA structure.
           */
    } function;

#ifdef USE_NEW_INLINES
    svalue_t context[ /* .lfun.context_size */ 1];
      /* lfun-closure context variables, if any.
       * Putting this array into the function.lfun somehow causes memory
       * corruption because some lambda structures won't be allocated large
       * enough.
       */
#endif
};

#define LAMBDA_VALUE_OFFSET \
  (sizeof(svalue_t) + offsetof(lambda_t, function.code[1]))
  /* Offset from the fun_hdr_p of a lambda closure to the first
   * constant value (the one with index number 0).
   */

#ifndef USE_NEW_INLINES
#define SIZEOF_LAMBDA(num) sizeof(struct lambda_s)
#else /* USE_NEW_INLINES */
#define SIZEOF_LAMBDA(num) (sizeof(struct lambda_s) + (((int)num)-1) * sizeof(svalue_t))
#endif /* USE_NEW_INLINES */
  /* size_t SIZEOF_LAMBDA(int num)
   *   Size of a lambda closure with <num> context variables.
   */

/* --- Prototypes --- */

extern long      find_function(const string_t *name, const program_t *prog);
extern Bool      closure_eq (svalue_t * left, svalue_t * right);
extern int       closure_cmp (svalue_t * left, svalue_t * right);
extern void      set_closure_user(svalue_t *svp, object_t *owner);
extern void      replace_program_lambda_adjust(replace_ob_t *r_ob);
extern void      closure_init_lambda (lambda_t * l, object_t * obj);
#ifndef USE_NEW_INLINES
extern void      closure_literal(svalue_t *dest, int ix, unsigned short inhIndex);
extern void      closure_lfun (svalue_t *dest, object_t *obj, program_t *prog, int ix, Bool raise_error);
extern lambda_t *closure_new_lambda (object_t * obj, Bool raise_error);
#else /* USE_NEW_INLINES */
extern lambda_t *closure_new_lambda (object_t * obj, unsigned short context_size, Bool raise_error);
extern void      closure_lfun (svalue_t *dest, object_t *obj, program_t *prog, int ix, unsigned short num, Bool raise_error);
extern void      closure_literal(svalue_t *dest, int ix, unsigned short inhIndex, unsigned short num);
#endif /* USE_NEW_INLINES */
extern void      closure_identifier (svalue_t *dest, object_t * obj, int ix, Bool raise_error);
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

#endif /* CLOSURE_H__ */
