#ifndef CLOSURE_H__
#define CLOSURE_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "exec.h"

/* --- Types --- */

/* --- struct lambda_s:  ---
 *
 * If the closure uses constant values, they are stored in an svalue[]
 * right before the struct lambda and indexed from the end.
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
    union               /* Closure information: */
    {
        unsigned short var_index;
          /*_IDENTIFIER: index in the variable table
           */
#       define VANISHED_VARCLOSURE_INDEX ((unsigned short)-1)
          /*              Special value for vanished variable closures.
           *              TODO: it's tested with >=0 partially :-(,
           */

        struct {
            /* CLOSURE_LFUN */
            unsigned short index;
              /* _LFUN/_ALIEN_LFUN: index in the function table
               */
            unsigned short inhIndex;
              /* 0, or (1+ inheritance index) for closure referencing
               * inherited functions (even if overloaded).
               */
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
        struct
        {
            /* CLOSURE_ALIEN_LFUN: */
            object_t       *ob;     /* Originating object */
            unsigned short  index;  /* Index in the objects variable table */
        } alien;
    } function;
};

#define LAMBDA_VALUE_OFFSET \
  (sizeof(svalue_t) + offsetof(lambda_t, function.code[1]))
  /* Offset from the fun_hdr_p of a lambda closure to the first
   * constant value (the one with index number 0).
   */

/* --- Prototypes --- */

extern long      find_function(const char *name, const program_t *prog);
extern Bool      closure_eq (svalue_t * left, svalue_t * right);
extern int       closure_cmp (svalue_t * left, svalue_t * right);
extern Bool      lambda_ref_replace_program(lambda_t *l, int type, p_int size, vector_t *args, svalue_t *block);
extern void      set_closure_user(svalue_t *svp, object_t *owner);
extern void      replace_program_lambda_adjust(replace_ob_t *r_ob);
extern void      closure_literal(svalue_t *dest, int ix, unsigned short inhIndex);
extern lambda_t *lambda(vector_t *args, svalue_t *block, object_t *origin);
extern void      free_closure(svalue_t *svp);
extern int       symbol_operator(char *symbol, char **endp);
extern void      symbol_efun(svalue_t *sp);
extern svalue_t *f_unbound_lambda(svalue_t *sp);
extern svalue_t *f_symbol_variable(svalue_t *sp);
extern void      align_switch(bytecode_p pc);

#endif /* CLOSURE_H__ */
