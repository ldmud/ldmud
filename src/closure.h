#ifndef __CLOSURE_H__
#define __CLOSURE_H__ 1

#include "driver.h"
#include "exec.h"      /* struct program */
#include "interpret.h" /* struct lambda, svalue, symbol, vector */
#include "object.h"    /* struct object, replace_ob */

/* --- Types --- */

/* --- struct lambda:  ---
 *
 * If the closure uses constant values, they are stored in an svalue[]
 * right before the struct lambda and indexed from the end.
 */

struct lambda {
    p_int ref;          /* ref count */
    struct object *ob;
      /* Object the closure is bound to (for bound UNBOUND_LAMBDAs just
       * during the execution of the lambda).
       */
    union               /* Closure information: */
    {
        unsigned short index;
          /* _LFUN/_ALIEN_LFUN: index in the function table
           * _IDENTIFIER: index in the variable table
           *              special (short)-1: vanished variable closure
           *              TODO: it's tested with >=0 partially :-(,
           *              TODO:: should be a constant.
           */
        unsigned char code[1];
          /* LAMBDA and UNBOUND_LAMBDA closures: the function code, starting
           * with uint8 'num_values' and continuing with FUNCTION_NUM_ARGS.
           * 'num_values' is the size of the svalue[] preceeding the lambda;
           * if it is 0xff, the actual size is stored in
           * svalue[-0xff].u.number.
           */
        struct lambda *lambda;
          /* BOUND_LAMBDA: pointer to the UNBOUND_LAMBDA structure.
           */
        struct
        {
            /* CLOSURE_ALIEN_LFUN: */
            struct object  *ob;     /* Originating object */
            unsigned short  index;  /* Index in the objects variable table */
        } alien;
    } function;
};

#define LAMBDA_VALUE_OFFSET \
  (sizeof(struct svalue) + offsetof(struct lambda, function.code[1]))
  /* Offset from the fun_hdr_p of a lambda closure to the first
   * constant value.
   */

/* --- Prototypes --- */

extern int find_function PROT((char *name, struct program *prog));
extern int lambda_ref_replace_program(struct lambda *l, int type, p_int size, struct vector *args, struct svalue *block);
extern void set_closure_user PROT((struct svalue *svp, struct object *owner));
extern void replace_program_lambda_adjust PROT((struct replace_ob *r_ob));
extern void closure_literal PROT((struct svalue *dest, int ix));
extern struct lambda *lambda PROT((struct vector *args, struct svalue *block, struct object *origin));
extern void free_closure PROT((struct svalue *svp));
extern int symbol_operator PROT((char *symbol, char **endp));
extern void symbol_efun PROT((struct svalue *sp));
extern struct svalue *f_unbound_lambda PROT((struct svalue *sp));
extern struct svalue *f_symbol_variable PROT((struct svalue *sp));
extern void align_switch PROT((unsigned char *pc));

#endif /* __CLOSURE_H__ */
