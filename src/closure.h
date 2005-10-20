#ifndef __CLOSURE_H__
#define __CLOSURE_H__ 1

#include "driver.h"
#include "exec.h"      /* struct program */
#include "interpret.h" /* struct lambda, svalue, symbol, vector */
#include "object.h"    /* struct object, replace_ob */

/* --- Prototypes --- */
extern int find_function PROT((char *name, struct program *prog));
extern int lambda_ref_replace_program PROT((struct lambda *l, int type, p_int size, struct vector *args, struct svalue *block));
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
