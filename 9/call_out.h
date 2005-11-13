#ifndef __CALL_OUT_H__
#define __CALL_OUT_H__ 1

#include "driver.h"
#include "interpret.h" /* struct svalue, struct vector */
#include "object.h"    /* struct object */

/* --- Prototypes --- */
extern struct svalue *new_call_out PROT((struct svalue *sp, int num_arg));
extern void  call_out PROT((void));
extern void  find_call_out PROT((struct object *ob, struct svalue *fun, int do_free_call));
extern int   print_call_out_usage PROT((int verbose));
extern void  remove_stale_call_outs PROT((void));
extern struct vector *get_all_call_outs PROT((void));

#ifdef DEBUG
extern void count_extra_ref_from_call_outs PROT((void));
#endif

#ifdef MALLOC_smalloc
extern void  clear_ref_from_call_outs PROT((void));
extern void  count_ref_from_call_outs PROT((void));
#endif

#endif /* __CALL_OUT_H__ */
