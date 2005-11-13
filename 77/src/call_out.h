#ifndef __CALL_OUT_H__
#define __CALL_OUT_H__ 1

#include "driver.h"
#include "interpret.h" /* struct svalue, struct vector */
#include "object.h"    /* struct object */

/* --- Prototypes --- */

extern struct svalue *new_call_out(struct svalue *sp, int num_arg);
extern void  call_out(void);
extern void  find_call_out(struct object *ob, struct svalue *fun, /* TODO: BOOL */ int do_free_call);
extern int   print_call_out_usage(/* TODO: BOOL */ int verbose);
extern void  remove_stale_call_outs(void);
extern struct vector *get_all_call_outs(void);

#ifdef DEBUG
extern void count_extra_ref_from_call_outs(void);
#endif

#ifdef MALLOC_smalloc
extern void  clear_ref_from_call_outs(void);
extern void  count_ref_from_call_outs(void);
#endif

#endif /* __CALL_OUT_H__ */
