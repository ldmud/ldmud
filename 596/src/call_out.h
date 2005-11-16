#ifndef CALL_OUT_H__
#define CALL_OUT_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "strfuns.h"  /* strbuf_t */

/* --- Prototypes --- */

extern svalue_t *new_call_out(svalue_t *sp, short num_arg);
extern void  call_out(void);
extern void  find_call_out(object_t *ob, svalue_t *fun, Bool do_free_call);
extern size_t  call_out_status(strbuf_t *sbuf, Bool verbose);
extern void  callout_dinfo_status(svalue_t *svp, int value);
extern void  remove_stale_call_outs(void);
extern vector_t *get_all_call_outs(void);

#ifdef DEBUG
extern void count_extra_ref_from_call_outs(void);
#endif

#ifdef GC_SUPPORT
extern void  clear_ref_from_call_outs(void);
extern void  count_ref_from_call_outs(void);
#endif

#endif /* CALL_OUT_H__ */
