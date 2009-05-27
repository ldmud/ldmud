#ifndef CALL_OUT_H__
#define CALL_OUT_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "strfuns.h"  /* strbuf_t */

/* --- Prototypes --- */

extern void  call_out(void);
extern void  next_call_out_cycle(void);
extern size_t  call_out_status(strbuf_t *sbuf, Bool verbose);
extern void  callout_dinfo_status(svalue_t *svp, int value);
extern void  remove_stale_call_outs(void);

extern svalue_t *v_call_out(svalue_t *sp, int num_arg);
extern svalue_t *f_call_out_info(svalue_t *sp);
extern svalue_t *f_find_call_out(svalue_t *sp);
extern svalue_t *f_remove_call_out(svalue_t *sp);

#ifdef DEBUG
extern void count_extra_ref_from_call_outs(void);
#endif

#ifdef GC_SUPPORT
extern void  clear_ref_from_call_outs(void);
extern void  count_ref_from_call_outs(void);
#endif

#endif /* CALL_OUT_H__ */
