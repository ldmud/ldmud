#ifndef HEARTBEAT_H__
#define HEARTBEAT_H__ 1

#include "driver.h"

#include "typedefs.h"
#include "strfuns.h"    /* strbuf_t */

/* --- Variables --- */

extern object_t *current_heart_beat;

#if defined(DEBUG)
extern mp_int num_hb_objs;
#endif

/* --- Prototypes --- */

extern void  call_heart_beat(void);
extern int   set_heart_beat (object_t *ob, Bool to);
extern int   heart_beat_status (strbuf_t *sbuf, Bool verbose);
extern void  hbeat_dinfo_status(svalue_t *svp, int value);
extern svalue_t *f_set_heart_beat (svalue_t *sp);
extern svalue_t *f_heart_beat_info (svalue_t *sp);

#ifdef GC_SUPPORT
extern void  count_heart_beat_refs (void);
#endif

#endif /* HEARTBEAT_H__ */
