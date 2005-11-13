#ifndef __HEARTBEAT_H__
#define __HEARTBEAT_H__ 1

#include "driver.h"

#include "datatypes.h"  /* struct svalue */
#include "object.h"     /* struct object */

/* --- Variables --- */

extern struct object *current_heart_beat;

#if !defined(OLD_RESET) && defined(DEBUG)
extern mp_int num_hb_objs;
#endif

/* --- Prototypes --- */

extern void  call_heart_beat(void);
extern int   set_heart_beat (struct object *ob, /* TODO: BOOL */ int to);
extern int   heart_beat_status (int /* TODO: BOOL */ verbose);
extern struct svalue *f_heart_beat_info (struct svalue *sp);

#ifdef MALLOC_smalloc
extern void  count_heart_beat_refs (void);
#endif

#endif /* __HEARTBEAT_H__ */
