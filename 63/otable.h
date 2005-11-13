#ifndef __OTABLE_H__
#define __OTABLE_H__ 1

#include "driver.h"
#include "object.h"  /* struct object */

extern void init_otable(void);
extern size_t show_otable_status(/* TODO: BOOL */ short verbose);

extern void enter_object_hash(struct object *ob);
extern void remove_object_hash(struct object *ob);
extern struct object * lookup_object_hash(char *s);

#ifndef OLD_RESET
extern void rtable_add(struct object *);
extern void rtable_remove(struct object *);
extern struct object *rtable_next_due(time_t);
extern size_t show_rtable_status (/* TODO: BOOL */ short verbose);
#endif

#ifdef MALLOC_smalloc
extern void note_otable_ref(void);
#endif

#endif /* __OTABLE_H__ */
