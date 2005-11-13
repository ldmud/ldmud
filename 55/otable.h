#ifndef __OTABLE_H__
#define __OTABLE_H__ 1

#include "driver.h"
#include "object.h"  /* struct object */

extern void init_otable(void);
extern void enter_object_hash(struct object *ob);
extern void remove_object_hash(struct object *ob);
extern struct object * lookup_object_hash(char *s);
extern size_t show_otable_status(/* TODO: BOOL */ short verbose);

#ifdef MALLOC_smalloc
extern void note_otable_ref(void);
#endif

#endif /* __OTABLE_H__ */
