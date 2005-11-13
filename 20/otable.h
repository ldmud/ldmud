#ifndef __OTABLE_H__
#define __OTABLE_H__ 1

#include "driver.h"
#include "object.h"  /* struct object */

extern void init_otable PROT((void));
extern void enter_object_hash PROT((struct object *ob));
extern void remove_object_hash PROT((struct object *ob));
extern struct object * lookup_object_hash PROT((char *s));
extern int show_otable_status PROT((int verbose));

#ifdef MALLOC_smalloc
extern void note_otable_ref PROT((void));
#endif

#endif /* __OTABLE_H__ */
