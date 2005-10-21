#ifndef OTABLE_H__
#define OTABLE_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "strfuns.h"

extern void init_otable(void);
extern size_t show_otable_status(strbuf_t *sbuf, Bool verbose);
extern void otable_dinfo_status(svalue_t *svp, int value);

extern void enter_object_hash(object_t *ob);
extern void remove_object_hash(object_t *ob);
extern object_t * lookup_object_hash(string_t *s);
extern object_t * lookup_object_hash_str(const char *s);

#ifdef GC_SUPPORT
extern void note_otable_ref(void);
#endif

#endif /* OTABLE_H__ */
