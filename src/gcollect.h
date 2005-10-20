#ifndef __GCOLLECT_H__
#define __GCOLLECT_H__ 1

#include "driver.h"

#include "exec.h"       /* struct program */
#include "interpret.h"  /* struct svalue */
#include "object.h"     /* struct object */

#if defined(MALLOC_smalloc)

/* --- Variables --- */

extern int gcollect_outfd;
extern int garbage_collection_in_progress;
extern struct object *gc_obj_list_destructed;
extern struct lambda *stale_misc_closures;
extern struct mapping *stale_mappings;


/* --- Prototypes --- */

extern void clear_memory_reference(char *p);
extern void clear_inherit_ref(struct program *p);
extern void mark_program_ref(struct program *p);
extern void reference_destructed_object(struct object *ob);
extern void note_malloced_block_ref(char *p);
extern void count_ref_from_string(char *p);
extern void count_ref_in_vector(struct svalue *svp, size_t num);
extern void clear_ref_in_vector(struct svalue *svp, size_t num);

#endif /* MALLOC_smalloc */

/* --- Variables --- */

extern time_t time_last_gc;

/* --- Prototypes --- */

extern void garbage_collection(void);
extern void setup_print_block_dispatcher(void);

#endif /* __GCOLLECT_H__ */
