#ifndef GCOLLECT_H__
#define GCOLLECT_H__ 1

#include "driver.h"
#include "typedefs.h"

#if defined(GC_SUPPORT)

/* --- Variables --- */

extern int gcollect_outfd;
extern int garbage_collection_in_progress;
extern object_t *gc_obj_list_destructed;
extern lambda_t *stale_misc_closures;
extern mapping_t *stale_mappings;


/* --- Prototypes --- */

extern void clear_memory_reference(void *p);
extern void clear_inherit_ref(program_t *p);
extern void mark_program_ref(program_t *p);
extern void reference_destructed_object(object_t *ob);
extern void note_malloced_block_ref(void *p);
extern void count_ref_from_string(char *p);
extern void count_ref_in_vector(svalue_t *svp, size_t num);
extern void clear_ref_in_vector(svalue_t *svp, size_t num);

#else

#define garbage_collection_in_progress (0)

#endif /* GC_SUPPORT */

/* --- Variables --- */

extern time_t time_last_gc;

/* --- Prototypes --- */

extern void garbage_collection(void);
extern void setup_print_block_dispatcher(void);

#endif /* GCOLLECT_H__ */
