#ifndef GCOLLECT_H__
#define GCOLLECT_H__ 1

#include "driver.h"
#include "typedefs.h"

#if defined(GC_SUPPORT)

/* --- Variables --- */

/* Status of the GC:
 *   gcInactive == 0: 'no collection' is active, i.e. all refcounts are valid
 *   gcClearRefs:     'clear refcounts' phase
 *   gcCountRefs:     'recompute refcounts' phase
 */
typedef enum { gcInactive = 0, gcClearRefs, gcCountRefs } gc_status_t;
extern gc_status_t gc_status;

extern int default_gcollect_outfd;
extern int gcollect_outfd;
extern object_t *gc_obj_list_destructed;
extern lambda_t *stale_misc_closures;
extern mapping_t *stale_mappings;

/* --- Macros --- */

/* void GC_REF_DUMP(type, pointer, txt, function)
 * void GC_STRING_REF_DUMP(type, pointer, txt, function)
 *
 * In order to be able to dump the references found in a GC, the
 * macro DUMP_GC_REFS is used to define the 'ref_this' functions as
 * wrapper around the actual function to call.
 *
 * If DUMP_GC_REFS is not defined, the wrapper is just the function call.
 *
 * <type> is the type of <pointer>, which is the structure referenced.
 * <txt> is the descriptive text to print, <function> the actual function
 * to call.
 *
 * GC_STRING_REF_DUMP() works like GC_REF_DUMP(), except that it prints
 * the additional information used by string references.
 */

#ifdef DUMP_GC_REFS

#define GC_REF_DUMP(type,p,txt,fun) \
    do { type p_ = p; \
         dprintf3(gcollect_outfd, txt " %x: %s %d\n", (long)p_, (long)__FILE__, (long)__LINE__); \
         fun(p_); \
    } while(0)

#define GC_STRING_REF_DUMP(type,p,txt,fun) \
    do { type p_ = p; \
         dprintf4(gcollect_outfd, txt " %x (%x): %s %d\n", (long)p_, (long)(p_-sizeof(StrRefCount)-sizeof(char*)), (long)__FILE__, (long)__LINE__); \
         fun(p_); \
    } while(0)

#else

#define GC_REF_DUMP(type,p,txt,fun) fun(p)
#define GC_STRING_REF_DUMP(type,p,txt,fun) fun(p)

#endif /* DUMP_GC_REFS */

/* --- Prototypes --- */

extern void clear_memory_reference(void *p);
extern void clear_inherit_ref(program_t *p);
extern void clear_object_ref (object_t *p);
extern void gc_mark_program_ref(program_t *p);
extern void gc_reference_destructed_object(object_t *ob);
#ifdef CHECK_OBJECT_GC_REF
extern void gc_note_malloced_block_ref(void *p, const char * file, int line);
extern void gc_count_ref_in_vector(svalue_t *svp, size_t num, const char * file, int line);
#else
extern void gc_note_malloced_block_ref(void *p);
extern void gc_count_ref_in_vector(svalue_t *svp, size_t num);
#endif
extern void gc_count_ref_from_string(char *p);
extern void clear_ref_in_vector(svalue_t *svp, size_t num);

extern void restore_default_gc_log (void);

#define mark_program_ref(p) \
    GC_REF_DUMP(program_t*, p, "Mark program", gc_mark_program_ref)

#define reference_destructed_object(p) \
    GC_REF_DUMP(object_t*, p, "Ref dest' object", gc_reference_destructed_object)

#ifdef CHECK_OBJECT_GC_REF
#define note_malloced_block_ref(p) gc_note_malloced_block_ref(p, __FILE__, __LINE__)
#else
#define note_malloced_block_ref(p) \
    GC_REF_DUMP(void*, p, "Note malloced block", gc_note_malloced_block_ref)
#endif

#define count_ref_from_string(p) \
    GC_STRING_REF_DUMP(char*, p, "Ref from string", gc_count_ref_from_string)

#ifdef DUMP_GC_REFS

#define count_ref_in_vector(p, num) \
    do { svalue_t *p_ = p; size_t num_ = num; \
         dprintf4(gcollect_outfd, "Count ref in vector %x size %d: %s %d\n", (long)p_, (long)num_, (long)__FILE__, (long)__LINE__); \
         gc_count_ref_in_vector(p_, num_); \
    } while(0)

#else

#ifdef CHECK_OBJECT_GC_REF
#define count_ref_in_vector(p, num)     gc_count_ref_in_vector(p, num, __FILE__, __LINE__)
#else
#define count_ref_in_vector(p, num)     gc_count_ref_in_vector(p, num)
#endif

#endif /* DUMP_GC_REFS */

#else /* no GC_SUPPORT */

#define gc_status (0)

#endif /* GC_SUPPORT */

/* --- Variables --- */

extern time_t time_last_gc;

/* --- Prototypes --- */

extern void garbage_collection(void);
extern void setup_print_block_dispatcher(void);

#endif /* GCOLLECT_H__ */
