#ifndef SMALLOC_H__
#define SMALLOC_H__ 1

#include "driver.h"
#include "typedefs.h"

#ifdef MALLOC_smalloc

/* --- Prototypes --- */

extern void mem_dump_data(strbuf_t *sbuf);
extern void mem_dump_extdata(strbuf_t *sbuf);
extern void mem_dinfo_data(svalue_t *svp, int value);
extern void mem_clear_ref_flags(void);
extern void mem_free_unrefed_memory(void);
extern void mem_consolidate (Bool force);
extern void walk_new_small_malloced( void (*func)(POINTER, long) );
#ifdef MALLOC_CHECK
extern Bool mem_is_freed (void *p, p_uint minsize);
#endif /* MALLOC_CHECK */

#ifdef CHECK_MAPPING_TOTAL
extern mp_int available_memory(void);
#endif /* CHECK_MAPPING_TOTAL */

#endif /* MALLOC_smalloc */

#endif /* SMALLOC_H__ */
