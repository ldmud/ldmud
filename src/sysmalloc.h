#ifndef SYSMALLOC_H__
#define SYSMALLOC_H__ 1

#include "driver.h"
#include "typedefs.h"

#ifdef MALLOC_sysmalloc

/* --- Prototypes --- */

extern void mem_dump_data(strbuf_t *sbuf);
extern void mem_dinfo_data(svalue_t *svp, int value);
extern void mem_consolidate (Bool force);

#endif /* MALLOC_sysmalloc */

#endif /* SYSMALLOC_H__ */
