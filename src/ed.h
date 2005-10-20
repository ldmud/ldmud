#ifndef __ED_H__
#define __ED_H__ 1

#include "driver.h"

#include "comm.h"      /* struct interactive */
#include "interpret.h" /* struct svalue */
#include "object.h"    /* struct object */

/* --- Types --- */
struct ed_buffer;

/* --- Prototypes --- */

extern void prompt_from_ed_buffer PROT((struct interactive *ip));
extern void prompt_to_ed_buffer PROT((struct interactive *ip));
extern void regerror PROT((char *s));
extern void ed_start PROT((char *file_arg, char *exit_fn, struct object *exit_ob));

#ifdef MALLOC_smalloc
extern void clear_ed_buffer_refs PROT((struct ed_buffer *b));
extern void count_ed_buffer_refs PROT((struct ed_buffer *b));
#endif /* MALLOC_smalloc */

#ifdef DEBUG
extern void count_ed_buffer_extra_refs PROT((struct ed_buffer *b));
#endif

extern void free_ed_buffer PROT((void));
extern void ed_cmd PROT((char *str));
extern void save_ed_buffer PROT((void));
extern struct svalue *f_query_editing PROT((struct svalue *sp));

#endif /* __ED_H__ */
