#ifndef __ED_H__
#define __ED_H__ 1

#include "driver.h"

#include "comm.h"      /* struct interactive */
#include "interpret.h" /* struct svalue */
#include "object.h"    /* struct object */

/* --- Types --- */

struct ed_buffer;

/* --- Prototypes --- */

extern void prompt_from_ed_buffer(struct interactive *ip);
extern void prompt_to_ed_buffer(struct interactive *ip);
extern void regerror(char *s);
extern void ed_start(char *file_arg, char *exit_fn, struct object *exit_ob);

#ifdef MALLOC_smalloc
extern void clear_ed_buffer_refs(struct ed_buffer *b);
extern void count_ed_buffer_refs(struct ed_buffer *b);
#endif /* MALLOC_smalloc */

#ifdef DEBUG
extern void count_ed_buffer_extra_refs(struct ed_buffer *b);
#endif

extern void free_ed_buffer(void);
extern void ed_cmd(char *str);
extern void save_ed_buffer(void);
extern struct svalue *f_query_editing(struct svalue *sp);

#endif /* __ED_H__ */
