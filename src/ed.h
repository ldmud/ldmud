#ifndef ED_H__
#define ED_H__ 1

#include "driver.h"
#include "typedefs.h"

/* --- Prototypes --- */

extern size_t ed_buffer_size (ed_buffer_t *buffer);
extern svalue_t * get_ed_prompt (interactive_t *ip);

#ifdef GC_SUPPORT
extern void clear_ed_buffer_refs(ed_buffer_t *b);
extern void count_ed_buffer_refs(ed_buffer_t *b);
#endif /* GC_SUPPORT */

#ifdef DEBUG
extern void count_ed_buffer_extra_refs(ed_buffer_t *b);
#endif

extern void free_ed_buffer(void);
extern void ed_cmd(char *str);
extern void save_ed_buffer(void);
extern svalue_t *f_ed(svalue_t *sp, int num_arg);
extern svalue_t *f_query_editing(svalue_t *sp);

#endif /* ED_H__ */
