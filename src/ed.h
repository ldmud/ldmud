#ifndef ED_H__
#define ED_H__ 1

#include "driver.h"
#include "typedefs.h"

/* --- Prototypes --- */

extern size_t ed_buffer_size (input_t *ih);

#ifdef GC_SUPPORT
extern void clear_ed_buffer_refs(input_t *ih);
extern void count_ed_buffer_refs(input_t *ih);
#endif /* GC_SUPPORT */

#ifdef DEBUG
extern void count_ed_buffer_extra_refs(input_t *ih);
#endif

extern void free_ed_buffer(input_t *ih);
extern void save_ed_buffer(input_t *ih);
extern void ed_cmd(char *str, input_t *ih);
extern svalue_t *v_ed(svalue_t *sp, int num_arg);
extern svalue_t *f_query_editing(svalue_t *sp);

#endif /* ED_H__ */
