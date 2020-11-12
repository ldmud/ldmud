#ifndef PARSE_H__
#define PARSE_H__ 1

#include "driver.h"
#include "typedefs.h"

#if defined(USE_PARSE_COMMAND)

extern svalue_t *v_parse_command(svalue_t *sp, int num_arg);

#if defined(GC_SUPPORT)
extern void clear_parse_refs(void);
extern void count_parse_refs(void);
#endif

#endif /* USE_PARSE_COMMAND */

#endif  /* PARSE_H__ */
