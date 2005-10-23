#ifndef PARSE_H__
#define PARSE_H__ 1

#include "driver.h"
#include "typedefs.h"

#if defined(USE_PARSE_COMMAND)

extern Bool e_parse_command( string_t *cmd, svalue_t *ob_or_array
                           , string_t *pattern
                           , svalue_t *stack_args, int num_arg);

#if defined(GC_SUPPORT)
extern void clear_parse_refs(void);
extern void count_parse_refs(void);
#endif

#endif /* USE_PARSE_COMMAND */

#endif  /* PARSE_H__ */
