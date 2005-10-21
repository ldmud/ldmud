#ifndef PARSE_H__
#define PARSE_H__ 1

#include "driver.h"
#include "typedefs.h"

#if defined(SUPPLY_PARSE_COMMAND)

/* implemented in parse.c (!compat) */
extern Bool e_parse_command( string_t *cmd, svalue_t *ob_or_array
                           , string_t *pattern
                           , svalue_t *stack_args, int num_arg);

#if defined(GC_SUPPORT)
extern void clear_parse_refs(void);
extern void count_parse_refs(void);
#endif

/* implemented in parse_old.c (compat) */
extern Bool e_old_parse_command( string_t *cmd, svalue_t *ob_or_array
                               , string_t *pattern
                               , svalue_t *stack_args, int num_arg);

#if defined(GC_SUPPORT)
extern void clear_old_parse_refs(void);
extern void count_old_parse_refs(void);
#endif

#endif /* SUPPLY_PARSE_COMMAND */

#endif  /* PARSE_H__ */
