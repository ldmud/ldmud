#ifndef __PARSE_H__
#define __PARSE_H__ 1

#include "driver.h"

#include "instrs.h"      /* F_PROCESS_STRING, F_DESCRIBE */
#include "interpret.h"   /* struct svalue */

#if defined(SUPPLY_PARSE_COMMAND)

extern struct svalue find_living_closures[2];
extern struct object *find_living_object PROT((char *name, int player));
extern int parse PROT((char *cmd, struct svalue *ob_or_array, char *pattern, struct svalue *stack_args, int num_arg));

#endif /* SUPPLY_PARSE_COMMAND */

#ifdef F_PROCESS_STRING
extern char * process_string PROT((char *str));
#endif /* F_PROCESS_STRING */

#endif  /* __PARSE_H__ */
