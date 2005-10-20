#ifndef __SPRINTF_H__
#define __SPRINTF_H__ 1

#include "driver.h"

#include "interpret.h"  /* struct svalue */
#include "instrs.h"     /* F_SPRINTF, F_PRINTF */

#if defined(F_SPRINTF) || defined(F_PRINTF)

extern char *string_print_formatted PROT((char *format_str, int argc, struct svalue *argv));

#endif /* defined(F_SPRINTF) || defined(F_PRINTF) */

#endif /* __SPRINTF_H__ */
