#ifndef SPRINTF_H__
#define SPRINTF_H__ 1

#include "driver.h"
#include "typedefs.h"

extern svalue_t *v_printf(svalue_t *sp, int num_arg);
extern svalue_t *v_sprintf(svalue_t *sp, int num_arg);

#define SPRINTF_LPC_INDENT 2


#endif /* SPRINTF_H__ */
