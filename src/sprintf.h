#ifndef SPRINTF_H__
#define SPRINTF_H__ 1

#include "driver.h"

#include "typedefs.h" 

extern char *string_print_formatted(char *format_str, int argc, svalue_t *argv);

#endif /* SPRINTF_H__ */
