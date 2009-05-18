#ifndef ARRAYLIST_H__
#define ARRAYLIST_H__ 1

#include "driver.h"
#include <stddef.h>

#include "typedefs.h"
#include "svalue.h"

extern void put_arraylist(svalue_t * list);
extern svalue_t * enhance_arraylist(svalue_t * list);
extern void finalize_arraylist(svalue_t * list);

#endif /* ARRAYLIST_H__ */
