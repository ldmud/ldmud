#ifndef __DUMPSTAT_H__
#define __DUMPSTAT_H__ 1

#include "driver.h"

#include "object.h"     /* struct object */

extern mp_int data_size(struct object *ob);
extern Bool dumpstat(char *name);

#endif
