#ifndef __DUMPSTAT_H__
#define __DUMPSTAT_H__ 1

#include "driver.h"

#include "object.h"     /* struct object */

extern mp_int data_size PROT((struct object *ob));
extern void dumpstat PROT((void));

#endif
