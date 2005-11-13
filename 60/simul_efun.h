#ifndef __SIMUL_EFUN_H__
#define __SIMUL_EFUN_H__ 1

#include "driver.h"

#include "exec.h"       /* struct program, struct function */
#include "interpret.h"  /* struct vector */
#include "object.h"     /* struct object */

extern struct function *simul_efunp;
extern struct object *simul_efun_object;
extern struct vector *simul_efun_vector;
extern struct simul_efun_table_s simul_efun_table[];

extern struct object *get_simul_efun_object PROT((void));
extern char *query_simul_efun_file_name PROT((void));

#ifdef MALLOC_smalloc
extern void clear_simul_efun_refs PROT((void));
extern void count_simul_efun_refs PROT((void));
#endif /* MALLOC_smalloc */

#ifdef DEBUG
extern void count_simul_efun_extra_refs PROT((void));
#endif

#endif  /* __SIMUL_EFUN_H__ */
