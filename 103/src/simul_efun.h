#ifndef __SIMUL_EFUN_H__
#define __SIMUL_EFUN_H__ 1

#include "driver.h"

#include "exec.h"       /* struct program, struct function, ... */
#include "interpret.h"  /* struct vector */
#include "object.h"     /* struct object */
#include "ptrtable.h"

/* --- struct simul_efun_table_s: information of one simul_efun ---
 *
 * For every simulated efun, one of these structures is generated
 * and holds the information about where to find the code, and
 * what indices are to be used in relation to the simul_efun object
 * (in case the program was inherited by the sefun object).
 */
struct simul_efun_table_s
{
    fun_hdr_p       funstart;
      /* TODO: This entry can be NULL. When and why? */
    struct program *program;
    p_int           function_index_offset;
    p_int           variable_index_offset;
};

/* --- Variables --- */

extern struct function *simul_efunp;
extern struct object *simul_efun_object;
extern struct vector *simul_efun_vector;
extern struct simul_efun_table_s simul_efun_table[];

/* --- Prototypes --- */

extern struct object *get_simul_efun_object PROT((void));
extern char *query_simul_efun_file_name PROT((void));

#ifdef MALLOC_smalloc
extern void clear_simul_efun_refs PROT((void));
extern void count_simul_efun_refs PROT((void));
#endif /* MALLOC_smalloc */

#ifdef DEBUG
extern void count_simul_efun_extra_refs PROT((struct pointer_table *ptable));
#endif

#endif  /* __SIMUL_EFUN_H__ */
