#ifndef SIMUL_EFUN_H__
#define SIMUL_EFUN_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "exec.h"        /* fun_hdr_p */
#include "ptrtable.h"

/* --- struct simul_efun_table_s: information of one simul_efun ---
 *
 * For every simulated efun, one of these structures is generated
 * and holds the information about where to find the code, and
 * what indices are to be used in relation to the simul_efun object
 * (in case the program was inherited by the simul-efun object).
 */
struct simul_efun_table_s
{
    function_t function;
        /* Copy from the function table.
         */
    bytecode_p  funstart;
        /* The function pointer, NULL for discarded sefuns.
         */
    program_t *program;
    p_int      function_index_offset;
    p_int      variable_index_offset;
};

/* --- Defines --- */

#define SEFUN_TABLE_SIZE (65534UL)
  /* The number of entries in the sefun table.
   *
   * The size mirrors the range of the .sim_efun index in ident_t.
   * (And considerung that I_GLOBAL_SEFUN_OTHER and
   * I_GLOBAL_SEFUN_BY_NAME are reserved indices.)
   */

/* --- Variables --- */

extern int num_simul_efun;
extern object_t *simul_efun_object;
extern vector_t *simul_efun_vector;
extern simul_efun_table_t simul_efun_table[];
extern ident_t *all_simul_efuns;

/* --- Prototypes --- */

extern void invalidate_simul_efuns (void);
extern Bool assert_simul_efun_object(void);
extern string_t *query_simul_efun_file_name(void);
extern function_t *get_simul_efun_header(ident_t* name) __attribute__((nonnull));

#ifdef GC_SUPPORT
extern void clear_simul_efun_refs(void);
extern void count_simul_efun_refs(void);
#endif /* GC_SUPPORT */

#ifdef DEBUG
extern void count_simul_efun_extra_refs(struct pointer_table *ptable);
#endif

#endif  /* SIMUL_EFUN_H__ */
