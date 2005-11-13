#ifndef __SWAP_H__
#define __SWAP_H__ 1

#include "driver.h"

#include "exec.h"    /* struct program */
#include "object.h"  /* struct object */

/* --- Variables --- */
extern /* TODO: bool */ short swap_compact_mode;

/* Statistics */
extern mp_int num_swapped;
extern mp_int num_unswapped;
extern mp_int total_bytes_swapped;
extern mp_int total_bytes_unswapped;
extern mp_int num_swapfree;
extern mp_int num_vb_swapped;
extern mp_int total_vb_bytes_swapped;
extern mp_int total_bytes_swapfree;
extern mp_int swapfile_size;
extern mp_int total_swap_reused;
extern long swap_num_searches;
extern long swap_total_searchlength;
extern long swap_free_searches;
extern long swap_free_searchlength;

/* More than just statistics? */
extern mp_int total_num_prog_blocks;
extern mp_int total_prog_block_size;

/* --- Prototypes --- */
extern int swap_program PROT((struct object *ob));
extern int swap_variables PROT((struct object *ob));
extern int swap PROT((struct object *ob, int mode));
extern int load_ob_from_swap PROT((struct object *ob));
extern int load_line_numbers_from_swap PROT((struct program *prog));
extern int remove_swap_file PROT((struct program *prog));
extern void name_swap_file PROT((const char *name));
extern void unlink_swap_file PROT((void));

#endif  /* __SWAP_H__ */
