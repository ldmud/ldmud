#ifndef SWAP_H__
#define SWAP_H__ 1

#include "driver.h"
#include "typedefs.h"

/* --- Variables --- */
extern Bool swap_compact_mode;
extern mp_int num_swapped;
extern mp_int num_unswapped;
extern mp_int total_bytes_swapped;
extern mp_int total_bytes_unswapped;
extern mp_int num_vb_swapped;
extern mp_int total_vb_bytes_swapped;
extern mp_int total_num_prog_blocks;
extern mp_int total_prog_block_size;

/* --- Prototypes --- */
extern Bool swap_program(object_t *ob);
extern Bool swap_variables(object_t *ob);
extern Bool swap(object_t *ob, int mode);
extern int load_ob_from_swap(object_t *ob);
extern Bool load_line_numbers_from_swap(program_t *prog);
extern void remove_prog_swap(program_t *prog, Bool load_line_numbers);
extern void name_swap_file(const char *name);
extern void unlink_swap_file(void);
extern size_t swap_overhead (void);
extern void swap_status(strbuf_t *sbuf);
extern void swap_dinfo_data (svalue_t *svp, int value);

#endif  /* SWAP_H__ */
