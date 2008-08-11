#ifndef BITSTRINGS_H__
#define BITSTRINGS_H__ 1

#include "driver.h"
#include "typedefs.h"

/* --- Variables --- */

/* --- Prototypes --- */

extern svalue_t *f_clear_bit (svalue_t *sp);
extern svalue_t *f_set_bit(svalue_t *);
extern svalue_t *f_test_bit(svalue_t *);
extern svalue_t *f_or_bits(svalue_t *);
extern svalue_t *f_and_bits(svalue_t *);
extern svalue_t *f_xor_bits(svalue_t *);
extern svalue_t *f_invert_bits(svalue_t *);
extern svalue_t *f_last_bit(svalue_t *);
extern svalue_t *f_next_bit(svalue_t *);
extern svalue_t *f_count_bits(svalue_t *);
extern svalue_t *v_copy_bits(svalue_t *, int num_arg);

#endif /* BITSTRINGS_H__ */

