#ifndef FILES_H__
#define FILES__ 1

#include "driver.h"
#include "typedefs.h"

/* --- Variables --- */

/* --- Prototypes --- */

extern svalue_t *f_copy_file (svalue_t *sp);
extern svalue_t *f_file_size (svalue_t *sp);
extern svalue_t *f_get_dir (svalue_t *sp);
extern svalue_t *f_mkdir (svalue_t *sp);
extern svalue_t *v_read_bytes (svalue_t *sp, int num_arg);
extern svalue_t *v_read_file (svalue_t *sp, int num_arg);
extern svalue_t *f_rename (svalue_t *sp);
extern svalue_t *f_rm (svalue_t *sp);
extern svalue_t *f_rmdir (svalue_t *sp);
extern svalue_t *f_write_bytes (svalue_t *sp);
extern svalue_t *f_write_file (svalue_t *sp);

#endif /* FILES_H__ */

