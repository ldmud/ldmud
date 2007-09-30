#ifndef PKG_SQLITE_H__
#define PKG_SQLITE_H__ 1

#include "driver.h"

#ifdef USE_SQLITE

#ifndef HAS_SQLITE3
#error "pkg-sqlite configured even though the machine doesn't support SQLite3."
#endif

#include "typedefs.h"

/* --- Prototypes --- */

extern Bool sl_close (object_t *ob);

extern svalue_t * f_sl_open (svalue_t *sp);
extern svalue_t * v_sl_exec (svalue_t * sp, int num_arg) ;
extern svalue_t * f_sl_insert_id (svalue_t * sp);
extern svalue_t * f_sl_close (svalue_t * sp) ;

#endif /* USE_SQLITE */

#endif /* PKG_SQLITE_H__ */
