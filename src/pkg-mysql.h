#ifndef PKG_MYSQL_H__
#define PKG_MYSQL_H__ 1

#include "driver.h"
#include "typedefs.h"

#ifdef USE_MYSQL

#ifndef HAS_MYSQL
#error "pkg-mysql configured even though the machine doesn't support mySQL."
#endif

/* --- Prototypes --- */

extern Bool pkg_mysql_init (void);
extern svalue_t *f_db_affected_rows(svalue_t *sp);
extern svalue_t *f_db_conv_string(svalue_t *sp);
extern svalue_t *v_db_connect(svalue_t *sp, int num_args);
extern svalue_t *f_db_close(svalue_t *sp);
extern svalue_t *f_db_exec(svalue_t *sp);
extern svalue_t *f_db_fetch(svalue_t *sp);
extern svalue_t *f_db_handles(svalue_t *sp);

#endif /* USE_MYSQL */

#endif /* PKG_MYSQL_H__ */

