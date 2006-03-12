#ifndef PKG_PGSQL_H__
#define PKG_PGSQL_H__ 1

#include "driver.h"

#ifdef USE_PGSQL

#ifndef HAS_PGSQL
#error "pkg-mysql configured even though the machine doesn't support mySQL."
#endif

#include <unistd.h>
#include "typedefs.h"

/* --- Prototypes --- */

extern void pg_setfds(fd_set *readfds, fd_set *writefds, int *nfds);
extern void pg_process_all(void);
extern void pg_purge_connections (void);

extern svalue_t *v_pg_connect(svalue_t *sp, int num_arg);
extern svalue_t *v_pg_query(svalue_t *sp, int numarg);
extern svalue_t *f_pg_close(svalue_t *sp);
extern svalue_t * f_pg_conv_string (svalue_t *sp);

#ifdef GC_SUPPORT
extern void pg_clear_refs (void);
extern void pg_count_refs (void);
#endif

#endif /* USE_PGSQL */

#endif /* PKG_PGSQL_H__ */
