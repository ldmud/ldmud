#ifndef PKG_ALISTS_H__
#define PKG_ALISTS_H__ 1

#include "driver.h"
#include "typedefs.h"

#ifdef USE_ALISTS

extern vector_t *order_alist (svalue_t *inlists, int listnum, Bool reuse);
extern svalue_t *v_assoc(svalue_t *sp, int num_arg);
extern svalue_t *f_intersect_alist(svalue_t *sp);
extern svalue_t *v_insert_alist(svalue_t *sp, int num_arg);
extern svalue_t *v_order_alist(svalue_t *sp, int num_arg);

#endif /* USE_ALISTS */

#endif /* PKG_ALISTS_H__ */
