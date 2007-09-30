#ifndef PKG_IDNA_H_
#define PKG_IDNA_H_ 1

#include "driver.h"

#ifdef HAS_IDN

#include "typedefs.h"

/* --- Prototypes --- */

extern svalue_t * f_idna_to_ascii(svalue_t *sp);
extern svalue_t * f_idna_to_unicode(svalue_t *sp);
extern svalue_t * f_idna_stringprep(svalue_t *sp); 

#endif /* HAS_IDN */

#endif /* PKG_IDNA_H_ */
