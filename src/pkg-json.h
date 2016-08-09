#ifndef PKG_JSON_H_
#define PKG_JSON_H_ 1

#include "driver.h"

#ifdef USE_JSON

#if !defined(HAS_JSONC) && !defined(HAS_JSON)
#error "pkg-json configured even though the machine doesn't support json-c."
#endif

#include "typedefs.h"

/* --- Prototypes --- */

extern svalue_t *f_json_parse (svalue_t *sp) __attribute__((nonnull(1)));
extern svalue_t *f_json_serialize (svalue_t *sp) __attribute__((nonnull(1)));

#endif /* USE_JSON */

#endif /* PKG_JSON_H_ */
