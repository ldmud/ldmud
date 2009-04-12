#ifndef PKG_GCRYPT_H__
#define PKG_GCRYPT_H__ 1

#include "driver.h"

#ifdef USE_GCRYPT

#ifndef HAS_GCRYPT
#error "pkg-gcrypt configured even though the machine doesn't support GCrypt."
#endif

#include "typedefs.h"

/* --- Typedefs --- */
typedef int digest_t;

/* --- Prototypes --- */
Bool pkg_gcrypt_init();

#endif /* USE_GCRYPT */

#endif /* PKG_GCRYPT_H__ */
