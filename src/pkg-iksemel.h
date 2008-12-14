#ifndef PKG_IKSEMEL_H__
#define PKG_IKSEMEL_H__ 1

#include "driver.h"

#ifdef USE_IKSEMEL

#ifndef HAS_IKSEMEL
#error "pkg-iksemel configured even though the machine doesn't support iksemel."
#endif

/* --- Prototypes --- */

void pkg_iksemel_init();

#endif /* USE_IKSEMEL */

#endif /* PKG_IKSEMEL_H__ */

