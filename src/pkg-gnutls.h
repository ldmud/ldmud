#ifndef PKG_GNUTLS_H__
#define PKG_GNUTLS_H__ 1

#include "driver.h"

#if defined(USE_TLS) && defined(HAS_GNUTLS)
#  include <gnutls/gnutls.h>

#include "typedefs.h"

/* --- Types --- */

typedef gnutls_session tls_session_t;

#endif /* USE_TLS && !HAS_OPENSSL */

#endif /* PKG_GNUTLS_H__ */

