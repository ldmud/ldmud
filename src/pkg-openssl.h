#ifndef PKG_OPENSSL_H__
#define PKG_OPENSSL_H__ 1

#include "driver.h"

#if defined(USE_TLS) && defined(HAS_OPENSSL)
#  include <openssl/ssl.h>

#include "typedefs.h"

/* --- Types --- */

typedef SSL*           tls_session_t;
typedef const EVP_MD * digest_t;

#endif /* USE_TLS && HAS_OPENSSL */

#endif /* PKG_OPENSSL_H__ */
