#ifndef PKG_OPENSSL_H__
#define PKG_OPENSSL_H__ 1

#include "driver.h"

#if defined(USE_TLS) && defined(HAS_OPENSSL)
#  include <openssl/ssl.h>

/* --- Types --- */

typedef SSL*           tls_session_t;

extern svalue_t *v_hash(svalue_t *sp, int num_arg);
extern svalue_t *f_hmac(svalue_t *sp);

#endif /* USE_TLS && HAS_OPENSSL */

#endif /* PKG_OPENSSL_H__ */
