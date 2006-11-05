#ifndef PKG_TLS_H__
#define PKG_TLS_H__ 1

#include "driver.h"

#ifdef USE_TLS

#ifdef HAS_OPENSSL
#  include <openssl/ssl.h>
#elif defined(HAS_GNUTLS)
#  include <gnutls/gnutls.h>
#endif /* SSL Package */

#include "typedefs.h"

/* --- Types --- */

#ifdef HAS_OPENSSL

typedef SSL*           tls_session_t;

#elif defined(HAS_GNUTLS)

typedef gnutls_session tls_session_t;

#endif /* SSL Package */

/* --- Macros --- */

#define TLS_DEFAULT_KEYFILE  "key.pem"
#define TLS_DEFAULT_CERTFILE "cert.pem"
#define TLS_DEFAULT_TRUSTDIRECTORY "/etc/ssl/certs"

/* --- Variables --- */

extern char * tls_keyfile;
extern char * tls_certfile;
extern char * tls_trustdirectory;
extern char * tls_trustfile;
extern char * tls_crlfile;
extern char * tls_crldirectory;

/* --- Prototypes --- */

extern void tls_global_init(void);
extern void tls_verify_init (void);
extern void tls_global_deinit(void);
extern int tls_read(interactive_t *ip, char *buffer, int length);
extern int tls_write(interactive_t *ip, char *buffer, int length);
extern int tls_continue_handshake (interactive_t *ip);
extern void tls_deinit_connection (interactive_t *ip);

extern svalue_t *f_tls_refresh_certs (svalue_t *sp);
extern svalue_t *v_tls_init_connection(svalue_t *sp, int num_arg);
extern svalue_t *f_tls_deinit_connection(svalue_t *sp);
extern svalue_t *f_tls_error(svalue_t *sp);
extern svalue_t *f_tls_query_connection_state(svalue_t *sp);
extern svalue_t *f_tls_query_connection_info(svalue_t *sp);
extern svalue_t *f_tls_available (svalue_t *sp);
extern svalue_t *f_tls_check_certificate(svalue_t *sp);

#endif /* USE_TLS */

#endif /* PKG_TLS_H__ */

