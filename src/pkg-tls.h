#ifndef PKG_TLS_H__
#define PKG_TLS_H__ 1

#include "driver.h"

#ifdef USE_TLS

#if defined(HAS_OPENSSL) && defined(HAS_GNUTLS)
#error Both, OpenSSL and GnuTLS enabled.
#endif
#if defined(HAS_OPENSSL) && defined(USE_GCRYPT) && defined(HAS_GCRYPT)
#error Both, OpenSSL and GCypt enabled.
#endif

#include "pkg-openssl.h"
#include "pkg-gnutls.h"
#include "typedefs.h"

/* --- Macros --- */

/* Number of bits for the Diffie Hellmann parameters. */
#define DH_BITS 1024

/* --- Variables --- */

extern char * tls_keyfile;
extern char * tls_certfile;
extern char * tls_trustdirectory;
extern char * tls_trustfile;
extern char * tls_crlfile;
extern char * tls_crldirectory;

/* --- Prototypes of pkg-tls.c --- */

extern int tls_continue_handshake (interactive_t *ip);
extern svalue_t *f_tls_refresh_certs (svalue_t *sp);
extern svalue_t *v_tls_init_connection(svalue_t *sp, int num_arg);
extern svalue_t *f_tls_deinit_connection(svalue_t *sp);
extern svalue_t *f_tls_error(svalue_t *sp);
extern svalue_t *f_tls_query_connection_state(svalue_t *sp);
extern svalue_t *f_tls_query_connection_info(svalue_t *sp);
extern svalue_t *f_tls_available (svalue_t *sp);
extern svalue_t *f_tls_check_certificate(svalue_t *sp);
extern svalue_t *v_hash(svalue_t *sp, int num_arg);
extern svalue_t *f_hmac(svalue_t *sp);

/* --- Prototypes for TLS packages --- */

extern void tls_global_init(void);
extern void tls_verify_init (void);
extern void tls_global_deinit(void);
extern int tls_read(interactive_t *ip, char *buffer, int length);
extern int tls_write(interactive_t *ip, char *buffer, int length);
extern int tls_do_handshake (interactive_t *ip);
extern int tls_init_connection (interactive_t *ip);
extern void tls_deinit_connection (interactive_t *ip);
extern const char *tls_error(int err);
extern vector_t *tls_query_connection_info(interactive_t *ip);
extern vector_t *tls_check_certificate(interactive_t *ip, Bool more);
extern Bool tls_available ();

#endif /* USE_TLS */

#endif /* PKG_TLS_H__ */

