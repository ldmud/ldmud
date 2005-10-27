/*------------------------------------------------------------------
 * Wrapper for the gnutls resp. OpenSSL module.
 *
#ifdef HAS_OPENSSL
 * The OpenSSL implementation was based on the easy_tls.c demo
 * and probably doesn't work, due to lack of usable documentation.
#endif
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_TLS

#include <stdio.h>

#if defined(HAS_OPENSSL)
#  include <openssl/ssl.h>
#  include <openssl/rand.h>
#  include <openssl/err.h>
#  include <openssl/x509.h>
#  include <sys/utsname.h>
#elif defined(HAS_GNUTLS)
#  include <gnutls/gnutls.h>
#endif

#include "pkg-tls.h"

#include "actions.h"
#include "array.h"
#include "comm.h"
#include "interpret.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/tls.h"

/*-------------------------------------------------------------------------*/

#define DH_BITS 1024

/*-------------------------------------------------------------------------*/
/* Variables */

char * tls_keyfile = NULL;
char * tls_certfile = NULL;
  /* The filenames of the x509 key and cert file, set by the argument
   * parser. If not set, the package will use defaults.
   */

static Bool tls_available = MY_FALSE;
  /* Set to TRUE when the TLS layer has been initialised successfully.
   */

#ifdef HAS_OPENSSL

static SSL_CTX * context = NULL;
  /* The SSL program context. */

static DH *dhe1024 = NULL;
  /* The Diffie-Hellmann parameters. */

#elif defined(HAS_GNUTLS)

static gnutls_certificate_server_credentials x509_cred;
  /* The x509 credentials. */

static gnutls_dh_params dh_params;
  /* The Diffie-Hellmann parameters */

#endif /* SSL Package */

#ifdef HAS_OPENSSL

/*-------------------------------------------------------------------------*/
static int
no_passphrase_callback (char * buf, int num, int w, void *arg)

/* OpenSSL: Empty method to hinder OpenSSL from asking for passphrases.
 */
{
    return -1;
} /* no_passphrase_callback() */

/*-------------------------------------------------------------------------*/
static Bool
set_dhe1024 (void)

/* Set the Diffie-Hellmann parameters.
 * Return MY_TRUE on success, and MY_FALSE on error.
 */

{
    int i;
    DSA *dsaparams;
    DH *dhparams;
    const char *seed[] = { ";-)  :-(  :-)  :-(  ",
                           ";-)  :-(  :-)  :-(  ",
                           "Random String no. 12",
                           ";-)  :-(  :-)  :-(  ",
                           "hackers have even mo", /* from jargon file */
                         };
    unsigned char seedbuf[20];

    if (dhe1024 != NULL)
        return MY_TRUE;

    RAND_bytes((unsigned char *) &i, sizeof i);

    /* Make sure that i is non-negative - pick one of the provided seeds */
    if (i < 0)
        i = -1;
    if (i < 0) /* happens if i == MININT */
        i = 0;

    i %= sizeof seed / sizeof seed[0];
    memcpy(seedbuf, seed[i], 20);
    dsaparams = DSA_generate_parameters(1024, seedbuf, 20, NULL, NULL, 0, NULL);

    if (dsaparams == NULL)
        return MY_FALSE;

    dhparams = DSA_dup_DH(dsaparams);
    DSA_free(dsaparams);
    if (dhparams == NULL)
        return MY_FALSE;

    dhe1024 = dhparams;

    return MY_TRUE;
} /* set_dhe1024() */
                                                  
#elif defined(HAS_GNUTLS)

/*-------------------------------------------------------------------------*/
static int
generate_dh_params (void)

/* GnuTLS: Generate Diffie Hellman parameters and store them in the global
 * <dh_params>.  They are for use with DHE kx algorithms. These should be
 * discarded and regenerated once a day, once a week or once a month. Depends
 * on the security requirements.
 *
 * tls_available must be TRUE.
 */

{
    gnutls_datum prime, generator;

    gnutls_dh_params_init( &dh_params);
    gnutls_dh_params_generate( &prime, &generator, DH_BITS);
    gnutls_dh_params_set( dh_params, prime, generator, DH_BITS);

    free( prime.data);
    free( generator.data);

    return 0;
} /* generate_dh_params() */

/*-------------------------------------------------------------------------*/
static void
initialize_tls_session (gnutls_session *session)

/* GnuTLS: Initialise a TLS <session>.
 * tls_available must be TRUE.
 */

{
    gnutls_init(session, GNUTLS_SERVER);

    /* avoid calling all the priority functions, since the defaults
     * are adequate.
     */
    gnutls_set_default_priority( *session);   
	    
    gnutls_credentials_set( *session, GNUTLS_CRD_CERTIFICATE, x509_cred);

    /* request client certificate if any.
     */
    gnutls_certificate_server_set_request( *session, GNUTLS_CERT_REQUEST);

    gnutls_dh_set_prime_bits( *session, DH_BITS);
} /* initialize_tls_session() */

#endif /* SSL Package */ 

/*-------------------------------------------------------------------------*/
void tls_global_init (void)

/* Initialise the TLS package; to be called once at program startup.
 */

{
    char * keyfile = tls_keyfile ? tls_keyfile : TLS_DEFAULT_KEYFILE;
    char * certfile = tls_certfile ? tls_certfile : TLS_DEFAULT_CERTFILE;

#ifdef HAS_OPENSSL

    printf("%s TLS: (OpenSSL) x509 keyfile '%s', certfile '%s'\n"
          , time_stamp(), keyfile, certfile);
    debug_message("%s TLS: (OpenSSL) Keyfile '%s', Certfile '%s'\n"
                 , time_stamp(), keyfile, certfile);

    SSL_load_error_strings();
    if (!SSL_library_init())
    {
	printf("%s TLS: Initialising the SSL library failed.\n"
              , time_stamp());
	debug_message("%s TLS: Initialising the SSL library failed.\n"
                     , time_stamp());
        return;
    }

    /* SSL uses the rand(3) generator from libcrypto(), which needs
     * to be seeded.
     */
    {
        struct {
            struct utsname uname;
            int uname_1;
            int uname_2;
            uid_t uid;
            uid_t euid;
            gid_t gid;
            gid_t egid;
        } data1;

        struct {
            pid_t pid;
            time_t time;
            void *stack;
        } data2;

        data1.uname_1 = uname(&data1.uname);
        data1.uname_2 = errno; /* Let's hope that uname fails randomly :-) */

        data1.uid = getuid();
        data1.euid = geteuid();
        data1.gid = getgid();
        data1.egid = getegid();

        RAND_seed((const void *)&data1, sizeof data1);

        data2.pid = getpid();
        data2.time = time(NULL);
        data2.stack = (void *)&data2;

        RAND_seed((const void *)&data2, sizeof data2);
    }

    context = SSL_CTX_new (SSLv23_server_method());
    if (!context)
    {
	printf("%s TLS: Can't get SSL context:\n"
              , time_stamp());
	debug_message("%s TLS: Can't get SSL context:\n"
                     , time_stamp());
        
        goto ssl_init_err;
    }

    SSL_CTX_set_default_passwd_cb(context, no_passphrase_callback);
    SSL_CTX_set_mode(context, SSL_MODE_ENABLE_PARTIAL_WRITE);

    if (!SSL_CTX_use_PrivateKey_file(context, keyfile, SSL_FILETYPE_PEM))
    {
	printf("%s TLS: Error setting x509 keyfile:\n"
              , time_stamp());
	debug_message("%s TLS: Error setting x509 keyfile:\n"
              , time_stamp());
        goto ssl_init_err;
    }

    if (!SSL_CTX_use_certificate_file(context, certfile, SSL_FILETYPE_PEM))
    {
	printf("%s TLS: Error setting x509 certfile:\n"
              , time_stamp());
	debug_message("%s TLS: Error setting x509 certfile:\n"
              , time_stamp());
        goto ssl_init_err;
    }

    if (!set_dhe1024()
     || !SSL_CTX_set_tmp_dh(context, dhe1024)
       )
    {
	printf("%s TLS: Error setting Diffie-Hellmann parameters:\n"
              , time_stamp());
	debug_message("%s TLS: Error setting Diffie-Hellmann parameters:\n"
              , time_stamp());
        goto ssl_init_err;
    }

    /* Avoid small subgroup attacks */
    SSL_CTX_set_options(context, SSL_OP_SINGLE_DH_USE);

    /* OpenSSL successfully initialised */
    tls_available = MY_TRUE;
    return;

    /* General error handling for the initialisation */
ssl_init_err:
    {
        unsigned long err;

        while (0 != (err = ERR_get_error()))
        {
            char * errstring = ERR_error_string(err, NULL);
            printf("%s TLS: SSL %s.\n"
                  , time_stamp(), errstring);
            debug_message("%s TLS: SSL %s.\n"
                         , time_stamp(), errstring);
        }

        if (dhe1024 != NULL)
        {
            DH_free(dhe1024);
            dhe1024 = NULL;
        }

        if (context != NULL)
        {
            SSL_CTX_free(context);
            context = NULL;
        }
        return;
    }

#elif defined(HAS_GNUTLS)

    int f;

    gnutls_global_init();

    gnutls_certificate_allocate_credentials(&x509_cred);

    printf("%s TLS: (GnuTLS) x509 keyfile '%s', certfile '%s'\n"
          , time_stamp(), keyfile, certfile);
    debug_message("%s TLS: (GnuTLS) Keyfile '%s', Certfile '%s'\n"
                 , time_stamp(), keyfile, certfile);

    f = gnutls_certificate_set_x509_key_file(x509_cred, certfile, keyfile, GNUTLS_X509_FMT_PEM);
    if (f < 0)
    {
	printf("%s TLS: Error setting x509 keyfile: %s\n"
              , time_stamp(), gnutls_strerror(f));
	debug_message("%s TLS: Error setting x509 keyfile: %s\n"
                     , time_stamp(), gnutls_strerror(f));
    }
    else
    {
        generate_dh_params();

        gnutls_certificate_set_dh_params( x509_cred, dh_params);

        tls_available = MY_TRUE;
    }
#endif /* SSL Package */

} /* tls_global_init() */

/*-------------------------------------------------------------------------*/
void
tls_global_deinit (void)

/* Clean up the TLS package on program termination.
 */

{
#ifdef HAS_OPENSSL

    if (dhe1024 != NULL)
    {
        DH_free(dhe1024);
        dhe1024 = NULL;
    }
    if (context != NULL)
    {
        SSL_CTX_free(context);
        context = NULL;
    }

#elif defined(HAS_GNUTLS)

    if (tls_available)
    {
        gnutls_certificate_free_credentials(x509_cred);
    }

    gnutls_global_deinit();
#endif /* SSL Package */
} /* tls_global_deinit() */

/*-------------------------------------------------------------------------*/
int
tls_read (interactive_t *ip, char *buffer, int length)

/* Read up to <length> bytes data for the TLS connection of <ip>
 * and store it in <buffer>.
 * Return then number of bytes read, or a negative number if an error
 * occured.
 */

{
    int ret;

#ifdef HAS_OPENSSL

    ret = SSL_read(ip->tls_session, buffer, length);
    if (ret == 0)
    {
        tls_deinit_connection(ip);
    }
    else if (ret < 0)
    {
        ret = SSL_get_error(ip->tls_session, ret);
	debug_message("%s TLS: Received corrupted data (%d). "
                      "Closing the connection.\n"
                     , time_stamp(), ret);
        tls_deinit_connection(ip);
    }

#elif defined(HAS_GNUTLS)

    ret = gnutls_record_recv( ip->tls_session, buffer, length);

    if (ret == 0)
    {
        tls_deinit_connection(ip);
    }
    else if (ret < 0)
    {
	debug_message("%s TLS: Received corrupted data (%d). "
                      "Closing the connection.\n"
                     , time_stamp(), ret);
	gnutls_bye(ip->tls_session, GNUTLS_SHUT_WR);
	gnutls_deinit(ip->tls_session);
	ip->tls_inited = MY_FALSE;
    }
#endif /* SSL Package */

    return ret;
} /* tls_read() */

/*-------------------------------------------------------------------------*/
int
tls_write (interactive_t *ip, char *buffer, int length)

/* Write <length> bytes from <buffer> to the TLS connection of <ip>
 * Return the number of bytes written, or a negative number if an error
 * occured.
 */

{
    int ret;

#ifdef HAS_OPENSSL

    ret = SSL_write(ip->tls_session, buffer, length);
    if (ret == 0)
    {
        tls_deinit_connection(ip);
    }
    else if (ret < 0)
    {
        ret = SSL_get_error(ip->tls_session, ret);
	debug_message("%s TLS: Sending data failed (%d). "
                      "Closing the connection.\n"
                     , time_stamp(), ret);
        tls_deinit_connection(ip);
    }

#elif defined(HAS_GNUTLS)

    ret = gnutls_record_send( ip->tls_session, buffer, length );
    if (ret < 0)
    {
        tls_deinit_connection(ip);
    }
#endif /* SSL Package */

    return ret;
} /* tls_write() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_init_connection (svalue_t *sp)

/* EFUN tls_init_connection()
 *
 *      int tls_init_connection(object ob)
 *
 * tls_init_connection() tries to start a TLS secured connection to the
 * interactive object <ob> (or this_object() if <ob> is not given).  Returns
 * an error (< 0) if not successful. Try tls_error() to get an useful
 * description of the error.
 */

{
    long ret;
    interactive_t *ip;

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        error("Bad arg 1 to tls_init_connection(): "
              "object not interactive.\n");

    if (!tls_available)
        error("tls_init_connection(): TLS layer hasn't been initialized.\n");

    if (ip->tls_inited)
        error("tls_init_connection(): Interactive already has a secure "
              "connection.\n");

    free_svalue(sp);

    {
        object_t * save_c_g = command_giver;
        command_giver = sp->u.ob;
        add_message(message_flush);
        command_giver = save_c_g;
    }

#ifdef HAS_OPENSSL

    ret = 0;

    do {
        int n;

        SSL * session = SSL_new(context);

        if (session == NULL)
        {
            ret = - ERR_get_error();
            break;
        }

        if (!SSL_set_fd(session, ip->socket))
        {
            SSL_free(session);
            ret = - ERR_get_error();
            break;
        }
        
        do {
            if ((n = SSL_do_handshake(session)) < 0)
                ret = SSL_get_error(session, n);
            else
                ret = 0;
        } while (SSL_ERROR_WANT_READ == ret || SSL_ERROR_WANT_WRITE == ret);

        if (ret)
        {
            ret = -ret;
            SSL_free(session);
            break;
        }

        /* TODO: Check SSL_in_init() at this place? */
        ip->tls_session = session;
        ip->tls_inited = MY_TRUE;
    } while(0);

    put_number(sp, ret);

#elif defined(HAS_GNUTLS)

    initialize_tls_session(&ip->tls_session);
    gnutls_transport_set_ptr(ip->tls_session, (gnutls_transport_ptr)(ip->socket));
    do {
        ret = gnutls_handshake(ip->tls_session);
    } while (GNUTLS_E_AGAIN == ret || GNUTLS_E_INTERRUPTED == ret);
    if (ret < 0)
    {
	gnutls_deinit(ip->tls_session);
	ip->tls_inited = MY_FALSE;
	put_number(sp, ret);
    }
    else
    {
	ip->tls_inited = MY_TRUE;
	put_number(sp, 0);
    }

#endif /* SSL Package */

    return sp;
} /* f_tls_init_connection() */

/*-------------------------------------------------------------------------*/
void
tls_deinit_connection (interactive_t *ip)

/* Close the TLS connection for the interactive <ip> if there is one.
 */

{
#ifdef HAS_OPENSSL

    if (ip->tls_inited)
    {
        SSL_shutdown(ip->tls_session);
        SSL_free(ip->tls_session);
        ip->tls_session = NULL;
    }

#elif defined(HAS_GNUTLS)

    if (ip->tls_inited)
    {
        gnutls_bye( ip->tls_session, GNUTLS_SHUT_WR);
        gnutls_deinit(ip->tls_session);
    }

#endif /* SSL Package */

    ip->tls_inited = MY_FALSE;
} /* tls_deinit_connection() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_deinit_connection(svalue_t *sp)

/* EFUN tls_deinit_connection()
 *
 *      void tls_deinit_connection(object ob)
 *
 * tls_deinit_connection() shuts down a TLS connection to the interactive
 * object <ob> (or this_object() if <ob> is not given) but the connection is
 * not closed.
 */

{
    interactive_t *ip;

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        error("Bad arg 1 to tls_deinit_connection(): "
              "object not interactive.\n");

    tls_deinit_connection(ip);

    free_svalue(sp);
    put_number(sp, 1);
    return sp;
} /* f_tls_deinit_connection() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_error(svalue_t *sp)

/* EFUN tls_error()
 *
 *     string tls_error(int errorno)
 *
 * tls_error() returns a string describing the error behind the
 * error number <errorno>.
 */

{
    string_t *s;
    const char *text;
    int err = sp->u.number;

#ifdef HAS_OPENSSL

    text = ERR_error_string(-err, NULL);

#elif defined(HAS_GNUTLS)

    text = gnutls_strerror(err);

#endif /* SSL Package */

    if (text)
    {
        memsafe(s = new_mstring(text), strlen(text), "tls_error()");
        free_svalue(sp);
        put_string(sp, s);
    }
    else
    {
        free_svalue(sp);
        put_number(sp, 0);
    }

    return sp;
} /* f_tls_error() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_query_connection_state (svalue_t *sp)

/* EFUN tls_query_connection_state()
 *
 *      int tls_query_connection_state(object ob)
 *
 * tls_query_connection_state() returns TRUE if <ob>'s connection
 * is TLS secured, FALSE otherwise.
 */

{
    interactive_t *ip;

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        error("Bad arg 1 to tls_deinit_connection_state(): "
              "object not interactive.\n");
    free_svalue(sp);
    put_number(sp, ip->tls_inited);
    return sp;
} /* f_tls_query_connection_state() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_query_connection_info (svalue_t *sp)

/* EFUN tls_query_connection_info()
 *
 *
 *       #include <sys/ tls.h>
 *       int *tls_query_connection_info (object ob)
 *
 * If <ob> does not have a TLS connection, the efun returns 0.
 *
 * If <ob> has a TLS connection, tls_query_connection_info() returns an array
 * that contains some parameters of <ob>'s connection:
 *
 *    int|string [TLS_CIPHER]: the cipher used
 *    int        [TLS_COMP]:   the compression used
 *    int        [TLS_KX]:     the key-exchange used
 *    int        [TLS_MAC]:    the digest algorithm used
 *    int|string [TLS_PROT]:   the protocol used
 *
 * To translate these numbers into strings, <tls.h> offers a number of macros:
 *
 *    TLS_xxx_TABLE: a literal array of strings describing the value in
 *        question.
 *    TLS_xxx_NAME(x): a macro translating the numeric result value into a
 *        string.
 *
 *    xxx: CIPHER, COMP, KX, MAC, PROT
 */

{
    interactive_t *ip;

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        error("Bad arg 1 to tls_query_connection_info(): "
              "object not interactive.\n");

    free_svalue(sp);

    if (ip->tls_inited)
    {
        vector_t * rc;
        rc = allocate_array(TLS_INFO_MAX);
#ifdef HAS_OPENSSL
        put_c_string(&(rc->item[TLS_CIPHER])
                    , SSL_get_cipher(ip->tls_session));
	put_number(&(rc->item[TLS_COMP]), 0);
	put_number(&(rc->item[TLS_KX]), 0);
	put_number(&(rc->item[TLS_MAC]), 0);
        put_c_string(&(rc->item[TLS_PROT])
                    , SSL_get_version(ip->tls_session));
#elif defined(HAS_GNUTLS)
        put_number(&(rc->item[TLS_CIPHER])
                  , gnutls_cipher_get(ip->tls_session));
	put_number(&(rc->item[TLS_COMP])
                  , gnutls_compression_get(ip->tls_session));
	put_number(&(rc->item[TLS_KX])
                  , gnutls_kx_get(ip->tls_session));
	put_number(&(rc->item[TLS_MAC])
                  , gnutls_mac_get(ip->tls_session));
	put_number(&(rc->item[TLS_PROT])
                  , gnutls_protocol_get_version(ip->tls_session));
#endif /* SSL Package */
        put_array(sp, rc);
    }
    else
    {
        put_number(sp, 0);
    }

    return sp;
} /* tls_query_connection_info() */

#endif /* USE_TLS */

/***************************************************************************/
