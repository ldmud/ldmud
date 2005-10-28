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

/*-------------------------------------------------------------------------*/
static void *
tls_xalloc (size_t size)

/* Wrapper function so that (gnu)tls will use the driver's allocator.
 * The wrapper is required as 'xalloc' itself is a macro.
 */

{
    return xalloc(size);
} /* tls_xalloc() */

/*-------------------------------------------------------------------------*/
static void *
tls_rexalloc (void *old, size_t size)

/* Wrapper function so that (gnu)tls will use the driver's allocator.
 * The wrapper is required as 'rexalloc' itself is a macro.
 */

{
    return rexalloc(old, size);
} /* tls_rexalloc() */

/*-------------------------------------------------------------------------*/
static void
tls_xfree (void *p)

/* Wrapper function so that (gnu)tls will use the driver's allocator.
 * The wrapper is not exactly required for xfree(),  but it keeps things
 * consistent.
 */

{
    return xfree(p);
} /* tls_xfree() */

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

  
    /* In order to be able to identify gnutls allocations as such, we redirect
     * all allocations through the driver's allocator. The wrapper functions
     * make sure that the allocations are annotated properly with this source
     * file.
     */
    gnutls_global_set_mem_functions(tls_xalloc,
                                    tls_xalloc,
                                    NULL,
                                    tls_rexalloc,
                                    tls_xfree);

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
	ip->tls_status = TLS_INACTIVE;
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
int
tls_continue_handshake (interactive_t *ip)

/* Continue the TLS initialisation handshake for interactive <ip>.
 * Return a negative error code if the connection can not be set up.
 * Return 0 if the connection is still begin set up.
 * Return 1 if the connection is now active (or if no secure connection
 * had been requested).
 *
 * If a callback is set for <ip> and connection comes out of the handshaking
 * state, it will be called with the result code.
 */

{
    int ret;

    if (ip->tls_status != TLS_HANDSHAKING)
        return 1;

    ret = 1;

#ifdef HAS_OPENSSL

    {
        int n;

        if ((n = SSL_do_handshake(ip->tls_session)) < 0)
            ret = SSL_get_error(ip->tls_session, n);
        else
            ret = 0;

        if (ret != SSL_ERROR_WANT_READ && ret != SSL_ERROR_WANT_WRITE)
        {
            if (ret != 0)
            {
                /* Setup failed */
                SSL_free(ip->tls_session);
                ip->tls_session = NULL;
                ip->tls_status = TLS_INACTIVE;
                ret = - ret;
            }
            else
            {
                /* Setup finished */
                /* TODO: Check SSL_in_init() at this place? */
                ip->tls_status = TLS_ACTIVE;
                ret = 1;
            }
        }
        else
            ret = 0;
    }

#elif defined(HAS_GNUTLS)

    ret = gnutls_handshake(ip->tls_session);

    if (ret != GNUTLS_E_AGAIN && ret != GNUTLS_E_INTERRUPTED)
    {
        if (ret < 0)
        {
            /* Setup failed */
            gnutls_deinit(ip->tls_session);
            ip->tls_session = NULL;
            ip->tls_status = TLS_INACTIVE;
        }
        else
        {
            /* Setup finished */
            ip->tls_status = TLS_ACTIVE;
            ret = 1;
        }
    }
    else
        ret = 0;

#endif /* SSL Package */

    /* If the connection is no longer in handshake, execute the callback */
    if (ip->tls_cb != NULL && ret != 0)
    {
        callback_t *cb = ip->tls_cb;

        push_number(inter_sp, ret ? ret : 0);
        push_ref_object(inter_sp, ip->ob, "tls_handshake");

        (void)apply_callback(cb, 2);

        free_callback(cb);
        xfree(cb);
        ip->tls_cb = NULL;
    }

    return ret;
} /* tls_continue_handshake() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_tls_init_connection (svalue_t *sp, int num_arg)

/* EFUN tls_init_connection()
 *
 *   int tls_init_connection(object ob)
 *   int tls_init_connection(object ob, string fun, string|object fob, mixed extra...)
 *   int tls_init_connection(object ob, closure fun, mixed extra...)
 *
 * tls_init_connection() tries to start a TLS secured connection to the
 * interactive object <ob> (or this_object() if <ob> is not given).
 * Result:
 *   errorcode < 0: unsuccessful, use tls_error() to get an useful
 *                  description of the error
 *      number > 0: the secure connection is still being set up in the
 *                   background
 *     number == 0: the secure connection is active.
 *
 * If the callback <fun>/<fun>:<fob> is specified, it will be called once
 * the fate of the secure connection has been determined. The first argument
 * will be the return code from the handshake (errorcode < 0 on failure,
 * or 0 on success), followed by the interactive object <ob> and any <extra>
 * arguments.
 */

{
    svalue_t * argp = sp - num_arg + 1;
    long ret;
    object_t * obj;
    interactive_t *ip;

    if (!tls_available)
        error("tls_init_connection(): TLS layer hasn't been initialized.\n");

    if (num_arg > 0)
    {
        obj = argp->u.ob;
        put_number(argp, 0); /* remove obj reference from the stack */
    }
    else
    {
        obj = ref_object(current_object, "tls_init_connection");
    }

    if (!O_SET_INTERACTIVE(ip, obj))
    {
        free_object(obj, "tls_init_connection");
        error("Bad arg 1 to tls_init_connection(): "
              "object not interactive.\n");
    }

    free_object(obj, "tls_init_connection");
      /* ip has another reference to obj, so this is safe to do */

    if (ip->tls_status != TLS_INACTIVE)
        error("tls_init_connection(): Interactive already has a secure "
              "connection.\n");

    /* Extract the callback information from the stack */
    if (num_arg > 1)
    {
        /* Extract the callback information from the stack */
        int error_index;
        callback_t * cb;

        inter_sp = sp;

        memsafe(cb = xalloc(sizeof *cb) , sizeof *cb , "callback structure");

        assign_eval_cost();

        error_index = setup_efun_callback(cb, argp+1, num_arg-1);

        if (error_index >= 0)
        {
            /* The callback values have already been removed. */
            
            xfree(cb);
            inter_sp = sp = argp;
            vefun_bad_arg(error_index+2, argp);
            /* NOTREACHED */
            return argp;
        }

        /* Callback creation successful */
        ip->tls_cb = cb;

        inter_sp = sp = argp;
    }

    /* Flush the connection */

    {
        object_t * save_c_g = command_giver;
        command_giver = obj;
        add_message(message_flush);
        command_giver = save_c_g;
    }

    ret = 0;

    do {

#ifdef HAS_OPENSSL

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

        SSL_set_accept_state(session);
        ip->tls_session = session;
        
#elif defined(HAS_GNUTLS)

        initialize_tls_session(&ip->tls_session);
        gnutls_transport_set_ptr(ip->tls_session, (gnutls_transport_ptr)(ip->socket));

#endif /* SSL Package */

        ip->tls_status = TLS_HANDSHAKING;
        ret = tls_continue_handshake(ip);

        /* Adjust the return value of tls_continue_handshake() */
        if (ret == 1)
            ret = 0;
        else if (ret == 0)
            ret = 1;

    } while(0);

    put_number(sp, ret);
    return sp;
} /* f_tls_init_connection() */

/*-------------------------------------------------------------------------*/
void
tls_deinit_connection (interactive_t *ip)

/* Close the TLS connection for the interactive <ip> if there is one.
 */

{
#ifdef HAS_OPENSSL

    if (ip->tls_status != TLS_INACTIVE)
    {
        SSL_shutdown(ip->tls_session);
        SSL_free(ip->tls_session);
        ip->tls_session = NULL;
    }

#elif defined(HAS_GNUTLS)

    if (ip->tls_status != TLS_INACTIVE)
    {
        gnutls_bye( ip->tls_session, GNUTLS_SHUT_WR);
        gnutls_deinit(ip->tls_session);
    }

#endif /* SSL Package */

    if (ip->tls_cb != NULL)
    {
        free_callback(ip->tls_cb);
        xfree(ip->tls_cb);
        ip->tls_cb = NULL;
    }
    ip->tls_status = TLS_INACTIVE;
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
 * tls_query_connection_state() returns a positive number if <ob>'s connection
 * is TLS secured, 0 if it's unsecured, and a negative number if the
 * TLS connection setup is still being set-up.
 * Returns 0 for non-interactive objects.
 */

{
    interactive_t *ip;
    Bool rc;

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        rc = 0;
    else if (ip->tls_status == TLS_HANDSHAKING)
        rc = -1;
    else if (ip->tls_status == TLS_INACTIVE)
        rc = 0;
    else
        rc = 1;
    free_svalue(sp);
    put_number(sp, rc);
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
 * If <ob> does not have a TLS connection, or if the TLS connection is
 * still being set up, the efun returns 0.
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

    if (O_SET_INTERACTIVE(ip, sp->u.ob) && ip->tls_status == TLS_ACTIVE)
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
        free_svalue(sp);
        put_array(sp, rc);
    }
    else
    {
        free_svalue(sp);
        put_number(sp, 0);
    }

    return sp;
} /* tls_query_connection_info() */

#endif /* USE_TLS */

/***************************************************************************/
