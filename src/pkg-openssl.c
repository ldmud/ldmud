/*------------------------------------------------------------------
 * Wrapper for the OpenSSL module.
 *------------------------------------------------------------------
 */

#include "driver.h"
#include "machine.h"

#if defined(USE_TLS) && defined(HAS_OPENSSL)

#include <stdio.h>
#include <assert.h>

#include <openssl/ssl.h>
#include <openssl/rand.h>
#include <openssl/err.h>
#include <openssl/x509.h>
#include <openssl/x509v3.h>
#include <sys/utsname.h>
#include <openssl/opensslconf.h>

#include <openssl/sha.h>
#include <openssl/md5.h>
#include <openssl/ripemd.h>

#include <openssl/hmac.h>
#include <openssl/evp.h>

#include "pkg-tls.h"

#include "actions.h"
#include "array.h"
#include "comm.h"
#include "interpret.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "sha1.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/tls.h"

// add some entropy by calling RAND_poll() every 15-45min.
#define PRNG_RESEED_PERIOD 1800

/*-------------------------------------------------------------------------*/
/* Variables */

static Bool tls_is_available = MY_FALSE;
  /* Set to TRUE when the TLS layer has been initialised successfully.
   */

static SSL_CTX * context = NULL;
  /* The SSL program context. */

static DH *dhe1024 = NULL;
  /* The Diffie-Hellmann parameters. */

/*-------------------------------------------------------------------------*/
static int
no_passphrase_callback (char * buf, int num, int w, void *arg)

/* OpenSSL: Empty method to hinder OpenSSL from asking for passphrases.
 */
{
    return -1;
} /* no_passphrase_callback() */

static void *
openssl_malloc (size_t size)
/*
 * Wrapper function for using our own allocator in openssl.
 */
{
    return pxalloc(size);
}

static void
openssl_free (void * ptr) __attribute__((nonnull));
static void
openssl_free (void * ptr)
/*
 * Wrapper function for using our own allocator in openssl.
 */
{
    pfree(ptr);
}

static void *
openssl_realloc (void * ptr, size_t size) __attribute__((nonnull));
static void *
openssl_realloc (void * ptr, size_t size)
/*
 * Wrapper function for using our own allocator in openssl.
 */
{
    return prexalloc(ptr, size);
}

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

/*-------------------------------------------------------------------------*/
static int
tls_verify_callback(int preverify_ok, X509_STORE_CTX *ctx) __attribute__((nonnull));
static int
tls_verify_callback(int preverify_ok, X509_STORE_CTX *ctx) 

/* This function will be called if the client did present a certificate
 * always returns MY_TRUE so that the handshake will succeed
 * and the verification status can later be checked on mudlib level
 * see also: SSL_set_verify(3)
 */

{
    if (d_flag)
    {
        char buf[512];
        printf("%s tls_verify_callback(%d, ...)\n", time_stamp(), preverify_ok);

        X509_NAME_oneline(X509_get_issuer_name(ctx->current_cert), buf, sizeof buf);
        printf("depth %d: %s\n", X509_STORE_CTX_get_error_depth(ctx), buf);
    }
    return MY_TRUE;
} /* tls_verify_callback() */

/*-------------------------------------------------------------------------*/
static INLINE void
tls_add_entropy()
/* Always add the current time to the openssl entropy pool - as little as it
 * may be. From time to time, call RAND_poll() to add some entropy from
 * external sources.
 */
{
    static time_t nextpoll = 0;
    struct timeval tv;
#ifdef HAVE_GETTIMEOFDAY
    gettimeofday(&tv, NULL);
#else
    tv.tv_sec = time(NULL);
#endif
    RAND_add(&tv, sizeof(tv), 0.0); // under-estimate entropy.

   /* OpenSSL 0.9.6 adds a RAND_poll function that knows about more kinds of
    * entropy than we do. We use that, but not more often than once per
    * PRNG_RESEED_PERIOD/2 + random(PRNG_RESEED_PERIOD) seconds.
    */
    if (nextpoll < tv.tv_sec)
    {
        unsigned char rbyte;
        RAND_pseudo_bytes(&rbyte, 1);
        nextpoll = tv.tv_sec + PRNG_RESEED_PERIOD/2
                   + ((PRNG_RESEED_PERIOD * rbyte) / UCHAR_MAX);
        if (RAND_poll() == 0)
            debug_message("%s TLS: Warning: Reseeding the PRNG with "
                          "RAND_poll() failed\n", time_stamp());
    }
} /* tls_add_entropy */

/*-------------------------------------------------------------------------*/
void
tls_verify_init (void)

/* initialize or reinitialize tls certificate storage and revocation lists
 */
{
    STACK_OF(X509_NAME) *stack = NULL;

    if (tls_trustfile != NULL && tls_trustdirectory != NULL)
    {
        printf("%s TLS: (OpenSSL) trusted x509 certificates from '%s' and directory '%s'.\n"
              , time_stamp(), tls_trustfile, tls_trustdirectory);
        debug_message("%s TLS: (OpenSSL) trusted x509 certificates from '%s' and directory '%s'.\n"
                     , time_stamp(), tls_trustfile, tls_trustdirectory);
    }
    else if (tls_trustfile != NULL)
    {
        printf("%s TLS: (OpenSSL) trusted x509 certificates from '%s'.\n"
              , time_stamp(), tls_trustfile);
        debug_message("%s TLS: (OpenSSL) trusted x509 certificates from '%s'.\n"
                     , time_stamp(), tls_trustfile);
    }
    else if (tls_trustdirectory != NULL)
    {
        printf("%s TLS: (OpenSSL) trusted x509 certificates from directory '%s'.\n"
              , time_stamp(), tls_trustdirectory);
        debug_message("%s TLS: (OpenSSL) trusted x509 certificates from directory '%s'.\n"
                     , time_stamp(), tls_trustdirectory);
    }
    else
    {
        printf("%s TLS: (OpenSSL) Trusted x509 certificates locations not specified.\n"
              , time_stamp());
        debug_message("%s TLS: (OpenSSL) trusted x509 certificates locations not specified.\n"
                     , time_stamp());
    }

    if (tls_crlfile != NULL || tls_crldirectory != NULL)
    {
        X509_STORE *store = X509_STORE_new();
        if (store != NULL)
        {
            if (tls_crlfile != NULL)
            {
                X509_LOOKUP *lookup = X509_STORE_add_lookup(store, X509_LOOKUP_file());
                if (lookup != NULL) 
                    X509_LOOKUP_load_file(lookup, tls_crlfile, X509_FILETYPE_PEM);
            }
            if (tls_crldirectory != NULL)
            {
                X509_LOOKUP *lookup = X509_STORE_add_lookup(store, X509_LOOKUP_hash_dir());
                if (lookup != NULL) 
                    X509_LOOKUP_add_dir(lookup, tls_crldirectory, X509_FILETYPE_PEM);
            }
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
            X509_STORE_set_flags(store, X509_V_FLAG_CRL_CHECK | X509_V_FLAG_CRL_CHECK_ALL);
            SSL_CTX_set_cert_store(context, store);
            if (tls_crlfile != NULL && tls_crldirectory != NULL)
            {
                printf("%s TLS: (OpenSSL) CRLs from '%s' and '%s'.\n"
                       , time_stamp(), tls_crlfile, tls_crldirectory);
                debug_message("%s TLS: (OpenSSL) CRLs from '%s' and '%s'.\n"
                       , time_stamp(), tls_crlfile, tls_crldirectory);
            }
            else if (tls_crlfile != NULL)
            {
                printf("%s TLS: (OpenSSL) CRLs from '%s'.\n"
                       , time_stamp(), tls_crlfile);
                debug_message("%s TLS: (OpenSSL) CRLs from '%s'.\n"
                       , time_stamp(), tls_crlfile);
            }
            else if (tls_crldirectory != NULL)
            {
                printf("%s TLS: (OpenSSL) CRLs from '%s'.\n"
                       , time_stamp(), tls_crldirectory);
                debug_message("%s TLS: (OpenSSL) CRLs from '%s'.\n"
                       , time_stamp(), tls_crldirectory);
            }
            else
            {
                printf("%s TLS: (OpenSSL) CRL checking disabled.\n"
                       , time_stamp());
                debug_message("%s TLS: (OpenSSL) CRL checking disabled.\n"
                       , time_stamp());
            }
#else
            printf("%s TLS: Warning: Your OpenSSL version does not support "
                   "Certificate revocation list checking\n"
                  , time_stamp());
            debug_message("%s TLS: Warning: Your OpenSSL version does not "
                          "support Certificate revocation list checking\n"
                  , time_stamp());
#endif
        }
        else
        {
            printf("%s TLS: Warning: There was a problem getting the "
                   "storage context from OpenSSL. Certificate revocation "
                   "list checking is not enabled.\n"
                  , time_stamp());
            debug_message("%s TLS: Warning: There was a problem getting the "
                          "storage context from OpenSSL. Certificate revocation "
                          "list checking is not enabled.\n"
                  , time_stamp());
        }
    }

    if (!SSL_CTX_load_verify_locations(context, tls_trustfile, tls_trustdirectory))
    {
        printf("%s TLS: Error preparing x509 verification certificates\n",
               time_stamp());
        debug_message("%s TLS: Error preparing x509 verification certificates\n",
               time_stamp());
    }
    if (tls_trustfile != NULL)
    {
        stack = SSL_load_client_CA_file(tls_trustfile);
        SSL_CTX_set_client_CA_list(context, stack);
    }
    else
    {
        stack = SSL_CTX_get_client_CA_list(context);
        if (stack == NULL)
        {
            stack = sk_X509_NAME_new_null();
            SSL_CTX_set_client_CA_list(context, stack);
        }
    }

    if (tls_trustdirectory != NULL && stack != NULL)
    {
        SSL_add_dir_cert_subjects_to_stack(stack, tls_trustdirectory);
    }
}

/*-------------------------------------------------------------------------*/
void
tls_global_init (void)

/* Initialise the TLS package; to be called once at program startup.
 */

{
    if (tls_keyfile == NULL)
    {
        printf("%s TLS deactivated.\n", time_stamp());
        return;
    }

    printf("%s TLS: (OpenSSL) x509 keyfile '%s', certfile '%s'\n"
          , time_stamp(), tls_keyfile, tls_certfile);
    debug_message("%s TLS: (OpenSSL) Keyfile '%s', Certfile '%s'\n"
                 , time_stamp(), tls_keyfile, tls_certfile);

    // Register pointers to our own allocator functions before calling any
    // other function from OpenSSL.
    CRYPTO_set_mem_functions(openssl_malloc, openssl_realloc, openssl_free);
    
    SSL_load_error_strings();
    ERR_load_BIO_strings();
    if (!SSL_library_init())
    {
        printf("%s TLS: Initialising the SSL library failed.\n"
               , time_stamp());
        debug_message("%s TLS: Initialising the SSL library failed.\n"
                      , time_stamp());
        return;
    }

    /* We deliberately DO NOT seed the pseudo random number generator from
     * OpenSSL. We don't have any reliable source of entropy available. OpenSSL
     * will use e.g. /dev/urandom, /dev/random, /dev/srandom, the EGD or
     * CryptoAPI automagically for adding entropy to its state. This is _way_
     * better than anything we can do.
     * If OpenSSL fails to get enough entropy by the built-in methods, it will
     * fail with an error message. On the other hand, if we add now some entropy
     * and overestimate its quality (easy for e.g. uninitialized memory), we
     * will disable this safeguard in OpenSSL, because it thinks, it has enough
     * entropy. We believe it better, that OpenSSL fails completely than working
     * with bad random numbers.
     * So, on platforms without strong source of entropy, which is automatically
     * used by OpenSSL, it will fail completely. Should this happen, we will
     * deal with the problem. Please file a bug report.
     * Background information: http://mantis.bearnip.com/view.php?id=678
     *
     * Note: in former times, we used to add uname, uid, euid, gid, egid, the
     * PID, an address from the stack and the current time to the entropy pool.
     * All this is not really random and contains very little (if any at all)
     * entropy. PID, UID and time is used automatically by OpenSSL and was
     * redundant. Additionally, there was some uninitialized memory used, which
     * was initialized to zero on some systems.
     */

    // RAND_status() will call RAND_poll() and seed the PRNG.
    if (RAND_status() != 1)
    {
        printf("%s TLS: OpenSSL PRNG not seeded - TLS not available. Please file a bug report.\n"
               , time_stamp());
        debug_message("%s TLS: OpenSSL PRNG not seeded - TLS not available. Please file a bug report.\n"
                      , time_stamp());
        return;
    }

    context = SSL_CTX_new (SSLv23_method());
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
    SSL_CTX_set_session_id_context(context, (unsigned char*) "ldmud", 5);

    if (!SSL_CTX_use_PrivateKey_file(context, tls_keyfile, SSL_FILETYPE_PEM))
    {
        printf("%s TLS: Error setting x509 keyfile:\n"
              , time_stamp());
        debug_message("%s TLS: Error setting x509 keyfile:\n"
              , time_stamp());
        goto ssl_init_err;
    }

    if (!SSL_CTX_use_certificate_file(context, tls_certfile, SSL_FILETYPE_PEM))
    {
        printf("%s TLS: Error setting x509 certfile:\n"
              , time_stamp());
        debug_message("%s TLS: Error setting x509 certfile:\n"
              , time_stamp());
        goto ssl_init_err;
    }
    tls_verify_init();

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

    // Disallow SSLv2 connections (only allow SSLv3 and TLSv1 connections.)
    SSL_CTX_set_options(context, SSL_OP_NO_SSLv2);

    // OpenSSL must accept a different buffer address for retries after
    // SSL_write() returned -1 with SSL_WANT_READ/_WRITE, because we transfer
    // the content to a queue of buffers before we try again (#737).
    SSL_CTX_set_mode(context, SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER);
#ifdef SSL_MODE_RELEASE_BUFFERS
    // OpenSSL 1.0.0a can save some memory for idle SSL connections
    SSL_CTX_set_mode(context, SSL_MODE_RELEASE_BUFFERS);
#endif

    /* OpenSSL successfully initialised */
    tls_is_available = MY_TRUE;
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

} /* tls_global_init() */

/*-------------------------------------------------------------------------*/
void
tls_global_deinit (void)

/* Clean up the TLS package on program termination.
 */

{
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

    tls_is_available = MY_FALSE;

} /* tls_global_deinit() */

/*-------------------------------------------------------------------------*/
static INLINE void
dump_openssl_error_stack()
{
#ifdef DEBUG
    unsigned long lerr;
    // dump the error stack of openssl
    debug_message("%s Dumping OpenSSL error stack...\n",time_stamp());
    while ( (lerr = ERR_get_error()) )
    {
        debug_message("  %4ld: %s\n",lerr, ERR_error_string(lerr, NULL));
    }
    debug_message("%s end of OpenSSL error stack dump.\n",time_stamp());
#endif
} // dump_ssl_error_stack

/*-------------------------------------------------------------------------*/
int
tls_read (interactive_t *ip, char *buffer, int length) __attribute__((nonnull));
int
tls_read (interactive_t *ip, char *buffer, int length)

/* Read up to <length> bytes data for the TLS connection of <ip>
 * and store it in <buffer>.
 * Return then number of bytes read, or a negative number if an error
 * occured.
 */

{
    int ret;
    int err;
    int retries = 5;

    do {
        ret = SSL_read(ip->tls_session, buffer, length);
    }
    while  (ret < 0 && (err = SSL_get_error(ip->tls_session, ret))
              && (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE)
              && (--retries));

    if (ret <= 0)
    {
        err = SSL_get_error(ip->tls_session, ret);
        if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE)
        {
            // recoverable errors and likely to succeed later. Caller should
            // try again.
            errno = EAGAIN;
            return -1;
        }
        else if (err == SSL_ERROR_SYSCALL
                 && (errno==EINTR || errno==EAGAIN))
        {
            // in these cases, we should also try again later.
            return -1;
        }
        else
        {
            // in case of other errors, disconnect...
            debug_message("%s TLS: Received corrupted data (%d). "
                          "Closing the connection.\n"
                          , time_stamp(), err);
            dump_openssl_error_stack();
            tls_deinit_connection(ip);
            // get_message() expects an errno value. ESHUTDOWN will close the connection.
            errno = ESHUTDOWN;
            return -1;
        }
    }

    return (ret < 0 ? -1 : ret);
} /* tls_read() */

/*-------------------------------------------------------------------------*/
int
tls_write (interactive_t *ip, char *buffer, int length) __attribute__((nonnull));
int
tls_write (interactive_t *ip, char *buffer, int length)

/* Write <length> bytes from <buffer> to the TLS connection of <ip>
 * Return the number of bytes written, or a negative number if an error
 * occured.
 */

{
#ifdef DEBUG
    // should never happen since OpenSSL's behaviour is undefined for length==0
    assert(length > 0);
#endif
    int ret = SSL_write(ip->tls_session, buffer, length);

    if (ret <= 0)
    {
        int err = SSL_get_error(ip->tls_session, ret);
        if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE)
        {
            // recoverable error, caller will try again with the same content.
            errno = EWOULDBLOCK;
        }
        else
        {
            debug_message("%s TLS: Sending data failed (%d). "
                          "Closing the connection.\n"
                          , time_stamp(), err);
            dump_openssl_error_stack();
            tls_deinit_connection(ip);
        }
    }

    return (ret<0 ? -1 : ret);
} /* tls_write() */

/*-------------------------------------------------------------------------*/
int
tls_do_handshake (interactive_t *ip) __attribute__((nonnull));
int
tls_do_handshake (interactive_t *ip)

/* Continue the TLS initialisation handshake for interactive <ip>.
 * Return a negative error code if the connection can not be set up.
 * Return 0 if the connection is still begin set up.
 * Return 1 if the connection is now active (or if no secure connection
 * had been requested).
 *
 * This function does only the package specific part of
 * tls_continue_handshake.
 */

{
    int ret, n;

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
            ret = - ret;
        }
        else
        {
            /* Setup finished */
            /* TODO: Check SSL_in_init() at this place? */
            ret = 1;
        }
    }
    else
        ret = 0;

    return ret;
} /* tls_do_handshake() */

/*-------------------------------------------------------------------------*/
int
tls_init_connection (interactive_t *ip) __attribute__((nonnull));
int
tls_init_connection (interactive_t *ip)

/* Starts a TLS secured connection to the interactive <ip>.
 * Returns a negative error code or 0 for success.
 *
 * After a successful start tls_do_handshake will be called.
 */

{
    // Add some fresh entropy to the PRNG state.
    tls_add_entropy();

    SSL * session = SSL_new(context);

    if (session == NULL)
    {
        return -ERR_get_error();
    }

    if (!SSL_set_fd(session, ip->socket))
    {
        SSL_free(session);
        return -ERR_get_error();
    }
    if (ip->outgoing_conn)
        SSL_set_connect_state(session);
    else
    {
        SSL_set_accept_state(session);
        /* request a client certificate */
        SSL_set_verify( session, SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE
                      , tls_verify_callback);
    }
    ip->tls_session = session;

    return 0;
} /* tls_init_connection() */

/*-------------------------------------------------------------------------*/
vector_t *
tls_check_certificate (interactive_t *ip, Bool more) __attribute__((nonnull));
vector_t *
tls_check_certificate (interactive_t *ip, Bool more)

/* Checks the certificate of the secured connection and returns
 * an array with the information about it for the efun
 * tls_check_certificate().
 */

{
    vector_t *v = NULL;
    X509 *peer;
    X509_NAME *subject;

    peer = SSL_get_peer_certificate(ip->tls_session);
    if (peer != NULL)
    {
        int i, j, len;
        char buf[256];
        vector_t *extra = NULL;

        v = allocate_array(more ? 3 : 2);

        /* the result of SSL verification, the most important thing here
         * see verify(1) for more details
         */
        put_number(&(v->item[0]), SSL_get_verify_result(ip->tls_session));

        subject = X509_get_subject_name(peer);

        j = X509_NAME_entry_count(subject);
        extra = allocate_array(3 * j);

        /* iterate all objects in the certificate */
        for (i = 0; i < j; i++)
        {
            X509_NAME_ENTRY *entry;
            ASN1_OBJECT *ob;

            entry = X509_NAME_get_entry(subject, i);
            ob = X509_NAME_ENTRY_get_object(entry);

            len = OBJ_obj2txt(buf, sizeof buf, ob, 1);
            put_c_n_string(&(extra->item[3 * i]), buf, len);

            len = OBJ_obj2txt(buf, sizeof buf, ob, 0);
            put_c_n_string(&(extra->item[3 * i + 1]), buf, len);
            
            put_c_string(&(extra->item[3 * i + 2])
                        , (char *)ASN1_STRING_data(X509_NAME_ENTRY_get_data(entry)));
        }
        put_array(&(v->item[1]), extra);

        /* also get all information from extensions like subjectAltName */
        if (more)
        {
            vector_t *extensions = NULL;
            vector_t *extension = NULL;

            j = X509_get_ext_count(peer);
            extensions = allocate_array(3 * j);
            for (i = X509_get_ext_by_NID(peer, NID_subject_alt_name, -1)
                ; i != -1
                ; i = X509_get_ext_by_NID(peer, NID_subject_alt_name, i))
            {
                int iter, count;

                X509_EXTENSION *ext = NULL;
                STACK_OF(GENERAL_NAME) *ext_vals = NULL;

                ext = X509_get_ext(peer, i);
                if (ext == NULL) {
                    break;
                }
                /* extension name */
                len = OBJ_obj2txt(buf, sizeof buf, ext->object, 1),
                put_c_n_string(&(extensions->item[3 * i]), (char *)buf, len);

                len = OBJ_obj2txt(buf, sizeof buf, ext->object, 0),
                put_c_n_string(&(extensions->item[3 * i + 1]), (char *)buf, len);

                /* extension values */
                ext_vals = X509V3_EXT_d2i(ext);
                if (ext_vals == NULL) {
                    break;
                }

                count = sk_GENERAL_NAME_num(ext_vals);
                extension = allocate_array(3 * count);

                put_array(&(extensions->item[3 * i + 2]), extension);
                for (iter = 0; iter < count; iter++) {
                    GENERAL_NAME *ext_val = NULL;
                    ASN1_STRING *value = NULL;

                    ext_val = sk_GENERAL_NAME_value(ext_vals, iter);

                    switch(ext_val->type) {
                    case GEN_OTHERNAME:
                        value = ext_val->d.otherName->value->value.asn1_string;

                        len = OBJ_obj2txt(buf, sizeof buf, ext_val->d.otherName->type_id, 1),
                        put_c_n_string(&(extension->item[3 * iter]), buf, len);
                        len = OBJ_obj2txt(buf, sizeof buf, ext_val->d.otherName->type_id, 0),
                        put_c_n_string(&(extension->item[3 * iter + 1]), buf, len);
                        put_c_string(&(extension->item[3 * iter + 2])
                                    , (char*)ASN1_STRING_data(value));
                        break;
                    case GEN_DNS:
                        value = ext_val->d.dNSName;
                        put_c_n_string(&(extension->item[3 * iter]), "dNSName", 7);
                        put_c_n_string(&(extension->item[3 * iter + 1]), "dNSName", 7);
                        put_c_string(&(extension->item[3 * iter + 2])
                                    , (char*)ASN1_STRING_data(value));

                        break;
                    case GEN_EMAIL:
                        value = ext_val->d.rfc822Name;
                        put_c_n_string(&(extension->item[3 * iter]), "rfc822Name", 10);
                        put_c_n_string(&(extension->item[3 * iter + 1]), "rfc822Name", 10);
                        put_c_string(&(extension->item[3 * iter + 2])
                                    , (char*)ASN1_STRING_data(value));
                        break;
                    case GEN_URI:
                        value = ext_val->d.uniformResourceIdentifier;
                        put_c_n_string(&(extension->item[3 * iter]), "uniformResourceIdentifier", 25);
                        put_c_n_string(&(extension->item[3 * iter + 1]), "uniformResourceIdentifier", 25);
                        put_c_string(&(extension->item[3 * iter + 2])
                                    , (char*)ASN1_STRING_data(value));
                        break;

                    /* TODO: the following are unimplemented 
                     *                 and the structure is getting ugly 
                     */
                    case GEN_X400:
                    case GEN_DIRNAME:
                    case GEN_EDIPARTY:
                    case GEN_IPADD:
                    case GEN_RID:
                    default:
                        break;
                    }
                }
            }
            put_array(&(v->item[2]), extensions);
        }
        X509_free(peer);
    }

    return v;
} /* tls_check_certificate() */

/*-------------------------------------------------------------------------*/
void
tls_deinit_connection (interactive_t *ip) __attribute__((nonnull));
void
tls_deinit_connection (interactive_t *ip)

/* Close the TLS connection for the interactive <ip> if there is one.
 */

{
    if (ip->tls_status != TLS_INACTIVE)
    {
        SSL_shutdown(ip->tls_session);
        SSL_free(ip->tls_session);
        ip->tls_session = NULL;
    }

    if (ip->tls_cb != NULL)
    {
        free_callback(ip->tls_cb);
        xfree(ip->tls_cb);
        ip->tls_cb = NULL;
    }
    ip->tls_status = TLS_INACTIVE;
} /* tls_deinit_connection() */

/*-------------------------------------------------------------------------*/
const char *
tls_error (int err)

/* Returns a string describing the error behind the
 * error number <err>, which is always negative.
 */

{
    return ERR_error_string(-err, NULL);
} /* tls_error() */

/*-------------------------------------------------------------------------*/
vector_t *
tls_query_connection_info (interactive_t *ip) __attribute__((nonnull));
vector_t *
tls_query_connection_info (interactive_t *ip)

/* Returns the connection info array for the efun
 * tls_query_connection_info(). <ip> is guaranteed
 * to have a TLS secured connection.
 */

{
    vector_t * rc;

    rc = allocate_array(TLS_INFO_MAX);  
    put_c_string(&(rc->item[TLS_CIPHER])
                , SSL_get_cipher(ip->tls_session));
    put_number(&(rc->item[TLS_COMP]), 0);
    put_number(&(rc->item[TLS_KX]), 0);
    put_number(&(rc->item[TLS_MAC]), 0);
    put_c_string(&(rc->item[TLS_PROT])
                , SSL_get_version(ip->tls_session));

    return rc;
} /* tls_query_connection_info() */

/*-------------------------------------------------------------------------*/
Bool
tls_available ()

/* If the global TLS Initialisation could not been set up, tls_available()
 * returns MY_FALSE, otherwise MY_TRUE.
 */

{
    return tls_is_available;
} /* tls_available() */


/*------------------------------------------------------------------
 * Interface to the openssl cryptography api
 *------------------------------------------------------------------
 */
Bool
get_digest (int num, digest_t * md, size_t *len) __attribute__((nonnull));
Bool
get_digest (int num, digest_t * md, size_t *len)

/* Determine the proper digest descriptor <*md> and length <*len>
 * from the designator <num>, which is one of the TLS_HASH_ constants.
 *
 * Return MY_FALSE if the desired digest isn't available.
 */

{
    switch(num)
    {
#ifndef OPENSSL_NO_SHA1
# ifdef SHA_DIGEST_LENGTH
    case TLS_HASH_SHA1:
        (*len) = SHA_DIGEST_LENGTH;
        (*md) = EVP_sha1();
        break;
# endif
#endif
#ifndef OPENSSL_NO_SHA256
# ifdef SHA224_DIGEST_LENGTH
    case TLS_HASH_SHA224:
        (*len) = SHA224_DIGEST_LENGTH;
        (*md) = EVP_sha224();
        break;
# endif
# ifdef SHA256_DIGEST_LENGTH
    case TLS_HASH_SHA256:
        (*len) = SHA256_DIGEST_LENGTH;
        (*md) = EVP_sha256();
        break;
# endif
#endif
#ifndef OPENSSL_NO_SHA512
# ifdef SHA384_DIGEST_LENGTH
    case TLS_HASH_SHA384:
        (*len) = SHA384_DIGEST_LENGTH;
        (*md) = EVP_sha384();
        break;
# endif
# ifdef SHA512_DIGEST_LENGTH
    case TLS_HASH_SHA512:
        (*len) = SHA512_DIGEST_LENGTH;
        (*md) = EVP_sha512();
        break;
# endif
#endif
#ifndef OPENSSL_NO_MD5
# ifdef MD5_DIGEST_LENGTH
    case TLS_HASH_MD5:
        (*len) = MD5_DIGEST_LENGTH;
        (*md) = EVP_md5();
        break;
# endif
#endif
#ifndef OPENSSL_NO_RIPEMD
# ifdef RIPEMD160_DIGEST_LENGTH
    case TLS_HASH_RIPEMD160:
        (*len) = RIPEMD160_DIGEST_LENGTH;
        (*md) = EVP_ripemd160();
        break;
# endif
#endif
    default:
        (*md) = NULL;
        return MY_FALSE;
    }

    return MY_TRUE;
} /* get_digest() */

/*-------------------------------------------------------------------------*/
void
calc_digest (digest_t md, void *dest, size_t destlen, void *msg, size_t msglen, void *key, size_t keylen)
             __attribute__((nonnull(2,4)));
void
calc_digest (digest_t md, void *dest, size_t destlen, void *msg, size_t msglen, void *key, size_t keylen)

/* Calculates the hash or the HMAC if <key> != NULL from <msg> as determined
 * by method <md> as it was returned by get_digest().
 */
{
    if(key)
    {
#if defined(OPENSSL_NO_HMAC)
        errorf("OpenSSL wasn't configured to provide the hmac() method.\n");
        /* NOTREACHED */
#else
        HMAC_CTX ctx;

        HMAC_Init(&ctx, key, keylen, md);
        HMAC_Update(&ctx, msg, msglen);
        HMAC_Final(&ctx, dest, NULL);
#endif
    }
    else
    {
        EVP_MD_CTX ctx;

        EVP_DigestInit(&ctx, md);
        EVP_DigestUpdate(&ctx, msg, msglen);
        EVP_DigestFinal(&ctx, dest, NULL);
    }
} /* calc_digest() */

/***************************************************************************/
#endif /* USE_TLS */
