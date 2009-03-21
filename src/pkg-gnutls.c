/*------------------------------------------------------------------
 * Wrapper for the GnuTLS module.
 *
 *------------------------------------------------------------------
 */

#include "driver.h"
#include "machine.h"

#if defined(USE_TLS) && defined(HAS_GNUTLS)

#include <stdio.h>

#include <gnutls/gnutls.h>

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

/*-------------------------------------------------------------------------*/
/* Variables */

static Bool tls_is_available = MY_FALSE;
  /* Set to TRUE when the TLS layer has been initialised successfully.
   */

static gnutls_certificate_server_credentials x509_cred;
  /* The x509 credentials. */

static gnutls_dh_params dh_params;
  /* The Diffie-Hellmann parameters */

/*-------------------------------------------------------------------------*/
static int
generate_dh_params (void)

/* GnuTLS: Generate Diffie Hellman parameters and store them in the global
 * <dh_params>.  They are for use with DHE kx algorithms. These should be
 * discarded and regenerated once a day, once a week or once a month. Depends
 * on the security requirements.
 *
 * tls_is_available must be TRUE.
 */

{
#if HAS_GNUTLS_VERSION < 8
    gnutls_datum prime, generator;

    gnutls_dh_params_init( &dh_params);
    gnutls_dh_params_generate( &prime, &generator, DH_BITS);
    gnutls_dh_params_set( dh_params, prime, generator, DH_BITS);

    free( prime.data);
    free( generator.data);
#else
    gnutls_dh_params_init( &dh_params);
    gnutls_dh_params_generate2( dh_params, DH_BITS);
#endif
    return 0;
} /* generate_dh_params() */

/*-------------------------------------------------------------------------*/
static void
initialize_tls_session (gnutls_session *session)

/* GnuTLS: Initialise a TLS <session>.
 * tls_is_available must be TRUE.
 */

{
    gnutls_init(session, GNUTLS_SERVER);

    /* avoid calling all the priority functions, since the defaults
     * are adequate.
     */
    gnutls_set_default_priority( *session);   
    
    gnutls_credentials_set( *session, GNUTLS_CRD_CERTIFICATE, x509_cred);

    gnutls_certificate_server_set_request( *session, GNUTLS_CERT_REQUEST);

    gnutls_dh_set_prime_bits( *session, DH_BITS);
} /* initialize_tls_session() */

/*-------------------------------------------------------------------------*/
static void *
tls_xalloc (size_t size)

/* Wrapper function so that (gnu)tls will use the driver's allocator.
 * The wrapper is required as 'pxalloc' itself is a macro.
 */

{
    return pxalloc(size);
} /* tls_xalloc() */

/*-------------------------------------------------------------------------*/
static void *
tls_rexalloc (void *old, size_t size)

/* Wrapper function so that (gnu)tls will use the driver's allocator.
 * The wrapper is required as 'prexalloc' itself is a macro.
 */

{
    return prexalloc(old, size);
} /* tls_rexalloc() */

/*-------------------------------------------------------------------------*/
static void
tls_xfree (void *p)

/* Wrapper function so that (gnu)tls will use the driver's allocator.
 * The wrapper is not exactly required for pfree(),  but it keeps things
 * consistent.
 */

{
    return pfree(p);
} /* tls_xfree() */

/*-------------------------------------------------------------------------*/
void
tls_verify_init (void)

/* initialize or reinitialize tls certificate storage and revocation lists.
 */
{
    /* TODO */
}

/*-------------------------------------------------------------------------*/
void
tls_global_init (void)

/* Initialise the TLS package; to be called once at program startup.
 */

{
    char * keyfile = tls_keyfile ? tls_keyfile : TLS_DEFAULT_KEYFILE;
    char * certfile = tls_certfile ? tls_certfile : TLS_DEFAULT_CERTFILE;

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
       printf("%s TLS: x509 keyfile and certificate set.\n", time_stamp());
        generate_dh_params();

        gnutls_certificate_set_dh_params( x509_cred, dh_params);

        tls_is_available = MY_TRUE;
    }
} /* tls_global_init() */

/*-------------------------------------------------------------------------*/
void
tls_global_deinit (void)

/* Clean up the TLS package on program termination.
 */

{
    if (tls_is_available)
    {
        gnutls_certificate_free_credentials(x509_cred);
    }

    gnutls_global_deinit();

    tls_is_available = MY_FALSE;

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
    int ret = -11;

    do {
           ret = gnutls_record_recv(ip->tls_session, buffer, length);
    } while ( ret < 0 && (ret == GNUTLS_E_INTERRUPTED || ret == GNUTLS_E_AGAIN) );

    if (ret == 0)
    {
        tls_deinit_connection(ip);
    }
    else if (ret < 0)
    {
        debug_message("%s GnuTLS: Error in receiving data (%s). "
                      "Closing the connection.\n"
                     , time_stamp(), gnutls_strerror(ret));
        tls_deinit_connection(ip);
    }

    return (ret < 0 ? -1 : ret);
} /* tls_read() */

/*-------------------------------------------------------------------------*/
int
tls_write (interactive_t *ip, char *buffer, int length)

/* Write <length> bytes from <buffer> to the TLS connection of <ip>
 * Return the number of bytes written, or a negative number if an error
 * occured.
 */

{
    int ret = -1;

    ret = gnutls_record_send( ip->tls_session, buffer, length );
    if (ret < 0)
    {
        debug_message("%s GnuTLS: Error in sending data (%s). "
                      "Closing the connection.\n"
                     , time_stamp(), gnutls_strerror(ret));
        tls_deinit_connection(ip);
    }

    return (ret<0 ? -1 : ret);
} /* tls_write() */

/*-------------------------------------------------------------------------*/
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
    int ret;

    ret = gnutls_handshake(ip->tls_session);

    if (ret != GNUTLS_E_AGAIN && ret != GNUTLS_E_INTERRUPTED)
    {
        if (ret < 0)
        {
            /* Setup failed */
            gnutls_deinit(ip->tls_session);
	    ip->tls_session = NULL;
        }
        else
        {
            /* Setup finished */
            ret = 1;
        }
    }
    else
        ret = 0;

    return ret;
} /* tls_do_handshake() */

/*-------------------------------------------------------------------------*/
int
tls_init_connection (interactive_t *ip)

/* Starts a TLS secured connection to the interactive <ip>.
 * Returns a negative error code or 0 for success.
 *
 * After a successful start tls_do_handshake will be called.
 */

{
    initialize_tls_session(&ip->tls_session);
    gnutls_transport_set_ptr(ip->tls_session, (gnutls_transport_ptr)(ip->socket));
    
    return 0;
} /* tls_init_connection() */

/*-------------------------------------------------------------------------*/
vector_t *
tls_check_certificate (interactive_t *ip, Bool more)
{
    /* TODO */
    errorf( "%s TLS: GnuTLS does not provide certificate checking yet.\n"
          , time_stamp());

    return NULL; /* NOTREACHED */
} /* tls_check_certificate() */

/*-------------------------------------------------------------------------*/
void
tls_deinit_connection (interactive_t *ip)

/* Close the TLS connection for the interactive <ip> if there is one.
 */

{
    if (ip->tls_status != TLS_INACTIVE)
    {
        gnutls_bye( ip->tls_session, GNUTLS_SHUT_WR);
        gnutls_deinit(ip->tls_session);
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
tls_error(int err)

/* Returns a string describing the error behind the
 * error number <err>, which is always negative.
 */

{
    return gnutls_strerror(err);
} /* tls_error() */

/*-------------------------------------------------------------------------*/
vector_t *
tls_query_connection_info (interactive_t *ip)

/* Returns the connection info array for the efun
 * tls_query_connection_info(). <ip> is guaranteed
 * to have a TLS secured connection.
 */

{
    vector_t * rc;
    rc = allocate_array(TLS_INFO_MAX);

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


/***************************************************************************/
#endif /* USE_TLS && !HAS_OPENSSL */
