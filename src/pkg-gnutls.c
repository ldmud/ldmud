/*------------------------------------------------------------------
 * Wrapper for the GnuTLS module.
 *
 *------------------------------------------------------------------
 */

#include "driver.h"
#include "machine.h"

#if defined(USE_TLS) && defined(HAS_GNUTLS)

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined(HAVE_DIRENT_H) || defined(_POSIX_VERSION)
#    include <dirent.h>
#    define generic_dirent dirent
#    define DIRENT_NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#    define generic_dirent direct
#    define DIRENT_NLENGTH(dirent) ((dirent)->d_namlen)
#    ifdef HAVE_SYS_NDIR_H
#        include <sys/ndir.h>
#    endif /* SYSNDIR */
#    ifdef HAVE_SYS_DIR_H
#        include <sys/dir.h>
#    endif /* SYSDIR */
#    ifdef HAVE_NDIR_H
#        include <ndir.h>
#    endif /* NDIR */
#endif /* not (HAVE_DIRENT_H or _POSIX_VERSION) */

#ifndef S_ISREG
#    define S_ISREG(m) (((m)&S_IFMT) == S_IFREG)
#endif


#include <gnutls/gnutls.h>
#include <gnutls/x509.h>

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
initialize_tls_session (gnutls_session *session) __attribute__((nonnull));
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
tls_rexalloc (void *old, size_t size) __attribute__((nonnull));
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
tls_xfree (void *p) __attribute__((nonnull));
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
    gnutls_certificate_free_cas(x509_cred);

    if (tls_trustfile != NULL)
    {
        int err;

        printf("%s TLS: (GnuTLS) trusted x509 certificates from '%s'.\n"
              , time_stamp(), tls_trustfile);
        debug_message("%s TLS: (GnuTLS) trusted x509 certificates from '%s'.\n"
                     , time_stamp(), tls_trustfile);
        err = gnutls_certificate_set_x509_trust_file(x509_cred, tls_trustfile, GNUTLS_X509_FMT_PEM);

        if (err < 0)
        {
            printf("%s TLS: Error setting x509 verification certificates: %s\n"
                  , time_stamp(), gnutls_strerror(err));
            debug_message("%s TLS: Error setting x509 verification certificates: %s\n"
                         , time_stamp(), gnutls_strerror(err));
        }
    }

    if (tls_trustdirectory)
    {
        DIR * d;
        char *fname;
        size_t dirlen;
        int err;

        printf("%s TLS: (GnuTLS) trusted x509 certificates from directory '%s'.\n"
              , time_stamp(), tls_trustdirectory);
        debug_message("%s TLS: (GnuTLS) trusted x509 certificates from directory '%s'.\n"
                     , time_stamp(), tls_trustdirectory);

        dirlen = strlen(tls_trustdirectory);
        fname = (char*) xalloc(dirlen + NAME_MAX + 2);
        if (!fname)
        {
            errno = ENOMEM;
            d = NULL;
        }
        else
        {
            strcpy(fname, tls_trustdirectory);
            fname[dirlen++] = '/';
            d = opendir(tls_trustdirectory);
        }

        if (d == NULL)
        {
            printf("%s TLS: Can't read trust directory: %s.\n"
                  , time_stamp(), strerror(errno));
            debug_message("%s TLS: Can't read trust directory: %s\n"
                         , time_stamp(), strerror(errno));
        }
        else
        {
            struct dirent *file;

            while ((file = readdir(d)) != NULL)
            {
                struct stat st;

                strcpy(fname+dirlen, file->d_name);
                stat(fname, &st);

                if (S_ISREG(st.st_mode))
                {
                    err = gnutls_certificate_set_x509_trust_file(x509_cred, fname, GNUTLS_X509_FMT_PEM);
                    if (err < 0)
                    {
                        printf("%s TLS: Error setting x509 verification certificates from '%s': %s\n"
                              , time_stamp(), fname, gnutls_strerror(err));
                        debug_message("%s TLS: Error setting x509 verification certificates from '%s': %s\n"
                                     , time_stamp(), fname, gnutls_strerror(err));
                    }
                }
            }

            closedir(d);
        }

        xfree(fname);
    }
    else if(tls_trustfile == NULL)
    {
        printf("%s TLS: (GnuTLS) Trusted x509 certificates locations not specified.\n"
              , time_stamp());
        debug_message("%s TLS: (GnuTLS) trusted x509 certificates locations not specified.\n"
                     , time_stamp());
    }

    gnutls_certificate_free_crls(x509_cred);
    if (tls_crlfile != NULL)
    {
        int err;

        printf("%s TLS: (GnuTLS) CRLs from '%s'.\n"
              , time_stamp(), tls_crlfile);
        debug_message("%s TLS: (GnuTLS) CRLs from '%s'.\n"
                     , time_stamp(), tls_crlfile);
        err = gnutls_certificate_set_x509_crl_file(x509_cred, tls_crlfile, GNUTLS_X509_FMT_PEM);

        if (err < 0)
        {
            printf("%s TLS: Error loading CRLs: %s\n"
                  , time_stamp(), gnutls_strerror(err));
            debug_message("%s TLS: Error loading CRLs: %s\n"
                         , time_stamp(), gnutls_strerror(err));
        }
    }

    if (tls_crldirectory)
    {
        DIR * d;
        char *fname;
        size_t dirlen;
        int err;

        printf("%s TLS: (GnuTLS) CRLs from directory '%s'.\n"
              , time_stamp(), tls_crldirectory);
        debug_message("%s TLS: (GnuTLS) CRLs from directory '%s'.\n"
                     , time_stamp(), tls_crldirectory);

        dirlen = strlen(tls_crldirectory);
        fname = (char*) xalloc(dirlen + NAME_MAX + 2);
        if (!fname)
        {
            errno = ENOMEM;
            d = NULL;
        }
        else
        {
            strcpy(fname, tls_crldirectory);
            fname[dirlen++] = '/';
            d = opendir(tls_crldirectory);
        }

        if (d == NULL)
        {
            printf("%s TLS: Can't read CRL directory: %s.\n"
                  , time_stamp(), strerror(errno));
            debug_message("%s TLS: Can't read CRL directory: %s\n"
                         , time_stamp(), strerror(errno));
        }
        else
        {
            struct dirent *file;

            while ((file = readdir(d)) != NULL)
            {
                struct stat st;

                strcpy(fname+dirlen, file->d_name);
                stat(fname, &st);

                if (S_ISREG(st.st_mode))
                {
                    err = gnutls_certificate_set_x509_crl_file(x509_cred, fname, GNUTLS_X509_FMT_PEM);
                    if (err < 0)
                    {
                        printf("%s TLS: Error loading CRL from '%s': %s\n"
                              , time_stamp(), fname, gnutls_strerror(err));
                        debug_message("%s TLS: Error loading CRL from '%s': %s\n"
                                     , time_stamp(), fname, gnutls_strerror(err));
                    }
                }
            }

            closedir(d);
        }

        xfree(fname);
    }
    else if(!tls_crlfile)
    {
        printf("%s TLS: (GnuTLS) CRL checking disabled.\n"
              , time_stamp());
        debug_message("%s TLS: (GnuTLS) CRL checking disabled.\n"
                     , time_stamp());
    }
}

/*-------------------------------------------------------------------------*/
void
tls_global_init (void)

/* Initialise the TLS package; to be called once at program startup.
 */

{
    int f;

    if (tls_keyfile == NULL)
    {
        printf("%s TLS deactivated.\n", time_stamp());
        return;
    }

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
          , time_stamp(), tls_keyfile, tls_certfile);
    debug_message("%s TLS: (GnuTLS) Keyfile '%s', Certfile '%s'\n"
                 , time_stamp(), tls_keyfile, tls_certfile);

    f = gnutls_certificate_set_x509_key_file(x509_cred, tls_certfile, tls_keyfile, GNUTLS_X509_FMT_PEM);
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

        tls_verify_init();

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
    int retries = 5;

    do {
           ret = gnutls_record_recv(ip->tls_session, buffer, length);
    } while ( ret < 0
           && (ret == GNUTLS_E_INTERRUPTED || ret == GNUTLS_E_AGAIN)
           && (--retries) );

    if (ret <= 0)
    {
        /* Let comm.c handle EINTR and EWOULDBLOCK.
         * We are then called again later with the
         * same content.
         */
        if (ret == GNUTLS_E_INTERRUPTED)
        {
            errno = EINTR;
            return -1;
        }
        else if (ret == GNUTLS_E_AGAIN)
        {
            errno = EWOULDBLOCK;
            return -1;
        }
        // all other errors are fatal.
        debug_message("%s GnuTLS: Error in receiving data (%s). "
                      "Closing the connection.\n"
                     , time_stamp(), gnutls_strerror(ret));
        tls_deinit_connection(ip);
        /* get_message() expects an errno value.
         * ESHUTDOWN will close the connection.
         */
        errno = ESHUTDOWN;
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
    int ret = gnutls_record_send( ip->tls_session, buffer, length );
    if (ret < 0)
    {
        /* Let comm.c handle EINTR and EWOULDBLOCK.
         * We are then called again later with the
         * same content. Whats happening here is the following:
         * GnuTLS stores the content in an internal buffer in case of
         * GNUTLS_E_INTERRUPTED or GNUTLS_E_AGAIN. If gnutls_record_send()
         * is called and there is content in this buffer, it will send the
         * content of this internal buffer instead the content of <buffer>.
         * According to the documentation we should call again with NULL as 
         * buffer in this case, but GnuTLS does not care and we will anyway
         * try to send the same content. It is way easier for us to not call
         * with NULL, we just have to ensure to try to re-send the same
         * content.
         */
        if (ret == GNUTLS_E_INTERRUPTED)
        {
            errno = EINTR;
            return -1;
        }
        else if (ret == GNUTLS_E_AGAIN)
        {
            errno = EWOULDBLOCK;
            return -1;
        }

        debug_message("%s GnuTLS: Error in sending data (%s). "
                      "Closing the connection.\n"
                     , time_stamp(), gnutls_strerror(ret));
        tls_deinit_connection(ip);
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
tls_init_connection (interactive_t *ip) __attribute__((nonnull));
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
tls_check_certificate (interactive_t *ip, Bool more) __attribute__((nonnull));
vector_t *
tls_check_certificate (interactive_t *ip, Bool more)

/* Checks the certificate of the secured connection and returns
 * an array with the information about it for the efun
 * tls_check_certificate().
 */

{
    vector_t *v = NULL;
    const gnutls_datum_t *cert_list;
    gnutls_x509_crt_t cert;
#if LIBGNUTLS_VERSION_MAJOR > 1 || (LIBGNUTLS_VERSION_MAJOR==1 && (LIBGNUTLS_VERSION_MINOR > 7 || (LIBGNUTLS_VERSION_MINOR == 7 && LIBGNUTLS_VERSION_MINOR >= 8)))
#define GNUTLS_NEW_DN_API
    gnutls_x509_dn_t subject;
#endif
    unsigned int cert_list_size;
    unsigned int result;
    time_t t, now;
    int err;

    cert_list = gnutls_certificate_get_peers(ip->tls_session, &cert_list_size);
    if (cert_list == NULL || cert_list_size == 0)
        return v;

    err = gnutls_certificate_verify_peers2(ip->tls_session, &result);
    if (err < 0)
        return v;

    if (gnutls_x509_crt_init(&cert) < 0)
        return v;

    if (gnutls_x509_crt_import(cert, &cert_list[0], GNUTLS_X509_FMT_DER) < 0)
    {
        gnutls_x509_crt_deinit(cert);
        return v;
    }

    now = time(0);
    t = gnutls_x509_crt_get_expiration_time(cert);
    if (t == (time_t)-1 || t < now)
        result |= GNUTLS_CERT_INVALID;

    t = gnutls_x509_crt_get_activation_time(cert);
    if (t == (time_t)-1 || t >= now)
        result |= GNUTLS_CERT_INVALID;

    v = allocate_array(more ? 3 : 2);
    put_number(&(v->item[0]), result);

#ifdef GNUTLS_NEW_DN_API
    err = gnutls_x509_crt_get_subject(cert, &subject);
    if (err < 0)
        put_number(&(v->item[1]), 0);
    else
    {
        int count;
        int rdn, ava_nr;
        gnutls_x509_ava_st ava;
        vector_t *extra;

        count = 0;
        for(rdn=0; !gnutls_x509_dn_get_rdn_ava(subject, rdn, 0, &ava); rdn++)
        {
            count++;
            for(ava_nr=1; !gnutls_x509_dn_get_rdn_ava(subject, rdn, ava_nr, &ava); ava_nr++)
                count++;
        }

        extra = allocate_array(3 * count);
        count = 0;

        for(rdn=0; !gnutls_x509_dn_get_rdn_ava(subject, rdn, 0, &ava); rdn++)
            for(ava_nr=0; ava_nr==0 || !gnutls_x509_dn_get_rdn_ava(subject, rdn, ava_nr, &ava); ava_nr++)
            {
                put_c_n_string(&(extra->item[count++]), (char*)ava.oid.data, ava.oid.size-1);
                put_number(&(extra->item[count]), 0); count++;
                put_c_n_string(&(extra->item[count++]), (char*)ava.value.data, ava.value.size);
            }

        put_array(&(v->item[1]), extra);
    }
#else
    {
        int count, nr;
        vector_t *extra;
        char oid[128];
        char data[256];
        char *ptr;
        size_t osize, dsize;

        count = 0;
        nr = 0;

        while(1)
        {
            osize = sizeof(oid);

            err = gnutls_x509_crt_get_dn_oid(cert, nr, oid, &osize);
            if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
                break;
            if (err >= 0)
                count++;
            nr++;
        }

        extra = allocate_array(3 * count);
        count = 0;
        ptr = 0;
        nr = 0;

        while (1)
        {
            osize = sizeof(oid);

            err = gnutls_x509_crt_get_dn_oid(cert, nr, oid, &osize);
            if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
                break;
            if (err >= 0)
            {
                dsize = sizeof(data);
                ptr = NULL;

                err = gnutls_x509_crt_get_dn_by_oid(cert, oid, 0, 0, data, &dsize);
                if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
                {
                    ptr = (char*) xalloc(dsize);
                    if (ptr)
                        err = gnutls_x509_crt_get_dn_by_oid(cert, oid, 0, 0, ptr, &dsize);
                }

                put_c_n_string(&(extra->item[count++]), oid, osize);
                put_number(&(extra->item[count]), 0); count++;
                if (err >= 0)
                    put_c_n_string(&(extra->item[count++]), (ptr!=NULL) ? ptr : data, dsize);
                else
                {
                    put_number(&(extra->item[count]), 0); count++;
                }

                if (ptr)
                    xfree(ptr);
            }

            nr++;
        }

        put_array(&(v->item[1]), extra);
    }
#endif

    if (more)
    {
        int count, nr;
        vector_t *extra;
        char oid[128];
        char data[256];
        char *ptr;
        size_t osize, dsize;

        count = 0;
        nr = 0;

        while(1)
        {
            osize = sizeof(oid);

            err = gnutls_x509_crt_get_extension_oid(cert, nr, oid, &osize);
            if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
                break;
            if (err >= 0)
                count++;
            nr++;
        }

        extra = allocate_array(3 * count);
        count = 0;
        ptr = 0;
        nr = 0;

        while (1)
        {
            osize = sizeof(oid);

            err = gnutls_x509_crt_get_extension_oid(cert, nr, oid, &osize);
            if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE)
                break;
            if (err >= 0)
            {
#ifndef GNUTLS_NEW_DN_API
                unsigned int critical;
#endif
                dsize = sizeof(data);
                ptr = NULL;

#ifdef GNUTLS_NEW_DN_API
                err = gnutls_x509_crt_get_extension_data(cert, nr, data, &dsize);
                if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
                {
                    ptr = (char*) xalloc(dsize);
                    if (ptr)
                        err = gnutls_x509_crt_get_extension_data(cert, nr, ptr, &dsize);
                }
#else
                err = gnutls_x509_crt_get_extension_by_oid(cert, oid, 0, data, &dsize, &critical);
                if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
                {
                    ptr = (char*) xalloc(dsize);
                    if (ptr)
                        err = gnutls_x509_crt_get_extension_by_oid(cert, oid, 0, ptr, &dsize, &critical);
                }
#endif

                put_c_n_string(&(extra->item[count++]), oid, osize);
                put_number(&(extra->item[count]), 0); count++;
                if (err >= 0)
                    put_c_n_string(&(extra->item[count++]), (ptr!=NULL) ? ptr : data, dsize);
                else
                {
                    put_number(&(extra->item[count]), 0); count++;
                }

                if (ptr)
                    xfree(ptr);
            }

            nr++;
        }

        put_array(&(v->item[2]), extra);
    }

    gnutls_x509_crt_deinit(cert);
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
#endif /* USE_TLS && HAS_GNUTLS */
