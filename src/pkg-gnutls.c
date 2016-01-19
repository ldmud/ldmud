/*------------------------------------------------------------------
 * Wrapper for the GnuTLS module.
 *
 *------------------------------------------------------------------
 */

#include "driver.h"
#include "machine.h"

#if defined(USE_TLS) && defined(HAS_GNUTLS)

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <gnutls/gnutls.h>
#include <gnutls/x509.h>

#include "pkg-tls.h"

#include "actions.h"
#include "array.h"
#include "comm.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "sha1.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/tls.h"
/*-------------------------------------------------------------------------*/
/* Structs */

/* Maximum number of certificates in a certificate chain.
 * If there are more certificates in the chain, the chain will not be loaded
 * and an error is logged.
 */
#define MAX_CHAIN_LENGTH 10

/* One server key and corresponding certicate chain. */
struct tls_key_s
{
    char fingerprint[20]; /* SHA1 */
    gnutls_x509_crt_t cert[MAX_CHAIN_LENGTH];
    gnutls_x509_privkey_t key;
    unsigned int num_certs;
};

/*-------------------------------------------------------------------------*/
/* Constants */
static const char *dh_pkcs3 =
  "-----BEGIN DH PARAMETERS-----\n"
  "MIIBCAKCAQEAnE/wdy2KvsDtoGcoeth2e1CceYOoiEoLTwTumYD3L2kmavYtCM5l\n"
  "Z9dUHoOZXKOvBtHUh4N5yld1AuEC6tE3a+Hr4TIkSCaRXUJhNh5kyebkxWM6zlJx\n"
  "hGTxDd6WJk1eeWwKa8KFgoEh2WHqNwuBWeSdoAHmw0iVSjbj2lpb/XIVJJQSX8HT\n"
  "mWUPIuRaKQmExS4F25dALeFXXYz0bX72FnPgab/fjBNVBbZksV++Plui7NLzn5q+\n"
  "gSJfIqbdAdQr7v25rrFowz/ClEMRH0IXM10h8shzr3Cx4e552Z2saV9SRPOgrlcD\n"
  "VxyEwepMIUNDCOCPNP2nwwBXav10bGmZ0wIBBQ==\n"
  "-----END DH PARAMETERS-----\n";
/* The statically provided Diffie-Hellmann parameters (2048 bits), used as
 * default if the administrator does not provide parameters. */

/*-------------------------------------------------------------------------*/
/* Variables */

static Bool tls_is_available = MY_FALSE;
  /* Set to TRUE when the TLS layer has been initialised successfully.
   */

static gnutls_certificate_server_credentials x509_cred;
  /* The x509 credentials. */

static gnutls_dh_params_t* dh_params = NULL;
  /* Pointer to the Diffie-Hellmann parameters, permanently allocated */

static struct tls_key_s* keys;
  /* Our certificate-key-pairs. */

static int num_keys;
  /* Number of elements in <keys>. */

static int current_key;
  /* Index into <keys>. */

/*-------------------------------------------------------------------------*/
int
tls_import_dh_params (const char* const buffer, size_t length)

/* GnuTLS: Import Diffie Hellman parameters. They are for use with DHE kx
 * algorithms. By default, sets the statically provided parameters in the
 * source.
 * Depending on security requirements, they may be provided by the
 * administrator or even re-newed from time to time.
 * If successful, the global <dh_params> points to the new parameters,
 * otherwise, <dh_params> still points to the old parameters. The memory block
 * for the parameters is allocated by pxalloc().
 * Unless the very first import fails, <x509_cred> will always point to valid
 * parameters.
 *
 * returns 1 on success, 0 otherwise.
 *
 * The global <x509_cred> must be initialized.
 */

{
    gnutls_dh_params_t* newparam = NULL;
    gnutls_dh_params_t* oldparam = dh_params; // pointer auf alte params sichern
    unsigned char* pemstr;
    int err;

    if (buffer == NULL || !length)
    {
      // use built-in defaults
      pemstr = (unsigned char*)dh_pkcs3;
      length = strlen(dh_pkcs3);
      printf("%s TLS: Importing built-in default DH parameters.\n"
             , time_stamp());
      debug_message("%s TLS: Importing built-in default DH parameters.\n"
                    , time_stamp());
    }
    else
    {
      pemstr = (unsigned char*)buffer;
      printf("%s TLS: Importing user-supplied DH parameters.\n"
             , time_stamp());
      debug_message("%s TLS: Importing user-supplied DH parameters.\n"
                    , time_stamp());
    }
    const gnutls_datum_t p3 = { pemstr, length };

    // neue struktur initialisieren
    newparam = pxalloc(sizeof(*newparam));
    if (!newparam)
    {
        printf("%s TLS: Error importing Diffie-Hellman parameters: Out of memory.\n"
              , time_stamp());
        debug_message("%s Error importing Diffie-Hellman parameters: Out ofmemory.\n"
                      , time_stamp());
        return 0;
    }
    err = gnutls_dh_params_init(newparam);
    if (err != GNUTLS_E_SUCCESS)
    {
        printf("%s TLS: Error importing Diffie-Hellman parameters: %s\n"
              , time_stamp(), gnutls_strerror(err) );
        debug_message("%s Error importing Diffie-Hellman parameters: %s\n"
                      , time_stamp(), gnutls_strerror(err));
        pfree(newparam);
        return 0;
    }

    // importieren
    err = gnutls_dh_params_import_pkcs3(*newparam, &p3, GNUTLS_X509_FMT_PEM);
    if (err != GNUTLS_E_SUCCESS)
    {
        printf("%s TLS: Error importing Diffie-Hellman parameters: %s\n"
              , time_stamp(), gnutls_strerror(err) );
        debug_message("%s Error importing Diffie-Hellman parameters: %s\n"
                      , time_stamp(), gnutls_strerror(err));
        gnutls_dh_params_deinit(*newparam);
        return 0;
    }

    // update pointers (never fails)
    dh_params = newparam;
    gnutls_certificate_set_dh_params(x509_cred, *dh_params);

    // alte struktur freigeben
    if (oldparam)
    {
        gnutls_dh_params_deinit(*oldparam);
        pfree(oldparam);
    }

    return 1;
} /* tls_import_dh_params() */

/*-------------------------------------------------------------------------*/
static void
initialize_tls_session (gnutls_session_t *session, Bool outgoing) __attribute__((nonnull));
static void
initialize_tls_session (gnutls_session_t *session, Bool outgoing)

/* GnuTLS: Initialise a TLS <session>.
 * tls_is_available must be TRUE.
 */

{
    gnutls_init(session, outgoing ? GNUTLS_CLIENT : GNUTLS_SERVER);

    /* avoid calling all the priority functions, since the defaults
     * are adequate.
     */
    gnutls_set_default_priority( *session);   

    gnutls_credentials_set( *session, GNUTLS_CRD_CERTIFICATE, x509_cred);

    gnutls_certificate_server_set_request( *session, GNUTLS_CERT_REQUEST);

    gnutls_dh_set_prime_bits( *session, DH_BITS);

    gnutls_session_set_ptr( *session, (void*)(intptr_t) current_key);
} /* initialize_tls_session() */

#if GNUTLS_VERSION_NUMBER < 0x030300
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
#endif // GNUTLS_VERSION_NUMBER

/*-------------------------------------------------------------------------*/
static gnutls_datum_t
tls_read_file (const char * fname, const char * desc)

/* Reads a file fully into memory.
 *
 * On error returns .data = NULL and logs the error
 * (with <desc> as the description of the file type).
 *
 * Otherwise .data contains the file's content in an xalloced memory block
 * (of .size bytes). It's the caller's responsibility to xfree it afterwards.
 */

{
    gnutls_datum_t result = { NULL, 0 };
    FILE * file;
    long fpos;

    file = fopen(fname, "rt");
    if (!file)
        return result;

    fseek(file, 0, SEEK_END);
    fpos = ftell(file);
    if (fpos < 0)
    {
        fclose(file);
        if (desc)
        {
            printf("%s TLS: Can't read %s: %s.\n"
                  , time_stamp(), desc, strerror(errno));
            debug_message("%s TLS: Can't read %s: %s\n"
                         , time_stamp(), desc, strerror(errno));
        }
        return result;
    }

    result.size = fpos;
    result.data = xalloc(result.size);
    if (result.data == NULL)
    {
        fclose(file);
        if (desc)
        {
            printf("%s TLS: Can't read %s: Out of memory.\n"
                  , time_stamp(), desc);
            debug_message("%s TLS: Can't read %s: Out of memory.\n"
                         , time_stamp(), desc);
        }
        return result;
    }

    fseek(file, 0, SEEK_SET);
    if (fread(result.data, 1, result.size, file) < result.size)
    {
        fclose(file);
        xfree(result.data);
        result.data = NULL;

        if (desc)
        {
            printf("%s TLS: Can't read %s: %s.\n"
                  , time_stamp(), desc, strerror(errno));
            debug_message("%s TLS: Can't read %s: %s\n"
                         , time_stamp(), desc, strerror(errno));
        }
        return result;
    }

    fclose(file);

    return result;
}

/*-------------------------------------------------------------------------*/
static Bool
tls_read_cert (int pos, const char * key, const char * cert)

/* Reads a key and certificate from the given files into keys[pos].
 * <key> and <cert> should be absolute filenames
 * (or relative to current i.e. mudlib directory).
 * cert may be NULL, then it will be read from the key file.
 *
 * Returns MY_TRUE on success.
 */

{
    int err;
    size_t fpsize;
    gnutls_datum_t data;

    /* Load the key. */

    data = tls_read_file(key, cert ? "key" : "key and certificate");
    if (data.data == NULL)
        return MY_FALSE;

    gnutls_x509_privkey_init(&keys[pos].key);
    err = gnutls_x509_privkey_import(keys[pos].key, &data, GNUTLS_X509_FMT_PEM);
    if (err < 0)
    {
        printf("%s TLS: Error loading x509 key from '%s': %s\n"
              , time_stamp(), key, gnutls_strerror(err));
        debug_message("%s TLS: Error loading x509 key from '%s': %s\n"
                     , time_stamp(), key, gnutls_strerror(err));
        gnutls_x509_privkey_deinit(keys[pos].key);
        xfree(data.data);
        return MY_FALSE;
    }

    /* Load the ceriticate chain. */

    if (cert)
    {
        /* The certificate has its own file. */
        xfree(data.data);
        data = tls_read_file(cert, "certificate");
        if (data.data == NULL)
        {
            gnutls_x509_privkey_deinit(keys[pos].key);
            return MY_FALSE;
        }
    }

    keys[pos].num_certs = MAX_CHAIN_LENGTH;
    err = gnutls_x509_crt_list_import(keys[pos].cert, &keys[pos].num_certs, &data, GNUTLS_X509_FMT_PEM, 
#if GNUTLS_VERSION_MAJOR >= 3
        GNUTLS_X509_CRT_LIST_FAIL_IF_UNSORTED | GNUTLS_X509_CRT_LIST_IMPORT_FAIL_IF_EXCEED
#else
        GNUTLS_X509_CRT_LIST_IMPORT_FAIL_IF_EXCEED
#endif
    );
    if (err < 0 || keys[pos].num_certs == 0)
    {
        printf("%s TLS: Error loading x509 certificate from '%s': %s\n"
              , time_stamp(), cert ? cert : key, gnutls_strerror(err));
        debug_message("%s TLS: Error loading x509 certificate from '%s': %s\n"
                     , time_stamp(), cert ? cert : key, gnutls_strerror(err));
        gnutls_x509_privkey_deinit(keys[pos].key);
        xfree(data.data);
        return MY_FALSE;
    }

    xfree(data.data);

    /* Generate and log its fingerprint. */

    fpsize = sizeof(keys[pos].fingerprint);
    err = gnutls_x509_crt_get_fingerprint(keys[pos].cert[0], GNUTLS_DIG_SHA1, keys[pos].fingerprint, &fpsize);
    if (err < 0)
    {
        printf("%s TLS: Error calculating fingerprint from '%s': %s\n"
              , time_stamp(), cert ? cert : key, gnutls_strerror(err));
        debug_message("%s TLS: Error calculating fingerprint from '%s': %s\n"
                     , time_stamp(), cert ? cert : key, gnutls_strerror(err));

        gnutls_x509_privkey_deinit(keys[pos].key);
        for (int i = 0; i < keys[pos].num_certs; ++i)
            gnutls_x509_crt_deinit(keys[pos].cert[i]);
        return MY_FALSE;
    }

    assert(fpsize == sizeof(keys[pos].fingerprint));

    printf("%s TLS: (GnuTLS) X509 certificate from '%s': "
          , time_stamp(), cert ? cert : key);
    debug_message("%s TLS: (GnuTLS) X509 certificate from '%s': "
                 , time_stamp(), cert ? cert : key);

    for (int i = 0; i < sizeof(keys[pos].fingerprint); ++i)
    {
        printf( i ? ":%02X" : "%02X", (unsigned char) keys[pos].fingerprint[i] );
        debug_message( i ? ":%02X" : "%02X", (unsigned char) keys[pos].fingerprint[i] );
    }

    printf("\n");
    debug_message("\n");

    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
static void
tls_free_keys (void)

/* Free all keys and certificates held in <keys>.
 */
{
    if (keys)
    {
        for (int i = 0; i < num_keys; ++i)
        {
            gnutls_x509_privkey_deinit(keys[i].key);
            for (int j = 0; j < keys[i].num_certs; ++j)
                gnutls_x509_crt_deinit(keys[i].cert[j]);
        }
        xfree(keys);
    }
}

/*-------------------------------------------------------------------------*/
void
tls_verify_init (void)

/* Initialize or reinitialize tls certificate storage and revocation lists.
 *
 * If there are no keys and certificates to be loaded, then this function
 * will keep the current keys, because there should be at least one key
 * to keep TLS working. We might still end up with no keys, if all files
 * are unreadable or contain no valid keys and certificates.
 * CAs and CRLs are cleared and reloaded in any case.
 */
{
    struct tls_dir_s dir;
    const char* fname;
    char oldfingerprint[sizeof(keys[0].fingerprint)];
    Bool havefingerprint = MY_FALSE;
    int num = 0;

    /* Remember the current key. */
    if (keys)
    {
        memcpy(oldfingerprint, keys[current_key].fingerprint, sizeof(oldfingerprint));
        havefingerprint = MY_TRUE;
    }
    current_key = 0;

    if (tls_opendir(tls_keydirectory, "key and certificate", &dir))
    {
        /* First get the number of pairs */
        while (tls_readdir(&dir) != NULL)
            num++;

    }
    if (tls_keyfile)
        num++;

    if (num)
    {
        tls_free_keys();

        num_keys = 0;
        keys = xalloc(sizeof(struct tls_key_s) * num);

        if (!keys)
        {
            printf("%s TLS: Error loading %d keys: Out of memory.\n"
                  , time_stamp(), num);
            debug_message("%s TLS: Error loading %d keys: Out of memory.\n"
                         , time_stamp(), num);
        }
    }

    if (num && keys)
    {
        if (tls_keyfile)
        {
            if (tls_read_cert(0, tls_keyfile, tls_certfile))
                num_keys++;
        }

        if (num_keys < num)
        {
            tls_opendir(tls_keydirectory, NULL, &dir);
            while ((fname = tls_readdir(&dir)) != NULL)
            {
                /* Some files mysteriously appeared,
                 * but we have to finish the tls_readdir loop.
                 */
                if (num_keys >= num)
                    continue;

                if (tls_read_cert(num_keys, fname, NULL))
                    num_keys++;
            }
        }

        /* Restore the old current key, if it's there.
         * Otherwise fail silently (then the first
         * found key will be the new current key).
         *
         * We do this silently because it's the usual
         * case with just a key file and no key directory.
         * In the other case the mudlib will hopefully
         * select the right key before the next TLS session starts.
         */
        if (havefingerprint)
            tls_set_certificate(oldfingerprint, sizeof(oldfingerprint));
    }

    /* CAs are reloaded in any case, even if there are no certificates there. */
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
    else if(tls_trustdirectory == NULL)
    {
        printf("%s TLS: (GnuTLS) Trusted x509 certificates locations not specified.\n"
              , time_stamp());
        debug_message("%s TLS: (GnuTLS) trusted x509 certificates locations not specified.\n"
                     , time_stamp());
    }

    tls_opendir(tls_trustdirectory, "trusted x509 certificates", &dir);
    while ((fname = tls_readdir(&dir)) != NULL)
    {
        int err;

        err = gnutls_certificate_set_x509_trust_file(x509_cred, fname, GNUTLS_X509_FMT_PEM);
        if (err < 0)
        {
            printf("%s TLS: Error setting x509 verification certificates from '%s': %s\n"
                  , time_stamp(), fname, gnutls_strerror(err));
            debug_message("%s TLS: Error setting x509 verification certificates from '%s': %s\n"
                         , time_stamp(), fname, gnutls_strerror(err));
        }
    }

    /* CRLs are also reloaded in any case, even if it's empty. */
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
    else if(!tls_crldirectory)
    {
        printf("%s TLS: (GnuTLS) CRL checking disabled.\n"
              , time_stamp());
        debug_message("%s TLS: (GnuTLS) CRL checking disabled.\n"
                     , time_stamp());
    }

    tls_opendir(tls_crldirectory, "CRLs", &dir);
    while ((fname = tls_readdir(&dir)) != NULL)
    {
        int err;

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

/*-------------------------------------------------------------------------*/
#if GNUTLS_VERSION_MAJOR >= 3 || (GNUTLS_VERSION_MAJOR == 2 && GNUTLS_VERSION_MINOR >= 11)
static int
tls_select_certificate (gnutls_session_t session
                       , const gnutls_datum_t *requested_dns
                       , int num_requested_dns
                       , const gnutls_pk_algorithm_t *available_algos
                       , int num_available_algos
                       , gnutls_retr2_st * st)

/* Called from GnuTLS to select a certificate and key to use.
 *
 * We store the index into our key list as the GnuTLS session
 * pointer (pointer for private use in a session object).
 * Note that it's really not a pointer, just the plain index.
 */

{
    int keyidx = (intptr_t) gnutls_session_get_ptr( session );

    if (!keys)
    {
        st->ncerts = 0;

        return 0;
    }

    /* There was a tls_refresh_certs() during handshake? */
    if (keyidx >= num_keys)
        keyidx = current_key;

    st->ncerts = keys[keyidx].num_certs;
    st->cert.x509 = keys[keyidx].cert;
    st->key.x509 = keys[keyidx].key;
    st->cert_type = GNUTLS_CRT_X509;
    st->key_type = GNUTLS_PRIVKEY_X509;
    st->deinit_all = 0;

    return 0;
}
#else
static int
tls_select_server_certificate (gnutls_session_t session
                              , gnutls_retr_st * st)

/* Called from GnuTLS to select a certificate and key to use.
 */

{
    int keyidx = (intptr_t) gnutls_session_get_ptr( session );

    if (!keys)
    {
        st->ncerts = 0;

        return 0;
    }

    /* There was a tls_refresh_certs() during handshake? */
    if (keyidx >= num_keys)
        keyidx = current_key;

    st->ncerts = keys[keyidx].num_certs;
    st->cert.x509 = keys[keyidx].cert;
    st->key.x509 = keys[keyidx].key;
    st->type = GNUTLS_CRT_X509;
    st->deinit_all = 0;

    return 0;
}

static int
tls_select_client_certificate (gnutls_session_t session
                              , const gnutls_datum_t *requested_dns
                              , int num_requested_dns
                              , const gnutls_pk_algorithm_t *available_algos
                              , int num_available_algos
                              , gnutls_retr_st * st)
{
    return tls_select_server_certificate(session, st);
}

#endif
/*-------------------------------------------------------------------------*/
Bool
tls_set_certificate (char *fingerprint, int len)

/* Sets the certificate used for the next sessions.
 * <fingerprint> contains the certificate's fingerprint as
 * <len> raw bytes. As long as we're using SHA1 <len>
 * should by 20.
 */

{
    if (len != sizeof(keys[0].fingerprint))
        return MY_FALSE;

    if (!keys)
        return MY_FALSE;

    for (int i = 0; i < num_keys; ++i)
        if (!memcmp(fingerprint, keys[i].fingerprint, len))
        {
            current_key = i;
            return MY_TRUE;
        }

    return MY_FALSE;
}

/*-------------------------------------------------------------------------*/
void
tls_global_init (void)

/* Initialise the TLS package; to be called once at program startup.
 */

{
    if (tls_keyfile == NULL && tls_keydirectory == NULL)
    {
        printf("%s TLS deactivated.\n", time_stamp());
        return;
    }

    keys = NULL;
    current_key = 0;

#if GNUTLS_VERSION_NUMBER < 0x030300
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
#endif

    if (strcmp(gnutls_check_version(NULL), GNUTLS_VERSION) != 0)
    {
        printf("%s TLS: The currently used version of GnuTLS (%s) "
               "differs from the one during compilation (%s). This "
               "might lead to problems.\n"
               , time_stamp(), gnutls_check_version(NULL), GNUTLS_VERSION );
        debug_message("%s TLS: The currently used version of OpenSSL (%s) "
               "differs from the one during compilation (%s). This "
               "might lead to problems.\n"
               , time_stamp(), gnutls_check_version(NULL), GNUTLS_VERSION );
    }
    // Check for decently recent version of gnutls (3.3.8 is currently in
    // Debian stable (2016-01-14)). Issue a warning only, we will still work
    // with old versions.
    if (!gnutls_check_version("3.3.8"))
    {
        printf("%s TLS: Detected outdated version of GnuTLS (%s). Please "
               "consider upgrading.\n"
              , time_stamp(), gnutls_check_version(NULL));
        debug_message("%s Detected outdated version of GnuTLS (%s). Please "
                      "consider upgrading."
                     , time_stamp(), gnutls_check_version(NULL));
    }

    gnutls_certificate_allocate_credentials(&x509_cred);

#if GNUTLS_VERSION_MAJOR >= 3 || (GNUTLS_VERSION_MAJOR == 2 && GNUTLS_VERSION_MINOR >= 11)
    gnutls_certificate_set_retrieve_function(x509_cred, &tls_select_certificate);
#else
    gnutls_certificate_client_set_retrieve_function(x509_cred, &tls_select_client_certificate);
    gnutls_certificate_server_set_retrieve_function(x509_cred, &tls_select_server_certificate);
#endif

    tls_verify_init();

    tls_import_dh_params(NULL, 0);

/* Would be nice, but setting this causes sporadic segfaults in
 * gnutls/nettle/libgmp on my system.
    // Set some priority default to favour strong ciphers and disable very
    // weak ones.
    char* err_pos=NULL;
    err=gnutls_priority_set_direct(x509_cred,
        "PFS:+SECURE128:-VERS-SSL3.0:-DHE-DSS:-3DES-CBC:-ARCFOUR-128:-MD5"
        ":-DES-CBC:%SERVER_PRECEDENCE"
        , &err_pos);
    if (err != GNUTLS_E_SUCCESS)
    {
        printf("%s TLS: Error setting priorities: %s at %s\n"
              , time_stamp(), gnutls_strerror(err)
              , (err_pos ? err_pos : "unknown" ));
        debug_message("%s Error setting priorities: %s at %s\n"
                     , time_stamp(), gnutls_strerror(err)
                     , (err_pos ? err_pos : "unknown" ));
    }
*/
    tls_is_available = MY_TRUE;

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
        gnutls_dh_params_deinit(*dh_params);

        tls_free_keys();
    }

#if GNUTLS_VERSION_NUMBER < 0x030300
    gnutls_global_deinit();
#endif

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
    initialize_tls_session(&ip->tls_session, ip->outgoing_conn);
    gnutls_transport_set_ptr(ip->tls_session, (gnutls_transport_ptr_t)(p_uint)(ip->socket));

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


#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
tls_clear_refs ()

/* GC Support: Clear all reference counts.
 */

{
    /* Nothing to do, we reference no ref-counted objects. */
} /* tls_clear_refs() */

/*-------------------------------------------------------------------------*/
void
tls_count_refs ()

/* GC Support: Mark all references to xalloc'ed memory.
 */

{
    if(keys)
        note_malloced_block_ref(keys);
} /* tls_count_refs() */

#endif /* GC_SUPPORT */

/***************************************************************************/
#endif /* USE_TLS && HAS_GNUTLS */
