/*------------------------------------------------------------------
 * Wrapper for the GCrypt library.
 *
 *------------------------------------------------------------------
 */
#include "driver.h"

#ifdef USE_GCRYPT

#include <stdarg.h>
#include <stdio.h>

#include <gcrypt.h>

#include "main.h"
#include "simulate.h"
#include "typedefs.h"
#include "xalloc.h"
#include "pkg-gcrypt.h"

#include "../mudlib/sys/tls.h"

/*-------------------------------------------------------------------------*/
static void *
gcrypt_xalloc (size_t size)

/* Wrapper function so that gcrypt will use the driver's allocator.
 * The wrapper is required as 'pxalloc' itself is a macro.
 */

{
    return pxalloc(size);
} /* tls_xalloc() */

/*-------------------------------------------------------------------------*/
static void *
gcrypt_rexalloc (void *old, size_t size)

/* Wrapper function so that gcrypt will use the driver's allocator.
 * The wrapper is required as 'prexalloc' itself is a macro.
 */

{
    return prexalloc(old, size);
} /* tls_rexalloc() */

/*-------------------------------------------------------------------------*/
static void
gcrypt_xfree (void *p)

/* Wrapper function so that gcrypt will use the driver's allocator.
 * The wrapper is not exactly required for pfree(),  but it keeps things
 * consistent.
 */

{
    return pfree(p);
} /* tls_xfree() */

/*-------------------------------------------------------------------------*/
static void
gcrypt_log_message (void *data UNUSED, int level UNUSED, const char* msg, va_list va_args)

/* Wrapper function that logs gcrypt messages.
 */

{
#ifdef __MWERKS__
#    pragma unused(data)
#    pragma unused(level)
#endif
    vprintf(msg, va_args);
    vdebug_message(msg, va_args);
} /* tls_xfree() */

/*-------------------------------------------------------------------------*/
Bool
pkg_gcrypt_init (void)

/* Initialize the gcrypt library.
 */

{
    if (!gcry_check_version(GCRYPT_VERSION))
    {
        printf("%s libgcrypt: version mismatch.\n", time_stamp());
        debug_message("%s libgcrypt: version mismatch.\n", time_stamp());

        return MY_FALSE;
    }

    /* We don't need secure memory and aren't root anyway. */
    gcry_control(GCRYCTL_DISABLE_SECMEM, 0);

    if (gcry_control(GCRYCTL_ANY_INITIALIZATION_P) == 0)
    {
        gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0);
        gcry_set_allocation_handler(gcrypt_xalloc, NULL, NULL,
            gcrypt_rexalloc, gcrypt_xfree);
        gcry_set_log_handler(gcrypt_log_message, NULL);
    }

    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
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
    case TLS_HASH_SHA1:
        (*md) = GCRY_MD_SHA1;
        break;
    case TLS_HASH_RIPEMD160:
        (*md) = GCRY_MD_RMD160;
        break;
    case TLS_HASH_MD5:
        (*md) = GCRY_MD_MD5;
        break;
    case TLS_HASH_SHA224:
        (*md) = GCRY_MD_SHA224;
        break;
    case TLS_HASH_SHA256:
        (*md) = GCRY_MD_SHA256;
        break;
    case TLS_HASH_SHA384:
        (*md) = GCRY_MD_SHA384;
        break;
    case TLS_HASH_SHA512:
        (*md) = GCRY_MD_SHA512;
        break;
    default:
        (*md) = GCRY_MD_NONE;
        return MY_FALSE;
    }

    if (gcry_md_test_algo(*md))
        return MY_FALSE;

    (*len) = gcry_md_get_algo_dlen(*md);
    if(!(*len))
        return MY_FALSE;

    return MY_TRUE;
} /* get_digest() */

/*-------------------------------------------------------------------------*/
void
calc_digest (digest_t md, void *dest, size_t destlen, void *msg, size_t msglen, void *key, size_t keylen)

/* Calculates the hash or the HMAC if <key> != NULL from <msg> as determined
 * by method <md> as it was returned by get_digest().
 */
{
    gcry_md_hd_t hd;
    gcry_error_t err;

    err = gcry_md_open(&hd, md, (key==NULL) ? 0 : GCRY_MD_FLAG_HMAC);
    if (err)
    {
        errorf((key==NULL)?"hash: %s/%s\n":"hmac: %s/%s\n"
              , gcry_strsource (err), gcry_strerror (err));
        /* NOTREACHED */
    }

    if(key)
    {
        err = gcry_md_setkey(hd, key, keylen);
        if (err)
        {
            errorf("hmac: %s/%s\n"
                  , gcry_strsource (err), gcry_strerror (err));
            /* NOTREACHED */
        }
    }

    gcry_md_write(hd, msg, msglen);
    gcry_md_final(hd);
    memcpy(dest, gcry_md_read(hd, md), destlen);
    gcry_md_close(hd);
} /* calc_digest() */

#endif /* USE_GCRYPT */

