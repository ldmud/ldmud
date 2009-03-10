/*------------------------------------------------------------------
 * IDNA Efuns.
 *
 *------------------------------------------------------------------
 * This file holds the efuns interfacing with the International
 * Domainname library.
 *
 *   efun: idna_to_ascii()
 *   efun: idna_to_unicode()
 *   efun: idna_stringprep()
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef HAS_IDN

#include "pkg-idna.h" 

#include <idna.h>
#include <stringprep.h>
#include <stdlib.h>

#include "typedefs.h"

#include "interpret.h"
#include "mstrings.h"
#include "simulate.h"
#include "xalloc.h"

#include "../mudlib/sys/idn.h"

/*-------------------------------------------------------------------------*/

/*=========================================================================*/

/*                           EFUNS                                         */

/*-------------------------------------------------------------------------*/
svalue_t *
f_idna_to_ascii (svalue_t *sp)

/* EFUN idna_to_ascii()
 *
 *     string idna_to_ascii (string name)
 *
 * Converts string name from utf-8 to idna representation (8z punycode)
 */

{
    char *out = NULL; 
    int rc;

    rc = idna_to_ascii_8z(get_txt(sp->u.str), &out, 0);

    free_svalue(sp);

    if (rc == IDNA_SUCCESS)
    {
        put_c_string(sp, out);
        free(out);
    }
    else
    {
        put_number(sp, -1);
        errorf("idna_to_ascii(): Error %s", idna_strerror(rc));
        /* NOTREACHED */
    }

    return sp;
} /* idna_to_ascii */

/*--------------------------------------------------------------------*/
svalue_t *
f_idna_to_unicode (svalue_t *sp)

/* EFUN idna_to_unicode()
 *
 *     string idna_to_unicode (string input)
 *
 * Converts string input from idna (punycode) to a utf-8 string 
 */
{
    char *out = NULL; 
    int rc;

    rc = idna_to_unicode_8z8z(get_txt(sp->u.str), &out, 0);

    free_svalue(sp);

    if (rc == IDNA_SUCCESS)
    {
        put_c_string(sp, out);
        free(out);
    }
    else
    {
        put_number(sp, -1);
        errorf("idna_to_unicode(): Error %s", idna_strerror(rc));
        /* NOTREACHED */
    }

    return sp;
} /* idna_to_unicode() */

/*--------------------------------------------------------------------*/
svalue_t *
f_idna_stringprep (svalue_t *sp)

/* EFUN idna_stringprep()
 *
 *   string idna_stringprep(string str, int profile, int flags = 0)
 *
 * Prepare the UTF-8 string str according to the stringprep profile
 * see also libidn stringprep(3)
 *
 * profile is one of the stringprep profiles defined in ldmuds idn.h
 * str is assumed to be in utf-8 charset (see convert_charset)
 * flags is one of the stringprep flags defined in LDMud's idn.h .
 */
{
    char *buf;
    size_t size;
    int flags = 0;
    int prof = 0; 
    int ret;
    const Stringprep_profile *profile;
    string_t *s;

    /* Get and check the flags. */
    {
        p_uint argflags = (p_uint)sp->u.number;
        sp--;

        if (argflags > (STRINGPREP_FLAG_MAX << 1)-1
         || argflags & (STRINGPREP_NO_NFKC_FLAG | STRINGPREP_NO_BIDI_FLAG)
           )
        {
            errorf("idna_stringprep(): Unsupported flag value %ld\n", (long)argflags);
            /* NOTREACHED */
            return sp;
        }

        flags = 0;
        if (argflags & STRINGPREP_NO_NFKC_FLAG) flags |= STRINGPREP_NO_NFKC;
        if (argflags & STRINGPREP_NO_BIDI_FLAG) flags |= STRINGPREP_NO_BIDI;
        if (argflags & STRINGPREP_NO_UNASSIGNED_FLAG) flags |= STRINGPREP_NO_UNASSIGNED;
    }

    /* Get and check the profile */
    prof = (int)sp->u.number;
    sp--;

    /* select the profile */
    switch(prof)
    {
    case STRINGPREP_NAMEPREP:
        profile = stringprep_nameprep;
        break;
    case STRINGPREP_SASLPREP:
        profile = stringprep_saslprep;
        break;
    case STRINGPREP_PLAIN:
        profile = stringprep_plain;
        break;
    case STRINGPREP_TRACE:
        profile = stringprep_trace;
        break;
    case STRINGPREP_KERBEROS5:
        profile = stringprep_kerberos5;
        break;
    case STRINGPREP_XMPP_NODEPREP:
        profile = stringprep_xmpp_nodeprep;
        break;
    case STRINGPREP_XMPP_RESOURCEPREP:
        profile = stringprep_xmpp_resourceprep;
        break;
    case STRINGPREP_ISCSI:
        profile = stringprep_iscsi;
        break;
    default:
        /* unknown profile */
        errorf("stringprep(): unknown profile %d.\n", prof);
        break; /* NOTREACHED */
    }

    /* Get the string */
    s = sp->u.str;

    /* this assumes that most strings will pass stringprep unchanged */
    size = mstrsize(s)+1; 
    memsafe(buf = xalloc(size), size, "stringprep buffer");

    memcpy(buf, get_txt(s), mstrsize(s));
    buf[mstrsize(s)] = 0;

    ret = stringprep(buf, size, flags, profile);
    while (ret == STRINGPREP_TOO_SMALL_BUFFER)
    {
        /* Increase the size until it fits. */
        /* TODO: Same pattern as in convert_charset() - make it a utility function?
         */
        size = size > 65536 ? (size + 33) : (2 * size);
        memsafe(buf = rexalloc(buf, size), size, "stringprep buffer");
        ret = stringprep(buf, size, flags, profile);
    }

    if (ret != STRINGPREP_OK)
    {
        errorf("stringprep(): Error %s", stringprep_strerror(ret));
        /* NOTREACHED */
    }
    else 
    {
        // free the string argument
        free_svalue(sp);
        put_c_string(sp, buf);
    }

    return sp;
} /* f_idna_stringprep */

/***************************************************************************/

#endif /* HAS_IDN */
