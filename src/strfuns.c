/*------------------------------------------------------------------
 * String functions.
 *
 *------------------------------------------------------------------
 * A collection of string related functions and utilities:
 *
 * strbuf_t: an extendable string buffer, useful for incremental
 *   construction of a string.
 * TODO: I am afraid the handling of length in _grow() itself and
 * TODO:: its calls is a bit too far on the conservative side.
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

#include "driver.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "strfuns.h"

#include "comm.h"
#include "datatypes.h"
#include "simulate.h"
#if 0 /* See below */
#define USES_SVALUE_STRLEN
#include "smalloc.h"
#endif

/*--------------------------------------------------------------------*/
void
strbuf_zero (strbuf_t *buf)

/* Initialise the given string buffer <buf>.
 */

{
    buf->alloc_len = 0;
    buf->length = 0;
    buf->buf = NULL;
}

/*--------------------------------------------------------------------*/
void
strbuf_free (strbuf_t *buf)

/* Free the given string buffer <buf>.
 * TODO: Not necessary once all strings are counted and with length?
 */

{
    if (buf->buf)
        xfree(buf->buf);
    strbuf_zero(buf);
}

/*--------------------------------------------------------------------*/
static INLINE size_t
strbuf_grow (strbuf_t *buf, size_t len)

/* Extend the stringbuffer <buf> to hold at least <len> more
 * bytes (ie. enough for a string of length <len>-1).
 *
 * Return <len> if all memory could be allocated, or a lower number
 * of only part of the required memory is available.
 *
 * N.B.: be careful with overflows when doing the checks.
 */

{
    size_t new_len;

    /* Catch some simple situations. */
    if (buf->alloc_len == USHRT_MAX)
        return 0;  /* Truncated */

    if (buf->alloc_len - buf->length > len)
        return len;

    /* Allocate more than we need in anticipation of further adds. */
    new_len = buf->length + len * 3;

    /* But don't allocate more than we can manage. */
    if (new_len > USHRT_MAX)
    {
        new_len = USHRT_MAX;
        if (new_len - buf->length < len)
            len = new_len - buf->length;
    }

    /* Is this the first allocation? */
    if (!buf->buf)
    {
        buf->buf = xalloc(new_len);
        buf->alloc_len = (ushort)new_len;
        buf->length = 0;
        *(buf->buf) = '\0';
        return len;
    }

    /* Extension of the existing buffer */

    /* Using malloc_increment_size() here is tempting, but somehow
     * allocates much bigger blocks than needed (or svalue_strlen()
     * is lying). TODO: Revisit this later.
     * Here is the code for now:
#ifdef MALLOC_smalloc
    char * new_buf;
    new_buf = malloc_increment_size(buf->buf, new_len - buf->alloc_len);
    if (new_buf)
    {
        buf->alloc_len = (ushort)new_len;
        return len;
    }
#endif
     */

    buf->buf = rexalloc(buf->buf, new_len);
    buf->alloc_len = (ushort)new_len;
    return len;
} /* strbuf_grow() */

/*--------------------------------------------------------------------*/
void
strbuf_add (strbuf_t *buf, char * text)

/* Add the <text> to string buffer <buf>.
 */

{
    size_t len;

    len = strlen(text) + 1;
    if (!len)
        return;

    if (len + buf->length > buf->alloc_len)
        len = strbuf_grow(buf, len);
    if (len)
    {
        memcpy(buf->buf+buf->length, text, len);
        buf->length += len-1;
        buf->buf[buf->length] = '\0';
    }
}  /* strbuf_add() */

/*--------------------------------------------------------------------*/
void
strbuf_addn (strbuf_t *buf, char * text, size_t len)

/* Add the <len> characters starting at <text> to string buffer <buf>.
 */

{
    if (!len)
        return;

    len += 1;
    if (len + buf->length > buf->alloc_len)
        len = strbuf_grow(buf, len);
    if (len)
    {
        len--;
        memcpy(buf->buf+buf->length, text, len);
        buf->length += len;
        buf->buf[buf->length] = '\0';
    }
}  /* strbuf_addn() */

/*--------------------------------------------------------------------*/
void
strbuf_addc (strbuf_t *buf, char ch)

/* Add the <ch>aracter to string buffer <buf>.
 */

{
    size_t len;

    len = 2;
    if (2 + buf->length > buf->alloc_len)
        len = strbuf_grow(buf, 2);
    if (len)
    {
        buf->buf[buf->length] = ch;
        buf->length++;
        buf->buf[buf->length] = '\0';
    }
}  /* strbuf_addc() */

/*--------------------------------------------------------------------*/
void
strbuf_addf (strbuf_t *buf, char * format, ...)

/* Create a string from <format> and the following arguments using
 * sprintf() rules, and add the result to the string buffer <buf>.
 */

{
    char tmpbuf[4096];
    va_list vargs;

    tmpbuf[sizeof tmpbuf - 1] = '\0';

    va_start(vargs, format);
    vsprintf(tmpbuf, format, vargs);
    va_end(vargs);

    if (tmpbuf[sizeof tmpbuf - 1])
        fatal("strbuf_addf: Internal buffer overflow.\n");

    strbuf_add(buf, tmpbuf);
}  /* strbuf_addf() */

/*--------------------------------------------------------------------*/
void
strbuf_send (strbuf_t *buf)

/* Send the string collected in <buf> out to the current user with
 * add_message(), and clear <buf>.
 */

{
    if (buf->buf && buf->length)
        add_message("%s", buf->buf);

    /* Empty the string buffer */
    if (buf->buf)
        xfree(buf->buf);
    buf->buf = NULL;
    buf->length = 0;
    buf->alloc_len = 0;
} /* strbuf_send() */

/*--------------------------------------------------------------------*/
void
strbuf_store (strbuf_t *buf, struct svalue *svp)

/* Store the string collected in <buf>, which may be the null string "",
 * into the empty svalue *<svp>, then clear <buf>.
 */

{
    svp->type = T_STRING;
    if (buf->buf && buf->length)
    {
        /* The buffer is most likely allocated longer than necessary,
         * and svalue_strlen() gets confused. Therefore create a fresh
         * copy.
         */
        svp->x.string_type = STRING_MALLOC;
        svp->u.string = xalloc((unsigned)(buf->length+1));
        memcpy(svp->u.string, buf->buf, (unsigned)(buf->length+1));
    }
    else
    {
        svp->x.string_type = STRING_VOLATILE;
        svp->u.string = "";
    }

    /* Empty the string buffer */
    if (buf->buf)
        xfree(buf->buf);
    buf->buf = NULL;
    buf->length = 0;
    buf->alloc_len = 0;
} /* strbuf_store() */

/*====================================================================*/

