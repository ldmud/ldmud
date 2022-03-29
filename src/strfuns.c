/*------------------------------------------------------------------
 * String functions.
 *
 *------------------------------------------------------------------
 * A collection of string related functions and utilities:
 *
 * xstrncpy(): a safer strncpy().
 * trim_all_spaces(): used in efun parse_command().
 *
 * strbuf_t: an extendable string buffer, useful for incremental
 *   construction of a string.
 * TODO: I am afraid the handling of length in _grow() itself and
 * TODO:: its calls is a bit too far on the conservative side.
 * TODO: Rewrite the strbuf_t to use a scatter-gather storing
 * TODO:: of data, to avoid allocations of large buffers (this happens
 * TODO:: when doing save/restore on large objects).
 *
 * --- Efuns and Operators ---
 *
 * v_to_bytes(): Convert unicode strings into a given encoding.
 * v_to_text(): Converts a given encoding to a unicode string.
 * intersect_strings(): Implements '&' and '-' on strings
 * x_filter_string(): Filter a string through a callback or mapping.
 * x_map_string(): Map a string through a callback or mapping.
 *
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <assert.h>

#include "strfuns.h"

#include "array.h"
#include "comm.h"
#include "iconv_opt.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "unidata.h"
#include "xalloc.h"

#include "i-current_object.h"

#define MAX_UNICODE_CHAR 0x10ffff
    /* Per definition the unicode character with the highest codepoint. */

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
    if (buf->alloc_len >= MAX_STRBUF_LEN)
        return 0;  /* Truncated */

    if (buf->alloc_len - buf->length > len)
        return len;

    /* Allocate more than we need in anticipation of further adds
     * (rounded to a multiple of the word size),
     * but not more than we can manage
     */
    if (MAX_STRBUF_LEN - buf->length < len * 3 + sizeof(void*)-1)
    {
        new_len = MAX_STRBUF_LEN;
        if (new_len - buf->length < len)
            len = new_len - buf->length;
    }
    else
        new_len = (buf->length + len * 3 + sizeof(void*)-1) & ~(sizeof(void*)-1);


    /* Is this the first allocation? */
    if (!buf->buf)
    {
        memsafe(buf->buf = xalloc(new_len), new_len, "new strbuf");
        buf->alloc_len = (u_long)new_len;
        buf->length = 0;
        *(buf->buf) = '\0';
        return len;
    }

    /* Extension of the existing buffer */

    memsafe(buf->buf = rexalloc(buf->buf, new_len), new_len, "larger strbuf");
    buf->alloc_len = (u_long)new_len;
    return len;
} /* strbuf_grow() */

/*--------------------------------------------------------------------*/
void
strbuf_add (strbuf_t *buf, const char * text)

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
strbuf_addn (strbuf_t *buf, const char * text, size_t len)

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
strbuf_addf (strbuf_t *buf, const char * format, ...)

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
    {
        add_message("%s", buf->buf);
    }

    /* Empty the string buffer */
    if (buf->buf)
        xfree(buf->buf);
    buf->buf = NULL;
    buf->length = 0;
    buf->alloc_len = 0;
} /* strbuf_send() */

/*--------------------------------------------------------------------*/
void
strbuf_store (strbuf_t *buf, svalue_t *svp)

/* Store the string collected in <buf>, which may be the null string "",
 * into the empty svalue *<svp>, then clear <buf>.
 */

{
    svp->type = T_STRING;
    if (buf->buf && buf->length)
    {
        svp->u.str = new_n_unicode_mstring(buf->buf, buf->length);
    }
    else
    {
        svp->u.str = ref_mstring(STR_EMPTY);
    }

    /* Empty the string buffer */
    if (buf->buf)
        xfree(buf->buf);
    buf->buf = NULL;
    buf->length = 0;
    buf->alloc_len = 0;
} /* strbuf_store() */

/*--------------------------------------------------------------------*/
void
strbuf_copy (strbuf_t *buf, char *cbuf)

/* Copy the string collected in <buf>, which may be the null string "",
 * into the buffer <cbuf> which must have been allocated by the caller
 * to a suitable size. The copied string will be terminated with a '\0'.
 */

{
    if (buf->buf && buf->length)
        memcpy(cbuf, buf->buf, buf->length);
    cbuf[buf->length] = '\0';
} /* strbuf_copy() */

/*--------------------------------------------------------------------*/
char *
xstrncpy (char * dest, const char * src, size_t num)

/* Copy string <src> at address <dest> up to and including the terminating
 * 0 or up to size <num>, whichever comes first. Result is <dest>.
 *
 * In contrast to strncpy(), the copying terminates if a terminating 0
 * is found (and copied) in <src> - strncpy() would add additional 0s
 * until a total of <num> characters has been written to <dest>.
 */

{
    char * p = dest;

    while (num-- != 0 && (*p++ = *src++) != '\0') NOOP;
    return dest;
} /* xstrncpy() */

/*-------------------------------------------------------------------------*/
string_t *
trim_all_spaces (const string_t * txt)

/* Trim the input string <txt> by removing all leading and trailing
 * space, and by folding embedded space runs into just one each.
 * Return the new string with one ref; the refcount of <txt> is not changed.
 *
 * Throw an error when out of memory.
 */

{
    char * dest;
    const char * src;
    size_t dest_ix, src_ix, srclen;
    string_t * rc;

    dest = alloca(mstrsize(txt));
    if (dest == NULL)
        errorf("Stack overflow (%zu bytes)\n", mstrsize(txt));

    src = get_txt((string_t *const)txt);
    srclen = mstrsize(txt);
    src_ix = 0;
    dest_ix = 0;

    /* Blank out trailing spaces */
    while (srclen > 0 && src[srclen-1] == ' ')
        srclen--;

    /* Skip leading spaces */
    while (src_ix < srclen && *src == ' ')
        src_ix++, src++;

    /* Copy characters, but fold embedded spaces. */
    for ( ; src_ix < srclen; src_ix++, src++, dest_ix++)
    {
        dest[dest_ix] = *src;

        /* If this and the next character is a space, forward
         * src until the last space in this run.
         */
        if (' ' == *src)
        {
            while (src_ix+1 < srclen && ' ' == src[1])
                src_ix++, src++;
        }
    }

    memsafe(rc = new_n_mstring(dest, dest_ix, txt->info.unicode), dest_ix, "trimmed result");
    return rc;
} /* trim_all_spaces() */

/*--------------------------------------------------------------------*/
size_t
get_escaped_character (p_int c, char* buf, size_t buflen)

/* Writes the character <c> into the <buf> which has space for
 * <buflen> bytes (<buflen> should be 10 or more). If the character
 * doesn't need escaping it is written as a regular character.
 * Returns the number of bytes written.
 */

{
    static const char hex[] = "0123456789abcdef";

    switch(c)
    {
        case '"':
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\\"", 2);
            return 2;

        case '\n':
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\n", 2);
            return 2;

        case '\r':
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\r", 2);
            return 2;

        case '\t':
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\t", 2);
            return 2;

        case '\a':
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\a", 2);
            return 2;

        case 0x1b:
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\e", 2);
            return 2;

        case 0x08:
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\b", 2);
            return 2;

        case 0x00:
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\0", 2);
            return 2;

        case '\\':
            if (buflen < 2)
                return 0;
            memcpy(buf, "\\\\", 2);
            return 2;

        default:
            if (c < 0x20)
            {
                if (buflen < 4)
                    return 0;

                *buf++ = '\\';
                *buf++ = 'x';
                *buf++ = hex[c >> 4];
                *buf++ = hex[c & 0xf];
                return 4;
            }
            else if (c < 0x7f)
            {
                if (!buflen)
                    return 0;

               *buf = (char)c;
               return 1;
            }
            else if (c < 0x10000)
            {
                if (buflen < 6)
                    return 0;

                *buf++ = '\\';
                *buf++ = 'u';
                *buf++ = hex[(c >> 12) & 0xf];
                *buf++ = hex[(c >> 8) & 0xf];
                *buf++ = hex[(c >> 4) & 0xf];
                *buf++ = hex[c & 0xf];
                return 6;
            }
            else
            {
                if (buflen < 10)
                    return 0;

                *buf++ = '\\';
                *buf++ = 'U';
                *buf++ = hex[(c >> 28) & 0xf];
                *buf++ = hex[(c >> 24) & 0xf];
                *buf++ = hex[(c >> 20) & 0xf];
                *buf++ = hex[(c >> 16) & 0xf];
                *buf++ = hex[(c >> 12) & 0xf];
                *buf++ = hex[(c >> 8) & 0xf];
                *buf++ = hex[(c >> 4) & 0xf];
                *buf++ = hex[c & 0xf];
                return 10;
            }
    }

    return 0;
} /* get_escaped_character() */

/*====================================================================*/

/*                          ENCODING                                  */

/*--------------------------------------------------------------------*/

size_t
byte_to_char_index (const char* text, size_t pos, bool* error)

/* Determines the character index in the string <text> at the
 * byte position <pos>. If there are errors in the encoding,
 * returns the position of the errorneous character and sets
 * the <error> flag (if not NULL).
 */

{
    const char* dest = text + pos;
    size_t idx = 0;

    if (error)
        *error = false;

    for (; text < dest; idx++)
    {
        unsigned char c = *(unsigned char*)(text++);

        if (!(c & 0x80))
            continue;

        /* Not a start byte or an invalid start byte? Abort. */
        if ((c & 0xc0) != 0xc0 ||
            c == 0xc0 || c == 0xc1 || c >= 0xf5)
        {
            if (error)
                *error = true;
            return idx;
        }

        /* Check the continuation bytes. */
        for (unsigned char bit = 0x20; bit > 0x04; bit >>= 1)
        {
            if (text == dest || ((*text++) & 0xc0) != 0x80)
            {
                if (error)
                    *error = true;
                return idx;
            }

            if (!(c & bit))
                break;
        }
    }

    return idx;
} /* byte_to_char_index() */

/*--------------------------------------------------------------------*/

size_t
char_to_byte_index (const char* text, size_t len, size_t pos, bool* error)

/* Determines the byte position of the character with index <pos>
 * in the text <text> with length <len>.
 * If there are errors in the encoding, returns the position of the
 * errorneous character and sets the <error> flag (if not NULL).
 */

{
    const char* dest = text;
    const char* end  = text + len;
    size_t idx = 0;

    if (error)
        *error = false;

    for (; idx < pos && dest < end; idx++)
    {
        unsigned char c = *(unsigned char*)(dest++);

        if (!(c & 0x80))
            continue;

        /* Not a start byte or an invalid start byte? Abort. */
        if ((c & 0xc0) != 0xc0 ||
            c == 0xc0 || c == 0xc1 || c >= 0xf5)
        {
            if (error)
                *error = true;
            return dest - text;
        }

        /* Check the continuation bytes. */
        for (unsigned char bit = 0x20; bit > 0x04; bit >>= 1)
        {
            if (dest == end || ((*dest++) & 0xc0) != 0x80)
            {
                if (error)
                    *error = true;
                return dest - text;
            }

            if (!(c & bit))
                break;
        }
    }

    return dest - text;
} /* char_to_byte_index() */

/*--------------------------------------------------------------------*/

bool
is_ascii (const char* text, size_t len)

/* Determines whether the given text only contains 7 bit ASCII characters.
 */

{
    for (const char* end = text + len; text != end; text++)
    {
        if ((*text) & 0x80)
            return false;
    }

    return true;
} /* is_ascii() */

/*--------------------------------------------------------------------*/

size_t
utf8_size (p_int code)

/* Determines the number of bytes needed for the unicode codepoint
 * in UTF-8 (or 0 for illegal codes).
 */

{
    if (!(code & ~0x7f))
        return 1;

    /* At most 3 continuation bytes are allowed. */
    for (int bytes = 1, bits = 11; bytes < 4; bytes++, bits+=5)
    {
        if (code & ~((1 << bits)-1))
            continue;

        return bytes + 1;
    }

    return 0;
} /* utf8_size() */

/*-------------------------------------------------------------------------*/

char*
utf8_prev (char* text, size_t pos)

/* Returns the pointer to the previous character at <text>.
 * It is assumed that <pos> bytes are in the string prior
 * to <text>.
 */

{
    do
    {
        if (!pos)
            return text;

        text--;
        pos--;
    } while (((*(unsigned char*)text) & 0xc0) == 0x80);

    return text;
} /* utf8_prev() */

/*--------------------------------------------------------------------*/

size_t
unicode_to_utf8 (p_int code, char* buf)

/* Converts a unicode codepoint to UTF-8.
 * <buf> needs to have at least 4 bytes of remaining space.
 * Returns the number of actually used bytes or
 * 0 for illegal codes.
 */

{
    if (!(code & ~0x7f))
    {
        *buf = code;
        return 1;
    }

    if (code < 0 || code > MAX_UNICODE_CHAR)
        return 0;

    /* At most 3 continuation bytes are allowed. */
    for (int bytes = 1, bits = 11; bytes < 4; bytes++, bits+=5)
    {
        /* Need more bits? */
        if (code & ~((1 << bits)-1))
            continue;

        for (int i = bytes; i > 0; i--)
        {
            buf[i] = 0x80 | (code & 0x3f);
            code >>= 6;
            bits -= 6;
        }

        buf[0] = code | ~((1 << (bits+1))-1);

        return bytes + 1;
    }

    return 0;
} /* unicode_to_utf8() */

/*--------------------------------------------------------------------*/

size_t
utf8_to_unicode (const char* buf, size_t len, p_int *code)

/* Converts a UTF-8 sequence to a unicode codepoint.
 * Returns the number of consumed bytes or
 * 0 for illegal or incomplete sequences.
 */

{
    unsigned char c = *(const unsigned char*)(buf++);

    if (len <= 0)
        return 0;

    if (!(c & 0x80))
    {
        *code = c;
        return 1;
    }

    /* Not a start byte or an invalid start byte? */
    if ((c & 0xc0) != 0xc0 ||
        c == 0xc0 || c == 0xc1 || c >= 0xf5)
    {
        return 0;
    }

    /* Read the continuation bytes. */
    for (unsigned char bit = 0x20, bytes = 1; bit > 0x04; bytes++, bit >>= 1)
    {
        if (bytes == len)
            return 0;

        if (((*buf++) & 0xc0) != 0x80)
            return 0;

        if (!(c & bit))
        {
            p_int result = c & (bit-1);

            buf -= bytes;
            for (int i = 0; i < bytes; i++, buf++)
                result = (result << 6) | (*buf & 0x3f);

            if (result > MAX_UNICODE_CHAR)
                return 0;

            *code = result;
            return bytes + 1;
        }
    }

    return 0; /* NOTREACHED. */
} /* utf8_to_unicode() */

/*--------------------------------------------------------------------*/

char*
get_illegal_sequence (char* buf, size_t len, iconv_t cd)

/* Return a string depicting an illegal sequence.
 * This function should be called when iconv with the given descriptor
 * return EILSEQ. It tries to detect the amount of illegal bytes
 * and returns a string with them as a byte sequence.
 */

{
    /* We try to convert a limited amount of bytes until we
     * receive EILSEQ as well.
     */
    for (size_t i = 1; i <= len; i++)
    {
        char tmp[8];
        char *inbuf = buf, *outbuf = tmp;
        size_t inleft = i, outleft = sizeof(tmp);

        size_t res = iconv(cd, &inbuf, &inleft, &outbuf, &outleft);
        assert(res == (size_t)-1);

        if (errno == EILSEQ || i == len)
        {
            static const char hex[] = "0123456789abcdef";
            static char result[36] = "\"";
            int pos = 1;

            /* Print at most 8 bytes. */
            if (i > 8)
                i = 8;

            for (size_t j = 0; j < i; j++)
            {
                unsigned char c = *buf++;
                result[pos++] = '\\';
                result[pos++] = 'x';
                result[pos++] = hex[c >> 4];
                result[pos++] = hex[c & 0xf];
            }

            result[pos++] = '"';
            result[pos] = 0;
            assert(pos < sizeof(result));
            return result;
        }

        assert(errno == EINVAL);
    }

    return "";
} /* get_illegal_sequence() */

/*====================================================================*/

/*                          GRAPHEMES                                 */

/*--------------------------------------------------------------------*/

static inline struct unicode_char_data_s*
get_unicode_char_data (p_int ch)

/* Return the entry from the unicode character database for <ch>.
 */

{
    return unicode_char_data + unicode_table2[unicode_table1[ch >> UNICODE_TABLE_SHIFT] + (ch & ((1 << UNICODE_TABLE_SHIFT)-1))];
}

/*--------------------------------------------------------------------*/

/* State machine for the Grapheme Breaks. A break between two codepoints is allowed,
 * whenever the resulting state is GRAPHEME_BREAK, otherwise the entry denotes the
 * new state which has to be looked up with the next codepoint.
 */
enum unicode_grapheme_cluster_break grapheme_state[GRAPHEME_BREAK][GRAPHEME_BREAK] =
{
    /* GRAPHEME_CONTROL */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_CARRIAGE_RETURN */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_LINE_FEED */
    {
        GRAPHEME_BREAK,
        GRAPHEME_CARRIAGE_RETURN,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_SPACING_MARK */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_EXTEND */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_PREPEND */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_PREPEND,
        GRAPHEME_HANGUL_SYLLABLE_L,
        GRAPHEME_HANGUL_SYLLABLE_V,
        GRAPHEME_HANGUL_SYLLABLE_LV,
        GRAPHEME_HANGUL_SYLLABLE_T,
        GRAPHEME_HANGUL_SYLLABLE_LVT,
        GRAPHEME_REGIONAL_INDICATOR,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_OTHER,
    },

    /* GRAPHEME_HANGUL_SYLLABLE_L */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_L,
        GRAPHEME_HANGUL_SYLLABLE_V,
        GRAPHEME_HANGUL_SYLLABLE_LV,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_LVT,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_HANGUL_SYLLABLE_V */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_V,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_T,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_HANGUL_SYLLABLE_LV */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_V,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_T,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_HANGUL_SYLLABLE_T */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_T,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_HANGUL_SYLLABLE_LVT */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_HANGUL_SYLLABLE_T,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_REGIONAL_INDICATOR */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_OTHER,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },

    /* GRAPHEME_ZERO_WIDTH_JOINER */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_PREPEND,
        GRAPHEME_HANGUL_SYLLABLE_L,
        GRAPHEME_HANGUL_SYLLABLE_V,
        GRAPHEME_HANGUL_SYLLABLE_LV,
        GRAPHEME_HANGUL_SYLLABLE_T,
        GRAPHEME_HANGUL_SYLLABLE_LVT,
        GRAPHEME_REGIONAL_INDICATOR,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_OTHER,
    },

    /* GRAPHEME_OTHER */
    {
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_SPACING_MARK,
        GRAPHEME_EXTEND,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_BREAK,
        GRAPHEME_ZERO_WIDTH_JOINER,
        GRAPHEME_BREAK,
    },
};

/*--------------------------------------------------------------------*/

size_t
next_grapheme_break (const char* str, size_t len, int* width)

/* Determines the position for the next break, i.e. the length of the
 * grapheme at the beginning of <str>.
 *
 * In <width> the terminal width of the grapheme will be returned.
 * There is no easy or standardized way to determine that, so we'll
 * just take the widest codepoint within the grapheme and return that.
 */

{
    size_t glen = 0;
    p_int ch;
    enum unicode_grapheme_cluster_break last_gcb;
    struct unicode_char_data_s* chdata;

    glen = utf8_to_unicode(str, len, &ch);
    if (!glen)
        return 0;
    chdata = get_unicode_char_data(ch);
    last_gcb = chdata->gcb;
    str += glen;
    len -= glen;
    *width = (p_int)chdata->width;

    if (ch == 27 || ch == 0x9a || ch == 0x9b)
    {
        /* Handle Escape sequences as a grapheme. */
        if (ch == 27)
        {
            size_t chlen = utf8_to_unicode(str, len, &ch);
            if (!chlen)
                return glen;
            glen += chlen;
            str += chlen;
            len -= chlen;
        }
        else if (ch == 0x9a)
            ch = '%';
        else if (ch == 0x9b)
            ch = '[';

        switch (ch)
        {
            case '%':   /* ROI */
            case '[':   /* CSI */
            {
                size_t pos = 0;
                while (pos < len && str[pos] >= 0x20 && str[pos] <= 0x3f)
                    pos++;
                glen += pos;
                str += pos;
                len -= pos;
                /* FALLTHROUGH */
            }

            case 'Z':   /* SCI */
            {
                /* A single additional character. */
                size_t chlen = utf8_to_unicode(str, len, &ch);
                glen += chlen;
                str += chlen;
                len -= chlen;
                break;
            }

            default:
                /* Just eat the next character. */
                break;
        }

        /* As control codes they are always a single grapheme. */
        return glen;
    }

    while (len)
    {
        /* Look at the next character. */
        enum unicode_grapheme_cluster_break cur_gcb;
        size_t chlen = utf8_to_unicode(str, len, &ch);
        if (!chlen)
            return glen;
        chdata = get_unicode_char_data(ch);
        cur_gcb = chdata->gcb;

        /* And walk through the state machine with state <last_gcb>
         * and transition <cur_gcb>.
         */
        last_gcb = grapheme_state[last_gcb][cur_gcb];
        if (last_gcb == GRAPHEME_BREAK)
            return glen;

        if ((p_int)chdata->width > *width)
            *width = (p_int)chdata->width;

        str += chlen;
        len -= chlen;
        glen += chlen;
    }

    return glen;
}

/*--------------------------------------------------------------------*/

int
get_string_width (const char* str, size_t len, bool* error)

/* Determines the displayed width of the given UTF-8 string.
 * If there are errors in the encoding returns the width up to the
 * faulty byte and sets the <error> flag (if not NULL).
 */

{
    int result = 0;

    while (len != 0)
    {
        int gwidth;
        size_t glen = next_grapheme_break(str, len, &gwidth);

        if (!glen)
        {
            if (error)
                *error = true;
            return result;
        }

        result += gwidth;
        str += glen;
        len -= glen;
    }

    if (error)
        *error = false;
    return result;
}


/*--------------------------------------------------------------------*/

size_t
get_string_up_to_width (const char* str, size_t len, int width, bool* error)

/* Determines the length (in bytes) for <str> that will not exceed
 * the given width. If there are errors in the encoding returns the length
 * up to the faulty byte and sets the <error> flag (if not NULL).
 */

{
    size_t result = 0;

    while (len != 0 && width >= 0)
    {
        int gwidth;
        size_t glen = next_grapheme_break(str, len, &gwidth);

        if (!glen)
        {
            if (error)
                *error = true;
            return result;
        }

        if (gwidth > width)
            break;

        result += glen;
        str += glen;
        len -= glen;
        width -= gwidth;
    }

    if (error)
        *error = false;
    return result;
}


/*====================================================================*/

/*                          EFUNS                                     */

/*--------------------------------------------------------------------*/

svalue_t *
v_to_bytes (svalue_t *sp, int num_arg)

/* EFUN to_bytes
 *
 *   string to_bytes(string unicode, string encoding)
 *   string to_bytes(int* characters, string encoding)
 *   string to_bytes(bytes bytesequence)
 *   string to_bytes(int* bytes)
 *
 * The first argument is converted to an encoded string.
 *
 * The first two variants convert a unicode string resp. a sequence
 * of unicode characters to a byte sequence that represents
 * the encoded string. The second argument denotes the name of
 * the encoding to use.
 *
 * The third variant just returns the argument if it's a byte string,
 * otherwise throws an error.
 *
 * The fourth variant converts an array of bytes to a byte string.
 */

{
    if (num_arg == 2)
    {
        /* We need to convert. */
        svalue_t* text = sp-1;
        string_t* result;

        if (text->type == T_POINTER)
        {
            /* So this is an array of unicode characters. */
            iconv_t cd;

            svalue_t* elem;
            svalue_t* end;
            size_t vec_size;
            size_t in_buf_left;

            char*  out_buf_ptr;
            char*  out_buf_start;
            size_t out_buf_size;
            size_t out_buf_left;

            /* For checking the endianness of the system. */
            p_int endian = 1;
            bool bigendian = !*((char*)&endian);

            /* In a 64 bit big endian system, we need an offset to get
             * to the least significat 32 bits.
             */
            size_t endianoffset = bigendian ? (sizeof(p_int) - 4) : 0;

            cd = iconv_open(get_txt(sp->u.str), bigendian ? "UTF-32BE" : "UTF-32LE");
            if (!iconv_valid(cd))
            {
                if (errno == EINVAL)
                    errorf("Bad arg 2 to to_bytes(): Unsupported encoding '%s'.\n", get_txt(sp->u.str));
                else
                    errorf("to_bytes(): %s\n", strerror(errno));
                return sp; /* NOTREACHED */
            }

            vec_size = VEC_SIZE(text->u.vec);
            elem = text->u.vec->item;
            end = elem + vec_size;

            if (vec_size == 0)
            {
                iconv_close(cd);

                /* We can't use STR_EMPTY, because that one is a unicode string. */
                memsafe(result = alloc_mstring(0), 0, "converted array");
                result->info.unicode = STRING_BYTES;

                sp = pop_n_elems(2, sp);
                push_bytes(sp, result);
                return sp;
            }

            /* For small texts, we reserve twice the space. */
            out_buf_left = out_buf_size = vec_size > 32768 ? (vec_size + 2048) : (2 * vec_size);
            xallocate(out_buf_start, out_buf_size, "conversion buffer");
            out_buf_ptr = out_buf_start;

            in_buf_left = 4;

            /* Convert the string, reallocating the output buffer where necessary */
            while (true)
            {
                size_t rc;
                bool at_end = elem == end;
                svalue_t *item = at_end ? NULL : get_rvalue(elem, NULL);

                /* We stop at non-numbers. */
                if (!at_end)
                    at_end = (item == NULL || item->type != T_NUMBER);

                /* At the end we need one final call. */
                if (at_end)
                    rc = iconv(cd, NULL, NULL, &out_buf_ptr, &out_buf_left);
                else
                {
                    char*  in_buf_ptr = ((char*) &item->u.number) + endianoffset + 4 - in_buf_left;

                    rc = iconv(cd, &in_buf_ptr, &in_buf_left, &out_buf_ptr, &out_buf_left);
                    if (in_buf_left == 0)
                    {
                        in_buf_left = 4;
                        elem++;
                    }
                }

                if (rc == (size_t)-1)
                {
                    if (errno == E2BIG)
                    {
                        /* Reallocate output buffer */
                        size_t new_size = out_buf_size + (vec_size > 128 ? vec_size : 128);
                        char* new_buf = rexalloc(out_buf_start, new_size);

                        if (!new_buf)
                        {
                            iconv_close(cd);

                            xfree(out_buf_start);
                            outofmem(new_size, "conversion buffer");
                            return sp; /* NOTREACHED */
                        }

                        out_buf_ptr   = new_buf + (out_buf_ptr - out_buf_start);
                        out_buf_start = new_buf;
                        out_buf_left  = out_buf_left + new_size - out_buf_size;
                        out_buf_size  = new_size;
                        continue;
                    }

                    /* Ignore EILSEQ at the end, they come from //IGNORE. */
                    if (errno == EILSEQ && !in_buf_left)
                        continue;

                    /* Other error: clean up */
                    iconv_close(cd);
                    xfree(out_buf_start);

                    if (errno == EILSEQ)
                        errorf("to_bytes(): Invalid character at index %zd.\n", elem - text->u.vec->item);

                    if (errno == EINVAL)
                        errorf("to_bytes(): Incomplete character at index %zd.\n", elem - text->u.vec->item);

                    errorf("to_bytes(): %s\n", strerror(errno));
                    return sp; /* NOTREACHED */
                }

                if (at_end)
                    break;
            }

            iconv_close(cd);

            result = new_n_mstring(out_buf_start, out_buf_ptr - out_buf_start, STRING_BYTES);
            xfree(out_buf_start);
            if (!result)
            {
                outofmem(out_buf_ptr - out_buf_start, "converted array");
                return sp; /* NOTREACHED */
            }
        }
        else if (text->type == T_STRING)
        {
            iconv_t cd;

            char*  in_buf_ptr;
            size_t in_buf_size;
            size_t in_buf_left;

            char*  out_buf_ptr;
            char*  out_buf_start;
            size_t out_buf_size;
            size_t out_buf_left;

            cd = iconv_open(get_txt(sp->u.str), "UTF-8");
            if (!iconv_valid(cd))
            {
                if (errno == EINVAL)
                    errorf("Bad arg 2 to to_bytes(): Unsupported encoding '%s'.\n", get_txt(sp->u.str));
                else
                    errorf("to_bytes(): %s\n", strerror(errno));
                return sp; /* NOTREACHED */
            }

            in_buf_ptr = get_txt(text->u.str);
            in_buf_left = in_buf_size = mstrsize(text->u.str);

            if (in_buf_size == 0)
            {
                iconv_close(cd);

                /* We can't use STR_EMPTY, because that one is a unicode string. */
                memsafe(result = alloc_mstring(0), 0, "converted array");
                result->info.unicode = STRING_BYTES;

                sp = pop_n_elems(2, sp);
                push_bytes(sp, result);
                return sp;
            }

            /* For small texts, we reserve twice the space. */
            out_buf_left = out_buf_size = in_buf_size > 32768 ? (in_buf_size + 2048) : (2 * in_buf_size);
            xallocate(out_buf_start, out_buf_size, "conversion buffer");
            out_buf_ptr = out_buf_start;

            /* Convert the string, reallocating the output buffer where necessary */
            while (true)
            {
                size_t rc;
                bool at_end = (in_buf_left == 0);

                /* At the end we need one final call. */
                if (at_end)
                    rc = iconv(cd, NULL, NULL, &out_buf_ptr, &out_buf_left);
                else
                    rc = iconv(cd, &in_buf_ptr, &in_buf_left, &out_buf_ptr, &out_buf_left);

                if (rc == (size_t)-1)
                {
                    size_t idx;
                    if (errno == E2BIG)
                    {
                        /* Reallocate output buffer */
                        size_t new_size = out_buf_size + (in_buf_size > 128 ? in_buf_size : 128);
                        char* new_buf = rexalloc(out_buf_start, new_size);

                        if (!new_buf)
                        {
                            iconv_close(cd);

                            xfree(out_buf_start);
                            outofmem(new_size, "conversion buffer");
                            return sp; /* NOTREACHED */
                        }

                        out_buf_ptr   = new_buf + (out_buf_ptr - out_buf_start);
                        out_buf_start = new_buf;
                        out_buf_left  = out_buf_left + new_size - out_buf_size;
                        out_buf_size  = new_size;
                        continue;
                    }

                    /* Ignore EILSEQ at the end, they come from //IGNORE. */
                    if (errno == EILSEQ && !in_buf_left)
                        continue;

                    /* Other error: clean up */
                    iconv_close(cd);
                    xfree(out_buf_start);

                    idx = byte_to_char_index(get_txt(text->u.str), in_buf_size - in_buf_left, NULL);
                    if (errno == EILSEQ)
                        errorf("to_bytes(): Invalid character sequence at index %zd.\n", idx);

                    if (errno == EINVAL)
                        errorf("to_bytes(): Incomplete character sequence at index %zd.\n", idx);

                    errorf("to_bytes(): %s\n", strerror(errno));
                    return sp; /* NOTREACHED */
                }

                if (at_end)
                    break;
            }

            iconv_close(cd);

            result = new_n_mstring(out_buf_start, out_buf_ptr - out_buf_start, STRING_BYTES);
            xfree(out_buf_start);
            if (!result)
            {
                outofmem(out_buf_ptr - out_buf_start, "converted string");
                return sp; /* NOTREACHED */
            }
        }
        else
            errorf("Bad arg 1 to to_bytes(): byte string and encoding given.\n");

        result->info.unicode = STRING_BYTES;
        sp = pop_n_elems(2, sp);
        push_bytes(sp, result);
        return sp;
    }
    else if (sp->type == T_POINTER)
    {
        /* An array of bytes convert to a byte string.
         * We do this until the first non-int, just like to_string().
         */

        string_t* result;
        char *ch;
        p_int size = VEC_SIZE(sp->u.vec);

        memsafe(result = alloc_mstring(size), size, "converted array");
        ch = get_txt(result);

        for (svalue_t *elem = sp->u.vec->item; size--; elem++, ch++)
        {
            svalue_t *item = get_rvalue(elem, NULL);
            if (item == NULL || item->type != T_NUMBER)
            {
                p_int newsize = ch - get_txt(result);
                memsafe(result = resize_mstring(result, newsize), newsize, "converted array");
                break;
            }

            *ch = (char)item->u.number;
        }

        result->info.unicode = STRING_BYTES;
        free_array(sp->u.vec);
        put_bytes(sp, result);

        return sp;
    }
    else if (sp->type == T_BYTES)
    {
        /* A byte string we just return. */
        return sp;
    }
    else
    {
        errorf("Bad arg 1 to to_bytes(): unicode string given without encoding.\n");
        return sp; /* NOTREACHED */
    }

} /* v_to_bytes() */

/*--------------------------------------------------------------------*/
svalue_t *
v_to_text (svalue_t *sp, int num_arg)

/* EFUN to_text
 *
 *   string to_text(bytes bytesequence, string encoding)
 *   string to_text(int* bytes, string encoding)
 *   string to_text(string unicode)
 *   string to_text(int* characters)
 *
 * The first argument is converted to a unicode string.
 *
 * The first two variants convert an encoded text, given as
 * a sequence of bytes, to string. The second argument denotes
 * the name of the encoding used to produce the byte sequence.
 *
 * The third variant just returns the argument if it's a
 * unicode string, otherwise throws an error
 *
 * The fourth variant converts a sequence of unicode characters
 * to string.
 */

{
    if (num_arg == 2)
    {
        /* We need to convert. */
        svalue_t* text = sp-1;
        string_t* result;

        iconv_t cd;

        char*  in_buf_start;
        char*  in_buf_ptr;
        size_t in_buf_size;
        size_t in_buf_left;

        char*  out_buf_ptr;
        char*  out_buf_start;
        size_t out_buf_size;
        size_t out_buf_left;

        if (text->type == T_POINTER)
        {
            /* We need to put these bytes into a byte sequence for iconv. */
            in_buf_size = VEC_SIZE(text->u.vec);
            if (in_buf_size == 0)
                in_buf_start = NULL;
            else
            {
                svalue_t *elem = text->u.vec->item;
                svalue_t *end = elem + in_buf_size;
                char *ch;

                xallocate(in_buf_start, in_buf_size, "conversion buffer");
                ch = in_buf_start;

                for (; elem != end; elem++, ch++)
                {
                    svalue_t *item = get_rvalue(elem, NULL);
                    if (item == NULL || item->type != T_NUMBER)
                    {
                        /* We stop here. */
                        in_buf_size = ch - in_buf_start;
                        break;
                    }

                    *ch = (char)item->u.number;
                }
            }
        }
        else if (text->type == T_BYTES)
        {
            in_buf_start = get_txt(text->u.str);
            in_buf_size = mstrsize(text->u.str);
        }
        else
            errorf("Bad arg 1 to to_text(): unicode string and encoding given.\n");

        cd = iconv_open("UTF-8", get_txt(sp->u.str));
        if (!iconv_valid(cd))
        {
            if (errno == EINVAL)
                errorf("Bad arg 2 to to_text(): Unsupported encoding '%s'.\n", get_txt(sp->u.str));
            else
                errorf("to_text(): %s\n", strerror(errno));
            return sp; /* NOTREACHED */
        }

        in_buf_ptr = in_buf_start;
        in_buf_left = in_buf_size;

        if (in_buf_size == 0)
        {
            iconv_close(cd);

            sp = pop_n_elems(2, sp);
            push_ref_string(sp, STR_EMPTY);
            return sp;
        }

        /* For small texts, we reserve twice the space. */
        out_buf_left = out_buf_size = in_buf_size > 32768 ? (in_buf_size + 2048) : (2 * in_buf_size);
        xallocate(out_buf_start, out_buf_size, "conversion buffer");
        out_buf_ptr = out_buf_start;

        /* Convert the string, reallocating the output buffer where necessary */
        while (true)
        {
            size_t rc;
            bool at_end = (in_buf_left == 0);

            /* At the end we need one final call. */
            if (at_end)
                rc = iconv(cd, NULL, NULL, &out_buf_ptr, &out_buf_left);
            else
                rc = iconv(cd, &in_buf_ptr, &in_buf_left, &out_buf_ptr, &out_buf_left);

            if (rc == (size_t)-1)
            {
                size_t idx;
                if (errno == E2BIG)
                {
                    /* Reallocate output buffer */
                    size_t new_size = out_buf_size + (in_buf_size > 128 ? in_buf_size : 128);
                    char* new_buf = rexalloc(out_buf_start, new_size);

                    if (!new_buf)
                    {
                        iconv_close(cd);

                        xfree(out_buf_start);
                        if (text->type == T_POINTER)
                            xfree(in_buf_start);
                        outofmem(new_size, "conversion buffer");
                        return sp; /* NOTREACHED */
                    }

                    out_buf_ptr   = new_buf + (out_buf_ptr - out_buf_start);
                    out_buf_start = new_buf;
                    out_buf_left  = out_buf_left + new_size - out_buf_size;
                    out_buf_size  = new_size;
                    continue;
                }

                idx = in_buf_size - in_buf_left;
                if (errno == EILSEQ)
                {
                    if (at_end)
                    {
                        iconv_close(cd);
                        xfree(out_buf_start);
                        if (text->type == T_POINTER)
                            xfree(in_buf_start);
                        errorf("to_text(): Invalid character sequence at byte %zd.\n", idx);
                    }
                    else
                    {
                        char* errseq = get_illegal_sequence(in_buf_ptr, in_buf_left, cd);
                        char context[128];
                        size_t pos = sizeof(context);

                        context[--pos] = 0;
                        context[--pos] = '"';

                        for (int contextlen = 0; contextlen < 10 && out_buf_ptr > out_buf_start; )
                        {
                            char escbuf[16];
                            char *prev = utf8_prev(out_buf_ptr, out_buf_ptr - out_buf_start);
                            p_int c;
                            size_t clen = utf8_to_unicode(prev, out_buf_ptr - prev, &c);
                            size_t esclen;

                            if (!clen)
                                c = *(unsigned char*)prev;

                            out_buf_ptr = prev;
                            contextlen++;

                            esclen = get_escaped_character(c, escbuf, sizeof(escbuf));
                            if (esclen && esclen < pos)
                            {
                                pos -= esclen;
                                memcpy(context + pos, escbuf, esclen);
                            }
                        }

                        context[--pos] = '"';

                        iconv_close(cd);
                        xfree(out_buf_start);
                        if (text->type == T_POINTER)
                            xfree(in_buf_start);

                        errorf("to_text(): Invalid character sequence at byte %zd after %s: %s.\n"
                              , idx
                              , context + pos
                              , errseq);
                    }
                }

                /* Other error: clean up */
                iconv_close(cd);
                xfree(out_buf_start);
                if (text->type == T_POINTER)
                    xfree(in_buf_start);

                if (errno == EINVAL)
                    errorf("to_text(): Incomplete character sequence at byte %zd.\n", idx);

                errorf("to_text(): %s\n", strerror(errno));
                return sp; /* NOTREACHED */
            }

            if (at_end)
                break;
        }

        iconv_close(cd);

        result = new_n_unicode_mstring(out_buf_start, out_buf_ptr - out_buf_start);
        xfree(out_buf_start);
        if (text->type == T_POINTER)
            xfree(in_buf_start);

        if (!result)
        {
            outofmem(out_buf_ptr - out_buf_start, "converted string");
            return sp; /* NOTREACHED */
        }

        sp = pop_n_elems(2, sp);
        push_string(sp, result);
        return sp;
    }
    else if (sp->type == T_POINTER)
    {
        /* An array of unicode characters convert to a UTF-8 string.
         * We do this until the first non-int, just like to_string().
         */

        char* out_buf_start;
        char* out_buf_ptr;
        size_t out_buf_size;
        size_t out_buf_left;

        bool ascii = true;
        string_t* result;

        p_int size = VEC_SIZE(sp->u.vec);

        if (size == 0)
        {
            free_array(sp->u.vec);
            put_ref_string(sp, STR_EMPTY);
            return sp;
        }

        out_buf_left = out_buf_size = size > 32768 ? (size + 2048) : (2 * size + 4);
        xallocate(out_buf_start, out_buf_size, "conversion buffer");
        out_buf_ptr = out_buf_start;

        for (svalue_t *elem = sp->u.vec->item; size--; elem++)
        {
            svalue_t *item = get_rvalue(elem, NULL);
            size_t added;

            if (item == NULL || item->type != T_NUMBER)
                break;

            if (out_buf_left < 4)
            {
                /* Reallocate output buffer */
                size_t new_size = out_buf_size + (size > 128 ? size : 128);
                char* new_buf = rexalloc(out_buf_start, new_size);

                if (!new_buf)
                {
                    xfree(out_buf_start);
                    outofmem(new_size, "conversion buffer");
                    return sp; /* NOTREACHED */
                }

                out_buf_ptr   = new_buf + (out_buf_ptr - out_buf_start);
                out_buf_start = new_buf;
                out_buf_left  = out_buf_left + new_size - out_buf_size;
                out_buf_size  = new_size;
            }

            added = unicode_to_utf8(item->u.number, out_buf_ptr);
            if (!added)
            {
                xfree(out_buf_start);
                errorf("to_text(): Invalid character at index %zd.\n", elem - sp->u.vec->item);
                return sp; /* NOTREACHED */
            }

            out_buf_ptr += added;
            out_buf_left -= added;
            if (added > 1)
                ascii = false;
        }

        result = new_n_mstring(out_buf_start, out_buf_ptr - out_buf_start, ascii ? STRING_ASCII : STRING_UTF8);
        xfree(out_buf_start);

        free_array(sp->u.vec);
        put_string(sp, result);
        return sp;
    }
    else if (sp->type == T_STRING)
    {
        /* A unicode string we just return. */
        return sp;
    }
    else
    {
        errorf("Bad arg 1 to to_text(): byte string given without encoding.\n");
        return sp;
    }

} /* v_to_text() */

/*--------------------------------------------------------------------*/
static p_int *
sort_string (const string_t * p_in, size_t size, size_t *plen, long ** pos)

/* Sort the characters of string <in> (with length <size> bytes) by
 * their numeric values and return a newly allocated memory block
 * with the sorted numeric values.
 * The number of characters is returned in <len>.
 * If <pos> is not NULL, it will be set to a newly allocated memory block
 * giving the original positions of the characters in the sorted string.
 * The string must not be empty.
 *
 * We use Mergesort to sort the strings.
 * TODO: Use Quicksort instead of Mergesort?
 */

{
    const char * in;  /* Input string */
    p_int  * out;     /* Result array */
    long   * outpos;  /* Result position array */
    p_int  * tmp;     /* Temporary array */
    long   * tmppos;  /* Temporary position array */
    size_t   len;     /* The number of characters. */
    size_t   step;
    size_t   i, j;

    in = get_txt((string_t *const)p_in);
    out = xalloc(size * sizeof(p_int));
    tmp = xalloc(size * sizeof(p_int));
    if (!out || !tmp)
    {
        if (out)
            xfree(out);
        if (tmp)
            xfree(tmp);
        errorf("(sort_string) Out of memory (2 * %zu bytes) for temporaries.\n"
             , size);
    }

    if (pos)
    {
        outpos = xalloc(size * sizeof(*outpos));
        tmppos = xalloc(size * sizeof(*outpos));
        if (!outpos || !tmppos)
        {
            if (out)
                xfree(out);
            if (tmp)
                xfree(tmp);
            if (outpos)
                xfree(outpos);
            if (tmppos)
                xfree(tmppos);
            errorf("(sort_string) Out of memory (2 * %zu bytes) for positions.\n"
                 , size*sizeof(*outpos));
        }
    }
    else
    {
        outpos = NULL;
        tmppos = NULL;
    }

    /* Initialize the out arrays. */
    if (p_in->info.unicode == STRING_UTF8)
    {
        for (i = 0, len = 0; i < size; len++)
        {
            step = utf8_to_unicode(in + i, size - i, out + len);
            if (!step)
            {
                if (out)
                    xfree(out);
                if (tmp)
                    xfree(tmp);
                if (outpos)
                    xfree(outpos);
                if (tmppos)
                    xfree(tmppos);
                errorf("Invalid character in string at index %zd.\n", len);
            }
            if (outpos)
                outpos[len] = len;
            i += step;
        } /* for(initial pass) */
    }
    else
    {
        for (i = 0; i < size; i++)
        {
            out[i] = ((unsigned char*)in)[i];
            if (outpos)
                outpos[i] = i;
        } /* for(initial pass) */

        len = size;
    }

    *plen = len;

    /* Mergesort loop: perform the mergesort passes with increasing steps.
     * Invariant: out is the (semi-sorted) data, tmp is the scratchspace.
     */
    for (step = 1; step < len; step *= 2)
    {
        size_t start, dest, left;

        /* Exchange out and tmp */
        {
            p_int *tmp2;
            long *tmp2pos;

            tmp2 = out; out = tmp; tmp = tmp2;
            if (outpos)
            {
                tmp2pos = outpos; outpos = tmppos; tmppos = tmp2pos;
            }
        }

        for (start = 0, dest = 0; start <= len; start += 2*step)
        {
            for ( i = start, j = start+step, left = 2 * step
                ; left && dest < len
                ; left--, dest++
                )
            {
                if (i >= start+step
                 || i >= len)
                {
                    if (j < len)
                    {
                        out[dest] = tmp[j];
                        if (outpos)
                            outpos[dest] = tmppos[j];
                        j++;
                    }
                }
                else if (j >= start+2*step
                      || j >= len)
                {
                    if (i < len)
                    {
                        out[dest] = tmp[i];
                        if (outpos)
                            outpos[dest] = tmppos[i];
                        i++;
                    }
                }
                else if (tmp[i] <= tmp[j])
                {
                    out[dest] = tmp[i];
                    if (outpos)
                        outpos[dest] = tmppos[i];
                    i++;
                }
                else /* (tmp[i] > tmp[i+step]) */
                {
                    out[dest] = tmp[j];
                    if (outpos)
                        outpos[dest] = tmppos[j];
                    j++;
                }
            } /* for (sort run) */
        } /* for (start) */
    } /* for(step) */

    /* Free the temporary data */
    if (tmppos)
        xfree(tmppos);
    xfree(tmp);

    /* Return the result */
    if (pos)
        *pos = outpos;

    return out;
} /* sort_string() */

/*--------------------------------------------------------------------*/
string_t *
intersect_strings (string_t * p_left, string_t * p_right, Bool bSubtract)

/* !bSubtract: Intersect string <left> with string <right> and return
 *   a newly allocated string with all those characters which are in
 *   both strings.
 * bSubtract:  Subtract string <right> from string <left> and return
 *   a newly allocated string with all those characters which are in
 *   <left> but not in <right>.
 * The order of the characters returned is their order of appearance
 * in <left>.
 */

{
    size_t   len_left, len_right, size_left, size_right, size_out;
    size_t   ix_left, ix_right;
    long   * pos, * rpos;
    CBool  * matches;
    p_int * left, * right;
    char  * result_txt;
    string_t *result;
    bool     utf8_left, utf8_out;

    size_left = mstrsize(p_left);
    size_right = mstrsize(p_right);
    utf8_left = p_left->info.unicode == STRING_UTF8;

    if (!size_left)
        return ref_mstring(p_left);
    if (!size_right)
        return ref_mstring(bSubtract ? p_left : p_right);

    /* Sort the two strings */
    left = sort_string(p_left, size_left, &len_left, &pos);
    right = sort_string(p_right, size_right, &len_right, NULL);

    xallocate(matches, size_left, "intersection matches");

    for (ix_left = 0; ix_left < size_left; ix_left++)
        matches[ix_left] = bSubtract ? MY_TRUE : MY_FALSE;

    rpos = xalloc(sizeof(long) * len_left);
    if (rpos == NULL)
    {
        xfree(pos);
        xfree(matches);
        xfree(left);
        xfree(right);

        errorf("(intersect_strings) Out of memory (%zu bytes) for temporaries.\n", sizeof(long) * len_left);
    }

    /* Intersect the two strings by mutual comparison.
     * Each non-matched character in left gets is pos[] set to -1.
     */
    for ( ix_left = 0, ix_right = 0
        ; ix_left < len_left && ix_right < len_right
        ; )
    {
        if (left[ix_left] < right[ix_right])
            ix_left++;
        else if (left[ix_left] > right[ix_right])
            ix_right++;
        else /* left[ix_left] == right[ix_right]) */
        {
            if (!bSubtract)
                matches[pos[ix_left]] = MY_TRUE;
            else
                matches[pos[ix_left]] = MY_FALSE;

            ix_left++;
        }
    }

    /* Calculate the resulting size. */
    size_out = 0;
    utf8_out = false;
    for (ix_left = 0; ix_left < len_left; ix_left++)
    {
        rpos[pos[ix_left]] = ix_left;

        if (matches[pos[ix_left]])
        {
            if (utf8_left)
            {
                size_t chlen = utf8_size(left[ix_left]);
                size_out += chlen;

                if (chlen > 1)
                    utf8_out = true;
            }
            else
                size_out++;
        }
    }

    /* Create the result: copy all flagged characters */
    memsafe(result = alloc_mstring(size_out), size_out, "intersection result");
    if (p_left->info.unicode == STRING_BYTES)
        result->info.unicode = STRING_BYTES;
    else
        result->info.unicode = utf8_out ? STRING_UTF8 : STRING_ASCII;

    result_txt = get_txt(result);
    for (ix_left = 0, ix_right = 0; ix_left < len_left; ix_left++)
        if (matches[ix_left])
        {
            if (utf8_out)
            {
                size_t chlen = unicode_to_utf8(left[rpos[ix_left]], result_txt + ix_right);
                ix_right += chlen;
            }
            else
                result_txt[ix_right++] = left[rpos[ix_left]];
        }

    /* Free intermediate results */
    xfree(pos);
    xfree(rpos);
    xfree(matches);
    xfree(left);
    xfree(right);

    return result;
} /* intersect_strings() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_filter_string (svalue_t *sp, int num_arg)

/* EFUN: filter() for strings.
 *
 *   string filter(string arr, string fun, string|object obj, mixed extra, ...)
 *   string filter(string arr, closure cl, mixed extra, ...)
 *   string filter(string arr, mapping map)
 *
 * Filter the elements of <arr> through a filter defined by the other
 * arguments, and return an array of those elements, for which the
 * filter yields non-zero.
 *
 * The filter can be a function call:
 *
 *    <obj>-><fun>(elem, <extra>...)
 *
 * or a mapping query:
 *
 *    <map>[elem]
 *
 * <obj> can both be an object reference or a filename. If omitted,
 * this_object() is used (this also works if the third argument is
 * neither a string nor an object).
 */

{
    string_t *rc;     /* Result string */
    string_t *str;    /* Argument string  */
    svalue_t *arg;    /* First argument the vm stack */
    mp_int    slen;   /* Argument string length */
    char     *src, *dest; /* String text work pointers */

    char     *flags;  /* Flag array, one flag for each element of <str>. */
    mp_int    res;    /* Final length in bytes. */
    bool      utf8_out = false; /* Whether the result has non-ASCII chars. */
    bool      utf8_in;          /* Whether the source has non-ASCII chars. */

    res = 0;

    /* Locate the args on the stack, extract the string to filter
     * and allocate the flags vector.
     */
    arg = sp - num_arg + 1;

    str = arg->u.str;
    slen = (mp_int)mstrsize(str);
    utf8_in = str->info.unicode == STRING_UTF8;

    /* Every element in flags is associated by index number with an
     * element in the vector to filter. The filter function is evaluated
     * for every string character, and the associated flag is set to 0
     * or 1 according to the result.
     * At the end, all 1-flagged elements are gathered and copied
     * into the result string.
     */

    if (arg[1].type == T_MAPPING)
    {
        mp_int cnt;
        size_t pos;

        /* --- Filter by mapping query --- */
        mapping_t *m;

        if (num_arg > 2) {
            errorf("Too many arguments to filter(array)\n");
        }
        /* Allocate memory for the flag array. Simultaneously an error
         * handler is pushed onto the stack (after the arguments) for freeing
         * the buffer in case of runtime errors. */
        flags = xalloc_with_error_handler((size_t)slen + 1);
        if (!flags)
        {
          errorf("Out of memory (%zu bytes) for temporary buffer in filter().\n",
                 (size_t)slen + 1);
        }
        sp = inter_sp;

        m = arg[1].u.map;
        
        for (src = get_txt(str), cnt = 0, pos = 0; pos < slen; cnt++)
        {
            svalue_t key = { T_NUMBER };
            size_t chlen;

            if (utf8_in)
            {
                chlen = utf8_to_unicode(src, slen - pos, &(key.u.number));
                if (!chlen)
                    errorf("Invalid character in string at index %zd.\n", pos);
            }
            else
            {
                chlen = 1;
                key.u.number = *(unsigned char*)src;
            }

            pos += chlen;
            src += chlen;

            if (get_map_value(m, &key) == &const0)
            {
                flags[cnt] = 0;
                continue;
            }
            flags[cnt] = 1;
            res += chlen;
        }

    }
    else
    {

        /* --- Filter by function call --- */

        int         error_index;
        callback_t *cb;
        mp_int cnt;
        size_t pos;

        assign_eval_cost();
        inter_sp = sp;

        /* setup_efun_callback() will adopt and therefore remove the 
         * arguments from arg+1 on to arg+num_arg from the stack and update 
         * inter_sp. New top-of-stack will be arg. */
        error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
        if (error_index >= 0)
        {
            vefun_bad_arg(error_index+2, arg);
            /* NOTREACHED */
            return arg;
        }
        /* push the callback structure onto the stack. */
        sp = arg + 1;
        put_callback(sp, cb);

        /* Allocate memory for the flag array. Simultaneously an error
         * handler is pushed onto the stack (after the arguments) for freeing
         * the buffer in case of runtime errors. */
        inter_sp = sp;
        flags = xalloc_with_error_handler((size_t)slen + 1);
        if (!flags)
        {
            errorf("Out of memory (%"PRIdMPINT" bytes) for temporary buffer "
                "in filter().\n", slen + 1);
        }
        sp = inter_sp;
        
        /* Loop over all elements in p and call the filter.
         * w is the current element filtered.
         */
        for (src = get_txt(str), cnt = 0, pos = 0; pos < slen; cnt++)
        {
            p_int num;
            size_t chlen;
            svalue_t *v;

            if (utf8_in)
            {
                chlen = utf8_to_unicode(src, slen - pos, &num);
                if (!chlen)
                    errorf("Invalid character in string at index %zd.\n", pos);
            }
            else
            {
                chlen = 1;
                num = *(unsigned char*)src;
            }

            pos += chlen;
            src += chlen;

            flags[cnt] = 0;

            if (is_current_object_destructed())
                continue;
                /* Don't call the filter anymore, but fill the
                 * flags array with 0es.
                 */

            if (!valid_callback_object(cb))
            {
                inter_sp = sp;
                errorf("object used by filter(array) destructed");
            }

            push_number(inter_sp, num);

            v = apply_callback(cb, 1);
            if (!v || (v->type == T_NUMBER && !v->u.number) )
                continue;

            flags[cnt] = 1;
            res += chlen;
        }
    }

    /* flags[] holds the filter results, res is the resulting
     * string length in bytes. Now create that string.
     */
    rc = alloc_mstring(res);
    if (!rc)
    {
        errorf("Out of memory (%"PRIdMPINT" bytes) for result in filter().\n",
            slen+1);
    }
  
    for (src = get_txt(str), dest = get_txt(rc); res > 0; flags++)
    {
        p_int num;
        size_t chlen = 1;

        if (utf8_in)
            chlen = utf8_to_unicode(src, 4, &num);
        else
            num = *(unsigned char*)src;

        if (*flags)
        {
            memcpy(dest, src, chlen);
            dest += chlen;
            res -= chlen;

            if (chlen > 1)
                utf8_out = true;
        }
        src += chlen;
    }

    rc->info.unicode = utf8_out ? STRING_UTF8 : (str->info.unicode == STRING_BYTES) ? STRING_BYTES : STRING_ASCII;

    assert(dest == get_txt(rc) + mstrsize(rc));

    /* Cleanup. Arguments for the closure have already been removed. On the
     * stack are now the string, the mapping or callback structure and the
     * error handler. (Not using pop_n_elems() for 2 elements for saving loop 
     * and function call overhead.) */
    free_svalue(sp--);  /* errorhandler, buffer and flags are freed by this. */
    free_svalue(sp--);  /* mapping or callback structure. */
    free_mstring(str);  /* string, at arg == sp */
    sp->u.str = rc;     /* put result here */

    return sp;
} /* x_filter_string() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_map_string (svalue_t *sp, int num_arg)

/* EFUN map() for strings
 *
 *   string map(string arg, string func, string|object ob, mixed extra...)
 *   string map(string arg, closure cl, mixed extra...)
 *   string map(string arg, mapping m [, int idx])
 *
 * Call the function <ob>-><func>() resp. the closure <cl> for
 * every element of the array/struct/mapping/string <arg>, and return a result
 * made up from the returned values.
 *
 * It is also possible to map every entry through a lookup <m>[element[,idx]].
 * If the mapping entry doesn't exist, the original value is kept, otherwise
 * the result of the mapping lookup.
 * [Note: argument type and range checking for idx is done in v_map()]
 *
 * Since <arg> is a string, only integer return values are allowed, of which
 * only the lower 8 bits are considered.
 *
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 */

{
    string_t *res;
    string_t *str;
    svalue_t *arg;
    mp_int    len;              /* Length of source string in characters. */
    mp_int    reslen;           /* Length of the result string in bytes. */
    bool      utf8_out = false; /* Whether the result has non-ASCII chars. */
    bool      utf8_in;          /* Whether the source has non-ASCII chars. */
    bool      bytes_in;         /* Whether the source is a byte sequence. */
    char     *src, *srcend, *dest;

    inter_sp = sp;

    arg = sp - num_arg + 1;

    str = arg->u.str;
    src = get_txt(str);
    len = mstrsize(str);
    srcend = src + len;
    utf8_in = str->info.unicode == STRING_UTF8;
    bytes_in = str->info.unicode == STRING_BYTES;

    if (utf8_in)
    {
        bool error = false;
        len = byte_to_char_index(get_txt(str), len, &error);
        if (error)
            errorf("Invalid character in string at index %" PRIdMPINT ".\n", len);
    }

    if (arg[1].type == T_MAPPING)
    {
        /* --- Map through mapping --- */

        mapping_t *m;
        p_int column = 0; /* mapping column to use */

        m = arg[1].u.map;

        if (num_arg > 2)
            column = arg[2].u.number;

        res = alloc_mstring(bytes_in ? len : (4 * len));
        if (!res)
            errorf("(map_string) Out of memory: string[%"PRIdMPINT
                   "] for result\n", len);

        push_string(inter_sp, res); /* In case of errors */

        reslen = 0;
        for (dest = get_txt(res); len > 0; len--)
        {
            svalue_t key = { T_NUMBER };
            size_t chlen;
            svalue_t *v;

            if (utf8_in)
            {
                chlen = utf8_to_unicode(src, srcend - src, &(key.u.number));
                if (!chlen)
                    errorf("Invalid character in string at index %zd.\n", src - get_txt(str));
            }
            else
            {
                key.u.number = *(unsigned char*)src;
                chlen = 1;
            }

            v = get_map_value(m, &key);
            if (v == &const0)
            {
                memcpy(dest, src, chlen);
                dest += chlen;
                reslen += chlen;

                if (chlen > 1)
                    utf8_out = true;
            }
            else
            {
                if (v[column].type != T_NUMBER)
                {
                    errorf("(map_string) Illegal value type: %s, expected int\n"
                         , typename(v[column].type)
                         );
                }

                if (bytes_in)
                {
                    *dest = v[column].u.number;
                    dest++;
                    reslen++;
                }
                else
                {
                    size_t newchlen = unicode_to_utf8(v[column].u.number, dest);
                    if (!newchlen)
                        errorf("Invalid resulting character: %"PRIdPINT"\n", v[column].u.number);
                    dest += newchlen;
                    reslen += newchlen;

                    if (newchlen > 1)
                        utf8_out = true;
                }
            }

            src += chlen;
        }

        assert(src == srcend);
        assert(dest <= get_txt(res) + mstrsize(res));

        if (num_arg > 2)
            free_svalue(arg+2);
        free_svalue(arg+1); /* the mapping */
        sp = arg;
    }
    else
    {
        /* --- Map through function call --- */

        callback_t *cb;
        int         error_index;

        error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
        if (error_index >= 0)
        {
            vefun_bad_arg(error_index+2, arg);
            /* NOTREACHED */
            return arg;
        }
        inter_sp = sp = arg+1;
        put_callback(sp, cb);
        num_arg = 2;

        res = alloc_mstring(bytes_in ? len : (4 * len));
        if (!res)
            errorf("(map_string) Out of memory: string[%"PRIdMPINT
                   "] for result\n", len);
        
        push_string(inter_sp, res); /* In case of errors */

        reslen = 0;
        for (dest = get_txt(res); len > 0; len--)
        {
            p_int num;
            size_t chlen;
            svalue_t *v;

            if (utf8_in)
            {
                chlen = utf8_to_unicode(src, srcend - src, &num);
                if (!chlen)
                    errorf("Invalid character in string at index %zd.\n", src - get_txt(str));
            }
            else
            {
                num = *(unsigned char*)src;
                chlen = 1;
            }

            if (is_current_object_destructed())
            {
                src += chlen;
                continue;
            }

            if (!valid_callback_object(cb))
                errorf("object used by map(string) destructed");

            push_number(inter_sp, num);

            v = apply_callback(cb, 1);

            if (v)
            {
                if (v->type != T_NUMBER)
                {
                    errorf("(map_string) Illegal value: %s, expected string\n"
                         , typename(v->type)
                         );
                }

                if (bytes_in)
                {
                    *dest = v->u.number;
                    dest++;
                    reslen++;
                }
                else
                {
                    size_t newchlen = unicode_to_utf8(v->u.number, dest);
                    if (!newchlen)
                        errorf("Invalid resulting character: %"PRIdPINT"\n", v->u.number);
                    dest += newchlen;
                    reslen += newchlen;

                    if (newchlen > 1)
                        utf8_out = true;
                }
            }
            else
            {
                memcpy(dest, src, chlen);
                dest += chlen;
                reslen += chlen;

                if (chlen > 1)
                    utf8_out = true;
            }

            src += chlen;
        }

        assert(src == srcend);
        assert(dest <= get_txt(res) + mstrsize(res));

        free_svalue(sp); /* The callback structure. */
    }

    /* The arguments have been removed already, now just replace
     * the string on the stack with the result.
     */
    free_mstring(str);
    res->info.unicode = utf8_out ? STRING_UTF8 : bytes_in ? STRING_BYTES : STRING_ASCII;
    arg->u.str = resize_mstring(res, reslen); /* Keep svalue type: T_STRING/T_BYTES */

    return arg;
} /* x_map_string () */

/*====================================================================*/

