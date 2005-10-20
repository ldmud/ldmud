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
#include "typedefs.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "strfuns.h"

#include "comm.h"
#include "simulate.h"
#include "svalue.h"
#if 0 /* See below */
#define USES_SVALUE_STRLEN
#include "smalloc.h"
#endif
#include "xalloc.h"

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

    /* Allocate more than we need in anticipation of further adds,
     * but not more than we can manage
     */
    if (MAX_STRBUF_LEN - buf->length < len * 3)
    {
        new_len = MAX_STRBUF_LEN;
        if (new_len - buf->length < len)
            len = new_len - buf->length;
    }
    else
        new_len = buf->length + len * 3;


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
strbuf_store (strbuf_t *buf, svalue_t *svp)

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
        svp->u.string = xalloc(buf->length+1);
        memcpy(svp->u.string, buf->buf, buf->length+1);
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

/*--------------------------------------------------------------------*/
static char *
sort_string (char * in, size_t len, long ** pos)

/* Sort the characters of string <in> (with length <len>) by their numeric
 * values and return a newly allocated memory block with the sorted string.
 * If <pos> is not NULL, it will be set to a newly allocated memory block
 * giving the original positions of the characters in the sorted string.
 *
 * We use Mergesort to sort the strings.
 * TODO: Use Quicksort instead of Mergesort?
 */

{
    char   * out;     /* Result string */
    long   * outpos;  /* Result position array */
    char   * tmp;     /* Temporary string */
    long   * tmppos;  /* Temporary position array */
    size_t   step;
    size_t   i, j;
    
    out = xalloc(len+1);
    tmp = xalloc(len+1);
    if (!out || !tmp)
        fatal("(sort_string) Out of memory (2 * %lu bytes) for temporaries.\n"
             , (unsigned long) len+1);
    out[len] = '\0';
    tmp[len] = '\0';

    if (pos)
    {
        outpos = xalloc(len * sizeof(*outpos) + 1);
        tmppos = xalloc(len * sizeof(*outpos) + 1);
          /* +1 so that smalloc won't complain when given an empty string */
        if (!outpos || !tmppos)
            fatal("(sort_string) Out of memory (2 * %lu bytes) for positions.\n"
                 , (unsigned long) len*sizeof(*outpos)+1);
    }
    else
    {
        outpos = NULL;
        tmppos = NULL;
    }

    /* First Mergesort pass: comparison of adjacent characters
     * and initialisation of the out arrays.
     */
    for (i = 0; i < len; i += 2)
    {
        if (i == len-1)
        {
            out[i] = in[i];
            if (outpos)
                outpos[i] = i;
        }
        else if (in[i] <= in[i+1])
        {
            out[i] = in[i];
            out[i+1] = in[i+1];
            if (outpos)
            {
                outpos[i] = i;
                outpos[i+1] = i+1;
            }
        }
        else /* (in[i] > in[i+1]) */
        {
            out[i] = in[i+1];
            out[i+1] = in[i];
            if (outpos)
            {
                outpos[i] = i+1;
                outpos[i+1] = i;
            }
        }
    } /* for(initial pass) */

    /* Mergesort loop: perform the mergesort passes with increasing steps.
     * Invariant: out is the (semi-sorted) data, tmp is the scratchspace.
     */
    for (step = 2; step < len; step *= 2)
    {
        size_t start, dest, left;
        
        /* Exchange out and tmp */
        {
            char *tmp2;
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
char *
intersect_strings (char * left, char * right, Bool bSubtract)

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
    size_t   len_left, len_right, len_out;
    size_t   ix_left, ix_right;
    long   * pos;
    CBool  * matches;
    char   * left_in, *result;

    left_in = left;            /* Needed to create the result */
    len_left = strlen(left);
    len_right = strlen(right);

    xallocate(matches, len_left+1, "intersection matches");
      /* +1 so that smalloc won't complain when given an empty left string */

    for (ix_left = 0; ix_left < len_left; ix_left++)
        matches[ix_left] = bSubtract ? MY_TRUE : MY_FALSE;

    /* Sort the two strings */
    left = sort_string(left, len_left, &pos);
    right = sort_string(right, len_right, NULL);

    /* Intersect the two strings by mutual comparison.
     * Each non-matched character in left gets is pos[] set to -1.
     */
    len_out = bSubtract ? len_left : 0;
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
            {
                matches[pos[ix_left]] = MY_TRUE;
                len_out++;
            }
            else
            {
                matches[pos[ix_left]] = MY_FALSE;
                len_out--;
            }
            ix_left++;
        }
    }

    /* Create the result: copy all flagged characters */
    xallocate(result, len_out+1, "intersection result");
    result[len_out] = '\0';
    for (ix_left = 0, ix_right = 0; ix_left < len_left; ix_left++)
        if (matches[ix_left])
            result[ix_right++] = left_in[ix_left];

    /* Free intermediate results */
    xfree(pos);
    xfree(matches);
    xfree(left);
    xfree(right);

    return result;
} /* intersect_strings() */

/*====================================================================*/

