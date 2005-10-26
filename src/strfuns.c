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
 *
 * --- Efuns and Operators ---
 *
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

#include "strfuns.h"

#include "comm.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
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
        svp->u.str = new_n_mstring(buf->buf, buf->length);
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
    char * dest, * src;
    size_t dest_ix, src_ix, srclen;
    string_t * rc;

    dest = alloca(mstrsize(txt));
    if (dest == NULL)
        error("Stack overflow (%ld bytes)\n", (long)mstrsize(txt));

    src = get_txt(txt);
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

    memsafe(rc = new_n_mstring(dest, dest_ix), dest_ix, "trimmed result");
    return rc;
} /* trim_all_spaces() */

/*====================================================================*/

/*                          EFUNS                                     */

/*--------------------------------------------------------------------*/
static char *
sort_string (const string_t * p_in, size_t len, long ** pos)

/* Sort the characters of string <in> (with length <len>) by their numeric
 * values and return a newly allocated memory block with the sorted string.
 * If <pos> is not NULL, it will be set to a newly allocated memory block
 * giving the original positions of the characters in the sorted string.
 *
 * We use Mergesort to sort the strings.
 * TODO: Use Quicksort instead of Mergesort?
 */

{
    char   * in;      /* Input string */
    char   * out;     /* Result string */
    long   * outpos;  /* Result position array */
    char   * tmp;     /* Temporary string */
    long   * tmppos;  /* Temporary position array */
    size_t   step;
    size_t   i, j;

    in = get_txt(p_in);
    out = xalloc(len+1);
    tmp = xalloc(len+1);
    if (!out || !tmp)
    {
        if (out)
            xfree(out);
        if (tmp)
            xfree(tmp);
        error("(sort_string) Out of memory (2 * %lu bytes) for temporaries.\n"
             , (unsigned long) len+1);
    }
    out[len] = '\0';
    tmp[len] = '\0';

    if (pos)
    {
        outpos = xalloc(len * sizeof(*outpos) + 1);
        tmppos = xalloc(len * sizeof(*outpos) + 1);
          /* +1 so that smalloc won't complain when given an empty string */
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
            error("(sort_string) Out of memory (2 * %lu bytes) for positions.\n"
                 , (unsigned long) len*sizeof(*outpos)+1);
        }
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
string_t *
intersect_strings (const string_t * p_left, const string_t * p_right, Bool bSubtract)

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
    char   * left, * right, *left_txt, *result_txt;
    string_t *result;

    len_left = mstrsize(p_left);
    len_right = mstrsize(p_right);

    xallocate(matches, len_left+1, "intersection matches");
      /* +1 so that smalloc won't complain when given an empty left string */

    for (ix_left = 0; ix_left < len_left; ix_left++)
        matches[ix_left] = bSubtract ? MY_TRUE : MY_FALSE;

    /* Sort the two strings */
    left = sort_string(p_left, len_left, &pos);
    right = sort_string(p_right, len_right, NULL);

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
    memsafe(result = alloc_mstring(len_out), len_out, "intersection result");
    left_txt = get_txt(p_left);
    result_txt = get_txt(result);
    for (ix_left = 0, ix_right = 0; ix_left < len_left; ix_left++)
        if (matches[ix_left])
            result_txt[ix_right++] = left_txt[ix_left];

    /* Free intermediate results */
    xfree(pos);
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

    char     *flags;  /* Flag array, one flag for each element of <str>
                       * (in reverse order). */
    mp_int    res;    /* Number of surviving elements */

    res = 0;

    /* Locate the args on the stack, extract the string to filter
     * and allocate the flags vector.
     */
    arg = sp - num_arg + 1;

    str = arg->u.str;
    slen = (mp_int)mstrsize(str);

    flags = alloca((size_t)slen+1);
    if (!flags)
    {
        error("Stack overflow in filter()");
        /* NOTREACHED */
        return sp;
    }

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

        /* --- Filter by mapping query --- */
        mapping_t *m;

        if (num_arg > 2) {
            inter_sp = sp;
            error("Too many arguments to filter(array)\n");
        }
        m = arg[1].u.map;

        for (src = get_txt(str), cnt = slen; --cnt >= 0; src++)
        {
            svalue_t key;

            put_number(&key,  *src);
            if (get_map_value(m, &key) == &const0)
            {
                flags[cnt] = 0;
                continue;
            }
            flags[cnt] = 1;
            res++;
        }

        free_svalue(arg+1); /* the mapping */
        sp = arg;

    } else {

        /* --- Filter by function call --- */

        int         error_index;
        callback_t  cb;
        mp_int cnt;

        assign_eval_cost();
        inter_sp = sp;

        error_index = setup_efun_callback(&cb, arg+1, num_arg-1);

        if (error_index >= 0)
        {
            vefun_bad_arg(error_index+2, arg);
            /* NOTREACHED */
            return arg;
        }
        inter_sp = sp = arg+1;
        put_callback(sp, &cb);

        /* Loop over all elements in p and call the filter.
         * w is the current element filtered.
         */
        for (src = get_txt(str), cnt = slen; --cnt >= 0; src++)
        {
            svalue_t *v;

            flags[cnt] = 0;

            if (current_object->flags & O_DESTRUCTED)
                continue;
                /* Don't call the filter anymore, but fill the
                 * flags array with 0es.
                 */

            if (!callback_object(&cb))
            {
                inter_sp = sp;
                error("object used by filter(array) destructed");
            }

            push_number(inter_sp, *src);

            v = apply_callback(&cb, 1);
            if (!v || (v->type == T_NUMBER && !v->u.number) )
                continue;

            flags[cnt] = 1;
            res++;
        }

        free_callback(&cb);
    }

    /* flags[] holds the filter results, res is the number of
     * elements to keep. Now create the result vector.
     */
    rc = alloc_mstring(res);
    if (rc)
    {
        for (src = get_txt(str), dest = get_txt(rc), flags = &flags[slen]
            ; res > 0 ; src++)
        {
            if (*--flags)
            {
                *dest++ = *src;
                res--;
            }
        }
    }

    /* Cleanup (everything but the string has been removed already) */
    free_mstring(str);
    arg->u.str = rc;

    return arg;
} /* x_filter_string() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_map_string (svalue_t *sp, int num_arg)

/* EFUN map() for strings
 *
 *   string map(string arg, string func, string|object ob, mixed extra...)
 *   string map(string arg, closure cl, mixed extra...)
 *   string map(string arg, mapping m)
 *
 * Call the function <ob>-><func>() resp. the closure <cl> for
 * every element of the array/struct/mapping/string <arg>, and return a result
 * made up from the returned values.
 *
 * It is also possible to map every entry through a lookup <m>[element]. If
 * the mapping entry doesn't exist, the original value is kept, otherwise the
 * result of the mapping lookup.
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
    mp_int    len;
    char     *src, *dest;

    inter_sp = sp;

    arg = sp - num_arg + 1;

    str = arg->u.str;
    len = (mp_int)mstrsize(str);

    if (arg[1].type == T_MAPPING)
    {
        /* --- Map through mapping --- */

        mapping_t *m;

        if (num_arg > 2) {
            inter_sp = sp;
            error("Too many arguments to map(string)\n");
        }
        m = arg[1].u.map;

        res = alloc_mstring(len);
        if (!res)
            error("(map_string) Out of memory: string[%ld] for result\n", len);
        push_string(inter_sp, res); /* In case of errors */

        for (src = get_txt(str), dest = get_txt(res); --len >= 0; src++, dest++)
        {
            svalue_t key, *v;

            put_number(&key, *src);
            v = get_map_value(m, &key);
            if (v == &const0)
                *dest = *src;
            else
            {
                if (v->type != T_NUMBER)
                {
                    error("(map_string) Illegal value: %s, expected string\n"
                         , typename(v->type)
                         );
                }
                *dest = (v->u.number & 0xFF);
            }
        }

        free_svalue(arg+1); /* the mapping */
        sp = arg;
    }
    else
    {
        /* --- Map through function call --- */

        callback_t  cb;
        int         error_index;

        error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
        if (error_index >= 0)
        {
            vefun_bad_arg(error_index+2, arg);
            /* NOTREACHED */
            return arg;
        }
        inter_sp = sp = arg+1;
        put_callback(sp, &cb);
        num_arg = 2;

        res = alloc_mstring(len);
        if (!res)
            error("(map_string) Out of memory: string[%ld] for result\n", len);
        push_string(inter_sp, res); /* In case of errors */

        for (src = get_txt(str), dest = get_txt(res); --len >= 0; src++, dest++)
        {
            svalue_t *v;

            if (current_object->flags & O_DESTRUCTED)
                continue;

            if (!callback_object(&cb))
                error("object used by map(string) destructed");

            push_number(inter_sp, *src);

            v = apply_callback(&cb, 1);

            if (v)
            {
                if (v->type != T_NUMBER)
                {
                    error("(map_string) Illegal value: %s, expected string\n"
                         , typename(v->type)
                         );
                }
                *dest = (v->u.number & 0xFF);
            }
        }

        free_callback(&cb);
    }

    /* The arguments have been removed already, now just replace
     * the arr on the stack with the result.
     */
    free_mstring(str);
    arg->u.str = res; /* Keep svalue type: T_POINTER */

    return arg;
} /* x_map_string () */

/*====================================================================*/

