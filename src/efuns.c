/*---------------------------------------------------------------------------
 * Various Efuns.
 *
 *---------------------------------------------------------------------------
 * This file acts as a repository for various (new) efuns. Over the time
 * it will probably grow large enough to justify a split into several files.
 *
 * The implemented efuns, sorted by topic, are:
 *
 * Strings:
 *    tefun: make_shared_string()
 *    tefun: trim()
 *    tefun: upper_case()
 *    efun:  terminal_colour()
 *
 * Objects:
 *    xefun: all_environment()
 *    tefun: object_info()
 *    tefun: present_clone() (preliminary)
 *    tefun: set_is_wizard() (optional)
 *    tefun: set_modify_command()
 *    tefun: set_prompt()
 *
 * Values:
 *    tefun: copy()
 *    tefun: deep_copy()
 *
 * Others:
 *    tefun: debug_info()
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define USES_SVALUE_STRLEN
#include "efuns.h"

#include "array.h"
#include "closure.h"
#include "comm.h"
#include "datatypes.h"
#include "dumpstat.h"
#include "interpret.h"
#include "instrs.h"
#include "prolang.h"        /* F_ILLEGAL */
#include "main.h"
#include "mapping.h"
#include "ptrtable.h"
#include "simulate.h"
#include "smalloc.h"
#include "stralloc.h"
#include "strfuns.h"
#include "swap.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/objectinfo.h"
#include "../mudlib/sys/strings.h"

/* Forward declarations */
static void copy_svalue (struct svalue *dest, struct svalue *, struct pointer_table *);

/* Macros */

/* Typetests for xefuns/tefuns */

#define TYPE_TEST1(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_arg(1, sp);

#define TYPE_TEST2(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_arg(2, sp);

#define TYPE_TEST3(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_arg(3, sp);

/* Typetests for vararg xefuns/vefuns */

#define TYPE_TESTV1(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_vararg(1, sp);

#define TYPE_TESTV2(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_vararg(2, sp);

#define TYPE_TESTV3(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_vararg(3, sp);

/* Typetests for efuns */

#define E_TYPE_TESTV1(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_efun_arg(1, -2, sp);

#define E_TYPE_TESTV2(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_efun_arg(2, -2, sp);

/*=========================================================================*/
/*                              STRINGS                                    */

/*-------------------------------------------------------------------------*/
struct svalue *
f_make_shared_string (struct svalue *sp)

/* TEFUN make_shared_string()
 *
 *    string make_shared_string(string s)
 *
 * If the passed string <s> is not shared, the efun enters it into
 * the shared string table and returns the shared version. Else the
 * passed string is returned.
 *
 * TODO: Improve the string handling of the driver so that this efun
 * TODO:: becomes unnecessary.
 */

{
    TYPE_TEST1(sp, T_STRING)

    if (sp->x.string_type != STRING_SHARED)
    {
        char *s = make_shared_string(sp->u.string);

        if (sp->x.string_type == STRING_MALLOC)
            xfree(sp->u.string);
        sp->u.string = s;
        sp->x.string_type = STRING_SHARED;
    }

    return sp;
} /* f_make_shared_string() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_trim (struct svalue *sp, int num_arg)

/* VEFUN trim()
 *
 *    string trim (string s [, int where [, string|int ch]])
 *
 * Remove all leading/trailing characters <ch> from the string <s>
 * and return the new string. <ch> may be a single character, or a string
 * of characters to be trimmed. If <ch> is not given or 0, it defaults
 * to " \t". <where> determines where to remove the characters:
 *   TRIM_LEFT:  remove the leading characters
 *   TRIM_RIGHT: remove the trailing characters
 *   TRIM_BOTH:  remove both leading and trailing characters.
 *
 * TODO: Expand this to remove interim characters as well?
 * TODO: Expand this to fold runs of embedded chs into just one?
 */

{
    struct svalue * argp;
    char *str, *end;     /* Pointer to string begin and end */
    char *left, *right;  /* Pointer to the strings left and right end */
    char def_ch[3]       /* Buffer for single characters to strip */
      = { '\t', ' ', '\0' };
    char *strip;         /* String of characters to strip */
    size_t strip_l;      /* Length of *strip */
    int  where;

    /* Get and test the arguments */
    if (num_arg > 3)
        error("Bad number of arguments to trim()\n");
    argp = sp - num_arg + 1;

    TYPE_TESTV1(argp, T_STRING)
    str = argp->u.string;

    if (num_arg > 1)
    {
        TYPE_TESTV2(argp+1, T_NUMBER)
        where = argp[1].u.number;
        if (!where)
            where = TRIM_LEFT|TRIM_RIGHT;
        if (where & ~(TRIM_LEFT|TRIM_RIGHT))
            error("Bad argument 2 to trim(): illegal value %ld\n", (long)where);
    }
    else
        where = TRIM_LEFT|TRIM_RIGHT;

    if (num_arg > 2)
    {
        if (argp[2].type == T_NUMBER)
        {
            if (argp[2].u.number <= 0 || argp[2].u.number >= 1 << CHAR_BIT)
                error("Bad argument 3 to trim(): %ld is not a character\n"
                     , argp[2].u.number);
            def_ch[0] = (char)argp[2].u.number;
            def_ch[1] = '\0';
            strip = def_ch;
            strip_l = 1;
        }
        else if (argp[2].type == T_STRING)
        {
            strip = argp[2].u.string;
            strip_l = svalue_strlen(argp+2);
        }
        else
            bad_xefun_vararg(3, sp);
    }
    else
    {
       strip = def_ch;
       strip_l = 2;
    }

    /* Get the string limits */
    end = str + strlen(str);
    if (where & TRIM_LEFT)
    {
        for (left = str
            ; *left != '\0' && strchr(strip, *left) != NULL
            ; left++
            ) NOOP;
    }
    else
        left = str;
    if (where & TRIM_RIGHT && end != left)
    {
        for (right = end; right != left && NULL != strchr(strip, right[-1])
            ; right--) NOOP;
    }
    else
        right = end;

    /* If there are things to strip, create a new string and put it
     * into the place of the old one.
     */
    if (left != str || right != end)
    {
        char * trimmed;
        size_t newlen;

        newlen = (unsigned)(right - left);
        trimmed = xalloc(newlen+1);
        if (!trimmed)
            error("Out of memory.\n");
        memcpy(trimmed, left, newlen);
        trimmed[newlen] = '\0';
        free_string_svalue(argp);
        put_malloced_string(argp, trimmed);
    }

    /* argp+2 might need to be freed, but argp+1 is always just a number.
     * And the result argp is fine as it is.
     */

    if (num_arg > 2 && argp[2].type == T_STRING)
        free_svalue(argp+2);

    return argp;
} /* f_trim() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_upper_case (struct svalue *sp)

/* TEFUN upper_case()
 *
 *    string upper_case (string s)
 *
 * Convert all characters in <s> to upper case and return the new string.
 */

{
    char *str, *s, *d, c;
    ptrdiff_t initial_len;

    TYPE_TEST1(sp, T_STRING)

    /* Find the first non-uppercase character in the string */
    for (s = sp->u.string; '\0' != (c = *s) && !islower((unsigned char)c); s++)
        NOOP;

    if ('\0' != *s)  /* there are lowercase characters */
    {
        if (STRING_MALLOC == sp->x.string_type)
        {
            /* MALLOCed strings can be changed in-place */
            for ( ; '\0' != (c = *s); s++)
            {
                if (islower(c))
                    *s = (char)toupper(c);
            }
        }
        else
        {
            /* Other strings must be duplicated and then changed */
            str = xalloc(svalue_strlen(sp)+1);
            if (!str)
            {
                error("Out of memory.\n");
                /* NOTREACHED */
                return sp;
            }

            initial_len = s - sp->u.string;
            /* Copy the initial part */
            if (initial_len)
                memcpy(str, sp->u.string, (size_t)initial_len);

            /* Copy and change the rest */
            for (d = str + initial_len; '\0' != (c = *s++) ; )
            {
                if (islower(c))
                    c = (char)toupper(c);
                *d++ = c;
            }

            *d = '\0';
            free_string_svalue(sp);
            put_malloced_string(sp, str);
        }
    }

    /* That's it */
    return sp;
}

/*-------------------------------------------------------------------------*/
static Bool
at_end (int i, int imax, int z, int *lens)

/* Auxilary function for e_terminal_colour().
 *
 * <z> is the position within string number <i>. <lens> is an array
 * with the lengths of all <imax> strings.
 *
 * The function returns true if there are no more characters to process
 * after <i>:<z> in all strings, else it returns false.
 */

{
    if (z + 1 != lens[i])
        return MY_FALSE;
    for (i++; i < imax; i++) {
        if (lens[i] > 0)
            return MY_FALSE;
    }
    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
char *
e_terminal_colour ( char * text, struct mapping * map
                  , int indent, int wrap
                  )

/* EFUN terminal_colour()
 *
 *   string terminal_colour (string text, mapping map
 *                          , int wrap, int indent )
 *
 * Expands all colour-defines from the input-string and replaces them by the
 * apropriate values found for the color-key inside the given mapping. The
 * mapping has the format "KEY" : "value", non-string contents are ignored.
 *
 * The parameters wrap and indent are both optional, if only wrap is given
 * then the str will be linewrapped at the column given with wrap.  If indent
 * is given too, then all wrapped lines will be indented with the number of
 * blanks specified with indent.
 *
 * The wrapper itself ignores the length of the color macros and that what
 * they contain, it wraps the string based on the length of the other chars
 * inside. Therefor it is color-aware.
 *
 * This function is called from the evaluator and provided with the
 * proper arguments.
 *
 * Result is a pointer to the final string. If no changes were necessary,
 * this is <text> again; otherwise it is a pointer to memory allocated
 * by the function.
 *
 * TODO: This efun needs better definitions, ie. how keywords and %^ relate,
 * TODO:: (%^key%^ or %^key or...), how to escape %^s, etc.
 */

{
#define CALLOCATE(num, type) ((type *)xalloc(sizeof(type[1]) * (num) ))
  /* Allocate a block of <num> elements of <type>
   */

#define RESIZE(ptr, num, type) ((type *)rexalloc((void *)ptr, sizeof(type) * (num)))
  /* Resize the block <ptr> to hold <num> elements of <type>.
   */

#define NSTRSEGS       32
  /* Allocation increment. */

#define TC_FIRST_CHAR  '%'
#define TC_SECOND_CHAR '^'
  /* The two magic characters.
   */

#define MAX_STRING_LENGTH 200000
  /* The maximum length of the result.
   */

    char *cp;              /* Workpointer */
    char *savestr = NULL;  /* Allocated auxiliary string */
    char *instr;
      /* The input string. This may be <text> itself, or a working copy. */
    char *deststr;         /* Result string */
    char **parts;
      /* The <num> delimited parts from <instr>. This are mostly
       * pointers into *<instr>, but can also be (uncounted) pointers to
       * the string data in <map>.
       */
    int num;               /* Number of delimited parts in <instr> */
    int *lens;
      /* Length of the <num> parts. This value is negative for strings
       * 'retrieved' from the <map>ping when wrapping is required. This
       * is necessary to determine which parts[] to exempt from the
       * wrapping calculation.
       */
    int k;                 /* Index within a string */
    int col;               /* Current print column */
    int j;                 /* Accumulated total length of result */
    int start;             /* Col of first non-blank character */
    int space;             /* Col of last space char */
    int i;
    Bool maybe_at_end;     /* TRUE if the next text might start a new line */
    Bool no_keys;          /* TRUE if no delimiter in the string */

    instr = text;

    /* Find the first occurance of the magic character pair.
     * If found, duplicate the input string into instr and
     * let cp point into that copy at the delimiter.
     * If not found, cp will be NULL.
     */
    cp = text;
    do {
        cp = strchr(cp, TC_FIRST_CHAR);
        if (cp)
        {
            if (cp[1] == TC_SECOND_CHAR)
            {
                savestr = string_copy(text);
                cp = savestr + (cp - text);
                instr = savestr;
                break;
            }
            cp++;
        }
    } while (cp);

    /* If the delimiter was found, split up the instr into the
     * parts and store them. Or just return.
     */
    no_keys = MY_FALSE;
    if (cp == NULL)
    {
        /* No delimiter found - but maybe we need to wrap */
        if (wrap)
        {
            /* Yup, just fake one delimited part which just happens
             * to not match anything in the mapping.
             */
            num = 1;
            parts = CALLOCATE(1, char *);
            parts[0] = instr;
            savestr = NULL;  /* should be NULL anyway */
            no_keys = MY_TRUE;
        }
        else
        {
            /* no delimiter in string and no wrapping, so return the original.
             */
            return text;
        }
    }
    else
    {
        /* There are delimiters in the string. Find them all, let the
         * pointers in *<parts> point to the strings delimited by
         * them, and let those parts end with a '\0'.
         * This means modifying the *<instr>, but it is already
         * a copy.
         */

        /* cp here points to the first delimiter found */

        parts = CALLOCATE( NSTRSEGS, char * );
        if (!parts)
        {
            error("Out of memory.\n");
            /* NOTREACHED */
            return NULL;
        }

        if (cp != instr)
        {
            /* instr starts with a delimiter, so we create
             * a null string to start with.
             */
            num = 1;
            parts[0] = instr;
            *cp = '\0';
        }
        else
            /* otherwise just search and count */
            num = 0;

        /* Search and find the other delimited segments.
         * Loop variant: cp points to the last delimiter found,
         * its first character replaced by \0, or cp points to NULL (exit
         * condition)
         * Loop invariant: instr points to the begin of the last delimited
         * segment.
         */
        while (cp)
        {
            /* Skip the delimiter found last and search the next */
            cp += 2;
            instr = cp;
            do
            {
                cp = strchr(cp,TC_FIRST_CHAR);
                if (cp) {
                    if (cp[1] == TC_SECOND_CHAR)
                        break;
                    cp++;
                }
            } while (cp);

            if (cp)
            {
                /* Another delimiter found: put it into the parts array,
                 * but only if it is not a null string.
                 */
                *cp = '\0';
                if (cp > instr)
                {
                    parts[num] = instr;
                    num++;
                    if (num % NSTRSEGS == 0)
                        parts = RESIZE(parts, num + NSTRSEGS, char * );
                }
            }
        }

        /* Trailing part, or maybe just a delimiter */
        if (*instr)
            parts[num++] = instr;
    } /* if (delimiter found or not) */

    /* Prepare the lens[] array */
    if ( num )
        lens = CALLOCATE(num, int);
    else
        lens = NULL;

    /* Do the the pointer replacement and calculate the lengths.
     * The lengths are collected in the lens[] array to save the
     * need for repeated strlens().
     */
    col = 0;
    start = -1;
    space = 0;
    maybe_at_end = MY_FALSE;
    j = 0; /* gathers the total length of the final string */
    for (i = 0; i < num; i++)
    {
        long len;
        char * str;
        struct svalue * mdata;

        /* If parts[i] is a valid colour key, there must exist a shared
         * string for it. Is that the case, look up parts[i] in the
         * mapping and set the result in mdata, otherwise save that effort.
         */
        str = findstring(parts[i]);
        if (!no_keys && str != NULL)
        {
            struct svalue mkey;

            put_string(&mkey, str);
              /* The only use of mkey is to index a mapping - an operation
               * which will not decrement the refcount for <str>. This
               * makes it safe to not count the ref by mkey here, and saves
               * a bit time.
               */

            /* now look for mapping data */
            mdata = get_map_value(map, &mkey);
        }
        else
            mdata = NULL;

        /* If mdata found a string, use it instead of the old parts[i].
         * Not its length, making it negative where necessary.
         */
        if ( mdata && mdata->type == T_STRING )
        {
            parts[i] = mdata->u.string;
            len = (long)svalue_strlen( mdata );
            if (wrap)
                len = -len;
        }
        else
            len = (long)strlen(parts[i]);

        lens[i] = len;
        if (len > 0)
        {
            /* This part must be considered for wrapping/indentation */

            if (maybe_at_end)
            {
                /* This part may start a new line, so count in the indent */

                if (j + indent > MAX_STRING_LENGTH)
                {
                    /* This string no longer counts, so we are still in a
                     * maybe_at_end condition.  This means we will end up
                     * truncating the rest of the fragments too, since the
                     * indent will never fit.
                     */
                    lens[i] = 0;
                    len = 0;
                }
                else
                {
                    j += indent;
                    col += indent;
                    maybe_at_end = MY_FALSE;
                }
            }

            /* Add the new string to the total length */
            j += len;
            if (j > MAX_STRING_LENGTH)
            {
                /* Overflow: shorten this fragment to fit (and all
                 * the following ones will be shortened to 0 length).
                 */
                lens[i] -= j - MAX_STRING_LENGTH;
                j = MAX_STRING_LENGTH;
            }

            /* If wrapping is requested, perform the analysis */
            if (wrap)
            {
                int   z;             /* Index into the current string */
                char *p = parts[i];  /* Pointer into the current string */

                for (z = 0; z < lens[i]; z++)
                {
                    char c = p[z];   /* current character */

                    if (c == '\n')
                    {
                        /* Hard line break: start a new line */
                        col = 0;
                        start = -1;
                    }
                    else
                    {
                        /* All space characters in columns before col <start>
                         * do not count.
                         */
                        if (col > start || c != ' ')
                            col++;
                        else
                            j--;

                        /* If space, remember the position */
                        if (c == ' ')
                            space = col;

                        if (col == wrap+1)
                        {
                            /* Wrapping necessary */

                            if (space)
                            {
                                /* Break the line at the last space */
                                col -= space;
                                space = 0;
                            }
                            else
                            {
                                /* No space within range: simply let this
                                 * one extent over the wrap margin and
                                 * restart counting.
                                 */
                                j++;
                                col = 1;
                            }

                            /* Reset the start column. */
                            start = indent;
                        }
                        else
                            continue; /* the for(z) */
                    }

                    /* If we get here, we ended a line */

                    if (col || z + 1 != lens[i])
                    {
                        /* Not at the end of the fragment: count in
                         * the indent from the new line.
                         */
                        j += indent;
                        col += indent;
                    }
                    else
                        maybe_at_end = MY_TRUE;

                    /* Guard against overflow */
                    if (j > MAX_STRING_LENGTH)
                    {
                        /* Reduce this part to fit; all the following
                         * parts will be reduced to shreds^W0.
                         */
                        lens[i] -= (j - MAX_STRING_LENGTH);
                        j = MAX_STRING_LENGTH;
                        if (lens[i] < z)
                        {
                            /* must have been ok or we wouldn't be here */
                            lens[i] = z;
                            break;
                        }
                    }
                } /* for (z = 0..lens[i]) */
            } /* if (wrap) */
        }
        else
        {
            /* This replacement does not need to be wrapped. */
            j += -len;
            if (j > MAX_STRING_LENGTH)
            {
                /* Max length exceeded: shrink the working length
                 * to something usable. All following fragments
                 * will be shrunk to length 0.
                 */
                lens[i] = -(-(lens[i]) - (j - MAX_STRING_LENGTH));
                j = MAX_STRING_LENGTH;
            }
        } /* if (len > 0) */
    } /* for (i = 0..num) */


    /* Now we have the final string in parts and length in j.
     * let's compose the result, wrapping it where necessary.
     */
    deststr = xalloc((size_t)(j+1));
    if (!deststr)
    {
        error("Out of memory.\n");
        /* NOTREACHED */
        return NULL;
    }

    cp = deststr; /* destination pointer */

    if (wrap)
    {
        /* Catenate and wrap the parts together. This will look similar
         * to the length computation above.
         */

        int space_garbage = 0;
          /* Number of characters to be ignored since the last space,
           * most of them are control codes and other junk.
           */
        size_t tmpmem_size;
        char *tmpmem;
          /* Temporary buffer for the current line */
        char *pt;
          /* Pointer into tmpmem */

        tmpmem_size = (size_t)j+1;
        tmpmem = xalloc(tmpmem_size);
        if (!tmpmem)
        {
            error("Out of memory.\n");
            /* NOTREACHED */
            return NULL;
        }

        col = 0;
        start = -1;
        space = 0;
        pt = tmpmem;

        /* Loop over all parts */
        for (i = 0; i < num; i++)
        {
            int kind;            /* The kind of a line break */
            int len;             /* Actual length of the line */
            int l = lens[i];     /* Length of current part */
            char *p = parts[i];  /* Current part */

            if (pt - tmpmem + ((l < 0) ? -l : l) >= tmpmem_size)
            {
                error("Partial string too long (> %ld).\n", (long)tmpmem_size);
                /* NOTREACHED */
                return NULL;
            }

            if (l < 0)
            {
                /* String retrieved from the mapping: not to be counted */
                memcpy(pt, p, (size_t)-l);
                pt += -l;
                space_garbage += -l;
                continue;
            }

            /* Loop over the current part, copying and wrapping */
            for (k = 0; k < lens[i]; k++)
            {
                int n;
                char c = p[k];  /* Current character */

                /* Copy the character into tmpmem */
                *pt++ = c;

                if (c == '\n')
                {
                    /* Start a new line */
                    col = 0;
                    kind = 0;
                    start = -1;
                }
                else
                {
                    /* All space characters in columns before col <start>
                     * do not count.
                     */
                    if (col > start || c != ' ')
                        col++;
                    else
                        pt--;

                    /* If space, remember the position */
                    if (c == ' ')
                    {
                        space = col;
                        space_garbage = 0;
                    }

                    /* Wrapping necessary? */
                    if (col == wrap+1)
                    {
                        if (space)
                        {
                            /* Break at last space */
                            col -= space;
                            space = 0;
                            kind = 1;
                        }
                        else
                        {
                            /* No space within range: simply let this
                             * one extent over the wrap margin and
                             * restart counting.
                             */
                            col = 1;
                            kind = 2;
                        }

                        /* Reset the start column */
                        start = indent;
                    }
                    else
                        continue;
                }

                /* If we get here, we ended a line, and kind tells us why:
                 *   kind == 0: hard line break
                 *           1: line wrapped at suitable space
                 *           2: line extended over the limit with no space
                 */

                len = (kind == 1 ? col + space_garbage : col);

                /* Determine the length of the _previous_ (and therefore
                 * wrapped) line and copy it from tmpmem into deststr.
                 */
                n = (pt - tmpmem) - len;
                memcpy(cp, tmpmem, (size_t)n);
                cp += n;

                if (kind == 1)
                {
                    /* replace the space with the newline */
                    cp[-1] = '\n';
                }
                if (kind == 2)
                {
                    /* need to insert a newline */
                    *cp++ = '\n';
                }

                /* Remove the previous line from tmpmem */
                move_memory(tmpmem, tmpmem + n, (size_t)len);
                pt = tmpmem + len;

                if (len > space_garbage || !at_end(i, num, k, lens))
                {
                    /* There will be data coming next: insert the
                     * indentation.
                     */
                    memset(cp, ' ', (size_t)indent);
                    cp += indent;
                    col += indent;
                }
            }
        } /* for(i = 0..num) */

        memcpy(cp, tmpmem, (size_t)(pt - tmpmem));
        cp += pt - tmpmem;
        xfree(tmpmem);
    }
    else
    {
        /* No wrapping: just catenate the parts (and all lens[] entries
         * are positive here)
         */
        for (i = 0; i < num; i++)
        {
            memcpy(cp, parts[i], (size_t)lens[i]);
            cp += lens[i];
        }
    }

    /* Terminate the string */
    *cp = '\0';

    if ( lens )
      xfree(lens);
    if ( parts )
      xfree(parts);
    if (savestr)
      xfree(savestr);

    /* now we have what we want */
#ifdef DEBUG
    if (cp - deststr != j) {
      fatal("Length miscalculated in terminal_colour()\n"
            "    Expected: %i Was: %ld\n"
            "    String: %s\n    Indent: %i Wrap: %i\n"
           , j, (long)(cp - deststr), text, indent, wrap);
    }
#endif
    return deststr;

#undef CALLOCATE
#undef RESIZE
#undef NSTRSEGS
#undef TC_FIRST_CHAR
#undef TC_SECOND_CHAR
} /* e_terminal_colour() */

/*-------------------------------------------------------------------------*/
/* Structures for sscanf() */

/* Flags for every argument whether to assign and/or count it
 */

struct sscanf_flags {
    int do_assign: 16;
    int count_match: 16;
};

/* Packet of information passed between the scan functions:
 */

struct sscanf_info
{
    struct svalue *arg_start;    /* first argument for the current %-spec */
    struct svalue *arg_current;  /* current argument to consider */
    struct svalue *arg_end;      /* the last argument */
    char          *fmt_end;
      /* After the match: the next character in the fmt-string to match.
       */
    char          *match_end;
      /* After the match: the next character in the in-string to match.
       * NULL for 'no match'.
       */
    mp_uint        field;        /* Numbers: parsed fieldwidth */
    mp_uint        min;          /* Numbers: parsed precision */
    mp_uint        string_max;   /* Strings: parsed fieldwidth */
    mp_uint        string_min;   /* Strings: parsed 'precision' */
    struct sscanf_flags flags;
    int            sign;         /* -1 for '%-d', 0 for '%d', '%+d' or '%u' */
    mp_int         number_of_matches;  /* Number of matches so far */
};

/*-------------------------------------------------------------------------*/
static void
sscanf_decimal (char *str, struct sscanf_info *info)

/* Parse a number from <str> according the .field and .min given in <info>,
 * and, if successfull, store it in <info>->arg_current, which is then
 * incremented.
 *
 * <info>.match_end and .fmt_end are set properly on return.
 */

{
    static struct svalue tmp_svalue = { T_NUMBER };

    mp_int i, num;
    char c;

    num = 0;

    i = (mp_int)info->min;
    if (i > 0)
    {
        /* The number must have at least i digits */
        info->field -= i;
        do
        {
            if (!lexdigit(c = *str))
            {
                if (info->fmt_end[-1] != 'd')
                {
                    info->match_end = NULL;
                }
                else
                {
                    info->match_end = str;
                    info->fmt_end = "d"+1;
                }
                return;
            }
            str++;
            num = num * 10 + c - '0';
        } while (--i);
    }

    /* There can be info->field more digits */
    i = (mp_int)info->field;
    while  (--i >= 0)
    {
        if (!lexdigit(c = *str))
            break;
        str++;
        num = num * 10 + c - '0';
    }

    info->match_end = str;

    if (info->flags.do_assign)
    {
        /* Assign the parsed number */
        if (info->arg_current >= info->arg_end)
            return;

        tmp_svalue.u.number = (p_int)((num ^ info->sign) - info->sign);
        transfer_svalue((info->arg_current++)->u.lvalue, &tmp_svalue);
    }

    info->number_of_matches += info->flags.count_match;
    return;
}

/*-------------------------------------------------------------------------*/
static char *
sscanf_match_percent (char *str, char *fmt, struct sscanf_info *info)

/* Match a %-specification, called from sscanf_match().
 *
 * <fmt> points to the first character after the '%'.
 * <str> points to the first character to match.
 *
 * Return new value for <str> if matching is to be continued, else
 * return NULL and write in info->match_end the match end if a match was
 * found,  NULL otherwise.
 *
 * If a match was found, also write info->fmt_end with a pointer to the
 * conversion character, and info->flags, info->field, info->min.
 */

{
    char c;
    mp_uint *nump; /* Pointer to parsed fieldwidth resp. precision */

    /* Initialize field with a large value that will become
     * zero when doubled. Because 10 is divisible by 2, the multiply
     * will zero it. Note that it is negative before we decrement it
     * the first time.
     */
    *(nump = &info->field) = (((mp_uint)-1 / 2)) + 1;
    info->min = 1;
    info->flags.do_assign = 1;
    info->flags.count_match = 1;

    for (;;)
    {
        switch(c = *fmt++)
        {
        case '!':
            info->flags.count_match ^= 1;
            info->flags.do_assign ^= 1;
            continue;

        case '~':
            info->flags.do_assign ^= 1;
            continue;

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            *nump = *nump * 10 + c - '0';
            continue;

        case '*':
            if (info->arg_current >= info->arg_end
             || info->arg_current->u.lvalue->type != T_NUMBER)
            {
                info->match_end = NULL;
                return NULL;
            }
            *nump = (mp_uint)((info->arg_current++)->u.lvalue->u.number);
            continue;

        case '.':
            *(nump = &info->min) = 0;
            continue;

        case 'd':
            /* Skip leading whitespace */
            while(isspace((unsigned char)*str))
                str++;
            /* FALLTHROUGH */

        case 'D':
            /* Match a signed number */
            if (*str == '-')
            {
                info->sign = -1;
                str++;
            }
            else
            {
                if (*str == '+')
                    str++;
                info->sign = 0;
            }
            info->fmt_end = fmt;
            sscanf_decimal(str, info);
            return NULL;

        case 'U':
            /* Match an unsigned number */
            info->sign = 0;
            info->fmt_end = fmt;
            sscanf_decimal(str, info);
            return NULL;

        case 's':
            /* Match a string */
            /* min = (min was explicitly given) ? min : 0; */
            info->string_max = info->field;
            info->field = 0;
            info->string_min = *nump;
            info->fmt_end = fmt;
            info->match_end = str;
            return NULL;

        default:
            error("Bad type : '%%%c' in sscanf fmt string.\n", fmt[-1]);
            return 0;

        case 't':
          {
            /* Skip whitespaces */

            mp_int i;

            info->field -= (i = (mp_int)info->min);

            /* Required whitespace */
            while (--i >= 0)
            {
                if (!isspace((unsigned char)*str))
                {
                    info->match_end = NULL;
                    return NULL;
                }
                str++;
            }

            /* Optional whitespace */
            i = (mp_int)info->field;
            while (--i >= 0)
            {
                if (!isspace((unsigned char)*str))
                    break;
                str++;
            }
            info->fmt_end = fmt;
            return str;
          }
        } /* switch(*fmt) */
    } /* forever */
} /* sscanf_match_percent() */

/*-------------------------------------------------------------------------*/
static void
sscanf_match (char *str, char *fmt, struct sscanf_info *info)

/* Find position in <str> after matching text from <fmt>, and place it in
 * info->match_end.
 * Set info->match_end to NULL for no match.
 * Set info->fmt_end to a guaranteed static '\0' when the fmt string ends.
 */

{
    char c;

    /* (Re)set the current argument */
    info->arg_current = info->arg_start;

    /* Loop over the format string, matching characters */
    for (;;)
    {
        if ( !(c = *fmt) )
        {
            info->match_end = str;
            info->fmt_end = "d"+1;
            return;
        }

        fmt++;

        if (c == '%')
        {
            c = *fmt;
            if (c != '%')
            {
                /* We have a format specifier! */
                char *new_str;

                new_str = sscanf_match_percent(str, fmt, info);
                if (!new_str)
                    return; /* Failure or string specifier */

                str = new_str;
                fmt = info->fmt_end;
                continue;
            }
            fmt++;
        }

        if (c == *str++)
        {
            continue;
        }
        else
        {
            info->match_end = NULL;
            return;
        }
    }
} /* sscanf_match() */

/*-------------------------------------------------------------------------*/
static char *
sscanf_search (char *str, char *fmt, struct sscanf_info *info)

/* sscanf() found a possible '%s' match. This function finds the start
 * of the next match in <str> and returns a pointer to it.
 * If none can be found, NULL is returned.
 */

{
    char a, b, c;
    mp_int n;

    a = *fmt;
    if (!a)
    {
        /* End of format: match all */
        info->fmt_end = "d"+1;
        info->arg_current = info->arg_start;
        return info->match_end = str + strlen(str);
    }
    fmt++;
    b = *fmt++;

    if (a == '%')
    {
        if (b != '%')
        {
            /* It's another %-spec: match it */
            for (fmt -= 2; *str; str++)
            {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str;
            }
            return NULL;
        }
        else
        {
            /* Double '%' stands for '%' itself */
            b = *fmt++;
        }
    }

    /* a and b are now the 'next two' characters from fmt, and they
     * don't start a %-spec.
     */

    if (b == a)
    {
        /* A run of identical characters: set n to the length */

        n = 0;
        do {
            n++;
            b = *fmt++;
        } while (b == a);

        if (a == '%')
        {
            /* n fmt-'%' represent (n/2) real '%'s */
            if (n & 1)
            {
                n >>= 1;
                fmt--;
                goto a_na_search;
            }
            n >>= 1;
        }

        if (b == '\0')
        {
            fmt--;
            goto a_na_search;
        }

        if (b == '%')
        {
            /* Since a is not '%' here, this may be the next %-spec */
            b = *fmt++;
            if (b != '%')
            {
                fmt -= 2;
                goto a_na_search;
            }
        }

        /* Search in <str> for the sequence <a>, (<n>+?)*<a>, <b>.
         * <b> is a character which starts a successfull new match.
         * To find this, the function tries a match at every possible <b>
         * it finds.
         *
         * If the <b> is found, all the characters before belong to
         * the previous %s match, if not found, the whole string
         * belongs to the match.
         */
        {
            char ch;
            mp_int i;

a_na_b_search:
            if ( !(ch = *str++) )
                return NULL;

            /* First <a> ? */
            if (ch != a)
                goto a_na_b_search;

            /* Followed by <n> <a>s? */
            i = n;
            do {
                if ( !(ch = *str++) )
                    return NULL;
                if (ch != a)
                    goto a_na_b_search;
            } while (--i);

            /* There may be more <a>s */
            do {
                if ( !(ch = *str++) )
                    return NULL;
            } while (ch == a);

            /* If followed by <b>, we may have found the next match */
            if (ch == b)
            {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - n - 2;
            }

            /* Not found: start all over */
            goto a_na_b_search;
        }
        /* NOTREACHED */
    }

    if (!b)
    {
        /* Special case: the sequence is just <a> */
        n = 0;
        fmt--;

        /* Search in <str> for the sequence <a>, (<n>+?)*<a>, 'x'.
         * 'x' is a character which starts a successfull new match.
         * To find this, the function tries a match at every possible 'x'
         * it finds.
         *
         * If the 'x' is found, all the characters before belong to
         * the previous %s match, if not found, the whole string
         * belongs to the match.
         */
        {
            char ch;
            mp_int i;

a_na_search:
            if ( !(ch = *str++) )
                return NULL;

            /* First <a>? */
            if (ch != a)
                goto a_na_search;

            /* Followed by <n> <a>s? */
            if ( 0 != (i = n)) do {
                if ( !(ch = *str++) )
                    return NULL;
                if (ch != a)
                    goto a_na_search;
            } while (--i);

            /* For every other character, test if the next match starts here */
            do {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - n - 1;
                if ( !(ch = *str++) )
                    return NULL;
            } while (ch == a);

            /* Not found: start all over */
            goto a_na_search;
        }
        /* NOTREACHED */
    }

    if (b == '%')
    {
        /* Special case: <a>, (<n>+?)*<a>, which we know will
         * be successfull.
         */
        b = *fmt++;
        if (b != '%')
        {
            fmt -= 2;
            n = 0;
            goto a_na_search;
            /* "goto, goto, goto - this is sooo ugly" says Tune */
        }
    }

    /* a != b && b != '%' here */

    c = *fmt;
    if (!c)
    {
        /* Special case: <a>, (0+?)*<b>, '\0' which we know will
         * be successfull because the fmt ends.
         */
        n = 0;
        goto ab_nab_search;
    }

    if (c == '%')
    {
        c = *++fmt;
        if (c != '%')
        {
            /* Special case: <a>, (0+?)*<b>, '%-spec', which we know will
             * be successfull because of the format spec.
             */
            fmt--;
            n = 0;
            goto ab_nab_search;
        }

        /* just a literal '%' */
    }

    fmt++;
    if (c == a)
    {
        c = *fmt++;
        if (c == '%')
        {
            c = *fmt;
            if (c != '%')
            {
                /* <a> (0+?)*<b> <a> '%-spec' */
                fmt -= 2 + (a == '%');
                n = 0;
                goto ab_nab_search;
            }
            fmt++;
            /* just a literal '%' */
        }

        if (c != b)
        {
            if (!c)
            {
                /* <a> (0+?)*<b> <a> '\0' */
                fmt -= 2 + (a == '%');
                n = 0;
                goto ab_nab_search;
            }

            /* Search in <str> for <a> ?*{<b> <a>} <a> <c>.
             * <c> is a character which starts a successfull new match.
             * To find this, the function tries a match at every possible <c>
             * it finds.
             *
             * If the <c> is found, all the characters before belong to
             * the previous %s match, if not found, the whole string
             * belongs to the match.
             */
            for (;;)
            {
                char ch;

                ch = *str++;
a_b_a_c_check_a:
                if (!ch)
                    return NULL;

                /* First <a>? */
                if (ch != a)
                    continue;

                ch = *str++;
a_b_a_c_check_b:

                /* Check for <b> <a> */
                if (ch != b)
                    goto a_b_a_c_check_a;

                ch = *str++;
                if (ch != a)
                    continue;
                ch = *str++;
                if (ch != c)
                    goto a_b_a_c_check_b;

                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - 4;

                goto a_b_a_c_check_a;
            }
            /* NOTREACHED */
        }

        /* c == b */
        n = 2;

        /* Search in <str> for <a> <b> n*{<a> <b>} ?*<b> 'x'.
         * 'x' is a character which starts a successfull new match.
         * To find this, the function tries a match at every possible
         * 'x' it finds.
         *
         * If the 'x' is found, all the characters before belong to
         * the previous %s match, if not found, the whole string
         * belongs to the match.
         */
        {
            char ch;
            int i;

            goto ab_nab_search;

ab_nab_check_0:
            if (!ch)
                return NULL;
ab_nab_search:
            ch = *str++;
ab_nab_check_a:

            /* First <a> */
            if (ch != a)
                goto ab_nab_check_0;

            /* A <b> should follow, introducing the repetition */
            ch = *str++;
            if (ch != b)
                goto ab_nab_check_a;

            /* <n> times the couple <a> <b> should follow */
            if (0 != (i = n)) do
            {
                ch = *str++;
                if (ch != a)
                    goto ab_nab_check_0;
                ch = *str++;
                if (ch != b)
                    goto ab_nab_check_a;
            } while (i -= 2);

            do {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - n - 2;
                ch = *str++;
                if (ch != a)
                    goto ab_nab_check_0;
                ch = *str++;
            } while (ch == b);

            goto ab_nab_check_0;
        }
        /* NOREACHED */
    }

    /* c != a */

    /* Search in <str> for <a> <b> <c> 'x'.
     * 'x' is a character which starts a successfull new match.
     * To find this, the function tries a match at every possible
     * 'x' it finds.
     *
     * If the 'x' is found, all the characters before belong to
     * the previous %s match, if not found, the whole string
     * belongs to the match.
     */
    for (;;) {
        char ch;

        ch = *str++;
a_b_c_check_a:
        if (!ch)
            return 0;
        if (ch != a)
            continue;
        ch = *str++;
        if (ch != b)
            goto a_b_c_check_a;
        ch = *str++;
        if (ch != c)
            goto a_b_c_check_a;
        sscanf_match(str, fmt, info);
        if (info->match_end)
            return str - 3;
    }

    /* NOTREACHED */
} /* sscanf_search() */

/*-------------------------------------------------------------------------*/
int
e_sscanf (int num_arg, struct svalue *sp)

/* EFUN sscanf()
 *
 *   int sscanf(string str, string fmt, mixed var1, mixed var2, ...)
 *
 * Execute the sscanf() function if <num_arg> arguments on the stack <sp>,
 * and return the number of matches.
 *
 * Parse a string str using the format fmt. fmt can contain strings seperated
 * by %d and %s. Every %d and %s corresponds to one of var1, var2, ... .
 *
 * The match operators in the format string have one of these formats:
 *   %[!|~][<size>[.<minmatch>]]<type>
 *
 * <type> may be:
 *    d: matches any number.
 *    D: matches any number.
 *    U: matches any unsigned number.
 *    s: matches any string.
 *    %: matches the % character.
 *    t: matches whitespace (spaces and tab characters), but does
 *       not store them (the simple ' ' matches just spaces and
 *       can't be given a size specification).
 *
 * <size> is the expected field size, <minmatch> the demanded minimal match
 * length (defaults are 0 for strings and 1 for numbers). Each of these both
 * may be specified numerically, or as '*' - then the value of the variable at
 * the current place in the argument list is used.
 *
 * Specifying ! will perform the match, but neither store the result nor count
 * the match.
 * Specifying ~ will perform and count the match, but not store the result.
 *
 * (You can think of '!' as negating on a wholesale basis, while '~'
 *  negates only individual bits. Thus, '%!' negates both do_assign
 *  and count_match, while '%~' only negates do_assign.)
 *
 * The difference between %d and %D/%U is that the latter will abort an
 * immediately preceeding %s as soon as possible, whereas the former will
 * attempt to make largest match to %s first.  %D/%U will still not skip
 * whitespace, use %.0t%D to skip optional whitespace.
 *
 * The number of matched arguments will be returned.
 *
 * The function sscanf is special, in that arguments are passed by reference
 * automatically.
 */

{
    char *fmt;                  /* Format description */
    char *in_string;            /* The string to be parsed. */
    struct svalue sv_tmp;
    struct svalue *arg0;        /* The first argument */
    struct sscanf_flags flags;  /* local copy of info.flags */
    struct sscanf_info info;    /* scan information packet */

    inter_sp = sp; /* we can have an error() deep inside */
    arg0 = sp - num_arg + 1;

    /* First get the string to be parsed.
     */
    E_TYPE_TESTV1(arg0, T_STRING);
    in_string = arg0[0].u.string;

    /* Now get the format description.
     */
    E_TYPE_TESTV2((arg0+1), T_STRING);
    fmt = arg0[1].u.string;

    info.arg_end = arg0 + num_arg;
    info.arg_current = arg0 + 2;

    /* Loop for every % or substring in the format. Update the
     * arg pointer continuosly. Assigning is done manually, for speed.
     */
    for (info.number_of_matches = 0; info.arg_current <= info.arg_end; )
    {
        info.arg_start = info.arg_current;
        sscanf_match(in_string, fmt, &info);
        in_string = info.match_end;

        if (!in_string) /* End of input? */
            break;

        /* Either fmt is out, or we found a string match */

match_skipped:

        fmt = info.fmt_end;
        if (fmt[-1] == 's')
        {
            mp_uint max;
            mp_int num;
            char *match;
            struct svalue *arg;

            flags = info.flags;

            /* Set match to the first possible end character of the string
             * to match.
             */
            num = (mp_int)info.string_min;
            if (num > 0)
            {
                if (num > (mp_int)strlen(in_string))
                    break;

                match = in_string + num;
            }
            else
            {
                /* num = 0 */
                match = in_string;
            }

            max = info.string_max;
            arg = info.arg_current;
            info.arg_start = arg + flags.do_assign;
            if (info.arg_start > info.arg_end)
            {
                break;
            }

            /* Search the real end of the string to match and set match
             * to it.
             */
            if (NULL != (match = sscanf_search(match, fmt, &info))
             && (mp_uint)(num = match - in_string) <= max)
            {
                /* Got the string: assign resp. skip it */

                if (flags.do_assign)
                {
                    match = xalloc((size_t)num+1);
                    if (!match)
                        error("Out of memory\n");
                    strncpy(match, in_string, (size_t)num);
                    match[num] = '\0';
                    put_malloced_string(&sv_tmp, match);
                    transfer_svalue(arg->u.lvalue, &sv_tmp);
                }

                in_string = info.match_end;
                info.number_of_matches += flags.count_match;
                info.arg_start = info.arg_current;
                goto match_skipped;
            }

            /* no match found */
            break;
        }

        if (!fmt[0]) /* End of format */
            break;
    }

    return info.number_of_matches;
}


/*=========================================================================*/
/*                              OBJECTS                                    */

/*-------------------------------------------------------------------------*/
struct svalue *
x_all_environment (struct svalue *sp, int numarg)

/* XEFUN all_environment()
 *
 *    object * all_environment(object o)
 *
 * Return an array with all environments of object <o> in 'outgoing'
 * order. If <o> has no environment, 0 is returned.
 *
 * The caller checked the correctness of the arguments.
 */

{
    struct object *o;

    /* Get the arg from the stack, if any */
    if (numarg)
    {
        o = ref_object(sp->u.ob, "all_environment");
        free_object_svalue(sp);
    }
    else
    {
        o = current_object;
        sp++;
    }


    /* Default return value: 0 */
    put_number(sp, 0);

    if (!(o->flags & O_DESTRUCTED))
    {
        mp_int num;
        struct object *env;
        struct vector *v;
        struct svalue *svp;

        /* Count the number of environments */
        for ( num = 0, env = o->super
            ; NULL != env
            ; num++, env = env->super)
            NOOP;

        if (num)
        {
            /* Get the array and fill it */
            v = allocate_uninit_array(num);
            for ( svp = v->item, env = o->super
                ; NULL != env
                ; svp++, env = env->super)
            {
                put_ref_object(svp, env, "all_environment");
            }

            /* Put the result on the stack and return */
            put_array(sp, v);
        }
    }

    if (numarg)
        free_object(o, "all_environment");
    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_object_info (struct svalue *sp)

/* TEFUN object_info()
 *
 *    mixed * object_info(object o, int type)
 *
 * Return an array with information about the object <o>. The
 * type of information returned is determined by <type>.
 */

{
    struct vector *v;
    struct object *o, *o2, *prev;
    struct program *prog;
    struct svalue *svp;
    int flags, pos;

    /* Test and get the arguments from the stack */
    TYPE_TEST1(sp-1, T_OBJECT)
    TYPE_TEST2(sp, T_NUMBER)

    o = sp[-1].u.ob;

    /* Depending on the <type> argument, determine the
     * data to return.
     */
    switch(sp->u.number)
    {
    default:
        error("Illegal value %ld for object_info().\n", sp->u.number);
        /* NOTREACHED */
        return sp;

    /* --- The basic information from the object structure */
    case OINFO_BASIC:
        v = allocate_array(OIB_MAX);
        svp = v->item;

        flags = o->flags;

        svp[OIB_HEART_BEAT].u.number = (flags & O_HEART_BEAT) ? 1 : 0;
#ifdef O_IS_WIZARD
        svp[OIB_IS_WIZARD].u.number  = (flags & O_IS_WIZARD) ? 1 : 0;
#else
        svp[OIB_IS_WIZARD].u.number  = 0;
#endif
        svp[OIB_ENABLE_COMMANDS].u.number
                                     = (flags & O_ENABLE_COMMANDS) ? 1 : 0;
        svp[OIB_CLONE].u.number      = (flags & O_CLONE) ? 1 : 0;
        svp[OIB_DESTRUCTED].u.number = (flags & O_DESTRUCTED) ? 1 : 0;
        svp[OIB_SWAPPED].u.number    = (flags & O_SWAPPED) ? 1 : 0;
        svp[OIB_ONCE_INTERACTIVE].u.number
                                     = (flags & O_ONCE_INTERACTIVE) ? 1 : 0;
        svp[OIB_RESET_STATE].u.number = (flags & O_RESET_STATE) ? 1 : 0;
        svp[OIB_WILL_CLEAN_UP].u.number
                                     = (flags & O_WILL_CLEAN_UP) ? 1 : 0;
        svp[OIB_LAMBDA_REFERENCED].u.number
                                     = (flags & O_LAMBDA_REFERENCED) ? 1 : 0;
        svp[OIB_SHADOW].u.number     = (flags & O_SHADOW) ? 1 : 0;
        svp[OIB_REPLACED].u.number   = (flags & O_REPLACED) ? 1 : 0;
#ifdef F_SET_LIGHT
        svp[OIB_TOTAL_LIGHT].u.number = o->total_light;
#else
        svp[OIB_TOTAL_LIGHT].u.number = 0;
#endif
        svp[OIB_NEXT_RESET].u.number = o->time_reset;
        svp[OIB_TIME_OF_REF].u.number = o->time_of_ref;
        svp[OIB_REF].u.number         = o->ref;
        svp[OIB_GIGATICKS].u.number   = (p_int)o->gigaticks;
        svp[OIB_TICKS].u.number       = (p_int)o->ticks;
        svp[OIB_SWAP_NUM].u.number    = O_SWAP_NUM(o);
        svp[OIB_PROG_SWAPPED].u.number = O_PROG_SWAPPED(o) ? 1 : 0;
        svp[OIB_VAR_SWAPPED].u.number = O_VAR_SWAPPED(o) ? 1 : 0;

        put_malloced_string(svp+OIB_NAME, string_copy(o->name));
        put_ref_string(svp+OIB_LOAD_NAME, o->load_name);

        o2 = o->next_all;
        if (o2)
        {
            put_ref_object(svp+OIB_NEXT_ALL, o2, "object_info(0)");
        } /* else the element was already allocated as 0 */

        o2 = o->prev_all;
        if (o2)
        {
            put_ref_object(svp+OIB_PREV_ALL, o2, "object_info(0)");
        } /* else the element was already allocated as 0 */

        break;

    /* --- Position in the object list */
    case OINFO_POSITION:
        v = allocate_array(OIP_MAX);
        svp = v->item;

        o2 = o->next_all;
        if (o2)
        {
            put_ref_object(svp+OIP_NEXT, o2, "object_info(1) next");
        } /* else the element was already allocated as 0 */

        /* Find the non-destructed predecessor of the object */
        if (obj_list == o)
        {
            pos = 0;
            prev = NULL;
        }
        else
        for (prev = NULL, o2 = obj_list, pos = 0; o2; o2 = o2->next_all)
        {
            prev = o2;
            pos++;
            if (o2->next_all == o)
                break;
        }

        if (o2) /* Found it in the list */
        {
            if (prev)
            {
                put_ref_object(svp+OIP_PREV, prev, "object_info(1) prev");
            } /* else the element was already allocated as 0 */
        }
        else /* Not found (this shouldn't happen) */
            pos = -1;

        svp[OIP_POS].u.number = pos;

        break;

    /* --- Memory information */
    case OINFO_MEMORY:
        v = allocate_array(OIM_MAX);
        svp = v->item;

        if (O_PROG_SWAPPED(o) && load_ob_from_swap(o) < 0)
            error("Out of memory.\n");

        prog = o->prog;

        svp[OIM_REF].u.number = prog->ref;

        put_malloced_string(svp+OIM_NAME, string_copy(prog->name));

        svp[OIM_PROG_SIZE].u.number
                               = (long)(PROGRAM_END(*prog) - prog->program);
          /* Program size */
        svp[OIM_NUM_FUNCTIONS].u.number = prog->num_functions;
        svp[OIM_SIZE_FUNCTIONS].u.number
                           = (p_int)(prog->num_functions * sizeof(uint32)
                             + prog->num_function_names * sizeof(short));
          /* Number of function names and the memory usage */
        svp[OIM_NUM_VARIABLES].u.number = prog->num_variables;
        svp[OIM_SIZE_VARIABLES].u.number
                     = (p_int)(prog->num_variables * sizeof(struct variable));
          /* Number of variables and the memory usage */
        svp[OIM_NUM_STRINGS].u.number = prog->num_strings;
        svp[OIM_SIZE_STRINGS].u.number
                                 = (p_int)(prog->num_strings * sizeof(char*));
          /* Number of strings and the memory usage */
        {
            int i = prog->num_inherited;
            int cnt = 0;
            struct inherit *inheritp;

            for (inheritp = prog->inherit; i--; inheritp++)
            {
                if (!inheritp->is_extra)
                    cnt++;
            }
            svp[OIM_NUM_INHERITED].u.number = cnt;
        }
        svp[OIM_SIZE_INHERITED].u.number
                     = (p_int)(prog->num_inherited * sizeof(struct inherit));
          /* Number of inherites and the memory usage */
        svp[OIM_TOTAL_SIZE].u.number = prog->total_size;
        break;
    }

    free_svalue(sp);
    sp--;
    free_svalue(sp);

    /* Assign the result */
    put_array(sp, v);

    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_present_clone (struct svalue *sp)

/* TEFUN present_clone()
 *
 *    object present_clone(string str, object env)
 *    object present_clone(object obj, object env)
 *
 * Search in the inventory of <env> for the first object with the
 * same blueprint as object <obj>, resp. for the first object with
 * the loadname <str>, and return that object.
 *
 * If not found, 0 is returned.
 */

{
    char * name;         /* the shared loadname to look for */
    struct object *obj;  /* the object under scrutiny */

    /* Test and get the arguments from the stack */
    TYPE_TEST2(sp, T_OBJECT)

    if (sp[-1].type == T_STRING)
    {
        size_t len;
        long i;
        char * end;
        char * name0;  /* Intermediate name */

        name0 = sp[-1].u.string;

        /* Normalize the given string and check if it is
         * in the shared string table. If not, we know that
         * there is no blueprint with that name
         */

        /* First, slash of a trailing '#<num>' */

        len = svalue_strlen(sp-1);
        i = (signed)len;
        end = name0 + len;

        while (--i > 0)
        {
            char c;

            c = *--end;
            if (c < '0' || c > '9' )
            {
                /* Not a digit: maybe a '#' */
                if ('#' == c && len - i > 1)
                {
                    name0 = alloca((size_t)i + 1);
                    if (!name0)
                        error("Out of stack memory.\n");
                    strncpy(name0, sp[-1].u.string, (size_t)i);
                    name0[i] = '\0';
                }

                break; /* in any case */
            }
        }

        /* Now make the name sane */
#ifndef COMPAT_MODE
        name = (char *)make_name_sane(name0, MY_TRUE);
#else
        name = (char *)make_name_sane(name0, MY_FALSE);
#endif
        if (name)
            name = findstring(name);
        else
            name = findstring(name0);

    }
    else if (sp[-1].type == T_OBJECT)
    {
        name = sp[-1].u.ob->load_name;
    }
    else
        bad_xefun_arg(1, sp);

    obj = NULL;
    if (name)
    {
        /* We have a name, now look for the object */
        for (obj = sp->u.ob->contains; obj != NULL; obj = obj->next_inv)
        {
            if (!(obj->flags & O_DESTRUCTED) && name == obj->load_name)
                break;
        }
    }

    /* Assign the result */
    sp = pop_n_elems(2, sp) + 1;
    if (obj != NULL)
        put_ref_object(sp, obj, "present_clone");
    else
        put_number(sp, 0);

    return sp;
} /* f_present_clone() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_to_object (struct svalue *sp)

/* TEFUN to_object()
 *
 *   object to_object(string arg)
 *   object to_object(closure arg)
 *   object to_object(object arg)
 *
 * The argument is converted into an object, if possible.  For strings, the
 * object with a matching file_name() is returned, or 0 if there is none, as
 * find_object() does.  For (bound!) closures, the object holding the closure
 * is returned.
 * Objects and the number 0 return themselves.
 */

{
    int n;
    struct object *o;

    switch(sp->type)
    {
    case T_NUMBER:
        if (!sp->u.number)
            return sp;
        /* FALLTHROUGH */
    default:
        bad_xefun_arg(1, sp);

    case T_CLOSURE:
        n = sp->x.closure_type;
        o = sp->u.ob;
        if (n == CLOSURE_EFUN + F_UNDEF - F_OFFSET)
            o = NULL;
        else if (CLOSURE_MALLOCED(n))
        {
            if (n == CLOSURE_UNBOUND_LAMBDA)
                bad_xefun_arg(1, sp);
            o = sp->u.lambda->ob;
        }
        if (o && o->flags & O_DESTRUCTED)
            o = NULL;
        free_closure(sp);
        break;

    case T_OBJECT:
        return sp;

    case T_STRING:
        o = find_object(sp->u.string);
        free_svalue(sp);
        break;
    }

    if (o)
        put_ref_object(sp, o, "to_object");
    else
        put_number(sp, 0);

    return sp;
} /* f_to_object() */

/*-------------------------------------------------------------------------*/
#ifdef F_SET_IS_WIZARD

struct svalue *
f_set_is_wizard (struct svalue *sp)

/* TEFUN set_is_wizard()
 *
 *   int set_is_wizard(object ob, int n)
 *
 * Change object ob's wizardhood flag.  If n is 0, it is cleared, if n is, it
 * is set, if n is -1 the current status is reported. The return value is
 * always the old value of the flag. Using this function sets a flag in the
 * parser, that affects permissions for dumpallobj etc, which are by default
 * free for every user.
 */

{
    int i;
    unsigned short *flagp;

    TYPE_TEST1(sp-1, T_OBJECT)
    TYPE_TEST2(sp,   T_NUMBER)

    flagp = &sp[-1].u.ob->flags;
    i = (*flagp & O_IS_WIZARD) != 0;

    switch (sp->u.number)
    {
        default: bad_xefun_arg(2, sp);
        case  0: *flagp &= ~O_IS_WIZARD; is_wizard_used = 1; break;
        case  1: *flagp |=  O_IS_WIZARD; is_wizard_used = 1; break;
        case -1: break; /* only report status */
    }
    sp--;
    free_object_svalue(sp);
    put_number(sp, i);
    return sp;
} /* f_set_is_wizard() */

#endif /* F_SET_IS_WIZARD */

/*-------------------------------------------------------------------------*/
struct svalue *
f_set_modify_command (struct svalue *sp)

/* TEFUN set_modify_command()
 *
 *   object set_modify_command(object)
 *   object set_modify_command(string)
 *   object set_modify_command(int)
 *
 * All commands for the current object (that must obviously be interactive)
 * will be passed to ob->modify_command() before actually being executed. The
 * argument can be passed an object or a file_name.
 *
 * When set_modify_command() was called, the parser won't expand the standard
 * abbreviations n,e,s,w,nw,sw,ne,se for that user anymore, nor use any hook
 * set for this.
 *
 * 0 as argument will stop the command modification and reinstall
 *   the standard abbreviations.
 * -1 as argument will just return the object previously set.
 *
 * The return value is the object that was previously set with
 * set_modify_command(), if any.
 */

{
    struct object *old, *new;
    struct interactive *ip;

    inter_sp = sp;

    /* Make sure the current_object is interactive */

    if ( !(ip = O_GET_INTERACTIVE(current_object))
     || ip->sent.type != SENT_INTERACTIVE
     || ip->closing)
    {
        error("set_modify_command in non-interactive object\n");
    }

    /* Get the old setting */
    old = ip->modify_command;
    if (old && old->flags & O_DESTRUCTED)
    {
        free_object(old, "set_modify_command");
        old = NULL;
        ip->modify_command = NULL;
    }

    /* Set the new setting */
    new = sp->u.ob;
    switch(sp->type)
    {
    default:
bad_arg_1:
        bad_xefun_arg(1, sp);

    case T_STRING:
        new = get_object(sp->u.string);
        if (!new) goto bad_arg_1;

    case T_OBJECT:
        ip->modify_command = ref_object(new, "set_modify_command");
        break;

    case T_NUMBER:
        if (sp->u.number == 0 )
        {
            /* ref count of old is reused below, so don't free now */
            ip->modify_command = NULL;
        }
        else
        {
            if (sp->u.number != -1) goto bad_arg_1;
            if (old) ref_object(old, "set_modify_command");
        }
    }

    free_svalue(sp);

    /* Return the old setting */
    if (old)
        put_object(sp, old); /* reuse ref count */
    else
        put_number(sp, 0);

    return sp;
} /* f_set_modify_command() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_set_prompt (struct svalue *sp)

/* TEFUN set_prompt()
 *
 *       string set_prompt(mixed prompt, object ob)
 *
 * Set the prompt given by the first argument for the interactive object
 * instead of the default ``> ''. If the second argument is omitted,
 * this_player() is used as default. The first arg can be a string or a
 * closure.
 *
 * The result returned is the old prompt.
 */

{
    struct svalue *prompt;
    struct interactive *ip;

    /* Make sure the object is interactive */
    if (sp->type != T_OBJECT
     || !(ip = O_GET_INTERACTIVE(sp->u.ob))
     || ip->sent.type != SENT_INTERACTIVE
     || ip->closing)
    {
        bad_xefun_arg(2, sp);
    }

    /* Get the address of the prompt svalue */
    prompt = query_prompt(sp->u.ob);

    free_object_svalue(sp);
    sp--;

    if (sp->type == T_STRING || sp->type == T_CLOSURE)
    {
        if (sp->type == T_STRING
         && sp->x.string_type == STRING_VOLATILE)
        {
            char *str = make_shared_string(sp->u.string);

            if (!str)
            {
                inter_sp = sp;
                error("Out of memory\n");
            }
            else
            {
                sp->u.string = str;
                sp->x.string_type = STRING_SHARED;
            }
        }

        /* Three-way exchange to set the new prompt and put
         * the old one onto the stack.
         */
        sp[1] = *prompt;
        *prompt = *sp;
        *sp = sp[1];
    }
    else if (sp->type == T_NUMBER
          && (sp->u.number == 0 || sp->u.number == -1) )
    {
        assign_svalue(sp, prompt);
    }
    else
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }

    return sp;
} /* f_set_prompt() */

/*=========================================================================*/
/*                              VALUES                                     */

/*-------------------------------------------------------------------------*/
struct svalue *
f_copy (struct svalue *sp)

/* TEFUN copy()
 *
 *    mixed copy(mixed data)
 *
 * Make a copy of <data> and return it. For everything but arrays and
 * mappings this is obviously a noop, but for arrays and mappings this
 * efuns returns a shallow value copy.
 */

{
    switch (sp->type)
    {
    default:
        NOOP
        break;

    case T_QUOTED_ARRAY:
    case T_POINTER:
      {
        struct vector *old, *new;
        size_t size, i;

        old = sp->u.vec;
        size = VEC_SIZE(old);
        if (old->ref != 1 && old != &null_vector)
        {
            new = allocate_uninit_array((int)size);
            if (!new)
                error("Out of memory.\n");
            for (i = 0; i < size; i++)
                assign_svalue_no_free(&new->item[i], &old->item[i]);
            free_array(old);
            sp->u.vec = new;
        }
        break;
      }
    case T_MAPPING:
      {
        struct mapping *old, *new;

        old = sp->u.map;
        if (old->ref != 1)
        {
            new = copy_mapping(old);
            if (!new)
                error("Out of memory.\n");
            free_mapping(old);
            sp->u.map = new;
        }
        break;
      }
    }

    return sp;
} /* f_copy() */

/*-------------------------------------------------------------------------*/

/* Data packet passed to deep_copy_mapping() during a mapping walk.
 */
struct csv_info {
    int width;                     /* width of the mapping */
    struct mapping * dest;         /* the mapping to copy into */
    struct pointer_table *ptable;  /* the pointer table to use */
};

/*-------------------------------------------------------------------------*/
static void
deep_copy_mapping (struct svalue *key, struct svalue *val, void *extra)

/* Called from copy_svalue() as part of the mapping walk to deeply copy
 * a mapping. <extra> is a (struct csv_info *).
 */

{
    struct csv_info *info = (struct csv_info *)extra;
    struct svalue newkey;
    struct svalue *newdata;
    int i;

    copy_svalue(&newkey, key, info->ptable);
    newdata = get_map_lvalue_unchecked(info->dest, &newkey);
    for (i = info->width; i-- > 0; newdata++, val++)
        copy_svalue(newdata, val, info->ptable);

    free_svalue(&newkey); /* no longer needed */
}

/*-------------------------------------------------------------------------*/
static void
copy_svalue (struct svalue *dest, struct svalue *src
            , struct pointer_table *ptable)

/* Copy the svalue <src> into the yet uninitialised svalue <dest>.
 * If <src> is an array or mapping, recurse to achieve a deep copy, using
 * <ptable> to keep track of the arrays and mappings encountered.
 *
 * The records in the pointer table store the svalue* of the created
 * copy for each registered array and mapping in the .data member.
 */

{
    switch (src->type)
    {
    default:
        assign_svalue_no_free(dest, src);
        break;

    case T_QUOTED_ARRAY:
    case T_POINTER:
      {
        struct pointer_record *rec;
        struct vector *old, *new;
        mp_int size, i;

        old = src->u.vec;

        /* No need to copy the null vector */
        if (old == &null_vector)
        {
            assign_svalue_no_free(dest, src);
            break;
        }

        /* Lookup/add this array to the pointer table */
        rec = find_add_pointer(ptable, old, MY_TRUE);

        if (rec->ref_count++ < 0) /* New array */
        {
            size = (mp_int)VEC_SIZE(old);

            /* Create a new array, assign it to dest, and store
             * it in the table, too.
             */
            new = allocate_uninit_array(size);
            put_array(dest, new);
            if (src->type == T_QUOTED_ARRAY)
                dest->x.quotes = src->x.quotes;
            rec->data = dest;

            /* Copy the values */
            for (i = 0; i < size; i++)
            {
                struct svalue * svp = &old->item[i];

                if (svp->type == T_MAPPING || svp->type == T_POINTER)
                    copy_svalue(&new->item[i], svp, ptable);
                else
                    assign_svalue_no_free(&new->item[i], svp);
            }
        }
        else /* shared array we already encountered */
        {
            assign_svalue_no_free(dest, (struct svalue *)rec->data);
        }
        break;
      }
    case T_MAPPING:
      {
        struct mapping *old, *new;
        struct pointer_record *rec;

        old = src->u.map;

        /* Lookup/add this mapping to the pointer table */
        rec = find_add_pointer(ptable, old, MY_TRUE);
        if (rec->ref_count++ < 0) /* New mapping */
        {
            mp_int size;
            struct csv_info info;

            /* Create a new array, assign it to dest, and store it
             * in the table, too.
             */
            check_map_for_destr(old);
            size = (mp_int)MAP_SIZE(old);
            info.width = old->num_values;
            new = allocate_mapping(size, info.width);
            if (!new)
                error("Out of memory.\n");
            put_mapping(dest, new);
            rec->data = dest;

            /* It is tempting to use copy_mapping() and then just
             * replacing all array/mapping references, but since this
             * can mess up the sorting order and needs a walk of the
             * mapping anyway, we do all the copying in the walk.
             */
            info.ptable = ptable;
            info.dest = new;
            walk_mapping(old, deep_copy_mapping, &info);
        }
        else /* shared mapping we already encountered */
        {
            assign_svalue_no_free(dest, (struct svalue *)rec->data);
        }
        break;
      }
    } /* switch(src->type) */
} /* copy_svalue() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_deep_copy (struct svalue *sp)

/* TEFUN deep_copy()
 *
 *    mixed deep_copy(mixed data)
 *
 * Make a copy of <data> and return it. For everything but arrays and
 * mappings this is obviously a noop, but for arrays and mappings this
 * efuns returns a deep value copy.
 *
 * Note: checking the ref-count of the array/mapping passed is of no use
 * here as it doesn't tell anything about the contained arrays/mappings.
 */

{
    struct pointer_table *ptable;

    switch (sp->type)
    {
    default:
        NOOP
        break;

    case T_POINTER:
      {
        struct vector *old;

        old = sp->u.vec;
        if (old != &null_vector)
        {
            struct svalue new;

            ptable = new_pointer_table();
            if (!ptable)
                error("Out of memory.\n");
            copy_svalue(&new, sp, ptable);
            transfer_svalue(sp, &new);
            free_pointer_table(ptable);
        }
        break;
      }
    case T_MAPPING:
      {
        struct mapping *old;
        struct svalue new;

        old = sp->u.map;
        ptable = new_pointer_table();
        if (!ptable)
            error("Out of memory.\n");
        copy_svalue(&new, sp, ptable);
        transfer_svalue(sp, &new);
        free_pointer_table(ptable);
        break;
      }
    }

    return sp;
} /* f_deep_copy() */

/*=========================================================================*/
/*                               OTHER                                     */

/*-------------------------------------------------------------------------*/
struct svalue *
f_debug_info (struct svalue *sp, int num_arg)

/* VEFUN debug_info()
 *
 *   mixed debug_info(int flag, object obj)
 *
 * Print out some driver internal debug information.
 *
 * DINFO_OBJECT (0): Information like heart_beat, enable_commands etc. of the
 * specified object will be printed, and 0 returned.
 *
 * DINFO_MEMORY (1): Memory usage information like how many strings, variables,
 * inherited files, object size etc. will be printed about the specified
 * object, and 0 returned.
 *
 * DINFO_OBJLIST (2): Objects from the global object list are returned.  If the
 * optional second arg is omitted, the first element (numbered 0) is returned.
 * If the second arg is a number n, the n'th element of the object list
 * returned. If the second arg is an object, it's successor in the object list
 * is returned.
 *
 * DINFO_MALLOC: Equivalent to typing ``malloc'' at the command line. No second
 * arg must be given. Returns 0.
 *
 * DINFO_STATUS (4): Collect the status information of the driver.  The
 * optional second arg can be 0, "tables", "swap", "malloc" or any other
 * argument accepted by the actual driver.  The result is a printable
 * string with the status information, or 0 if an invalid argument was
 * given.
 *
 * DINFO_DUMP (5): Dump the information specified by <arg2> into the
 * filename specified by <arg3>. If <arg3> is omitted, a default file
 * name is used. The function calls master->valid_write() to check that
 * it can write the files. The file in question is always written anew.
 * Result is 1 on success, or 0 if an error occured.
 *
 * <arg2> == "objects": dump information about all objects. Default
 *   filename is '/OBJ_DUMP', the valid_write() will read 'objdump' for
 *   the function.
 *
 * <arg2> == "opcodes": dump the usage statistics of the opcodes. Default
 *   filename is '/OPC_DUMP', the valid_write() will read 'opcdump' for
 *   the function. If the driver is compiled without OPCPROF, this call
 *   will always return 0.
 */

{
    struct svalue *arg;
    struct svalue res;
    struct object *ob;

    arg = sp-num_arg+1;
    inter_sp = sp;

    TYPE_TESTV1(arg, T_NUMBER)

    assign_svalue_no_free(&res, &const0);
    assign_eval_cost();
    switch ( arg[0].u.number )
    {
    case DINFO_OBJECT:  /* --- DINFO_OBJECT --- */
      {
        /* Give information about an object, deciphering it's flags, nameing
         * it's position in the list of all objects, total light and all the
         * stuff that is of interest with respect to look_for_objects_to_swap.
         */
        /* TODO: Return the data, don't dump it */

        int flags;
        struct object *prev, *obj2;

        if (num_arg != 2)
            error("bad number of arguments to debug_info\n");
        TYPE_TESTV2(arg+1, T_OBJECT)
        ob = arg[1].u.ob;
        flags = ob->flags;
        add_message("O_HEART_BEAT      : %s\n",
          flags&O_HEART_BEAT      ?"TRUE":"FALSE");
#ifdef O_IS_WIZARD
        add_message("O_IS_WIZARD       : %s\n",
          flags&O_IS_WIZARD       ?"TRUE":"FALSE");
#endif
        add_message("O_ENABLE_COMMANDS : %s\n",
          flags&O_ENABLE_COMMANDS ?"TRUE":"FALSE");
        add_message("O_CLONE           : %s\n",
          flags&O_CLONE           ?"TRUE":"FALSE");
        add_message("O_DESTRUCTED      : %s\n",
          flags&O_DESTRUCTED      ?"TRUE":"FALSE");
        add_message("O_SWAPPED         : %s\n",
          flags&O_SWAPPED          ?"TRUE":"FALSE");
        add_message("O_ONCE_INTERACTIVE: %s\n",
          flags&O_ONCE_INTERACTIVE?"TRUE":"FALSE");
        add_message("O_RESET_STATE     : %s\n",
          flags&O_RESET_STATE     ?"TRUE":"FALSE");
        add_message("O_WILL_CLEAN_UP   : %s\n",
          flags&O_WILL_CLEAN_UP   ?"TRUE":"FALSE");
        add_message("O_REPLACED        : %s\n",
          flags&O_REPLACED        ?"TRUE":"FALSE");
#ifdef F_SET_LIGHT
        add_message("total light : %d\n", ob->total_light);
#endif
        add_message("time_reset  : %ld\n", (long)ob->time_reset);
        add_message("time_of_ref : %ld\n", (long)ob->time_of_ref);
        add_message("ref         : %ld\n", ob->ref);
#ifdef DEBUG
        add_message("extra_ref   : %ld\n", ob->extra_ref);
#endif
        if (ob->gigaticks)
            add_message("evalcost   :  %lu%09lu\n", ob->gigaticks, ob->ticks);
        else
            add_message("evalcost   :  %lu\n", ob->ticks);
        add_message("swap_num    : %ld\n", O_SWAP_NUM(ob));
        add_message("name        : '%s'\n", ob->name);
        add_message("load_name   : '%s'\n", ob->load_name);
        obj2 = ob->next_all;
        if (obj2)
            add_message("next_all    : OBJ(%s)\n",
              obj2->next_all?obj2->name:"NULL");
        prev = ob->prev_all;
        if (prev) {
            add_message("Previous object in object list: OBJ(%s)\n"
                       , prev->name);
        } else
            add_message("This object is the head of the object list.\n");
        break;
      }

    case DINFO_MEMORY:  /* --- DINFO_MEMORY --- */
      {
        /* Give information about an object's program with regard to memory
         * usage. This is meant to point out where memory can be saved in
         * program structs.
         */
        /* TODO: Return the data, don't dump it */

        struct program *pg;

        if (num_arg != 2)
            error("bad number of arguments to debug_info\n");
        TYPE_TESTV2(arg+1, T_OBJECT)
        if (O_PROG_SWAPPED(sp->u.ob) && load_ob_from_swap(sp->u.ob) < 0)
            error("Out of memory\n");
        pg = sp->u.ob->prog;
        add_message("program ref's %3ld\n",        pg->ref);
        add_message("Name: '%s'\n",                pg->name);
        add_message("program size    %6ld\n"
          ,(long)(PROGRAM_END(*pg) - pg->program));
        add_message("num func's:  %3d (%4ld)\n", pg->num_functions
          , (long)(pg->num_functions * sizeof(uint32) +
                  pg->num_function_names * sizeof(short)));
        add_message("num vars:    %3d (%4ld)\n", pg->num_variables
          , (long)(pg->num_variables * sizeof(struct variable)));
        add_message("num strings: %3d (%4ld)\n", pg->num_strings
          , (long)(pg->num_strings   * sizeof(char *)));
        {
            int i = pg->num_inherited;
            int cnt = 0;
            struct inherit *inheritp;

            for (inheritp = pg->inherit; i--; inheritp++)
            {
                if (!inheritp->is_extra)
                    cnt++;
            }
            add_message("num inherits %3d (%4ld)\n", cnt
                , (long)(pg->num_inherited * sizeof(struct inherit)));
        }
        add_message("total size      %6ld\n"
          ,pg->total_size);
        break;
      }

    case DINFO_OBJLIST:  /* --- DINFO_OBJLIST --- */
      {
        /* Get the first/next object in the object list */

        int i;
        ob = obj_list;
        i = 0;

        if (num_arg > 1)
        {
            if (num_arg > 2)
                error("bad number of arguments to debug_info\n");

            if (sp->type == T_NUMBER)
            {
                i = sp->u.number;
            }
            else
            {
                TYPE_TESTV2(sp, T_OBJECT)
                ob = sp->u.ob;
                i = 1;
            }
        }

        while (ob && --i >= 0) ob = ob->next_all;
        if (ob)
            put_ref_object(&res, ob, "debug_info");
        break;
      }

    case DINFO_MALLOC:  /* --- DINFO_MALLOC --- */
      {
        /* Print the malloc data */
        /* TODO: This case can go, DINFO_STATUS "malloc" is sufficient */

        strbuf_t sbuf;

        status_parse(&sbuf, "malloc");
        strbuf_send(&sbuf);
        break;
      }

    case DINFO_STATUS:  /* --- DINFO_STATUS --- */
      {
        /* Execute the 'status' command */

        strbuf_t sbuf;

        if (num_arg != 1 && num_arg != 2)
            error("bad number of arguments to debug_info\n");
        if (num_arg == 1
         || (sp->type == T_NUMBER && sp->u.number == 0)) {
            sp->u.string = "";
        } else {
            TYPE_TESTV2(arg+1, T_STRING)
        }
        if (status_parse(&sbuf, sp->u.string))
            strbuf_store(&sbuf, &res);
        else
            strbuf_free(&sbuf);
        break;
      }

    case DINFO_DUMP:  /* --- DINFO_DUMP --- */
      {
        /* Dump information into files */

        char * fname;

        if (num_arg != 2 && num_arg != 3)
            error("bad number of arguments to debug_info\n");

        TYPE_TESTV2(arg+1, T_STRING);
        if (num_arg == 2
         || (sp->type == T_NUMBER && sp->u.number == 0)) {
            fname = NULL;
        } else {
            TYPE_TESTV3(arg+2, T_STRING)
            fname = sp->u.string;
        }

        if (!strcmp(arg[1].u.string, "objects"))
        {
            res.u.number = dumpstat(fname ? fname : "/OBJ_DUMP") ? 1 : 0;
            break;
        }

        if (!strcmp(arg[1].u.string, "opcodes"))
        {
#ifdef OPCPROF
            res.u.number = opcdump(fname ? fname : "/OPC_DUMP") ? 1 : 0;
#endif
            break;
        }

        error("Bad argument '%s' to debug_info(DINFO_DUMP).\n", arg[1].u.string);
        break;
      }

    default: bad_xefun_vararg(1, sp);
    }

    sp = pop_n_elems(num_arg, sp);

    sp++;
    *sp = res;
    return sp;
} /* f_debug_info() */

/***************************************************************************/

