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
 *    tefun: upper_case()
 *    efun:  terminal_colour()
 *
 * Objects:
 *    xefun: all_environment()
 *    tefun: object_info()
 *
 * Values:
 *    tefun: copy()
 *    tefun: deep_copy()
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
#include "datatypes.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "ptrtable.h"
#include "simulate.h"
#include "smalloc.h"
#include "stralloc.h"
#include "swap.h"


/* Forward declarations */
static void copy_svalue (struct svalue *dest, struct svalue *, struct pointer_table *);

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
    if (sp->type != T_STRING)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }

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

    if (sp->type != T_STRING)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }

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
                if (islower((unsigned char)c))
                    *s = toupper(c);
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
                memcpy(str, sp->u.string, initial_len);

            /* Copy and change the rest */
            for (d = str + initial_len; '\0' != (c = *s++) ; )
            {
                if (islower((unsigned char)c))
                    c = toupper(c);
                *d++ = c;
            }

            *d = '\0';
            free_string_svalue(sp);
            sp->type = T_STRING;
            sp->x.string_type = STRING_MALLOC;
            sp->u.string = str;
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

        if (cp == instr)
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
    }

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
        int len;
        char * str;
        struct svalue * mdata;

        /* If parts[i] is a valid colour key, there must exist a shared
         * string for it. Is that the case, look up parts[i] in the
         * mapping and set the result in mdata, otherwise save that effort.
         */
        str = findstring(parts[i]);
        if (str != NULL)
        {
            struct svalue mkey;

            mkey.type = T_STRING;
            mkey.x.string_type = STRING_SHARED;
            mkey.u.string = str;
              /* The only use of mkey is to index a mapping - an operation
               * which will not decrement the refcount for <str>. This
               * makes it safe to not count the ref by mkey here, and saves
               * a bit time.
               */

            /* now look for mapping data */
            mdata = get_map_lvalue(map, &mkey, MY_FALSE);
        }
        else
            mdata = NULL;

        /* If mdata found a string, use it instead of the old parts[i].
         * Not its length, making it negative where necessary.
         */
        if ( mdata && mdata->type == T_STRING )
        {
            parts[i] = mdata->u.string;
            len = svalue_strlen( mdata );
            if (wrap)
                len = -len;
        }
        else
            len = strlen(parts[i]);

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
    deststr = xalloc(j+1);
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
#       define TMPMEM_SIZE 8192
        char *tmpmem;
          /* Temporary buffer for the current line */
        char *pt;
          /* Pointer into tmpmem */

        tmpmem = xalloc(TMPMEM_SIZE);
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
            
            if (pt - tmpmem + ((l < 0) ? -l : l) >= TMPMEM_SIZE)
            {
                error("Partial string too long (> %d).\n", TMPMEM_SIZE);
                /* NOTREACHED */
                return NULL;
            }

            if (l < 0)
            {
                /* String retrieved from the mapping: not to be counted */
                memcpy(pt, p, -l);
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
                memcpy(cp, tmpmem, n);
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
                move_memory(tmpmem, tmpmem + n, len);
                pt = tmpmem + len;
                
                if (len > space_garbage || !at_end(i, num, k, lens))
                {
                    /* There will be data coming next: insert the
                     * indentation.
                     */
                    memset(cp, ' ', indent);
                    cp += indent;
                    col += indent;
                }
            }
        } /* for(i = 0..num) */

        memcpy(cp, tmpmem, pt - tmpmem);
        cp += pt - tmpmem;
        xfree(tmpmem);
#       undef TMPMEM_SIZE
    }
    else
    {
        /* No wrapping: just catenate the parts (and all lens[] entries
         * are positive here)
         */
        for (i = 0; i < num; i++)
        {
            memcpy(cp, parts[i], lens[i]);
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
        o = sp->u.ob;
        add_ref(o, "all_environment");
        free_object_svalue(sp);
    }
    else
    {
        o = current_object;
        sp++;
    }


    /* Default return value: 0 */
    sp->type = T_NUMBER;
    sp->u.number = 0;

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
                svp->type = T_OBJECT;
                svp->u.ob = env;
                add_ref(env, "all_environment");
            }

            /* Put the result on the stack and return */
            sp->type = T_POINTER;
            sp->u.vec = v;
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
    if (sp[-1].type != T_OBJECT)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }
    o = sp[-1].u.ob;

    if (sp->type != T_NUMBER)
    {
        bad_xefun_arg(2, sp);
        /* NOTREACHED */
        return sp;
    }

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
    case 0:
        v = allocate_array(26);
        svp = v->item;

        flags = o->flags;

        svp[ 0].u.number = (flags & O_HEART_BEAT) ? 1 : 0;
        svp[ 1].u.number = (flags & O_IS_WIZARD) ? 1 : 0;
        svp[ 2].u.number = (flags & O_ENABLE_COMMANDS) ? 1 : 0;
        svp[ 3].u.number = (flags & O_CLONE) ? 1 : 0;
        svp[ 4].u.number = (flags & O_DESTRUCTED) ? 1 : 0;
        svp[ 5].u.number = (flags & O_SWAPPED) ? 1 : 0;
        svp[ 6].u.number = (flags & O_ONCE_INTERACTIVE) ? 1 : 0;
        svp[ 7].u.number = (flags & O_APPROVED) ? 1 : 0;
        svp[ 8].u.number = (flags & O_RESET_STATE) ? 1 : 0;
        svp[ 9].u.number = (flags & O_WILL_CLEAN_UP) ? 1 : 0;
        svp[10].u.number = (flags & O_LAMBDA_REFERENCED) ? 1 : 0;
        svp[11].u.number = (flags & O_SHADOW) ? 1 : 0;
        svp[12].u.number = (flags & O_REPLACED) ? 1 : 0;
#ifdef F_SET_LIGHT
        svp[13].u.number = o->total_light;
#else
        svp[13].u.number = 0;
#endif
#ifndef OLD_RESET
        svp[14].u.number = o->time_reset;
#else
        svp[14].u.number = o->next_reset;
#endif
        svp[15].u.number = o->time_of_ref;
        svp[16].u.number = o->ref;
        svp[17].u.number = o->gigaticks;
        svp[18].u.number = o->ticks;
        svp[19].u.number = O_SWAP_NUM(o);
        svp[20].u.number = O_PROG_SWAPPED(o) ? 1 : 0;
        svp[21].u.number = O_VAR_SWAPPED(o) ? 1 : 0;

        svp[22].type = T_STRING;
        svp[22].x.string_type = STRING_MALLOC;
        svp[22].u.string = string_copy(o->name);

        svp[23].type = T_STRING;
        svp[23].x.string_type = STRING_SHARED;
        svp[23].u.string = o->load_name;
        increment_string_ref(o->load_name);

        o2 = o->next_all;
        if (o2)
        {
            svp[24].type = T_OBJECT;
            svp[24].u.ob = o2;
            add_ref(o2, "object_info(0)");
        } /* else the element was already allocated as 0 */

        o2 = o->prev_all;
        if (o2)
        {
            svp[25].type = T_OBJECT;
            svp[25].u.ob = o2;
            add_ref(o2, "object_info(0)");
        } /* else the element was already allocated as 0 */

        break;

    /* --- Position in the object list */
    case 1:
        v = allocate_array(3);
        svp = v->item;

        o2 = o->next_all;
        if (o2)
        {
            svp[0].type = T_OBJECT;
            svp[0].u.ob = o2;
            add_ref(o2, "object_info(1) next");
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
                svp[1].type = T_OBJECT;
                svp[1].u.ob = prev;
                add_ref(prev, "object_info(1) prev");
            } /* else the element was already allocated as 0 */
        }
        else /* Not found (this shouldn't happen) */
            pos = -1;

        svp[2].u.number = pos;

        break;

    /* --- Memory information */
    case 2:
        v = allocate_array(12);
        svp = v->item;

        if (O_PROG_SWAPPED(o) && load_ob_from_swap(o) < 0)
            error("Out of memory.\n");

        prog = o->prog;

        svp[ 0].u.number = prog->ref;

        svp[ 1].type = T_STRING;
        svp[ 1].x.string_type = STRING_MALLOC;
        svp[ 1].u.string = string_copy(prog->name);

        svp[ 2].u.number = (long)(PROGRAM_END(*prog) - prog->program);
          /* Program size */
        svp[ 3].u.number = prog->num_functions;
        svp[ 4].u.number = prog->num_functions * sizeof(uint32)
                        + prog->num_function_names * sizeof(short);
          /* Number of function names and the memory usage */
        svp[ 5].u.number = prog->num_variables;
        svp[ 6].u.number = prog->num_variables * sizeof(struct variable);
          /* Number of variables and the memory usage */
        svp[ 7].u.number = prog->num_strings;
        svp[ 8].u.number = prog->num_strings * sizeof(char*);
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
            svp[ 9].u.number = cnt;
        }
        svp[10].u.number = prog->num_inherited * sizeof(struct inherit);
          /* Number of inherites and the memory usage */
        svp[11].u.number = prog->total_size;
        break;
    }

    free_svalue(sp);
    sp--;
    free_svalue(sp);

    /* Assign the result */
    sp->type = T_POINTER;
    sp->u.vec = v;

    return sp;
}

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
        mp_int size, i;

        old = sp->u.vec;
        size = VEC_SIZE(old);
        if (old->ref != 1 && old != &null_vector)
        {
            new = allocate_uninit_array(size);
            if (!new)
                error("Out of memory.\n");
            for (i = 0; i < size; i++)
                assign_svalue_no_free(&new->item[i], &old->item[i]);
            free_vector(old);
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
    newdata = get_map_lvalue(info->dest, &newkey, MY_TRUE);
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
            size = VEC_SIZE(old);

            /* Create a new array, assign it to dest, and store
             * it in the table, too.
             */
            new = allocate_uninit_array(size);
            dest->type = T_POINTER;
            dest->u.vec = new;
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
            size =   old->condensed->string_size/sizeof(char *)
                   + old->condensed->misc_size/sizeof(struct svalue *)
                   + (old->hash
                        ? old->hash->used - old->hash->condensed_deleted : 0);
            info.width = old->num_values;
            new = allocate_mapping(size, info.width);
            if (!new)
                error("Out of memory.\n");
            dest->type = T_MAPPING;
            dest->u.map = new;
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
/***************************************************************************/

