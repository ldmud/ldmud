/*---------------------------------------------------------------------------
 * Various Efuns.
 *
 *---------------------------------------------------------------------------
 * This file acts as a repository for various old and new efuns. Over the time
 * it will probably grow large enough to justify a split into several files.
 *
 * The implemented efuns, sorted by topic, are:
 *
 * Strings:
 *    TODO: Move into strfuns.c, rename the old strfuns to strutil.
 *    efun: capitalize()
 *    efun: crypt()
 *    efun: make_shared_string()
 *    efun: md5_encrypt()
 *    efun: regexp()
 *    efun: regexplode()
 *    efun: regreplace()
 *    efun: process_string() (optional)
 *    efun: sscanf()
 *    efun: strstr()
 *    efun: terminal_colour()
 *    efun: trim()
 *    efun: upper_case()
 *
 * Objects:
 *    TODO: Move into object.c.
 *    efun: clones()
 *    efun: object_info()
 *    efun: present_clone()
 *    efun: set_is_wizard() (optional)
 *
 * Values:
 *    efun: abs()
 *    efun: sin()
 *    efun: asin()
 *    efun: cos()
 *    efun: acos()
 *    efun: tan()
 *    efun: atan()
 *    efun: atan2()
 *    efun: log()
 *    efun: exp()
 *    efun: sqrt()
 *    efun: ceil()
 *    efun: floor()
 *    efun: pow()
 *    efun: to_int()
 *    efun: to_float()
 *    efun: to_string()
 *    efun: to_array()
 *    efun: to_object()
 *    efun: copy()
 *    efun: deep_copy()
 *    efun: filter()
 *    efun: get_type_info()
 *    efun: map()
 *    efun: member()
 *    efun: min()
 *    efun: max()
 *    efun: sgn()
 *    efun: quote()
 *
 * Others:
 *    efun: ctime()
 *    efun: debug_info()
 *    efun: rusage() (optional)
 *    efun: shutdown()
 *    efun: time()
 *    efun: utime()
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include "my-rusage.h"
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "efuns.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "dumpstat.h"
#include "heartbeat.h"
#include "instrs.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "md5.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#include "ptrtable.h"
#include "random.h"
#include "rxcache.h"
#include "stdstrings.h"
#include "simulate.h"
#include "smalloc.h" /* smalloc_dinfo_data() */
#include "strfuns.h"
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/objectinfo.h"
#include "../mudlib/sys/strings.h"

/* Forward declarations */
static void copy_svalue (svalue_t *dest, svalue_t *, struct pointer_table *);

/* Macros */

/*-------------------------------------------------------------------------*/

#ifdef F_SET_IS_WIZARD
Bool is_wizard_used = MY_FALSE;
  /* TODO: This flag can go when the special commands are gone. */
#endif


/*=========================================================================*/
/*                              STRINGS                                    */

/*-------------------------------------------------------------------------*/
svalue_t *
f_capitalize(svalue_t *sp)

/* EFUN capitalize()
 *
 *     string capitalize(string str)
 *
 * Convert the first character in str to upper case, and return
 * the new string.
 */

{
    if (islower((unsigned char)(get_txt(sp->u.str)[0])))
    {
        string_t *new;
        memsafe(new = dup_mstring(sp->u.str), mstrsize(sp->u.str), "result string");
        free_mstring(sp->u.str);
        sp->u.str = new;
        get_txt(sp->u.str)[0] += 'A' - 'a';
    }
    return sp;
} /* f_capitalize() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_crypt(svalue_t *sp)

/* EFUN crypt()
 *
 *   string crypt(string str, int seed)
 *   string crypt(string str, string seed)
 *
 * Crypt the string str using the integer seed or two characters
 * from the string seed as a seed. If seed is equal 0, then
 * a random seed is used.
 *
 * The result has the first two characters as the seed.
 */

{
    char *salt;
    char *res;
    char temp[3];
    static char choise[] =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./";

    if (sp->type == T_STRING && mstrsize(sp->u.str) >= 2)
    {
        salt = get_txt(sp->u.str);
    }
    else if (sp->type == T_NUMBER)
    {
        temp[0] = choise[random_number((sizeof choise) - 1)];
        temp[1] = choise[random_number((sizeof choise) - 1)];
        temp[2] = '\0';
        salt = temp;
    }
    else /* it can't be anything but a too short string */
        error("Bad argument 2 to crypt(): string too short.\n");

    res = crypt(get_txt((sp-1)->u.str), salt);
    sp = pop_n_elems(2, sp);
    push_c_string(sp, res);

    return sp;
} /* f_crypt() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_explode (svalue_t * sp)

/* EFUN explode()
 *
 *   string *explode(string str, string del)
 *
 * Return an array of strings, created when the string str is
 * split into substrings as divided by del.
 */

{
    vector_t *v;

    v = explode_string((sp-1)->u.str, sp->u.str);
    free_string_svalue(sp);
    sp--;
    free_string_svalue(sp);
    put_array(sp,v);

    return sp;
} /* f_explode() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_implode (svalue_t * sp)

/* EFUN implode()
 *
 *   string implode(mixed *arr, string del)
 *
 * Concatenate all strings found in array arr, with the string
 * del between each element. Only strings are used from the array.
 */

{
    string_t *str;

    str = implode_string((sp-1)->u.vec, sp->u.str);
    if (!str)
        error("Out of memory for implode() result.\n");

    free_string_svalue(sp);
    sp--;
    free_array(sp->u.vec);

    if (str)
        put_string(sp, str);
    else
        put_number(sp, 0);
    return sp;
} /* f_implode() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_lower_case (svalue_t *sp)

/* EFUN lower_case()
 *
 *   string lower_case(string str)
 *
 * Convert all characters in str to lower case, and return the
 * new string.
 */

{
    char *s, c;
    size_t count, len;

    /* Find the first uppercase character */
    len = mstrsize(sp->u.str);
    for ( s = get_txt(sp->u.str), count = 0
        ; count < len && ('\0' == (c = *s) || !isupper((unsigned char)c))
        ; s++, count++) NOOP;

    if (count < len)
    {
        /* Yes, there is something to change... */

        string_t *new;
        memsafe(new = dup_mstring(sp->u.str), mstrsize(sp->u.str), "result string");
        free_mstring(sp->u.str);
        sp->u.str = new;

        for ( s = get_txt(sp->u.str)+count; count < len; s++, count++)
        {
            c = *s;
            if (c != '\0' && isupper((unsigned char)c))
                *s = (char)tolower(c);
        }
    }

    return sp;
} /* f_lower_case() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_make_shared_string (svalue_t *sp)

/* EFUN make_shared_string()
 *
 *    string make_shared_string(string s)
 *
 * If the passed string <s> is not shared, the efun enters it into
 * the shared string table and returns the shared version. Else the
 * passed string is returned.
 */

{
    memsafe(sp->u.str = make_tabled(sp->u.str), mstrsize(sp->u.str), "result string");

    return sp;
} /* f_make_shared_string() */

/*--------------------------------------------------------------------*/
svalue_t *
f_md5_encrypt (svalue_t *sp)

/* EFUN: md5_encrypt()
 *
 *   string md5_encrypt(string arg)
 *
 * Create and return a MD5 message digest from the string <arg>.
 */

{
    MD5_CTX context;
    string_t *s_digest;
    unsigned char *digest, d[17];
    int i;
   
    memsafe(s_digest = alloc_mstring(32), 32, "md5 encryption result");
    digest = (unsigned char *)get_txt(s_digest);

    MD5Init(&context);
    MD5Update(&context, (unsigned char *)get_txt(sp->u.str), mstrsize(sp->u.str));
    MD5Final(&context, d);

    d[16]='\0';

    for (i = 0; i < 16; i++)
        sprintf((char *)digest+2*i, "%02x", d[i]);

    free_string_svalue(sp);
    put_string(sp, s_digest);

    return sp;
} /* f_md5_encrypt() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_regexp (svalue_t *sp)

/* EFUN regexp()
 *
 *   string *regexp(string *list, string pattern)
 *
 * Match the pattern pattern against all strings in list, and return a
 * new array with all strings that matched. This function uses the
 * same syntax for regular expressions as ed().
 */

{
    vector_t *v;               /* The vector to match */
    struct regexp *reg;        /* compiled regexp */
    CBool *res;                /* res[i] true -> v[i] matches */
    mp_int num_match, v_size;  /* Number of matches, size of <v> */
    vector_t *ret;             /* The result vector */
    string_t * pattern;        /* The pattern passed in */
    mp_int i;

    v = (sp-1)->u.vec;
    pattern = sp->u.str;
    ret = NULL;

    do {
        /* Simple case: empty input yields empty output */
        if ((v_size = (mp_int)VEC_SIZE(v)) == 0)
        {
            ret = allocate_array(0);
            break;
        }

        /* Compile the regexp (or take it from the cache) */
        reg = REGCOMP(pattern, 0, MY_FALSE);
        if (reg == NULL)
        {
            break;
        }

        /* Check every string in <v> if it matches and set res[]
         * accordingly.
         */
        res = alloca(v_size * sizeof(*res));
        if (!res)
        {
            REGFREE(reg);
            error("Stack overflow in regexp()");
            /* NOTREACHED */
            return sp;
        }

        for (num_match = i = 0; i < v_size; i++) {
            string_t *line;

            res[i] = MY_FALSE;

            if (v->item[i].type != T_STRING)
                continue;

            eval_cost++;
            line = v->item[i].u.str;
            if (regexec(reg, get_txt(line), get_txt(line)) == 0)
                continue;

            res[i] = MY_TRUE;
            num_match++;
        }

        /* Create the result vector and copy the matching lines */
        ret = allocate_array(num_match);
        for (num_match=i=0; i < v_size; i++) {
            if (!res[i])
                continue;
            assign_svalue_no_free(&ret->item[num_match], &v->item[i]);
            num_match++;
        }

        REGFREE(reg);
    }while(0);

    free_svalue(sp--);
    free_svalue(sp);
    if (ret == NULL)
        put_number(sp, 0);
    else
        put_array(sp, ret);

    return sp;
} /* f_regexp() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_regexplode (svalue_t *sp)

/* EFUN regexplode()
 *
 *   string *regexplode (string text, string pattern)
 *
 * Explode the <text> by the delimiter <pattern>, returning a vector
 * of the exploded text. Every second element in the result vector
 * is the text that matched the delimiter.
 * Evalcost: number of matches.
 */

{
    /* The found delimiter matches are kept in a list of these
     * structures which are allocated on the stack.
     */
    struct regexplode_match {
        char *start, *end;              /* Start and end of the match in text */
        struct regexplode_match *next;  /* Next list element */
    };

    char     *text;                    /* Input text from the vm stack */
    string_t *textstr;                 /* ditto, as string_t */
    string_t *pattern;                 /* Delimiter pattern from the vm stack */
    struct regexp *reg;                /* Compiled pattern */
    struct regexplode_match *matches;  /* List of matches */
    struct regexplode_match **matchp;  /* Pointer to previous_match.next */
    struct regexplode_match *match;    /* Current match structure */
    vector_t *ret;                     /* Result vector */
    svalue_t *svp;                     /* Next element in ret to fill in */
    int num_match;                     /* Number of matches */
    char *str;

    /* Get the efun arguments */

    textstr = sp[-1].u.str;
    text = get_txt(textstr);
    pattern = sp->u.str;

    reg = REGCOMP(pattern, 0, MY_FALSE);
    if (reg == 0) {
        inter_sp = sp;
        error("Unrecognized search pattern");
        /* NOTREACHED */
        return sp;
    }

    /* Loop over <text>, repeatedly matching it against the pattern,
     * until all matches have been found and recorded.
     */
    str = text;        /* Remaining <text> to analyse */
    num_match = 0;
    matchp = &matches;
    while (regexec(reg, str, text)) {
        eval_cost++;
        match = (struct regexplode_match *)alloca(sizeof *match);
        if (!match)
        {
            error("Stack overflow in regexplode()");
            /* NOTREACHED */
            return sp;
        }
        match->start = reg->startp[0];
        match->end = str = reg->endp[0];
        *matchp = match;
        matchp = &match->next;
        num_match++;
        if (!*str || (match->start == str && !*++str) )
            break;
    }
    *matchp = 0; /* Terminate list properly */

    /* Prepare the result vector */
    if (max_array_size && num_match > ((max_array_size-1) >> 1) ) {
        REGFREE(reg);
        inter_sp = sp;
        error("Illegal array size");
        /* NOTREACHED */
        return sp;
    }
    ret = allocate_array((num_match << 1) + 1);

    /* Walk down the list of matches, extracting the
     * text parts and matched delimiters, copying them
     * into ret.
     */
    svp = ret->item;
    for (match = matches; match; match = match->next) {
        mp_int len;
        string_t *txt;

        /* Copy the text leading up to the current delimiter match. */
        len = match->start - text;
        memsafe(txt = new_n_mstring(text, (size_t)len), (size_t)len, "text before delimiter");
        text += len;
        put_string(svp, txt);
        svp++;

        /* Copy the matched delimiter */
        len = match->end - text;
        memsafe(txt = new_n_mstring(text, (size_t)len), (size_t)len, "matched delimiter");
        text += len;
        put_string(svp, txt);
        svp++;
    }

    /* Copy the remaining text (maybe the empty string) */
    {
        mp_int len;
        string_t *txt;

        len = (mp_int)mstrsize(textstr) - (text - get_txt(textstr));
        memsafe(txt = new_n_mstring(text, (size_t)len), (size_t)len, "remaining text");
        put_string(svp, txt);
    }

    /* Cleanup */
    REGFREE(reg);
    free_string_svalue(sp);
    sp--;
    free_string_svalue(sp);

    /* Return the result */
    put_array(sp, ret);
    return sp;
} /* f_regexplode() */

/*-------------------------------------------------------------------------*/
svalue_t*
f_regreplace (svalue_t *sp)

/* EFUN regreplace()
 *
 *     string regreplace (string txt, string pattern, string replace
 *                                                  , int flags)
 *
 * Search through <txt> for one/all occurences of <pattern> and replace them
 * with the <replace> pattern, returning the result. <flags> is the bit-or
 * of these values:
 *   F_GLOBAL   = 1: when given, all occurences of <pattern> are replace,
 *                   else just the first
 *   F_EXCOMPAT = 2: when given, the expressions are ex-compatible,
 *                   else they aren't.
 * TODO: The gamedriver should write these values into an include file.
 *
 * The function behaves like the s/<pattern>/<replace>/<flags> command
 * in sed or vi. It offers an efficient and far more powerful replacement
 * for implode(regexplode()).
 */

{
#define F_GLOBAL   0x1
#define F_EXCOMPAT 0x2

    struct regexp *pat;
    int   flags;
    char *oldbuf, *buf, *curr, *new, *start, *old, *sub;
    long  space;
    size_t  origspace;

    /*
     * Must set inter_sp before call to regcomp,
     * because it might call regerror.
     */
    inter_sp = sp;

    /* Extract the arguments */
    flags = sp->u.number;
    sub = get_txt(sp[-1].u.str);
    start = curr = get_txt(sp[-3].u.str);

    space = (long)(origspace = (mstrsize(sp[-3].u.str) + 1)*2);
      /* The '+1' so that empty strings don't cause a 'malloc size = 0' error.
       */

/* reallocate on the fly */
#define XREALLOC \
    space += origspace;\
    origspace = origspace*2;\
    oldbuf = buf;\
    buf = (char*)rexalloc(buf,origspace);\
    if (!buf) { \
        xfree(oldbuf); \
        if (pat) REGFREE(pat); \
        error("(regreplace) Out of memory (%lu bytes) for buffer\n"\
             , (unsigned long)origspace); \
    } \
    new = buf + (new-oldbuf)

/* The rexalloc() above read originally 'rexalloc(buf, origspace*2)'.
 * Marcus inserted the '*2' since he experienced strange driver
 * crashes without. I think that the error corrected in dev.28 was the
 * real reason for the crashes, so that the safety factor is no longer
 * necessary. However, if regreplace() causes crashes again, this
 * is one thing to try.
 */

    xallocate(buf, (size_t)space, "buffer");
    new = buf;
    pat = REGCOMP(sp[-2].u.str,(flags & F_EXCOMPAT) ? 1 : 0, MY_FALSE);
    /* regcomp returns NULL on bad regular expressions. */

    if (pat && regexec(pat,curr,start)) {
        do {
            size_t diff = (size_t)(pat->startp[0]-curr);
            space -= diff; /* TODO: space -= diff+1 ? */
            while (space <= 0) {
                XREALLOC;
            }
            strncpy(new,curr,(size_t)diff);
            new += diff;
            old  = new;
            *old = '\0';

            /* Now what may have happen here. We *could*
             * be out of mem (as in 'space') or it could be
             * a regexp problem. the 'space' problem we
             * can handle, the regexp problem not.
             * hack: we store a \0 into *old ... if it is
             * still there on failure, it is a real failure.
             * if not, increase space. The player could get
             * some irritating messages from regerror()
             */
            while (NULL == (new = regsub(pat, sub, new, space, 1)) )
            {
                int xold;

                if (!*old)
                {
                    xfree(buf);
                    if (pat)
                        REGFREE(pat);
                    error("Internal error in regreplace().\n");
                    /* NOTREACHED */
                    return NULL;
                }
                xold = old - buf;
                XREALLOC;
                new = buf + xold;
                old = buf + xold;
                *old='\0';
            }
            space -= new - old;
            while (space <= 0) {
                XREALLOC;
            }
            if (curr == pat->endp[0])
            {
                /* prevent infinite loop
                 * by advancing one character.
                 */
                if (!*curr) break;
                --space;
                while (space <= 0) {
                    XREALLOC;
                }
                *new++ = *curr++;
            }
            else
                curr = pat->endp[0];
        } while ((flags&F_GLOBAL) && !pat->reganch && regexec(pat,curr,start));
        space -= strlen(curr)+1;
        if (space <= 0) {
            XREALLOC;
        }
        strcpy(new,curr);
    }
    else
    {
        /* Pattern not found -> no editing necessary */
        strcpy(buf,start);
    }

    if (pat)
        REGFREE(pat);

    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    put_c_string(sp, buf);
    xfree(buf);
    return sp;

#undef F_EXCOMPAT
#undef F_GLOBAL
#undef XREALLOC
}

/*-------------------------------------------------------------------------*/
svalue_t *
f_strstr (svalue_t *sp)

/* EFUN strstr()
 *
 *   int strstr (string str, string str2, int pos)
 *
 * Returns the index of str2 in str searching from position pos.
 * If str2 is not found in str, -1 is returned. The returned
 * index is relativ to the beginning of the string.
 *
 * If pos is negativ, it counts from the end of the string.
 */

{
    char *found;
    string_t *base, *pattern;
    p_int start;

    base = sp[-2].u.str;
    pattern = sp[-1].u.str;

    if ( 0 != (start = sp->u.number) )
    {
        if (start < 0)
        {
            start += mstrsize(base);
            if (start < 0)
                start = 0;
        }
    }

    found = mstring_mstr_n_str(base, start, get_txt(pattern), mstrsize(pattern));

    sp--;
    free_svalue(sp--);
    free_string_svalue(sp);
    put_number(sp, found ? (found - get_txt(base)) : -1);

    return sp;
} /* f_strstr() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_trim (svalue_t *sp, int num_arg)

/* EFUN trim()
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
    svalue_t * argp;
    string_t *strarg;    /* The string argument */
    size_t    strarg_l;  /* Length of *strarg */
    char *str, *end;     /* Pointer to string begin and end */
    char *left, *right;  /* Pointer to the strings left and right end */
    char def_ch[3]       /* Buffer for single characters to strip */
      = { '\t', ' ', '\0' };
    char *strip;         /* String of characters to strip */
    size_t strip_l;      /* Length of *strip */
    int  where;

    /* Get and test the arguments */
    argp = sp - num_arg + 1;

    strarg = argp->u.str;
    str = get_txt(strarg);
    strarg_l = mstrsize(strarg);

    if (num_arg > 1)
    {
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
        else /* it's a string */
        {
            strip = get_txt(argp[2].u.str);
            strip_l = mstrsize(argp[2].u.str);
        }
    }
    else
    {
       strip = def_ch;
       strip_l = 2;
    }

    /* Get the string limits */
    end = str + strarg_l;
    if (where & TRIM_LEFT)
    {
        for ( left = str
            ; left < str+strarg_l && memchr(strip, *left, strip_l) != NULL
            ; left++
            ) NOOP;
    }
    else
        left = str;

    if (where & TRIM_RIGHT && end != left)
    {
        for (right = end
            ; right != left && NULL != memchr(strip, right[-1], strip_l)
            ; right--) NOOP;
    }
    else
        right = end;

    /* If there are things to strip, create a new string and put it
     * into the place of the old one.
     */
    if (left != str || right != end)
    {
        string_t * trimmed;
        size_t newlen;

        newlen = (size_t)(right - left);
        memsafe(trimmed = new_n_mstring(left, newlen), newlen, "trimmed result");
        free_string_svalue(argp);
        put_string(argp, trimmed);
    }

    /* argp+2 might need to be freed, but argp+1 is always just a number.
     * And the result argp is fine as it is.
     */

    if (num_arg > 2 && argp[2].type == T_STRING)
        free_svalue(argp+2);

    return argp;
} /* f_trim() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_upper_case (svalue_t *sp)

/* EFUN upper_case()
 *
 *    string upper_case (string s)
 *
 * Convert all characters in <s> to upper case and return the new string.
 */

{
    char *s, c;
    size_t count, len;

    /* Find the first non-uppercase character in the string */
    len = mstrsize(sp->u.str);
    for (s = get_txt(sp->u.str), count = 0
        ; count < len && ('\0' == (c = *s) || !islower((unsigned char)c))
        ; s++, count++)
        NOOP;

    if (count < len)  /* there are lowercase characters */
    {
        string_t *new;
        memsafe(new = dup_mstring(sp->u.str), mstrsize(sp->u.str), "result string");
        free_mstring(sp->u.str);
        sp->u.str = new;

        for (s = get_txt(sp->u.str)+count; count < len; s++, count++)
        {
            c = *s;

            if ('\0' != c && islower((unsigned char)c))
                *s = (char)toupper(c);
        }
    }

    /* That's it */
    return sp;
}

/*-------------------------------------------------------------------------*/
static Bool
at_end (int i, int imax, int z, p_int *lens)

/* Auxilary function for e_terminal_colour().
 *
 * <z> is the position within string number <i>. <lens> is an array
 * with the lengths of all <imax> strings.
 *
 * The function returns true if there are no more characters to process
 * after <i>:<z> in all strings, else it returns false.
 */

{
    if (z + 1 < lens[i])
        return MY_FALSE;
    for (i++; i < imax; i++) {
        if (lens[i] > 0)
            return MY_FALSE;
    }
    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
static string_t *
e_terminal_colour ( string_t * text, mapping_t * map
                  , int indent, int wrap
                  )

/* Implementation of the efun terminal_colour().
 * See f_terminal_colour() for the complete description.
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
    string_t *savestr = NULL;  /* Allocated auxiliary string */
    char *instr;
      /* The input string. This may be get_txt(<text>) itself, or a working
       * copy. */
    string_t *deststr;         /* Result string */
    char **parts;
      /* The <num> delimited parts from <instr>. This are mostly
       * pointers into *<instr>, but can also be (uncounted) pointers to
       * the string data in <map>.
       */
    int num;               /* Number of delimited parts in <instr> */
    p_int *lens = NULL;
      /* Length of the <num> parts. This value is negative for strings
       * 'retrieved' from the <map>ping when wrapping is required. This
       * is necessary to determine which parts[] to exempt from the
       * wrapping calculation.
       */
    int k;                 /* Index within a string */
    int col;               /* Current print column */
    int j;                 /* Accumulated total length of result */
    int j_extra;           /* Temporary extra length of result before fmt'ing */
    int start;             /* Col of first non-blank character */
    int space;             /* Col of last space char */
    int i;
    Bool maybe_at_end;     /* TRUE if the next text might start a new line */
    Bool no_keys;          /* TRUE if no delimiter in the string */

    instr = get_txt(text);

    /* Find the first occurance of the magic character pair.
     * If found, duplicate the input string into instr and
     * let cp point into that copy at the delimiter.
     * If not found, cp will be NULL.
     */

    if (map != NULL)
    {
        cp = instr;
        do {
            cp = memchr(cp, TC_FIRST_CHAR, mstrsize(text));
            if (cp)
            {
                if (cp[1] == TC_SECOND_CHAR)
                {
                    memsafe(savestr = dup_mstring(text), mstrsize(text)
                           , "working string");
                    cp = get_txt(savestr) + (cp - instr);
                    instr = get_txt(savestr);
                    break;
                }
                cp++;
            }
        } while (cp);
    }
    else
        cp = NULL;

    /* If the delimiter was found, split up the instr into the
     * parts and store them. If not found, just return.
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
            lens = CALLOCATE(1, p_int);
            lens[0] = mstrsize(text);
            savestr = NULL;  /* should be NULL anyway */
            no_keys = MY_TRUE;
        }
        else
        {
            /* no delimiter in string and no wrapping, so return the original.
             */
            return ref_mstring(text);
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

        p_int left;

        /* cp here points to the first delimiter found */

        parts = CALLOCATE( NSTRSEGS, char * );
        if (!parts)
        {
            error("(terminal_colour) Out of memory (%lu bytes) "
                  "for %d parts.\n"
                 , (unsigned long) NSTRSEGS * sizeof(char*), NSTRSEGS);
            /* NOTREACHED */
            return NULL;
        }

        lens = CALLOCATE(NSTRSEGS, p_int);
        if (!lens)
        {
            xfree(parts);
            error("(terminal_colour) Out of memory (%lu bytes) "
                  "for %d parts.\n"
                 , (unsigned long) NSTRSEGS * sizeof(p_int), NSTRSEGS);
            /* NOTREACHED */
            return NULL;
        }

        /* The string by definition starts with a non-keyword,
         * which might be empty.
         * Initialize our variables accordingly.
         */
        num = 1;
        parts[0] = instr;
        lens[0] = cp - instr;
        left = mstrsize(text) - lens[0];

        /* Search and find the other delimited segments.
         * Loop variant: cp points to the last delimiter found,
         * or cp is NULL (exit condition)
         * Loop invariant: instr points to the begin of the last delimited
         * segment, left is the number of characters left in the string.
         */
        while (cp)
        {
            /* Skip the delimiter found last and search the next */
            cp += 2;
            instr = cp;
            left -= 2;

            do
            {
                cp = memchr(cp, TC_FIRST_CHAR, left);
                if (cp) {
                    if (cp[1] == TC_SECOND_CHAR)
                        break;
                    cp++;
                }
            } while (cp);

            if (cp)
            {
                /* Another delimiter found: put it into the parts array.
                 */
                parts[num] = instr;
                lens[num] = cp - instr;
                left -= lens[num];
                num++;
                if (num % NSTRSEGS == 0)
                {
                    parts = RESIZE(parts, num + NSTRSEGS, char * );
                    lens = RESIZE(lens, num + NSTRSEGS, p_int );
                }
            }
        }

        /* Trailing part, or maybe just a delimiter */
        if (*instr)
        {
            parts[num] = instr;
            lens[num] = left;
            num++;
        }
    } /* if (delimiter found or not) */

    /* Do the the keyword replacement and calculate the lengths.
     * The lengths are collected in the lens[] array to save the
     * need for repeated strlens().
     */
    col = 0;
    start = -1;
    space = 0;
    maybe_at_end = MY_FALSE;
    j = 0; /* gathers the total length of the final string */
    j_extra = 0; /* gathers the extra length needed during fmt'ing */
    for (i = 0; i < num; i++)
    {
        string_t * str;
        svalue_t * mdata;

        /* If parts[i] is a valid colour key, there must exist a shared
         * string for it. Is that the case, look up parts[i] in the
         * mapping and set the result in mdata, otherwise save that effort.
         * However, if i is even, parts[i] is by definition not a colour
         * key.
         */
        mdata = NULL;
        if (i % 2 && !no_keys)
        {
            if (lens[i] == 0) /* Empty key - already handled */
                str = NULL;
            else
                str = find_tabled_str_n(parts[i], lens[i]);
            if (str != NULL)
            {
                svalue_t mkey;

                put_string(&mkey, str);
                 /* The only use of mkey is to index a mapping - an
                  * operation which will not decrement the refcount
                  * for <str>. This makes it safe to not count the
                  * ref by mkey here, and saves a bit time.
                  */

                /* now look for mapping data */
                mdata = get_map_value(map, &mkey);
            }
        }
        else if (!(i % 2) && !no_keys
              && i < num -1 && lens[i+1] == 0)
        {
            /* Special case: the following colour key is the empty "%^%^".
             * We interpret it as literal "%^" and add it to this part.
             * Both part[i] and part[i+1] will end with the same char.
             */
            lens[i] += 2;
        }

        /* If mdata found a string, use it instead of the old parts[i].
         * Note its length, making it negative where necessary.
         */
        if ( mdata && mdata->type == T_STRING )
        {
            parts[i] = get_txt(mdata->u.str);
            lens[i] = (p_int)mstrsize(mdata->u.str);
            if (wrap)
                lens[i] = -lens[i];
        }

        if (lens[i] > 0)
        {
            /* This part must be considered for wrapping/indentation */
            p_int len;

            len = lens[i];
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
                        {
                            j--;
                            j_extra++;
                        }

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
            j += -lens[i];
            if (j > MAX_STRING_LENGTH)
            {
                /* Max length exceeded: shrink the working length
                 * to something usable. All following fragments
                 * will be shrunk to length 0.
                 */
                lens[i] = -(-(lens[i]) - (j - MAX_STRING_LENGTH));
                j = MAX_STRING_LENGTH;
            }
        } /* if (lens[i] > 0) */
    } /* for (i = 0..num) */


    /* Now we have the final string in parts and length in j.
     * let's compose the result, wrapping it where necessary.
     */
    memsafe(deststr = alloc_mstring((size_t)j), (size_t)j, "result string");

    cp = get_txt(deststr); /* destination pointer */

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

        tmpmem_size = (size_t)j+j_extra+1;
          /* Actually, the allocated '+j_extra' size is never used, but
           * it makes the sanity check below simpler.
           */
        xallocate(tmpmem, tmpmem_size, "temporary string");

        col = 0;
        start = -1;
        space = 0;
        pt = tmpmem;

        /* Loop over all parts */
        for (i = 0; i < num; i++)
        {
            int kind;            /* The kind of a line break */
            int len;             /* Actual length of the line */
            p_int l = lens[i];   /* Length of current part */
            char *p = parts[i];  /* Current part */

            if (pt - tmpmem + ((l < 0) ? -l : l) >= tmpmem_size)
            {
                error("Partial string too long (%ld+%ld >= %ld).\n"
                     , (long)(pt - tmpmem), (long)((l < 0) ? -l : l)
                     , (long)tmpmem_size);
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

                /* If we are indenting, check if we have to add the
                 * indentation space.
                 * Note: if kind == 2, it's the current character which 
                 *   will go onto the next line, otherwise it's the next
                 *   character will. The difference is important in the
                 *   call to at_end().
                 */
                if (indent != 0
                 && (   len > space_garbage
                     || !at_end(i, num, (kind == 2) ? k-1 : k, lens))
                   )
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

        /* Append the last fragment from the tmpmem to the result */
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

    if ( lens )
      xfree(lens);
    if ( parts )
      xfree(parts);
    if (savestr)
      free_mstring(savestr);

    /* now we have what we want */
#ifdef DEBUG
    if (cp - get_txt(deststr) != j) {
      fatal("Length miscalculated in terminal_colour()\n"
            "    Expected: %i Was: %ld\n"
            "    In string: %.*s\n"
            "    Out string: %.*s\n"
            "    Indent: %i Wrap: %i\n"
           , j, (long)(cp - get_txt(deststr))
           , (int)mstrsize(text), get_txt(text)
           , (int)mstrsize(deststr), get_txt(deststr)
           , indent, wrap);
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
svalue_t *
f_terminal_colour (svalue_t *sp, int num_arg)

/* EFUN terminal_colour()
 *
 *   varargs string terminal_colour( string str, mapping map,
 *                                   int wrap, int indent )
 *
 * Expands all colour-defines from the input-string and replaces them by the
 * apropriate values found for the color-key inside the given mapping. The
 * mapping has the format "KEY" : "value", non-string contents are ignored.
 *
 * If <map> is given as 0, no keyword detection or replacement will be
 * performed and the efun acts just as a text wrapper and indenter (assuming
 * that <wrap> and <indent> are given).
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
    int        indent = 0;
    int        wrap = 0;
    string_t * str;
    mapping_t * map = NULL;

    if ( num_arg >= 3 )
    {
        if ( num_arg == 4 )
        {
            indent = (sp--)->u.number;
            if (indent < 0)
            {
                error("terminal_colour() requires an indent >= 0.\n");
                /* NOTREACHED */
                return sp;
            }
        }
        wrap = (sp--)->u.number;
        if (wrap < 0)
        {
            error("terminal_colour() requires a wrap >= 0.\n");
            /* NOTREACHED */
            return sp;
        }
    }

    if (sp->type == T_MAPPING)
    {
        map = sp->u.map;
        if (map->num_values < 1)

        {
            error("terminal_colour() requires a mapping with values.\n");
            /* NOTREACHED */
            return sp;
        }
    }
    else
        map = NULL;

    inter_sp = sp;

    str = e_terminal_colour(sp[-1].u.str, map, indent, wrap);

    free_svalue(sp--);
    free_svalue(sp);
    put_string(sp, str);

    return sp;
} /* f_terminal_colour() */

#ifdef F_PROCESS_STRING
/*-------------------------------------------------------------------------*/
static string_t *
process_value (const char *str, Bool original)

/* Helper function for process_string(): take a function call in <str>
 * in the form "function[:objectname]{|arg}" and try to call it.
 * If the function exists and returns a string, the result is an uncounted
 * pointer to the string.
 * If the function can't be called, or does not return a string, the
 * result is NULL.
 */

{
    svalue_t *ret;     /* Return value from the function call */
    char     *func;    /* Copy of the <str> string for local modifications */
    string_t *func2;   /* Shared string with the function name from <func> */
    char     *obj;     /* NULL or points to the object part in <func> */
    char     *arg;     /* NULL or points to the first arg in <func> */
    char     *narg;    /* Next argument while pushing them */
    int       numargs; /* Number of arguments to the call */
    object_t *ob;

    /* Simple check if the argument is valid */
    if (strlen(str) < 1 || !isalpha((unsigned)(str[0])))
        return NULL;

    /* If necessary, copy the argument so that we can separate the various
     * parts with \0 characters.
     */
    if (original)
    {
        func = alloca(strlen(str)+1);
        if (!func)
            error("Out of stack memory (%lu bytes)\n"
                 , (unsigned long)(strlen(str)+1));
        strcpy(func, str);
    }
    else
    {
        func = (char *)str;
    }

    /* Find the object and the argument part */
    arg = strchr(func,'|'); if (arg) { *arg='\0'; arg++; }
    obj = strchr(func,':'); if (obj) { *obj='\0'; obj++; }

    /* Check if the function exists at all. apply() will be delighted
     * over the shared string anyway.
     */
    if ( NULL == (func2 = find_tabled_str(func)) )
    {
        return NULL;
    }


    /* Get the object */
    if (!obj)
        ob = current_object;
    else
    {
        string_t *objstr;

        memsafe(objstr = new_mstring(obj), strlen(obj), "object name");
        ob = find_object(objstr);
        free_mstring(objstr);
    }

    if (!ob)
    {
        return NULL;
    }

    /* Push all arguments as strings to the stack
     */
    for (numargs = 0; arg; arg = narg)
    {
        narg = strchr(arg,'|');
        if (narg)
            *narg = '\0';
        push_c_string(inter_sp, arg);
        numargs++;
        if (narg)
        {
            *narg = '|';
            narg++;
        }
    }

    /* Apply the function and see if adequate answer is returned.
     */
    ret = apply(func2, ob, numargs);

    if (ret && ret->type == T_STRING)
        return ret->u.str;
        /* The svalue is stored statically in apply_return_value */

    return NULL;
} /* process_value() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_process_string(svalue_t *sp)

/* EFUN process_string()
 *
 *     string process_string(string str)
 *
 * Searches string str for occurences of a "value by function
 * call", which is @@ followed by an implicit function call. See
 * "value_by_function_call" in the principles section.
 *
 * The value should contain a string like this:
 * @@function[:filename][|arg|arg]@@
 *
 * function must return a string or else the string which should be
 * processed will be returned unchanged.
 *
 * Note that process_string() does not recurse over returned
 * replacement values. If a function returns another function
 * description, that description will not be replaced.
 *
 * Both filename and args are optional.
 *
 * TODO: OSB has a bugfix for this function to handle spaces in
 * TODO:: arguments.
 */

{
    vector_t   *vec;           /* Arg string exploded by '@@' */
    object_t   *old_cur;       /* Old current object */
    wiz_list_t *old_eff_user;  /* Old euid */
    int         il;            /* Index in vec */
    Bool        changed;       /* True if there was a replacement */
    Bool        ch_last;       /* True if the last vec-entry was replaced */
    string_t *buf;             /* Result string(s) */
    string_t *str;             /* The argument string */

    str = sp->u.str;
    
    if (NULL == strchr(get_txt(str), '@'))
        return sp;  /* Nothing to do */

    old_eff_user = NULL;
    old_cur = current_object;

    if (!current_object)
    {
        /* This means we are called from notify_ in comm1
         * We must temporary set eff_user to backbone uid for
         * security reasons.
         */

        svalue_t *ret;

        current_object = command_giver;
        ret = apply_master_ob(STR_GET_BB_UID,0);
        if (!ret)
            return sp;

        if (ret->type != T_STRING
         && (strict_euids || ret->type != T_NUMBER || ret->u.number))
            return sp;

        if (current_object->eff_user)
        {
            old_eff_user = current_object->eff_user;
            if (ret->type == T_STRING)
                current_object->eff_user = add_name(ret->u.str);
            else
                current_object->eff_user = NULL;
        }
    }

    /* Explode the argument by the '@@' */
    vec = explode_string(str, STR_ATAT);
    if (!vec)
        return sp;
    push_array(inter_sp, vec); /* automatic free in case of errors */

    for ( ch_last = MY_FALSE, changed = MY_FALSE, il = 1
        ; (size_t)il < VEC_SIZE(vec)
        ; il++)
    {
        string_t *p0, *p2;
        char *p1;

        p0 = vec->item[il].u.str;

        /* Try to interpret the entry as function call.
         * If that succeeds, hold the result (freshly allocated) in p2.
         */
        p1 = strchr(get_txt(p0), ' ');
        if (!p1)
        {
            /* No space, the whole entry might be a function call */
            p2 = process_value(get_txt(p0), MY_TRUE);
            if (p2)
            {
                /* Yup, it is: copy the result */
                p2 = ref_mstring(p2);
                ch_last = MY_TRUE;
            }
        }
        else
        {
            /* There is a space: just interpret the characters before
             * as possible function call.
             */
            size_t len;
            char * tmpbuf;

            len = (size_t)(p1 - get_txt(p0));
            tmpbuf = xalloc(len + 1);
            strncpy(tmpbuf, get_txt(p0), len);
            tmpbuf[len] = '\0';
            p2 = process_value(tmpbuf, MY_FALSE);
            if (p2)
            {
                /* We got a result: join it with the remains after the
                 * space and put it into p2.
                 */
                string_t * tmp;

                len = mstrsize(p2);
                memsafe(tmp = alloc_mstring(len+strlen(p1)), len+strlen(p1)
                       , "intermediate result string");
                memcpy(get_txt(tmp), get_txt(p2), len);
                strcpy(get_txt(tmp)+len, p1);
                p2 = tmp;
            }
            xfree(tmpbuf);
        }
        
        if (!p2)
        {
            /* No replacement by function call */
            if (!ch_last)
            {
                /* ...but we have to recreate the '@@' from the original */
                memsafe(p2 = alloc_mstring(2+mstrsize(p0)), 2+mstrsize(p0)
                       , "intermediate result string");
                memcpy(get_txt(p2), "@@", 2);
                memcpy(get_txt(p2)+2, get_txt(p0), mstrsize(p0));
            }
            else
            {
                ch_last = MY_FALSE;
            }
        }
        else
        {
            /* Mark that we have a true replacement */
            changed = MY_TRUE;
        }

        /* If we have a replacement string, put it into place. */
        if (p2)
        {
            free_mstring(p0);
            vec->item[il].u.str = p2;
        }
    } /* for() */

    /* If there were changes, implode the vector again */
    if (changed)
        buf = implode_string(vec, STR_EMPTY);
    else
        buf = NULL;

    /* Clean up */
    inter_sp--;
    free_array(vec);

    if (old_eff_user)
    {
        current_object->eff_user = old_eff_user;
    }

    current_object = old_cur;

    /* Return the result */
    if (buf)
    {
        free_string_svalue(sp);
        put_string(sp, buf);
    }

    return sp;
}

#endif /* F_PROCESS_STRING */

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
    svalue_t *arg_start;    /* first argument for the current %-spec */
    svalue_t *arg_current;  /* current argument to consider */
    svalue_t *arg_end;      /* the last argument */
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
    static svalue_t tmp_svalue = { T_NUMBER };

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
e_sscanf (int num_arg, svalue_t *sp)

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
    svalue_t sv_tmp;
    svalue_t *arg0;        /* The first argument */
    struct sscanf_flags flags;  /* local copy of info.flags */
    struct sscanf_info info;    /* scan information packet */

    inter_sp = sp; /* we can have an error() deep inside */
    arg0 = sp - num_arg + 1;

    /* First get the string to be parsed.
     */
    in_string = get_txt(arg0[0].u.str);

    /* Now get the format description.
     */
    fmt = get_txt(arg0[1].u.str);

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
            svalue_t *arg;

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
                    string_t *matchstr;
                    memsafe(matchstr = new_n_mstring(in_string, (size_t)num)
                           , num, "matchstring");
                    put_string(&sv_tmp, matchstr);
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
} /* f_sscanf() */


/*=========================================================================*/
/*                              OBJECTS                                    */

/*-------------------------------------------------------------------------*/
svalue_t *
f_clones (svalue_t *sp, int num_arg)

/* EFUN clones()
 *
 *   object* clones ()
 *   object* clones (int what)
 *   object* clones (string|object obj [, int what])
 *
 * The efuns returns an array with all clones of a certain blueprint.
 *
 * If <obj> is given, all clones of the blueprint of <obj> (which
 * may be <obj> itself) are returned, otherwise all clones of the
 * current object resp. of the current object's blueprint. If <obj>
 * is given as string, it must name an existing object.
 *
 * <what> selects how to treat clones made from earlier versions
 * of the blueprint:
 *   == 0: (default) return the clones of the current blueprint only.
 *   == 1: return the clones of the previous blueprints only.
 *   == 2: return all clones of the blueprint.
 */
 
{
    string_t  *name;     /* The (tabled) load-name to search */
    mp_int     mintime;  /* 0 or lowest load_time for an object to qualify */
    mp_int     maxtime;  /* 0 or highest load_time for an object to qualify */
    mp_int     load_id;  /* The load_id of the reference */
    object_t **ores;     /* Table pointing to the found objects */
    size_t     found;    /* Number of objects found */
    size_t     osize;    /* Size of ores[] */
    vector_t  *res;      /* Result vector */
    svalue_t  *svp;
    object_t  *ob;

    mintime = 0;
    maxtime = 0;
    load_id = 0;

    /* Evaluate the arguments */
    {
        int what;
        object_t * reference;

        /* Defaults */
        reference = current_object;
        what = 0;

        if (num_arg == 1)
        {
            if (sp->type == T_OBJECT)
                reference = sp->u.ob;
            else if (sp->type == T_STRING) {
                reference = get_object(sp->u.str);
                if (!reference) {
                    error("Object not found: %s\n", get_txt(sp->u.str));
                    /* NOTREACHED */
                    return sp;
                }
            }
            else /* it's a number */
            {
                what = sp->u.number;
                if (what < 0 || what > 2) {
                    error("Bad num arg 1 to clones(): got %d, expected 0..2\n"
                         , what);
                    /* NOTREACHED */
                    return sp;
                }
            }
        }
        else if (num_arg == 2)
        {
            what = sp->u.number;
            if (what < 0 || what > 2)
            {
                error("Bad num arg 2 to clones(): got %d, expected 0..2\n"
                     , what);
                /* NOTREACHED */
                return sp;
            }

            free_svalue(sp--); inter_sp = sp;
            
            if (sp->type == T_OBJECT)
                reference = sp->u.ob;
            else /* it's a string */
            {
                reference = get_object(sp->u.str);
                if (!reference)
                {
                    error("Object not found: %s\n", get_txt(sp->u.str));
                    /* NOTREACHED */
                    return sp;
                }
            }
        }

        name = reference->load_name;

        /* If we received a clone as reference, we have
         * to find the blueprint.
         */
        if (reference->flags & O_CLONE)
            reference = get_object(reference->load_name);

        /* Encode the 'what' parameter into the two
         * time bounds: during the search we just have to
         * compare the load_times against these bounds.
         */
        if (!reference)
        {
            if (!what)
            {
                /* We know that there is nothing to find,
                 * therefore return immediately.
                 */
                res = allocate_array(0);
                if (!num_arg)
                    sp++;
                else
                    free_svalue(sp);
                put_array(sp, res);
                return sp;
            }

            /* otherwise we can return all we find */
        }
        else if (!what)
        {
            /* Just the new objects */
            mintime = reference->load_time;
            load_id = reference->load_id;
        }
        else if (what == 1)
        {
            /* Just the old objects */
            maxtime = reference->load_time;
            load_id = reference->load_id;
        }

    } /* evaluation of arguments */

    /* Prepare the table with the object pointers */
    osize = 256;
    found = 0;
    xallocate(ores, sizeof(*ores) * osize, "initial object table");

    /* Loop through the object list */
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        if ((ob->flags & (O_DESTRUCTED|O_CLONE)) == O_CLONE
         && ob->load_name == name
         && (!mintime || ob->load_time > mintime
                      || (ob->load_time == mintime && ob->load_id >= load_id)
            )
         && (!maxtime || ob->load_time < maxtime
                      || (ob->load_time == maxtime && ob->load_id < load_id)
            )
           )
        {
            /* Got one */
            if (found == osize)
            {
                /* Need to extend the array */
                osize += 256;
                ores = rexalloc(ores, sizeof(*ores) * osize);
                if (!ores)
                {
                    error("(clones) Out of memory (%lu bytes) for increased "
                          "object table.\n"
                         , (unsigned long) sizeof(*ores)*osize);
                    /* NOTREACHED */
                    return sp;
                }
            }
            ores[found++] = ob;
        }
    }

    /* Create the result and put it onto the stack */
    if (max_array_size && found > max_array_size)
    {
        xfree(ores);
        error("Illegal array size: %ld\n", (long)found);
        /* NOTREACHED */
        return sp;
    }
    res = allocate_uninit_array(found);
    if (!res)
    {
        xfree(ores);
        error("(clones) Out of memory: array[%lu] for result.\n"
             ,(unsigned long)  found);
        /* NOTREACHED */
        return sp;
    }

    osize = found;
    for (found = 0, svp = res->item; found < osize; found++, svp++)
    {
        put_ref_object(svp, ores[found], "clones");
    }

    if (!num_arg)
        sp++;
    else
        free_svalue(sp);
    put_array(sp, res);

    xfree(ores);
    
    return sp;
} /* f_clones() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_object_info (svalue_t *sp, int num_args)

/* EFUN object_info()
 *
 *    mixed * object_info(object o, int type)
 *    mixed * object_info(object o, int type, int which)
 *
 * Return an array with information about the object <o>. The
 * type of information returned is determined by <type>.
 *
 * If <which> is specified, the function does not return the full array, but
 * just the single value from index <which>.
 */

{
    vector_t *v;
    object_t *o, *o2;
    program_t *prog;
    svalue_t *svp, *argp;
    mp_int v0, v1, v2;
    int flags, pos, value;
    svalue_t result;

    /* Get the arguments from the stack */
    argp = sp - num_args + 1;
    if (num_args == 3)
    {
        value = argp[2].u.number;
        transfer_svalue_no_free(&result, &const0);
    }
    else
        value = -1;
  
    o = argp->u.ob;

    /* Depending on the <type> argument, determine the
     * data to return.
     */
    switch(argp[1].u.number)
    {
#define PREP(max) \
    if (num_args == 2) { \
        v = allocate_array(max); \
        if (!v) \
            error("Out of memory: array[%d] for result.\n" \
                 , max); \
        svp = v->item; \
    } else { \
        v = NULL; \
        if (value < 0 || value >= max) \
            error("Illegal index for object_info(): %d, " \
                  "expected 0..%d\n", value, max-1); \
        svp = &result; \
    }

#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code; \
    else {}
    
#define ST_DOUBLE(which,code) \
    if (value == -1) { \
        svp[which].type = T_FLOAT; \
        STORE_DOUBLE(svp+which, code); \
    } else if (value == which) { \
        svp->type = T_FLOAT; \
        STORE_DOUBLE(svp, code); \
    } else {}
            
#define ST_STRING(which,code) \
    if (value == -1) { \
        put_ref_string(svp+which, code); \
    } else if (value == which) { \
        put_ref_string(svp, code); \
    } else {}
            
#define ST_NOREF_STRING(which,code) \
    if (value == -1) { \
        put_string(svp+which, code); \
    } else if (value == which) { \
        put_string(svp, code); \
    } else {}
            
#define ST_OBJECT(which,code,tag) \
    if (value == -1) { \
        put_ref_object(svp+which, code, tag); \
    } else if (value == which) { \
        put_ref_object(svp, code, tag); \
    } else {}
            
    default:
        error("Illegal value %ld for object_info().\n", sp->u.number);
        /* NOTREACHED */
        return sp;

    /* --- The basic information from the object structure */
    case OINFO_BASIC:
        PREP(OIB_MAX);

        flags = o->flags;

        ST_NUMBER(OIB_HEART_BEAT,        (flags & O_HEART_BEAT) ? 1 : 0);
#ifdef O_IS_WIZARD
        ST_NUMBER(OIB_IS_WIZARD,         (flags & O_IS_WIZARD) ? 1 : 0);
#else
        ST_NUMBER(OIB_IS_WIZARD,         0);
#endif
        ST_NUMBER(OIB_ENABLE_COMMANDS,   (flags & O_ENABLE_COMMANDS) ? 1 : 0);
        ST_NUMBER(OIB_CLONE,             (flags & O_CLONE) ? 1 : 0);
        ST_NUMBER(OIB_DESTRUCTED,        (flags & O_DESTRUCTED) ? 1 : 0);
        ST_NUMBER(OIB_SWAPPED,           (flags & O_SWAPPED) ? 1 : 0);
        ST_NUMBER(OIB_ONCE_INTERACTIVE,  (flags & O_ONCE_INTERACTIVE) ? 1 : 0);
        ST_NUMBER(OIB_RESET_STATE,       (flags & O_RESET_STATE) ? 1 : 0);
        ST_NUMBER(OIB_WILL_CLEAN_UP,     (flags & O_WILL_CLEAN_UP) ? 1 : 0);
        ST_NUMBER(OIB_LAMBDA_REFERENCED, (flags & O_LAMBDA_REFERENCED) ? 1 : 0);
        ST_NUMBER(OIB_SHADOW,            (flags & O_SHADOW) ? 1 : 0);
        ST_NUMBER(OIB_REPLACED,          (flags & O_REPLACED) ? 1 : 0);
#ifdef F_SET_LIGHT
        ST_NUMBER(OIB_TOTAL_LIGHT,       o->total_light);
#else
        ST_NUMBER(OIB_TOTAL_LIGHT,       0);
#endif
        ST_NUMBER(OIB_NEXT_RESET,        o->time_reset);
        ST_NUMBER(OIB_TIME_OF_REF,       o->time_of_ref);
        ST_NUMBER(OIB_REF,               o->ref);
        ST_NUMBER(OIB_GIGATICKS,         (p_int)o->gigaticks);
        ST_NUMBER(OIB_TICKS,             (p_int)o->ticks);
        ST_NUMBER(OIB_SWAP_NUM,          O_SWAP_NUM(o));
        ST_NUMBER(OIB_PROG_SWAPPED,      O_PROG_SWAPPED(o) ? 1 : 0);
        ST_NUMBER(OIB_VAR_SWAPPED,       O_VAR_SWAPPED(o) ? 1 : 0);

        if (compat_mode)
        {
            ST_STRING(OIB_NAME, o->name);
        }
        else
        {
            ST_NOREF_STRING(OIB_NAME, add_slash(o->name));
        }
  
        ST_STRING(OIB_LOAD_NAME, o->load_name);

        o2 = o->next_all;
        if (o2)
        {
            ST_OBJECT(OIB_NEXT_ALL, o2, "object_info(0)");
        } /* else the element was already allocated as 0 */

        o2 = o->prev_all;
        if (o2)
        {
            ST_OBJECT(OIB_PREV_ALL, o2, "object_info(0)");
        } /* else the element was already allocated as 0 */

        break;

    /* --- Position in the object list */
    case OINFO_POSITION:
        PREP(OIP_MAX);

        o2 = o->next_all;
        if (o2)
        {
            ST_OBJECT(OIP_NEXT, o2, "object_info(1) next");
        } /* else the element was already allocated as 0 */

        o2 = o->prev_all;
        if (o2)
        {
            ST_OBJECT(OIP_PREV, o2, "object_info(1) next");
        } /* else the element was already allocated as 0 */
  
        if (value == -1 || value == OIP_POS)
        {
            /* Find the non-destructed predecessor of the object */
            if (obj_list == o)
            {
                pos = 0;
            }
            else
            for (o2 = obj_list, pos = 0; o2; o2 = o2->next_all)
            {
                pos++;
                if (o2->next_all == o)
                    break;
            }

            if (!o2) /* Not found in the list (this shouldn't happen) */
                pos = -1;

            ST_NUMBER(OIP_POS, pos);
        }

        break;

    /* --- Memory and program information */
    case OINFO_MEMORY:
        PREP(OIM_MAX);

        if ((o->flags & O_SWAPPED) && load_ob_from_swap(o) < 0)
            error("Out of memory: unswap object '%s'.\n", get_txt(o->name));

        prog = o->prog;

        ST_NUMBER(OIM_REF, prog->ref);
  
        ST_STRING(OIM_NAME, prog->name);
  
        ST_NUMBER(OIM_PROG_SIZE, (long)(PROGRAM_END(*prog) - prog->program));

          /* Program size */
        ST_NUMBER(OIM_NUM_FUNCTIONS, prog->num_functions);
        ST_NUMBER(OIM_SIZE_FUNCTIONS
                 , (p_int)(prog->num_functions * sizeof(uint32)
                    + prog->num_function_names * sizeof(short)));
          /* Number of function names and the memory usage */
        ST_NUMBER(OIM_NUM_VARIABLES, prog->num_variables);
        ST_NUMBER(OIM_SIZE_VARIABLES
                 , (p_int)(prog->num_variables * sizeof(variable_t)));
          /* Number of variables and the memory usage */
        v1 = program_string_size(prog, &v0, &v2);
        ST_NUMBER(OIM_NUM_STRINGS, prog->num_strings);
        ST_NUMBER(OIM_SIZE_STRINGS, (p_int)v0);
        ST_NUMBER(OIM_SIZE_STRINGS_DATA, v1);
        ST_NUMBER(OIM_SIZE_STRINGS_TOTAL, v2);
          /* Number of strings and the memory usage */
        {
            int i = prog->num_inherited;
            int cnt = 0;
            inherit_t *inheritp;

            for (inheritp = prog->inherit; i--; inheritp++)
            {
                if (inheritp->inherit_type == INHERIT_TYPE_NORMAL)
                    cnt++;
            }
            ST_NUMBER(OIM_NUM_INHERITED, cnt);
        }
        ST_NUMBER(OIM_SIZE_INHERITED
                 , (p_int)(prog->num_inherited * sizeof(inherit_t)));
          /* Number of inherites and the memory usage */
        ST_NUMBER(OIM_TOTAL_SIZE, prog->total_size);

        {
            mp_int totalsize;

            ST_NUMBER(OIM_DATA_SIZE, data_size(o, &totalsize));
            ST_NUMBER(OIM_TOTAL_DATA_SIZE, totalsize);
        }

        ST_NUMBER(OIM_NO_INHERIT, (prog->flags & P_NO_INHERIT) ? 1 : 0);
        ST_NUMBER(OIM_NO_CLONE, (prog->flags & P_NO_CLONE) ? 1 : 0);
        ST_NUMBER(OIM_NO_SHADOW, (prog->flags & P_NO_SHADOW) ? 1 : 0);
        break;

#undef PREP
#undef ST_NUMBER
#undef ST_DOUBLE
#undef ST_STRING
#undef ST_RSTRING
#undef ST_OBJECT
    }

    free_svalue(sp);
    sp--;
    free_svalue(sp);
    if (num_args == 3)
    {
        sp--;
        free_svalue(sp);
    }

    /* Assign the result */
    if (num_args == 2)
        put_array(sp, v);
    else
        transfer_svalue_no_free(sp, &result);

    return sp;
} /* f_object_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_present_clone (svalue_t *sp)

/* EFUN present_clone()
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
    string_t * name; /* the shared loadname to look for */
    object_t *obj;   /* the object under scrutiny */

    /* Test and get the arguments from the stack */
    if (sp[-1].type == T_STRING)
    {
        size_t len;
        long i;
        char * end;
        char * sane_name;
        char * name0;  /* Intermediate name */

        name0 = get_txt(sp[-1].u.str);

        /* Normalize the given string and check if it is
         * in the shared string table. If not, we know that
         * there is no blueprint with that name
         */

        /* First, slash of a trailing '#<num>' */

        len = mstrsize((sp-1)->u.str);
        i = (long)len;
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
                    strncpy(name0, get_txt(sp[-1].u.str), (size_t)i);
                    name0[i] = '\0';
                }

                break; /* in any case */
            }
        }

        /* Now make the name sane */
        sane_name = (char *)make_name_sane(name0, !compat_mode);

        if (sane_name)
            name = find_tabled_str(sane_name);
        else
            name = find_tabled_str(name0);

    }
    else if (sp[-1].type == T_OBJECT)
    {
        name = sp[-1].u.ob->load_name;
    }
    else
        efun_gen_arg_error(1, sp[-1].type, sp);

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
#ifdef F_SET_IS_WIZARD

svalue_t *
f_set_is_wizard (svalue_t *sp)

/* EFUN set_is_wizard()
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

    flagp = &sp[-1].u.ob->flags;
    i = (*flagp & O_IS_WIZARD) != 0;

    switch (sp->u.number)
    {
        default:
            error("Bad arg to set_is_wizard(): got %ld, expected -1..1\n"
                 , sp->u.number);
            /* NOTREACHED */
        case  0: *flagp &= ~O_IS_WIZARD; is_wizard_used = MY_TRUE; break;
        case  1: *flagp |=  O_IS_WIZARD; is_wizard_used = MY_TRUE; break;
        case -1: break; /* only report status */
    }
    sp--;
    free_object_svalue(sp);
    put_number(sp, i);
    return sp;
} /* f_set_is_wizard() */

#endif /* F_SET_IS_WIZARD */

/*-------------------------------------------------------------------------*/
static svalue_t *
x_min_max (svalue_t *sp, int num_arg, Bool bMax)

/* Implementation of VEFUNs max() and min().
 * <bMax> is true if the maximum is to be returned, false for the minimum.
 */

{
    char * fname = bMax ? "max" : "min";
    svalue_t *argp = sp-num_arg+1;
    svalue_t *valuep = argp;
    int left = num_arg;
    Bool gotArray = MY_FALSE;
    svalue_t *result = NULL;


    if (argp->type == T_POINTER)
    {
        if (num_arg > 1)
        {
           error("Bad arguments to %s: only one array accepted.\n", fname); 
           /* NOTREACHED */
        }
        valuep = argp->u.vec->item;
        left = (int)VEC_SIZE(argp->u.vec);
        gotArray = MY_TRUE;
        if (left < 1)
        {
           error("Bad argument 1 to %s: array must not be empty.\n", fname); 
           /* NOTREACHED */
        }
    }

    if (valuep->type == T_STRING)
    {
        result = valuep;

        for (valuep++, left--; left > 0; valuep++, left--)
        {
            int cmp;

            if (valuep->type != T_STRING)
            {
                if (gotArray)
                    error("Bad argument to %s(): array[%d] is a '%s', "
                          "expected 'string'.\n"
                         , fname, (int)VEC_SIZE(argp->u.vec) - left + 1
                         , typename(valuep->type));
                else
                    vefun_arg_error(num_arg - left + 1, T_STRING, valuep->type, sp);
                /* NOTREACHED */
            }
            
            cmp = mstrcmp(valuep->u.str, result->u.str);
            if (bMax ? (cmp > 0) : (cmp < 0))
                result = valuep;
        }
    }
    else if (valuep->type == T_NUMBER || valuep->type == T_FLOAT)
    {
        result = valuep;

        for (valuep++, left--; left > 0; valuep++, left--)
        {
            if (valuep->type != T_FLOAT && valuep->type != T_NUMBER)
            {
                if (gotArray)
                    error("Bad argument to %s(): array[%d] is a '%s', "
                          "expected 'int' or 'float'.\n"
                         , fname, (int)VEC_SIZE(argp->u.vec) - left + 1
                         , typename(valuep->type));
                else
                    vefun_exp_arg_error(num_arg - left + 1, TF_NUMBER|TF_FLOAT, valuep->type, sp);
                /* NOTREACHED */
            }

            if (valuep->type == T_NUMBER && result->type == T_NUMBER)
            {
                if (bMax ? (valuep->u.number > result->u.number)
                         : (valuep->u.number < result->u.number))
                    result = valuep;
            }
            else
            {
                double v, r;

                if (valuep->type == T_FLOAT)
                    v = READ_DOUBLE(valuep);
                else
                    v = (double)(valuep->u.number);

                if (result->type == T_FLOAT)
                    r = READ_DOUBLE(result);
                else
                    r = (double)(result->u.number);

                if (bMax ? (v > r)
                         : (v < r))
                    result = valuep;
            }
        } /* for (values) */
    }
    else
    {
        if (gotArray)
            error("Bad argument to %s(): array[0] is a '%s', "
                  "expected 'string', 'int' or 'float'.\n"
                 , fname, typename(valuep->type));
        else
            vefun_exp_arg_error(1, TF_STRING|TF_NUMBER|TF_FLOAT, valuep->type, sp);
        /* NOTREACHED */
    }

    /* Assign the result.
     * We need to make a local copy, otherwise we might lose it in the pop.
     */
    {
        svalue_t resvalue;

        assign_svalue_no_free(&resvalue, result);
        sp = pop_n_elems(num_arg, sp) + 1;
        transfer_svalue_no_free(sp, &resvalue);
    }

    return sp;
} /* x_min_max() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_max (svalue_t *sp, int num_arg)

/* VEFUN max()
 *
 *   string    max (string arg, ...)
 *   string    max (string * arg_array)
 *
 *   int|float max (int|float arg, ...)
 *   int|float max (int|float * arg_array)
 *
 * Determine the maximum value of the <arg>uments and return it.
 * If max() is called with an array (which must not be empty) as only
 * argument, it returns the maximum value of the array contents.
 */

{
    return x_min_max(sp, num_arg, MY_TRUE);
} /* f_max() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_min (svalue_t *sp, int num_arg)

/* VEFUN min()
 *
 *   string    min (string arg, ...)
 *   string    min (string * arg_array)
 *
 *   int|float min (int|float arg, ...)
 *   int|float min (int|float * arg_array)
 *
 * Determine the minimum value of the <arg>uments and return it.
 * If min() is called with an array (which must not be empty) as only
 * argument, it returns the minimum value of the array contents.
 */

{
    return x_min_max(sp, num_arg, MY_FALSE);
} /* f_min() */

/*=========================================================================*/
/*                              VALUES                                     */

/*-------------------------------------------------------------------------*/
svalue_t *
f_abs (svalue_t *sp)

/* EFUN abs()
 *
 *  int   abs (int arg)
 *  float abs (float arg)
 *
 * Returns the absolute value of the argument <arg>.
 */

{
    if (sp->type == T_NUMBER)
    {
        if (sp->u.number < 0)
            sp->u.number = - sp->u.number;
    }
    else
    {
        STORE_DOUBLE_USED
        double x;

        x = READ_DOUBLE(sp);
        if (x < 0.0)
            STORE_DOUBLE(sp, -(x));
    }

    return sp;
} /* f_abs() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_sin (svalue_t *sp)

/* EFUN sin()
 *
 *  float sin(int|float)
 *
 * Returns the sinus of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type != T_FLOAT)
        d = sin((double)(sp->u.number));
    else 
        d = sin(READ_DOUBLE(sp));
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_sin() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_asin (svalue_t *sp)

/* EFUN asin()
 *
 *  float asin(float)
 *
 * Returns the inverse sinus of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    d = READ_DOUBLE(sp);
    if (d < -1.0 || d > 1.0)
        error("Bad arg 1 for asin(): value %f out of range\n", d);
    d = asin(d);
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_asin() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_cos (svalue_t *sp)

/* EFUN cos()
 *
 *  float cos(int|float)
 *
 * Returns the cosinus of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type != T_FLOAT)
        d = cos((double)(sp->u.number));
    else 
        d = cos(READ_DOUBLE(sp));
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_cos() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_acos (svalue_t *sp)

/* EFUN acos()
 *
 *  float acos(float)
 *
 * Returns the inverse cosinus of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    d = READ_DOUBLE(sp);
    if (d < -1.0 || d > 1.0)
        error("Bad arg 1 for acos(): value %f out of range\n", d);
    d = acos(d);
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_acos() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tan (svalue_t *sp)

/* EFUN tan()
 *
 *  float tan(int|float)
 *
 * Returns the tangens of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type != T_FLOAT)
        d = tan((double)(sp->u.number));
    else 
        d = tan(READ_DOUBLE(sp));
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_tan() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_atan (svalue_t *sp)

/* EFUN atan()
 *
 *  float atan(int|float)
 *
 * Returns the inverse tangens of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type != T_FLOAT)
        d = atan((double)(sp->u.number));
    else 
        d = atan(READ_DOUBLE(sp));
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_atan() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_atan2 (svalue_t *sp)

/* EFUN atan2()
 *
 *   float atan2(int|float y, int|float x)
 *
 * Returns the inverse tangens of the argument.
 */

{
    STORE_DOUBLE_USED
    double x, y, d;

    if (sp->type != T_FLOAT)
        x = (double)(sp->u.number);
    else
        x = READ_DOUBLE(sp);
    if (sp[-1].type != T_FLOAT)
        y = (double)sp[-1].u.number;
    else
        y = READ_DOUBLE(sp-1);
    d = atan2(y, x);
    sp--;
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_atan2() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_log (svalue_t *sp)

/* EFUN log()
 *
 *   float log(int|float)
 *
 * Returns the natural logarithm of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    d = READ_DOUBLE(sp);
    if (sp->type != T_FLOAT)
        d = (double)sp->u.number;
    else
        d = READ_DOUBLE(sp);
    if (d <= 0.)
        error("Bad arg 1 for log(): value %f out of range\n", d);
    d = log(d);
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_log() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_exp (svalue_t *sp)

/* EFUN exp()
 *
 *   float exp(int|float)
 *
 * Returns the e to the power of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type != T_FLOAT)
        d = exp((double)sp->u.number);
    else 
        d = exp(READ_DOUBLE(sp));
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_exp() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_sqrt (svalue_t *sp)

/* EFUN sqrt()
 *
 *   float sqrt(int|float)
 *
 * Returns the square root of the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type != T_FLOAT)
        d = (double)sp->u.number;
    else
        d = READ_DOUBLE(sp);
    if (d < 0.)
        error("Bad arg 1 for sqrt(): value %f out of range\n", d);
    d = sqrt(d);
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_sqrt() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_ceil (svalue_t *sp)

/* EFUN ceil()
 *
 *   float ceil(float)
 *
 * Returns the smallest whole number which is still bigger
 * than the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    d = ceil(READ_DOUBLE(sp));
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_ceil() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_floor (svalue_t *sp)

/* EFUN floor()
 *
 *   float floor(float)
 *
 * Returns the biggest whole number which is not larger
 * than the argument.
 */

{
    STORE_DOUBLE_USED
    double d;

    d = floor(READ_DOUBLE(sp));
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_floor() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_pow (svalue_t *sp)

/* EFUN pow()
 *
 *   float pow(int|float x, int|float y)
 *
 * Returns x to the power of y.
 */

{
    STORE_DOUBLE_USED
    double x, y, d;

    if (sp->type != T_FLOAT)
        y = (double)(sp->u.number);
    else
        y = READ_DOUBLE(sp);
    if (sp[-1].type != T_FLOAT)
        x = (double)sp[-1].u.number;
    else
        x = READ_DOUBLE(sp-1);
    if (x == 0.0 && y < 0.0)
        error("Can't raise 0 to negative powers.\n");
    if (x < 0.0  && y != (double)((long)y))
        error("Can't raise negative number to fractional powers.\n");
    d = pow(x, y);
    sp--;
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, d);

    return sp;
} /* f_pow() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_to_int (svalue_t *sp)

/* EFUN to_int()
 *
 *   int to_int(string)
 *   int to_int(float)
 *   int to_int(int)
 *   int to_int(closure)
 *
 * Floats are truncated to integer values, strings with leadings
 * digits are converted to integers up to the first non-digit.
 * variable-closures are converted into their function index.
 * Integers are just returned.
 */

{
    int n;

    switch(sp->type)
    {
    default:
        fatal("Bad arg 1 to to_int(): type %s\n", typename(sp->type));
        break;

    case T_FLOAT:
        n = (long)READ_DOUBLE(sp);
        break;

    case T_STRING:
        n = strtol(get_txt(sp->u.str), NULL, 10);
        free_string_svalue(sp);
        break;

    case T_CLOSURE:
        if (sp->x.closure_type != CLOSURE_IDENTIFIER)
            error("Bad arg 1 to to_int(): not a lfun closure.\n");
        n = sp->u.lambda->function.index;
        free_closure(sp);
        break;

    case T_NUMBER:
        n = sp->u.number;
        break;
    }
    put_number(sp, n);

    return sp;
} /* f_to_int() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_to_float (svalue_t *sp)

/* EFUN to_float()
 *
 *   float to_float(int)
 *   float to_float(string)
 *   float to_float(float)
 *
 * Ints are expanded to floats, strings are converted up to the
 * first character that doesnt belong into a float.
 * Floats are just returned.
 */

{
    STORE_DOUBLE_USED
    double d;

    d = 0.0;
    switch(sp->type)
    {
    default:
        fatal("Bad arg 1 to to_float(): type %s\n", typename(sp->type));
        break;

    case T_NUMBER:
        d = (double)sp->u.number;
        break;

    case T_FLOAT:
        NOOP;
        break;

    case T_STRING:
        d = strtod(get_txt(sp->u.str), NULL);
        free_string_svalue(sp);
        break;
    }

    if (sp->type != T_FLOAT)
    {
        sp->type = T_FLOAT;
        STORE_DOUBLE(sp, d);
    }

    return sp;
} /* f_to_float() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_to_string (svalue_t *sp)

/* EFUN to_string()
 *
 *   string to_string(mixed)
 *
 * The argument is converted to a string. Works with int, float,
 * object, arrays (to convert an array of int back into a string),
 * symbols, strings, and closures.
 *
 * Converts variable/lfun closure to the appropriate names.
 */

  {
    char buf[1024];
    string_t *s;

    s = NULL;
    buf[sizeof(buf)-1] = '\0';
    switch(sp->type)
    {
    default:
        error("Bad arg 1 to to_string(): type %s\n", typename(sp->type));
        break;

    case T_NUMBER:
        sprintf(buf,"%ld", sp->u.number);
        if (buf[sizeof(buf)-1] != '\0')
            fatal("Buffer overflow in to_string(): "
                  "int number too big.\n");
        memsafe(s = new_mstring(buf), strlen(buf), "converted number");
        break;

    case T_FLOAT:
        sprintf(buf,"%g", READ_DOUBLE(sp));
        if (buf[sizeof(buf)-1] != '\0')
            fatal("Buffer overflow in to_string: "
                  "int number too big.\n");
        memsafe(s = new_mstring(buf), strlen(buf), "converted number");
        break;

    case T_OBJECT:
        if (!compat_mode)
            s = add_slash(sp->u.ob->name);
        else
            s = ref_mstring(sp->u.ob->name);
        if (!s)
            error("Out of memory\n");
        free_object_svalue(sp);
        break;

    case T_POINTER:
      {
        /* Arrays of ints are considered exploded strings and
         * converted back accordingly, ie. up to the first non-int.
         */

        long size;
        svalue_t *svp;
        char *d;

        size = (long)VEC_SIZE(sp->u.vec);
        svp = sp->u.vec->item;
        memsafe(s = alloc_mstring(size), size, "converted array");
        d = get_txt(s);
        for (;;)
        {
            if (!size--)
            {
                break;
            }
            if (svp->type != T_NUMBER)
            {
                if (d == get_txt(s))
                {
                    free_mstring(s);
                    s = ref_mstring(STR_EMPTY);
                }
                else
                    memsafe(s = resize_mstring(s, d-get_txt(s))
                           , d-get_txt(s), "converted array");
                break;
            }
            *d++ = (char)svp->u.number;
            svp++;
        }
        free_array(sp->u.vec);
        break;
      }

    case T_CLOSURE:
      {
        /* Convert the various types of closures into a string */

        lambda_t *l = sp->u.lambda;
        object_t *ob;
        int ix;

        switch(sp->x.closure_type)
        {

        case CLOSURE_IDENTIFIER: /* Variable Closure */
          {
            /* We need the program resident */
            if (O_PROG_SWAPPED(l->ob))
            {
                l->ob->time_of_ref = current_time;
                if (load_ob_from_swap(l->ob) < 0)
                    error("Out of memory.\n");
            }

            if (l->function.index != VANISHED_VARCLOSURE_INDEX)
            {
                /* Get the variable name */
                put_ref_string(sp
                 , l->ob->prog->variable_names[l->function.index].name
                );
            }
            else
            {
                /* Variable vanished in a replace_program() */
                put_ref_string(sp, STR_DANGLING_V_CL);
            }
            break;
          }

        case CLOSURE_LFUN: /* Lfun closure */
        case CLOSURE_ALIEN_LFUN:
          {
            program_t *prog;
            fun_hdr_p fun;
            funflag_t flags;
            string_t *function_name;
            inherit_t *inheritp;

            if (sp->x.closure_type == CLOSURE_LFUN)
            {
                ob = l->ob;
                ix = l->function.index;
            }
            else
            {
                ob = l->function.alien.ob;
                ix = l->function.alien.index;
                /* TODO: ix: After a replace_program, can this index
                 * TODO:: be negative?
                 */
            }

            /* Get the program resident */
            if (O_PROG_SWAPPED(ob)) {
                ob->time_of_ref = current_time;
                if (load_ob_from_swap(ob) < 0)
                    error("Out of memory\n");
            }

            /* Find the true definition of the function */
            prog = ob->prog;
            flags = prog->functions[ix];
            while (flags & NAME_INHERITED)
            {
                inheritp = &prog->inherit[flags & INHERIT_MASK];
                ix -= inheritp->function_index_offset;
                prog = inheritp->prog;
                flags = prog->functions[ix];
            }

            /* Copy the function name pointer (a shared string) */
            fun = prog->program + (flags & FUNSTART_MASK);
            memcpy(&function_name, FUNCTION_NAMEP(fun)
                  , sizeof function_name
            );
            put_ref_string(sp, function_name);
            break;
          }

        case CLOSURE_UNBOUND_LAMBDA: /* Unbound-Lambda Closure */
        case CLOSURE_PRELIMINARY:    /* Preliminary Lambda Closure */
          {
              string_t *rc;

              if (sp->x.closure_type == CLOSURE_PRELIMINARY)
                  sprintf(buf, "<prelim lambda 0x%p>", l);
              else
                  sprintf(buf, "<free lambda 0x%p>", l);
              memsafe(rc = new_mstring(buf), strlen(buf), "converted lambda");
              put_string(sp, rc);
              break;
          }

        case CLOSURE_LAMBDA:         /* Lambda Closure */
        case CLOSURE_BOUND_LAMBDA:   /* Bound-Lambda Closure */
          {
              string_t *rc;

              if (sp->x.closure_type == CLOSURE_BOUND_LAMBDA)
                  sprintf(buf, "<bound lambda 0x%p:", l);
              else
                  sprintf(buf, "<lambda 0x%p:", l);

              ob = l->ob;

              if (!ob)
              {
                  strcat(buf, "{null}>");
                  memsafe(rc = new_mstring(buf), strlen(buf), "converted lambda");
              }
              else
              {
                  char *tmp;
                  size_t len;

                  if (ob->flags & O_DESTRUCTED)
                      strcat(buf, "{dest}");
                  len = strlen(buf)+mstrsize(ob->name)+2;
                  memsafe(rc = alloc_mstring(len), len
                           , "string-repr of lambda closure");
                  tmp = get_txt(rc);
                  strcat(buf, "/");
                  strcpy(tmp, buf);
                  strcat(tmp, get_txt(ob->name));
                  strcat(tmp, ">");
              }

              put_string(sp, rc);
              break;
          }

        default:
            error("Bad arg 1 to to_string(): closure type %d.\n", sp->x.closure_type);
        }
        break;
      }

    case T_SYMBOL:
      {
        /* Easy: the symbol value is a string */
        sp->type = T_STRING;
        break;
      }

    case T_STRING:
        break;
    }

    if (sp->type != T_STRING)
        put_string(sp, s);

    return sp;
} /* f_to_string() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_to_array (svalue_t *sp)

/* EFUN to_array()
 *
 *   mixed *to_array(string)
 *   mixed *to_array(symbol)
 *   mixed *to_array(quotedarray)
 *   mixed *to_array(mixed *)
 *
 * Strings and symbols are converted to an int array that
 * consists of the args characters.
 * Quoted arrays are ``dequoted'', and arrays are left as they
 * are.
 */

{
    vector_t *v;
    char *s, ch;
    svalue_t *svp;
    p_int len;

    switch (sp->type)
    {
    default:
        fatal("Bad arg 1 to to_array(): type %s\n", typename(sp->type));
        break;
    case T_STRING:
    case T_SYMBOL:
        /* Split the string into an array of ints */

        len = (p_int)mstrsize(sp->u.str);
        v = allocate_uninit_array((mp_int)len);
        s = get_txt(sp->u.str);
        svp = v->item;
        while (len-- > 0) {
            ch = *s++;
            put_number(svp, ch);
            svp++;
        }
        free_string_svalue(sp);
        put_array(sp, v);
        break;
    case T_QUOTED_ARRAY:
        /* Unquote it fully */
        sp->type = T_POINTER;
        break;
    case T_POINTER:
        /* Good as it is */
        break;
    }

    return sp;
} /* f_to_array() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_to_object (svalue_t *sp)

/* EFUN to_object()
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
    object_t *o;

    switch(sp->type)
    {
    case T_NUMBER:
        if (!sp->u.number)
            return sp;
        /* FALLTHROUGH */
    default:
        error("Bad arg 1 to to_object(): type %s\n", typename(sp->type));
        break;

    case T_CLOSURE:
        n = sp->x.closure_type;
        o = sp->u.ob;
        if (n == CLOSURE_EFUN + F_UNDEF)
            o = NULL;
        else if (CLOSURE_MALLOCED(n))
        {
            if (n == CLOSURE_UNBOUND_LAMBDA)
            {
                error("Bad arg 1 to to_object(): unbound lambda.\n");
                /* NOTREACHED */
            }
            o = sp->u.lambda->ob;
        }
        if (o && o->flags & O_DESTRUCTED)
            o = NULL;
        free_closure(sp);
        break;

    case T_OBJECT:
        return sp;

    case T_STRING:
        o = find_object(sp->u.str);
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
svalue_t *
f_copy (svalue_t *sp)

/* EFUN copy()
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
        vector_t *old, *new;
        size_t size, i;

        old = sp->u.vec;
        size = VEC_SIZE(old);
        if (old->ref != 1 && old != &null_vector)
        {
            new = allocate_uninit_array((int)size);
            if (!new)
                error("(copy) Out of memory: array[%lu] for copy.\n"
                     , (unsigned long) size);
            for (i = 0; i < size; i++)
                assign_svalue_no_free(&new->item[i], &old->item[i]);
            free_array(old);
            sp->u.vec = new;
        }
        break;
      }
    case T_MAPPING:
      {
        mapping_t *old, *new;

        old = sp->u.map;
        if (old->ref != 1)
        {
            new = copy_mapping(old);
            if (!new)
                error("(copy) Out of memory: mapping[%lu] for copy.\n"
                     , MAP_SIZE(old));
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
    mapping_t * dest;         /* the mapping to copy into */
    struct pointer_table *ptable;  /* the pointer table to use */
};

/*-------------------------------------------------------------------------*/
static void
deep_copy_mapping (svalue_t *key, svalue_t *val, void *extra)

/* Called from copy_svalue() as part of the mapping walk to deeply copy
 * a mapping. <extra> is a (struct csv_info *).
 */

{
    struct csv_info *info = (struct csv_info *)extra;
    svalue_t newkey;
    svalue_t *newdata;
    int i;

    copy_svalue(&newkey, key, info->ptable);
    newdata = get_map_lvalue_unchecked(info->dest, &newkey);
    for (i = info->width; i-- > 0; newdata++, val++)
        copy_svalue(newdata, val, info->ptable);

    free_svalue(&newkey); /* no longer needed */
}

/*-------------------------------------------------------------------------*/
static void
copy_svalue (svalue_t *dest, svalue_t *src
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
        vector_t *old, *new;
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
                svalue_t * svp = &old->item[i];

                if (svp->type == T_MAPPING || svp->type == T_POINTER)
                    copy_svalue(&new->item[i], svp, ptable);
                else
                    assign_svalue_no_free(&new->item[i], svp);
            }
        }
        else /* shared array we already encountered */
        {
            assign_svalue_no_free(dest, (svalue_t *)rec->data);
        }
        break;
      }
    case T_MAPPING:
      {
        mapping_t *old, *new;
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
                error("(copy) Out of memory: new mapping[%lu, %u].\n"
                     , size, info.width);
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
            assign_svalue_no_free(dest, (svalue_t *)rec->data);
        }
        break;
      }
    } /* switch(src->type) */
} /* copy_svalue() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_deep_copy (svalue_t *sp)

/* EFUN deep_copy()
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
        vector_t *old;

        old = sp->u.vec;
        if (old != &null_vector)
        {
            svalue_t new;

            ptable = new_pointer_table();
            if (!ptable)
                error("(deep_copy) Out of memory for pointer table.\n");
            copy_svalue(&new, sp, ptable);
            transfer_svalue(sp, &new);
            free_pointer_table(ptable);
        }
        break;
      }
    case T_MAPPING:
      {
        mapping_t *old;
        svalue_t new;

        old = sp->u.map;
        ptable = new_pointer_table();
        if (!ptable)
            error("(deep_copy) Out of memory for pointer table.\n");
        copy_svalue(&new, sp, ptable);
        transfer_svalue(sp, &new);
        free_pointer_table(ptable);
        break;
      }
    }

    return sp;
} /* f_deep_copy() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_filter (svalue_t *sp, int num_arg)

/* EFUN filter()
 *
 *   mixed * filter (mixed *arg, string fun, string|object ob, mixed extra...)
 *   mixed * filter (mixed *arg, closure cl, mixed extra...)
 *   mixed * filter (mixed *arg, mapping map, mixed extra...)
 *
 *  mapping filter (mapping arg, string fun, string|object ob, mixed extra...)
 *  mapping filter (mapping arg, closure cl, mixed extra...)
 *
 * Call the function <ob>-><fun>() resp. the closure <cl> for
 * every element of the array or mapping <arg>, and return
 * a result made from those elements for which the function
 * call returns TRUE.
 *
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 */

{
    if (sp[-num_arg+1].type == T_MAPPING)
        return x_filter_mapping(sp, num_arg, MY_TRUE);
    else
        return x_filter_array(sp, num_arg);

} /* f_filter() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_get_type_info (svalue_t *sp, int num_arg)

/* EFUN get_type_info()
 *
 *   mixed get_type_info(mixed arg [, int flag])
 *
 * Returns info about the type of arg, as controlled by the flag.
 *
 * If the optional argument flag is not given, an array is
 * returned, whose first element is an integer denoting the data
 * type, as defined in <lpctypes.h>. The second entry can contain
 * additional information about arg.
 * If flag is the number 0, only the first element of that array
 * (i.e. the data type) is returned (as int). If flag is 1, the
 * second element is returned.
 * If <arg> is a closure, the <flag> setting 2 lets the efun
 * return the object the closure is bound to.
 * For every other <flag> setting, -1 is returned.
 *
 * The secondary information is:
 *   - for mappings the width, ie the number of data items per key.
 *   - for closures, symbols and quoted arrays the number of quotes.
 *   - for strings 0 for shared strings, and non-0 for others.
 *   - -1 for all other datatypes.
 *
 * TODO: The flags should be defined in an include file.
 * TODO: The array returned for closures should contain all
 * TODO:: three items.
 */

{
    mp_int i, j;
    svalue_t *argp;

    argp = sp - num_arg + 1;
    i = argp->type;

    /* Determine the second return value */
    switch(i)
    {
    default:
        j = -1;
        break;
    case T_STRING:
        j = (mstr_tabled(sp[-1].u.str)) ? 0 : 1;
        break;
    case T_MAPPING:
        j = argp->u.map->num_values;
        break;
    case T_CLOSURE:
        if (num_arg == 2 && sp->type == T_NUMBER && sp->u.number == 2)
        {
            object_t *ob;

            sp--;
            switch(sp->x.closure_type)
            {
            default:
                ob = NULL;
                break;
            case CLOSURE_LFUN:
            case CLOSURE_IDENTIFIER:
            case CLOSURE_BOUND_LAMBDA:
            case CLOSURE_LAMBDA:
                ob = sp->u.lambda->ob;
                break;
            case CLOSURE_ALIEN_LFUN:
                ob = sp->u.lambda->function.alien.ob;
                break;
            }
            free_closure(sp);
            if (!ob || ob->flags & O_DESTRUCTED)
                put_number(sp, 0);
            else
                put_ref_object(sp, ob, "get_type_info");
            return sp;
            /* NOTREACHED */
        }
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
        j = argp->x.generic;
        break;
    }

    /* Depending on flag, return the proper value */
    if (num_arg == 2)
    {
        p_int flagvalue = sp->u.number;

        free_svalue(sp--);
        free_svalue(sp);
        if (flagvalue != 1)
        {
            if (flagvalue)
                j = -1;
            else
                j = i;
        }
        put_number(sp, j);
    }
    else
    {
        vector_t *v;

        v = allocate_array(2);
        v->item[0].u.number = i;
        v->item[1].u.number = j;
        if (num_arg == 2)
            free_svalue(sp--);
        free_svalue(sp);
        put_array(sp,v);
    }

    return sp;
} /* f_get_type_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_map (svalue_t *sp, int num_arg)

/* EFUN map()
 *
 *   mixed * map(mixed *arg, string func, string|object ob, mixed extra...)
 *   mixed * map(mixed *arg, closure cl, mixed extra...)
 *
 *   mapping map(mapping arg, string func, string|object ob, mixed extra...)
 *   mapping map(mapping arg, closure cl, mixed extra...)
 *
 * Call the function <ob>-><func>() resp. the closure <cl> for
 * every element of the array or mapping <arg>, and return a result
 * made up from the returned values.
 *
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 */

{
    if (sp[-num_arg+1].type == T_MAPPING)
        return x_map_mapping(sp, num_arg, MY_TRUE);
    else
        return x_map_array(sp, num_arg);

} /* f_map() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_member (svalue_t *sp)

/* EFUN member()
 *
 *   int member(mixed *array, mixed elem)
 *   int member(mapping m, mixed key)
 *   int member(string s, int elem)
 *
 * For arrays and strings, returns the index of the second arg in
 * the first arg, or -1 if none found. For mappings it checks, if
 * key is present in mapping m and returns 1 if so, 0 if key is
 * not in m.
 */

{
    /* --- Search an array --- */

    if (sp[-1].type == T_POINTER)
    {
        vector_t *vec;
        union  u       sp_u;
        long cnt;

        vec = sp[-1].u.vec;
        cnt = (long)VEC_SIZE(vec);
        sp_u = sp->u;

        switch(sp->type)
        {
        case T_STRING:
          {
            string_t *str;
            svalue_t *item;

            str = sp_u.str;
            for(item = vec->item; --cnt >= 0; item++)
            {
                if (item->type == T_STRING
                 && mstreq(str, item->u.str))
                    break;
            }
            break;
          }

        case T_FLOAT:
        case T_CLOSURE:
        case T_SYMBOL:
        case T_QUOTED_ARRAY:
          {
            short x_generic;
            short type;
            svalue_t *item;

            type = sp->type;
            x_generic = sp->x.generic;
            for(item = vec->item; --cnt >= 0; item++)
            {
                /* TODO: Is this C99 compliant? */
                if (sp_u.str == item->u.str
                 && x_generic == item->x.generic
                 && item->type == type)
                    break;
            }
            break;
          }

        case T_NUMBER:
            if (!sp_u.number)
            {
                /* Search for 0 is special: it also finds destructed
                 * objects resp. closures on destructed objects (and
                 * changes them to 0).
                 */

                svalue_t *item;
                short type;

                for (item = vec->item; --cnt >= 0; item++)
                {
                    if ( (type = item->type) == T_NUMBER)
                    {
                        if ( !item->u.number )
                            break;
                    }
                    else if (destructed_object_ref(item))
                    {
                        assign_svalue(item, &const0);
                        break;
                    }
                }
                break;
            }

            /* FALLTHROUGH */

        case T_MAPPING:
        case T_OBJECT:
        case T_POINTER:
          {
            svalue_t *item;
            short type = sp->type;

            for (item = vec->item; --cnt >= 0; item++)
            {
                /* TODO: Is this C99 compliant? */
                if (sp_u.number == item->u.number
                 && item->type == type)
                    break;
            }
            break;
          }

        default:
            if (sp->type == T_LVALUE)
                error("Reference passed to member()\n");
            fatal("Bad type to member(): %s\n", typename(sp->type));
        }

        if (cnt >= 0)
        {
            cnt = (long)VEC_SIZE(vec) - cnt - 1;
        }
        /* else return -1 for failure */

        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, cnt);
        return sp;
    }

    /* --- Search a string --- */

    if (sp[-1].type == T_STRING)
    {
        string_t *str;
        char *str2;
        ptrdiff_t i;

        if (sp->type != T_NUMBER)
            efun_arg_error(2, T_NUMBER, sp->type, sp);
        str = sp[-1].u.str;
        i = sp->u.number;
        str2 = (i & ~0xff) ? NULL : memchr(get_txt(str), i, mstrsize(str));
        i = str2 ? (str2 - get_txt(str)) : -1;
        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, i);
        return sp;
    }

    /* --- Search a string --- */

    if (sp[-1].type == T_MAPPING)
    {
        int i;

        i = get_map_value(sp[-1].u.map, sp) != &const0;
        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, i);
        return sp;
    }

    /* Otherwise it's not searchable */

    fatal("Bad arg 1 to member(): type %s\n", typename(sp[-1].type));
    return sp;
} /* f_member() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_quote (svalue_t *sp)

/* EFUN quote()
 *
 *   mixed quote(mixed)
 *
 * Converts arrays to quoted arrays and strings to symbols.
 * Symbols and quoted arrays get quoted once more.
 */

{
    switch (sp->type)
    {
    case T_QUOTED_ARRAY:
    case T_SYMBOL:
        sp->x.quotes++;
        break;

    case T_POINTER:
        sp->type = T_QUOTED_ARRAY;
        sp->x.quotes = 1;
        break;

    case T_STRING:
        memsafe(sp->u.str = make_tabled(sp->u.str), mstrsize(sp->u.str)
               , "tabled symbol string");
        sp->type = T_SYMBOL;
        sp->x.quotes = 1;
        break;

    default:
        efun_gen_arg_error(1, sp->type, sp);
        /* NOTREACHED */
    }

    return sp;
} /* f_quote() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_sgn (svalue_t *sp)

/* VEFUN sgn()
 *
 *   int sgn (int|float arg)
 *
 * Return the sign of the argument: -1 if it's < 0, +1 if it's > 0, and
 * 0 if it is 0.
 */

{
    if (sp->type == T_NUMBER)
    {
        if (sp->u.number > 0)
            sp->u.number = 1;
        else if (sp->u.number < 0)
            sp->u.number = -1;
        else
            sp->u.number = 0;
    }
    else if (sp->type == T_FLOAT)
    {
        double d = READ_DOUBLE(sp);

        sp->type = T_NUMBER;
        if (d > 0.0)
            sp->u.number = 1;
        else if (d < 0.0)
            sp->u.number = -1;
        else
            sp->u.number = 0;
    }
    else
      error("Bad argument 1 to sgn(): not a number or float.\n");

    return sp;
} /* f_sgn() */

/*=========================================================================*/
/*                               OTHER                                     */

/*-------------------------------------------------------------------------*/
svalue_t *
f_debug_info (svalue_t *sp, int num_arg)

/* EFUN debug_info()
 *
 *   mixed debug_info(int flag)
 *   mixed debug_info(int flag, object obj)
 *   mixed debug_info(int flag, int arg2)
 *   mixed debug_info(int flag, int arg2, int arg3)
 *
 * Print out some driver internal debug information.
 *
 * DINFO_OBJECT (0): Information like heart_beat, enable_commands etc. of the
 *     specified object will be printed, and 0 returned.
 *
 * DINFO_MEMORY (1): Memory usage information like how many strings,
 *     variables, inherited files, object size etc. will be printed about the
 *     specified object, and 0 returned.
 *
 * DINFO_OBJLIST (2): Objects from the global object list are returned.  If
 *     the optional second arg is omitted, the first element (numbered 0)
 *     is returned. If the second arg is a number n, the n'th element of the
 *     object list returned. If the second arg is an object, it's successor
 *     in the object list is returned.
 *
 * DINFO_MALLOC: Equivalent to typing ``malloc'' at the command line.
 *     No second arg must be given. Returns 0.
 *
 * DINFO_STATUS (4): Collect the status information of the driver.  The
 *     optional second arg can be 0, "tables", "swap", "malloc" or any other
 *     argument accepted by the actual driver.  The result is a printable
 *     string with the status information, or 0 if an invalid argument was
 *     given.
 *
 * DINFO_DUMP (5): Dump the information specified by <arg2> into the
 *     filename specified by <arg3>. If <arg3> is omitted, a default file
 *     name is used. The function calls master->valid_write() to check that
 *     it can write the files. The file in question is always written anew.
 *     Result is 1 on success, or 0 if an error occured.
 *
 *     <arg2> == "objects": dump information about all objects. Default
 *       filename is '/OBJ_DUMP', the valid_write() will read 'objdump' for
 *       the function.
 *
 *     <arg2> == "opcodes": dump the usage statistics of the opcodes. Default
 *       filename is '/OPC_DUMP', the valid_write() will read 'opcdump' for
 *       the function. If the driver is compiled without OPCPROF, this call
 *       will always return 0.
 *
 * DINFO_DATA (6): Return raw information about an aspect of
 *     the driver specified by <arg2>. The result of the function
 *     is an array with the information, or 0 for unsupported values
 *     of <arg2>. If <arg3> is given and in the range of array indices for
 *     the given <arg2>, the result will be just the indexed array entry,
 *     but not the full array.
 *
 *     Allowed values for <arg2> are: DID_STATUS, DID_SWAP, DID_MALLOC.
 *
 *     <arg2> == DID_STATUS (0): Returns the "status" and "status tables"
 *        information:
 *
 *        int DID_ST_ACTIONS
 *        int DID_ST_ACTIONS_SIZE
 *            Number and size of allocated actions.
 *
 *        int DID_ST_SHADOWS
 *        int DID_ST_SHADOWS_SIZE
 *            Number and size of allocated shadows.
 *
 *        int DID_ST_OBJECTS
 *        int DID_ST_OBJECTS_SIZE
 *            Number and size of swapped-in objects.
 *
 *        int DID_ST_OBJECTS_SWAPPED
 *        int DID_ST_OBJECTS_SWAP_SIZE
 *            Number and size of swapped-out object variable blocks.
 *
 *        int DID_ST_OBJECTS_LIST
 *            Number of objects in the object list.
 *
 *        int DID_ST_OBJECTS_PROCESSED
 *            Number of listed objects processed in the last backend
 *            cycle.
 *
 *        float DID_ST_OBJECTS_AVG_PROC
 *            Average number of objects processed each cycle, expressed
 *            as fraction (0..1.0).
 *
 *        int DID_ST_OTABLE
 *            Number of objects listed in the object table.
 *
 *        int DID_ST_OTABLE_SLOTS
 *            Number of hash slots provided by the object table.
 *
 *        int DID_ST_OTABLE_SIZE
 *            Size occupied by the object table.
 *
 *        int DID_ST_HBEAT_OBJS
 *            Number of objects with a heartbeat.
 *
 *        int DID_ST_HBEAT_CALLS
 *            Number of heart_beats executed so far.
 *
 *        int DID_ST_HBEAT_CALLS_TOTAL
 *            Number of heart_beats calls so far. The difference to
 *            ST_HBEAT_CALLS is that the latter only counts heart beat
 *            calls during which at least one heart beat was actually executed.
 *
 *        int DID_ST_HBEAT_SLOTS
 *        int DID_ST_HBEAT_SIZE
 *            Number of allocated entries in the heart_beat table
 *            and its size.
 *
 *        int DID_ST_HBEAT_PROCESSED
 *            Number of heart_beats called in the last backend cycle.
 *
 *        float DID_ST_HBEAT_AVG_PROC
 *            Average number of heart_beats called each cycle, expressed
 *            as fraction (0..1.0).
 *
 *        int DID_ST_CALLOUTS
 *            Number of pending call_outs.
 *
 *        int DID_ST_CALLOUT_SLOTS
 *        int DID_ST_CALLOUT_SIZE
 *            Number of allocated entries in the call_out table
 *            and its size.
 *
 *        int DID_ST_ARRAYS
 *        int DID_ST_ARRAYS_SIZE
 *            Number and size of all arrays.
 *
 *        int DID_ST_MAPPINGS
 *        int DID_ST_MAPPINGS_SIZE
 *            Number and size of all mappings.
 *
 *        int DID_ST_PROGS
 *        int DID_ST_PROGS_SIZE
 *            Number and size of all programs.
 *
 *        int DID_ST_PROGS_SWAPPED
 *        int DID_ST_PROGS_SWAP_SIZE
 *            Number and size of swapped-out programs.
 *
 *        int DID_ST_USER_RESERVE
 *        int DID_ST_MASTER_RESERVE     
 *        int DID_ST_SYSTEM_RESERVE
 *            Current sizes of the three memory reserves.
 *
 *        int DID_ST_ADD_MESSAGE
 *        int DID_ST_PACKETS
 *        int DID_ST_PACKET_SIZE
 *            Number of calls to add_message(), number and total size
 *            of sent packets.
 *            If the driver is not compiled with COMM_STAT, all three
 *            values are returned as -1.
 *
 *        int DID_ST_APPLY
 *        int DID_ST_APPLY_HITS
 *            Number of calls to apply_low(), and how many of these
 *            were cache hits.
 *            If the driver is not compiled with APPLY_CACHE_STAT, all two
 *            values are returned as -1.
 *
 *
 *        int DID_ST_STRINGS
 *        int DID_ST_STRING_SIZE
 *            Total number and size of string requests.
 *
 *        int DID_ST_STR_TABLE_SIZE
 *            Size of the string table structure itself.
 *
 *        int DID_ST_STR_OVERHEAD
 *            Size of the overhead per string.
 *
 *        int DID_ST_STR_IT_OVERHEAD
 *            Size of the additional overhead per indirectly tabled string.
 *
 *        int DID_ST_UNTABLED
 *        int DID_ST_UNTABLED_SIZE
 *            Total number and size of existing untabled strings.
 *
 *        int DID_ST_ITABLED
 *        int DID_ST_ITABLED_SIZE
 *            Total number and size of existing indirectly tabled strings.
 *            Of the memory, only DID_ST_ITABLED * DID_ST_STR_IT_OVERHEAD
 *            is not shared with the tabled strings.
 *
 *        int DID_ST_TABLED
 *        int DID_ST_TABLED_SIZE
 *            Total number and size of existing directly tabled strings.
 *
 *        int DID_ST_STR_SEARCHES
 *        int DID_ST_STR_SEARCHLEN
 *            Number and accumulated length of string searches by address.
 *
 *        int DID_ST_STR_SEARCHES_BYVALUE
 *        int DID_ST_STR_SEARCHLEN_BYVALUE
 *            Number and accumulated length of string searches by value.
 *
 *
 *        int DID_ST_RX_CACHED
 *            Number of regular expressions cached.
 *
 *        int DID_ST_RX_TABLE
 *        int DID_ST_RX_TABLE_SIZE
 *            Number of slots in the regexp cache table, and size of the
 *            memory currently held by it and the cached expressions.
 *
 *        int DID_ST_RX_REQUESTS
 *            Number of requests for new regexps.
 *
 *        int DID_ST_RX_REQ_FOUND
 *            Number of requested regexps found in the table.
 *
 *        int DID_ST_RX_REQ_COLL
 *            Number of requested new regexps which collided with
 *            a cached one.
 *
 *
 *     <arg2> == DID_SWAP (1): Returns the "status swap" information:
 *
 *        int DID_SW_PROGS
 *        int DID_SW_PROG_SIZE
 *            Number and size of swapped-out program blocks.
 *
 *        int DID_SW_PROG_UNSWAPPED
 *        int DID_SW_PROG_U_SIZE
 *            Number and size of unswapped program blocks.
 *
 *        int DID_SW_VARS
 *        int DID_SW_VAR_SIZE
 *            Number and size of swapped-out variable blocks.
 *
 *        int DID_SW_FREE
 *        int DID_SW_FREE_SIZE
 *            Number and size of free blocks in the swap file.
 *
 *        int DID_SW_FILE_SIZE
 *            Size of the swap file.
 *
 *        int DID_SW_REUSED
 *            Total reused space in the swap file.
 *
 *        int DID_SW_SEARCHES
 *        int DID_SW_SEARCH_LEN
 *            Number and total length of searches for block to reuse
 *            in the swap file.
 *
 *        int DID_SW_F_SEARCHES
 *        int DID_SW_F_SEARCH_LEN
 *            Number and total length of searches for a block to free.
 *
 *        int DID_SW_COMPACT
 *            TRUE if the swapper is running in compact mode.
 *
 *        int DID_SW_RECYCLE_FREE
 *            TRUE if the swapper is currently recycling free block.
 *
 *
 *     <arg2> == DID_MEMORY (2): Returns the "status malloc" information:
 *
 *        string DID_MEM_NAME
 *            The name of the allocator: "sysmalloc" or "smalloc".
 *
 *        int DID_MEM_SBRK
 *        int DID_MEM_SBKR_SIZE
 *            Number and size of memory blocks requested from the
 *            operating system (smalloc only).
 *
 *        int DID_MEM_LARGE
 *        int DID_MEM_LARGE_SIZE
 *        int DID_MEM_LFREE
 *        int DID_MEM_LFREE_SIZE
 *            Number and size of large allocated resp. free blocks.
 *            (smalloc only)
 *
 *        int DID_MEM_LWASTED
 *        int DID_MEM_LWASTED_SIZE
 *            Number and size of unusable large memory fragments.
 *            (smalloc only).
 *
 *        int DID_MEM_CHUNK
 *        int DID_MEM_CHUNK_SIZE
 *            Number and size of small chunk blocks (smalloc only).
 *
 *        int DID_MEM_UNUSED
 *            Unused space in the current small chunk block
 *            (smalloc only).
 *
 *        int DID_MEM_SMALL
 *        int DID_MEM_SMALL_SIZE
 *        int DID_MEM_SFREE
 *        int DID_MEM_SFREE_SIZE
 *            Number and size of small allocated resp. free blocks
 *            (smalloc only).
 *
 *        int DID_MEM_SWASTED
 *        int DID_MEM_SWASTED_SIZE
 *            Number and size of unusably small memory fragments.
 *            (smalloc only).
 *
 *        int DID_MEM_MINC_CALLS
 *        int DID_MEM_MINC_SUCCESS
 *        int DID_MEM_MINC_SIZE
 *            Number of calls to malloc_increment(), the number
 *            of successes and the size of memory allocated this
 *            way (smalloc only).
 *
 *        int DID_MEM_PERM
 *        int DID_MEM_PERM_SIZE
 *            Number and size of permanent (non-GCable) allocations
 *            (smalloc only).
 *
 *        int DID_MEM_CLIB
 *        int DID_MEM_CLIB_SIZE
 *            Number and size of allocations done through the
 *            clib functions (smalloc only with SBRK_OK).
 *
 *
 * DINFO_TRACE (7): Return the call stack 'trace' information as specified
 *     by <arg2>. The result of the function is either an array (format
 *     explained below), or a printable string. Omitting <arg2> defaults
 *     to DIT_CURRENT.
 *
 *     <arg2> == DIT_CURRENT (0) or == DIT_ERROR (1): Return the information in
 *        array form. For DIT_CURRENT, the current call trace is returned,
 *        for DIT_ERROR the trace of the last uncaught error.
 *
 *        If the array has just one entry, the trace information is not
 *        available and the one entry is string with the reason.
 *
 *        If the array has more than one entries, the first entry is 0 or the
 *        name of the object with the heartbeat which started the current
 *        thread; all following entries describe the call stack starting with
 *        the topmost function called.
 *
 *        All call entries are arrays themselves with the following elements:
 *
 *        int[TRACE_TYPE]: The type of the call frame:
 *            TRACE_TYPE_SYMBOL (0): a function symbol (shouldn't happen).
 *            TRACE_TYPE_SEFUN  (1): a simul-efun.
 *            TRACE_TYPE_EFUN   (2): an efun closure.
 *            TRACE_TYPE_LAMBDA (3): a lambda closure.
 *            TRACE_TYPE_LFUN   (4): a normal lfun.
 *
 *        mixed[TRACE_NAME]: The 'name' of the called frame:
 *            _TYPE_EFUN:   either the name of the efun, or the code of
 *                          the instruction for operator closures
 *            _TYPE_LAMBDA: the numeric lambda identifier.
 *            _TYPE_LFUN:   the name of the lfun.
 *
 *        string[TRACE_PROGRAM]: The (file)name of the program holding the
 *            code.
 *        string[TRACE_OBJECT]:  The name of the object for which the code
 *                               was executed.
 *        int[TRACE_LOC]:
 *            _TYPE_LAMBDA: current program offset from the start of the
 *                          closure code.
 *            _TYPE_LFUN:   the line number.
 *
 *     <arg2> == DIT_STR_CURRENT (2): Return the information about the current
 *        call trace as printable string.
 *
 * TODO: debug_info() and all associated routines are almost big enough
 * TODO:: to justify a file on their own.
 */

{
    svalue_t *arg;
    svalue_t res;
    object_t *ob;
  
    arg = sp-num_arg+1;
    inter_sp = sp;

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

        int flags;
        object_t *prev, *obj2;

        if (num_arg != 2)
            error("bad number of arguments to debug_info\n");
        if (arg[1].type != T_OBJECT)
            vefun_arg_error(2, T_OBJECT, arg[1].type, sp);
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
        add_message("name        : '%s'\n", get_txt(ob->name));
        add_message("load_name   : '%s'\n", get_txt(ob->load_name));
        obj2 = ob->next_all;
        if (obj2)
            add_message("next_all    : OBJ(%s)\n",
              obj2->next_all ? get_txt(obj2->name) : "NULL");
        prev = ob->prev_all;
        if (prev) {
            add_message("Previous object in object list: OBJ(%s)\n"
                       , get_txt(prev->name));
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

        program_t *pg;
        mp_int v0, v1, v2;

        if (num_arg != 2)
            error("bad number of arguments to debug_info\n");
        if (sp->type != T_OBJECT)
            vefun_arg_error(2, T_OBJECT, sp->type, sp);
        if ((sp->u.ob->flags & O_SWAPPED) && load_ob_from_swap(sp->u.ob) < 0)
            error("Out of memory: unswap object '%s'\n", get_txt(sp->u.ob->name));
        pg = sp->u.ob->prog;
        add_message("program ref's %3ld\n",        pg->ref);
        add_message("Name: '%s'\n",                get_txt(pg->name));
        add_message("program size    %6ld\n"
          ,(long)(PROGRAM_END(*pg) - pg->program));
        add_message("num func's:  %3d (%4ld)\n", pg->num_functions
          , (long)(pg->num_functions * sizeof(uint32) +
                  pg->num_function_names * sizeof(short)));
        add_message("num vars:    %3d (%4ld)\n", pg->num_variables
          , (long)(pg->num_variables * sizeof(variable_t)));

        v1 = program_string_size(pg, &v0, &v2);
        add_message("num strings: %3d (%4ld) : overhead %ld + data %ld (%ld)\n"
                   , pg->num_strings
                   , (long)(v0 + v1)
                   , (long)v0
                   , (long)v1
                   , (long)v2
                   );

        {
            int i = pg->num_inherited;
            int cnt = 0;
            inherit_t *inheritp;

            for (inheritp = pg->inherit; i--; inheritp++)
            {
                if (inheritp->inherit_type == INHERIT_TYPE_NORMAL)
                    cnt++;
            }
            add_message("num inherits %3d (%4ld)\n", cnt
                , (long)(pg->num_inherited * sizeof(inherit_t)));
        }
        add_message("total size      %6ld\n"
          ,pg->total_size);

        v1 = data_size(sp->u.ob, &v2);
        add_message("data size       %6ld (%6ld)\n", v1, v2);
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
                if (sp->type != T_OBJECT)
                    vefun_exp_arg_error(2, (1 << T_OBJECT)|(1 << T_NUMBER)
                                         , sp->type, sp);
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
            sp->u.str = STR_EMPTY; /* Just for status_parse() */
        } else {
            if (arg[1].type != T_OBJECT)
                vefun_exp_arg_error(2, (1 << T_OBJECT)|(1 << T_NULL)
                                     , arg[1].type, sp);
        }
        if (status_parse(&sbuf, get_txt(sp->u.str)))
            strbuf_store(&sbuf, &res);
        else
            strbuf_free(&sbuf);
        break;
      }

    case DINFO_DUMP:  /* --- DINFO_DUMP --- */
      {
        /* Dump information into files */

        string_t * fname;

        if (num_arg != 2 && num_arg != 3)
            error("bad number of arguments to debug_info\n");

        if (arg[1].type != T_STRING)
            vefun_arg_error(2, T_STRING, arg[1].type, sp);
        if (num_arg == 2
         || (sp->type == T_NUMBER && sp->u.number == 0)) {
            fname = NULL;
        } else {
            if (arg[2].type != T_STRING)
                vefun_exp_arg_error(3, TF_NULL|TF_STRING
                                     , arg[2].type, sp);
            fname = sp->u.str;
        }

        if (!strcmp(get_txt(arg[1].u.str), "objects"))
        {
            res.u.number = dumpstat(fname ? fname : STR_OBJDUMP) ? 1 : 0;
            break;
        }

        if (!strcmp(get_txt(arg[1].u.str), "opcodes"))
        {
#ifdef OPCPROF
            res.u.number = opcdump(fname ? fname : STR_OPCDUMP) ? 1 : 0;
#endif
            break;
        }

        error("Bad argument '%s' to debug_info(DINFO_DUMP).\n", get_txt(arg[1].u.str));
        break;
      }

    case DINFO_DATA:  /* --- DINFO_DATA --- */
      {
        /* Return information about the one or other driver interna.
         * This is basically the same information returned by DINFO_STATUS,
         * just not pre-processed into nice strings.
         */

        vector_t *v;
        svalue_t *dinfo_arg;
        int       value = -1;
  
        if (num_arg != 2 && num_arg != 3)
            error("bad number of arguments to debug_info\n");
        if (arg[1].type != T_NUMBER)
            vefun_arg_error(2, T_NUMBER, arg[1].type, sp);
        if (num_arg == 3)
        {
            if (arg[2].type != T_NUMBER)
                vefun_arg_error(3, T_NUMBER, arg[2].type, sp);
            value = arg[2].u.number;
        }

        switch(arg[1].u.number)
        {
#define PREP(which) \
            if (value == -1) { \
                v = allocate_array(which); \
                if (!v) \
                    error("Out of memory: array[%d] for result.\n" \
                         , which); \
                dinfo_arg = v->item; \
            } else { \
                v = NULL; \
                if (value < 0 || value >= which) \
                    error("Illegal index for debug_info(): %d, " \
                          "expected 0..%d\n", value, which-1); \
                dinfo_arg = &res; \
            }

        case DID_STATUS:
            PREP(DID_STATUS_MAX)

            dinfo_data_status(dinfo_arg, value);
            otable_dinfo_status(dinfo_arg, value);
            hbeat_dinfo_status(dinfo_arg, value);
            callout_dinfo_status(dinfo_arg, value);
            string_dinfo_status(dinfo_arg, value);
#ifdef RXCACHE_TABLE
            rxcache_dinfo_status(dinfo_arg, value);
#endif

            if (value == -1)
                put_array(&res, v);
            break;

        case DID_SWAP:
            PREP(DID_SWAP_MAX)

            swap_dinfo_data(dinfo_arg, value);
            if (value == -1)
                put_array(&res, v);
            break;

        case DID_MEMORY:
            PREP(DID_MEMORY_MAX)

#if defined(MALLOC_smalloc)
            smalloc_dinfo_data(dinfo_arg, value);
#endif
#if defined(MALLOC_sysmalloc)
            if (value == -1)
                put_ref_string(v->item+DID_MEM_NAME, STR_SYSTEM_MALLOC);
            else if (value == DID_MEM_NAME)
                put_ref_string(dinfo_arg, STR_SYSTEM_MALLOC);
#endif
            if (value == -1)
                put_array(&res, v);
            break;

#undef PREP
        }
        break;
      }

    case DINFO_TRACE:  /* --- DINFO_TRACE --- */
      {
        /* Return the trace information */

        if (num_arg != 1 && num_arg != 2)
            error("bad number of arguments to debug_info\n");

        if (num_arg == 2 && sp->type != T_NUMBER)
            error("bad arg 2 to debug_info(): not a number.\n");

        if (num_arg == 1 || sp->u.number == DIT_CURRENT)
        {
            vector_t * vec;

            (void)collect_trace(NULL, &vec);
            put_array(&res, vec);
        }
        else if (sp->u.number == DIT_ERROR)
        {
            if (current_error_trace)
                put_ref_array(&res, current_error_trace);
            else
            {
                vector_t *vec;

                vec = allocate_uninit_array(1);
                put_ref_string(vec->item, STR_NO_TRACE);
                put_array(&res, vec);
            }
        }
        else if (sp->u.number == DIT_STR_CURRENT)
        {
            strbuf_t sbuf;

            strbuf_zero(&sbuf);
            (void)collect_trace(&sbuf, NULL);
            put_string(&res, new_mstring(sbuf.buf));
            strbuf_free(&sbuf);
        }
        else
            error("bad arg 2 to debug_info(): %ld, expected 0..2\n"
                 , sp->u.number);
        break;
      }

    default:
        error("Bad debug_info() request value: %ld\n", arg[0].u.number);
        /* NOTREACHED */
        break;
    }

    /* Clean up the stack and return the result */
    
    sp = pop_n_elems(num_arg, sp);

    sp++;
    *sp = res;
    return sp;
} /* f_debug_info() */

/*-------------------------------------------------------------------------*/
#ifdef F_RUSAGE

svalue_t *
f_rusage (svalue_t *sp)

/* EFUN rusage()
 *
 *   int *rusage(void)
 *
 * Return an array with current system resource usage statistics,
 * as returned by the getrusage(2) of Unix.
 * namely: utime, stime, maxrss, rus.ru_ixrss, rus.ru_idrss,
 * rus.ru_isrss, rus.ru_minflt, rus.ru_majflt, rus.ru_nswap,
 * rus.ru_inblock, rus.ru_oublock, rus.ru_msgsnd,
 * rus.ru_msgrcv, rus.ru_nsignals, rus.ru_nvcsw,
 * rus.ru_nivcsw
 * TODO: The indices should be in an include file.
 */

{
    struct rusage rus;
    vector_t *res;
    svalue_t *v;
#ifndef GETRUSAGE_RESTRICTED
    int maxrss;
#endif

    if (getrusage(RUSAGE_SELF, &rus) < 0) {
        push_number(sp, 0);
        return sp;
    }

    res = allocate_array(16);
    v = res->item;
    v[ 0].u.number = RUSAGE_TIME(rus.ru_utime);
    v[ 1].u.number = RUSAGE_TIME(rus.ru_stime);
#ifndef GETRUSAGE_RESTRICTED
    maxrss = rus.ru_maxrss;
#ifdef sun
    maxrss *= getpagesize() / 1024;
#endif
    v[ 2].u.number = maxrss;
    v[ 3].u.number = rus.ru_ixrss;
    v[ 4].u.number = rus.ru_idrss;
    v[ 5].u.number = rus.ru_isrss;
    v[ 6].u.number = rus.ru_minflt;
    v[ 7].u.number = rus.ru_majflt;
    v[ 8].u.number = rus.ru_nswap;
    v[ 9].u.number = rus.ru_inblock;
    v[10].u.number = rus.ru_oublock;
    v[11].u.number = rus.ru_msgsnd;
    v[12].u.number = rus.ru_msgrcv;
    v[13].u.number = rus.ru_nsignals;
    v[14].u.number = rus.ru_nvcsw;
    v[15].u.number = rus.ru_nivcsw;
#endif /* GETRUSAGE_RESTRICTED */

    push_array(sp, res);

    return sp;
} /* f_rusage() */

#endif /* F_RUSAGE */

/*-------------------------------------------------------------------------*/
svalue_t *
f_random (svalue_t *sp)
    
/* EFUN random()
 *
 *   int random(int n)
 *
 * Returns a number in the random range [0 .. n-1].
 *
 * The random number generator is proven to deliver an equal
 * distribution of numbers over a big range, with no repetition of
 * number sequences for a long time.
 */

{
    if (sp->u.number <= 0)
        sp->u.number = 0;
    else
        sp->u.number = (p_int)random_number((uint32)sp->u.number);

    return sp;
} /* f_random() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_shutdown (svalue_t *sp)

/* EFUN shutdown()
 *
 *   void shutdown()
 *
 * Shutdown the mud. Never use this efun. Instead if you have a
 * need to shutdown the mud use the shutdown command.
 * You may be asking yourself, if you're not supposed
 * to use it why is it here?  Sorry, I cannot answer
 * that.  Its top secret.
 */

{
    extra_jobs_to_do = MY_TRUE;
    game_is_being_shut_down = MY_TRUE;
    return sp;
} /* f_shutdown() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_ctime(svalue_t *sp)

/* EFUN ctime()
 *
 *   string ctime(int clock = time())
 *   string ctime(int* uclock)
 *
 * Interpret the argument clock as number of seconds since Jan,
 * 1st, 1970, 0.00 and convert it to a nice date and time string.
 *
 * Alternatively, accept an array of two ints: the first is <clock>
 * value as in the first form, the second int is the number of
 * microseconds elapsed in the current second.
 */

{
    char *ts, *cp;
    string_t *rc;

    if (sp->type != T_NUMBER)
    {
        if (VEC_SIZE(sp->u.vec) != 2)
            error("Bad arg 1 to ctime(): Invalid array size %ld, expected 2.\n"
                 , (long)VEC_SIZE(sp->u.vec));
        if (sp->u.vec->item[0].type != T_NUMBER)
            error("Bad arg 1 to ctime(): Element 0 is '%s', expected 'int'.\n"
                 , efun_arg_typename(sp->u.vec->item[0].type));
        if (sp->u.vec->item[1].type != T_NUMBER)
            error("Bad arg 1 to ctime(): Element 1 is '%s', expected 'int'.\n"
                 , efun_arg_typename(sp->u.vec->item[0].type));
        ts = utime_string( sp->u.vec->item[0].u.number
                         , sp->u.vec->item[1].u.number);
    }
    else
    {
        ts = time_string(sp->u.number);
    }

    /* If the string contains nl characters, extract the substring
     * before the first one. Else just copy the (volatile) result
     * we got.
     */
    cp = strchr(ts, '\n');
    if (cp)
    {
        int len = cp - ts;
        memsafe(rc = new_n_mstring(ts, len), len, "ctime() result");
    }
    else
    {
        memsafe(rc = new_mstring(ts), strlen(ts), "ctime() result");
    }
    put_string(sp, rc);
    return sp;
} /* f_ctime() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_time (svalue_t *sp)

/* EFUN time()
 *
 *   int time()
 *
 * Return number of seconds ellapsed since 1. Jan 1970, 0.0:0 GMT
 *
 * Actually the time is updated only once in every backend cycle.
 */

{
    push_number(sp, current_time);

    return sp;
} /* f_time() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_utime (svalue_t *sp)

/* EFUN utime()
 *
 *   int* utime()
 *
 * Return the time since 1. Jan 1970, 00:00:00 GMT in microsecond
 * precision.
 * 
 * Return is an array:
 *   int[0]: number of seconds elapsed
 *   int[1]: number of microseconds within the current second.
 */

{
    svalue_t *v;
    vector_t *res;
    struct timeval tv;

    res = allocate_array(2);
    v = res->item;
    if (!gettimeofday(&tv, NULL))
    {
        v[0].u.number = tv.tv_sec;
        v[1].u.number = tv.tv_usec;
    }
    else
    {
        int errnum = errno;
        fprintf(stderr, "%s gettimeofday() failed: %d %s\n"
               , time_stamp(), errnum, strerror(errnum));
        v[0].u.number = current_time;
        v[1].u.number = 0;
    }
    push_array(sp, res);

    return sp;
} /* f_utime() */

/***************************************************************************/

