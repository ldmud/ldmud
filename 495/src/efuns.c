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
 *    vefun: copy_bits()
 *    tefun: make_shared_string()
 *    tefun: md5()
 *    tefun: process_string() (optional)
 *    efun:  sscanf()
 *    efun:  terminal_colour()
 *    tefun: trim()
 *    tefun: upper_case()
 *
 * Objects:
 *    xefun: all_environment()
 *    tefun: blueprint()
 *    vefun: clones()
 *    tefun: object_info()
 *    tefun: present_clone() (preliminary)
 *    tefun: present()
 *    tefun: set_is_wizard() (optional)
 *    tefun: set_modify_command()
 *    tefun: set_prompt()
 *    tefun: transfer() (optional)
 *     efun: say()
 *     efun: tell_room()
 *
 * Values:
 *    tefun: copy()
 *    tefun: deep_copy()
 *    vefun: filter()
 *    vefun: map()
 *    vefun: min()
 *    vefun: max()
 *
 * Others:
 *    tefun: debug_info()
 *    tefun: gmtime()
 *    tefun: localtime()
 *    tefun: shutdown()
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>

#define USES_SVALUE_STRLEN
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
#include "object.h"
#include "otable.h"
#include "ptrtable.h"
#include "rxcache.h"
#include "simulate.h"
#include "smalloc.h"
#include "stdstrings.h"
#include "stralloc.h"
#include "strfuns.h"
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/objectinfo.h"
#include "../mudlib/sys/strings.h"
#include "../mudlib/sys/time.h"

/* Forward declarations */
static void copy_svalue (svalue_t *dest, svalue_t *, struct pointer_table *, int);

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

#define TYPE_TESTV4(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_vararg(4, sp);

#define TYPE_TESTV5(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_xefun_vararg(5, sp);

/* Typetests for efuns */

#define E_TYPE_TESTV1(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_efun_arg(1, -2, sp);

#define E_TYPE_TESTV2(arg1,type1) \
  if ((arg1)->type != type1) \
      bad_efun_arg(2, -2, sp);

/*-------------------------------------------------------------------------*/

#ifdef F_SET_IS_WIZARD
Bool is_wizard_used = MY_FALSE;
  /* TODO: This flag can go when the special commands are gone. */
#endif


/*=========================================================================*/
/*                              STRINGS                                    */

/*-------------------------------------------------------------------------*/
static INLINE p_int
last_bit (const char *str)

/* Return the number of the last set bit in bitstring <str>.
 */

{
    mp_int   pos;
    long     len;
    int      c;

    pos = -1;

    /* Get the arguments */
    len = (long)strlen(str);

    /* First, find the last non-zero character */
    c = 0;
    while (len-- > 0 && (c = str[len]) == ' ') NOOP;

    if (len >= 0)
    {
        int pattern;

        /* Found a character, now determine the bit position */
        c -= ' ';
        pos = 6 * len + 5;
        for ( pattern = 1 << 5
            ; pattern && !(c & pattern)
            ; pattern >>= 1, pos--)
            NOOP;
    }

    return pos;
} /* last_bit() */

/*-------------------------------------------------------------------------*/
static INLINE void
copy_bits ( char * dest, p_int deststart
          , char * src,  p_int srcstart
          , p_int len
          )

/* Copy the bitrange <src>[<srcstart>..<srcstart>+<len>[ into the
 * <dest> string starting at <deststart>.
 * <dest> is supposed to be allocated big enough.
 */

{
    char * pDest, * pSrc;

    if (len < 1)
        return;

    pDest = dest + deststart / 6;
    pSrc = src + srcstart / 6;

    /* Special case: both source and target bit range start on the same bit.
     */
    if (deststart % 6 == srcstart % 6)
    {
        /* Copy the first few bits until the next byte boundary.
         * Make sure not to overwrite preexisting bytes
         */
        if (deststart % 6 != 0)
        {
            int getmask, storemask;

            storemask = (1 << deststart % 6) - 1;
              /* storemask is now 00011111 through 00000001 */
            getmask = ~storemask;
              /* getmask   is now 11100000 through 11111110 */

            if (len < 6 - deststart % 6)
            {
                /* A really small copy: we don't even need all bits from
                 * the src byte.
                 */
                int len2 = 6 - deststart % 6 - len;
                int lenmask;

                lenmask = (1 << len2) - 1;
                lenmask <<= 6 - len2;
                  /* lenmask has now 1-bits for the bits we need not to copy */
                storemask &= ~lenmask;
            }

            *pDest = ' ' + (  ((*pDest - ' ') & storemask)
                            | ((*pSrc - ' ') & getmask)
                           );

            len -= 6 - deststart % 6; /* might become negative here */
            pDest++;
            pSrc++;
        }

        /* Do a fast byte copy */
        if (len >= 6)
        {
            memcpy(pDest, pSrc, len / 6);
        }

        /* Copy the remaining bytes, if any */
        if (len > 0 && len % 6 != 0)
        {
            int mask;

            mask = (1 << (len % 6)) - 1;
              /* mask is now 00011111 through 00000001 */
            pDest[len / 6] = ((pSrc[len / 6] - ' ') & mask) + ' ';
        }
    }
    else
    {
        /* Copy enough bits so that deststart points to a byte boundary */
        if (deststart % 6 != 0)
        {
            /* Since these are going to be at max five bits, we copy
             * them in a loop bit by bit.
             */

            int getmask, getbit;
            int storemask, storebit;
            char src_bits, dest_bits;

            getbit = srcstart % 6;
            getmask = 1 << getbit;
            storebit = deststart % 6;
            storemask = 1 << storebit;

            src_bits = *pSrc - ' ';
            dest_bits = *pDest - ' ';

            while (len > 0 && storebit != 6)
            {
                if (0 != (src_bits & getmask))
                    dest_bits = dest_bits | storemask;
                else
                    dest_bits = dest_bits & (~storemask);

                srcstart++;
                getbit++; getmask <<= 1;
                if (getbit == 6)
                {
                    getbit = 0;
                    getmask = 0x01;
                    pSrc++;
                    src_bits = *pSrc - ' ';
                }

                deststart++;
                storebit++; storemask <<= 1;

                len--;
            }

            *pDest = dest_bits + ' ';
            pDest++;
        } /* if need to align deststart */

        /* deststart now points to byte boundary, that means we can
         * now copy the bits by blockwise selection from the src string.
         */
        if (len >= 6)
        {
            int getmask1, getmask2, len2;
            int getshift1, getshift2;

            len2 = 6 - srcstart % 6; /* Number of bits in first byte */

            getmask1 = (1 << len2) - 1;
            getmask1 <<= 6 - len2;
              /* getmask1 has now the mask for the high order bits of the
               * first src byte
               */
            getshift1 = 6 - len2;
            getshift2 = len2;

            len2 = 6 - len2;  /* Number of bits in second byte */

            getmask2 = (1 << len2) - 1;
              /* getmask2 has now the mask for the low order bits of the
               * second src byte
               */

            while (len >= 6)
            {
                *pDest = ' ' + ( ((pSrc[0]-' ') & getmask1) >> getshift1
                                |((pSrc[1]-' ') & getmask2) << getshift2
                               );
                len -= 6;
                pDest++;
                pSrc++;
                srcstart += 6;
                deststart += 6;
            }
        } /* if >= 6 bits left */

        /* deststart still points to byte boundary, but there are
         * less than 6 bits left to copy.
         */
        if (len > 0)
        {
            /* Since these are going to be at max five bits, we copy
             * them in a loop bit by bit.
             */

            int getmask, getbit;
            int storemask, storebit;
            char src_bits, dest_bits;

            getbit = srcstart % 6;
            getmask = 1 << getbit;
            storebit = 0;
            storemask = 0x01;

            src_bits = *pSrc - ' ';
            dest_bits = 0;

            while (len > 0)
            {
                if (0 != (src_bits & getmask))
                    dest_bits |= storemask;

                srcstart++;
                getbit++; getmask <<= 1;
                if (getbit == 6)
                {
                    getbit = 0;
                    getmask = 0x01;
                    pSrc++;
                    src_bits = *pSrc - ' ';
                }

                deststart++;
                storebit++; storemask <<= 1;

                len--;
            }

            *pDest = dest_bits + ' ';
        } /* if (1..5 bits left) */
    } /* case selection */
} /* copy_bits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_copy_bits (svalue_t *sp, int num_arg)

/* EFUN copy_bits()
 *
 *     string copy_bits(string src, string dest
 *                     [, int srcstart [, int deststart [, int copylen ]]]
 *                      )
 *
 * Copy the bitrange [<srcstart>..<srcstart>+<copylen>[ from bitstring <src>
 * and copy it into the bitstring <dest> starting at <deststart>, overwriting
 * the original bits at those positions.
 * The resulting combined string is returned, the input strings remain
 * unaffected.
 *
 * If <srcstart> is not given, <src> is copied from the start.
 *   If <srcstart> is negative, it is counted from one past the last set
 *   bit in the string (ie. '-1' will index the last set bit).
 * If <deststart> is not given, <dest> will be overwritten from the start.
 *   If <deststart> is negative, it is counted from one past the last set
 *   bit in the string (ie. '-1' will index the last set bit).
 * If <copylen> is not given, it is assumed to be infinite, ie. the result
 *   will consist of <dest> up to position <deststart>, followed by
 *   the data copied from <src>.
 *   If <copylen> is negative, the function will copy the abs(<copylen>)
 *   bits _before_ <srcstart> in to the result.
 *
 * None of the range limits can become negative.
 */

{
    p_int srcstart, deststart, copylen;
    p_int srclen, destlen, resultlen;
    Bool copyall;
    char *src, *dest, *result;
    int result_string_type;

    /* Get the optional command arguments */
    srcstart = deststart = copylen = 0;
    copyall = MY_TRUE;

    switch (num_arg)
    {
    case 5:
        TYPE_TESTV5(sp, T_NUMBER);
        copyall = MY_FALSE;
        copylen = sp->u.number;
        sp--;
        num_arg--;
        /* FALL THROUGH */

    case 4:
        TYPE_TESTV4(sp, T_NUMBER);
        deststart = sp->u.number;
        sp--;
        num_arg--;
        /* FALL THROUGH */

    case 3:
        TYPE_TESTV3(sp, T_NUMBER);
        srcstart = sp->u.number;
        sp--;
        num_arg--;
        /* FALL THROUGH */
    }
    inter_sp = sp;

    /* Get the fixed command arguments and check for consistency */
    TYPE_TESTV2(sp, T_STRING);
    TYPE_TESTV1(sp-1, T_STRING);
    dest = sp->u.string;
    src = sp[-1].u.string;

    sp++; /* We might need to save a precautionary reference to the result */

    srclen = last_bit(src)+1;
    destlen = last_bit(dest)+1;

    if (srcstart < 0 && srcstart + srclen < 0)
        error("Bad argument 3 to copy_bits(): Index %ld is out of range "
              "(last bit: %ld).\n"
             , (long)srcstart, (long)srclen);
    if (srcstart < 0)
        srcstart += srclen;

    if (deststart < 0 && deststart + destlen < 0)
        error("Bad argument 4 to copy_bits(): Index %ld is out of range "
              "(last bit: %ld).\n"
             , (long)deststart, (long)destlen);
    if (deststart < 0)
        deststart += destlen;

    if (!copyall && copylen < 0)
    {
        if (srcstart + copylen < 0)
            error("Bad argument 5 to copy_bits(): Length %ld out of range "
                  "(start index: %ld).\n"
                 , (long)copylen, (long)srcstart);

        srcstart += copylen;
        copylen = -copylen;
    }

    /* Test the input strings for sanity */
    {
        char *cp;
        long len;

        len = (long)svalue_strlen(sp-2);
        cp = src;
        for ( ; len > 0; len--, cp++)
        {
            int c = *cp - ' ';
            if (c < 0 || c > 0x3f)
                error("Bad argument 1 to copy_bits(): String contains "
                      "illegal character %d\n", c + ' ');
        }

        len = (long)svalue_strlen(sp-1);
        cp = dest;
        for ( ; len > 0; len--, cp++)
        {
            int c = *cp - ' ';
            if (c < 0 || c > 0x3f)
                error("Bad argument 2 to copy_bits(): String contains "
                      "illegal character %d\n", c + ' ');
        }
    }

    /* Do the copying - some constellations are really simple */
    if (copyall)
    {
        if (srcstart == 0 && deststart == 0)
        {
            if (sp[-2].x.string_type == STRING_SHARED)
            {
                result_string_type = STRING_SHARED;
                result = ref_string(src);
            }
            else
            {
                result_string_type = STRING_MALLOC;
                result = string_copy(src);
            }
        }
        else
        {
            if (srclen > srcstart)
                copylen = srclen - srcstart;
            else
                copylen = 0;

            if (PINT_MAX - copylen < deststart)
                error("copy_bits: result length exceeds numerical limit: "
                      "%ld + %ld\n"
                     , (long)deststart, (long)copylen
                     );
            resultlen = deststart + copylen;
            if (resultlen > MAX_BITS || resultlen < 0)
                error("copy_bits: Result too big: %lu bits\n"
                     , (unsigned long)resultlen);

            /* Get the result string and store the reference on the stack
             * for error cleanups.
             */
            result_string_type = STRING_MALLOC;
            result = xalloc((resultlen + 5) / 6 + 1);
            if (result == NULL)
            {
                error("Out of memory.\n");
                /* NOTREACHED */
            }
            memset(result, ' ', (resultlen + 5) / 6);
            result[(resultlen + 5) / 6] = '\0';
            inter_sp = sp; put_malloced_string(sp, result);

            /* Copy the first part of dest into the result */
            if (deststart > 0)
            {
                if (deststart > destlen)
                    copy_bits(result, 0, dest, 0, destlen);
                else
                    copy_bits(result, 0, dest, 0, deststart);
            }

            /* Now append the src */
            copy_bits(result, deststart, src, srcstart, copylen);
        }
    }
    else if (copylen == 0)
    {
        if (sp[-1].x.string_type == STRING_SHARED)
        {
            result_string_type = STRING_SHARED;
            result = ref_string(dest);
        }
        else
        {
            result_string_type = STRING_MALLOC;
            result = string_copy(dest);
        }
    }
    else if (srcstart == 0 && deststart == 0
          && copylen >= destlen && copylen >= srclen)
    {
        if (sp[-2].x.string_type == STRING_SHARED)
        {
            result_string_type = STRING_SHARED;
            result = ref_string(src);
        }
        else
        {
            result_string_type = STRING_MALLOC;
            result = string_copy(src);
        }
    }
    else
    {
        p_int destendlen;   /* Length of the end part of dest to copy */
        p_int srccopylen;   /* Actual number of bits to copy from src */

        /* Get the length to copy and the length of the result */
        srccopylen = copylen;
        if (srcstart >= srclen - copylen)
            srccopylen = srclen - srcstart;

        if (PINT_MAX - copylen < deststart)
            error("copy_bits: result length exceeds numerical limit: %ld + %ld\n"
                 , (long)deststart, (long)copylen
                 );
        resultlen = deststart + copylen;
        if (resultlen < destlen)
            resultlen = destlen;
        if (resultlen > MAX_BITS || resultlen < 0)
            error("copy_bits: Result too big: %lu bits\n"
                 , (unsigned long)resultlen);

        destendlen = destlen - (deststart + copylen);

        /* Get the result string and store the reference on the stack
         * for error cleanups.
         */
        result_string_type = STRING_MALLOC;
        result = xalloc((resultlen + 5) / 6 + 1);
        if (result == NULL)
        {
            error("Out of memory.\n");
            /* NOTREACHED */
        }
        memset(result, ' ', (resultlen + 5) / 6);
        result[(resultlen + 5) / 6] = '\0';
        inter_sp = sp; put_malloced_string(sp, result);

        /* Copy the first part of dest into the result */
        if (deststart > 0)
        {
            if (deststart > destlen)
                copy_bits(result, 0, dest, 0, destlen);
            else
                copy_bits(result, 0, dest, 0, deststart);
        }

        /* Copy the source part into the result */
        copy_bits(result, deststart, src, srcstart, srccopylen);

        /* Copy the remainder of dest into the result */
        if (destendlen > 0)
        {
            copy_bits( result, deststart + copylen
                     , dest, deststart + copylen
                     , destendlen);
        }
    }

    /* Clean up the stack and push the result */
    sp--; /* The result reference for error cleanup */
    free_svalue(sp--);
    free_svalue(sp);

    put_string(sp, result); sp->x.string_type = result_string_type;
    return sp;
} /* f_copy_bits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_make_shared_string (svalue_t *sp)

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

/*--------------------------------------------------------------------*/
svalue_t *
f_md5(svalue_t *sp)

/* TEFUN: md5()
 *
 *   string md5(string arg)
 *
 * Create and return a MD5 message digest from the string <arg>.
 */

{
    MD5_CTX context;
    unsigned char *digest, d[17];
    int i;

    TYPE_TEST1(sp, T_STRING);

    xallocate(digest, 33, "md5 result");

    MD5Init(&context);
    MD5Update(&context, (unsigned char *)sp->u.string, svalue_strlen(sp));
    MD5Final(&context, d);

    d[16]='\0';

    for (i = 0; i < 16; i++)
        sprintf((char *)digest+2*i, "%02x", d[i]);

    digest[32] = '\0';
    free_string_svalue(sp);
    put_malloced_string(sp, (char *)digest);

    return sp;
} /* f_md5() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_trim (svalue_t *sp, int num_arg)

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
    svalue_t * argp;
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

        newlen = (size_t)(right - left);
        xallocate(trimmed, newlen+1, "trimmed result");
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
svalue_t *
f_upper_case (svalue_t *sp)

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
                if (islower((unsigned char)c))
                    *s = (char)toupper((unsigned char)c);
            }
        }
        else
        {
            /* Other strings must be duplicated and then changed */
            xallocate(str, svalue_strlen(sp)+1, "uppercase result");

            initial_len = s - sp->u.string;
            /* Copy the initial part */
            if (initial_len)
                memcpy(str, sp->u.string, (size_t)initial_len);

            /* Copy and change the rest */
            for (d = str + initial_len; '\0' != (c = *s++) ; )
            {
                if (islower((unsigned char)c))
                    c = (char)toupper((unsigned char)c);
                *d++ = c;
            }

            *d = '\0';
            free_string_svalue(sp);
            put_malloced_string(sp, str);
        }
    }

    /* That's it */
    return sp;
} /* f_upper_case() */

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
    if (z + 1 < lens[i])
        return MY_FALSE;
    for (i++; i < imax; i++) {
        if (lens[i] > 0)
            return MY_FALSE;
    }
    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
char *
e_terminal_colour ( char * text, mapping_t * map, svalue_t *cl
                  , int indent, int wrap
                  )

/* EFUN terminal_colour()
 *
 *   string terminal_colour (string text, mapping|closure map
 *                          , int wrap, int indent )
 *
 * Expands all colour-defines from the input-string and replaces them by the
 * apropriate values found for the color-key inside the given mapping. The
 * mapping has the format "KEY" : "value", non-string contents are ignored
 * with one exception: the entry (0 : value) is used for otherwise
 * unrecognized tags, if existing; <value> may be a string or a closure (see
 * below).
 *
 * If <map> is given as 0, no keyword detection or replacement will be
 * performed and the efun acts just as a text wrapper and indenter (assuming
 * that <wrap> and <indent> are given).
 *
 * If <map> is given as a closure, it is called for each KEY with the key
 * as argument, and it has to return the replacement string.
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
 * TODO: Instead of computing the wrapping twice, the first pass
 * TODO:: should record what to break where.
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
    svalue_t * mdata_save = NULL;
      /* Pointer into an array on the stack, pointing to the next
       * free entry.
       * The array is used to keep copies of the replacement string
       * svalues to make sure that the strings exist as long as we
       * need them.
       * By keeping the array itself on the stack, cleanup is automatic.
       */
    int num_tmp;           /* Number of temporary svalues on the stack */
    int k;                 /* Index within a string */
    int col;               /* Current print column */
    int j;                 /* Accumulated total length of result */
    int j_extra;           /* Temporary extra length of result before fmt'ing */
    int start;             /* Col of first non-blank character */
    int space;             /* Col of last space char */
    int i;
    Bool maybe_at_end;     /* TRUE if the next text might start a new line */
    Bool no_keys;          /* TRUE if no delimiter in the string */
    Bool indent_overflows;
      /* Used to catch this boundary condition:
       *   t_c("\\/ "*32, 0, indent > MAX_STRING_LENGTH - 40, 40)
       * In this case, the last indent is followed by no data, which the
       * data copying part notices, but not the previous length calculation
       * part.
       * Set to TRUE in the length calculation when the possibility arises.
       */

    if (wrap && indent > wrap)
    {
        error("(terminal_colour) indent %ld > wrap %ld\n"
             , (long)indent, (long)wrap);
        /* NOTREACHED */
        return NULL;
    }

    instr = text;
    num_tmp = 0;

    /* Find the first occurance of the magic character pair.
     * If found, duplicate the input string into instr and
     * let cp point into that copy at the delimiter.
     * If not found (or no mapping/closure given), cp will be NULL.
     */
    if (map != NULL || cl != NULL)
    {
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

                    /* Check for the special escape '%%^^'.
                     * If found, modify it to '%^%^, and let cp
                     * point to it.
                     */
                    if (cp > savestr
                     && cp[-1] == TC_FIRST_CHAR
                     && cp[2] == TC_SECOND_CHAR
                       )
                    {
                        cp--;
                        cp[1] = TC_SECOND_CHAR;
                        cp[2] = TC_FIRST_CHAR;
                    }
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

        /* If we got a mapping, do a one-time lookup for the default
         * entry and store it in <cl>.
         */
        if (map != NULL)
        {
            cl = get_map_value(map, &const0);
            if (cl->type == T_NUMBER && cl->u.number == 0)
                cl = NULL; /* No default entry */

            if (cl && cl->type != T_STRING && cl->type != T_CLOSURE)
            {
                error("(terminal_colour) Illegal type for default entry.\n");
                /* NOTREACHED */
                return text;
            }
        }

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

        /* The string by definition starts with a non-keyword,
         * which might be empty.
         * Initialize our variables accordingly.
         */
        num = 1;
        parts[0] = instr;
        *cp = '\0';

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
                    {
                        /* Check for the special escape '%%^^'.
                         * If found, modify it to '%^%^, and let cp
                         * point to it.
                         */
                        if (cp > savestr
                         && cp[-1] == TC_FIRST_CHAR
                         && cp[2] == TC_SECOND_CHAR
                           )
                        {
                            cp--;
                            cp[1] = TC_SECOND_CHAR;
                            cp[2] = TC_FIRST_CHAR;
                        }
                        break;
                    }
                    cp++;
                }
            } while (cp);

            if (cp)
            {
                /* Another delimiter found: put it into the parts array.
                 */
                *cp = '\0';
                parts[num] = instr;
                num++;
                if (num % NSTRSEGS == 0)
                    parts = RESIZE(parts, num + NSTRSEGS, char * );
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

    /* If required, allocate the mdata save array on the stack */
    if (!no_keys)
    {
        vector_t *vec;

        vec = allocate_array_unlimited(num/2 + 1);
          /* Slightly bigger than required */
        mdata_save = vec->item;
        inter_sp++; put_array(inter_sp, vec);
        num_tmp++;
    }

    /* Do the the keyword replacement and calculate the lengths.
     * The lengths are collected in the lens[] array to save the
     * need for repeated strlens().
     */
    for (i = 0; i < num; i++)
    {
        long len;
        char * str;
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
            if (parts[i][0] == '\0') /* Empty key - already handled */
                str = NULL;
            else
                str = findstring(parts[i]);
            if (str != NULL && map != NULL)
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
                if (mdata->type == T_NUMBER && mdata->u.number == 0)
                    mdata = NULL; /* No entry */
            }

            /* If the map lookup didn't find anything, try the
             * <cl>osure (which might be the default entry)
             */
            if (mdata == NULL && cl != NULL && parts[i][0] != '\0')
            {
                if (cl->type == T_STRING)
                {
                    mdata = cl;
                }
                else
                {
                    /* It's a closure.
                     * We keep the result in the array on the stack
                     * to make sure it lives until we are done processing it.
                     */
                    push_volatile_string(parts[i]);
                    call_lambda(cl, 1);
                    *mdata_save = *inter_sp;
                    inter_sp--;
                    mdata = mdata_save++;
                    if (mdata->type != T_STRING)
                    {
                        error("(terminal_colour) Closure did not return a string.\n");
                        /* NOTREACHED */
                        return NULL;
                    }
                }
            }
        }
        else if (!(i % 2) && !no_keys
              && i < num -1 && parts[i+1][0] == '\0')
        {
            /* Special case: the following colour key is the empty "%^%^".
             * We interpret it as literal "%^" and add it to this part.
             * Both part[i] and part[i+1] will end with the same '\0'.
             */
            parts[i+1][-2] = TC_FIRST_CHAR;
        }

        /* If mdata found a string, use it instead of the old parts[i].
         * Note its length, making it negative where necessary.
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
    } /* for (i = 0..num) for length gathering */


    /* Do the wrapping analysis.
     * In order to do this, we need to have all lengths already
     * available.
     */
    col = 0;
    start = -1;
    space = 0;
    maybe_at_end = MY_FALSE;
    indent_overflows =  MY_FALSE;
    j = 0; /* gathers the total length of the final string */
    j_extra = 0; /* gathers the extra length needed during fmt'ing */
    for (i = 0; i < num; i++)
    {
        long len;

        len = lens[i];

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
                                /* Break the line at the last space. */
                                int next_word_len = 0;

                                if (col - space > 2)
                                {
                                    /* Check if the current word is too
                                     * long to be put on one line. If it
                                     * is, don't bother breaking at the last
                                     * space.
                                     */
                                    int test_z = z;
                                    int test_i = i;
                                    Bool done = MY_FALSE;

                                    next_word_len = col - space;
                                    for ( ; !done && test_i <  num; test_i++)
                                    {
                                        if (lens[test_i] < 0)
                                            continue;
                                        for ( ; !done && test_z < lens[test_i]
                                              ; test_z++)
                                        {
                                            char testc = parts[test_i][test_z];
                                            if (testc == ' ' || testc == '\n')
                                            {
                                                done = MY_TRUE;
                                                break;
                                            }
                                            next_word_len++;
                                        }
                                        test_z = 0;
                                    }
                                }

                                if (next_word_len+indent > wrap)
                                {
                                    /* Word is too long, just treat it
                                     * as if there is no space within range.
                                     */
                                    space = 0;
                                    j++;
                                    col = 1;
                                }
                                else
                                {
                                    /* It makes sense to break properly */
                                    col -= space;
                                    space = 0;
                                }
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
                    {
                        maybe_at_end = MY_TRUE;
                    }

                    /* Guard against overflow */
                    if (j > MAX_STRING_LENGTH)
                    {
                        /* Reduce this part to fit; all the following
                         * parts will be reduced to shreds.
                         */
                        indent_overflows = MY_TRUE;
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
            indent_overflows = MY_FALSE;
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
    } /* for (i = 0..num) for wrapping analysis */


    /* Now we have the final string in parts and length in j.
     * let's compose the result, wrapping it where necessary.
     */
    xallocate(deststr, (size_t)(j+1), "result string");

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
            int l = lens[i];     /* Length of current part */
            char *p = parts[i];  /* Current part */

            if (pt - tmpmem + ((l < 0) ? -l : l) >= tmpmem_size)
            {
                error("Partial string '%s' too long (%ld+%ld >= %ld).\n"
                     , p
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
                            int next_word_len = 0;

                            if (col - space > 2)
                            {
                                /* Check if the following word is too
                                 * long to be put on one line. If it
                                 * is, don't bother breaking at the last
                                 * space.
                                 */
                                int test_k = k;
                                int test_i = i;
                                Bool done = MY_FALSE;

                                next_word_len = col - space;
                                for ( ; !done && test_i <  num; test_i++)
                                {
                                    if (lens[test_i] < 0)
                                        continue;
                                    for ( ; !done && test_k < lens[test_i]
                                          ; test_k++)
                                    {
                                        char testc = parts[test_i][test_k];
                                        if (testc == ' ' || testc == '\n')
                                        {
                                            done = MY_TRUE;
                                            break;
                                        }
                                        next_word_len++;
                                    }
                                    test_k = 0;
                                }
                            }

                            if (next_word_len + indent > wrap)
                            {
                                /* Word is too long: treat it as if there
                                 * is no space within range.
                                 */
                                space = 0;
                                col = 1;
                                kind = 2;
                            }
                            else
                            {
                                col -= space;
                                space = 0;
                                kind = 1;
                            }
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
                } /* if (type of c) */

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

                /* Since we're in a new line, all the 'garbage' is gone. */
                space_garbage = 0;
            } /* for(k = 0.. lens[i] */
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

    /* Terminate the string */
    *cp = '\0';

    if ( lens )
      xfree(lens);
    if ( parts )
      xfree(parts);
    if (savestr)
      xfree(savestr);
    while (num_tmp > 0)
    {
        free_svalue(inter_sp);
        inter_sp--;
        num_tmp--;
    }

    /* now we have what we want */
#ifdef DEBUG
    if (cp - deststr != j
     && (!indent_overflows || cp - deststr != wrap)
       ) {
      fatal("Length miscalculated in terminal_colour()\n"
            "    Expected: %i (or %i) Was: %ld\n"
            "    In string: %s\n"
            "    Out string: %s\n"
            "    Indent: %i Wrap: %i, indent overflow: %s\n"
           , j, wrap
           , (long)(cp - deststr), text, deststr, indent, wrap
           , indent_overflows ? "true" : "false");
    }
#endif
    return deststr;

#undef CALLOCATE
#undef RESIZE
#undef NSTRSEGS
#undef TC_FIRST_CHAR
#undef TC_SECOND_CHAR
} /* e_terminal_colour() */

#ifdef F_PROCESS_STRING
/*-------------------------------------------------------------------------*/
static char *
process_value (char *str)

/* Helper function for process_string(): take a function call in <str>
 * in the form "function[:objectname]{|arg}" and try to call it.
 * If the function exists and returns a string, the result is a pointer
 * to the string, which must be copied immediately.
 * If the function can't be called, or does not return a string, the
 * result is NULL.
 */

{
    svalue_t *ret;     /* Return value from the function call */
    char     *func;    /* Copy of the <str> string for local modifications */
    char     *func2;   /* Shared string with the function name from <func> */
    char     *obj;     /* NULL or points to the object part in <func> */
    char     *arg;     /* NULL or points to the first arg in <func> */
    char     *narg;    /* Next argument while pushing them */
    int       numargs; /* Number of arguments to the call */
    object_t *ob;

    /* Simple check if the argument is valid */
    if (strlen(str) < 1 || !isalpha((unsigned char)(str[0])))
        return NULL;

    /* Copy the argument so that we can separate the various
     * parts with \0 characters.
     */
    func = string_copy(str);

    /* Find the object and the argument part */
    arg = strchr(func,'|'); if (arg) { *arg='\0'; arg++; }
    obj = strchr(func,':'); if (obj) { *obj='\0'; obj++; }

    /* Check if the function exists at all. apply() will be delighted
     * over the shared string anyway.
     */
    if ( NULL == (func2 = findstring(func)) )
    {
        xfree(func);
        return NULL;
    }


    /* Get the object */
    if (!obj)
        ob = current_object;
    else
        ob = find_object(obj);

    if (!ob)
    {
        xfree(func);
        return NULL;
    }

    /* Push all arguments as strings to the stack
     */
    for (numargs = 0; arg; arg = narg)
    {
        narg = strchr(arg,'|');
        if (narg)
            *narg = '\0';
        push_string_malloced(arg);
        numargs++;
        if (narg)
        {
            *narg = '|';
            narg++;
        }
    }

    /* We no longer need this */
    xfree(func);

    /* Apply the function and see if adequate answer is returned.
     */
    ret = apply(func2, ob, numargs);

    if (ret && ret->type == T_STRING)
        return ret->u.string;
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
 * TODO:: arguments. Basically using the new explode solves the problem.
 * TODO:: public string process_string(string str)
 * TODO:: {
 * TODO::   int il;
 * TODO::   string * parts;
 * TODO::   string tmp;
 * TODO::   parts = explode(str, "@@");
 * TODO::   for ( il = 1; il < sizeof(parts); il += 2) {
 * TODO::     tmp = process_value(parts[il]);
 * TODO::     if (stringp(tmp))
 * TODO::       parts[il] = tmp;
 * TODO::   }
 * TODO::   return implode(parts, "");
 * TODO:: }
 */

{
    vector_t   *vec;           /* Arg string exploded by '@@' */
    object_t   *old_cur;       /* Old current object */
    wiz_list_t *old_eff_user;  /* Old euid */
    int         pr_start;      /* Index of the first pr-spec in vec */
    int         il;            /* Index in vec */
    Bool        changed;       /* True if there was a replacement */
    Bool        ch_last;       /* True if the last vec-entry was replaced */
    char *p0, *p1, *p2;
    char *buf;                 /* Result string(s) */
    char *str;                 /* The argument string */

    TYPE_TEST1(sp, T_STRING);
    str = sp->u.string;

    if (!str || !(p1 = strchr(str,'@')))
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
        ret = apply_master(STR_GET_BB_UID,0);
        if (!ret)
            return sp;

        if (ret->type != T_STRING
         && (strict_euids || ret->type != T_NUMBER || ret->u.number))
            return sp;

        if (current_object->eff_user)
        {
            old_eff_user = current_object->eff_user;
            if (ret->type == T_STRING)
                current_object->eff_user = add_name(ret->u.string);
            else
                current_object->eff_user = NULL;
        }
    }

    /* Explode the argument by the '@@' */
    /* TODO: Rewrite to use new explode_string() */
    vec = old_explode_string(str,"@@");
    if (!vec)
        return sp;
    push_referenced_vector(vec); /* automatic free in case of errors */

    pr_start = ((str[0]=='@') && (str[1]=='@')) ? 0 : 1;

    for ( ch_last = MY_FALSE, changed = MY_FALSE, il = pr_start
        ; (size_t)il < VEC_SIZE(vec)
        ; il++)
    {
        p0 = vec->item[il].u.string;

        /* Try to interpret the entry as function call.
         * If that succeeds, hold the result (freshly allocated) in p2.
         */
        p1 = strchr(p0, ' ');
        if (!p1)
        {
            /* No space, the whole entry might be a function call */
            p2 = process_value(p0);
            if (p2)
            {
                /* Yup, it is: copy the result */
                p2 = string_copy(p2);
                ch_last = MY_TRUE;
            }
        }
        else
        {
            /* There is a space: just interpret the characters before
             * as possible function call.
             */
            size_t len;

            len = (size_t)(p1 - p0);
            buf = xalloc(len + 1);
            strncpy(buf, p0, len);
            buf[len] = '\0';
            p2 = process_value(buf);
            if (p2)
            {
                /* We got a result: join it with the remains after the
                 * space and put it into p2.
                 */
                char * tmp;

                len = strlen(p2);
                tmp = xalloc(len + strlen(p1) + 1);
                strcpy(tmp,p2);
                strcpy(tmp+len,p1);
                p2 = tmp;
            }
            xfree(buf);
        }

        if (!p2)
        {
            /* No replacement by function call */
            if (!ch_last)
            {
                /* ...but we have to recreate the '@@' from the original */
                p2 = xalloc(3+strlen(p0));
                strcpy(p2,"@@");
                strcpy(p2+2,p0);
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
            xfree(p0);
            vec->item[il].u.string = p2;
        }
    } /* for() */

    /* If there were changes, implode the vector again */
    if (changed)
        buf = implode_string(vec, "");
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
        put_malloced_string(sp, buf);
    }

    return sp;
} /* f_process_string() */

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
            /* It's another %-spec: try to find its match within the
             * <str> by attempting the match at one character after the
             * other.
             */
            for (fmt -= 2; *str; str++)
            {
                sscanf_match(str, fmt, info);

                /* If the sequence was '%s%d', the '%d' has to match
                 * on the first try, otherwise all will be assigned to
                 * the '%s'.
                 */
                if (b == 'd' && info->match_end == str)
                    return str + strlen(str);

                /* If we found a match at the current position of str,
                 * the '%s' ends here and the next match starts.
                 */
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
                    xallocate(match, (size_t)num+1, "matchstring");
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
svalue_t *
x_all_environment (svalue_t *sp, int numarg)

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
    object_t *o;

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
        object_t *env;
        vector_t *v;
        svalue_t *svp;

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
} /* x_all_environment() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_blueprint (svalue_t *sp)

/* EFUN blueprint()
 *
 *   object blueprint ()
 *   object blueprint (string|object ob)
 *
 * The efuns returns the blueprint for the given object <ob>, or for
 * the current object if <ob> is not specified.
 *
 * If the blueprint is destructed, the efun returns 0.
 * For objects with replaced programs, the efun returns the blueprint
 * for the replacement program.
 */

{
    object_t * obj, * blueprint;

    if (sp->type == T_OBJECT)
        obj = sp->u.ob;
    else if (sp->type == T_STRING)
    {
        obj = get_object(sp->u.string);
        if (!obj)
        {
            error("Object not found: %s\n", sp->u.string);
            /* NOTREACHED */
            return sp;
        }
    }
    else
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }

    if ((obj->flags & O_SWAPPED) && load_ob_from_swap(obj) < 0)
        error("Out of memory: unswap object '%s'.\n", obj->name);

    blueprint = NULL;
#ifndef NO_BLUEPRINT
    if (obj->prog != NULL
     && obj->prog->blueprint != NULL
     && !(obj->prog->blueprint->flags & O_DESTRUCTED)
       )
        blueprint = ref_object(obj->prog->blueprint, "blueprint()");
#endif /* !NO_BLUEPRINT */

    free_svalue(sp);
    if (blueprint != NULL)
        put_object(sp, blueprint);
    else
        put_number(sp, 0);

    return sp;
} /* f_blueprint() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_clones (svalue_t *sp, int num_arg)

/* VEFUN clones()
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
 *
 * If the driver is compiled with DYNAMIC_COSTS, the cost of this
 * efun is proportional to the number of objects in the game.
 */

{
    char      *name;     /* The load-name to search */
    mp_int     mintime;  /* 0 or lowest load_time for an object to qualify */
    mp_int     maxtime;  /* 0 or highest load_time for an object to qualify */
    mp_int     load_id;  /* The load_id of the reference */
    object_t **ores;     /* Table pointing to the found objects */
    size_t     found;    /* Number of objects found */
    size_t     checked;  /* Number of objects checked */
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
                reference = get_object(sp->u.string);
                if (!reference) {
                    error("Object not found: %s\n", sp->u.string);
                    /* NOTREACHED */
                    return sp;
                }
            }
            else if (sp->type == T_NUMBER) {
                what = sp->u.number;
                if (what < 0 || what > 2) {
                    bad_xefun_vararg(1, sp);
                    /* NOTREACHED */
                    return sp;
                }
            }
            else {
                bad_xefun_vararg(1, sp);
                /* NOTREACHED */
                return sp;
            }
        }
        else if (num_arg == 2)
        {
            if (sp->type == T_NUMBER) {
                what = sp->u.number;
                if (what < 0 || what > 2)
                {
                    bad_xefun_vararg(2, sp);
                    /* NOTREACHED */
                    return sp;
                }
            }
            else {
                bad_xefun_vararg(2, sp);
                /* NOTREACHED */
                return sp;
            }

            free_svalue(sp--); inter_sp = sp;

            if (sp->type == T_OBJECT)
                reference = sp->u.ob;
            else if (sp->type == T_STRING) {
                reference = get_object(sp->u.string);
                if (!reference)
                {
                    error("Object not found: %s\n", sp->u.string);
                    /* NOTREACHED */
                    return sp;
                }
            }
            else {
                bad_xefun_vararg(1, sp);
                /* NOTREACHED */
                return sp;
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
    checked = 0;
    xallocate(ores, sizeof(*ores) * osize, "initial object table");

    /* Loop through the object list */
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        checked++;

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

#if defined(DYNAMIC_COSTS)
    eval_cost += checked / 100 + found / 256;
#endif /* DYNAMIC_COSTS */

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
static object_t *
object_present_in (char *str, object_t *ob)

/* <ob> is the first object in an environment: test all the objects there
 * if they match the id <str>.
 * <str> may be of the form "<id> <num>" - then the <num>th object with
 * this <id> is returned, it it is found.
 */

{
    svalue_t *ret;
    char *p;
    int   count = 0; /* >0: return the <count>th object */
    int   length;
    char *item;

    length = strlen(str);
    xallocate(item, (size_t)length + 1, "work string");
    strcpy(item, str);
    push_malloced_string(item); /* free on error */

    /* Check if there is a number in the string */
    p = item + length - 1;
    if (*p >= '0' && *p <= '9')
    {
        while(p > item && *p >= '0' && *p <= '9')
            p--;

        if (p > item && *p == ' ')
        {
            count = atoi(p+1) - 1;
            *p = '\0';
        }
    }

    /* Now look for the object */
    for (; ob; ob = ob->next_inv)
    {
        push_volatile_string(item);
        ret = sapply(STR_ID, ob, 1);
        if (ob->flags & O_DESTRUCTED)
        {
            xfree(item);
            inter_sp--;
            return NULL;
        }

        if (ret == NULL || (ret->type == T_NUMBER && ret->u.number == 0))
            continue;

        if (count-- > 0)
            continue;
        xfree(item);
        inter_sp--;
        return ob;
    }
    xfree(item);
    inter_sp--;

    /* Not found */
    return NULL;
} /* object_present_in() */

/*-------------------------------------------------------------------------*/
object_t *
e_object_present (svalue_t *v, object_t *ob)

/* EFUN present()
 *
 *   object present(mixed str)
 *   object present(mixed str, object ob)
 *
 * If an object that identifies (*) to the name ``str'' is present
 * in the inventory or environment of this_object (), then return
 * it. If "str" has the form "<id> <n>" the <n>-th object matching
 * <id> will be returned.
 *
 * "str" can also be an object, in which case the test is much faster
 * and easier.
 *
 * A second optional argument ob is the enviroment where the search
 * for str takes place. Normally this_player() is a good choice.
 * Only the inventory of ob is searched, not its environment.
 * TODO: Make this a nice efuns.c-Efun and also implement
 * TODO:: deep_present() and present_clone() (see bugs/f-something)
 */

{
    svalue_t *ret;
    object_t *ret_ob;
    Bool specific = MY_FALSE;

    /* Search where? */
    if (!ob)
        ob = current_object;
    else
        specific = MY_TRUE;

    if (ob->flags & O_DESTRUCTED)
        return NULL;

    if (v->type == T_OBJECT)
    {
        /* Oooh, that's easy. */

        if (specific)
        {
            if (v->u.ob->super == ob)
                return v->u.ob;
            else
                return NULL;
        }
        if (v->u.ob->super == ob
         || (v->u.ob->super == ob->super && ob->super != 0))
            return v->u.ob;
        return NULL;
    }

    /* Always search in the object's inventory */
    ret_ob = object_present_in(v->u.string, ob->contains);
    if (ret_ob)
        return ret_ob;

    if (specific)
        return NULL;

    /* Search in the environment of <ob> if it was not specified */
    if (!specific && ob->super)
    {
        /* Is it _the_ environment? */
        push_volatile_string(v->u.string);
        ret = sapply(STR_ID, ob->super, 1);
        if (ob->super->flags & O_DESTRUCTED)
            return NULL;
        if (ret && !(ret->type == T_NUMBER && ret->u.number == 0))
            return ob->super;

        /* No, search the other objects here. */
        return object_present_in(v->u.string, ob->super->contains);
    }

    /* Not found */
    return NULL;
} /* e_object_present() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_object_info (svalue_t *sp, int num_args)

/* TEFUN object_info()
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

    /* Test and get the arguments from the stack */
    argp = sp - num_args + 1;
    TYPE_TESTV1(argp, T_OBJECT)
    TYPE_TESTV2(argp+1, T_NUMBER)
    if (num_args == 3)
    {
        TYPE_TESTV3(argp+2, T_NUMBER)
        value = argp[2].u.number;
        assign_svalue_no_free(&result, &const0);
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
        put_malloced_string(svp+which, code); \
    } else if (value == which) { \
        put_malloced_string(svp, code); \
    } else {}

#define ST_RSTRING(which,code) \
    if (value == -1) { \
        put_ref_string(svp+which, code); \
    } else if (value == which) { \
        put_ref_string(svp, code); \
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
            ST_STRING(OIB_NAME, string_copy(o->name));
        }
        else
        {
            ST_STRING(OIB_NAME, add_slash(o->name));
        }

        ST_RSTRING(OIB_LOAD_NAME, o->load_name);

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
            error("Out of memory: unswap object '%s'.\n", o->name);

        prog = o->prog;

        ST_NUMBER(OIM_REF, prog->ref);

        ST_STRING(OIM_NAME, string_copy(prog->name));

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

        ST_NUMBER(OIM_NUM_INCLUDES, prog->num_includes);
        {
            int i = prog->num_inherited;
            int cnt = 0;
            inherit_t *inheritp;

            for (inheritp = prog->inherit; i--; inheritp++)
            {
                if (inheritp->inherit_type == INHERIT_TYPE_NORMAL
                 || inheritp->inherit_type == INHERIT_TYPE_VIRTUAL
                   )
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
}

/*-------------------------------------------------------------------------*/
svalue_t *
f_present_clone (svalue_t *sp)

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
    object_t *obj;  /* the object under scrutiny */

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
                    strncpy(name0, sp[-1].u.string, (size_t)i);
                    name0[i] = '\0';
                }

                break; /* in any case */
            }
        }

        /* Now make the name sane */
        name = (char *)make_name_sane(name0, !compat_mode);

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
svalue_t *
f_to_object (svalue_t *sp)

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
    object_t *o;

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
        if (n == CLOSURE_EFUN + F_UNDEF)
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
#ifdef F_TRANSFER

svalue_t *
f_transfer (svalue_t *sp)

/* TEFUN transfer()
 *
 *   int transfer(object item, object dest)
 *
 * This efun is for backward compatibility only. It is only
 * available in compat mode.
 *
 * Move the object "item" to the object "dest". All kinds of
 * tests are done, and a number is returned specifying the
 * result:
 *
 *     0: Success.
 *     1: To heavy for destination.
 *     2: Can't be dropped.
 *     3: Can't take it out of it's container.
 *     4: The object can't be inserted into bags etc.
 *     5: The destination doesn't allow insertions of objects.
 *     6: The object can't be picked up.
 *
 * If an object is transfered to a newly created object, make
 * sure that the new object first is transfered to it's
 * destination.
 *
 * The efun calls add_weight(), drop(), get(), prevent_insert(),
 * add_weight(), and can_put_and_get() where needed.
 */

{
    object_t *ob, *to;
    svalue_t *v_weight, *ret;
    int       weight;
    object_t *from;
    int       result;

    /* Get and test the arguments */
    if (sp[-1].type != T_OBJECT)
    {
        bad_xefun_arg(1, sp);
    }
    ob = sp[-1].u.ob;

    if (sp->type == T_OBJECT)
        to = sp->u.ob;
    else if (sp->type == T_STRING)
    {
        to = get_object(sp->u.string);
        if (!to)
            error("Object %s not found.\n", sp->u.string);
        free_string_svalue(sp);
        put_ref_object(sp, to, "transfer"); /* for move_object() below */
    }
    else
        bad_xefun_arg(2, sp);

    from = ob->super;
    result = 0; /* Default: success result */

    /* Perform the transfer step by step */
    switch(0){default:

        /* Get the weight of the object
         */
        weight = 0;
        v_weight = sapply(STR_QUERY_WEIGHT, ob, 0);
        if (v_weight && v_weight->type == T_NUMBER)
            weight = v_weight->u.number;

        if (ob->flags & O_DESTRUCTED)
        {
            result = 3;
            break;
        }

        /* If the original place of the object is a living object,
         * then we must call drop() to check that the object can be dropped.
         */
        if (from && (from->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_DROP, ob, 0);
            if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            {
                result = 2;
                break;
            }

            /* This should not happen, but we can not trust LPC hackers. :-) */
            if (ob->flags & O_DESTRUCTED)
            {
                result = 2;
                break;
            }
        }

        /* If 'from' is not a room and not a player, check that we may
         * remove things out of it.
         */
        if (from && from->super && !(from->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_CANPUTGET, from, 0);
            if (!ret
             || (ret->type == T_NUMBER && ret->u.number == 0)
             || (from->flags & O_DESTRUCTED))
            {
                result = 3;
                break;
            }
        }

        /* If the destination is not a room, and not a player,
         * Then we must test 'prevent_insert', and 'can_put_and_get'.
         */
        if (to->super && !(to->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_PREVENT_INSERT, ob, 0);
            if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            {
                result = 4;
                break;
            }

            ret = sapply(STR_CANPUTGET, to, 0);
            if (!ret
             || (ret->type == T_NUMBER && ret->u.number == 0)
             || (to->flags & O_DESTRUCTED)
             || (ob->flags & O_DESTRUCTED))
            {
                result = 5;
                break;
            }
        }

        /* If the destination is a player, check that he can pick it up.
         */
        if (to->flags & O_ENABLE_COMMANDS)
        {
            ret = sapply(STR_GET, ob, 0);
            if (!ret
             || (ret->type == T_NUMBER && ret->u.number == 0)
             || (ob->flags & O_DESTRUCTED))
            {
                result = 6;
                break;
            }
        }

        /* If it is not a room, correct the total weight in
         * the destination.
         */
        if (to->super && weight)
        {
            /* Check if the destination can carry that much.
             */
            push_number(weight);
            ret = sapply(STR_ADD_WEIGHT, to, 1);
            if (ret && ret->type == T_NUMBER && ret->u.number == 0)
            {
                result = 1;
                break;
            }

            if (to->flags & O_DESTRUCTED)
            {
                result = 1;
                break;
            }
        }

        /* If it is not a room, correct the weight in
         * the 'from' object.
         */
        if (from && from->super && weight)
        {
            push_number(-weight);
            (void)sapply(STR_ADD_WEIGHT, from, 1);
        }

        /* When we come here, the move is ok */
    } /* pseudo-switch() */

    if (result)
    {
        /* All the applys might have changed these */
        free_svalue(sp);
        free_svalue(sp-1);
    }
    else
    {
        /* The move is ok: do it (and use up both arguments) */
        inter_sp = sp;
        move_object();
    }

    put_number(sp-1, result);
    return sp-1;
} /* f_transfer() */

#endif /* F_TRANSFER */

/*-------------------------------------------------------------------------*/
void
e_say (svalue_t *v, vector_t *avoid)

/* Implementation of the EFUN say() (see interpret.c for the
 * full manpage).
 * <v> is the value to say, <avoid> the array of objects to exclude.
 * If the first element of <avoid> is not an object, the function
 * will store its command_giver object into it.
 */

{
    static svalue_t ltmp = { T_POINTER };
    static svalue_t stmp = { T_OBJECT };

    object_t *ob;
    object_t *save_command_giver = command_giver;
    object_t *origin;
    char buff[256];
    char *message;
#define INITIAL_MAX_RECIPIENTS 48
    int max_recipients = INITIAL_MAX_RECIPIENTS;
      /* Current size of the recipients table.
       */
    object_t *first_recipients[INITIAL_MAX_RECIPIENTS];
      /* Initial table of recipients.
       */
    object_t **recipients = first_recipients;
      /* Pointer to the current table of recipients.
       * The end is marked with a NULL entry.
       */
    object_t **curr_recipient = first_recipients;
      /* Next recipient to enter.
       */
    object_t **last_recipients =
                 &first_recipients[INITIAL_MAX_RECIPIENTS-1];
      /* Last entry in the current table.
       */
    object_t *save_again;

    /* Determine the command_giver to use */
    if (current_object->flags & O_ENABLE_COMMANDS)
    {
        command_giver = current_object;
    }
    else if (current_object->flags & O_SHADOW
          && O_GET_SHADOW(current_object)->shadowing)
    {
        command_giver = O_GET_SHADOW(current_object)->shadowing;
    }

    /* Determine the originating object */
    if (command_giver)
    {
        interactive_t *ip;

        if (O_SET_INTERACTIVE(ip, command_giver))
        {
            trace_level |= ip->trace_level;
        }
        origin = command_giver;

        /* Save the commandgiver to avoid, if needed */
        if (avoid->item[0].type == T_NUMBER)
        {
            put_ref_object(avoid->item, command_giver, "say");
        }
    }
    else
        origin = current_object;

    /* Sort the avoid vector for fast lookups
     */
    ltmp.u.vec = avoid;
    avoid = order_alist(&ltmp, 1, 1);
    push_referenced_vector(avoid); /* in case of errors... */
    avoid = avoid->item[0].u.vec;

    /* Collect the list of propable recipients.
     * First, look in the environment.
     */
    if ( NULL != (ob = origin->super) )
    {
        interactive_t *ip;

        /* The environment itself? */
        if (ob->flags & O_ENABLE_COMMANDS
         || O_SET_INTERACTIVE(ip, ob))
        {
            *curr_recipient++ = ob;
        }

        for (ob = ob->contains; ob; ob = ob->next_inv)
        {
            if (ob->flags & O_ENABLE_COMMANDS
             || O_SET_INTERACTIVE(ip,ob))
            {
                if (curr_recipient >= last_recipients)
                {
                    /* Increase the table */
                    max_recipients *= 2;
                    curr_recipient = alloca(max_recipients * sizeof(object_t *));
                    memcpy( curr_recipient, recipients
                           , max_recipients * sizeof(object_t *) / 2);
                    recipients = curr_recipient;
                    last_recipients = &recipients[max_recipients-1];
                    curr_recipient += (max_recipients / 2) - 1;
                }
                *curr_recipient++ = ob;
            }
        } /* for() */
    } /* if(environment) */

    /* Now check this environment */
    for (ob = origin->contains; ob; ob = ob->next_inv)
    {
        interactive_t *ip;

        if (ob->flags & O_ENABLE_COMMANDS
         || O_SET_INTERACTIVE(ip, ob))
        {
            if (curr_recipient >= last_recipients)
            {
                /* Increase the table */
                max_recipients *= 2;
                curr_recipient = alloca(max_recipients * sizeof(object_t *));
                memcpy( curr_recipient, recipients
                      , max_recipients * sizeof(object_t *) / 2);
                recipients = curr_recipient;
                last_recipients = &recipients[max_recipients-1];
                curr_recipient += (max_recipients / 2) - 1;
            }
            *curr_recipient++ = ob;
        }
    }

    *curr_recipient = NULL;  /* Mark the end of the list */

    /* Construct the message. */

    switch(v->type)
    {
    case T_STRING:
        message = v->u.string;
        break;

    case T_OBJECT:
        xstrncpy(buff, v->u.ob->name, sizeof buff);
        buff[sizeof buff - 1] = '\0';
        message = buff;
        break;

    case T_NUMBER:
        sprintf(buff, "%ld", v->u.number);
        message = buff;
        break;

    case T_POINTER:
        /* say()'s evil twin: send <v> to all recipients' catch_msg() lfun */

        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++) ; )
        {
            if (ob->flags & O_DESTRUCTED)
                continue;
            stmp.u.ob = ob;
            if (assoc(&stmp, avoid) >= 0)
                continue;
            push_vector(v->u.vec);
            push_object(origin);
            sapply(STR_CATCH_MSG, ob, 2);
        }
        pop_stack(); /* free avoid alist */
        command_giver = check_object(save_command_giver);
        return;

    default:
        error("Invalid argument %d to say()\n", v->type);
    }

    /* Now send the message to all recipients */

    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
    {
        interactive_t *ip;

        if (ob->flags & O_DESTRUCTED)
            continue;
        stmp.u.ob = ob;
        if (assoc(&stmp, avoid) >= 0)
            continue;
        if (!(O_SET_INTERACTIVE(ip, ob)))
        {
            tell_npc(ob, message);
            continue;
        }
        save_again = command_giver;
        command_giver = ob;
        add_message("%s", message);
        command_giver = save_again;
    }

    pop_stack(); /* free avoid alist */
    command_giver = check_object(save_command_giver);
} /* e_say() */

/*-------------------------------------------------------------------------*/
void
e_tell_room (object_t *room, svalue_t *v, vector_t *avoid)

/* Implementation of the EFUN tell_room() (see interpret.c for
 * the full manpage).
 *
 * Value <v> is sent to all living objects in <room>, except those
 * in <avoid>. <avoid> has to be in order_alist() order.
 */

{
    object_t *ob;
    object_t *save_command_giver;
    int num_recipients = 0;
    object_t *some_recipients[20];
    object_t **recipients;
    object_t **curr_recipient;
    char buff[256], *message;
    static svalue_t stmp = { T_OBJECT, } ;

    /* Like in say(), collect the possible recipients.
     * First count how many there are.
     */

    for (ob = room->contains; ob; ob = ob->next_inv)
    {
        interactive_t *ip;

        if ( ob->flags & O_ENABLE_COMMANDS
         ||  O_SET_INTERACTIVE(ip, ob))
        {
            num_recipients++;
        }
    }

    /* Allocate the table */
    if (num_recipients < 20)
        recipients = some_recipients;
    else
        recipients =
          alloca( (num_recipients+1) * sizeof(object_t *) );

    /* Now fill the table */
    curr_recipient = recipients;
    for (ob = room->contains; ob; ob = ob->next_inv)
    {
        interactive_t *ip;

        if ( ob->flags & O_ENABLE_COMMANDS
         ||  O_SET_INTERACTIVE(ip, ob))
        {
            *curr_recipient++ = ob;
        }
    }

    *curr_recipient = NULL; /* Mark the end of the table */

    /* Construct the message */
    switch(v->type)
    {
    case T_STRING:
        message = v->u.string;
        break;

    case T_OBJECT:
        xstrncpy(buff, v->u.ob->name, sizeof buff);
        buff[sizeof buff - 1] = '\0';
        message = buff;
        break;

    case T_NUMBER:
        sprintf(buff, "%ld", v->u.number);
        message = buff;
        break;

    case T_POINTER:
      {
        /* say()s evil brother: send <v> to all recipients'
         * catch_msg() lfun
         */
        object_t *origin = command_giver;

        if (!origin)
            origin = current_object;

        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
        {
            if (ob->flags & O_DESTRUCTED)
                continue;
            stmp.u.ob = ob;
            if (assoc(&stmp, avoid) >= 0)
                continue;
            push_vector(v->u.vec);
            push_object(origin);
            sapply(STR_CATCH_MSG, ob, 2);
        }
        return;
      }

    default:
        error("Invalid argument %d to tell_room()\n", v->type);
    }

    /* Now send the message to all recipients */

    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
    {
        interactive_t *ip;

        if (ob->flags & O_DESTRUCTED) continue;
        stmp.u.ob = ob;
        if (assoc(&stmp, avoid) >= 0) continue;
        if (!(O_SET_INTERACTIVE(ip, ob)))
        {
            tell_npc(ob, message);
            continue;
        }
        save_command_giver = command_giver;
        command_giver = ob;
        add_message("%s", message);
        command_giver = save_command_giver;
    }
} /* e_tell_room() */

/*-------------------------------------------------------------------------*/
#ifdef F_SET_IS_WIZARD

svalue_t *
f_set_is_wizard (svalue_t *sp)

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
svalue_t *
f_set_modify_command (svalue_t *sp)

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
    object_t *old, *new;
    interactive_t *ip;

    inter_sp = sp;

    /* Make sure the current_object is interactive */

    if (!(O_SET_INTERACTIVE(ip, current_object))
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

/*=========================================================================*/
/*                              VALUES                                     */

/*-------------------------------------------------------------------------*/
svalue_t *
f_copy (svalue_t *sp)

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
        vector_t *old, *new;
        size_t size, i;

        old = sp->u.vec;
        size = VEC_SIZE(old);
        if (old->ref != 1 && old != &null_vector)
        {
            DYN_ARRAY_COST(size);
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
            check_map_for_destr(old);
            DYN_MAPPING_COST(MAP_SIZE(old));
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
    int depth;                     /* Depth of the copy procedure */
    int width;                     /* width of the mapping */
    mapping_t * dest;              /* the mapping to copy into */
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

    copy_svalue(&newkey, key, info->ptable, info->depth);
    newdata = get_map_lvalue_unchecked(info->dest, &newkey);
    if (!newdata)
    {
        error("Out of memory.\n");
        /* NOTREACHED */
        return;
    }
    for (i = info->width; i-- > 0; newdata++, val++)
        copy_svalue(newdata, val, info->ptable, info->depth);

    free_svalue(&newkey); /* no longer needed */
}

/*-------------------------------------------------------------------------*/
static void
copy_svalue ( svalue_t *dest, svalue_t *src
            , struct pointer_table *ptable
            , int depth)

/* Copy the svalue <src> into the yet uninitialised svalue <dest>.
 * If <src> is an array or mapping, recurse to achieve a deep copy, using
 * <ptable> to keep track of the arrays and mappings encountered.
 * <depth> is the nesting depth of this value.
 *
 * The records in the pointer table store the svalue* of the created
 * copy for each registered array and mapping in the .data member.
 */

{
    assert_stack_gap();
    if (EVALUATION_TOO_LONG())
    {
        put_number(dest, 0); /* Need to store something! */
        return;
    }

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
            DYN_ARRAY_COST(size);
#if defined(DYNAMIC_COSTS)
            eval_cost += (depth+1) / 10;
#endif

            /* Create a new array, assign it to dest, and store
             * it in the table, too.
             */
            new = allocate_uninit_array(size);
            put_array(dest, new);
            if (src->type == T_QUOTED_ARRAY)
            {
                dest->type = T_QUOTED_ARRAY;
                dest->x.quotes = src->x.quotes;
            }
            rec->data = dest;

            /* Copy the values */
            for (i = 0; i < size; i++)
            {
                svalue_t * svp = &old->item[i];

                if (svp->type == T_MAPPING
                 || svp->type == T_POINTER
                 || svp->type == T_QUOTED_ARRAY
                   )
                    copy_svalue(&new->item[i], svp, ptable, depth+1);
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
            DYN_MAPPING_COST(size);
#if defined(DYNAMIC_COSTS)
            eval_cost += (depth+1) / 10;
#endif
            info.depth = depth+1;
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

    case T_QUOTED_ARRAY:
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
            copy_svalue(&new, sp, ptable, 0);
            if (sp->type == T_QUOTED_ARRAY)
                new.x.quotes = sp->x.quotes;
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
        copy_svalue(&new, sp, ptable, 0);
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

/* VEFUN filter()
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
f_map (svalue_t *sp, int num_arg)

/* VEFUN map()
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
                    error("Bad argument to %s: array[%d] is not a string.\n"
                         , fname, (int)VEC_SIZE(argp->u.vec) - left + 1);
                else
                    error("Bad argument %d to %s: not a string.\n"
                         , num_arg - left + 1, fname);
                /* TODO: Give type and value */
                /* NOTREACHED */
            }

            cmp = strcmp(valuep->u.string, result->u.string);
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
                    error("Bad argument to %s: array[%d] is not a number.\n"
                         , fname, (int)VEC_SIZE(argp->u.vec) - left + 1);
                else
                    error("Bad argument %d to %s: not a number.\n"
                         , num_arg - left + 1, fname);
                /* TODO: Give type and value */
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
            error("Bad argument to %s: array[0] is not a string or number.\n"
                 , fname);
        else
            error("Bad argument 1 to %s: not a string or number.\n"
                 , fname);
        /* TODO: Give type and value */
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

/* VEFUN debug_info()
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
 *     <arg2> == "objects": dump information about all live objects. Default
 *       filename is '/OBJ_DUMP', the valid_write() will read 'objdump' for
 *       the function.
 *
 *     <arg2> == "destructed": dump information about all destructed objects.
 *       Default filename is '/DEST_OBJ_DUMP', the valid_write() will read
 *       'objdump' for the function.
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
 *            Total number and size of objects.
 *
 *        int DID_ST_OBJECTS_SWAPPED
 *        int DID_ST_OBJECTS_SWAP_SIZE
 *            Number and size of swapped-out object variable blocks.
 *
 *        int DID_ST_OBJECTS_LIST
 *            Number of objects in the object list.
 *
 *        int DID_ST_OBJECTS_NEWLY_DEST
 *            Number of newly destructed objects (ie. objects destructed
 *            in this execution thread).
 *
 *        int DID_ST_OBJECTS_DESTRUCTED
 *            Number of destructed but still referenced objects, not
 *            counting the DID_ST_OBJECTS_NEWLY_DEST.
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
 *        int DID_ST_STRINGS
 *        int DID_ST_STRING_SIZE
 *            Number of distinct strings in the string table, and
 *            their size.
 *
 *        int DID_ST_STR_TABLE_SIZE
 *            Size of the string table.
 *
 *        int DID_ST_STR_REQ
 *        int DID_ST_STR_REQ_SIZE
 *            Total number of string allocations, and their size.
 *
 *        int DID_ST_STR_SEARCHES
 *        int DID_ST_STR_SEARCH_LEN
 *            Number of searches in the string table, and the
 *            accumulated search length.
 *
 *        int DID_ST_STR_FOUND
 *            Number successful searches.
 *
 *        int DID_ST_STR_ENTRIES
 *            Number of entries (hash chains) in the string table.
 *
 *        int DID_ST_STR_ADDED
 *            Number of strings added to the table so far.
 *
 *        int DID_ST_STR_DELETED
 *            Number of strings delete from the table so far.
 *
 *        int DID_ST_STR_COLLISIONS
 *            Number of strings added to an existing hash chain.
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
 *        int DID_MEM_SBRK_SIZE
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
 *        int DID_MEM_OVERHEAD
 *            Overhead for every allocation (smalloc only).
 *
 *        int DID_MEM_ALLOCATED
 *            The amount of memory currently allocated from the
 *            allocator, including the overhead for the allocator
 *            (smalloc only).
 *
 *        int DID_MEM_USED
 *            The amount of memory currently used for driver data,
 *            excluding the overhead from the allocator (smalloc only).
 *
 *        int DID_MEM_TOTAL_UNUSED
 *            The amount of memory allocated from the system, but
 *            not used by the driver.
 *
 * DINFO_TRACE (7): Return the call stack 'trace' information as specified
 *     by <arg2>. The result of the function is either an array (format
 *     explained below), or a printable string. Omitting <arg2> defaults
 *     to DIT_CURRENT.
 *
 *     <arg2> == DIT_CURRENT (0): Current call trace
 *            == DIT_ERROR   (1): Most recent error call trace (caught or
 *                                uncaught)
 *            == DIT_UNCAUGHT_ERROR (2): Most recent uncaught-error call trace
 *        Return the information in array form.
 *
 *        The error traces are changed only when an appropriate error
 *        occurs; in addition a GC deletes them. After an uncaught
 *        error, both error traces point to the same array (so the '=='
 *        operator holds true).
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
 *     <arg2> == DIT_STR_CURRENT (3): Return the information about the current
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

        int flags;
        object_t *prev, *obj2;

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

        program_t *pg;
        mp_int v0, v1, v2;

        if (num_arg != 2)
            error("bad number of arguments to debug_info\n");
        TYPE_TESTV2(arg+1, T_OBJECT)
        if ((sp->u.ob->flags & O_SWAPPED) && load_ob_from_swap(sp->u.ob) < 0)
            error("Out of memory: unswap object '%s'\n", sp->u.ob->name);
        pg = sp->u.ob->prog;
        add_message("program ref's %3ld\n",        pg->ref);
        add_message("Name: '%s'\n",                pg->name);
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
                if (inheritp->inherit_type == INHERIT_TYPE_NORMAL
                 || inheritp->inherit_type == INHERIT_TYPE_VIRTUAL
                   )
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

        if (!strcmp(arg[1].u.string, "destructed"))
        {
            res.u.number = dumpstat_dest(fname ? fname : "/DEST_OBJ_DUMP") ? 1 : 0;
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
        TYPE_TESTV2(arg+1, T_NUMBER)
        if (num_arg == 3)
        {
            TYPE_TESTV3(arg+2, T_NUMBER)
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
                put_volatile_string(v->item+DID_MEM_NAME, "system malloc");
            else if (value == DID_MEM_NAME)
                put_volatile_string(dinfo_arg, "system malloc");
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
        else if (sp->u.number == DIT_UNCAUGHT_ERROR)
        {
            if (uncaught_error_trace)
                put_ref_array(&res, uncaught_error_trace);
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
            put_malloced_string(&res, string_copy(sbuf.buf));
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
static INLINE svalue_t *
x_gm_localtime (svalue_t *sp, Bool localTime)

/* Implementation of the efuns gmtime() and localtime()
 * localTime = TRUE: return localtime(), otherwise gmtime()
 */

{
    time_t      clk;
    struct tm * pTm;
    vector_t  * v;

    if (sp->type != T_NUMBER)
    {
        TYPE_TEST1(sp, T_POINTER)
        if (VEC_SIZE(sp->u.vec) != 2)
            error("Invalid array size for argument 1: %ld, expected 2\n"
                 , (long)VEC_SIZE(sp->u.vec));
        if (sp->u.vec->item[0].type != T_NUMBER
         || sp->u.vec->item[1].type != T_NUMBER)
            error("Invalid array for argument 1\n");
        clk = sp->u.vec->item[0].u.number;
    }
    else
    {
        clk = sp->u.number;
    }

    pTm = (localTime ? localtime : gmtime)(&clk);

    v = allocate_array(TM_MAX);
    if (!v)
        error("Out of memory: array[%d] for result.\n", TM_MAX);

    v->item[TM_SEC].u.number = pTm->tm_sec;
    v->item[TM_MIN].u.number = pTm->tm_min;
    v->item[TM_HOUR].u.number = pTm->tm_hour;
    v->item[TM_MDAY].u.number = pTm->tm_mday;
    v->item[TM_MON].u.number = pTm->tm_mon;
    v->item[TM_YEAR].u.number = pTm->tm_year + 1900;
    v->item[TM_WDAY].u.number = pTm->tm_wday;
    v->item[TM_YDAY].u.number = pTm->tm_yday;
    v->item[TM_ISDST].u.number = pTm->tm_isdst ? 1 : 0;

    free_svalue(sp);
    put_array(sp, v); /* Adopt the ref */

    return sp;
} /* x_gm_localtime() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_gmtime (svalue_t *sp)

/* TEFUN gmtime()
 *
 *   int * gmtime(int clock = time())
 *   int * gmtime(int* uclock)
 *
 * Interpret the argument clock as number of seconds since Jan,
 * 1st, 1970, 0:00, and return the time in UTC in a nice structure.
 *
 * Alternatively, accept an array of two ints: the first is <clock>
 * value as in the first form, the second int is the number of
 * microseconds elapsed in the current second.
 *
 * The result is an array of integers:
 *
 *   int TM_SEC   (0) : Seconds (0..59)
 *   int TM_MIN   (1) : Minutes (0..59)
 *   int TM_HOUR  (2) : Hours (0..23)
 *   int TM_MDAY  (3) : Day of the month (1..31)
 *   int TM_MON   (4) : Month of the year (0..11)
 *   int TM_YEAR  (5) : Year (e.g.  2001)
 *   int TM_WDAY  (6) : Day of the week (Sunday = 0)
 *   int TM_YDAY  (7) : Day of the year (0..365)
 *   int TM_ISDST (8) : TRUE: Daylight saving time
 */

{
    return x_gm_localtime(sp, MY_FALSE);
} /* f_gmtime() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_localtime (svalue_t *sp)

/* TEFUN localtime()
 *
 *   int * localtime(int clock = time())
 *   int * localtime(int* uclock)
 *
 * Interpret the argument clock as number of seconds since Jan,
 * 1st, 1970, 0:00, and return the time in local time in a nice structure.
 *
 * Alternatively, accept an array of two ints: the first is <clock>
 * value as in the first form, the second int is the number of
 * microseconds elapsed in the current second.
 *
 * The result is an array of integers:
 *
 *   int TM_SEC   (0) : Seconds (0..59)
 *   int TM_MIN   (1) : Minutes (0..59)
 *   int TM_HOUR  (2) : Hours (0..23)
 *   int TM_MDAY  (3) : Day of the month (1..31)
 *   int TM_MON   (4) : Month of the year (0..11)
 *   int TM_YEAR  (5) : Year (e.g.  2001)
 *   int TM_WDAY  (6) : Day of the week (Sunday = 0)
 *   int TM_YDAY  (7) : Day of the year (0..365)
 *   int TM_ISDST (8) : TRUE: Daylight saving time
 */

{
    return x_gm_localtime(sp, MY_TRUE);
} /* f_localtime() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_shutdown (svalue_t *sp)

/* TEFUN shutdown()
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

/***************************************************************************/

