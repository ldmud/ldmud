/*---------------------------------------------------------------------------
 * Bitstring Efuns.
 *
 *---------------------------------------------------------------------------
 * Bitstrings are a compact yet portable way to store a large number of
 * bitflags in one value. The bits are stored by encoding them in
 * character, and then keeping the characters around as normal strings.
 *
 * Each character contains 6 bits. So you can store a value
 * between 0 and 63 ( 2^6=64) in one character. Starting
 * character is the blank character " " which has the value 0.
 * The first charcter in the string is the one with the lowest
 * bits (0-5).
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"


#include "bitstrings.h"

#include "interpret.h"
#include "mstrings.h"
#include "simulate.h"
#include "svalue.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/
#if 0

#include <stdio.h>

static void
printbits (string_t *bstr)

/* Auxiliary function for debugging: print the data from a bitstring */

{
    long len = (long)mstrsize(bstr);
    char *cp = get_txt(bstr);

    while (len > 0)
    {
        int bit;

        for (bit = 0; bit < 6; bit++)
        {
            if ((*cp-' ') & (1 << bit)) putchar('1'); else putchar('.');
        }
        if (len > 1) putchar(' ');
        len--;
        cp++;
    }
} /* printbits() */

#endif

/*-------------------------------------------------------------------------*/
static INLINE p_int
last_bit (string_t *str)

/* Return the number of the last set bit in bitstring <str>.
 */

{
    mp_int       pos;
    long         len;
    const char * cstr;
    int          c;

    pos = -1;

    /* Get the arguments */
    cstr = get_txt(str);
    len = (long)mstrsize(str);

    /* First, find the last non-zero character */
    c = 0;
    while (len-- > 0 && (c = cstr[len]) == ' ') NOOP;

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
svalue_t *
f_clear_bit (svalue_t *sp)

/* EFUN clear_bit()
 *
 *     string clear_bit(string str, int n)
 *
 * Return the new string where bit n is cleared in string str.
 * Note that the old string str is not modified.
 */

{
    char *str;
    string_t *new;
    size_t len, ind, bitnum;
    svalue_t *strp;

    /* Get the arguments */
    bitnum = (size_t)sp->u.number;
    if (sp->u.number < 0)
        errorf("clear_bit: negative bit number: %ld\n", (long)sp->u.number);
    if (bitnum > MAX_BITS)
        errorf("clear_bit: too big bit number: %ld\n", (long)bitnum);
    sp = strp = sp-1;

    len = mstrsize(strp->u.str);
    ind = bitnum/6;
    if (ind >= len)
    {
        /* Bits beyond the current end of the string are assumged to
         * be cleared anyway. Therefore, return the argument unmodified.
         */
        return sp;
    }

    memsafe(new = unshare_mstring(strp->u.str), mstrsize(strp->u.str)
           , "new bitstring");
    strp->u.str = new;
    str = get_txt(strp->u.str);

    if (str[ind] > 0x3f + ' ' || str[ind] < ' ')
        errorf("Illegal bit pattern in clear_bit character %ld\n", (long)ind);

    str[ind] = (char)(((str[ind] - ' ') & ~(1 << (bitnum % 6))) + ' ');

    return sp;
} /* f_clear_bit() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_bit (svalue_t *sp)

/* EFUN set_bit()
 *
 *   string set_bit(string str, int n)
 *
 * Return the new string where bit n is set in string str. Note
 * that the old string str is not modified.
 *
 * The new string will automatically be extended if needed.
 * TODO: Apply to an optional range (start, length) only.
 */

{
    char *str;
    size_t len, ind, bitnum;
    svalue_t *strp;

    bitnum = (size_t)sp->u.number;
    if (sp->u.number < 0)
        errorf("set_bit: negative bit number: %ld\n", (long)sp->u.number);
    if (bitnum > MAX_BITS)
        errorf("set_bit: too big bit number: %ld\n", (long)bitnum);
    sp = strp = sp-1;

    len = mstrsize(strp->u.str);
    ind = bitnum/6;

    if (ind < len)
    {
        string_t *new;

        memsafe(new = unshare_mstring(strp->u.str), mstrsize(strp->u.str)
               , "new bitstring");
        strp->u.str = new;
        str = get_txt(strp->u.str);
    }
    else
    {
        string_t *new;

        (void)ref_mstring(strp->u.str); /* In case resize_ fails */
        memsafe(new = resize_mstring(strp->u.str, ind+1), ind+1
               , "new bitstring");
        free_mstring(strp->u.str);
        strp->u.str = new;
        str = get_txt(strp->u.str);
        for ( ; len <= ind; len++)
            str[len] = ' ';
    }

    if (str[ind] > 0x3f + ' ' || str[ind] < ' ')
        errorf("Illegal bit pattern in set_bit character %ld\n", (long)ind);

    str[ind] = (char)(((str[ind] - ' ') | 1 << (bitnum % 6) ) + ' ');
    sp = strp;

    return sp;
} /* f_set_bit() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_test_bit (svalue_t *sp)

/* EFUN test_bit()
 *
 *   int test_bit(string str, int n)
 *
 * Return 0 or 1 of bit n was set in string str.
 * TODO: Apply to an optional range (start, length) only.
 */

{
    size_t len;
    int bitnum;

    bitnum = sp->u.number;

    if (bitnum < 0)
        errorf("test_bit: negative bit number: %ld\n", (long)bitnum);

    len = mstrsize(sp[-1].u.str);
    if ((size_t)bitnum/6 >= len)
    {
        sp--;
        free_string_svalue(sp);
        put_number(sp, 0);
        return sp;
    }

    if ( (get_txt((sp-1)->u.str)[bitnum/6] - ' ')
        & 1 << (bitnum % 6) )
    {
        sp--;
        free_string_svalue(sp);
        put_number(sp, 1);
    }
    else
    {
        sp--;
        free_string_svalue(sp);
        put_number(sp, 0);
    }

    return sp;
} /* f_test_bit() */

/*-------------------------------------------------------------------------*/
static svalue_t *
binop_bits (svalue_t *sp, int instr)

/* IMPLEMENTATION or_bits(), and_bits(), xor_bits()
 *
 *     string or_bits(string str1, string str2)
 *     string and_bits(string str1, string str2)
 *     string xor_bits(string str1, string str2)
 *
 * Perform a binary operation <instr> on the bitstrings <str1> and <str2>
 * and return the resulting string.
 *
 * TODO: Apply to an optional range (start, length) only.
 */

#define INSTR_OR_BITS   0
#define INSTR_AND_BITS  1
#define INSTR_XOR_BITS  2

{
    size_t  len1, len2, arg_len;
    string_t *result, *arg;
    char *restxt, *argtxt;
    Bool  use_short; /* TRUE for AND: use shorter string for result */

    use_short = (instr == INSTR_AND_BITS);

    /* Get the arguments.
     * Sort the two arguments in shorter and longer string.
     * We will try to modify one of the two strings in-place.
     */
    result = NULL;
    len1 = mstrsize(sp[-1].u.str);
    len2 = mstrsize(sp->u.str);

    if ((len1 >= len2 && !use_short)
     || (len1 < len2 && use_short)
       )
    {
        /* AND: sp-1 is the shorter result; sp the longer argument
         * else: sp-1 is the longer result; sp the shorter argument
         */

        arg = sp->u.str;
        memsafe(result = unshare_mstring(sp[-1].u.str), mstrsize(sp[-1].u.str)
               , "new bitstring");
        put_number(sp-1, 0);
    }
    else
    {
        /* AND: sp is the shorter result; sp-1 the longer argument
         * else: sp is the longer result; sp-1 the shorter argument
         */
        arg = (sp-1)->u.str;
        memsafe(result = unshare_mstring(sp->u.str), mstrsize(sp->u.str)
               , "new bitstring");
        put_number(sp, 0);
    }

    /* Now perform the operation. */

    restxt = get_txt(result);
    argtxt = get_txt(arg);

    arg_len = (len2 > len1) ? len1 : len2;
    while (arg_len-- != 0)
    {
        char c1, c2;

        c1 = restxt[arg_len];
        c2 = argtxt[arg_len];
        if (c1 > 0x3f + ' ' || c1 < ' ')
        {
            free_mstring(result);
            errorf("Illegal bit pattern in %s character %d\n"
                   , instr == INSTR_OR_BITS
                     ?  "or_bits()"
                     : (instr == INSTR_AND_BITS
                        ?  "and_bits()"
                         : (instr == INSTR_XOR_BITS
                            ? "xor_bits()"
                            : "unknown"
                       ))
                   , (int)c1);
        }
        if (c2 > 0x3f + ' ' || c2 < ' ')
        {
            free_mstring(result);
            errorf("Illegal bit pattern in %s character %d\n"
                   , instr == INSTR_OR_BITS
                     ?  "or_bits()"
                     : (instr == INSTR_AND_BITS
                        ?  "and_bits()"
                         : (instr == INSTR_XOR_BITS
                            ? "xor_bits()"
                            : "unknown"
                       ))
                   , (int)c2);
        }
        if (instr == INSTR_AND_BITS)
            restxt[arg_len] = (char)((c1-' ') & (c2-' ')) + ' ';
        else if (instr == INSTR_OR_BITS)
            restxt[arg_len] = (char)((c1-' ') | (c2-' ')) + ' ';
        else if (instr == INSTR_XOR_BITS)
            restxt[arg_len] = (char)((c1-' ') ^ (c2-' ')) + ' ';
    }

    /* Clean up the stack and push the result. */
    free_svalue(sp--);
    free_svalue(sp);
    put_string(sp, result);

    return sp;
} /* binop_bits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_or_bits (svalue_t *sp)

/* EFUN or_bits()
 *
 *     string or_bits(string str1, string str2)
 *
 * Perform a binary operation on the bitstrings <str1> and <str2>
 * and return the resulting string.
 * TODO: Apply to an optional range (start, length) only.
 */

{
    return binop_bits(sp, INSTR_OR_BITS);
} /* f_or_bits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_and_bits (svalue_t *sp)

/* EFUN and_bits()
 *
 *     string and_bits(string str1, string str2)
 *
 * Perform a binary operation on the bitstrings <str1> and <str2>
 * and return the resulting string.
 * TODO: Apply to an optional range (start, length) only.
 */

{
    return binop_bits(sp, INSTR_AND_BITS);
} /* f_and_bits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_xor_bits (svalue_t *sp)

/* EFUN xor_bits()
 *
 *     string xor_bits(string str1, string str2)
 *
 * Perform a binary operation on the bitstrings <str1> and <str2>
 * and return the resulting string.
 * TODO: Apply to an optional range (start, length) only.
 */

{
    return binop_bits(sp, INSTR_XOR_BITS);
} /* f_xor_bits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_invert_bits (svalue_t *sp)

/* EFUN invert_bits()
 *
 *     string invert_bits(string str)
 *
 * Invert all bits in the bitstring <str> and return the
 * new string.
 *
 * TODO: Apply to an optional range (start, length) only.
 */

{
    char * str;
    string_t *new;
    long   len;

    /* Get the arguments */

    len = (long)mstrsize(sp->u.str);
    memsafe(new = unshare_mstring(sp->u.str), len
           , "new bitstring");
    sp->u.str = new;
    str = get_txt(sp->u.str);

    /* Invert the string */
    while (len-- > 0)
    {
        *str = (char)(' ' + (~(*str - ' ') & 0x3F));
        str++;
    }

    return sp;
} /* f_invert_bits() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_last_bit (svalue_t *sp)

/* EFUN last_bit()
 *
 *     int last_bit(string str)
 *
 * Return the number of the last set bit in bitstring <str>.
 * If no bit is set, return -1.
 *
 * TODO: extend this to true int-bitflags?
 * TODO: Apply to an optional range (start, length) only.
 */

{
    mp_int   pos;

    pos = last_bit(sp->u.str);

    /* Clear the stack and push the result */
    free_svalue(sp);
    put_number(sp, pos);

    return sp;
} /* f_last_bit() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_next_bit (svalue_t *sp)

/* EFUN next_bit()
 *
 *     int next_bit(string str, int start, int find_zero)
 *
 * Return the number of the next bit after position <start>
 * in bitstring <str>. If <find_zero> is non-null, the next
 * unset bit is found, else the next set bit.
 * If there is no such bit, return -1.
 *
 * If <start> is negative, the string is searched from the
 * beginning.
 *
 * TODO: extend this to true int-bitflags?
 */

{
    mp_int   found;    /* Resultvalue */
    size_t   pos;      /* Searchposition */
    size_t   search;   /* Searchindex */
    int      pattern;  /* Pattern for next bit to test */
    long     len;      /* Length of the string */
    long     start;    /* Startposition */
    char   * str;      /* the bitstring */
    int      invert;   /* when looking for 0 bits, an inverter mask */

    /* Get the arguments */

    str = get_txt((sp-2)->u.str);
    len = (long)mstrsize((sp-2)->u.str);

    start = (sp-1)->u.number;
    if (start < 0)
    {
        pattern = 0x01;
        pos = 0;
        search = 0;
    }
    else if (start % 6 == 5)
    {
        pattern = 0x01;
        pos = (size_t)start + 1;
        search = (size_t)start / 6 + 1;
    }
    else
    {
        pattern = 1 << (start % 6 + 1);
        pos = (size_t)start + 1;
        search = (size_t)start / 6;
    }

    invert = 0;
    if (!sp->type == T_NUMBER || sp->u.number)
        invert = 0x3f;

    /* Now search for the next bit */
    found = -1;

    while (found < 0 && (long)search < len)
    {
        int c = str[search] - ' ';

        if (c < 0 || c > 0x3f)
            errorf("Illegal bit pattern in next_bit character %d\n"
                   , c+' ');
        c ^= invert;

        while (found < 0 && pattern < (1 << 6))
        {
            if (c & pattern)
            {
                found = (mp_int)pos;
                break;
            }
            pattern <<= 1;
            pos++;
        }
        pattern = 0x01;
        search++;
    }

    /* Cleanup the stack and push the result */
    free_svalue(sp--);
    free_svalue(sp--);
    free_svalue(sp);
    put_number(sp, found);

    return sp;
} /* f_next_bit() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_count_bits (svalue_t *sp)

/* EFUN count_bits()
 *
 *     int count_bits(string str)
 *
 * Return the number of set bits in bitstring <str>.
 *
 * TODO: Apply to an optional range (start, length) only.
 */

{
    char * str;
    long   count;
    long   len;

    /* Get the arguments */
    str = get_txt(sp->u.str);
    len = (long)mstrsize(sp->u.str);

    for (count = 0; len > 0; str++, len--)
    {
        int c = *str - ' ';

        if (c > 0x3F || c < 0)
            errorf("Illegal character in count_bits: %d\n", (int)c + ' ');

        /* Count the bits in this character */
        count += ( (c & 0x01) )
               + ( (c & 0x02) >> 1)
               + ( (c & 0x04) >> 2)
               + ( (c & 0x08) >> 3)
               + ( (c & 0x10) >> 4)
               + ( (c & 0x20) >> 5);
    }

    /* Return the result */

    free_svalue(sp);
    put_number(sp, count);

    return sp;
} /* f_count_bits() */

/*-------------------------------------------------------------------------*/
static void
copy_bits ( string_t * dest, p_int deststart
          , string_t * src,  p_int srcstart
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

    pDest = get_txt(dest) + deststart / 6;
    pSrc = get_txt(src) + srcstart / 6;

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
v_copy_bits (svalue_t *sp, int num_arg)

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
    string_t *src, *dest, *result;

    /* Get the optional command arguments */
    srcstart = deststart = copylen = 0;
    copyall = MY_TRUE;

    switch (num_arg)
    {
    case 5:
        copyall = MY_FALSE;
        copylen = sp->u.number;
        sp--;
        num_arg--;
        /* FALL THROUGH */

    case 4:
        deststart = sp->u.number;
        sp--;
        num_arg--;
        /* FALL THROUGH */

    case 3:
        srcstart = sp->u.number;
        sp--;
        num_arg--;
        /* FALL THROUGH */
    }
    inter_sp = sp;

    /* Get the fixed command arguments and check for consistency */
    dest = sp->u.str;
    src = sp[-1].u.str;

    sp++; /* We might need to save a precautionary reference to the result */

    srclen = last_bit(src)+1;
    destlen = last_bit(dest)+1;

    if (srcstart < 0 && srcstart + srclen < 0)
        errorf("Bad argument 3 to copy_bits(): Index %ld is out of range "
              "(last bit: %ld).\n"
             , (long)srcstart, (long)srclen);
    if (srcstart < 0)
        srcstart += srclen;

    if (deststart < 0 && deststart + destlen < 0)
        errorf("Bad argument 4 to copy_bits(): Index %ld is out of range "
              "(last bit: %ld).\n"
             , (long)deststart, (long)destlen);
    if (deststart < 0)
        deststart += destlen;

    if (!copyall && copylen < 0)
    {
        if (srcstart + copylen < 0)
            errorf("Bad argument 5 to copy_bits(): Length %ld out of range "
                  "(start index: %ld).\n"
                 , (long)copylen, (long)srcstart);

        srcstart += copylen;
        copylen = -copylen;
    }

    /* Test the input strings for sanity */
    {
        char *cp;
        long len;

        len = (long)mstrsize(src);
        cp = get_txt(src);
        for ( ; len > 0; len--, cp++)
        {
            int c = *cp - ' ';
            if (c < 0 || c > 0x3f)
                errorf("Bad argument 1 to copy_bits(): String contains "
                      "illegal character %d\n", c + ' ');
        }

        len = (long)mstrsize(dest);
        cp = get_txt(dest);
        for ( ; len > 0; len--, cp++)
        {
            int c = *cp - ' ';
            if (c < 0 || c > 0x3f)
                errorf("Bad argument 2 to copy_bits(): String contains "
                      "illegal character %d\n", c + ' ');
        }
    }

    /* Do the copying - some constellations are really simple */
    if (copyall)
    {
        if (srcstart == 0 && deststart == 0)
            result = ref_mstring(src);
        else
        {
            if (srclen > srcstart)
                copylen = srclen - srcstart;
            else
                copylen = 0;

            if (PINT_MAX - copylen < deststart)
                errorf("copy_bits: result length exceeds numerical limit: "
                      "%ld + %ld\n"
                     , (long)deststart, (long)copylen
                     );
            resultlen = deststart + copylen;
            if (resultlen > MAX_BITS || resultlen < 0)
                errorf("copy_bits: Result too big: %lu bits\n"
                     , (unsigned long)resultlen);

            /* Get the result string and store the reference on the stack
             * for error cleanups.
             */
            memsafe(result = alloc_mstring((resultlen + 5) / 6)
                   , (resultlen + 5) / 6, "new bitstring");
            memset(get_txt(result), ' ', (resultlen + 5) / 6);
            inter_sp = sp; put_string(sp, result);

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
        result = ref_mstring(dest);
    else if (srcstart == 0 && deststart == 0
          && copylen >= destlen && copylen >= srclen)
    {
        result = ref_mstring(src);
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
            errorf("copy_bits: result length exceeds numerical limit: %ld + %ld\n"
                 , (long)deststart, (long)copylen
                 );

        resultlen = deststart + copylen;
        if (resultlen < destlen)
            resultlen = destlen;

        if (resultlen > MAX_BITS || resultlen < 0)
            errorf("copy_bits: Result too big: %lu bits\n"
                 , (unsigned long)resultlen);

        destendlen = destlen - (deststart + copylen);

        /* Get the result string and store the reference on the stack
         * for error cleanups.
         */
        memsafe(result = alloc_mstring((resultlen + 5) / 6)
               , (resultlen + 5) / 6, "new bitstring");
        memset(get_txt(result), ' ', (resultlen + 5) / 6);
        inter_sp = sp; put_string(sp, result);

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

    put_string(sp, result);
    return sp;
} /* v_copy_bits() */

/***************************************************************************/

