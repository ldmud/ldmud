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
#include "lex.h"
#include "main.h"
#include "mstrings.h"
#include "simulate.h"
#include "svalue.h"
#include "xalloc.h"

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
    sp = strp = sp-1;
    if (sp->u.number < 0)
        error("clear_bit: negative bit number: %ld\n", (long)sp->u.number);
    if (bitnum > MAX_BITS)
        error("clear_bit: too big bit number: %ld\n", (long)bitnum);

    len = mstrsize(strp->u.str);
    ind = bitnum/6;
    if (ind >= len)
    {
        /* Bits beyond the current end of the string are assumged to
         * be cleared anyway. Therefore, return the argument unmodified.
         */
        return sp;
    }

    memsafe(new = dup_mstring(strp->u.str), mstrsize(strp->u.str)
           , "new bitstring");
    free_mstring(strp->u.str);
    strp->u.str = new;
    str = get_txt(strp->u.str);

    if (str[ind] > 0x3f + ' ' || str[ind] < ' ')
        error("Illegal bit pattern in clear_bit character %ld\n", (long)ind);

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
    sp = strp = sp-1;
    if (sp->u.number < 0)
        error("set_bit: negative bit number: %ld\n", (long)sp->u.number);
    if (bitnum > MAX_BITS)
        error("set_bit: too big bit number: %ld\n", (long)bitnum);

    len = mstrsize(strp->u.str);
    ind = bitnum/6;

    if (ind < len)
    {
        string_t *new;

        memsafe(new = dup_mstring(strp->u.str), mstrsize(strp->u.str)
               , "new bitstring");
        free_mstring(strp->u.str);
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
        error("Illegal bit pattern in set_bit character %ld\n", (long)ind);

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
        error("test_bit: negative bit number: %ld\n", (long)bitnum);

    len = mstrsize(sp[-1].u.str);
    if (bitnum/6 >= len)
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

{
    size_t  len1, len2, arg_len;
    string_t *result, *arg;
    char *restxt, *argtxt;
    Bool  use_short; /* TRUE for AND: use shorter string for result */

    use_short = (instr == F_AND_BITS);

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
        memsafe(result = dup_mstring(sp[-1].u.str), mstrsize(sp[-1].u.str)
               , "new bitstring");
        free_mstring(sp[-1].u.str);
    }
    else
    {
        /* AND: sp is the shorter result; sp-1 the longer argument
         * else: sp is the longer result; sp-1 the shorter argument
         */
        arg = (sp-1)->u.str;
        memsafe(result = dup_mstring(sp->u.str), mstrsize(sp->u.str)
               , "new bitstring");
        free_mstring(sp->u.str);
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
            error("Illegal bit pattern in %s character %d\n"
                   , get_f_name(instr), (int)c1);
        if (c2 > 0x3f + ' ' || c2 < ' ')
            error("Illegal bit pattern in %s character %d\n"
                   , get_f_name(instr), (int)c2);
        if (instr == F_AND_BITS)
            restxt[arg_len] = (char)((c1-' ') & (c2-' ')) + ' ';
        else if (instr == F_OR_BITS)
            restxt[arg_len] = (char)((c1-' ') | (c2-' ')) + ' ';
        else if (instr == F_XOR_BITS)
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
    return binop_bits(sp, F_OR_BITS);
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
    return binop_bits(sp, F_AND_BITS);
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
    return binop_bits(sp, F_XOR_BITS);
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
    memsafe(new = dup_mstring(sp->u.str), len
           , "new bitstring");
    free_mstring(sp->u.str);
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
    long     len;
    char   * str;
    int      c;

    pos = -1;

    /* Get the arguments */
    str = get_txt(sp->u.str);
    len = (long)mstrsize(sp->u.str);

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

    while (found < 0 && search < len)
    {
        int c = str[search] - ' ';

        if (c < 0 || c > 0x3f)
            error("Illegal bit pattern in next_bit character %d\n"
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
            error("Illegal character in count_bits: %d\n", (int)c + ' ');

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

/***************************************************************************/

