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

#define USES_SVALUE_STRLEN
#include "bitstrings.h"

#include "instrs.h"
#include "interpret.h"
#include "lex.h"
#include "main.h"
#include "simulate.h"
#include "svalue.h"
#include "xalloc.h"
#include "smalloc.h" /* svalue_strlen() */

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
    size_t len, ind, bitnum;
    svalue_t *strp;

    /* Get the arguments */
    bitnum = (size_t)sp->u.number;
    sp = strp = sp-1;
    if (bitnum > MAX_BITS)
        error("clear_bit: too big bit number: %ld\n", (long)bitnum);
    if (bitnum < 0)
        error("clear_bit: negative bit number: %ld\n", (long)bitnum);

    len = svalue_strlen(strp);
    ind = bitnum/6;
    if (ind >= len)
    {
        /* Return first argument unmodified! */
        return sp;
    }

    /* Malloc'ed strings are modified in place, others are copied first.
     */
    if (strp->x.string_type == STRING_MALLOC)
    {
        str = strp->u.string;
    }
    else
    {
        str = xalloc(len+1);
        memcpy(str, strp->u.string, len+1); /* Including null byte */
        free_string_svalue(strp);
        strp->x.string_type = STRING_MALLOC;
        strp->u.string = str;
    }

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
    size_t len, old_len, ind, bitnum;
    svalue_t *strp;

    bitnum = (size_t)sp->u.number;
    sp = strp = sp-1;
    if (bitnum > MAX_BITS)
        error("set_bit: too big bit number: %ld\n", (long)bitnum);
    if (bitnum < 0)
        error("set_bit: negative bit number: %ld\n", (long)bitnum);

    len = svalue_strlen(strp);
    old_len = len;
    ind = bitnum/6;

    /* Malloc'ed strings of the right size are modified in place,
     * others are copied first.
     */
    if ( (ind < len || (len = ind + 1, MY_FALSE) )
     &&  strp->x.string_type == STRING_MALLOC )
    {
        str = strp->u.string;
    }
    else
    {
        str = xalloc(len+1);
        str[len] = '\0';
        if (old_len)
            memcpy(str, strp->u.string, old_len);
        if (len > old_len)
            memset(str + old_len, ' ', len - old_len);
        free_string_svalue(strp);
        strp->x.string_type = STRING_MALLOC;
        strp->u.string = str;
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

    len = svalue_strlen(sp-1);
    if (bitnum/6 >= len)
    {
        sp--;
        free_string_svalue(sp);
        put_number(sp, 0);
        return sp;
    }

    if ( ((sp-1)->u.string[bitnum/6] - ' ')
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
    size_t  len1, len2, result_len, arg_len;
    char   *arg, *result, *to_copy;
    Bool  use_short; /* TRUE for AND: use shorter string for result */

    result_len = 0;
    to_copy = NULL;

    use_short = (instr == F_AND_BITS);

    /* Get the arguments.
     * Sort the two arguments in shorter and longer string.
     * We will try to modify one of the two strings in-place.
     */
    result = NULL;
    len1 = svalue_strlen(sp-1);
    len2 = svalue_strlen(sp);

    if ((len1 >= len2 && !use_short)
     || (len1 < len2 && use_short)
       )
    {
        /* AND: sp-1 is the shorter result; sp the longer argument
         * else: sp-1 is the longer result; sp the shorter argument
         */

        arg = sp->u.string;

        if ((sp-1)->x.string_type == STRING_MALLOC)
        {
            result = (sp-1)->u.string;
            *(sp-1) = const0;
        }
        else
        {
            result_len = len1;
            to_copy = (sp-1)->u.string;
        }
    }
    else
    {
        /* AND: sp is the shorter result; sp-1 the longer argument
         * else: sp is the longer result; sp-1 the shorter argument
         */
        arg = (sp-1)->u.string;

        if (sp->x.string_type == STRING_MALLOC)
        {
            result = sp->u.string;
            *sp = const0;
        }
        else
        {
            result_len = len2;
            to_copy = sp->u.string;
        }
    }

    /* If needed, allocate a copy of the result string.
     */
    if (!result)
    {
        inter_sp = sp;
        result = xalloc(result_len+1);
        if (!result)
            error("Out of memory.\n");
        memcpy(result, to_copy, result_len+1);
    }

    /* Now perform the operation. */

    arg_len = (len2 > len1) ? len1 : len2;
    while (arg_len-- != 0)
    {
        char c1, c2;

        c1 = result[arg_len];
        c2 = arg[arg_len];
        if (c1 > 0x3f + ' ' || c1 < ' ')
            error("Illegal bit pattern in %s character %d\n"
                   , get_f_name(instr), (int)c1);
        if (c2 > 0x3f + ' ' || c2 < ' ')
            error("Illegal bit pattern in %s character %d\n"
                   , get_f_name(instr), (int)c2);
        if (instr == F_AND_BITS)
            result[arg_len] = (char)(c1 & c2);
        else if (instr == F_OR_BITS)
            result[arg_len] = (char)(c1 | c2);
        else if (instr == F_XOR_BITS)
            result[arg_len] = (char)((c1 ^ c2) + ' ');
    }

    /* Clean up the stack and push the result. */
    free_svalue(sp--);
    free_svalue(sp);
    put_malloced_string(sp, result);

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
    char * src, * dest;
    long   len;

    /* Get the arguments */

    src = sp->u.string;
    len = (size_t)_svalue_strlen(sp);

    /* If it is a malloced string, modify it in place,
     * otherwise allocate a copy.
     */
    if (sp->x.string_type == STRING_MALLOC)
        dest = src;
    else
    {
        inter_sp = sp;
        dest = xalloc((size_t)len+1);
        if (!dest)
            error("Out of memory\n");
    }

    /* Invert the string */
    while (len-- > 0)
    {
        *dest++ = (char)(' ' + (~(*src++ - ' ') & 0x3F));
    }

    *dest = '\0';

    /* Push the result */
    if (src != dest)
    {
        free_svalue(sp);
        put_malloced_string(sp, dest);
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
    str = sp->u.string;
    len = (long)_svalue_strlen(sp);

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

    str = (sp-2)->u.string;
    len = (long)_svalue_strlen(sp-2);

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

    /* Get the arguments */
    str = sp->u.string;

    for (count = 0; *str; str++)
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

