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
 *    efun: md5()
 *    efun: md5_crypt()
 *    efun: hash()
 *    efun: hmac()
 *    efun: regexp()
 *    efun: regexplode()
 *    efun: regreplace()
 *    efun: process_string() (optional)
 *    efun: sha()
 *    efun: sscanf()
 *    efun: strstr()
 *    efun: strrstr()
 *    efun: terminal_colour()
 *    efun: trim()
 *    efun: upper_case()
 *
 * Objects:
 *    TODO: Move into object.c.
 *    efun: blueprint()
 *    efun: clones()
 *    efun: object_info()
 *    efun: present_clone()
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
 *    efun: to_struct()
 *    efun: to_object()
 *    efun: copy()
 *    efun: deep_copy()
 *    efun: filter()
 *    efun: get_type_info()
 *    efun: map()
 *    efun: member()
 *    efun: min()
 *    efun: max()
 *    efun: reverse()
 *    efun: sgn()
 *    efun: quote()
 *
 * Others:
 *    efun: ctime()
 *    efun: rusage() (optional)
 *    efun: shutdown()
 *    efun: gmtime()
 *    efun: localtime()
 *    efun: time()
 *    efun: utime()
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include "my-rusage.h"
#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>
#include <wctype.h>

#include "efuns.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "dumpstat.h"
#include "exec.h"
#include "gcollect.h"
#include "heartbeat.h"
#include "iconv_opt.h"
#include "interpret.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "mempools.h"
#include "md5.h"
#include "mregex.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#include "ptrtable.h"
#include "random.h"
#include "sha1.h"
#include "stdstrings.h"
#include "simulate.h"
#include "strfuns.h"
#include "structs.h"
#ifdef USE_TLS
#include "pkg-tls.h"
#endif /* USE_TLS */
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/driver_info.h"
#include "../mudlib/sys/configuration.h"
#include "../mudlib/sys/object_info.h"
#include "../mudlib/sys/regexp.h"
#include "../mudlib/sys/strings.h"
#include "../mudlib/sys/time.h"
#include "../mudlib/sys/tls.h"

/* Variables */
string_t *last_ctime_result = NULL;
  /* points to the result of the last f_ctime() call. If the caller asks for 
   * the same timestamp, it will be returned. */

static char* sscanf_format_str_end;
  /* This will point to the zero byte (string end) after a harmless
   * type specifier ('d'). The sscanf() routines use that to abort
   * parsing the format string.
   */

/* Forward declarations */
static void copy_svalue (svalue_t *dest, svalue_t *, struct pointer_table *, int);

/* Macros */

/*-------------------------------------------------------------------------*/

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
    string_t *str = sp->u.str;
    p_int ch;
    size_t chlen = utf8_to_unicode(get_txt(str), mstrsize(str), &ch);

    if (chlen && iswlower(ch))
    {
        string_t *new;
        size_t newchlen;

        ch = towupper(ch);
        newchlen = utf8_size(ch);

        if (newchlen == chlen)
        {
            memsafe(new = unshare_mstring(str), mstrsize(str), "result string");
        }
        else
        {
            size_t remaining = mstrsize(str) - chlen;
            memsafe(new = alloc_mstring(newchlen + remaining), newchlen + remaining, "result string");
            memcpy(get_txt(new) + newchlen, get_txt(str) + chlen, remaining);
            if (newchlen > 1 || !is_ascii(get_txt(str) + chlen, remaining))
                new->info.unicode = STRING_UTF8;
            else
                new->info.unicode = STRING_ASCII;
        }

        sp->u.str = new;
        unicode_to_utf8(ch, get_txt(new));
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
        errorf("Bad argument 2 to crypt(): string too short.\n");

    res = crypt(get_txt((sp-1)->u.str), salt);
    /* According to POSIX crypt shall return NULL upon error,
     * but there are some strange implementations out there
     * returning a string starting with '*' instead.
     */
    if (!res || *res == '*')
    {
        if (errno == EINVAL && sp->type == T_STRING)
            errorf("Bad argument 2 to crypt(): Invalid salt.\n");
        else
            errorf("crypt() is not available.\n");
    }

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
 *   string *explode(bytes str, bytes del)
 *
 * Return an array of strings, created when the string str is
 * split into substrings as divided by del.
 */

{
    vector_t *v;

    if (sp[-1].type != sp[0].type)
        efun_arg_error(2, sp[-1].type, sp[0].type, sp);

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
        errorf("Out of memory for implode() result.\n");

    free_string_svalue(sp);
    sp--;
    free_array(sp->u.vec);

    if (sp[1].type == T_BYTES)
        put_bytes(sp, str);
    else
        put_string(sp, str);

    return sp;
} /* f_implode() */

/*-------------------------------------------------------------------------*/
static svalue_t *
change_case (svalue_t *sp, bool upper)

/* Implements the efuns upper_case() and lower_case().
 * Replaces the string <sp> with a string where all characters
 * are converted to upper case (<upper> == true)
 * resp. lower case (<upper> == false)
 */

{
    string_t* str = sp->u.str;
    char *s;
    size_t count, len;

    /* Find the first non-uppercase resp. non-lowercase character in the string */
    len = mstrsize(str);
    for (s = get_txt(str), count = 0; count < len;)
    {
        p_int ch;
        size_t chlen = utf8_to_unicode(s, len - count, &ch);

        if (chlen && (upper ? iswlower(ch) : iswupper(ch)))
            break;
        if (!chlen)
            chlen++;

        s += chlen;
        count += chlen;
    }

    if (count < len)  /* there are characters with the wrong case */
    {
        string_t *new;
        memsafe(new = unshare_mstring(str), len, "result string");
        sp->u.str = new;

        for (s = get_txt(new) + count; count < len;)
        {
            p_int ch;
            size_t oldchlen = utf8_to_unicode(s, len - count, &ch);

            if (oldchlen && (upper ? iswlower(ch) : iswupper(ch)))
            {
                size_t newchlen;

                ch = upper ? towupper(ch) : towlower(ch);
                newchlen = utf8_size(ch);

                if (newchlen != oldchlen)
                {
                    /* We need to resize the string. */
                    size_t remaining = len - count - oldchlen;
                    string_t *rep;

                    memsafe(rep = alloc_mstring(count + newchlen + remaining), count + newchlen + remaining, "result string");
                    memcpy(get_txt(rep), get_txt(new), count);
                    memcpy(get_txt(rep) + count + newchlen, get_txt(new) + count + oldchlen, remaining);

                    if (newchlen > 1 || !is_ascii(get_txt(new), count) || !is_ascii(get_txt(new) + count + oldchlen, remaining))
                        rep->info.unicode = STRING_UTF8;
                    else
                        rep->info.unicode = STRING_ASCII;

                    free_mstring(new);

                    new = rep;
                    sp->u.str = new;

                    s = get_txt(new) + count;
                    len += newchlen - oldchlen;
                    oldchlen = newchlen;
                }

                unicode_to_utf8(ch, s);
            }
            else if (!oldchlen)
                oldchlen++;

            count += oldchlen;
            s += oldchlen;
        }
    }

    /* That's it */
    return sp;
} /* change_case() */

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
    return change_case(sp, false);
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
    sp->u.str = make_tabled(sp->u.str);

    return sp;
} /* f_make_shared_string() */

/*--------------------------------------------------------------------*/
svalue_t *
v_md5 (svalue_t *sp, int num_arg)

/* EFUN: md5()
 *
 *   string md5(string arg [, int iterations ] )
 *   string md5(int *  arg [, int iterations ] )
 *
 * Create and return a MD5 message digest from the string/array <arg>.
 * If iterations is specified to number > 0, the digest is calculated
 * using the given number of iterations.
 */

{
    M_MD5_CTX context;
    string_t *s_digest;
    unsigned char *digest, d[17];
    int i;
    p_int iterations;

    if (num_arg == 2)
    {
        iterations = sp->u.number;
        sp--;
    }
    else
        iterations = 1;

    if (iterations < 1)
    {
        errorf("Bad argument 2 to md5(): expected a number > 0, but got %"
               PRIdPINT"\n", iterations);
        /* NOTREACHED */
        return sp;
    }

    if (add_eval_cost_n(10, iterations))
    {
        free_svalue(sp);
        put_number(sp, 0);

        // The interpreter loop will catch the exceeded evaluation cost.
        return sp;
    }

    if (sp->type == T_POINTER)
    {
        string_t * arg;
        char * argp;

        memsafe(arg = alloc_mstring(VEC_SIZE(sp->u.vec)), VEC_SIZE(sp->u.vec)
               , "md5 argument string");
        argp = get_txt(arg);

        for (i = 0; i < VEC_SIZE(sp->u.vec); i++)
        {
            svalue_t *item = get_rvalue(sp->u.vec->item + i, NULL);
            if (item == NULL)
                item = sp->u.vec->item + i;
            if (item->type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 1 to md5(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
                return sp;
            }
            argp[i] = (char)item->u.number & 0xff;
        }

        free_svalue(sp);
        put_string(sp, arg);
    }

    MD5Init(&context);
    MD5Update(&context, (unsigned char *)get_txt(sp->u.str), mstrsize(sp->u.str));
    MD5Final(&context, d);

    while (--iterations > 0)
    {
        MD5Init(&context);
        MD5Update(&context, d, sizeof(d)-1);
        MD5Final(&context, d);
    }

    memsafe(s_digest = alloc_mstring(32), 32, "md5 encryption result");
    digest = (unsigned char *)get_txt(s_digest);

    d[16]='\0';

    for (i = 0; i < 16; i++)
        sprintf((char *)digest+2*i, "%02x", d[i]);

    free_svalue(sp);
    put_string(sp, s_digest);

    return sp;
} /* f_md5() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_md5_crypt(svalue_t *sp)

/* EFUN md5_crypt()
 *
 *   string md5_crypt(string str, null|int seed)
 *   string md5_crypt(string str, string seed)
 *
 * Crypt the string <str> using the first two characters
 * from the string <seed> as a seed. If <seed> is equal 0, then
 * a random seed is used.
 *
 * The result has the first two characters as the seed.
 *
 * The efun uses the MD5 algorithm for encryption, and is compatible
 * with the Apache webserver.
 */

{
    char *salt;
    char temp[10];
    char crypted [120];
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
        temp[2] = choise[random_number((sizeof choise) - 1)];
        temp[3] = choise[random_number((sizeof choise) - 1)];
        temp[4] = choise[random_number((sizeof choise) - 1)];
        temp[5] = choise[random_number((sizeof choise) - 1)];
        temp[6] = choise[random_number((sizeof choise) - 1)];
        temp[7] = choise[random_number((sizeof choise) - 1)];
        temp[8] = choise[random_number((sizeof choise) - 1)];
        temp[9] = '\0';
        salt = temp;
    }
    else /* it can't be anything but a too short string */
        errorf("Bad argument 2 to md5_crypt(): string too short.\n");

    MD5Encode((unsigned char *)get_txt((sp-1)->u.str)
             ,(unsigned char *)salt
             , crypted
             , sizeof(crypted));
    sp = pop_n_elems(2, sp);
    push_c_string(sp, crypted);

    return sp;
} /* f_md5_crypt() */

/*--------------------------------------------------------------------*/
svalue_t *
v_sha1 (svalue_t *sp, int num_arg)

/* EFUN: sha1()
 *
 *   string sha1(string arg [, int iterations ] )
 *   string sha1(int *  arg [, int iterations ] )
 *
 * Create and return a SHA1 message digest from the string/array <arg>.
 * If iterations is specified to number > 0, the digest is calculated
 * using the given number of iterations.
 */

{
    SHA1Context context;
    string_t *s_digest;
    unsigned char *digest, d[SHA1HashSize];
    int i;
    p_int iterations;

    if (num_arg == 2)
    {
        iterations = sp->u.number;
        sp--;
    }
    else
        iterations = 1;

    if (iterations < 1)
    {
        errorf("Bad argument 2 to sha1(): expected a number > 0, but got %"
               PRIdPINT"\n", iterations);
        /* NOTREACHED */
        return sp;
    }

    if (add_eval_cost_n(10, iterations))
    {
        free_svalue(sp);
        put_number(sp, 0);

        /* The interpreter loop will catch the exceeded evaluation cost. */
        return sp;
    }

    if (sp->type == T_POINTER)
    {
        string_t * arg;
        char * argp;

        memsafe(arg = alloc_mstring(VEC_SIZE(sp->u.vec)), VEC_SIZE(sp->u.vec)
               , "sha1 argument string");
        argp = get_txt(arg);

        for (i = 0; i < VEC_SIZE(sp->u.vec); i++)
        {
            svalue_t *item = get_rvalue(sp->u.vec->item + i, NULL);
            if (item == NULL)
                item = sp->u.vec->item + i;
            if (item->type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 1 to sha1(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
            }
            argp[i] = (char)item->u.number & 0xff;
        }

        free_svalue(sp);
        put_string(sp, arg);
    }

    SHA1Reset(&context);
    SHA1Input(&context, (unsigned char *)get_txt(sp->u.str), mstrsize(sp->u.str));
    SHA1Result(&context, d);

    while (--iterations > 0)
    {
        SHA1Reset(&context);
        SHA1Input(&context, d, sizeof(d));
        SHA1Result(&context, d);
    }

    memsafe(s_digest = alloc_mstring(2 * SHA1HashSize)
           , 2 & SHA1HashSize, "sha1 encryption result");
    digest = (unsigned char *)get_txt(s_digest);

    for (i = 0; i < SHA1HashSize; i++)
        sprintf((char *)digest+2*i, "%02x", d[i]);

    free_svalue(sp);
    put_string(sp, s_digest);

    return sp;
} /* v_sha1() */

/*-------------------------------------------------------------------------*/
#if (!defined(USE_TLS) || !defined(HAS_OPENSSL)) && !defined(USE_GCRYPT) 
Bool
get_digest (int num, digest_t * md, size_t *len)

/* Determine the proper digest descriptor <*md> and length <*len>
 * from the designator <num>, which is one of the TLS_HASH_ constants.
 *
 * Return MY_FALSE if the desired digest isn't available.
 */

{
    switch(num)
    {
        case TLS_HASH_MD5:
            *md = num;
            *len = 16;
            break;

        case TLS_HASH_SHA1:
            *md = num;
            *len = SHA1HashSize;
            break;

        default:
            return MY_FALSE;
    }

    return MY_TRUE;
} /* get_digest() */

/*-------------------------------------------------------------------------*/
void
calc_digest (digest_t md, void *dest, size_t destlen, void *msg, size_t msglen, void *key, size_t keylen)

/* Calculates the hash or the HMAC if <key> != NULL from <msg> as determined
 * by method <md> as it was returned by get_digest().
 */
{
    if(key)
    {
        errorf("hmac() is not supported without OpenSSL or GCrypt.\n");
        /* NOTREACHED */
    }
    else
        switch(md)
        {
            case TLS_HASH_MD5:
            {
                M_MD5_CTX context;

                MD5Init(&context);
                MD5Update(&context, msg, msglen);
                MD5Final(&context, dest);
                break;
            }
            case TLS_HASH_SHA1:
            {
                SHA1Context context;

                SHA1Reset(&context);
                SHA1Input(&context, msg, msglen);
                SHA1Result(&context, dest);
                break;
            }
            break;
        }
}

#endif

/*-------------------------------------------------------------------------*/
svalue_t *
v_hash (svalue_t *sp, int num_arg)

/* EFUN hash()
 *
 *   string hash(int method, string arg [, int iterations ] )
 *   string hash(int method, int *  arg [, int iterations ] )
 *
 * Calculate the hash from <arg> as determined by <method>. The
 * hash is calculated with <iterations> iterations, default is 1 iteration.
 *
 * <method> is one of the TLS_HASH_ constants defined in tls.h; not
 * all recognized methods may be supported for a given driven.
 */

{
    string_t *digest;
    digest_t md;
    char *tmp;
    size_t hashlen;
    p_int iterations;
    int i;

    if (num_arg == 3)
    {
        iterations = sp->u.number;
        sp--;

        inter_sp = sp;
    }
    else
        iterations = 1;

    if (iterations < 1)
    {
        errorf("Bad argument 3 to hash(): expected a number > 0, but got %ld\n"
              , (long) iterations);
        /* NOTREACHED */
        return sp;
    }

    if (add_eval_cost_n(10, iterations))
    {
        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, 0);

        /* The interpreter loop will catch the exceeded evaluation cost. */
        return sp;
    }

    if (sp->type == T_POINTER)
    {
        string_t * arg;
        char * argp;

        memsafe(arg = alloc_mstring(VEC_SIZE(sp->u.vec)), VEC_SIZE(sp->u.vec)
               , "hash argument string");
        argp = get_txt(arg);

        for (i = 0; i < VEC_SIZE(sp->u.vec); i++)
        {
            svalue_t *item = get_rvalue(sp->u.vec->item + i, NULL);
            if (item == NULL)
                item = sp->u.vec->item + i;
            if (item->type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 2 to hash(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
            }
            argp[i] = (char)item->u.number & 0xff;
        }

        free_svalue(sp);
        put_string(sp, arg);
    }

    if (!get_digest(sp[-1].u.number, &md, &hashlen))
    {
        errorf("Bad argument 1 to hash(): hash function %d unknown or unsupported.\n", (int) sp[-1].u.number);
    }

    memsafe(tmp = xalloc_with_error_handler(hashlen), hashlen, "hash result");
    sp = inter_sp;

    // sp points to the error handler, sp-1 is the message.
    calc_digest(md, tmp, hashlen, get_txt(sp[-1].u.str), mstrsize(sp[-1].u.str), NULL, 0);

    while (--iterations > 0)
        calc_digest(md, tmp, hashlen, tmp, hashlen, NULL, 0);

    memsafe(digest = alloc_mstring(2 * hashlen), 2 & hashlen, "hex hash result");
    for (i = 0; i < hashlen; i++)
        sprintf(get_txt(digest)+2*i, "%02x", tmp[i] & 0xff);

    free_svalue(sp--); /* The error handler. */
    free_svalue(sp--); /* The message. */
    free_svalue(sp);   /* The method. */
    put_string(sp, digest);

    return sp;
} /* v_hash() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_hmac(svalue_t *sp)

/* EFUN hmac()
 *
 *   string hmac(int method, string key, string arg)
 *   string hmac(int method, string key, int * arg)
 *
 * Calculate the Hashed Message Authenication Code for <arg> based
 * on the digest <method> and the password <key>. Return the HMAC.
 *
 * <method> is one of the TLS_HASH_ constants defined in tls.h; not
 * all recognized methods may be supported for a given driven.
 */

{
    string_t *digest;
    digest_t md;
    char *tmp;
    size_t hashlen;
    int i;

    if (sp->type == T_POINTER)
    {
        string_t * arg;
        char * argp;

        memsafe(arg = alloc_mstring(VEC_SIZE(sp->u.vec)), VEC_SIZE(sp->u.vec)
               , "hash argument string");
        argp = get_txt(arg);

        for (i = 0; i < VEC_SIZE(sp->u.vec); i++)
        {
            svalue_t *item = get_rvalue(sp->u.vec->item + i, NULL);
            if (item == NULL)
                item = sp->u.vec->item + i;
            if (item->type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 2 to hmac(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
            }
            argp[i] = (char)item->u.number & 0xff;
        }

        free_svalue(sp);
        put_string(sp, arg);
    }

    if (!get_digest(sp[-2].u.number, &md, &hashlen))
    {
        errorf("Bad argument 1 to hmac(): hash function %d unknown or unsupported.\n", (int) sp[-2].u.number);
    }

    memsafe(tmp = xalloc_with_error_handler(hashlen), hashlen, "hash result");
    sp = inter_sp;

    calc_digest(md, tmp, hashlen
               , get_txt(sp[-1].u.str), mstrsize(sp[-1].u.str)
               , get_txt(sp[-2].u.str), mstrsize(sp[-2].u.str));

    memsafe(digest = alloc_mstring(2 * hashlen)
           , 2 & hashlen, "hmac result");
    for (i = 0; i < hashlen; i++)
        sprintf(get_txt(digest)+2*i, "%02x", tmp[i] & 0xff);

    free_svalue(sp--); /* The error handler. */
    free_svalue(sp--); /* The message. */
    free_svalue(sp--); /* The key. */
    free_svalue(sp);   /* The method. */
    put_string(sp, digest);

    return sp;
} /* f_hmac */

/*-------------------------------------------------------------------------*/
svalue_t *
f_regexp_package (svalue_t *sp)

/* EFUN regexp()
 *
 *   int regexp_package()
 *
 * Return which regexp package is used by default:
 *   RE_TRADITIONAL: traditional regexps
 *   RE_PCRE:        PCRE
 */

{
    p_int pkg = 0;

    if (driver_hook[H_REGEXP_PACKAGE].u.number)
        pkg = driver_hook[H_REGEXP_PACKAGE].u.number;
    else
        pkg = regex_package;

    push_number(sp, pkg);
    return sp;
} /* f_regexp_package() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_regexp (svalue_t *sp)

/* EFUN regexp()
 *
 *   string *regexp(string *list, string pattern)
 *   string *regexp(string *list, string pattern, int opt)
 *
 * Match the pattern pattern against all strings in list, and return a
 * new array with all strings that matched. This function uses the
 * same syntax for regular expressions as ed(), and can be given
 * additional options <opt> for the pattern interpretation.
 */

{
    vector_t *v;               /* The vector to match */
    regexp_t *reg;             /* compiled regexp */
    CBool *res;                /* res[i] true -> v[i] matches */
    mp_int num_match, v_size;  /* Number of matches, size of <v> */
    vector_t *ret;             /* The result vector */
    string_t * pattern;        /* The pattern passed in */
    int        opt;            /* The RE options passed in */
    int        rc;             /* Resultcode from the rx_exec() call */
    mp_int i, j;

    v = (sp-2)->u.vec;
    pattern = (sp-1)->u.str;
    opt = (int)sp->u.number;
    ret = NULL;

    do {
        /* Simple case: empty input yields empty output */
        if ((v_size = (mp_int)VEC_SIZE(v)) == 0)
        {
            ret = allocate_array(0);
            break;
        }

        /* Compile the regexp (or take it from the cache) */
        reg = rx_compile(pattern, opt, MY_FALSE);
        if (reg == NULL)
        {
            break;
        }

        /* Check every string in <v> if it matches and set res[]
         * accordingly.
         * Allocate memory and push error handler on the stack.
         */
        res = xalloc_with_error_handler(v_size * sizeof(*res));
        if (!res)
        {
            free_regexp(reg);
            errorf("Out of memory (%"PRIdMPINT" bytes) in regexp()",
                    v_size * sizeof(*res));
            /* NOTREACHED */
            return sp;
        }
        sp = inter_sp;

        for (num_match = i = 0; i < v_size; i++)
        {
            svalue_t *item = get_rvalue(v->item + i, NULL);
            svalue_t tmp_line = { T_NUMBER };
            string_t *line;

            res[i] = MY_FALSE;

            if (item == NULL)
            {
                struct protected_range_lvalue *r = v->item[i].u.protected_range_lvalue;
                if (r->vec.type != T_STRING)
                    continue;

                item = &tmp_line;
                assign_rvalue_no_free(item, v->item + i);
            }
            else if (item->type != T_STRING)
                continue;

            if (add_eval_cost(1))
            {
                /* Evalution cost exceeded: we abort matching at this point
                 * and let the interpreter detect the exception.
                 */
                free_svalue(&tmp_line);
                break;
            }

            line = item->u.str;
            rc = rx_exec(reg, line, 0);
            free_svalue(&tmp_line);
            if (rc == 0)
                continue;
            if (rc < 0)
            {
                const char * emsg = rx_error_message(rc, reg);
                free_regexp(reg);
                errorf("regexp: %s\n", emsg);
                /* NOTREACHED */
                return NULL;
            }

            res[i] = MY_TRUE;
            num_match++;
        }

        /* Create the result vector and copy the matching lines */
        ret = allocate_array(num_match);
        for (j=i=0; i < v_size && j < num_match; i++) {
            if (!res[i])
                continue;
            assign_rvalue_no_free(&ret->item[j++], &v->item[i]);
        }
        /* Free regexp and the intermediate buffer res by freeing the error
         * handler. */
        free_regexp(reg);
        free_svalue(sp--);
    } while(0);

    free_svalue(sp--);
    free_svalue(sp--);
    free_svalue(sp);
    if (ret == NULL)
        put_number(sp, 0);
    else
        put_array(sp, ret);

    return sp;
} /* f_regexp() */

/*-------------------------------------------------------------------------*/
/* The found delimiter matches in f_regexplode() are kept in a list of these
 * structures.
 */
struct regexplode_match {
    size_t start, end;              /* Start and end of the match in text */
    struct regexplode_match *next;  /* Next list element */
};

/* We need a special error handling for f_reg_explode(). It allocates a
 * chained list of regexplode_match structures in a mempool and the compiled
 * regexp which we have to free.
 */
struct regexplode_cleanup_s {
    error_handler_t head;
    regexp_t *reg;
    Mempool matchmempool;
};

static void
regexplode_error_handler( error_handler_t * arg)
/* The error handler: delete the mempool and free the compiled regexp.
 * Note: it is static, but the compiler will have to emit a function and 
 * symbol for this because the address of the function is taken and it is 
 * therefore not suitable to be inlined.
 */
{
    struct regexplode_cleanup_s *handler = (struct regexplode_cleanup_s *)arg;
    
    if (handler->reg)
        free_regexp(handler->reg);
    
    if (handler->matchmempool) {
        mempool_delete(handler->matchmempool);
    }
    xfree(handler);
} /* regexplode_error_handler() */

svalue_t *
f_regexplode (svalue_t *sp)

/* EFUN regexplode()
 *
 *   string *regexplode (string text, string pattern)
 *   string *regexplode (string text, string pattern, int opt)
 *
 * Explode the <text> by the delimiter <pattern> (interpreted according
 * to <opt> if given), returning a vector of the exploded text.
 * If flag RE_OMIT_DELIM is not set, then every second element in the result
 * vector will be the text that matched the delimiter.
 * Evalcost: number of matches.
 */

{

    string_t *text;                    /* Input string */
    string_t *pattern;                 /* Delimiter pattern from the vm stack */
    regexp_t *reg;                     /* Compiled pattern */
    struct regexplode_match *matches;  /* List of matches */
    struct regexplode_match **matchp;  /* Pointer to previous_match.next */
    struct regexplode_match *match;    /* Current match structure */
    vector_t *ret;                     /* Result vector */
    svalue_t *svp;                     /* Next element in ret to fill in */
    int num_match;                     /* Number of matches */
    p_int arraysize;                   /* Size of result array */
    int       opt;                     /* RE options */
    int       rc;                      /* Result from rx_exec() */
    size_t    start;                   /* Start position for match */
    Mempool   pool;                    /* Mempool for the list of matches */
    /* cleanup structure holding the head of chain of matches */
    struct regexplode_cleanup_s *cleanup;

    
    /* Get the efun arguments */
    text = sp[-2].u.str;
    pattern = sp[-1].u.str;
    opt = (int)sp->u.number;
    
    /* allocate space for cleanup structure. */
    cleanup = xalloc(sizeof(*cleanup));
    if (!cleanup)
        errorf("Out of memory (%zu bytes) for cleanup structure in "
               "regexplode().\n",sizeof(*cleanup));

    /* create mempool */
    pool = new_mempool(size_mempool(sizeof(*matches)));
    if (!pool)
    {
        xfree(cleanup);
        errorf("Out of memory (%zu) for mempool in regexplode().\n",
               sizeof(*matches));
    }
    cleanup->matchmempool = pool;
    cleanup->reg = NULL;
    /*  push error handler above the args on the stack */
    sp = push_error_handler(regexplode_error_handler, &(cleanup->head));
        
    reg = rx_compile(pattern, opt, MY_FALSE);
    if (reg == 0) {
        errorf("Unrecognized search pattern");
        /* NOTREACHED */
        return sp;
    }
    cleanup->reg = reg;
    
    /* Loop over <text>, repeatedly matching it against the pattern,
     * until all matches have been found and recorded.
     */
    start = 0;
    num_match = 0;
    matches = NULL;
    matchp = &matches;
    while ((rc = rx_exec(reg, text, start)) > 0)
    {
        if (add_eval_cost(1))
        {
            /* Evaluation cost exceeded: terminate matching early, but
             * let the interpreter loop handle the exception.
             */
            rc = 0;
            break;
        }

        match = mempool_alloc(pool, sizeof *match);
        if (!match)
        {
            errorf("Out of memory (%zu bytes) in regexplode().\n",
                   sizeof(*match));
            /* NOTREACHED */
            return sp;
        }
        rx_get_match(reg, text, &(match->start), &(match->end));
        start = match->end;
        /* add match to the match list */
        *matchp = match;
        matchp = &match->next;
        num_match++;
        
        if (start == mstrsize(text))
            break;
        if (start == match->start)
        {
            /* Empty match, we have to skip the character. */
            bool error = false;
            size_t next = char_to_byte_index(get_txt(text) + start, mstrsize(text) - start, 1, &error);
            if (error || !next)
                break;

            start += next;
            if (start == mstrsize(text))
                break;
        }
    }

    if (rc < 0) /* Premature abort on error */
    {
        const char * emsg = rx_error_message(rc, reg);
        errorf("regexp: %s\n", emsg);
        /* NOTREACHED */
        return NULL;
    }

    *matchp = 0; /* Terminate list properly */

    /* Prepare the result vector */
    if (opt & RE_OMIT_DELIM)
        arraysize = num_match+1;
    else
        arraysize = 2 * num_match + 1;

    if (max_array_size && arraysize > (long)max_array_size-1 ) {
        errorf("Illegal array size: %"PRIdPINT".\n", arraysize);
        /* NOTREACHED */
        return sp;
    }
    ret = allocate_array(arraysize);

    /* Walk down the list of matches, extracting the text parts and matched 
     * delimiters, copying them into ret.
     */
    svp = ret->item;
    start = 0;
    for (match = matches; match; match = match->next) {
        mp_int len;
        string_t *txt;

        /* Copy the text leading up to the current delimiter match. */
        len = match->start - start;
        if (len)
        {
            memsafe(txt = mstr_extract(text, start, match->start-1), 
                    (size_t)len, "text before delimiter");
            put_string(svp, txt);
        }
        else
            put_ref_string(svp, STR_EMPTY);
        
        svp++;

        /* Copy the matched delimiter */
        if (!(opt & RE_OMIT_DELIM))
        {
            len = match->end - match->start;
            if (len)
            {
                memsafe(txt = mstr_extract(text, match->start, match->end-1), (size_t)len, "matched delimiter");
                put_string(svp, txt);
            }
            else
                put_ref_string(svp, STR_EMPTY);
            
            svp++;
        }

        start = match->end;
    }

    /* Copy the remaining text (maybe the empty string) */
    {
        size_t len;
        string_t *txt;

        len = mstrsize(text) - start;
        if (len > 0)
        {
            memsafe(txt = mstr_extract(text, start, mstrsize(text)-1), (size_t)len, "remaining text");
            put_string(svp, txt);
        }
        else
            put_ref_string(svp, STR_EMPTY);
    }

    /* Cleanup: free error handler and 3 arguments. Freeing the error handler
     * will free the regexp and the chain of matches. */
    sp = pop_n_elems(4, sp);

    /* Return the result */
    sp++;
    put_array(sp, ret);
    return sp;
} /* f_regexplode() */

/*-------------------------------------------------------------------------*/
/* The found delimiter matches are kept in a list of these structures.
 */
struct regreplace_match {
    size_t start, end;              /* Start and end of the match in text */
    string_t *sub;                  /* Substituted string (counted ref) */
    struct regreplace_match *next;  /* Next list element */
};

/* To facilitate automatic cleanup of the temporary structures in case
 * of an error, the following structure will be pushed onto the VM stack
 * as T_ERROR_HANDLER.
 */

struct regreplace_cleanup_s {
    error_handler_t head;  /* The link to the error handler function */

    struct regreplace_match *matches;  /* List of matches */
    regexp_t *reg;                     /* Compiled pattern */
};

static void
regreplace_cleanup (error_handler_t * arg)
{
    struct regreplace_cleanup_s * data = (struct regreplace_cleanup_s *)arg;
    struct regreplace_match *match;

    free_regexp(data->reg);
    for (match = data->matches; match != NULL;)
    {
        struct regreplace_match * next = match->next;

        if (match->sub)
            free_mstring(match->sub);
        xfree(match);

        match = next;
    }
    xfree(arg);
} /* regreplace_cleanup() */

/*-------------------------------------------------------------------------*/
svalue_t*
f_regreplace (svalue_t *sp)

/* EFUN regreplace()
 *
 *     string regreplace (string txt, string pattern, closure|string replace
 *                                                  , int flags)
 *
 * Search through <txt> for one/all occurrences of <pattern> and replace them
 * with the <replace> pattern, returning the result.
 * <replace> can be a string, or a closure returning a string. If it is
 * a closure, it will be called with the matched substring and
 * the position at which it was found as arguments.
 *
 * <flags> is the bit-or of the regexp options, including:
 *   RE_GLOBAL       = 1: when given, all occurrences of <pattern> are replace,
 *                        else just the first
 *
 * The function behaves like the s/<pattern>/<replace>/<flags> command
 * in sed or vi. It offers an efficient and far more powerful replacement
 * for implode(regexplode()).
 */

{
    int       flags;                   /* RE options */
    string_t *sub = NULL;              /* Replacement string */
    svalue_t *subclosure = NULL;       /* Replacement closure */
    string_t *text;                    /* Input string */
    string_t *pattern;                 /* Delimiter pattern from the vm stack */
    string_t *result;                  /* Result string */
    char     *dst;                     /* Result copy pointer */
    regexp_t *reg;                     /* Compiled pattern */
    struct regreplace_match **matchp;  /* Pointer to previous_match.next */
    struct regreplace_match *match;    /* Current match structure */
    int       num_matches;             /* Number of matches */
    int       rc;                      /* Result from rx_exec() */
    size_t    start;                   /* Start position for match */
    size_t    reslen;                  /* Result length */

    struct regreplace_cleanup_s * rcp;

    /* Must set inter_sp before call to rx_compile(),
     * because it might call errorf().
     */
    inter_sp = sp;

    /* Extract the arguments */
    flags = sp->u.number;
    if (sp[-1].type == T_STRING)
    {
        sub = sp[-1].u.str;
        subclosure = NULL;
    }
    else /* it's a closure */
    {
        sub = NULL;
        subclosure = sp-1;
    }

    pattern = sp[-2].u.str;
    text = sp[-3].u.str;

    reg = rx_compile(pattern, flags, MY_FALSE);
    if (reg == 0)
    {
        errorf("Unrecognized search pattern");
        /* NOTREACHED */
        return sp;
    }

    /* Create the automatic cleanup structure */
    rcp = xalloc(sizeof(*rcp));
    if (!rcp)
    {
        free_regexp(reg);
        errorf("(regreplace) Out of memory: (%lu bytes) for cleanup structure\n"
             , (unsigned long)sizeof(*rcp));
    }

    rcp->reg = reg;
    rcp->matches = NULL;

    sp = push_error_handler(regreplace_cleanup, &(rcp->head));

    /* Loop over <text>, repeatedly matching it against the pattern,
     * until all matches have been found and recorded.
     */
    start = 0;
    num_matches = 0;
    matchp = &(rcp->matches);
    reslen = 0;
    while ((rc = rx_exec(reg, text, start)) > 0)
    {
        if (add_eval_cost(1))
        {
            /* Evaluation cost exceeded: terminate the matching early,
             * but let the interpreter handle the exception.
             */
            rc = 0;
            break;
        }

        xallocate(match, sizeof(*match), "regreplace match structure");
        rx_get_match(reg, text, &(match->start), &(match->end));
        match->sub = NULL;
        match->next = NULL;
        *matchp = match;
        matchp = &match->next;
        num_matches++;

        /* Compute the replacement string */
        /* Determine the replacement pattern.
         */
        if (subclosure != NULL)
        {
            mp_int len;
            string_t *matched_text;

            len = match->end - match->start;
            if (len)
            {
                matched_text = mstr_extract(text, match->start, match->end-1);
                if (!matched_text)
                {
                    outofmem((size_t)len, "matched text");
                    /* NOTREACHED */
                    return NULL;
                }
            }
            else
                matched_text = ref_mstring(STR_EMPTY);

            push_string(inter_sp, matched_text); /* Gives up the ref */
            push_number(inter_sp, match->start);
            call_lambda(subclosure, 2);
            transfer_svalue(&apply_return_value, inter_sp);
            inter_sp--;

            if (apply_return_value.type != T_STRING)
            {
                errorf("Invalid type for replacement pattern: %s, expected string.\n", typename(apply_return_value.type));
                /* NOTREACHED */
                return NULL;
            }

            sub = apply_return_value.u.str;
        }

        match->sub = rx_sub(reg, text, sub);
        if (!match->sub)
        {
            outofmemory("substituted string");
            /* NOTREACHED */
            return NULL;
        }

        /* Count the length(s) */
        reslen += match->start - start;
        reslen += mstrsize(match->sub);

        /* Prepare for the next match
         * Avoid another rx_exec() call if we are at the end.
         */
        start = match->end;

        if (start > mstrsize(text))
            break;
        if (match->start == start)
        {
            /* Empty match leaves old char in place */
            bool error = false;
            size_t next = char_to_byte_index(get_txt(text) + start, mstrsize(text) - start, 1, &error);
            if (error || !next)
                break;

            reslen += next;
            start += next;
        }

        /* If RE_GLOBAL is not set, don't look for a second match */
        if (num_matches && (flags & RE_GLOBAL) == 0)
            break;
    } /* while(matches) */

    if (rc < 0) /* Premature abort on error */
    {
        const char * emsg = rx_error_message(rc, reg);
        errorf("regexp: %s\n", emsg);
        /* NOTREACHED */
        return NULL;
    }

    /* Add the remaining length */
    reslen += mstrsize(text) - start;

    /* Prepare the result string */
    result = alloc_mstring(reslen);
    if (!result)
    {
        outofmem(reslen, "result string");
        /* NOTREACHED */
        return NULL;
    }

    /* Walk down the list of matches, extracting the
     * text parts and substitute strings, copying them
     * into the result.
     */
    dst = get_txt(result);
    start = 0;
    for (match = rcp->matches; match; match = match->next)
    {
        size_t len;

        /* Copy the text leading up to the current delimiter match. */
        len = match->start - start;
        if (len)
        {
            memcpy(dst, get_txt(text)+start, len);
            dst += (size_t)len;
        }

        /* Copy the substitute string */
        len = mstrsize(match->sub);
        if (len)
        {
            memcpy(dst, get_txt(match->sub), len);
            dst += (size_t)len;
        }

        start = match->end;
    }

    /* Copy the remaining text if any */
    {
        size_t len;

        len = mstrsize(text) - start;
        if (len)
        {
            memcpy(dst, get_txt(text)+start, len);
            dst += (size_t)len;
        }
    }

    result->info.unicode = is_ascii(get_txt(result), reslen) ? STRING_ASCII : STRING_UTF8;

    /* Cleanup */
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp--;
    free_svalue(sp);

    /* Return the result */
    put_string(sp, result);
    return sp;
} /* f_regreplace() */

/*-------------------------------------------------------------------------*/
svalue_t*
v_regmatch (svalue_t *sp, int num_arg)

/* EFUN regmatch()
 *
 *     string    regmatch (string txt, string pattern)
 *     string[*] regmatch (string txt, string pattern, int flags)
 *     string[*] regmatch (string txt, string pattern, int flags, int start)
 *
 * Match the string <txt> against <pattern>, which is interpreted according
 * to the RE options given in <flags>. If <start> is given, it is the start
 * position for the match and must be in the range [0..strlen(txt)].
 *
 * If there is no match, the result is 0. If there is a match, the exact
 * result is determined by the flag RE_MATCH_SUBS:
 *
 * If the flag RE_MATCH_SUBS is not set, the result is the matched expression.
 *
 * If the flag RE_MATCH_SUBS is set, the result is an array of the matched
 * string(s) of the first match. Entry [0] is the full string matching the
 * <pattern>, following entries are the string segments matching
 * parenthesized subexpressions in <pattern>. If a particular subexpression
 * didn't have a match, the corresponding array entry will be 0.
 * The last entry in the array will be the new start index in case you
 * want to repeat the match on the remaining parts of the string. This new
 * index is usually equal the length of the match, but at least one higher
 * than the original start index.
 */

{
    svalue_t *argp;     /* Arguments */
    regexp_t *reg;      /* The compiled RE */
    int       flags;    /* RE options */
    size_t    startpos; /* Match start argument */
    string_t *text;     /* Input string */
    string_t *pattern;  /* Delimiter pattern from the vm stack */
    int       rc;       /* Result from rx_exec() */
    vector_t *result;   /* Result vector */
    string_t *resstr;   /* Result string */

    /* Must set inter_sp before call to rx_compile(),
     * because it might call errorf().
     */
    inter_sp = sp;

    /* Extract the arguments */
    argp = sp - num_arg + 1;
    text = argp[0].u.str;
    pattern = argp[1].u.str;
    flags = startpos = 0;
    if (num_arg > 2)
    {
        flags = argp[2].u.number;
        if (num_arg > 3)
        {
            startpos = (size_t)argp[3].u.number;
            if (startpos > mstrsize(text))
            {
                errorf("regmatch(): Start index out of range: %zu, "
                      "should be in [0..%zu]\n",
                      startpos, mstrsize(text)
                     );
                /* NOTREACHED */
                startpos = 0;
            }

            if (startpos == mstrsize(text))
            {
                /* No match possible - return right here */
                sp -= 2; /* No need to free_svalue() the known two integers */
                free_svalue(sp); /* Pattern */
                sp--;
                free_svalue(sp); /* Text */
                put_number(sp, 0);
                return sp;
            }

            sp--;
        }
        sp--;

        num_arg = 2;
    }

    reg = rx_compile(pattern, flags, MY_FALSE);
    if (reg == 0) {
        errorf("Unrecognized match pattern");
        /* NOTREACHED */
        return sp;
    }

    rc = rx_exec(reg, text, startpos);
    if (rc < 0)
    {
        const char * emsg = rx_error_message(rc, reg);
        free_regexp(reg);
        errorf("regexp: %s\n", emsg);
        /* NOTREACHED */
        return NULL;
    }

    result = NULL;
    resstr = NULL;
    if (rc != 0)
    {
        if (flags & RE_MATCH_SUBS)
        {
            int num_matches = rx_num_matches(reg);
            int i;

            if (max_array_size && num_matches+1 > (long)max_array_size-1 ) {
                free_regexp(reg);
                inter_sp = sp;
                errorf("Illegal array size: %d", num_matches+1);
                /* NOTREACHED */
                return sp;
            }
            result = allocate_array(num_matches+1);
            if (!result)
            {
                free_regexp(reg);
                outofmemory("result array");
                /* NOTREACHED */
                return NULL;
            }

            for (i = 0; i < num_matches; i++)
            {
                size_t start, end;

                if (!rx_get_match_n(reg, text, i, &start, &end)
                 || start > end
                   )
                {
                    put_number(&(result->item[i]), 0);
                }
                else if (start == end)
                {
                    put_ref_string(result->item + i, STR_EMPTY);
                }
                else
                {
                    string_t *str = mstr_extract(text, start, end-1);

                    if (!str)
                    {
                        free_regexp(reg);
                        free_array(result);
                        outofmem(end-start, "matched string");
                        /* NOTREACHED */
                        return NULL;
                    }

                    put_string(&(result->item[i]), str);
                }
            } /* for (i) */

            /* As last element, store the length of the match to give
             * the new starting position.
             */
            {
                size_t new_start;
                if (result->item[0].type == T_STRING)
                    new_start = mstrsize(result->item[0].u.str);
                else
                    new_start = 0;
                if (new_start == 0)
                    new_start++;
                put_number(&(result->item[num_matches]), (long)(startpos+new_start));
            }
        }
        else
        {
            size_t start, end;

            rx_get_match(reg, text, &start, &end);
            if (start > end)
            {
                resstr = NULL;
            }
            else if (start == end)
            {
                resstr = ref_mstring(STR_EMPTY);
            }
            else
            {
                resstr = mstr_extract(text, start, end-1);

                if (!resstr)
                {
                    free_regexp(reg);
                    outofmem(end-start, "matched string");
                    /* NOTREACHED */
                    return NULL;
                }
            }
        } /* if (flag & RE_MATCH_SUBS) */
    } /* if (rc > 0) */

    /* Cleanup */
    free_regexp(reg);
    free_svalue(sp); /* Pattern */
    sp--;
    free_svalue(sp); /* Text */

    /* Return the result */
    if (result)
        put_array(sp, result);
    else if (resstr)
        put_string(sp, resstr);
    else
        put_number(sp, 0);
    return sp;
} /* v_regmatch() */

/*-------------------------------------------------------------------------*/
static svalue_t *
find_string (svalue_t *sp, bool forward)

/* Implements the efuns strstr (forward = true) and strrstr (forward = false).
 *
 * Returns the index of <sp-1> in <sp-2> searching from position <sp>
 * forward (forward = true) or backward (forward = false).
 * If the string is not found, -1 is returned. The returned
 * index is relativ to the beginning of the string.
 *
 * If pos is negativ, it counts from the end of the string.
 */

{
    const char *found;
    string_t *base, *pattern;
    p_int start, rc;

    base = sp[-2].u.str;
    pattern = sp[-1].u.str;

    if (sp[-2].type != sp[-1].type)
        efun_arg_error(2, sp[-2].type, sp[-1].type, sp);

    if ( 0 != (start = sp->u.number) )
    {
        if (base->info.unicode == STRING_UTF8)
        {
            /* Need to translate char index into bytes. */
            bool error = false;

            if (start < 0)
            {
                size_t len = byte_to_char_index(get_txt(base), mstrsize(base), &error);
                if (error)
                    errorf("Invalid character in string at index %zd.\n", len);
                start += len;
                if (start < 0)
                    start = 0;
            }

            if (start > 0)
            {
                size_t pos = char_to_byte_index(get_txt(base), mstrsize(base), start, &error);
                if (error)
                    errorf("Invalid character in string at byte %zd.\n", pos);

                start = pos;
            }
        }
        else if (start < 0)
        {
            start += mstrsize(base);
            if (start < 0)
                start = 0;
        }
    }

    if (forward)
        found = mstring_mstr_n_str(base, start, get_txt(pattern), mstrsize(pattern));
    else
        found = mstring_mstr_rn_str(base, start, get_txt(pattern), mstrsize(pattern));

    rc = found ? (found - get_txt(base)) : -1;

    if (rc > 0 && base->info.unicode == STRING_UTF8)
    {
        bool error = false;
        rc = byte_to_char_index(get_txt(base), rc, &error);
    }

    sp--;
    free_svalue(sp--);
    free_string_svalue(sp); /* Frees base ! */
    put_number(sp, rc);

    return sp;
} /* find_string() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_strstr (svalue_t *sp)

/* EFUN strstr()
 *
 *   int strstr (string str, string str2, int pos)
 *   int strstr (bytes str, bytes str2, int pos)
 *
 * Returns the index of str2 in str searching from position pos forward.
 * If str2 is not found in str, -1 is returned. The returned
 * index is relativ to the beginning of the string.
 *
 * If pos is negativ, it counts from the end of the string.
 */

{
    return find_string(sp, true);
} /* f_strstr() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_strrstr (svalue_t *sp)

/* EFUN strrstr()
 *
 *   int strrstr (string str, string str2, int pos)
 *   int strrstr (bytes str, bytes str2, int pos)
 *
 * Returns the index of str2 in str searching from position pos backward.
 * If str2 is not found in str, -1 is returned. The returned
 * index is relativ to the beginning of the string.
 *
 * If pos is negativ, it counts from the end of the string.
 */

{
    return find_string(sp, false);
} /* f_strrstr() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_trim (svalue_t *sp, int num_arg)

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
    char def_ch[4]       /* Buffer for single characters to strip */
      = { '\t', ' ' };
    char *strip;         /* String of characters to strip */
    size_t strip_l;      /* Length of *strip */
    p_int  where;

    /* Get and test the arguments */
    argp = sp - num_arg + 1;

    strarg = argp->u.str;
    str = get_txt(strarg);
    strarg_l = mstrsize(strarg);

    if (num_arg > 1)
    {
        where = argp[1].u.number;
        if (!where)
            where = TRIM_BOTH;
        if (where > TRIM_BOTH)
            errorf("Bad argument 2 to trim(): illegal value %"PRIdPINT"\n",
                   where);
    }
    else
        where = TRIM_BOTH;

    if (num_arg > 2)
    {
        if (argp[2].type == T_NUMBER)
        {
            size_t clen;

            if (argp[2].u.number < 0)
                errorf("Bad argument 3 to trim(): %"PRIdPINT
                       " is not a character\n", argp[2].u.number);

            clen = unicode_to_utf8(argp[2].u.number, def_ch);
            strip = def_ch;
            strip_l = clen;
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
        for (left = str; left < end; )
        {
            bool error = false;
            size_t clen = char_to_byte_index(left, end - left, 1, &error);
            char* chars = strip;
            size_t chars_l = strip_l - clen + 1;

            if (error || clen > strip_l)
                break;

            for (; chars_l; chars_l--, chars++)
            {
                if (!memcmp(left, chars, clen))
                    break;
            }

            if (!chars_l)
                break;

            left += clen;
        }
    }
    else
        left = str;

    if (where & TRIM_RIGHT && end != left)
    {
        for (right = end; right > left; )
        {
            char* prev = utf8_prev(right, right - left);
            char* chars = strip;
            size_t clen = right - prev;
            size_t chars_l = strip_l - clen + 1;

            if (!clen || clen > strip_l)
                break;

            for (; chars_l; chars_l--, chars++)
            {
                if (!memcmp(prev, chars, clen))
                    break;
            }

            if (!chars_l)
                break;

            right = prev;
        }
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
        memsafe(trimmed = new_n_unicode_mstring(left, newlen), newlen, "trimmed result");
        free_string_svalue(argp);
        put_string(argp, trimmed);
    }

    /* argp+2 might need to be freed, but argp+1 is always just a number.
     * And the result argp is fine as it is.
     */

    if (num_arg > 2 && argp[2].type == T_STRING)
        free_svalue(argp+2);

    return argp;
} /* v_trim() */

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
    return change_case(sp, true);
} /* f_upper_case() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_text_width (svalue_t *sp)

/* EFUN text_width()
 *
 *    int text_width (string s)
 *
 * Calculate the screen width of <s>.
 */

{
    string_t* str = sp->u.str;
    char *s = get_txt(str);
    size_t len = mstrsize(str);
    p_int width = 0, col = 0;

    for (size_t pos = 0; pos < len;)
    {
        switch (s[pos])
        {
            case '\t':
                pos++;
                col += 8 - (col % 8);
                break;

            case '\n':
                pos++;
                if (col > width)
                    width = col;
                col = 0;
                break;

            default:
            {
                int gwidth;
                size_t glen = next_grapheme_break(s + pos, len - pos, &gwidth);

                if (!glen)
                    errorf("Invalid character in string at index %zd.\n", pos);

                col += gwidth;
                pos += glen;
                break;
            }
        }
    }

    if (col > width)
        width = col;

    free_mstring(str);
    put_number(sp, width);

    return sp;
} /* f_text_width() */

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
    if (z < lens[i])
        return MY_FALSE;
    for (i++; i < imax; i++) {
        if (lens[i] > 0)
            return MY_FALSE;
    }
    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
static size_t
chop_string (char* str, size_t oldsize, size_t newsize)

/* Auxiliary function for e_terminal_colour().
 *
 * We need to reduce the string from <oldsize> to <newsize>.
 * Find the position of the first character tbat is not
 * fully within newsize and return that position.
 */

{
    size_t pos = 0, idx = 0;
    while (pos != newsize)
    {
        p_int c;
        size_t clen = utf8_to_unicode(str + pos, oldsize - pos, &c);
        if (!clen)
            errorf("Invalid character in string at index %zd.\n", idx);

        if (pos + clen > newsize)
            return pos;

        idx++;
        pos += clen;
    }

    return pos;
} /* chop_string() */

/*-------------------------------------------------------------------------*/
static string_t *
e_terminal_colour ( string_t * text, mapping_t * map, svalue_t * cl
                  , int indent, int wrap
                  )

/* Implementation of the efun terminal_colour().
 * See f_terminal_colour() for the complete description.
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
        errorf("(terminal_colour) indent %ld > wrap %ld\n"
             , (long)indent, (long)wrap);
        /* NOTREACHED */
        return NULL;
    }

    instr = get_txt(text);
    num_tmp = 0;

    /* Find the first occurance of the magic character pair.
     * If found, duplicate the input string into instr and
     * let cp point into that copy at the delimiter.
     * If not found (or no mapping/closure given), cp will be NULL.
     */

    if (map != NULL || cl != NULL)
    {
        p_int left = mstrsize(text);

        cp = instr;
        do {
            char * last_cp = cp;
            cp = memchr(cp, TC_FIRST_CHAR, (size_t)left);
            if (cp)
            {
                if (cp[1] == TC_SECOND_CHAR)
                {
                    memsafe(savestr = dup_mstring(text), mstrsize(text)
                           , "working string");
                    cp = get_txt(savestr) + (cp - instr);
                    instr = get_txt(savestr);

                    /* Check for the special escape '%%^^'.
                     * If found, modify it to '%^%^, and let cp
                     * point to it.
                     */
                    if (cp > get_txt(savestr)
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

                /* Single '%': skip it and continue searching */
                cp++;
                left -= (cp - last_cp);
            }
        } while (cp && left > 0);

        if (left <= 0)
            cp = NULL;
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
            savestr = NULL;  /* should be NULL already anyway */
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
                errorf("(terminal_colour) Illegal type for default entry: %s, expected string or closure.\n", typename(cl->type));
                /* NOTREACHED */
                return text;
            }
        }

        /* cp here points to the first delimiter found */

        parts = CALLOCATE( NSTRSEGS, char * );
        if (!parts)
        {
            errorf("(terminal_colour) Out of memory (%lu bytes) "
                  "for %d parts.\n"
                 , (unsigned long) NSTRSEGS * sizeof(char*), NSTRSEGS);
            /* NOTREACHED */
            return NULL;
        }

        lens = CALLOCATE(NSTRSEGS, p_int);
        if (!lens)
        {
            xfree(parts);
            errorf("(terminal_colour) Out of memory (%lu bytes) "
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
        while (cp && left > 0)
        {
            /* Skip the delimiter found last and search the next */
            cp += 2;
            instr = cp;
            left -= 2;

            do
            {
                char * last_cp = cp;
                cp = memchr(cp, TC_FIRST_CHAR, left);
                if (cp) {
                    left -= (cp - last_cp);
                    if (cp[1] == TC_SECOND_CHAR)
                    {
                        /* Check for the special escape '%%^^'.
                         * If found, modify it to '%^%^, and let cp
                         * point to it.
                         */
                        if (cp > get_txt(savestr)
                         && cp[-1] == TC_FIRST_CHAR
                         && cp[2] == TC_SECOND_CHAR
                           )
                        {
                            cp--;
                            cp[1] = TC_SECOND_CHAR;
                            cp[2] = TC_FIRST_CHAR;
                            left++;
                        }
                        break;
                    }
                    cp++;
                    left--;
                }
            } while (cp && left > 0);
            if (left <= 0)
                cp = NULL;

            if (cp)
            {
                /* Another delimiter found: put it into the parts array.
                 */
                parts[num] = instr;
                lens[num] = cp - instr;
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
            lens[num] = strlen(instr); /* Note: left is 0 here */
            num++;
        }
    } /* if (delimiter found or not) */

    /* If required, allocate the mdata save array on the stack */
    if (!no_keys)
    {
        vector_t *vec;

        vec = allocate_array_unlimited(num/2 + 1);
          /* Slightly bigger than required */
        mdata_save = vec->item;
        push_array(inter_sp, vec);
        num_tmp++;
    }

    /* Do the the keyword replacement and calculate the lengths.
     * The lengths are collected in the lens[] array to save the
     * need for repeated strlens().
     */
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
            {
                str = find_tabled_str_n(parts[i], lens[i], STRING_UTF8);
            }
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
                    push_c_n_string(inter_sp, parts[i], lens[i]);
                    call_lambda(cl, 1);
                    *mdata_save = *inter_sp;
                    inter_sp--;
                    mdata = mdata_save++;
                    if (mdata->type != T_STRING)
                    {
                        errorf("(terminal_colour) Closure did not return a string.\n");
                        /* NOTREACHED */
                        return NULL;
                    }
                }
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
        if (indent_overflows || j >= MAX_STRING_LENGTH)
        {
            /* We are already at the maximum string length.
             * reduce the remaining parts to zero.
             */
            lens[i] = 0;
        }
        else if (lens[i] > 0)
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
                size_t newlen = chop_string(parts[i], lens[i], lens[i] + MAX_STRING_LENGTH - j);
                j -= lens[i] - newlen;
                lens[i] = newlen;
                indent_overflows = MY_TRUE;
            }

            /* If wrapping is requested, perform the analysis */
            if (wrap)
            {
                int   z;             /* Index into the current string */
                char *p = parts[i];  /* Pointer into the current string */

                for (z = 0; z < lens[i];)
                {
                    int width; /* Number of columns for the next grapheme. */
                    size_t clen = next_grapheme_break(p + z, lens[i] - z, &width);
                    if (!clen)
                        errorf("Invalid character in string at index %d.\n", z);
                    z += clen;

                    if (p[z-1] == '\n')
                    {
                        /* Hard line break: start a new line */
                        col = 0;
                        start = -1;
                    }
                    else
                    {
                        bool isspace = clen == 1 && p[z-1] == ' ';

                        /* All space characters in columns before col <start>
                         * do not count.
                         */
                        if (col > start || !isspace)
                            col += width;
                        else
                        {
                            j--;
                            j_extra++;
                        }

                        /* If space, remember the position */
                        if (isspace)
                            space = col;

                        if (col > wrap && col > width)
                        {
                            /* Wrapping necessary */

                            if (space)
                            {
                                /* Break the line at the last space */
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
                                        for ( ; !done && test_z < lens[test_i]; )
                                        {
                                            int testwidth;
                                            size_t testclen = next_grapheme_break(parts[test_i] + test_z, lens[test_i] - test_z, &testwidth);
                                            if (!testclen)
                                                errorf("Invalid character in string at index %d.\n", test_z);
                                            test_z += testclen;

                                            if (parts[test_i][test_z-1] == '\n'
                                             || (testclen == 1 && parts[test_i][test_z-1] == ' '))
                                            {
                                                done = MY_TRUE;
                                                break;
                                            }
                                            next_word_len += testwidth;
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

                    if (col || z != lens[i])
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
                        /* The newline and indentation resulted in an overflow.
                         */
                        if (j - MAX_STRING_LENGTH <= lens[i] - z)
                        {
                            /* We can do the line break and indentation.
                             * Just strip a little of the end of this part.
                             */
                            size_t newlen = chop_string(parts[i], lens[i], lens[i] + MAX_STRING_LENGTH - j);
                            j -= lens[i] - newlen;
                            lens[i] = newlen;
                            indent_overflows = MY_TRUE;
                        }
                        else
                        {
                            /* The end is right here,
                             * Let's break it up at the line break.
                             */
                            indent_overflows = MY_TRUE;

                            z -= clen;

                            if (!maybe_at_end)
                                j -= indent;
                            j -= lens[i] - z;
                            lens[i] = z;
                            break;
                        }
                    }

                    space = 0;
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
                size_t newlen = chop_string(parts[i], -lens[i], -lens[i] + MAX_STRING_LENGTH - j);
                j -= -lens[i] - newlen;
                lens[i] = -newlen;
                indent_overflows = MY_TRUE;
            }
        } /* if (lens[i] > 0) */
    } /* for (i = 0..num) for wrapping analysis */


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

        size_t tmpmem_size;
        char *tmpmem;
          /* Temporary buffer for the current line */
        char *pt;
          /* Pointer into tmpmem */
        char *spacept;
          /* Pointer to the last space (corresponding to the space variable). */

        tmpmem_size = (size_t)j+j_extra+1;
          /* Actually, the allocated '+j_extra' size is never used, but
           * it makes the sanity check below simpler.
           */
        xallocate(tmpmem, tmpmem_size, "temporary string");

        col = 0;
        start = -1;
        space = 0;
        spacept = NULL;
        pt = tmpmem;

        /* Loop over all parts */
        for (i = 0; i < num; i++)
        {
            int kind;            /* The kind of a line break */
            int spill;           /* Bytes to spill into the next line */
            p_int l = lens[i];   /* Length of current part */
            char *p = parts[i];  /* Current part */

            if (pt - tmpmem + ((l < 0) ? -l : l) >= (ptrdiff_t)tmpmem_size)
            {
                xfree(tmpmem);
                free_mstring(deststr);
                errorf("Partial string '%s' too long (%td+%"PRIdPINT" >= %zu).\n"
                     , p
                     , (ptrdiff_t)(pt - tmpmem), ((l < 0) ? -l : l)
                     , tmpmem_size);
                /* NOTREACHED */
                return NULL;
            }

            if (l < 0)
            {
                /* String retrieved from the mapping: not to be counted */
                memcpy(pt, p, (size_t)-l);
                pt += -l;
                continue;
            }

            /* Loop over the current part, copying and wrapping */
            for (k = 0; k < l; )
            {
                int n, width;
                size_t clen = next_grapheme_break(p + k, l - k, &width);
                if (!clen)
                {
                    xfree(tmpmem);
                    free_mstring(deststr);
                    errorf("Invalid character in string at index %d.\n", k);
                }

                /* Copy the character into tmpmem */
                memcpy(pt, p + k, clen);
                pt += clen;
                k += clen;

                if (p[k-1] == '\n')
                {
                    /* Start a new line */
                    col = 0;
                    spill = 0;
                    kind = 0;
                    start = -1;
                }
                else
                {
                    bool isspace = clen == 1 && p[k-1] == ' ';

                    /* All space characters in columns before col <start>
                     * do not count.
                     */
                    if (col > start || !isspace)
                        col += width;
                    else
                        pt--;

                    /* If space, remember the position */
                    if (isspace)
                    {
                        space = col;
                        spacept = pt;
                    }

                    /* Wrapping necessary? */
                    if (col > wrap && col > width)
                    {
                        if (space)
                        {
                            /* Break at last space */
                            int next_word_len = 0;

                            if (col - space > 2)
                            {
                                /* Check if the current word is too
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
                                    for ( ; !done && test_k < lens[test_i]; )
                                    {
                                        int testwidth;
                                        size_t testclen = next_grapheme_break(parts[test_i] + test_k, lens[test_i] - test_k, &testwidth);
                                        if (!testclen)
                                        {
                                            xfree(tmpmem);
                                            free_mstring(deststr);
                                            errorf("Invalid character in string at index %d.\n", test_k);
                                        }
                                        test_k += testclen;

                                        if (parts[test_i][test_k-1] == '\n'
                                         || (testclen == 1 && parts[test_i][test_k-1] == ' '))
                                        {
                                            done = MY_TRUE;
                                            break;
                                        }
                                        next_word_len += testwidth;
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
                                spill = clen;
                                kind = 2;
                            }
                            else
                            {
                                col -= space;
                                spill = pt - spacept;
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
                            spill = clen;
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
                 *
                 * col denotes the number of characters we have to take
                 * over into the next line.
                 */

                /* Determine the length of the _previous_ (and therefore
                 * wrapped) line and copy it from tmpmem into deststr.
                 */
                n = (pt - tmpmem) - spill;
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
                memmove(tmpmem, tmpmem + n, (size_t)spill);
                pt = tmpmem + spill;

                /* If we are indenting, check if we have to add the
                 * indentation space.
                 * Note: if kind == 2, it's the current character which
                 *   will go onto the next line, otherwise it's the next
                 *   character will. The difference is important in the
                 *   call to at_end().
                 */
                if (indent != 0
                 && (   col > 0
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

                space = 0;
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

    if ( lens )
      xfree(lens);
    if ( parts )
      xfree(parts);
    if (savestr)
      free_mstring(savestr);
    while (num_tmp > 0)
    {
        free_svalue(inter_sp);
        inter_sp--;
        num_tmp--;
    }

    deststr->info.unicode = is_ascii(get_txt(deststr), mstrsize(deststr)) ? STRING_ASCII : STRING_UTF8;

    /* now we have what we want */
#ifdef DEBUG
    if ((long)(cp - get_txt(deststr)) != j)
    {
      fatal("Length miscalculated in terminal_colour()\n"
            "    Expected: %i (or %i) Was: %td\n"
            "    In string: %.*s\n"
            "    Out string: %.*s\n"
            "    Indent: %i Wrap: %i, indent overflow: %s\n"
           , j, wrap
           , (ptrdiff_t)(cp - get_txt(deststr))
           , (int)mstrsize(text), get_txt(text)
           , (int)mstrsize(deststr), get_txt(deststr)
           , indent, wrap
           , indent_overflows ? "true" : "false"
           );
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
v_terminal_colour (svalue_t *sp, int num_arg)

/* EFUN terminal_colour()
 *
 *   varargs string terminal_colour( string str, mapping|closure map,
 *                                   int wrap, int indent )
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
 */

{
    int        indent = 0;
    int        wrap = 0;
    string_t * str;
    mapping_t * map = NULL;
    svalue_t * cl = NULL;

    if ( num_arg >= 3 )
    {
        if ( num_arg == 4 )
        {
            indent = (sp--)->u.number;
            if (indent < 0)
            {
                errorf("terminal_colour() requires an indent >= 0.\n");
                /* NOTREACHED */
                return sp;
            }
        }
        wrap = (sp--)->u.number;
        if (wrap < 0)
        {
            errorf("terminal_colour() requires a wrap >= 0.\n");
            /* NOTREACHED */
            return sp;
        }
    }

    if (sp->type == T_MAPPING)
    {
        map = sp->u.map;
        if (map->num_values < 1)

        {
            errorf("terminal_colour() requires a mapping with values.\n");
            /* NOTREACHED */
            return sp;
        }
        cl = NULL;
    }
    else if (sp->type == T_CLOSURE)
    {
        map = NULL;
        cl = sp;
    }
    else
    {
        map = NULL;
        cl = NULL;
    }

    inter_sp = sp;

    str = e_terminal_colour(sp[-1].u.str, map, cl, indent, wrap);

    free_svalue(sp--);
    free_svalue(sp);
    put_string(sp, str);

    return sp;
} /* v_terminal_colour() */

#ifdef USE_PROCESS_STRING
/*-------------------------------------------------------------------------*/
static string_t *
process_value (const char *str, Bool original)

/* Helper function for process_string(): take a function call in <str>
 * in the form "function[:objectname]{|arg}" and try to call it.
 * If the function exists and returns a string, the result is an uncounted
 * pointer to the string (which itself is referenced by apply_return_value).
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
    if (strlen(str) < 1 || !isalpha((unsigned char)(str[0])))
        return NULL;

    if (current_object->flags & O_DESTRUCTED)
        return NULL;

    /* If necessary, copy the argument so that we can separate the various
     * parts with \0 characters.
     */
    if (original)
    {
        /* allocate memory and push error handler */
        func = xalloc_with_error_handler(strlen(str)+1);
        if (!func)
            errorf("Out of memory (%zu bytes) in process_value().\n"
                 , strlen(str)+1);
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
    if ( NULL == (func2 = find_tabled_str(func, STRING_UTF8)) )
    {
        /* free the error handler if necessary. */
        if (original)
            free_svalue(inter_sp--);
        return NULL;
    }


    /* Get the object */
    if (!obj)
        ob = current_object;
    else
    {
        string_t *objstr;

        memsafe(objstr = new_unicode_mstring(obj), strlen(obj), "object name");
        ob = find_object(objstr);
        free_mstring(objstr);
    }

    if (!ob || (ob->flags & O_DESTRUCTED))
    {
        /* free the error handler if necessary. */
        if (original)
            free_svalue(inter_sp--);
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
    
    /* Apply the function */
    ret = apply(func2, ob, numargs);

    /* Free func by freeing the error handler (if we allocated func).
     * Has to be done now, after the arguments have been popped by apply().
     */
    if (original)
        free_svalue(inter_sp--);
    
    /* see if adequate answer is returned by the apply(). */
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
 * Searches string str for occurrences of a "value by function
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
        ; il < VEC_SIZE(vec)
        ; il++)
    {
        string_t *p0, *p2;

        p0 = vec->item[il].u.str;

        /* The entry might be a function call */
        p2 = process_value(get_txt(p0), MY_TRUE);
        if (p2)
        {
            /* Yup, it is: reference the result */
            p2 = ref_mstring(p2);
            ch_last = MY_TRUE;
            changed = MY_TRUE;
        }
        else
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
} /* f_process_string() */

#endif /* USE_PROCESS_STRING */

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
       * NULL for 'no match' or 'all matched'.
       */
    Bool           match_req;
      /* Before a match: TRUE if the subsequent chars need to match as well.
       */
    Bool           no_match;
      /* After a match: TRUE if there was a mismatch in the non-% match.
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
static INLINE mp_int
sscanf_add_digit (mp_int num, char c, struct sscanf_info *info, bool *overflow)

/* Add the digit <c> to the numer <num> and return the result.
 * If there is a numeric overflow, set *<overflow> to true.
 */

{
    c -= '0';
    if (info->sign)
    {
        if (num < (PINT_MIN + c) / 10)
            *overflow = true;
        return num * 10 - c;
    }
    else
    {
        if (num > (PINT_MAX - c) / 10)
            *overflow = true;
        return num * 10 + c;
    }
} /* sscanf_add_digit() */

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
    char c, *strp = str;
    bool overflow = false;

    num = 0;

    i = (mp_int)info->min;
    if (i > 0)
    {
        /* The number must have at least i digits */
        info->field -= i;
        do
        {
            if (!lexdigit(c = *strp))
            {
                if (info->fmt_end[-1] != 'd')
                {
                    /* Signal no match for this %D/%U,
                     * so that a previous %s can retry.
                     */
                    info->match_end = NULL;
                }
                else
                {
                    /* Stop the whole sscanf(). */
                    info->match_end = str;
                    info->fmt_end = sscanf_format_str_end;
                }
                return;
            }
            strp++;
            num = sscanf_add_digit(num, c, info, &overflow);
        } while (--i);
    }

    /* There can be info->field more digits */
    i = (mp_int)info->field;
    while  (--i >= 0)
    {
        if (!lexdigit(c = *strp))
            break;
        strp++;
        num = sscanf_add_digit(num, c, info, &overflow);
    }

    if (overflow)
    {
        debug_message("%s Numeric overflow: sscanf for %s%.*s\n",
            time_stamp(), info->sign ? "-" : "", (int)(strp - str), str);

        /* Stop matching.
         * fmt_end needs to point to zero character after a format specifier.
         */
        info->match_end = str;
        info->fmt_end = sscanf_format_str_end;
        return;
    }

    info->match_end = strp;

    if (info->flags.do_assign)
    {
        /* Assign the parsed number */
        if (info->arg_current >= info->arg_end)
            return;

        tmp_svalue.u.number = num;
        transfer_svalue(info->arg_current++, &tmp_svalue);
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
    info->match_req = MY_FALSE;

    for (;;)
    {
        switch(c = *fmt++)
        {
        case '+':
            info->match_req = MY_TRUE;
            continue;

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
            if (info->arg_current >= info->arg_end)
            {
                info->match_end = NULL;
                return NULL;
            }
            else
            {
                svalue_t *val = get_rvalue(info->arg_current, NULL);
                if (!val || val->type != T_NUMBER)
                {
                    info->match_end = NULL;
                    return NULL;
                }
                else
                {
                    info->arg_current++;
                    *nump = (mp_uint)(val->u.number);
                }
            }
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
            errorf("Bad type : '%%%c' in sscanf fmt string.\n", fmt[-1]);
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

    info->no_match = MY_FALSE;

    /* Loop over the format string, matching characters */
    for (;;)
    {
        if ( !(c = *fmt) )
        {
            info->match_end = str;
            info->fmt_end = sscanf_format_str_end;
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
            info->no_match = MY_TRUE;
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
        info->fmt_end = sscanf_format_str_end;
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

    if (b == a && (b != '%' || *fmt == '%'))
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
                /* The last '%' belongs to a %-spec. */
                n >>= 1;
                fmt-=2;
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
svalue_t *
v_sscanf (svalue_t *sp, int num_arg)

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

    /* Initialize sscanf_format_str_end.
     */
    sscanf_format_str_end = "d";
    sscanf_format_str_end++;

    inter_sp = sp; /* we can have an errorf() deep inside */
    arg0 = sp - num_arg + 1;

    /* First get the string to be parsed.
     */
    in_string = get_txt(arg0[0].u.str);

    /* Now get the format description.
     */
    fmt = get_txt(arg0[1].u.str);

    info.arg_end = arg0 + num_arg;
    info.arg_current = arg0 + 2;
    info.match_req = MY_FALSE;

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
                    memsafe(matchstr = new_n_unicode_mstring(in_string, (size_t)num)
                           , num, "matchstring");
                    put_string(&sv_tmp, matchstr);
                    transfer_svalue(arg, &sv_tmp);
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

    /* If the characters after the last % specifiers didn't match
     * undo the % match.
     */
    if (info.match_req && info.no_match && info.number_of_matches > 0)
        info.number_of_matches--;

    sp = pop_n_elems(num_arg, sp);
    push_number(sp, info.number_of_matches);
    return sp;
} /* v_sscanf() */


/*=========================================================================*/
/*                              OBJECTS                                    */

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
        obj = get_object(sp->u.str);
        if (!obj)
        {
            errorf("Object not found: %s\n", get_txt(sp->u.str));
            /* NOTREACHED */
            return sp;
        }
    }
    else
    {
        efun_gen_arg_error(1, sp->type, sp);
        /* NOTREACHED */
        return sp;
    }

    if ((obj->flags & O_SWAPPED) && load_ob_from_swap(obj) < 0)
        errorf("Out of memory: unswap object '%s'.\n", get_txt(obj->name));

    blueprint = NULL;
    if (obj->prog != NULL
     && obj->prog->blueprint != NULL
     && !(obj->prog->blueprint->flags & O_DESTRUCTED)
       )
        blueprint = ref_object(obj->prog->blueprint, "blueprint()");

    free_svalue(sp);
    if (blueprint != NULL)
        put_object(sp, blueprint);
    else
        put_number(sp, 0);

    return sp;
} /* f_blueprint() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_clones (svalue_t *sp, int num_arg)

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
 *
 * If the driver is compiled with DYNAMIC_COSTS, the cost of this
 * efun is proportional to the number of objects in the game.
 */

{
    string_t  *name;     /* The (tabled) load-name to search */
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
                reference = get_object(sp->u.str);
                if (!reference) {
                    errorf("Object not found: %s\n", get_txt(sp->u.str));
                    /* NOTREACHED */
                    return sp;
                }
            }
            else /* it's a number */
            {
                what = sp->u.number;
                if (what < 0 || what > 2) {
                    errorf("Bad num arg 1 to clones(): got %d, expected 0..2\n"
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
                errorf("Bad num arg 2 to clones(): got %d, expected 0..2\n"
                     , what);
                /* NOTREACHED */
                return sp;
            }

            free_svalue(sp--); inter_sp = sp;

            if (sp->type == T_OBJECT)
                reference = sp->u.ob;
            else if (sp->type == T_STRING)
            {
                reference = get_object(sp->u.str);
                if (!reference)
                {
                    errorf("Object not found: %s\n", get_txt(sp->u.str));
                    /* NOTREACHED */
                    return sp;
                }
            }
            else
            {
                vefun_exp_arg_error(1, TF_STRING|TF_OBJECT, sp->type, sp);
                /* NOTREACHED */
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
                    errorf("(clones) Out of memory (%lu bytes) for increased "
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
    (void)add_eval_cost(checked / 100 + found / 256);
#endif /* DYNAMIC_COSTS */

    /* Create the result and put it onto the stack */
    if (max_array_size && found > max_array_size)
    {
        xfree(ores);
        errorf("Illegal array size: %zu\n", found);
        /* NOTREACHED */
        return sp;
    }
    res = allocate_uninit_array(found);
    if (!res)
    {
        xfree(ores);
        errorf("(clones) Out of memory: array[%zu] for result.\n",
               found);
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
} /* v_clones() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_configure_object (svalue_t *sp)

/* EFUN configure_object()
 *
 *    void configure_object(object ob, int what, mixed data)
 *
 * Configures several aspects of the object <ob>, or the default for all objects
 * if <ob> is 0.
 *
 * <what> == OC_...
 *
 * If the first argument <ob> is not this_object(), the privilege violation
 * ("configure_object", this_object(), ob, what, data) occurs.
 */

{
    object_t *ob;

    if (sp[-2].type == T_OBJECT)
        ob = sp[-2].u.ob;
    else
        ob = NULL;

    if ((current_object->flags & O_DESTRUCTED)
     || ((ob != current_object || sp[-1].u.number == OC_EUID)
      && !privilege_violation_n(STR_CONFIGURE_OBJECT, ob, sp, 2)))
    {
        sp = pop_n_elems(3, sp);
        return sp;
    }

    switch (sp[-1].u.number)
    {
    default:
        errorf("Illegal value %"PRIdPINT" for configure_object().\n", sp[-1].u.number);
        return sp; /* NOTREACHED */

    case OC_COMMANDS_ENABLED:
        if (!ob)
            errorf("Default value for OC_COMMANDS_ENABLED is not supported.\n");
        if (sp->type != T_NUMBER)
            efun_arg_error(2, T_NUMBER, sp->type, sp);

        if (sp->u.number)
            ob->flags |= O_ENABLE_COMMANDS;
        else
            ob->flags &= ~O_ENABLE_COMMANDS;
        break;

    case OC_HEART_BEAT:
        if (!ob)
            errorf("Default value for OC_HEART_BEAT is not supported.\n");
        if (sp->type != T_NUMBER)
            efun_arg_error(2, T_NUMBER, sp->type, sp);

        set_heart_beat(ob, sp->u.number != 0);
        break;

    case OC_EUID:
        if (!ob)
            errorf("Default value for OC_EUID is not supported.\n");

        if (sp->type == T_NUMBER)
        {
            if (sp->u.number != 0)
                efun_arg_error(2, T_STRING, sp->type, sp);

            ob->eff_user = 0;
        }
        else if (sp->type == T_STRING)
        {
            ob->eff_user = add_name(sp->u.str);
        }
        else
            efun_arg_error(2, T_STRING, sp->type, sp);

        break;
    }

    sp = pop_n_elems(3, sp);
    return sp;
} /* f_configure_object() */

/*-------------------------------------------------------------------------*/
static void
assert_ob_not_swapped (object_t *ob)

/* Makes sure that <ob> is swapped by swapping in
 * or throwing an error otherwise.
 */

{
    if ((ob->flags & O_SWAPPED) && load_ob_from_swap(ob) < 0)
        errorf("Out of memory: unswap object '%s'.\n", get_txt(ob->name));
} /* assert_ob_not_swapped */

/*-------------------------------------------------------------------------*/
svalue_t *
f_object_info (svalue_t *sp)

/* EFUN object_info()
 *
 *    mixed object_info(object ob, int what)
 *
 * Return information about the object <ob>.
 * <what> can either be a configuration option as given to
 * configure_object() or one of the OI_xxx options.
 */

{
    object_t *ob;
    svalue_t result;

    if (sp[-1].type == T_OBJECT)
        ob = sp[-1].u.ob;
    else
        ob = NULL;

    if (!ob && sp[0].u.number < 0)
        errorf("There is no default value for non-configuration values.\n");

    switch (sp[0].u.number)
    {
    default:
        errorf("Illegal value %"PRIdPINT" for object_info().\n", sp[0].u.number);
        return sp; /* NOTREACHED */

    /* Configuration */
    case OC_COMMANDS_ENABLED:
        if (!ob)
            errorf("Default value for OC_COMMANDS_ENABLED is not supported.\n");
        put_number(&result, (ob->flags & O_ENABLE_COMMANDS) ? 1 : 0);
        break;

    case OC_HEART_BEAT:
        if (!ob)
            errorf("Default value for OC_HEART_BEAT is not supported.\n");
        put_number(&result, (ob->flags & O_HEART_BEAT) ? 1 : 0);
        break;

    case OC_EUID:
        if (!ob)
            errorf("Default value for OC_EUID is not supported.\n");
        if (ob->eff_user && ob->eff_user->name)
            put_ref_string(&result, ob->eff_user->name);
        else
            put_number(&result, 0);
        break;

    /* Object flags */
    case OI_ONCE_INTERACTIVE:
        put_number(&result, (ob->flags & O_ONCE_INTERACTIVE) ? 1 : 0);
        break;

    case OI_RESET_STATE:
        put_number(&result, (ob->flags & O_RESET_STATE) ? 1 : 0);
        break;

    case OI_WILL_CLEAN_UP:
        put_number(&result, (ob->flags & O_WILL_CLEAN_UP) ? 1 : 0);
        break;

    case OI_LAMBDA_REFERENCED:
        put_number(&result, (ob->flags & O_LAMBDA_REFERENCED) ? 1 : 0);
        break;

    case OI_REPLACED:
        put_number(&result, (ob->flags & O_REPLACED) ? 1 : 0);
        break;

    /* Program flags */
    case OI_NO_INHERIT:
        assert_ob_not_swapped(ob);
        put_number(&result, (ob->prog->flags & P_NO_INHERIT) ? 1 : 0);
        break;

    case OI_NO_CLONE:
        assert_ob_not_swapped(ob);
        put_number(&result, (ob->prog->flags & P_NO_CLONE) ? 1 : 0);
        break;

    case OI_NO_SHADOW:
        assert_ob_not_swapped(ob);
        put_number(&result, (ob->prog->flags & P_NO_SHADOW) ? 1 : 0);
        break;

    case OI_SHARE_VARIABLES:
        assert_ob_not_swapped(ob);
        put_number(&result, (ob->prog->flags & P_SHARE_VARIABLES) ? 1 : 0);
        break;

    /* Swapping */
    case OI_SWAPPED:
        put_number(&result, (ob->flags & O_SWAPPED) ? 1 : 0);
        break;

    case OI_PROG_SWAPPED:
        put_number(&result, O_PROG_SWAPPED(ob) ? 1 : 0);
        break;

    case OI_VAR_SWAPPED:
        put_number(&result, O_VAR_SWAPPED(ob) ? 1 : 0);
        break;

    case OI_SWAP_NUM:
        put_number(&result, O_SWAP_NUM(ob));
        break;

    /* Timing */
    case OI_NEXT_RESET_TIME:
        put_number(&result, ob->time_reset);
        break;

    case OI_NEXT_CLEANUP_TIME:
        put_number(&result, ob->time_cleanup);
        break;

    case OI_LAST_REF_TIME:
        put_number(&result, ob->time_of_ref);
        break;

    /* Object list */
    case OI_OBJECT_NEXT:
        if (ob->next_all)
            put_ref_object(&result, ob->next_all, "object_info(OI_OBJECT_NEXT)");
        else
            put_number(&result, 0);
        break;

    case OI_OBJECT_PREV:
        if (ob->prev_all)
            put_ref_object(&result, ob->prev_all, "object_info(OI_OBJECT_PREV)");
        else
            put_number(&result, 0);
        break;

    case OI_OBJECT_POS:
        {
            int pos = 0;
            object_t *pos_ob = obj_list;

            for (; pos_ob; pos_ob = pos_ob->next_all)
            {
                if (pos_ob == ob)
                    break;
                pos++;
            }

            if (!pos_ob)
                pos = -1;
            put_number(&result, pos);
            break;
        }

    /* Shadows */
    case OI_SHADOW_NEXT:
        {
            object_t *sh = (ob->flags & O_SHADOW) ? O_GET_SHADOW(ob)->shadowed_by : NULL;
            if (sh)
                put_ref_object(&result, sh, "object_info(OI_SHADOW_NEXT)");
            else
                put_number(&result, 0);
            break;
        }

    case OI_SHADOW_PREV:
        {
            object_t *sh = (ob->flags & O_SHADOW) ? O_GET_SHADOW(ob)->shadowing : NULL;
            if (sh)
                put_ref_object(&result, sh, "object_info(OI_SHADOW_PREV)");
            else
                put_number(&result, 0);
            break;
        }

    case OI_SHADOW_ALL:
        {
            int num = 0;
            object_t *sh = ob;
            vector_t *vec;

            for(; sh; sh = (sh->flags & O_SHADOW) ? O_GET_SHADOW(sh)->shadowed_by : NULL)
                num++;

            /* The first object is <ob> itself, so skipping that. */
            vec = allocate_array(num-1);
            num = 0;
            for(sh = ob; sh; sh = (sh->flags & O_SHADOW) ? O_GET_SHADOW(sh)->shadowed_by : NULL)
            {
                if(num)
                    put_ref_object(vec->item + num - 1, sh, "object_info(OI_SHADOW_ALL)");
                num++;
            }

            put_array(&result, vec);
            break;
        }

    /* Object Statistics */
    case OI_OBJECT_REFS:
        put_number(&result, ob->ref);
        break;

    case OI_TICKS:
        put_number(&result, (p_int)ob->ticks);
        break;

    case OI_GIGATICKS:
        put_number(&result, (p_int)ob->gigaticks);
        break;

    case OI_DATA_SIZE:
    case OI_DATA_SIZE_TOTAL:
        {
            mp_int totalsize, datasize;

            assert_ob_not_swapped(ob);
            datasize = data_size(ob, &totalsize);

            put_number(&result, (sp[0].u.number == OI_DATA_SIZE) ? datasize : totalsize);
            break;
        }

    /* Program Statistics */
    case OI_PROG_REFS:
        assert_ob_not_swapped(ob);
        put_number(&result, ob->prog->ref);
        break;

    case OI_NUM_FUNCTIONS:
        assert_ob_not_swapped(ob);
        put_number(&result, ob->prog->num_functions);
        break;

    case OI_NUM_VARIABLES:
        assert_ob_not_swapped(ob);
        put_number(&result, ob->prog->num_variables);
        break;

    case OI_NUM_STRINGS:
        assert_ob_not_swapped(ob);
        put_number(&result, ob->prog->num_strings);
        break;

    case OI_NUM_INHERITED:
        assert_ob_not_swapped(ob);
        {
            /* Need to filter artificial entries. */
            int i = ob->prog->num_inherited;
            int cnt = 0;
            inherit_t *inheritp;

            for (inheritp = ob->prog->inherit; i--; inheritp++)
            {
                if (inheritp->inherit_type == INHERIT_TYPE_NORMAL
                 || inheritp->inherit_type == INHERIT_TYPE_VIRTUAL
                   )
                    cnt++;
            }

            put_number(&result, cnt);
            break;
        }

    case OI_NUM_INCLUDED:
        assert_ob_not_swapped(ob);
        put_number(&result, ob->prog->num_includes);
        break;

    case OI_SIZE_FUNCTIONS:
        assert_ob_not_swapped(ob);
        put_number(&result, (p_int)
            ( ob->prog->num_functions      * sizeof(*ob->prog->functions)
            + ob->prog->num_function_names * sizeof(*ob->prog->function_names)));
        break;

    case OI_SIZE_VARIABLES:
        assert_ob_not_swapped(ob);
        put_number(&result, (p_int)
            ( ob->prog->num_variables      * sizeof(*ob->prog->variables)));
        break;

    case OI_SIZE_STRINGS:
        assert_ob_not_swapped(ob);
        put_number(&result, (p_int)
            ( ob->prog->num_strings        * sizeof(*ob->prog->strings)));
        break;

    case OI_SIZE_STRINGS_DATA:
    case OI_SIZE_STRINGS_DATA_TOTAL:
        {
            mp_int size, total, overhead;

            assert_ob_not_swapped(ob);
            size = program_string_size(ob->prog, &overhead, &total);

            put_number(&result, (sp[0].u.number == OI_SIZE_STRINGS_DATA) ? size : total);
            break;
        }

    case OI_SIZE_INHERITED:
        assert_ob_not_swapped(ob);
        put_number(&result, (p_int)
            ( ob->prog->num_inherited      * sizeof(*ob->prog->inherit)));
        break;

    case OI_SIZE_INCLUDED:
        assert_ob_not_swapped(ob);
        put_number(&result, (p_int)
            ( ob->prog->num_includes       * sizeof(*ob->prog->includes)));

    case OI_PROG_SIZE:
        assert_ob_not_swapped(ob);
        put_number(&result, (long)(PROGRAM_END(*ob->prog) - ob->prog->program));
        break;

    case OI_PROG_SIZE_TOTAL:
        assert_ob_not_swapped(ob);
        put_number(&result, ob->prog->total_size);
        break;
    }

    sp = pop_n_elems(2, sp);

    sp++;
    *sp = result;
    return sp;

} /* f_object_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_present_clone (svalue_t *sp, int num_arg)

/* EFUN present_clone()
 *
 *    object present_clone(string str [, object env] [, [int n])
 *    object present_clone(object obj [, object env] [, [int n])
 *
 * Search in the inventory of <env> for the <n>th object with the
 * same blueprint as object <obj>, resp. for the <n>th object with
 * the loadname <str>, and return that object.
 *
 * If not found, 0 is returned.
 */

{
    string_t * name; /* the shared loadname to look for */
    object_t *obj;   /* the object under scrutiny */
    object_t *env;   /* the environment to search in */
    p_int     count; /* the <count> object is searched */

    /* Get the arguments */
    svalue_t *arg = sp - num_arg + 1;   // first argument
    env = current_object;  // default
    count = -1;
    if (num_arg == 3)
    {
        // if we got 3 args, the third must be a number.
        count = arg[2].u.number;
        // but 0 and negative ones make no sense.
        if (count <= 0)
        {
            errorf("Bad argument 3 to present_clone(): got %"PRIdPINT
                   ", expected a positive number.\n",count);
            return sp; /* NOT REACHED */
        }
        free_svalue(sp--);
        num_arg--;
    }
    if (num_arg == 2)
    {
        // the second arg may be an object or a number
        if (arg[1].type == T_NUMBER)
        {
            // But it must not be 0 (which is probably a destructed object)
            // and we don't accept two numbers (as second and third arg)
            if (arg[1].u.number == 0 || count != -1)
            {
                vefun_arg_error(2, T_OBJECT, T_NUMBER, sp);
                return sp; /* NOTREACHED */
            }
            count = arg[1].u.number;
            if (count < 0)
            {
                errorf("Bad argument 2 to present_clone(): got %"PRIdPINT
                       ", expected a positive number or an object.\n",count);
                return sp; /* NOT REACHED */
            }
        }
        else if (arg[1].type == T_OBJECT)
        {
            env = arg[1].u.ob;
        }
        free_svalue(sp--);
        num_arg--;
    }
    /* if no number given and count is still ==-1, the for loop below searches
     * implicitly for the first object */


    /* Get the name/object to search for */
    if (arg->type == T_STRING)
    {
        size_t len, i;
        char * end;
        char * sane_name;
        char * name0;  /* Intermediate name */
        char * tmpbuf; /* intermediate buffer for stripping any #xxxx */
        
        name0 = get_txt(arg->u.str);
        tmpbuf = NULL;
        
        /* Normalize the given string and check if it is
         * in the shared string table. If not, we know that
         * there is no blueprint with that name
         */

        /* First, slash off a trailing '#<num>' */

        len = mstrsize(arg->u.str);
        i = len;
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
                    tmpbuf = xalloc(i + 1);
                    if (!tmpbuf)
                    {
                        errorf("Out of memory (%zu bytes) for temporary "
                               "buffer in present_clone().\n", i+1);
                    }
                    strncpy(tmpbuf, get_txt(arg->u.str), i);
                    tmpbuf[i] = '\0';
                }

                break; /* in any case */
            }
        }
        /* if we got a clone name, tmpbuf is filled with the BP name. In any
         * case, name0 contains now the name to be used. */
        if (tmpbuf)
            name0 = tmpbuf;
        
        /* Now make the name sane */
        sane_name = (char *)make_name_sane(name0, !compat_mode);

        if (sane_name)
            name = find_tabled_str(sane_name, STRING_UTF8);
        else
            name = find_tabled_str(name0, STRING_UTF8);
        
        /* tmpbuf (and name0 which might point to the same memory) is unneeded
         * from now on. Setting both to NULL, just in case somebody uses 
         * them later below. */
        if (tmpbuf) {
            xfree(tmpbuf);
            tmpbuf = name0 = NULL;
        }
    }
    else if (arg->type == T_OBJECT)
    {
        name = arg->u.ob->load_name;
    }
    else
        vefun_exp_arg_error(1, TF_STRING|TF_OBJECT, arg->type, sp);

    obj = NULL;
    if (name)
    {
        /* We have a name, now look for the object */
        for (obj = env->contains; obj != NULL; obj = obj->next_inv)
        {
            /* check for <= is deliberate, count is -1 if no number is
             * given and then the loop is terminated upon the first object
             * matching the name. */
            if (!(obj->flags & O_DESTRUCTED) && name == obj->load_name
                && --count <= 0)
                break;
        }
    }

    /* Free first argument and assign the result */
    free_svalue(sp);
    if (obj != NULL)
        put_ref_object(sp, obj, "present_clone");
    else
        put_number(sp, 0);

    return sp;
} /* f_present_clone() */

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
    svalue_t *rvaluep;
    int left = num_arg;
    Bool gotArray = MY_FALSE;
    svalue_t *result = NULL;
    svalue_t tmp_str = { T_NUMBER };


    if (argp->type == T_POINTER)
    {
        if (num_arg > 1)
        {
           errorf("Bad arguments to %s: only one array accepted.\n", fname);
           /* NOTREACHED */
        }
        valuep = argp->u.vec->item;
        left = (int)VEC_SIZE(argp->u.vec);
        gotArray = MY_TRUE;
        if (left < 1)
        {
           errorf("Bad argument 1 to %s: array must not be empty.\n", fname);
           /* NOTREACHED */
        }
    }

    rvaluep = get_rvalue(valuep, NULL);
    if (rvaluep == NULL)
    {
        struct protected_range_lvalue *r = valuep->u.protected_range_lvalue;
        if (r->vec.type == T_POINTER)
        {
            /* We only need this for error messages. */
            rvaluep = &(r->vec);
        }
        else
        {
            rvaluep = &tmp_str;
            assign_rvalue_no_free(rvaluep, valuep);
        }
    }

    if (rvaluep->type == T_STRING || rvaluep->type == T_BYTES)
    {
        result = rvaluep;

        for (valuep++, left--; left > 0; valuep++, left--)
        {
            int cmp;
            svalue_t *item = get_rvalue(valuep, NULL);
            svalue_t item_str = { T_NUMBER };
            if (item == NULL)
            {
                struct protected_range_lvalue *r = valuep->u.protected_range_lvalue;
                if (r->vec.type != T_STRING)
                {
                    /* We only need this for error messages. */
                    item = &(r->vec);
                }
                else
                {
                    item = &item_str;
                    assign_rvalue_no_free(item, valuep);
                }
            }

            if (item->type != rvaluep->type)
            {
                free_svalue(&tmp_str);
                if (gotArray)
                    errorf("Bad argument to %s(): array[%d] is a '%s', "
                          "expected '%s'.\n"
                         , fname, (int)VEC_SIZE(argp->u.vec) - left + 1
                         , typename(item->type)
                         , typename(rvaluep->type));
                else
                    vefun_arg_error(num_arg - left + 1, rvaluep->type, item->type, sp);
                /* NOTREACHED */
            }

            cmp = mstrcmp(item->u.str, result->u.str);
            if (bMax ? (cmp > 0) : (cmp < 0))
            {
                if (item == &item_str)
                {
                    free_svalue(&tmp_str);
                    transfer_svalue_no_free(&tmp_str, &item_str);
                    result = &(tmp_str);
                }
                else
                    result = item;
            }
            else
                free_svalue(&item_str);
        }
    }
    else if (rvaluep->type == T_NUMBER || rvaluep->type == T_FLOAT)
    {
        result = rvaluep;

        for (valuep++, left--; left > 0; valuep++, left--)
        {
            svalue_t *item = get_rvalue(valuep, NULL);
            if (item == NULL)
                item = &(valuep->u.protected_range_lvalue->vec);

            if (item->type != T_FLOAT && item->type != T_NUMBER)
            {
                if (gotArray)
                    errorf("Bad argument to %s(): array[%d] is a '%s', "
                          "expected 'int' or 'float'.\n"
                         , fname, (int)VEC_SIZE(argp->u.vec) - left + 1
                         , typename(item->type));
                else
                    vefun_exp_arg_error(num_arg - left + 1, TF_NUMBER|TF_FLOAT, item->type, sp);
                /* NOTREACHED */
            }

            if (item->type == T_NUMBER && result->type == T_NUMBER)
            {
                if (bMax ? (item->u.number > result->u.number)
                         : (item->u.number < result->u.number))
                    result = item;
            }
            else
            {
                double v, r;

                if (item->type == T_FLOAT)
                    v = READ_DOUBLE(item);
                else
                    v = (double)(item->u.number);

                if (result->type == T_FLOAT)
                    r = READ_DOUBLE(result);
                else
                    r = (double)(result->u.number);

                if (bMax ? (v > r)
                         : (v < r))
                    result = item;
            }
        } /* for (values) */
    }
    else
    {
        if (gotArray)
            errorf("Bad argument to %s(): array[0] is a '%s', "
                  "expected 'string', 'bytes', 'int' or 'float'.\n"
                 , fname, typename(rvaluep->type));
        else
            vefun_exp_arg_error(1, TF_STRING|TF_BYTES|TF_NUMBER|TF_FLOAT, rvaluep->type, sp);
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

    free_svalue(&tmp_str);

    return sp;
} /* x_min_max() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_max (svalue_t *sp, int num_arg)

/* VEFUN max()
 *
 *   string|bytes max (string|bytes arg, ...)
 *   string|bytes max (string|bytes * arg_array)
 *
 *   int|float    max (int|float arg, ...)
 *   int|float    max (int|float * arg_array)
 *
 * Determine the maximum value of the <arg>uments and return it.
 * If max() is called with an array (which must not be empty) as only
 * argument, it returns the maximum value of the array contents.
 */

{
    return x_min_max(sp, num_arg, MY_TRUE);
} /* v_max() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_min (svalue_t *sp, int num_arg)

/* VEFUN min()
 *
 *   string|bytes min (string|bytes arg, ...)
 *   string|bytes min (string|bytes * arg_array)
 *
 *   int|float    min (int|float arg, ...)
 *   int|float    min (int|float * arg_array)
 *
 * Determine the minimum value of the <arg>uments and return it.
 * If min() is called with an array (which must not be empty) as only
 * argument, it returns the minimum value of the array contents.
 */

{
    return x_min_max(sp, num_arg, MY_FALSE);
} /* v_min() */

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
        if (sp->u.number == PINT_MIN)
        {
            errorf("Numeric overflow: abs(%"PRIdPINT")\n", sp->u.number);
            /* NOTREACHED */
            return NULL;
        }
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
        errorf("Bad arg 1 for asin(): value %f out of range\n", d);
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
        errorf("Bad arg 1 for acos(): value %f out of range\n", d);
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
    {
        d = atan((double)(sp->u.number));
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: atan(%"PRIdPINT")\n", sp->u.number);
    }
    else
    {
        d = atan(READ_DOUBLE(sp));
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: atan(%g)\n", READ_DOUBLE(sp));
    }
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
    if (d < (-DBL_MAX) || d > DBL_MAX)
        errorf("Numeric overflow: atan(%g, %g)\n", y, x);
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
    double d, e;

    d = READ_DOUBLE(sp);
    if (sp->type != T_FLOAT)
        d = (double)sp->u.number;
    else
        d = READ_DOUBLE(sp);
    if (d <= 0.)
        errorf("Bad arg 1 for log(): value %f out of range\n", d);
    e = log(d);
    if (e < (-DBL_MAX) || e > DBL_MAX)
        errorf("Numeric overflow: log(%g)\n", d);
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, e);

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
    {
        d = exp((double)sp->u.number);
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: exp(%"PRIdPINT")\n", sp->u.number);
    }
    else
    {
        d = exp(READ_DOUBLE(sp));
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: exp(%g)\n", READ_DOUBLE(sp));
    }
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
    double d, e;

    if (sp->type != T_FLOAT)
        d = (double)sp->u.number;
    else
        d = READ_DOUBLE(sp);
    if (d < 0.)
        errorf("Bad arg 1 for sqrt(): value %f out of range\n", d);
    e = sqrt(d);
    if (e < (-DBL_MAX) || e > DBL_MAX)
        errorf("Numeric overflow: sqrt(%g)\n", d);
    sp->type = T_FLOAT;
    STORE_DOUBLE(sp, e);

    return sp;
} /* f_sqrt() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_ceil (svalue_t *sp)

/* EFUN ceil()
 *
 *   float ceil(int|float)
 *
 * Returns the smallest whole number which is still bigger
 * than the argument. If the argument value is an integer, the result
 * will be the argument value, converted to float.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type == T_FLOAT)
    {
        d = ceil(READ_DOUBLE(sp));
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: ceil(%g)\n", READ_DOUBLE(sp));
    }
    else
    {
        d = sp->u.number;
        sp->type = T_FLOAT;
    }

    STORE_DOUBLE(sp, d);

    return sp;
} /* f_ceil() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_floor (svalue_t *sp)

/* EFUN floor()
 *
 *   float floor(int|float)
 *
 * Returns the biggest whole number which is not larger
 * than the argument. If the argument value is an integer, the result
 * will be the argument value, converted to float.
 */

{
    STORE_DOUBLE_USED
    double d;

    if (sp->type == T_FLOAT)
    {
        d = floor(READ_DOUBLE(sp));
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: floor(%g)\n", READ_DOUBLE(sp));
    }
    else
    {
        d = sp->u.number;
        sp->type = T_FLOAT;
    }

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
        errorf("Can't raise 0 to negative powers.\n");
    if (x < 0.0  && y != (double)((long)y))
        errorf("Can't raise negative number to fractional powers.\n");
    d = pow(x, y);
    if (d < (-DBL_MAX) || d > DBL_MAX)
        errorf("Numeric overflow: pow(%g, %g)\n", x, y);
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
 * variable- and lfun-closures are converted into their variable
 * resp.function index.
 * Integers are just returned.
 */

{
    p_int n;

    switch(sp->type)
    {
    default:
        fatal("Bad arg 1 to to_int(): type %s\n", typename(sp->type));
        break;

    case T_FLOAT:
      {
        double d;

        d = READ_DOUBLE(sp);
        if (d < (-DBL_MAX) || d > DBL_MAX)
            errorf("Numeric overflow: to_int(%g)\n", d);
        n = (long)d;
        break;
      }

    case T_STRING:
      {
        unsigned long num = 0;
        char * end;
        char * cp = get_txt(sp->u.str);
        Bool hasMinus = MY_FALSE;
        Bool overflow;

        /* Check if the number begins with a '-' or '+' */
        while (*cp && isspace(*cp)) cp++;
        if (*cp == '-' || *cp == '+')
        {
            hasMinus = (*cp == '-');
            cp++;
        }

        end = lex_parse_number(cp, &num, &overflow);
        if (end != cp)
        {
            if (overflow || ((p_int)num)<0)
            {
                n = hasMinus ? PINT_MIN : PINT_MAX;
            }
            else
            {
                n = (p_int)num;
                if (hasMinus)
                    n = -n;
            }
        }
        else
            n = 0;

        free_string_svalue(sp);
        break;
      }

    case T_CLOSURE:
        if (sp->x.closure_type == CLOSURE_IDENTIFIER)
            n = sp->u.lambda->function.var_index;
        else if (sp->x.closure_type == CLOSURE_LFUN)
            n = sp->u.lambda->function.lfun.index;
        else
            errorf("Bad arg 1 to to_int(): not a lfun or variable closure.\n");
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
 * structs, symbols, strings, and closures.
 *
 * Converts variable/lfun closures and structs to the appropriate names.
 */

  {
    char buf[1024];
    string_t *s;

    s = NULL;
    buf[sizeof(buf)-1] = '\0';
    switch(sp->type)
    {
    default:
        errorf("Bad arg 1 to to_string(): type %s\n", typename(sp->type));
        break;

    case T_NUMBER:
        sprintf(buf,"%"PRIdPINT, sp->u.number);
        if (buf[sizeof(buf)-1] != '\0')
            fatal("Buffer overflow in to_string(): "
                  "int number too big.\n");
        memsafe(s = new_mstring(buf, STRING_ASCII), strlen(buf), "converted number");
        break;

    case T_FLOAT:
        sprintf(buf,"%g", READ_DOUBLE(sp));
        if (buf[sizeof(buf)-1] != '\0')
            fatal("Buffer overflow in to_string: "
                  "int number too big.\n");
        memsafe(s = new_mstring(buf, STRING_ASCII), strlen(buf), "converted number");
        break;

    case T_OBJECT:
        if (!compat_mode)
            s = add_slash(sp->u.ob->name);
        else
            s = ref_mstring(sp->u.ob->name);
        if (!s)
            errorf("Out of memory\n");
        free_object_svalue(sp);
        break;

    case T_POINTER:
        /* Alias for to_text(array). */
        return v_to_text(sp, 1);

    case T_STRUCT:
      {
        string_t *rc;
        string_t *name;
        size_t    size;
        const char * fmt = "<struct %s>";

        name = struct_name(sp->u.strct);
        size = strlen(fmt)+mstrsize(name)-2;

        memsafe(rc = alloc_mstring(size), size, "converted struct");
        sprintf(get_txt(rc), fmt, get_txt(name));
        if (!is_ascii(get_txt(name), size))
            rc->info.unicode = STRING_UTF8;
        free_struct(sp->u.strct);
        put_string(sp, rc);
        break;
      }

    case T_CLOSURE:
      {
        string_t * rc = closure_to_string(sp, MY_FALSE);
        free_svalue(sp);
        put_string(sp, rc);
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
 *   mixed *to_array(bytes)
 *   mixed *to_array(symbol)
 *   mixed *to_array(quotedarray)
 *   mixed *to_array(mixed *)
 *   mixed *to_array(struct)
 *
 * Strings and symbols are converted to an int array that
 * consists of the args characters.
 * Quoted arrays are ``dequoted'', and arrays are left as they
 * are.
 * structs are converted into normal arrays.
 */

{
    vector_t *v;
    char *s;
    unsigned char ch;
    svalue_t *svp;
    p_int len;

    switch (sp->type)
    {
    default:
        fatal("Bad arg 1 to to_array(): type %s\n", typename(sp->type));
        break;
    case T_STRING:
    case T_BYTES:
    case T_SYMBOL:
        /* Split the string into an array of ints */

        len = (p_int)mstrsize(sp->u.str);
        s = get_txt(sp->u.str);

        if (sp->type == T_STRING && sp->u.str->info.unicode == STRING_UTF8)
        {
            bool error;
            size_t chars = byte_to_char_index(s, len, &error);
            if (error)
                errorf("to_array(): Invalid character in string at index %zd.\n", chars);

            v = allocate_uninit_array((mp_int)chars);
            svp = v->item;

            /* This is a UTF8 string, let's decode it. */
            while (len)
            {
                p_int code;
                size_t codelen = utf8_to_unicode(s, len, &code);

                if (!codelen)
                    errorf("to_array(): Invalid character in string at index %zd.\n",
                        byte_to_char_index(get_txt(sp->u.str), mstrsize(sp->u.str) - len, NULL));

                len -= codelen;
                s += codelen;

                put_number(svp, code);
                svp++;
            }

            /* We should be at the end, otherwise utf8_to_unicode()
             * or byte_to_char_index() did something wrong.
             */
            assert(svp == v->item + chars);
        }
        else
        {
            v = allocate_uninit_array((mp_int)len);
            svp = v->item;

            while (len-- > 0)
            {
                ch = (unsigned char)*s++;
                put_number(svp, ch);
                svp++;
            }
        }

        free_svalue(sp);
        put_array(sp, v);
        break;
    case T_STRUCT:
      {
        vector_t *vec;
        size_t left;

        left = struct_size(sp->u.strct);
        vec = allocate_array(left);
        while (left-- > 0)
            assign_rvalue_no_free(vec->item+left, sp->u.strct->member+left);
        free_struct(sp->u.strct);
        put_array(sp, vec);
        break;
      }
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

/* -- struct mtos_member_s --
 *
 * One entry from the mapping to be transported into the anonymous struct.
 */

struct mtos_member_s
{
    struct mtos_member_s * next;  /* The next entry */
    string_t * name;              /* The name of the key/member (uncounted) */
    svalue_t * data;              /* The (first) value */
};

/* -- struct mtos_data_s --
 *
 * Structure to collect the data during the mapping walk when converting
 * a mapping into an anonymous structure.
 */
struct mtos_data_s
{
    Mempool                pool;   /* The pool holding the mtos_member_s */
    struct mtos_member_s * first;  /* List of found members */
    struct mtos_member_s * last;
    int                    num;    /* Number of members */
};

static void
map_to_struct_filter (svalue_t *key, svalue_t *data, void *extra)
{
    struct mtos_data_s * pData = (struct mtos_data_s *)extra;

    if (key->type == T_STRING)
    {
        struct mtos_member_s * member;

        member = mempool_alloc(pData->pool, sizeof(*member));
        if (member != NULL)
        {
            member->name = key->u.str;
            member->data = data;
            member->next = NULL;

            if (pData->first == NULL)
            {
                pData->first = member;
                pData->last = member;
            }
            else
            {
                pData->last->next = member;
                pData->last = member;
            }

            pData->num++;
        }
    }
} /* map_to_struct_filter() */

svalue_t *
v_to_struct (svalue_t *sp, int num_arg)

/* EFUN to_struct()
 *
 *   mixed to_struct(mixed *|mapping)
 *   mixed to_struct(mixed *|mapping, struct)
 *   mixed to_struct(struct)
 *   mixed to_struct(struct, struct)
 *
 * An array is converted into a struct of the same length.
 * A mapping is converted into a struct, using those keys with string
 * values as member names.
 *
 * The returned struct is anonymous, or if a template struct is given, a
 * struct of the same type.
 *
 * Structs are converted to the template struct (2nd arg) if given. If no 
 * template given, they are returned unchanged.
 * Struct conversion is only possible between either a base struct and its
 * children or a child and one of its base structs, otherwise an error is
 * raised.
 */

{
    svalue_t * argp;

    argp = sp - num_arg + 1;
    switch (argp->type)
    {
    default:
        fatal("Bad arg 1 to to_struct(): type %s\n", typename(argp->type));
        break;

    case T_POINTER:
      {
        struct_t *st;
        size_t left = VEC_SIZE(argp->u.vec);

        if (num_arg > 1)
        {
            if (argp[1].type != T_STRUCT)
                fatal("Bad arg 2 to to_struct(): type %s\n"
                     , typename(argp[1].type));

            if (left > struct_size(argp[1].u.strct))
                left = struct_size(argp[1].u.strct);

            st = struct_new(argp[1].u.strct->type);
        }
        else
            st = struct_new_anonymous(VEC_SIZE(argp->u.vec));

        for (; left-- > 0; )
            assign_rvalue_no_free(st->member+left, argp->u.vec->item+left);
        free_array(argp->u.vec);
        put_struct(argp, st);
        break;
      }

    case T_MAPPING:
      {
        struct_t  * st;
        mapping_t * m;
        int         num_values;

        m = argp->u.map;
        num_values = m->num_values;

        if (num_arg > 1)
        {
            int i;

            if (argp[1].type != T_STRUCT)
                fatal("Bad arg 2 to to_struct(): type %s\n"
                     , typename(argp[1].type));

            st = struct_new(argp[1].u.strct->type);

            /* Now loop over all members and assign the data */
            for (i  = 0; i < struct_size(st); i++)
            {
                svalue_t   key;
                svalue_t * data;

                put_string(&key, st->type->member[i].name);
                data = get_map_value(m, &key);

                if (data != &const0)
                {
                    /* Copy the data */
                    if (num_values == 0)
                        put_number(&st->member[i], 1);
                    else if (num_values == 1)
                    {
                        assign_rvalue_no_free(&st->member[i], data);
                    }
                    else
                    {
                        vector_t * vec;
                        svalue_t * dest;
                        int        j;

                        vec = allocate_uninit_array(num_values);
                        if (vec == NULL)
                        {
                            struct_free(st);
                            outofmemory("result data");
                            /* NOTREACHED */
                        }

                        dest = vec->item;
                        for (j = 0; j < num_values; j++)
                        {
                            assign_rvalue_no_free(dest++, data++);
                        }

                        put_array(&st->member[i], vec);
                    } /* if (num_values) */
                } /* if (has data) */
            } /* for (all members) */
        }
        else
        {
            struct mtos_data_s     data;
            struct mtos_member_s * member;
            int                    i;

            /* Gather the data from the mapping */
            data.pool = new_mempool(size_mempool(sizeof(struct mtos_member_s)));
            if (data.pool == NULL)
            {
                outofmemory("memory pool");
                /* NOTREACHED */
            }
            data.num = 0;
            data.first = data.last = NULL;

            walk_mapping(argp->u.map, map_to_struct_filter, &data);

            /* Get the result struct */
            st = struct_new_anonymous(data.num);
            if (st == NULL)
            {
                mempool_delete(data.pool);
                outofmemory("result");
                /* NOTREACHED */
            }

            /* Copy the data into the result struct, and also update
             * the member names.
             */
            for ( i = 0, member = data.first
                ; member != NULL && i < data.num
                ; i++, member = member->next
                )
            {
                /* Update the member name */
                free_mstring(st->type->member[i].name);
                st->type->member[i].name = ref_mstring(member->name);

                /* Copy the data */
                if (num_values == 0)
                    put_number(&st->member[i], 1);
                else if (num_values == 1)
                {
                    assign_rvalue_no_free(&st->member[i], member->data);
                }
                else
                {
                    vector_t * vec;
                    svalue_t * src, * dest;
                    int        j;

                    vec = allocate_uninit_array(num_values);
                    if (vec == NULL)
                    {
                        mempool_delete(data.pool);
                        struct_free(st);
                        outofmemory("result data");
                        /* NOTREACHED */
                    }
                    dest = vec->item;
                    src = member->data;
                    for (j = 0; j < num_values; j++)
                    {
                        assign_rvalue_no_free(dest++, src++);
                    }

                    put_array(&st->member[i], vec);
                } /* if (num_values) */
            } /* for (all data) */

            /* Deallocate helper structures */
            mempool_delete(data.pool);
        }

        free_mapping(argp->u.map);
        put_struct(argp, st);
        break;
      }

    case T_STRUCT:
        {
            if (num_arg > 1)
            {
                int rc;
                p_int size = 0;
                struct_type_t *oldbase, *newbase;
                
                struct_t *oldstruct = argp->u.strct;
                struct_t *newstruct = argp[1].u.strct;
                svalue_t *memberp; // pointer to the first member of the new struct
                svalue_t *omemberp; // pointer to the first member of the old struct
                struct_member_t *memberdef;

                if (argp[1].type != T_STRUCT)
                    fatal("Bad arg 2 to to_struct(): type %s\n"
                          , typename(argp[1].type));

                // a template struct was given for conversion
                // check if template is a base of the old struct or the old struct is
                // a base of the template.
                rc = struct_baseof(newstruct->type, oldstruct->type);
                
                // special case, same structs.
                if (rc == 2)
                {
                    // no change required, we leave oldstruct on the stack.
                    break;
                }
                if (rc == 1)
                    size = struct_size(newstruct); // newstruct is base and has <= members than oldstruct.
                else if ((rc = struct_baseof(oldstruct->type, newstruct->type)) == 1)
                    size = struct_size(oldstruct); // oldstruct is base and has <= members than newstruct.

                if (rc == 1)
                {
                    // Both types are directly related. We just need to copy the first
                    // <size> members over.

                    if (oldstruct->ref == 1
                        && struct_size(newstruct) == struct_size(oldstruct))
                    {
                        // special case, the structs have the same number of members. Since it
                        // is not possible to remove/change members inherited from a base struct,
                        // the two structs have the same members. We can just exchange the types
                        // of the structs.
                        free_struct_type(oldstruct->type);
                        oldstruct->type = ref_struct_type(newstruct->type);
                        break;
                    }

                    newstruct = struct_new(newstruct->type);
                    if (!newstruct)
                        outofmemory("new struct in to_struct()");
                    for (memberp = newstruct->member, omemberp = oldstruct->member;
                         --size >= 0;
                         ++memberp, ++omemberp)
                    {
                        // *_no_free, because the members of the new struct only contain 0,
                        // which need not to be freed.
                        assign_rvalue_no_free(memberp, omemberp);
                    }
                    // the new struct may have more members than the old one (if oldstruct was
                    // the base. That is OK, the extra svalues just remain 0. On the other hand,
                    // if the old struct has more members, we just ignore them.

                    put_struct(argp, newstruct);
                    break;
                }

                // Check whether both types are related by struct name.
                oldbase = struct_baseof_name(newstruct->type->name, oldstruct->type);
                if (oldbase)
                    newbase = newstruct->type;
                else if ((newbase = struct_baseof_name(oldstruct->type->name, newstruct->type)))
                    oldbase = oldstruct->type;

                if (oldbase && newbase)
                {
                    // Each member in <newbase> we look up using <oldbase> in <oldstruct>.
                    newstruct = struct_new(newstruct->type);
                    if (!newstruct)
                        outofmemory("new struct in to_struct()");

                    for (memberdef = newbase->member, memberp = newstruct->member, size = struct_t_size(newbase); size--; ++memberdef, ++memberp)
                    {
                        int idx = struct_find_member(oldbase, memberdef->name);
                        if (idx < 0)
                            continue;

                        // We'll copy only if the types are compatible.
                        if (!check_rtt_compatibility(memberdef->type, oldstruct->member + idx))
                            continue;

                        assign_rvalue_no_free(memberp, oldstruct->member + idx);
                    }

                    put_struct(argp, newstruct);
                    break;
                }
                else
                {
                    // completely unrelated structs? Then we don't convert.
                    errorf("Can't convert struct %s into struct %s. Neither is a base of the other.\n",
                           get_txt(struct_unique_name(argp->u.strct)), 
                           get_txt(struct_unique_name(argp[1].u.strct)));
                }
            }
            /* No conversion template given - good as it is, leave it on the stack. */
            break;
        }
    }

    while (num_arg > 1)
    {
        free_svalue(sp);
        sp--;
        num_arg--;
    }

    /* sp is now argp */
    return sp;
} /* f_to_struct() */

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
        errorf("Bad arg 1 to to_object(): type %s\n", typename(sp->type));
        break;

    case T_CLOSURE:
        n = sp->x.closure_type;
        o = sp->u.ob;
        if (is_undef_closure(sp)) /* this shouldn't happen */
            o = NULL;
        else if (CLOSURE_MALLOCED(n))
        {
            if (n == CLOSURE_UNBOUND_LAMBDA)
            {
                errorf("Bad arg 1 to to_object(): unbound lambda.\n");
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
            DYN_ARRAY_COST(size);
            new = allocate_uninit_array((int)size);
            if (!new)
                errorf("(copy) Out of memory: array[%lu] for copy.\n"
                     , (unsigned long) size);
            for (i = 0; i < size; i++)
                assign_rvalue_no_free(&new->item[i], &old->item[i]);
            free_array(old);
            sp->u.vec = new;
        }
        break;
      }
    case T_STRUCT:
      {
        struct_t *old;

        old = sp->u.strct;
        if (old->ref != 1)
        {
            struct_t *new;
            size_t size, i;
            size = struct_size(old);
            DYN_ARRAY_COST(size);
            new = struct_new(old->type);
            if (!new)
                errorf("(copy) Out of memory: struct '%s' for copy.\n"
                     , get_txt(struct_name(old)));
            for (i = 0; i < size; i++)
                assign_rvalue_no_free(&new->member[i], &old->member[i]);
            free_struct(old);
            sp->u.strct = new;
        }
        break;
      }
    case T_MAPPING:
      {
        mapping_t *old, *new;

        old = sp->u.map;
        if (old->ref != 1)
        {
            DYN_MAPPING_COST(old->num_entries);
            new = copy_mapping(old);
            if (!new)
                errorf("(copy) Out of memory: mapping[%"PRIdPINT"] for copy.\n"
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
 * TODO: change width to p_int, because mappings can have p_int values
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
        outofmemory("copied mapping value");
        /* NOTREACHED */
        return;
    }
    for (i = info->width; i-- > 0; newdata++, val++)
        copy_svalue(newdata, val, info->ptable, info->depth);

    free_svalue(&newkey); /* no longer needed */
} /* deep_copy_mapping() */

/*-------------------------------------------------------------------------*/
static void
copy_svalue (svalue_t *dest, svalue_t *src
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

    normalize_svalue(src, true);

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
            (void)add_eval_cost((depth+1) / 10);
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
            rec->id_number = (src->type << 16) | (src->x.quotes & 0xFFFF);
            rec->data = new;

            /* Copy the values */
            for (i = 0; i < size; i++)
            {
                copy_svalue(new->item + i, old->item + i, ptable, depth+1);
            }
        }
        else /* shared array we already encountered */
        {
            svalue_t sv;

            sv.type = rec->id_number >> 16;
            sv.x.quotes = rec->id_number & 0xFFFF;
            sv.u.vec = (vector_t *)rec->data;
            assign_svalue_no_free(dest, &sv);
        }
        break;
      }
    case T_STRUCT:
      {
        struct pointer_record *rec;
        struct_t *old, *new;
        mp_int size, i;

        old = src->u.strct;

        /* Lookup/add this struct to the pointer table */
        rec = find_add_pointer(ptable, old, MY_TRUE);

        if (rec->ref_count++ < 0) /* New struct */
        {
            size = (mp_int)struct_size(old);
            DYN_ARRAY_COST(size);
#if defined(DYNAMIC_COSTS)
            (void)add_eval_cost((depth+1) / 10);
#endif

            /* Create a new array, assign it to dest, and store
             * it in the table, too.
             */
            new = struct_new(old->type);
            put_struct(dest, new);
            rec->data = new;

            /* Copy the values */
            for (i = 0; i < size; i++)
            {
                copy_svalue(new->member + i, old->member + i, ptable, depth+1);
            }
        }
        else /* shared struct we already encountered */
        {
            svalue_t sv;

            put_struct(&sv, (struct_t *)rec->data);
            assign_svalue_no_free(dest, &sv);
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
            size = (mp_int)MAP_SIZE(old);
              /* Doesn't matter if this is too big due to destructed
               * elements.
               */
            DYN_MAPPING_COST(size);
#if defined(DYNAMIC_COSTS)
            (void)add_eval_cost((depth+1) / 10);
#endif
            info.depth = depth+1;
            info.width = old->num_values;
            new = allocate_mapping(size, info.width);
            if (!new)
                errorf("(copy) Out of memory: new mapping[%"PRIdMPINT", %u].\n"
                     , size, info.width);
            put_mapping(dest, new);
            rec->data = new;

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
            svalue_t sv;

            put_mapping(&sv, (mapping_t *)rec->data);
            assign_svalue_no_free(dest, &sv);
        }
        break;
      }

    case T_LVALUE:
        switch (src->x.lvalue_type)
        {
            default:
                fatal("(deep_copy) Illegal lvalue %p type %d\n", src, src->x.lvalue_type);
                break;

            case LVALUE_PROTECTED:
            {
                struct protected_lvalue *l = src->u.protected_lvalue;
                struct pointer_record *rec = find_add_pointer(ptable, l, MY_TRUE);

                if (rec->ref_count++ < 0)
                {
                    /* Create a new protected lvalue to the copy of the contents. */
                    svalue_t content;

#if defined(DYNAMIC_COSTS)
                    add_eval_cost((depth+1) / 10);
#endif

                    copy_svalue(&content, &(l->val), ptable, depth+1);
                    assign_protected_lvalue_no_free(dest, &content);
                    rec->data = dest->u.protected_lvalue;
                    free_svalue(&content);
                }
                else
                {
                    /* Recreate the svalue from the pointer table to copy it
                     * (so we don't have to do the proper ref-counting here.)
                     */
                    svalue_t sv;
                    sv.type = T_LVALUE;
                    sv.x.lvalue_type = LVALUE_PROTECTED;
                    sv.u.protected_lvalue = (struct protected_lvalue *) rec->data;

                    assign_svalue_no_free(dest, &sv);
                }

                break;
            }

            case LVALUE_PROTECTED_CHAR:
            {
                /* This should be a mutable string. */
                struct protected_char_lvalue *l = src->u.protected_char_lvalue;
                struct pointer_record *rec = find_add_pointer(ptable, l, MY_TRUE);

                if (rec->ref_count++ < 0)
                {
                    string_t *dup;
                    struct protected_lvalue *var = NULL;
                    struct pointer_record *str_rec = find_add_pointer(ptable, l->str, MY_TRUE);
                    if (str_rec->ref_count++ < 0)
                    {
                        dup = make_mutable(dup_mstring(l->str));
                        str_rec->data = dup;
                    }
                    else
                    {
                        dup = ref_mstring((string_t*) str_rec->data);
                    }

                    /* Check whether r->var is still valid. */
                    if (l->var != NULL
                     && (l->var->val.type == T_STRING || l->var->val.type == T_BYTES)
                     && l->str == l->var->val.u.str)
                    {
                        struct pointer_record *var_rec = find_add_pointer(ptable, l->var, MY_TRUE);
                        if (var_rec->ref_count++ < 0)
                        {
                            svalue_t var_content = { l->var->val.type };
                            svalue_t var_lvalue;

                            var_content.u.str = ref_mstring(dup);

                            assign_protected_lvalue_no_free(&var_lvalue, &var_content);
                            var_rec->data = var = var_lvalue.u.protected_lvalue;
                            free_svalue(&var_content);
                        }
                        else
                        {
                            var = (struct protected_lvalue *) var_rec->data;
                            var->ref++;
                        }
                    }

                    assign_protected_char_lvalue_no_free(dest, var, dup, get_txt(dup) + (l->charp - get_txt(l->str)));
                    rec->data = dest->u.protected_char_lvalue;
                    deref_mstring(dup);

                    if (var != NULL)
                        var->ref--;
                }
                else
                {
                    svalue_t sv;
                    sv.type = T_LVALUE;
                    sv.x.lvalue_type = LVALUE_PROTECTED_CHAR;
                    sv.u.protected_char_lvalue = (struct protected_char_lvalue *) rec->data;

                    assign_svalue_no_free(dest, &sv);
                }
                break;
            }

            case LVALUE_PROTECTED_RANGE:
            {
                struct protected_range_lvalue *r = src->u.protected_range_lvalue;
                struct pointer_record *rec = find_add_pointer(ptable, r, MY_TRUE);

                if (rec->ref_count++ < 0)
                {
                    svalue_t vec;                          /* The copy of the vector.           */
                    svalue_t vec_lvalue;                   /* Lvalue to the copy of the vector. */
                    struct protected_lvalue *var = NULL;   /* = vec_lvalue.u.protected_lvalue.  */

#if defined(DYNAMIC_COSTS)
                    add_eval_cost((depth+1) / 10);
#endif

                    /* Check whether r->var is still valid. */
                    if (r->var != NULL
                     && ((r->vec.type == T_POINTER && r->var->val.type == T_POINTER && r->vec.u.vec == r->var->val.u.vec)
                      || (r->vec.type == T_STRING  && r->var->val.type == T_STRING  && r->vec.u.str == r->var->val.u.str)
                      || (r->vec.type == T_BYTES   && r->var->val.type == T_BYTES   && r->vec.u.str == r->var->val.u.str)))
                    {
                        struct pointer_record *var_rec = find_add_pointer(ptable, r->var, MY_TRUE);
                        if (var_rec->ref_count++ < 0)
                        {
                            svalue_t var_content;

                            copy_svalue(&var_content, &(r->var->val), ptable, depth+1);
                            assign_protected_lvalue_no_free(&vec_lvalue, &var_content);
                            var_rec->data = vec_lvalue.u.protected_lvalue;
                            free_svalue(&var_content);
                        }
                        else
                        {
                            svalue_t sv;
                            sv.type = T_LVALUE;
                            sv.x.lvalue_type = LVALUE_PROTECTED;
                            sv.u.protected_lvalue = (struct protected_lvalue *) var_rec->data;

                            assign_svalue_no_free(&vec_lvalue, &sv);
                        }
                        var = vec_lvalue.u.protected_lvalue;
                    }

                    /* By virtue of the pointer table this should yield the same vector
                     * as the one above for r->var.
                     */
                    copy_svalue(&vec, &(r->vec), ptable, depth+1);
                    assign_protected_range_lvalue_no_free(dest, var, &vec, r->index1, r->index2);
                    rec->data = dest->u.protected_range_lvalue;
                    free_svalue(&vec);
                    if (var != NULL)
                        free_svalue(&vec_lvalue);
                }
                else
                {
                    svalue_t sv;
                    sv.type = T_LVALUE;
                    sv.x.lvalue_type = LVALUE_PROTECTED_RANGE;
                    sv.u.protected_range_lvalue = (struct protected_range_lvalue *) rec->data;

                    assign_svalue_no_free(dest, &sv);
                }

                break;
            }

            case LVALUE_PROTECTED_MAPENTRY:
            {
                struct protected_mapentry_lvalue *e = src->u.protected_mapentry_lvalue;
                struct pointer_record *rec = find_add_pointer(ptable, e, MY_TRUE);

                if (rec->ref_count++ < 0)
                {
                    svalue_t origmap = { T_MAPPING };
                    svalue_t copymap;
                    svalue_t copykey;

#if defined(DYNAMIC_COSTS)
                    add_eval_cost((depth+1) / 10);
#endif

                    origmap.u.map = e->map;
                    copy_svalue(&copykey, &(e->key), ptable, depth+1);
                    copy_svalue(&copymap, &origmap, ptable, depth+1);
                    assign_protected_mapentry_lvalue_no_free(dest, copymap.u.map, &(copykey), e->index);
                    free_svalue(&(copykey));
                    free_svalue(&(copymap));

                    rec->data = dest->u.protected_mapentry_lvalue;
                }
                else
                {
                    svalue_t sv;
                    sv.type = T_LVALUE;
                    sv.x.lvalue_type = LVALUE_PROTECTED_MAPENTRY;
                    sv.u.protected_mapentry_lvalue = (struct protected_mapentry_lvalue *) rec->data;

                    assign_svalue_no_free(dest, &sv);
                }

                break;
            }
        }
        break;

    case T_STRING:
    case T_BYTES:
        if (mstr_mutable(src->u.str))
        {
            /* We must make a mutable copy of a mutable string. */
            struct pointer_record *rec = find_add_pointer(ptable, src->u.str, MY_TRUE);
            if (rec->ref_count++ < 0)
            {
                string_t *dup = make_mutable(dup_mstring(src->u.str));
                dest->type = src->type;
                dest->u.str = dup;
                rec->data = dup;
            }
            else
            {
                dest->type = src->type;
                dest->u.str = ref_mstring((string_t*) rec->data);
            }
        }
        else
            assign_svalue_no_free(dest, src);
        break;
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

    case T_QUOTED_ARRAY:
    case T_POINTER:
        if (sp->u.vec == &null_vector)
            break;
        /* FALLTHROUGH */
    case T_STRUCT:
    case T_MAPPING:
    case T_LVALUE:
      {
        svalue_t new;

        ptable = new_pointer_table();
        if (!ptable)
            errorf("(deep_copy) Out of memory for pointer table.\n");
        copy_svalue(&new, sp, ptable, 0);
        if (sp->type == T_QUOTED_ARRAY)
            new.x.quotes = sp->x.quotes;
        transfer_svalue(sp, &new);
        free_pointer_table(ptable);
        break;
      }
    }

    return sp;
} /* f_deep_copy() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_filter (svalue_t *sp, int num_arg)

/* EFUN filter()
 *
 *   mixed * filter (mixed *arg, string fun, string|object ob, mixed extra...)
 *   mixed * filter (mixed *arg, closure cl, mixed extra...)
 *   mixed * filter (mixed *arg, mapping map, mixed extra...)
 *
 *   mapping filter (mapping arg, string fun, string|object ob, mixed extra...)
 *   mapping filter (mapping arg, closure cl, mixed extra...)
 *
 *   string filter (string arg, string fun, string|object ob, mixed extra...)
 *   string filter (string arg, closure cl, mixed extra...)
 *   string filter (string arg, mapping map, mixed extra...)
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
    else if (sp[-num_arg+1].type == T_STRING || sp[-num_arg+1].type == T_BYTES)
        return x_filter_string(sp, num_arg);
    else
        return x_filter_array(sp, num_arg);

} /* v_filter() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_get_type_info (svalue_t *sp, int num_arg)

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
 * return the object the closure is bound to, resp. for lfun closures
 * it returns the object the closure function is defined in..
 * If <arg> is a struct, the <flag> setting 2 lets the efun
 * return the basic name of the struct.
 * If <arg> is a lfun or context closure, the <flag> setting 3 lets the efun
 * return the name of the program the closure was defined in. For other
 * closures, <flag> setting 3 returns 0.
 *
 * If <arg> is a lfun or context closure, the <flag> setting 4 lets the efun
 * return the base name of the function (without any program name adorments).
 * For other closures, <flag> setting 4 returns 0.
 *
 * For every other <flag> setting, -1 is returned.
 *
 * The secondary information is:
 *   - for mappings the width, ie the number of data items per key.
 *   - for symbols and quoted arrays the number of quotes.
 *   - for closures, the (internal) closure type, as defined in <lpctypes.h>
 *   - for strings 0 for shared strings, and non-0 for others.
 *   - for structs, the unique name of the struct is returned.
 *   - -1 for all other datatypes.
 *
 * TODO: The flags should be defined in an include file.
 * TODO: The array returned for closures should contain all
 * TODO:: three items.
 */

{
    mp_int primary, secondary;
    string_t *str; /* != NULL: to use instead of secondary */
    svalue_t *argp;
    p_int flag = -1;

    argp = sp - num_arg + 1;
    primary = argp->type;
    secondary = -1;
    str = NULL;

    if (num_arg == 2 && sp->type == T_NUMBER)
        flag = sp->u.number;

    /* Determine the second return value */
    switch(primary)
    {
    case T_STRING:
    case T_BYTES:
        secondary = (mstr_tabled(sp[-1].u.str)) ? 0 : 1;
        break;
    case T_MAPPING:
        secondary = argp->u.map->num_values;
        break;
    case T_CLOSURE:
        if (flag == 2)
        {
            object_t *ob;

            ob = NULL;
            sp--;
            switch(sp->x.closure_type)
            {
            default:
                /* efun, simul-efun, operator closure */
                ob = sp->u.ob;
                break;
            case CLOSURE_IDENTIFIER:
            case CLOSURE_BOUND_LAMBDA:
            case CLOSURE_LAMBDA:
                ob = sp->u.lambda->ob;
                break;
            case CLOSURE_LFUN:
                ob = sp->u.lambda->function.lfun.ob;
                break;
            case CLOSURE_UNBOUND_LAMBDA:
                ob = NULL;
                break;
            }
            free_svalue(sp);
            if (!ob || ob->flags & O_DESTRUCTED)
                put_number(sp, 0);
            else
                put_ref_object(sp, ob, "get_type_info");
            return sp;
            /* NOTREACHED */
        }
        if (flag == 3)
        {
            string_t  *progname = NULL;

            sp--;
            if (sp->x.closure_type == CLOSURE_LFUN)
            {
                program_t *prog;
                string_t  *function_name;
                Bool       is_inherited;

                closure_lookup_lfun_prog(sp->u.lambda, &prog, &function_name, &is_inherited);

                memsafe(progname = mstring_cvt_progname(prog->name MTRACE_ARG)
                       , mstrsize(prog->name)
                       , "closure program name");
            }

            free_svalue(sp);
            if (!progname)
                put_number(sp, 0);
            else
                put_string(sp, progname);
            return sp;
            /* NOTREACHED */
        }
        if (flag == 4)
        {
            string_t  *function_name = NULL;

            sp--;
            if (sp->x.closure_type == CLOSURE_LFUN)
            {
                program_t *prog;
                Bool       is_inherited;

                closure_lookup_lfun_prog(sp->u.lambda, &prog, &function_name, &is_inherited);
            }

            free_svalue(sp);
            if (!function_name)
                put_number(sp, 0);
            else
                // don't forget to reference the function name
                put_ref_string(sp, function_name);
            return sp;
            /* NOTREACHED */
        }
        /* FALLTHROUGH */

    case T_SYMBOL:
    case T_QUOTED_ARRAY:
        secondary = argp->x.generic;
        break;
    case T_STRUCT:
        if (flag == 2)
        {
            sp--;

            str = ref_mstring(struct_unique_name(sp->u.strct));
            free_svalue(sp);
            put_string(sp, str);
            return sp;
            /* NOTREACHED */
        }
        else if (num_arg == 2)
        {
            str = ref_mstring(struct_name(sp[-1].u.strct));
        }
        else
        {
            str = ref_mstring(struct_name(sp->u.strct));
        }
        break;
    }

    /* Depending on flag, return the proper value */
    if (num_arg == 2)
    {
        free_svalue(sp--);
        free_svalue(sp);

        /* flag is 0, 1 or unsupported. */
        switch (flag)
        {
            case 0:
                put_number(sp, primary);
                if (str)
                    free_mstring(str);
                break;

            case 1:
                if (str)
                    put_string(sp, str);
                else
                    put_number(sp, secondary);
                break;

            default:
                put_number(sp, -1);
                if (str)
                    free_mstring(str);
        }
    }
    else
    {
        vector_t *v;

        v = allocate_array(2);
        v->item[0].u.number = primary;
        if (str != NULL)
            put_string(v->item+1, str);
        else
            v->item[1].u.number = secondary;
        if (num_arg == 2)
            free_svalue(sp--);
        free_svalue(sp);
        put_array(sp,v);
    }

    return sp;
} /* v_get_type_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_map (svalue_t *sp, int num_arg)

/* EFUN map()
 *
 *   mixed * map(mixed *arg, string func, string|object ob, mixed extra...)
 *   mixed * map(mixed *arg, closure cl, mixed extra...)
 *   mixed * map(mixed *arg, mapping m [, int idx])
 *
 *   mixed * map(struct arg, string func, string|object ob, mixed extra...)
 *   mixed * map(struct arg, closure cl, mixed extra...)
 *   mixed * map(struct arg, mapping m [, int idx])
 *
 *   mapping map(mapping arg, string func, string|object ob, mixed extra...)
 *   mapping map(mapping arg, closure cl, mixed extra...)
 *
 *   string map(string arg, string func, string|object ob, mixed extra...)
 *   string map(string arg, closure cl, mixed extra...)
 *   string map(string arg, mapping m [, int idx])
 *
 * Call the function <ob>-><func>() resp. the closure <cl> for
 * every element of the array/struct/mapping/string <arg>, and return a result
 * made up from the returned values.
 *
 * For strings and arrays, it is also possible to map every entry through
 * a lookup <m>[element]. If the mapping entry doesn't exist, the original
 * value is kept, otherwise the result of the mapping lookup.
 *
 * If <arg> is a string, only integer return values are allowed, of which only
 * the lower 8 bits are considered.
 *
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 */

{
    svalue_t *arg = sp - num_arg + 1;

    if (num_arg > 2 && arg[0].type != T_MAPPING && arg[1].type == T_MAPPING) {
        if (num_arg > 3) {
            inter_sp = sp;
            errorf("Too many arguments to map(%s, mapping).\n",
                   typename(arg[0].type));
        }
        if (arg[2].type != T_NUMBER) {
            inter_sp = sp;
            errorf("Bad arg 3 to map(): got '%s', expected 'number'.\n",
                   typename(arg[2].type));
        }
        p_int num_values = arg[1].u.map->num_values;
        if (arg[2].u.number < 0 || arg[2].u.number >= num_values) {
            inter_sp = sp;
            errorf("map: Bad column index %"PRIdPINT" for mapping of "
                   "width %"PRIdPINT".\n", arg[2].u.number, num_values);
        }
    }

    if (arg[0].type == T_MAPPING)
        return x_map_mapping(sp, num_arg, MY_TRUE);
    else if (arg[0].type == T_STRING || arg[0].type == T_BYTES)
        return x_map_string(sp, num_arg);
    else if (arg[0].type == T_STRUCT)
        return x_map_struct(sp, num_arg);
    else /* T_POINTER */
        return x_map_array(sp, num_arg);

} /* v_map() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_member (svalue_t *sp, int num_arg)

/* EFUN member()
 *
 *   int member(mixed *array, mixed elem, [int start])
 *   int member(mapping m, mixed key)
 *   int member(string s, int elem, [int start])
 *   int member(bytes s, int elem, [int start])
 *
 * For arrays, strings and bytes, returns the index of the first occurance
 * of second arg in the first arg, or -1 if none found. If <start> is
 * given and non-negative, the search starts at that position. A start
 * position beyond the end of the string or array will cause the efun
 * to return -1.
 *
 * For mappings it checks, if key is present in mapping m and returns
 * 1 if so, 0 if key is not in m.
 */

{
    p_int startpos = 0;
    Bool hasStart = MY_FALSE;
    if (num_arg > 2)
    {
        startpos = sp->u.number;
        sp--;
        hasStart = MY_TRUE;
        num_arg--;
    }

    if (hasStart && startpos < 0)
    {
        errorf("Illegal arg 3 to member(): %"PRIdPINT", expected positive number.\n"
             , startpos);
        /* NOTREACHED */
        return sp;
    }

    /* --- Search an array --- */

    if (sp[-1].type == T_POINTER)
    {
        vector_t *vec;
        union  u       sp_u;
        long cnt;

        vec = sp[-1].u.vec;
        cnt = (long)VEC_SIZE(vec);
        sp_u = sp->u;

        if (hasStart && startpos >= cnt)
            cnt = -1;
        else
        {
            cnt -= startpos;

            switch(sp->type)
            {
            case T_STRING:
            case T_BYTES:
              {
                string_t *str;
                svalue_t *entry;

                str = sp_u.str;
                for(entry = vec->item + startpos; --cnt >= 0; entry++)
                {
                    svalue_t *item = get_rvalue(entry, NULL);
                    if (item == NULL)
                    {
                        struct protected_range_lvalue* r = entry->u.protected_range_lvalue;
                        size_t len;

                        if (r->vec.type != sp->type)
                            continue;

                        len = mstrsize(str);
                        if (len != r->index2 - r->index1)
                            continue;

                        if (memcmp(get_txt(str), get_txt(r->vec.u.str) + r->index1, len) == 0)
                            break;
                    }
                    else if (item->type == sp->type && mstreq(str, item->u.str))
                        break;
                }
                break;
              }

            case T_CLOSURE:
              {
                short type;
                svalue_t *entry;

                type = sp->type;
                for(entry = vec->item + startpos; --cnt >= 0; entry++)
                {
                    svalue_t *item = get_rvalue(entry, NULL);
                    if (item != NULL && item->type == type && closure_eq(sp, item))
                        break;
                }
                break;
              }

            case T_FLOAT:
            case T_SYMBOL:
            case T_QUOTED_ARRAY:
              {
                short x_generic;
                short type;
                svalue_t *entry;

                type = sp->type;
                x_generic = sp->x.generic;
                for(entry = vec->item + startpos; --cnt >= 0; entry++)
                {
                    svalue_t *item = get_rvalue(entry, NULL);
                    if (item == NULL)
                        continue;

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

                    svalue_t *entry;
                    short type;

                    for (entry = vec->item + startpos; --cnt >= 0; entry++)
                    {
                        svalue_t *item = get_rvalue(entry, NULL);
                        if (item == NULL)
                            continue;

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
            case T_STRUCT:
              {
                svalue_t *entry;
                short type = sp->type;

                for (entry = vec->item + startpos; --cnt >= 0; entry++)
                {
                    svalue_t *item = get_rvalue(entry, NULL);
                    if (item == NULL)
                        continue;

                    /* TODO: Is this C99 compliant? */
                    if (sp_u.number == item->u.number
                     && item->type == type)
                        break;
                }
                break;
              }

            default:
                if (sp->type == T_LVALUE)
                    errorf("Reference passed to member()\n");
                fatal("Bad type to member(): %s\n", typename(sp->type));
            }
        } /* if (startpos in range) */

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

    if (sp[-1].type == T_STRING || sp[-1].type == T_BYTES)
    {
        string_t *str;
        char *str2;
        ptrdiff_t i;

        if (sp->type != T_NUMBER)
            efun_arg_error(2, T_NUMBER, sp->type, sp);
        str = sp[-1].u.str;

        if (str->info.unicode == STRING_UTF8)
        {
            p_int ch = sp->u.number;
            bool error = false;
            size_t offset = char_to_byte_index(get_txt(str), mstrsize(str), startpos, &error);

            if (error || ch < 0)
                i = -1;
            else
            {
                size_t len = mstrsize(str) - offset;

                i = startpos;
                str2 = get_txt(str) + offset;

                while (true)
                {
                    p_int elem;
                    size_t elemlen = utf8_to_unicode(str2, len, &elem);

                    if (!elemlen)
                    {
                        i = -1;
                        break;
                    }

                    if (elem == ch)
                        break;

                    i++;
                    str2 += elemlen;
                    len -= elemlen;
                }
            }
        }
        else
        {
            /* Byte or ASCII Sequence. We can use the fast memchr here. */
            if (hasStart && (size_t)startpos >= mstrsize(str))
                i = -1;
            else
            {
                i = sp->u.number;
                str2 = (i & ~0xff) ? NULL
                                   : memchr(get_txt(str)+startpos, i, mstrsize(str)-startpos);
                i = str2 ? (str2 - get_txt(str)) : -1;
            }
        }

        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, i);
        return sp;
    }

    /* --- Search a mapping --- */

    if (sp[-1].type == T_MAPPING)
    {
        int i;

        if (hasStart)
        {
            errorf("Illegal arg 3 to member(): searching a mapping doesn't "
                  "take a start position.\n");
            /* NOTREACHED */
            return sp;
        }

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
v_rmember (svalue_t *sp, int num_arg)

/* EFUN rmember()
 *
 *   int rmember(mixed *array, mixed elem [, int startpos])
 *   int rmember(string s, int elem [, int startpos])
 *   int rmember(bytes s, int elem [, int startpos])
 *
 * For arrays and strings, returns the index of the last occurance of
 * second arg in the first arg, or -1 if none found
 * If <start> is given and non-negative, the search starts at that
 * position.
 */

{
    p_int startpos = 0;
    Bool hasStart = MY_FALSE;
    if (num_arg > 2)
    {
        startpos = sp->u.number;
        sp--;
        hasStart = MY_TRUE;
        num_arg--;
    }

    if (hasStart && startpos < 0)
    {
        errorf("Illegal arg 3 to rmember(): %"PRIdPINT", expected positive number.\n"
             , startpos);
        /* NOTREACHED */
        return sp;
    }

    /* --- Search an array --- */

    if (sp[-1].type == T_POINTER)
    {
        vector_t *vec;
        union  u       sp_u;
        long cnt;

        vec = sp[-1].u.vec;
        cnt = (long)VEC_SIZE(vec);
        sp_u = sp->u;

        if (hasStart && startpos < cnt)
            cnt = startpos;

        switch(sp->type)
        {
        case T_STRING:
        case T_BYTES:
          {
            string_t *str;
            svalue_t *entry;

            str = sp_u.str;
            for (entry = vec->item+cnt; --cnt >= 0; )
            {
                svalue_t *item;
                entry--;

                item = get_rvalue(entry, NULL);
                if (item == NULL)
                {
                    struct protected_range_lvalue* r = entry->u.protected_range_lvalue;
                    size_t len;

                    if (r->vec.type != sp->type)
                        continue;

                    len = mstrsize(str);
                    if (len != r->index2 - r->index1)
                        continue;

                    if (memcmp(get_txt(str), get_txt(r->vec.u.str) + r->index1, len) == 0)
                        break;
                }
                else if (item->type == sp->type && mstreq(str, item->u.str))
                    break;
            }
            break;
          }

        case T_CLOSURE:
          {
            short type;
            svalue_t *entry;

            type = sp->type;
            for (entry = vec->item+cnt; --cnt >= 0; )
            {
                svalue_t *item;

                entry--;
                item = get_rvalue(entry, NULL);
                if (item != NULL && item->type == type && closure_eq(sp, item))
                    break;
            }
            break;
          }

        case T_FLOAT:
        case T_SYMBOL:
        case T_QUOTED_ARRAY:
          {
            short x_generic;
            short type;
            svalue_t *entry;

            type = sp->type;
            x_generic = sp->x.generic;
            for (entry = vec->item+cnt; --cnt >= 0; )
            {
                svalue_t *item;

                entry--;
                item = get_rvalue(entry, NULL);
                if (item == NULL)
                    continue;

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

                svalue_t *entry;
                short type;

                for (entry = vec->item+cnt; --cnt >= 0; )
                {
                    svalue_t *item;

                    entry--;
                    item = get_rvalue(entry, NULL);
                    if (item == NULL)
                        continue;

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
        case T_STRUCT:
          {
            svalue_t *entry;
            short type = sp->type;

            for (entry = vec->item+cnt; --cnt >= 0; )
            {
                svalue_t *item;

                entry--;
                item = get_rvalue(entry, NULL);
                if (item == NULL)
                    continue;

                /* TODO: Is this C99 compliant? */
                if (sp_u.number == item->u.number
                 && item->type == type)
                    break;
            }
            break;
          }

        default:
            if (sp->type == T_LVALUE)
                errorf("Reference passed to member()\n");
            fatal("Bad type to member(): %s\n", typename(sp->type));
        } /* if (startpos in range) */

        /* cnt is the correct result */

        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, cnt);
        return sp;
    }

    /* --- Search a string --- */

    if (sp[-1].type == T_STRING || sp[-1].type == T_BYTES)
    {
        string_t *str;
        ptrdiff_t i;

        if (sp->type != T_NUMBER)
            efun_arg_error(2, T_NUMBER, sp->type, sp);
        str = sp[-1].u.str;

        if (str->info.unicode == STRING_UTF8)
        {
            p_int ch = sp->u.number;
            bool error = false;
            size_t offset;

            if (!hasStart)
                offset = mstrsize(str);
            else
                offset = char_to_byte_index(get_txt(str), mstrsize(str), startpos, &error);

            /* The start position can be way behind the string length, recalculating. */
            if (offset == mstrsize(str))
                startpos = byte_to_char_index(get_txt(str), offset, &error);

            if (error || ch < 0)
                i = -1;
            else
            {
                size_t len = offset;
                char *pos = get_txt(str) + offset;

                i = startpos - 1;

                while (true)
                {
                    char *prevpos;
                    p_int elem;
                    size_t elemlen;

                    if (!len)
                    {
                        i = -1;
                        break;
                    }

                    prevpos = utf8_prev(pos, len);
                    elemlen = utf8_to_unicode(prevpos, pos - prevpos, &elem);
                    if (!elemlen)
                    {
                        i = -1;
                        break;
                    }

                    if (elem == ch)
                        break;

                    i--;
                    len -= (pos - prevpos);
                    pos = prevpos;
                }
            }
        }
        else
        {
            /* Byte or ASCII Sequence. */

            if (!hasStart || (size_t)startpos >= mstrsize(str))
                startpos = mstrsize(str);
            i = sp->u.number;
            if ((i & ~0xff) != 0)
            {
                i = -1;
            }
            else
            {
                unsigned char * cp, *start, *str2;
                start = (unsigned char*) get_txt(str);
                cp = start + startpos;
                str2 = NULL;

                do
                {
                    cp--;
                    if (*cp == i)
                    {
                        str2 = cp;
                        break;
                    }
                } while (str2 == NULL && cp != start);

                i = str2 ? (str2 - (unsigned char*)get_txt(str)) : -1;
            }
        }
        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, i);
        return sp;
    }

    /* Otherwise it's not searchable */

    fatal("Bad arg 1 to rmember(): type %s\n", typename(sp[-1].type));
    return sp;
} /* f_rmember() */

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
        sp->u.str = make_tabled(sp->u.str);
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
f_unquote (svalue_t *sp)

/* EFUN unquote()
 *
 *   mixed unquote(mixed)
 *
 * Removes a quote from quoted arrays and symbols. When the
 * last quote from a symbol is removed, the result is a string.
 */

{
    switch (sp->type)
    {
    case T_QUOTED_ARRAY:
        sp->x.quotes--;
        if (!sp->x.quotes)
            sp->type = T_POINTER;
        break;

    case T_SYMBOL:
        sp->x.quotes--;
        if (!sp->x.quotes)
            sp->type = T_STRING;
        break;

    default:
        efun_gen_arg_error(1, sp->type, sp);
        /* NOTREACHED */
    }

    return sp;
} /* f_unquote() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_reverse(svalue_t *sp)

/* EFUN reverse()
 *
 *    int    reverse(int)
 *    string reverse(string)
 *    mixed* reverse(mixed *)
 *    mixed* reverse(mixed * &)
 *
 * Reverse the order of the elements in the array or string, and return
 * the result. If the argument is an integer, the bits in the integer
 * are reversed.
 *
 * Note that in the reference variant, the given array is reversed in-place.
 */

{
    bool changeInPlace = false;
    mp_int index1, index2;
    svalue_t *data, *var = NULL;

    /* If the argument is passed in by reference, make sure that it is
     * an array, note the fact, and place it directly into the stack.
     */
    if (sp->type == T_LVALUE)
    {
        bool last_reference = false;
        data = get_rvalue(sp, &last_reference);

        if (data == NULL)
        {
            /* This is a range. */
            struct protected_range_lvalue *r;

            assert(sp->x.lvalue_type == LVALUE_PROTECTED_RANGE);
            r = sp->u.protected_range_lvalue;

            index1 = r->index1;
            index2 = r->index2;
            data = &(r->vec);
            var = &(r->var->val);
        }
        else if (data->type == T_POINTER)
        {
            vector_t *vec = ref_array(data->u.vec);
            free_svalue(sp);
            put_array(sp, vec);

            index1 = 0;
            index2 = (mp_int)VEC_SIZE(vec);
            data = sp;
        }
        else if (data->type == T_STRING || data->type == T_BYTES)
        {
            string_t *str = ref_mstring(data->u.str);
            free_svalue(sp);
            put_string(sp, str);

            if(!last_reference)
                var = data;

            index1 = 0;
            index2 = (mp_int)mstrsize(str);
            data = sp;
        }
        else
        {
            inter_sp = sp;
            errorf("Bad arg 1 to reverse(): got '%s &', "
                  "expected 'string/string &/mixed */mixed * &'.\n"
                 , typename(data->type));
            /* NOTREACHED */
            return sp;
        }

        changeInPlace = true;
    }
    else
        data = sp;

    if (data->type == T_NUMBER)
    {
        p_int res;

        assert(data == sp);

        /* Try to use a fast bit swapping algorithm.
         * The slow fallback default is a loop swapping bit-by-bit.
         */

#if SIZEOF_PINT == 8

        res = data->u.number;

        res =   ((res & 0xaaaaaaaaaaaaaaaa) >> 1)
              | ((res & 0x5555555555555555) << 1);
	res =   ((res & 0xcccccccccccccccc) >> 2)
              | ((res & 0x3333333333333333) << 2);
	res =   ((res & 0xf0f0f0f0f0f0f0f0) >> 4)
              | ((res & 0x0f0f0f0f0f0f0f0f) << 4);
	res =   ((res & 0xff00ff00ff00ff00) >> 8)
              | ((res & 0x00ff00ff00ff00ff) << 8);
	res =   ((res & 0xffff0000ffff0000) >> 16)
              | ((res & 0x0000ffff0000ffff) << 16);
	res = (res >> 32) | (res << 32);

#elif SIZEOF_PINT == 4

        res = data->u.number;

        res = ((res & 0xaaaaaaaa) >> 1) | ((res & 0x55555555) << 1);
	res = ((res & 0xcccccccc) >> 2) | ((res & 0x33333333) << 2);
	res = ((res & 0xf0f0f0f0) >> 4) | ((res & 0x0f0f0f0f) << 4);
	res = ((res & 0xff00ff00) >> 8) | ((res & 0x00ff00ff) << 8);
	res = (res >> 16) | (res << 16);

#else

        unsigned char * from, * to;
        int num;

        from = (unsigned char *)&data->u.number;
        to = (unsigned char *)&res + sizeof(res) - 1;

        for (num = sizeof(res); num > 0; num--, from++, to--)
        {
            unsigned char ch = *from;

#  if CHAR_BIT == 8
#           warning "Efun reverse() uses a slow bit swapping algorithm."
            ch = (((ch & 0xaa) >> 1) | ((ch & 0x55) << 1));
            ch = (((ch & 0xcc) >> 2) | ((ch & 0x33) << 2));
            *to = ((ch >> 4) | (ch << 4));
#  else
#           warning "Efun reverse() uses the slowest bit swapping algorithm."
            unsigned char tch = 0;
            unsigned char f_mask, t_mask;
            int bits;

            f_mask = 0x01;
            t_mask = 0x01 << (CHAR_BIT-1);

            for (bits = CHAR_BIT; bits > 0; bits--, f_mask <<= 1, t_mask >>=1)
            {
                tch |= (ch & f_mask) ? t_mask : 0;
            }

            *to = tch;
#  endif
        }
#endif /* SIZEOF_PINT selection */

        put_number(sp, res);
    }
    else if (data->type == T_STRING || data->type == T_BYTES)
    {
        size_t len = mstrsize(data->u.str);

        /* If the length of the string is less than 2, there nothing to do */
        if (len > 1)
        {
            char *p1, *p2;

            if (changeInPlace)
            {
                string_t *str;
                memsafe(str = make_mutable(data->u.str), mstrsize(data->u.str)
                       , "modifiable string");

                if (var != NULL && var->type == T_STRING && var->u.str == data->u.str)
                {
                    free_mstring(var->u.str);
                    var->u.str = ref_mstring(str);
                }
                data->u.str = str;

                p1 = get_txt(str);
            }
            else
            {
                string_t *res = NULL;
                ph_int stringtype = data->type;

                memsafe(res = alloc_mstring(len), len, "reversed string");
                res->info.unicode = data->u.str->info.unicode;
                p1 = get_txt(res);

                memcpy(p1, get_txt(data->u.str), len);

                free_string_svalue(data);
                sp->type = stringtype;
                sp->u.str = res;
            }

            if(!changeInPlace)
            {
                index1 = 0;
                index2 = (mp_int)len;
            }

            p2 = p1 + index2;
            p1 += index1;

            if (data->u.str->info.unicode == STRING_UTF8)
            {
                /* We reverse the UTF8 bytes beforehand,
                 * so they will be correct when the whole string is reversed.
                 */
                for (char *ptr = p1; ptr < p2;)
                {
                    p_int dummy;
                    size_t chlen = utf8_to_unicode(ptr, p2 - ptr, &dummy);

                    if (chlen < 2)
                    {
                        ptr++;
                        continue;
                    }
                    else
                    {
                        char* p3 = ptr;
                        char* p4 = ptr + chlen - 1;

                        while (p3 < p4)
                        {
                            char c = *p3;
                            *p3 = *p4;
                            *p4 = c;

                            p3++;
                            p4--;
                        }

                        ptr += chlen;
                    }
                }
            }

            p2--;

            while (p1 < p2)
            {
                char c = *p1;
                *p1 = *p2;
                *p2 = c;

                p1++;
                p2--;
            }
        }
    }
    else if (data->type == T_POINTER)
    {
        mp_int r_size;
        vector_t *vec = NULL;

        /* If we change in place, the 'new' vector is the old one
         * that lies already on the stack.
         */
        if (changeInPlace
         || data->u.vec->ref == 1
         || data->u.vec == &null_vector)
        {
            vec = data->u.vec;
            if (!changeInPlace)
            {
                changeInPlace = true;
                index1 = 0;
                index2 = (mp_int)VEC_SIZE(vec);
            }
        }
        else
        {
            vector_t *old;
            size_t size, i;

            old = data->u.vec;
            size = VEC_SIZE(old);
            vec = allocate_uninit_array((int)size);
            if (!vec)
                errorf("(reverse) Out of memory: array[%lu] for copy.\n"
                     , (unsigned long) size);
            for (i = 0; i < size; i++)
                assign_svalue_no_free(&vec->item[i], &old->item[i]);

            index1 = 0;
            index2 = size;
        }

        r_size = index2 - index1;

        /* If the length of the range is less than 2, there nothing to do */
        if (r_size > 1)
        {
            mp_int i1, i2;

            DYN_ARRAY_COST(r_size);

            i1 = index1;
            i2 = index2 - 1;
            while (i1 < i2)
            {
                svalue_t tmp;
                tmp   = *(vec->item + i1);
                *(vec->item + i1) = *(vec->item + i2);
                *(vec->item + i2) = tmp;
                i1++;
                i2--;
            }
        }

        if (!changeInPlace)
        {
            /* Replace the old array by the new one. */
            free_svalue(sp);
            put_array(sp, vec);
        }
    }
    else
    {
        inter_sp = sp;
        errorf("Bad arg 1 to reverse(): got '%s &', "
              "expected 'string/mixed */mixed * &'.\n"
             , typename(sp->type));
        /* NOTREACHED */
        return sp;
    }
    return sp;
} /* f_reverse() */

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
      errorf("Bad argument 1 to sgn(): not a number or float.\n");

    return sp;
} /* f_sgn() */

/*=========================================================================*/
/*                               OTHER                                     */

/*-------------------------------------------------------------------------*/
svalue_t *
f_configure_driver (svalue_t *sp)

/* EFUN void configure_driver(int what, mixed data)
 *
 * This efun configures several aspects of the driver at run-time.
 * 
 * <what> is an identifier the setting:
 *        - DC_MEMORY_LIMIT        (0): configures the memory limits
 *        - DC_ENABLE_HEART_BEATS  (1): activate/deactivate heart beats globally
 *        - DC_LONG_EXEC_TIME      (2): execution time considered (too) 'long'
 *        - DC_DATA_CLEAN_TIME     (3): time delay between data cleans
 *        - DC_TLS_CERTIFICATE     (4): TLS certificate to use (fingerprint)
 *        - DC_TLS_DHE_PARAMETER   (5): TLS Diffie-Hellman paramter to use
 *        - DC_SWAP_TIME           (10): time to swap programs out
 *        - DC_SWAP_VAR_TIME       (11): time to swap variables out
 *        - DC_CLEANUP_TIME        (12): time to call cleanup hook
 *        - DC_RESET_TIME          (13): time to call reset hook
 * 
 * <data> is dependent on <what>:
 *   DC_MEMORY_LIMIT:        ({soft-limit, hard-limit}) both <int>, given in Bytes.
 *   DC_ENABLE_HEART_BEATS:  0/1 (int)
 *   DC_LONG_EXEC_TIME:      0 - __INT_MAX__ (int), given in microseconds.
 *   DC_DATA_CLEAN_TIME:     0 - __INT_MAX__/9 (int), given in seconds
 *   DC_TLS_CERTIFICATE      (string) SHA1 fingerprint
 *   DC_TLS_DHE_PARAMETER    (string) TLS Diffie-Hellman paramter (PEM-encoded)
 *   DC_SWAP_TIME            (int) time (s) to swap programs >=0
 *   DC_SWAP_VAR_TIME        (int) time (s) to swap variables >=0
 *   DC_CLEANUP_TIME         (int) time (s) for calling cleanup, >= 0
 *   DC_RESET_TIME           (int) time (s) for calling reset, >= 0
 *
 */

{

    // Check for privilege_violation.
    if ((current_object->flags & O_DESTRUCTED)
     || !privilege_violation2(STR_CONFIGURE_DRIVER, sp-1, sp, sp))
    {
        return pop_n_elems(2, sp);
    }

    switch(sp[-1].u.number) 
    {
        default:
            errorf("Illegal value %"PRIdPINT" for configure_driver().\n", sp[-1].u.number);
            return sp; /* NOTREACHED */
        case DC_MEMORY_LIMIT:
            if (sp->type != T_POINTER)
                efun_arg_error(2, T_POINTER, sp->type, sp);
            if (VEC_SIZE(sp->u.vec) != 2)
                errorf("Bad arg 2 to configure_driver(): Invalid array size %"PRIdPINT
                       ", expected 2.\n"
                       , VEC_SIZE(sp->u.vec));
            if (sp->u.vec->item[0].type != T_NUMBER)
                errorf("Bad arg 2 to configure_driver(): Element 0 is '%s', expected 'int'.\n"
                       , typename(sp->u.vec->item[0].type));
            if (sp->u.vec->item[1].type != T_NUMBER)
                errorf("Bad arg 2 to configure_driver(): Element 1 is '%s', expected 'int'.\n"
                       , typename(sp->u.vec->item[1].type));
            if (!set_memory_limit(MALLOC_SOFT_LIMIT, sp->u.vec->item[0].u.number))
                errorf("Could not set the soft memory limit (%"PRIdPINT") in configure_driver()\n",
                       sp->u.vec->item[0].u.number);
            if (!set_memory_limit(MALLOC_HARD_LIMIT, sp->u.vec->item[1].u.number))
                errorf("Could not set the hard memory limit (%"PRIdPINT") in configure_driver()\n",
                       sp->u.vec->item[1].u.number);
            break;

        case DC_ENABLE_HEART_BEATS:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            heart_beats_enabled = sp->u.number != 0 ? MY_TRUE : MY_FALSE;
            break;

        case DC_LONG_EXEC_TIME:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            if (!set_profiling_time_limit(sp->u.number))
                errorf("Could not set the profiling time limit for long executions "
                       "(%"PRIdPINT") in configure_driver()\n",
                       sp->u.vec->item[0].u.number);
            break;

        case DC_DATA_CLEAN_TIME:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            if (sp->u.number > 0 && sp->u.number < PINT_MAX/9)
                time_to_data_cleanup = sp->u.number;
            else
                errorf("DC_DATA_CLEAN_TIME must be > 0 and < %"PRIdPINT
                    ", but is (%"PRIdPINT") in configure_driver()\n",
                    PINT_MAX/9,sp->u.number);
            break;

#ifdef USE_TLS
        case DC_TLS_CERTIFICATE:
            if (sp->type != T_STRING)
                efun_arg_error(2, T_STRING, sp->type, sp);
            else
            {
                int len = mstrsize(sp->u.str);
                int out = 0;
                char * text = get_txt(sp->u.str);
                char * buf = xalloc(len/2);
                char * ptr = buf;

                /* Let's convert the hex string into raw bytes.
                 * We'll ignore any colons inbetween.
                 */
                for (int pos = 0; pos < len; ++pos, ++text)
                {
                    char c = *text;
                    if (c >= '0' && c <= '9')
                        c -= '0';
                    else if (c >= 'a' && c <= 'f')
                        c -= 'a' - 10;
                    else if (c >= 'A' && c <= 'F')
                        c -= 'A' - 10;
                    else if (c == ':' || c == ' ')
                        continue;
                    else
                    {
                        xfree(buf);
                        errorf("Illegal char in fingerprint '%c'\n", c);
                    }

                    if (out & 1)
                        *(ptr++) |= c;
                    else
                        *ptr = c << 4;
                    out++;
                }

                /* <out> is the count of nibbles (hex characters),
                 * so there should be an even number of them at the end.
                 */
                if (out & 1)
                {
                    xfree(buf);
                    errorf("Unexpected end of fingerprint.\n");
                }
                else if (!tls_set_certificate(buf, out/2))
                {
                    xfree(buf);
                    errorf("Certificate not found.\n");
                }

                xfree(buf);
            }

            break;

        case DC_TLS_DHE_PARAMETER:
            if (!tls_available())
                errorf("Diffie-Hellmann parameters could not be imported: "
                       "TLS layer hasn't been initialized.\n");

            if (sp->type == T_NUMBER)
            {
                if (sp->u.number != 0)
                    errorf("Unexpected value %"PRIdPINT" for DC_TLS_DHE_PARAMETER.\n",
                           sp->u.number);
                // set built-in defaults
                if (!tls_import_dh_params(NULL, 0))
                {
                    errorf("Built-in default Diffie-Hellman parameters could not be imported. Please "
                           "refer to debug log for further information.\n");
                }
            }
            else if (sp->type == T_STRING)
            {
                int len = mstrsize(sp->u.str);
                char * text = get_txt(sp->u.str);
                if (!tls_import_dh_params(text, len))
                {
                    errorf("Diffie-Hellman parameters could not be imported. Please "
                           "refer to debug log for further information.\n");
                }
            }
            else
            {
                vefun_exp_arg_error(1, TF_STRING|TF_NUMBER, sp->type, sp);
            }
            break;

        case DC_TLS_CIPHERLIST:
            if (!tls_available())
                errorf("Cipher list could not be set: "
                       "TLS layer hasn't been initialized.\n");

            if (sp->type == T_NUMBER)
            {
                if (sp->u.number != 0)
                    errorf("Unexpected value %"PRIdPINT" for DC_TLS_CIPHERLIST.\n",
                           sp->u.number);
                // set built-in defaults
                if (!tls_set_ciphers(NULL))
                {
                    errorf("Built-in default cipher list could not be set. Please "
                           "refer to debug log for further information.\n");
                }
            }
            else if (sp->type == T_STRING)
            {
                if (!tls_set_ciphers(get_txt(sp->u.str)))
                {
                    errorf("Cipher list could not be set. Please "
                           "refer to debug log for further information.\n");
                }
            }
            else
            {
                vefun_exp_arg_error(1, TF_STRING|TF_NUMBER, sp->type, sp);
            }
            break;

#endif /* USE_TLS */

        case DC_EXTRA_WIZINFO_SIZE:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            wiz_info_extra_size = sp->u.number;
            break;

        case DC_DEFAULT_RUNTIME_LIMITS:
            if (sp->type != T_POINTER)
                efun_arg_error(2, T_POINTER, sp->type, sp);
            set_default_limits(sp->u.vec);
            break;

        case DC_SWAP_COMPACT_MODE:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            swap_compact_mode = (sp->u.number != 0);
            break;

        case DC_SWAP_TIME:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            if (sp->u.number < 0)
            {
                errorf("Time to swap programs must be >= 0!\n");
            }
            time_to_swap = sp->u.number;
            break;

        case DC_SWAP_VAR_TIME:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            if (sp->u.number < 0)
            {
                errorf("Time to swap variables must be >= 0!\n");
            }
            time_to_swap_variables = sp->u.number;
            break;

        case DC_CLEANUP_TIME:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            if (sp->u.number < 0)
            {
                errorf("Time to call cleanup hook must be >= 0!\n");
            }
            time_to_cleanup = sp->u.number;
            break;

        case DC_RESET_TIME:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            if (sp->u.number < 0)
            {
                errorf("Time to call reset hook must be >= 0!\n");
            }
            time_to_reset = sp->u.number;
            break;

        case DC_DEBUG_FILE:
            if (sp->type != T_STRING)
                efun_arg_error(2, T_STRING, sp->type, sp);
            else
            {
                char *native = convert_path_to_native_or_throw(get_txt(sp->u.str), mstrsize(sp->u.str));
                free(debug_file);
                debug_file = strdup(native);

                reopen_debug_log = true;
            }
            break;

        case DC_FILESYSTEM_ENCODING:
            if (sp->type != T_STRING)
                efun_arg_error(2, T_STRING, sp->type, sp);
            else
            {
                /* We do a quick check. */
                iconv_t cd = iconv_open(get_txt(sp->u.str), "UTF-8");
                if (!iconv_valid(cd))
                    errorf("Unknown encoding '%s'.\n", get_txt(sp->u.str));

                iconv_close(cd);
                xfree(filesystem_encoding);
                filesystem_encoding = string_copy(get_txt(sp->u.str));
            }
            break;

        case DC_SIGACTION_SIGHUP:
        case DC_SIGACTION_SIGINT:
        case DC_SIGACTION_SIGUSR1:
        case DC_SIGACTION_SIGUSR2:
            if (sp->type != T_NUMBER)
                efun_arg_error(2, T_NUMBER, sp->type, sp);
            else if (sp->u.number < 0 || sp->u.number > DCS_THROW_EXCEPTION)
                errorf("Illegal value for signal action in configure_driver()\n");
            else
            {
                switch(sp[-1].u.number)
                {
                    case DC_SIGACTION_SIGHUP:
                        sigaction_sighup = (char) sp->u.number;
                        break;

                    case DC_SIGACTION_SIGINT:
                        sigaction_sigint = (char) sp->u.number;
                        break;

                    case DC_SIGACTION_SIGUSR1:
                        sigaction_sigusr1 = (char) sp->u.number;
                        break;

                    case DC_SIGACTION_SIGUSR2:
                        sigaction_sigusr2 = (char) sp->u.number;
                        break;
                }
            }
            break;
    }

    // free arguments
    return pop_n_elems(2, sp);
} /* f_configure_driver() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_driver_info (svalue_t *sp)

/* EFUN driver_info()
 *
 *   mixed driver_info (int what)
 *
 * Returns information about the runtime environment.
 * <what> can either be a configuration option as given to
 * configure_driver() or one of the following options:
 *
 * <what> == DI_...
 */
{
    svalue_t result;
    p_int what = sp->u.number;

    /* We'll init with 0, because the availability of several
     * options depends on the compile time configuration.
     */
    put_number(&result, 0);

    switch (what)
    {
        default:
            errorf("Illegal value %"PRIdPINT" for driver_info().\n", what);
            return sp; /* NOTREACHED */

        case DC_MEMORY_LIMIT:
        {
            vector_t *v;

            memsafe(v = allocate_array(2), sizeof(*v), "result array");

            put_number(v->item,     get_memory_limit(MALLOC_SOFT_LIMIT));
            put_number(v->item + 1, get_memory_limit(MALLOC_HARD_LIMIT));
            put_array(&result, v);
            break;
        }

        case DC_ENABLE_HEART_BEATS:
            put_number(&result, heart_beats_enabled ? 1 : 0);
            break;

        case DC_LONG_EXEC_TIME:
            put_number(&result, get_profiling_time_limit());
            break;

        case DC_DATA_CLEAN_TIME:
            put_number(&result, time_to_data_cleanup);
            break;

#ifdef USE_TLS
        case DC_TLS_CERTIFICATE:
        {
            int len;
            const unsigned char *fp;

            fp = tls_get_certificate_fingerprint(&len);
            if (!fp)
                put_number(&result, 0);
            else
            {
                string_t *str;
                char * text;

                memsafe(str = alloc_mstring(len*3 - 1), sizeof(*str) + len*3 - 1, "fingerprint");
                text = get_txt(str);

                for (int pos = 0; pos < len; pos++, fp++, text+=3)
                {
                    int b = *fp;
                    int n1 = b >> 4, n2 = b & 0x0f;

                    text[0] = n1 < 10 ? ('0' + n1) : ('A' + n1 - 10);
                    text[1] = n2 < 10 ? ('0' + n2) : ('A' + n2 - 10);
                    if (pos + 1 < len)
                        text[2] = ':';
                }

                put_string(&result, str);
            }
            break;
        }
#endif /* USE_TLS */

        case DC_EXTRA_WIZINFO_SIZE:
            put_number(&result, wiz_info_extra_size);
            break;

        case DC_DEFAULT_RUNTIME_LIMITS:
            put_limits(&result, true);
            break;

        case DC_SWAP_COMPACT_MODE:
            put_number(&result, swap_compact_mode);
            break;

        case DC_DEBUG_FILE:
        {
            char *encoded = convert_path_from_native_or_throw(debug_file, strlen(debug_file));
            put_c_string(&result, encoded);
            break;
        }

        case DC_FILESYSTEM_ENCODING:
            put_c_string(&result, filesystem_encoding);
            break;

        case DC_SIGACTION_SIGHUP:
            put_number(&result, sigaction_sighup);
            break;

        case DC_SIGACTION_SIGINT:
            put_number(&result, sigaction_sigint);
            break;

        case DC_SIGACTION_SIGUSR1:
            put_number(&result, sigaction_sigusr1);
            break;

        case DC_SIGACTION_SIGUSR2:
            put_number(&result, sigaction_sigusr2);
            break;

        /* Driver Environment */
        case DI_BOOT_TIME:
            put_number(&result, boot_time);
            break;

        case DC_SWAP_TIME:
            put_number(&result, time_to_swap);
            break;

        case DC_SWAP_VAR_TIME:
            put_number(&result, time_to_swap_variables);
            break;


        case DC_CLEANUP_TIME:
            put_number(&result, time_to_cleanup);
            break;


        case DC_RESET_TIME:
            put_number(&result, time_to_reset);
            break;


        /* LPC Runtime status */
        case DI_CURRENT_RUNTIME_LIMITS:
            put_limits(&result, false);
            break;

        case DI_EVAL_NUMBER:
            put_number(&result, eval_number);
            break;

        /* Network configuration */
        case DI_MUD_PORTS:
        {
            vector_t *v;
            int i;

            memsafe(v = allocate_array(numports), sizeof(*v), "result array");

            for (i = 0; i < numports; i++)
                put_number(v->item + i, port_numbers[i]);
            put_array(&result, v);
            break;
        }

        case DI_UDP_PORT:
            put_number(&result, udp_port);
            break;

        /* Memory management */
        case DI_MEMORY_RESERVE_USER:
            put_number(&result, reserved_user_size);
            break;

        case DI_MEMORY_RESERVE_MASTER:
            put_number(&result, reserved_master_size);
            break;

        case DI_MEMORY_RESERVE_SYSTEM:
            put_number(&result, reserved_system_size);
            break;

        /* Traces */
        case DI_TRACE_CURRENT:
        {
            vector_t * vec;

            collect_trace(NULL, &vec);
            put_array(&result, vec);
            break;
        }

        case DI_TRACE_CURRENT_DEPTH:
            put_number(&result, control_stack_depth());
            break;

        case DI_TRACE_CURRENT_AS_STRING:
        {
            strbuf_t sbuf;

            strbuf_zero(&sbuf);
            collect_trace(&sbuf, NULL);
            put_string(&result, new_unicode_mstring(sbuf.buf));
            strbuf_free(&sbuf);
            break;
        }

        case DI_TRACE_LAST_ERROR:
            if (current_error_trace)
                put_ref_array(&result, current_error_trace);
            else
                put_number(&result, 0);
            break;

        case DI_TRACE_LAST_ERROR_AS_STRING:
            if (current_error_trace_string)
                put_ref_string(&result, current_error_trace_string);
            else
                put_number(&result, 0);
            break;

        case DI_TRACE_LAST_UNCAUGHT_ERROR:
            if (uncaught_error_trace)
                put_ref_array(&result, uncaught_error_trace);
            else
                put_number(&result, 0);
            break;

        case DI_TRACE_LAST_UNCAUGHT_ERROR_AS_STRING:
            if (uncaught_error_trace_string)
                put_ref_string(&result, uncaught_error_trace_string);
            else
                put_number(&result, 0);
            break;

        /* LPC Runtime statistics */
#ifdef APPLY_CACHE_STAT
        case DI_NUM_FUNCTION_NAME_CALLS:
            put_number(&result, apply_cache_hit+apply_cache_miss);
            break;

        case DI_NUM_FUNCTION_NAME_CALL_HITS:
            put_number(&result, apply_cache_hit);
            break;

        case DI_NUM_FUNCTION_NAME_CALL_MISSES:
            put_number(&result, apply_cache_miss);
            break;
#else
#endif

        case DI_NUM_HEARTBEAT_TOTAL_CYCLES:
            /* FALLTHROUGH */
        case DI_NUM_HEARTBEAT_ACTIVE_CYCLES:
            /* FALLTHROUGH */
        case DI_NUM_HEARTBEATS_LAST_PROCESSED:
            hbeat_driver_info(&result, what);
            break;

        case DI_NUM_STRING_TABLE_STRINGS_ADDED:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_STRINGS_REMOVED:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_LOOKUPS_BY_VALUE:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_LOOKUPS_BY_INDEX:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_LOOKUP_STEPS_BY_VALUE:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_LOOKUP_STEPS_BY_INDEX:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_HITS_BY_VALUE:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_HITS_BY_INDEX:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_COLLISIONS:
            string_driver_info(&result, what);
            break;

        case DI_NUM_REGEX_LOOKUPS:
            /* FALLTHROUGH */
        case DI_NUM_REGEX_LOOKUP_HITS:
            /* FALLTHROUGH */
        case DI_NUM_REGEX_LOOKUP_MISSES:
            /* FALLTHROUGH */
        case DI_NUM_REGEX_LOOKUP_COLLISIONS:
            rxcache_driver_info(&result, what);
            break;

        /* Network statistics */
#ifdef COMM_STAT
        case DI_NUM_MESSAGES_OUT:
            put_number(&result, add_message_calls);
            break;

        case DI_NUM_PACKETS_OUT:
            put_number(&result, inet_packets);
            break;

        case DI_NUM_PACKETS_IN:
            put_number(&result, inet_packets_in);
            break;

        case DI_SIZE_PACKETS_OUT:
            put_number(&result, inet_volume);
            break;

        case DI_SIZE_PACKETS_IN:
            put_number(&result, inet_volume_in);
            break;
#endif

        /* Load */
        case DI_LOAD_AVERAGE_COMMANDS:
            put_float(&result, stat_load.weighted_avg);
            break;

        case DI_LOAD_AVERAGE_LINES:
            put_float(&result, stat_compile.weighted_avg);
            break;

        case DI_LOAD_AVERAGE_PROCESSED_OBJECTS:
            put_float(&result, stat_last_processed.weighted_avg);
            break;

        case DI_LOAD_AVERAGE_PROCESSED_OBJECTS_RELATIVE:
            put_float(&result, relate_statistics(stat_last_processed, stat_in_list));
            break;

        case DI_LOAD_AVERAGE_PROCESSED_HEARTBEATS_RELATIVE:
            hbeat_driver_info(&result, what);
            break;

        /* Memory use statistics */
        case DI_NUM_ACTIONS:
            simulate_driver_info(&result, what);
            break;

        case DI_NUM_CALLOUTS:
            callout_driver_info(&result, what);
            break;

        case DI_NUM_HEARTBEATS:
            hbeat_driver_info(&result, what);
            break;

        case DI_NUM_SHADOWS:
            simulate_driver_info(&result, what);
            break;

        case DI_NUM_OBJECTS:
            put_number(&result, tot_alloc_object);
            break;

        case DI_NUM_OBJECTS_SWAPPED:
            put_number(&result, num_vb_swapped);
            break;

        case DI_NUM_OBJECTS_IN_LIST:
            put_number(&result, num_listed_objs);
            break;

        case DI_NUM_OBJECTS_IN_TABLE:
            otable_driver_info(&result, what);
            break;

        case DI_NUM_OBJECTS_DESTRUCTED:
            /* FALLTHROUGH */
        case DI_NUM_OBJECTS_NEWLY_DESTRUCTED:
            simulate_driver_info(&result, what);
            break;

        case DI_NUM_OBJECTS_LAST_PROCESSED:
            put_number(&result, num_last_processed);
            break;

        case DI_NUM_OBJECT_TABLE_SLOTS:
            otable_driver_info(&result, what);
            break;

        case DI_NUM_PROGS:
            put_number(&result, total_num_prog_blocks + num_swapped - num_unswapped);
            break;

        case DI_NUM_PROGS_SWAPPED:
            put_number(&result, num_swapped - num_unswapped);
            break;

        case DI_NUM_PROGS_UNSWAPPED:
            put_number(&result, num_unswapped);
            break;

        case DI_NUM_ARRAYS:
            put_number(&result, num_arrays);
            break;

        case DI_NUM_MAPPINGS:
            put_number(&result, num_mappings);
            break;

        case DI_NUM_MAPPINGS_CLEAN:
            put_number(&result, num_mappings - num_hash_mappings - num_dirty_mappings);
            break;

        case DI_NUM_MAPPINGS_HASH:
            put_number(&result, num_hash_mappings);
            break;

        case DI_NUM_MAPPINGS_HYBRID:
            put_number(&result, num_dirty_mappings);
            break;

        case DI_NUM_STRUCTS:
            /* FALLTHROUGH */
        case DI_NUM_STRUCT_TYPES:
            struct_driver_info(&result, what);
            break;

        case DI_NUM_VIRTUAL_STRINGS:
            /* FALLTHROUGH */
        case DI_NUM_STRINGS:
            /* FALLTHROUGH */
        case DI_NUM_STRINGS_TABLED:
            /* FALLTHROUGH */
        case DI_NUM_STRINGS_UNTABLED:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_SLOTS:
            /* FALLTHROUGH */
        case DI_NUM_STRING_TABLE_SLOTS_USED:
            string_driver_info(&result, what);
            break;

        case DI_NUM_REGEX:
            /* FALLTHROUGH */
        case DI_NUM_REGEX_TABLE_SLOTS:
            rxcache_driver_info(&result, what);
            break;

        case DI_NUM_LVALUES:
            put_number(&result, num_protected_lvalues);
            break;

        case DI_SIZE_ACTIONS:
            simulate_driver_info(&result, what);
            break;

        case DI_SIZE_CALLOUTS:
            callout_driver_info(&result, what);
            break;

        case DI_SIZE_HEARTBEATS:
            hbeat_driver_info(&result, what);
            break;

        case DI_SIZE_SHADOWS:
            simulate_driver_info(&result, what);
            break;

        case DI_SIZE_OBJECTS:
            put_number(&result, tot_alloc_object_size);
            break;

        case DI_SIZE_OBJECTS_SWAPPED:
            put_number(&result, total_vb_bytes_swapped);
            break;

        case DI_SIZE_OBJECT_TABLE:
            otable_driver_info(&result, what);
            break;

        case DI_SIZE_PROGS:
            put_number(&result, total_prog_block_size + total_bytes_swapped - total_bytes_unswapped);
            break;

        case DI_SIZE_PROGS_SWAPPED:
            put_number(&result, total_bytes_swapped - total_bytes_unswapped);
            break;

        case DI_SIZE_PROGS_UNSWAPPED:
            put_number(&result, total_bytes_unswapped);
            break;

        case DI_SIZE_ARRAYS:
            put_number(&result, total_array_size());
            break;

        case DI_SIZE_MAPPINGS:
            put_number(&result, total_mapping_size());
            break;

        case DI_SIZE_STRUCTS:
            /* FALLTHROUGH */
        case DI_SIZE_STRUCT_TYPES:
            struct_driver_info(&result, what);
            break;

        case DI_SIZE_STRINGS:
            /* FALLTHROUGH */
        case DI_SIZE_STRINGS_TABLED:
            /* FALLTHROUGH */
        case DI_SIZE_STRINGS_UNTABLED:
            /* FALLTHROUGH */
        case DI_SIZE_STRING_TABLE:
            /* FALLTHROUGH */
        case DI_SIZE_STRING_OVERHEAD:
            string_driver_info(&result, what);
            break;

        case DI_SIZE_REGEX:
            rxcache_driver_info(&result, what);
            break;

        case DI_SIZE_BUFFER_FILE:
            /* FALLTHROUGH */
        case DI_SIZE_BUFFER_SWAP:
            mempools_driver_info(&result, what);
            break;


        /* Memory swapper statistics */
        case DI_NUM_SWAP_BLOCKS:
            /* FALLTHROUGH */
        case DI_NUM_SWAP_BLOCKS_FREE:
            /* FALLTHROUGH */
        case DI_NUM_SWAP_BLOCKS_REUSE_LOOKUPS:
            /* FALLTHROUGH */
        case DI_NUM_SWAP_BLOCKS_REUSE_LOOKUP_STEPS:
            /* FALLTHROUGH */
        case DI_NUM_SWAP_BLOCKS_FREE_LOOKUPS:
            /* FALLTHROUGH */
        case DI_NUM_SWAP_BLOCKS_FREE_LOOKUP_STEPS:
            /* FALLTHROUGH */
        case DI_SIZE_SWAP_BLOCKS:
            /* FALLTHROUGH */
        case DI_SIZE_SWAP_BLOCKS_FREE:
            /* FALLTHROUGH */
        case DI_SIZE_SWAP_BLOCKS_REUSED:
            /* FALLTHROUGH */
        case DI_SWAP_RECYCLE_PHASE:
            swap_driver_info(&result, what);
            break;


        /* Memory allocator statistics */
        case DI_MEMORY_ALLOCATOR_NAME:
            /* FALLTHROUGH */

        case DI_NUM_SYS_ALLOCATED_BLOCKS:
        case DI_NUM_LARGE_BLOCKS_ALLOCATED:
        case DI_NUM_LARGE_BLOCKS_FREE:
        case DI_NUM_LARGE_BLOCKS_WASTE:
        case DI_NUM_SMALL_BLOCKS_ALLOCATED:
        case DI_NUM_SMALL_BLOCKS_FREE:
        case DI_NUM_SMALL_BLOCKS_WASTE:
        case DI_NUM_SMALL_BLOCK_CHUNKS:
        case DI_NUM_UNMANAGED_BLOCKS:
        case DI_NUM_FREE_BLOCKS_AVL_NODES:
            /* FALLTHROUGH */

        case DI_SIZE_SYS_ALLOCATED_BLOCKS:
        case DI_SIZE_LARGE_BLOCKS_ALLOCATED:
        case DI_SIZE_LARGE_BLOCKS_FREE:
        case DI_SIZE_LARGE_BLOCKS_WASTE:
        case DI_SIZE_LARGE_BLOCK_OVERHEAD:
        case DI_SIZE_SMALL_BLOCKS_ALLOCATED:
        case DI_SIZE_SMALL_BLOCKS_FREE:
        case DI_SIZE_SMALL_BLOCKS_WASTE:
        case DI_SIZE_SMALL_BLOCK_OVERHEAD:
        case DI_SIZE_SMALL_BLOCK_CHUNKS:
        case DI_SIZE_UNMANAGED_BLOCKS:
        case DI_SIZE_MEMORY_USED:
        case DI_SIZE_MEMORY_UNUSED:
        case DI_SIZE_MEMORY_OVERHEAD:
            /* FALLTHROUGH */

        case DI_NUM_INCREMENT_SIZE_CALLS:
        case DI_NUM_INCREMENT_SIZE_CALL_SUCCESSES:
        case DI_SIZE_INCREMENT_SIZE_CALL_DIFFS:
        case DI_NUM_REPLACEMENT_MALLOC_CALLS:
        case DI_SIZE_REPLACEMENT_MALLOC_CALLS:
        case DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_FULL:
        case DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_TARGETED:
        case DI_NUM_MEMORY_DEFRAGMENTATION_CALL_TARGET_HITS:
        case DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_INSPECTED:
        case DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_MERGED:
        case DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_RESULTING:
            /* FALLTHROUGH */

        case DI_MEMORY_EXTENDED_STATISTICS:
            mem_driver_info(&result, what);
            break;

        /* Status texts */
        case DI_STATUS_TEXT_MEMORY:
        {
            strbuf_t sbuf;
            if (status_parse(&sbuf, ""))
                strbuf_store(&sbuf, &result);
            else
            {
                strbuf_free(&sbuf);
                put_number(&result, 0);
            }
            break;
        }

        case DI_STATUS_TEXT_TABLES:
        {
            strbuf_t sbuf;
            if (status_parse(&sbuf, "tables"))
                strbuf_store(&sbuf, &result);
            else
            {
                strbuf_free(&sbuf);
                put_number(&result, 0);
            }
            break;
        }

        case DI_STATUS_TEXT_SWAP:
        {
            strbuf_t sbuf;
            if (status_parse(&sbuf, "swap"))
                strbuf_store(&sbuf, &result);
            else
            {
                strbuf_free(&sbuf);
                put_number(&result, 0);
            }
            break;
        }

        case DI_STATUS_TEXT_MALLOC:
        {
            strbuf_t sbuf;
            if (status_parse(&sbuf, "malloc"))
                strbuf_store(&sbuf, &result);
            else
            {
                strbuf_free(&sbuf);
                put_number(&result, 0);
            }
            break;
        }

        case DI_STATUS_TEXT_MALLOC_EXTENDED:
        {
            strbuf_t sbuf;
            if (status_parse(&sbuf, "malloc extstats"))
                strbuf_store(&sbuf, &result);
            else
            {
                strbuf_free(&sbuf);
                put_number(&result, 0);
            }
            break;
        }
    }

    /* Clean up the stack and return the result */
    free_svalue(sp);

    *sp = result;
    return sp;
} /* f_driver_info */

/*-------------------------------------------------------------------------*/
svalue_t *
f_dump_driver_info (svalue_t *sp)

/* EFUN dump_driver_info()
 *
 *   int dump_driver_info(int what [, string filename])
 *
 * Dumps runtime information in <file>. Returns 1 on success, 0 otherwise.
 */
{
    bool success;
    string_t * fname;

    if (sp[0].type == T_STRING)
        fname = sp[0].u.str;
    else
        fname = NULL;

    switch (sp[-1].u.number)
    {
        default:
            errorf("Illegal value %"PRIdPINT" for dump_driver_info().\n", sp[-1].u.number);
            return sp; /* NOTREACHED */

        case DDI_OBJECTS:
            success = dumpstat(fname ? fname : STR_OBJDUMP_FNAME);
            break;

        case DDI_OBJECTS_DESTRUCTED:
            success = dumpstat_dest(fname ? fname : STR_DESTOBJDUMP_FNAME);
            break;

        case DDI_OPCODES:
#ifdef OPCPROF
            success = opcdump(fname ? fname : STR_OPCDUMP);
#else
            success = false;
#endif
            break;

        case DDI_MEMORY:
            success = false;
            if (mem_dump_memory(-1))
            {
                int fd;

                if (!fname)
                    fname = STR_MEMDUMP_FNAME;
                fname = check_valid_path(fname, current_object, STR_MEMDUMP, MY_TRUE);
                if (fname)
                {
                    char *native = convert_path_str_to_native_or_throw(fname);
                    fd = open(native, O_CREAT|O_APPEND|O_WRONLY, 0664);
                    if (fd < 0)
                    {
                        perror("open memdump file");
                    }
                    else
                    {
                        writes(fd, "------------------------------------"
                                   "--------------\n");
                        dprintf1(fd, "Date: %s\n", (p_int)time_stamp());
                        success = mem_dump_memory(fd);
                        writes(fd, "\n");
                        close(fd);
                    }
                }
            }
            break;
    }

    sp = pop_n_elems(2, sp);
    push_number(sp, success ? 1 : 0);
    return sp;

} /* f_dump_driver_info */

/*-------------------------------------------------------------------------*/
svalue_t *
v_objects (svalue_t *sp, int num_arg)

/* EFUN objects()
 *
 *   object* objects(int pos = 0, int num = __INT_MAX__)
 *   object* objects(object prev_ob, int num = __INT_MAX__)
 *
 * Returns an array of objects from the global object list.
 * The first form will return the objects starting with position <pos>
 * in the object list. The second form will start with the object
 * following <prev_ob>. Both will return at most <num> objects
 * (given a suitable maximum array size).
 *
 */

{
    svalue_t *arg;
    object_t *ob, *firstob;
    vector_t *vec;
    svalue_t *svp;
    mp_int num, count;

    arg = sp-num_arg+1;
    inter_sp = sp;

    if (num_arg > 2)
        errorf("Too many arguments to objects()\n");

    /* Get the starting object (that is to be included in the result). */
    if (num_arg == 0)
        ob = obj_list;
    else if (arg[0].type == T_OBJECT)
        ob = arg[0].u.ob->next_all;
    else if (arg[0].type == T_NUMBER)
    {
        p_int i;

        ob = obj_list;
        i = arg[0].u.number;

        if (i < 0)
            errorf("Bad arg 1 to objects(): %"PRIdPINT", expected a number >= 0.\n"
                 , i);

        while (ob && --i >= 0) ob = ob->next_all;
    }
    else
        vefun_exp_arg_error(1, TF_NUMBER | TF_OBJECT, arg[0].type, sp);

    /* Get the number of objects in the result array. */
    if (num_arg != 2)
        num = PINT_MAX;
    else if (arg[1].type == T_NUMBER)
    {
        num = arg[1].u.number;

        if (num <= 0)
            errorf("Bad arg 2 to objects(): %"PRIdMPINT", expected a number > 0.\n"
                 , num);
    }
    else
        vefun_exp_arg_error(2, TF_NUMBER, arg[1].type, sp);

    /* First count how many objects that will be. */
    count = 0;
    firstob = ob;

    while (ob && count < num)
    {
        ob = ob->next_all;
        count++;
    }

    /* Now let's get to it. */
    memsafe(vec = allocate_uninit_array(count), sizeof(*vec) + (count-1) * sizeof(*vec->item), "objects");

    count = 0;
    ob = firstob;
    svp = vec->item;

    while (ob && count < num)
    {
        put_ref_object(svp, ob, "objects");

        ob = ob->next_all;
        count++;
        svp++;
    }

    sp = pop_n_elems(num_arg, sp);
    push_array(sp, vec);
    return sp;

} /* v_objects */

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
        svalue_t *clock, *ms;

        if (VEC_SIZE(sp->u.vec) != 2)
            errorf("Bad arg 1 to %s(): Invalid array size %"PRIdPINT
                   ", expected 2.\n"
                 , localTime ? "localtime" : "gmtime"
                 , VEC_SIZE(sp->u.vec));

        clock = get_rvalue(sp->u.vec->item + 0, NULL);
        if (clock == NULL || clock->type != T_NUMBER)
            errorf("Bad arg 1 to %s(): Element 0 is '%s', expected 'int'.\n"
                 , localTime ? "localtime" : "gmtime"
                 , efun_arg_typename((clock ? clock : (sp->u.vec->item + 0))->type));

        ms = get_rvalue(sp->u.vec->item + 1, NULL);
        if (ms == NULL || ms->type != T_NUMBER)
            errorf("Bad arg 1 to %s(): Element 1 is '%s', expected 'int'.\n"
                 , localTime ? "localtime" : "gmtime"
                 , efun_arg_typename((ms ? ms : (sp->u.vec->item + 1))->type));

        clk = clock->u.number;
    }
    else
    {
        clk = sp->u.number;
    }

    pTm = (localTime ? localtime : gmtime)(&clk);
    if (!pTm) {
      errorf("Bad arg 1 to %s(): time value %jd"
          " can't be represented by the host system. Maybe too large?\n",
          localTime ? "localtime" : "gmtime", (intmax_t)clk);
    }

    v = allocate_array(TM_MAX);
    if (!v)
        errorf("Out of memory: array[%d] for result.\n", TM_MAX);

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
        sp->u.number = (p_int)random_number(sp->u.number);

    return sp;
} /* f_random() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_shutdown (svalue_t *sp)

/* EFUN shutdown()
 *
 *   void shutdown()
 *   void shutdown(int exit_code)
 *
 * Shutdown the mud, setting the process result code to
 * <exit_code>, or 0 if not given.
 * 
 * Never use this efun. Instead if you have a need to shutdown
 * the mud use the shutdown command.  You may be asking yourself,
 * if you're not supposed to use it why is it here?  Sorry, I
 * cannot answer that.  Its top secret.
 */

{
    // privilege violation check
    if (privilege_violation(STR_SHUTDOWN, sp, sp))
    {
        extra_jobs_to_do = MY_TRUE;
        game_is_being_shut_down = MY_TRUE;
        exit_code = sp->u.number;
    }
    return --sp;
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
 * In this case, the result string will be cached and tabled.
 *
 * Alternatively, accept an array of two ints: the first is <clock>
 * value as in the first form, the second int is the number of
 * microseconds elapsed in the current second.
 * In this case the result will not be cached as the value is very
 * unlikely to be the same in 2 consecutive calls.
 */

{
    char *ts, *cp;
    string_t *rc;
    
    static mp_int last_time = -1;  // letzte Uhrzeit

    if (sp->type != T_NUMBER)
    {
      /* utime case */
        svalue_t *clock, *ms;
        if (VEC_SIZE(sp->u.vec) != 2)
            errorf("Bad arg 1 to ctime(): Invalid array size %"PRIdPINT
                   ", expected 2.\n", VEC_SIZE(sp->u.vec));

        clock = get_rvalue(sp->u.vec->item + 0, NULL);
        if (clock == NULL || clock->type != T_NUMBER)
            errorf("Bad arg 1 to ctime(): Element 0 is '%s', expected 'int'.\n"
                 , efun_arg_typename((clock ? clock : (sp->u.vec->item + 0))->type));

        ms = get_rvalue(sp->u.vec->item + 1, NULL);
        if (ms == NULL || ms->type != T_NUMBER)
            errorf("Bad arg 1 to ctime(): Element 1 is '%s', expected 'int'.\n"
                 , efun_arg_typename((ms ? ms : (sp->u.vec->item + 1))->type));

        ts = utime_string( clock->u.number, ms->u.number);
        if (!ts)
            errorf("Bad time in ctime(): ({%"PRIdPINT", %"PRIdPINT
                "}) can't be represented by host system. Maybe too large?\n",
                clock->u.number, ms->u.number);

        /* If the string contains nl characters, extract the substring
         * before the first one. Else just copy the (volatile) result
         * we got.
         */
        cp = strchr(ts, '\n');
        if (cp)
        {
            int len = cp - ts;
            memsafe(rc = new_n_unicode_mstring(ts, len), len, "ctime() result");
        }
        else
        {
            memsafe(rc = new_unicode_mstring(ts), strlen(ts), "ctime() result");
        }
    }
    else
    {
      /* second-precision case */
        // test if string for this time is cached
        if (last_time != sp->u.number || last_ctime_result == NULL)
        {
          /* cache is outdated */
            ts = time_fstring(sp->u.number, "%a %b %d %H:%M:%S %Y", 0);
            if (!ts)
                errorf("Bad time in ctime(): %"PRIdMPINT" can't be "
                    "represented by the host system. Maybe too large?\n", 
                    sp->u.number);

            /* If the string contains nl characters, extract the substring
             * before the first one. Else just copy the (volatile) result
             * we got.
             * Table strings, because they are probably used more then once. 
             */
            cp = strchr(ts, '\n');
            if (cp)
            {
                int len = cp - ts;
                memsafe(rc = new_n_unicode_tabled(ts, len), len,
                        "ctime() result");
            }
            else
            {
                memsafe(rc = new_unicode_tabled(ts), strlen(ts),
                        "ctime() result");
            }
            /* fill cache, free last (invalid) string first and don't forget 
             * to increase the ref count for the cache. */
            free_mstring(last_ctime_result);
            last_ctime_result = rc;
            ref_mstring(rc);
            last_time = sp->u.number;
        }
        else {
            // return last result (and increase ref count)
            rc = last_ctime_result;
            ref_mstring(rc);
        }
    }  // if (sp->type != T_NUMBER)
    
    free_svalue(sp);
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

/*-------------------------------------------------------------------------*/
svalue_t *
f_mktime (svalue_t *sp)

/* EFUN mktime()
 *
 *   int time(int* datum)
 *
 * Return the unix timestamp (number of seconds ellapsed since 1. Jan 1970, 
 * 0.0:0 GMT) of the date given in the array datum. datum being an array
 * like the one localtime() or gmtime() return:
 *   int TM_SEC   (0) : Seconds (0..59)
 *   int TM_MIN   (1) : Minutes (0..59)
 *   int TM_HOUR  (2) : Hours (0..23)
 *   int TM_MDAY  (3) : Day of the month (1..31)
 *   int TM_MON   (4) : Month of the year (0..11)
 *   int TM_YEAR  (5) : Year (e.g.  2001)
 *   int TM_WDAY  (6) : Day of the week (Sunday = 0)
 *   int TM_YDAY  (7) : Day of the year (0..365)
 *   int TM_ISDST (8) : TRUE: Daylight saving time
 * TM_YDAY and TM_WDAY are ignored (but must also be ints).
 *
 */
{
    struct tm * pTm; // broken-down time structure for mktime()
    time_t      clk; // unix timestamp corresponding to datum
    vector_t  * v;   // just for convenience, stores argument array 
    int i;
    p_int val[9];

    v = sp->u.vec;
    if (VEC_SIZE(v) != 9)
        errorf("Bad arg 1 to mktime(): Invalid array size %ld, expected 9.\n"
                 , (long)VEC_SIZE(v));
    // all elements must be ints.
    for(i=0; i<VEC_SIZE(v); i++) 
    {
        svalue_t *item = get_rvalue(v->item + i, NULL);
        if ( item == NULL || item->type != T_NUMBER)
            errorf("Bad arg 1 to ctime(): Element %d is '%s', expected 'int'.\n"
                 ,i, efun_arg_typename((item ? item : (v->item + i))->type));
        val[i] = item->u.number;
    }

    // create the time structure
    xallocate(pTm, sizeof(*pTm), "broken-down time structure for mktime()");
    pTm->tm_sec   = val[TM_SEC];
    pTm->tm_min   = val[TM_MIN];
    pTm->tm_hour  = val[TM_HOUR];
    pTm->tm_mday  = val[TM_MDAY];
    pTm->tm_mon   = val[TM_MON];
    pTm->tm_year  = val[TM_YEAR] - 1900;
    pTm->tm_isdst = val[TM_ISDST];
    
    clk = mktime(pTm);

    // free time structure first
    xfree(pTm);
    
    if (clk == -1)
        errorf("Specified date/time cannot be represented as unix timestamp.\n");
    
    // free argument and put result.
    free_svalue(sp);
    put_number(sp, (p_int)clk);
    
    return sp;
} /* f_mktime() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_strftime(svalue_t *sp, int num_arg)
/* EFUN strftime()
 *
 *   string strftime()
 *   string strftime(string fmt)
 *   string strftime(int clock)
 *   string strftime(string fmt, int clock)
 *   string strftime(string fmt, int clock, int localized)
 *
 * Interpret the argument clock as number of seconds since Jan,
 * 1st, 1970, 0.00 and convert it to a nice date and time string.
 * The formatstring must be given in fmt and may contain the placeholders
 * defined in 'man 3 strftime'.
 * If localized == MY_TRUE then the time string will be created with the
 * locale set in the environment variable LC_TIME
 * Defaults: fmt="%c", clock=current_time, localized=MY_TRUE
 * NOTE: the returned string will have at most 511 Characters.
 * TODO: Implement proper caching of the result.
 */

{
    char *ts;
    string_t *rc = NULL;  // ergebnisstring
    
    /* Begin of arguments on the stack */
    svalue_t *arg = sp - num_arg + 1;
    
    // defaults:
    Bool localized = MY_TRUE;
    mp_int clk = current_time;
    char *cfmt = "%c";
    
    // evaluate arguments
    switch(num_arg) {
        case 3:
            localized = (Bool)arg[2].u.number;
            // fall-through
        case 2:
            if (arg[1].u.number < 0)
                errorf("Bad arg 2 to strftime(): got %"PRIdPINT
                    ", expected 0 .. %"PRIdPINT"\n",
                     arg[1].u.number, PINT_MAX);
            clk = arg[1].u.number;
            // fall-through
        case 1:
            // empty strings default to "%c" => only set fmt if non-empty
            if (arg[0].type == T_STRING && mstrsize(arg[0].u.str)) {
                cfmt = get_txt(arg[0].u.str);
            }
            else if (arg[0].type == T_NUMBER) {
                if (num_arg>1) // bei > 1 argument nur strings erlaubt
                    vefun_exp_arg_error(1, TF_STRING, sp->type, sp);
                else if (arg[0].u.number >= 0)
                    clk = arg[0].u.number;
                else
                    errorf("Bad argument 1 to strftime(): got %"PRIdPINT
                        ", expected 0 .. %"PRIdPINT"\n",
                        arg[0].u.number, PINT_MAX);
            }
            break;
    }

    ts = time_fstring(clk,cfmt,localized);
    if (!ts)
        errorf("Bad time in strftime(): %"PRIdMPINT" can't be "
            "represented by the host system. Maybe too large?\n", clk);

    memsafe(rc = new_unicode_tabled(ts), strlen(ts)+sizeof(string_t), "strftime() result");
    
    sp = pop_n_elems(num_arg, sp);
    push_string(sp, rc);
    
    return sp;
} /* f_strftime() */

/***************************************************************************/

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
clear_ref_from_efuns (void)

/* GC support: Clear the refs for the memory containing the (ctime) cache.
 */

{
  if (last_ctime_result)
      clear_string_ref(last_ctime_result);
} /* clear_ref_from_efuns() */

/*-------------------------------------------------------------------------*/
void
count_ref_from_efuns (void)

/* GC support: Count the refs for the memory containing the (ctime) cache.
 */

{
  if (last_ctime_result)
      count_ref_from_string(last_ctime_result);
} /* count_ref_from_wiz_list() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/

