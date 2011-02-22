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
#ifdef USE_STRUCTS
 *    efun: to_struct()
#endif
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
 *    efun: debug_info()
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
#include <ctype.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>

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
#ifdef USE_STRUCTS
#include "structs.h"
#endif /* USE_STRUCTS */
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/configuration.h"
#include "../mudlib/sys/objectinfo.h"
#include "../mudlib/sys/regexp.h"
#include "../mudlib/sys/strings.h"
#include "../mudlib/sys/time.h"
#include "../mudlib/sys/tls.h"

/* Variables */
string_t *last_ctime_result = NULL;
  /* points to the result of the last f_ctime() call. If the caller asks for 
   * the same timestamp, it will be returned. */

/* Forward declarations */
static void copy_svalue (svalue_t *dest, svalue_t *, struct pointer_table *, int);

/* Macros */

/*-------------------------------------------------------------------------*/

#ifdef USE_SET_IS_WIZARD
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
        memsafe(new = unshare_mstring(sp->u.str), mstrsize(sp->u.str), "result string");
        sp->u.str = new;
        get_txt(sp->u.str)[0] = toupper((unsigned char)get_txt(sp->u.str)[0]);
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
        errorf("Out of memory for implode() result.\n");

    free_string_svalue(sp);
    sp--;
    free_array(sp->u.vec);

    put_string(sp, str);

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
        memsafe(new = unshare_mstring(sp->u.str), mstrsize(sp->u.str), "result string");
        sp->u.str = new;

        for ( s = get_txt(sp->u.str)+count; count < len; s++, count++)
        {
            c = *s;
            if (c != '\0' && isupper((unsigned char)c))
                *s = (char)tolower((unsigned char)c);
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
            if (sp->u.vec->item[i].type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 1 to md5(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
                return sp;
            }
            argp[i] = (char)sp->u.vec->item[i].u.number & 0xff;
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
            if (sp->u.vec->item[i].type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 1 to sha1(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
            }
            argp[i] = (char)sp->u.vec->item[i].u.number & 0xff;
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
            if (sp->u.vec->item[i].type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 2 to hash(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
            }
            argp[i] = (char)sp->u.vec->item[i].u.number & 0xff;
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
            if (sp->u.vec->item[i].type != T_NUMBER)
            {
                free_mstring(arg);
                errorf("Bad argument 2 to hash(): got mixed*, expected string/int*.\n");
                /* NOTREACHED */
            }
            argp[i] = (char)sp->u.vec->item[i].u.number & 0xff;
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

        for (num_match = i = 0; i < v_size; i++) {
            string_t *line;

            res[i] = MY_FALSE;

            if (v->item[i].type != T_STRING)
                continue;

            if (add_eval_cost(1))
            {
                /* Evalution cost exceeded: we abort matching at this point
                 * and let the interpreter detect the exception.
                 */
                break;
            }

            line = v->item[i].u.str;
            rc = rx_exec(reg, line, 0);
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
            assign_svalue_no_free(&ret->item[j++], &v->item[i]);
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
        
        if (start == mstrsize(text)
         || (match->start == start && ++start == mstrsize(text)) )
            break;
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
 * Search through <txt> for one/all occurences of <pattern> and replace them
 * with the <replace> pattern, returning the result.
 * <replace> can be a string, or a closure returning a string. If it is
 * a closure, it will be called with the matched substring and
 * the position at which it was found as arguments.
 *
 * <flags> is the bit-or of the regexp options, including:
 *   RE_GLOBAL       = 1: when given, all occurences of <pattern> are replace,
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
            ++reslen; /* Empty match leaves old char in place */
            if (++start > mstrsize(text))
                break;
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
                 || start >= end
                   )
                {
                    put_number(&(result->item[i]), 0);
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
            if (start >= end)
            {
                resstr = NULL;
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
svalue_t *
f_strstr (svalue_t *sp)

/* EFUN strstr()
 *
 *   int strstr (string str, string str2, int pos)
 *
 * Returns the index of str2 in str searching from position pos forward.
 * If str2 is not found in str, -1 is returned. The returned
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
    rc = found ? (found - get_txt(base)) : -1;

    sp--;
    free_svalue(sp--);
    free_string_svalue(sp); /* Frees base ! */
    put_number(sp, rc);

    return sp;
} /* f_strstr() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_strrstr (svalue_t *sp)

/* EFUN strrstr()
 *
 *   int strrstr (string str, string str2, int pos)
 *
 * Returns the index of str2 in str searching from position pos backward.
 * If str2 is not found in str, -1 is returned. The returned
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

    if ( 0 != (start = sp->u.number) )
    {
        if (start < 0)
        {
            start += mstrsize(base);
            if (start < 0)
                start = 0;
        }
    }

    found = mstring_mstr_rn_str(base, start, get_txt(pattern), mstrsize(pattern));
    rc = found ? (found - get_txt(base)) : -1;

    sp--;
    free_svalue(sp--);
    free_string_svalue(sp); /* Frees base ! */
    put_number(sp, rc);

    return sp;
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
    char def_ch[3]       /* Buffer for single characters to strip */
      = { '\t', ' ', '\0' };
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
            if (argp[2].u.number < 0 || argp[2].u.number >= 1 << CHAR_BIT)
                errorf("Bad argument 3 to trim(): %"PRIdPINT
                       " is not a character\n", argp[2].u.number);
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
        memsafe(new = unshare_mstring(sp->u.str), mstrsize(sp->u.str), "result string");
        sp->u.str = new;

        for (s = get_txt(sp->u.str)+count; count < len; s++, count++)
        {
            c = *s;

            if ('\0' != c && islower((unsigned char)c))
                *s = (char)toupper((unsigned char)c);
        }
    }

    /* That's it */
    return sp;
} /* f_upper_case() */

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
                str = find_tabled_str_n(parts[i], lens[i]);
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
                                        for ( ; !done && test_z < lens[test_i]; test_z++)
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
                        maybe_at_end = MY_TRUE;

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

            if (pt - tmpmem + ((l < 0) ? -l : l) >= (ptrdiff_t)tmpmem_size)
            {
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
                memmove(tmpmem, tmpmem + n, (size_t)len);
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


    /* now we have what we want */
#ifdef DEBUG
    if ((long)(cp - get_txt(deststr)) != j
     && (!indent_overflows || (long)(cp - get_txt(deststr)) != wrap)
       ) {
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
    if ( NULL == (func2 = find_tabled_str(func)) )
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

        memsafe(objstr = new_mstring(obj), strlen(obj), "object name");
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

    /* If the characters after the last % specifiers didn't match
     * undo the % match.
     */
    if (info.match_req && info.no_match && info.number_of_matches > 0)
        info.number_of_matches--;

    return info.number_of_matches;
} /* e_sscanf() */


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
v_object_info (svalue_t *sp, int num_args)

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
            errorf("Out of memory: array[%d] for result.\n" \
                 , max); \
        svp = v->item; \
    } else { \
        v = NULL; \
        if (value < 0 || value >= max) \
            errorf("Illegal index for object_info(): %d, " \
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
        errorf("Illegal value %"PRIdPINT" for object_info().\n", sp->u.number);
        /* NOTREACHED */
        return sp;

    /* --- The basic information from the object structure */
    case OINFO_BASIC:
        PREP(OIB_MAX);

        flags = o->flags;

        ST_NUMBER(OIB_HEART_BEAT,        (flags & O_HEART_BEAT) ? 1 : 0);
#ifdef USE_SET_IS_WIZARD
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
#ifdef USE_SET_LIGHT
        ST_NUMBER(OIB_TOTAL_LIGHT,       o->total_light);
#else
        ST_NUMBER(OIB_TOTAL_LIGHT,       0);
#endif
        ST_NUMBER(OIB_NEXT_RESET,        o->time_reset);
        ST_NUMBER(OIB_NEXT_CLEANUP,      o->time_cleanup);
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
            errorf("Out of memory: unswap object '%s'.\n", get_txt(o->name));

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
        ST_NUMBER(OIM_SHARE_VARIABLES, (prog->flags & P_SHARE_VARIABLES) ? 1 : 0);
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
} /* v_object_info() */

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
            name = find_tabled_str(sane_name);
        else
            name = find_tabled_str(name0);
        
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
#ifdef USE_SET_IS_WIZARD

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
            errorf("Bad arg to set_is_wizard(): got %"PRIdPINT
                   ", expected -1..1\n", sp->u.number);
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

#endif /* USE_SET_IS_WIZARD */

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

    if (valuep->type == T_STRING)
    {
        result = valuep;

        for (valuep++, left--; left > 0; valuep++, left--)
        {
            int cmp;

            if (valuep->type != T_STRING)
            {
                if (gotArray)
                    errorf("Bad argument to %s(): array[%d] is a '%s', "
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
                    errorf("Bad argument to %s(): array[%d] is a '%s', "
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
            errorf("Bad argument to %s(): array[0] is a '%s', "
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
v_max (svalue_t *sp, int num_arg)

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
} /* v_max() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_min (svalue_t *sp, int num_arg)

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
            errorf("Out of memory\n");
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

#ifdef USE_STRUCTS
    case T_STRUCT:
      {
        string_t *rc;
        string_t *name;
        size_t    size;
        const char * fmt = "<struct %s>";

        name = struct_name(sp->u.strct);
        size = strlen(fmt)+mstrsize(name);

        memsafe(rc = alloc_mstring(size), size, "converted struct");
        sprintf(get_txt(rc), fmt, get_txt(name));
        free_struct(sp->u.strct);
        put_string(sp, rc);
        break;
      }
#endif /* USE_STRUCTS */

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
    case T_SYMBOL:
        /* Split the string into an array of ints */

        len = (p_int)mstrsize(sp->u.str);
        v = allocate_uninit_array((mp_int)len);
        s = get_txt(sp->u.str);
        svp = v->item;
        while (len-- > 0) {
            ch = (unsigned char)*s++;
            put_number(svp, ch);
            svp++;
        }
        free_svalue(sp);
        put_array(sp, v);
        break;
#ifdef USE_STRUCTS
    case T_STRUCT:
      {
        vector_t *vec;
        size_t left;

        left = struct_size(sp->u.strct);
        vec = allocate_array(left);
        while (left-- > 0)
            assign_svalue_no_free(vec->item+left, sp->u.strct->member+left);
        free_struct(sp->u.strct);
        put_array(sp, vec);
        break;
      }
#endif
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
        size_t left;

        if (num_arg > 1)
        {
            if (argp[1].type != T_STRUCT)
                fatal("Bad arg 2 to to_struct(): type %s\n"
                     , typename(argp[1].type));
            if (VEC_SIZE(argp->u.vec) > struct_size(argp[1].u.strct))
            {
                errorf("Too many elements for struct %s: %"PRIdPINT
                       ", expected %ld\n"
                     , get_txt(struct_name(argp[1].u.strct))
                     , VEC_SIZE(argp->u.vec)
                     , (long)struct_size(argp[1].u.strct)
                    );
                /* NOTREACHED */
            }
            st = struct_new(argp[1].u.strct->type);
        }
        else
            st = struct_new_anonymous(VEC_SIZE(argp->u.vec));

        for (left = VEC_SIZE(argp->u.vec); left-- > 0; )
            assign_svalue_no_free(st->member+left, argp->u.vec->item+left);
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
            if (VEC_SIZE(argp->u.vec) > struct_size(argp[1].u.strct))
            {
                errorf("Too many elements for struct %s: %"PRIdPINT
                       ", expected %ld\n"
                     , get_txt(struct_name(argp[1].u.strct))
                     , VEC_SIZE(argp->u.vec)
                     , (long)struct_size(argp[1].u.strct)
                    );
                /* NOTREACHED */
            }
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
                        assign_svalue(&st->member[i], data);
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
                            assign_svalue_no_free(dest++, data++);
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
                    assign_svalue(&st->member[i], member->data);
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
                        assign_svalue_no_free(dest++, src++);
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
                p_int size;
                
                struct_t *oldstruct = argp->u.strct;
                struct_t *newstruct = argp[1].u.strct;
                svalue_t *memberp; // pointer to the first member of the new struct
                svalue_t *omemberp; // pointer to the first member of the old struct

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
                else if (struct_baseof(oldstruct->type, newstruct->type) == 1)
                    size = struct_size(oldstruct); // oldstruct is base and has <= members than newstruct.
                else
                {
                    // completely unrelated structs? Then we don't convert.
                    errorf("Can't convert struct %s into struct %s. Neither is a base of the other.\n",
                           get_txt(struct_unique_name(argp->u.strct)), 
                           get_txt(struct_unique_name(argp[1].u.strct)));
                }
                
                if (oldstruct->ref == 1 
                    && struct_size(newstruct) == struct_size(oldstruct))
                {
                    // special case, the structs have the same number of members. Since it
                    // is not possible to remove/change members inherited from a base struct,
                    // the two structs have the same members. We can just exchange the types
                    // of the structs.
                    struct_free_type(oldstruct->type);
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
                    assign_svalue_no_free(memberp, omemberp);
                }
                // the new struct may have more members than the old one (if oldstruct was
                // the base. That is OK, the extra svalues just remain 0. On the other hand,
                // if the old struct has more members, we just ignore them.
                
                put_struct(argp, newstruct);
                break;
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
                assign_svalue_no_free(&new->item[i], &old->item[i]);
            free_array(old);
            sp->u.vec = new;
        }
        break;
      }
#ifdef USE_STRUCTS
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
                assign_svalue_no_free(&new->member[i], &old->member[i]);
            free_struct(old);
            sp->u.strct = new;
        }
        break;
      }
#endif /* USE_STRUCTS */
    case T_MAPPING:
      {
        mapping_t *old, *new;

        old = sp->u.map;
        if (old->ref != 1)
        {
            DYN_MAPPING_COST(old->num_entries);
            check_map_for_destr(old);
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
                svalue_t * svp = &old->item[i];

                if (svp->type == T_QUOTED_ARRAY
                 || svp->type == T_MAPPING
                 || svp->type == T_POINTER
#ifdef USE_STRUCTS
                 || svp->type == T_STRUCT
#endif /* USE_STRUCTS */
                   )
                    copy_svalue(&new->item[i], svp, ptable, depth+1);
                else
                    assign_svalue_no_free(&new->item[i], svp);
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

#ifdef USE_STRUCTS
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
                svalue_t * svp = &old->member[i];

                if (svp->type == T_QUOTED_ARRAY
                 || svp->type == T_MAPPING
                 || svp->type == T_POINTER
                 || svp->type == T_STRUCT
                   )
                    copy_svalue(&new->member[i], svp, ptable, depth+1);
                else
                    assign_svalue_no_free(&new->member[i], svp);
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
#endif /* USE_STRUCTS */

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
      {
        vector_t *old;

        old = sp->u.vec;
        if (old != &null_vector)
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
        }
        break;
      }
#ifdef USE_STRUCTS
    case T_STRUCT:
      {
        struct_t *old;
        svalue_t new;

        old = sp->u.strct;
        ptable = new_pointer_table();
        if (!ptable)
            errorf("(deep_copy) Out of memory for pointer table.\n");
        copy_svalue(&new, sp, ptable, 0);
        transfer_svalue(sp, &new);
        free_pointer_table(ptable);
        break;
      }
#endif /* USE_STRUCTS */
    case T_MAPPING:
      {
        mapping_t *old;
        svalue_t new;

        old = sp->u.map;
        ptable = new_pointer_table();
        if (!ptable)
            errorf("(deep_copy) Out of memory for pointer table.\n");
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
    else if (sp[-num_arg+1].type == T_STRING)
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
#ifdef USE_STRUCTS
 * If <arg> is a struct, the <flag> setting 2 lets the efun
 * return the basic name of the struct.
#endif
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
#ifdef USE_STRUCTS
 *   - for structs, the unique name of the struct is returned.
#endif
 *   - -1 for all other datatypes.
 *
 * TODO: The flags should be defined in an include file.
 * TODO: The array returned for closures should contain all
 * TODO:: three items.
 */

{
    mp_int i, j;
    string_t *str; /* != NULL: to use instead of j */
    svalue_t *argp;
    p_int flag = -1;

    argp = sp - num_arg + 1;
    i = argp->type;
    j = -1;
    str = NULL;

    if (num_arg == 2 && sp->type == T_NUMBER)
        flag = sp->u.number;

    /* Determine the second return value */
    switch(i)
    {
    case T_STRING:
        j = (mstr_tabled(sp[-1].u.str)) ? 0 : 1;
        break;
    case T_MAPPING:
        j = argp->u.map->num_values;
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
        j = argp->x.generic;
        break;
#ifdef USE_STRUCTS
    case T_STRUCT:
        if (flag == 2)
        {
            sp--;

            str = struct_unique_name(sp->u.strct);
            free_svalue(sp);
            put_ref_string(sp, str);
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
#endif /* USE_STRUCTS */
    }

    /* Depending on flag, return the proper value */
    if (num_arg == 2)
    {
        free_svalue(sp--);
        free_svalue(sp);
        if (flag == 2)
        if (flag != 1) /* 0 or else */
        {
            if (flag) /* neither 0 nor 1 */
            {
                j = -1;
            }
            else
            {
                j = i;
            }
            if (str != NULL)
            {
                free_mstring(str); str = NULL;
            }
        }

        if (str != NULL)
            put_string(sp, str);
        else
            put_number(sp, j);
    }
    else
    {
        vector_t *v;

        v = allocate_array(2);
        v->item[0].u.number = i;
        if (str != NULL)
            put_string(v->item+1, str);
        else
            v->item[1].u.number = j;
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
    else if (arg[0].type == T_STRING)
        return x_map_string(sp, num_arg);
#ifdef USE_STRUCTS
    else if (arg[0].type == T_STRUCT)
        return x_map_struct(sp, num_arg);
#endif /* USE_STRUCTS */
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
 *
 * For arrays and strings, returns the index of the first occurance of
 * second arg in the first arg, or -1 if none found. If <start> is
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
              {
                string_t *str;
                svalue_t *item;

                str = sp_u.str;
                for(item = vec->item + startpos; --cnt >= 0; item++)
                {
                    if (item->type == T_STRING
                     && mstreq(str, item->u.str))
                        break;
                }
                break;
              }

            case T_CLOSURE:
              {
                short type;
                svalue_t *item;

                type = sp->type;
                for(item = vec->item + startpos; --cnt >= 0; item++)
                {
                    /* TODO: Is this C99 compliant? */
                    if (item->type == type && closure_eq(sp, item))
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
                svalue_t *item;

                type = sp->type;
                x_generic = sp->x.generic;
                for(item = vec->item + startpos; --cnt >= 0; item++)
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

                    for (item = vec->item + startpos; --cnt >= 0; item++)
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
#ifdef USE_STRUCTS
            case T_STRUCT:
#endif /* USE_STRUCTS */
              {
                svalue_t *item;
                short type = sp->type;

                for (item = vec->item + startpos; --cnt >= 0; item++)
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

    if (sp[-1].type == T_STRING)
    {
        string_t *str;
        char *str2;
        ptrdiff_t i;

        if (sp->type != T_NUMBER)
            efun_arg_error(2, T_NUMBER, sp->type, sp);
        str = sp[-1].u.str;
        
        if (hasStart && (size_t)startpos >= mstrsize(str))
            i = -1;
        else
        {
            i = sp->u.number;
            str2 = (i & ~0xff) ? NULL
                               : memchr(get_txt(str)+startpos, i, mstrsize(str)-startpos);
            i = str2 ? (str2 - get_txt(str)) : -1;
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
          {
            string_t *str;
            svalue_t *item;

            str = sp_u.str;
            for (item = vec->item+cnt; --cnt >= 0; )
            {
                item--;
                if (item->type == T_STRING
                 && mstreq(str, item->u.str))
                    break;
            }
            break;
          }

        case T_CLOSURE:
          {
            short type;
            svalue_t *item;

            type = sp->type;
            for (item = vec->item+cnt; --cnt >= 0; )
            {
                item--;
                if (item->type == type && closure_eq(sp, item))
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
            svalue_t *item;

            type = sp->type;
            x_generic = sp->x.generic;
            for (item = vec->item+cnt; --cnt >= 0; )
            {
                item--;
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

                for (item = vec->item+cnt; --cnt >= 0; )
                {
                    item--;
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
#ifdef USE_STRUCTS
        case T_STRUCT:
#endif /* USE_STRUCTS */
          {
            svalue_t *item;
            short type = sp->type;

            for (item = vec->item+cnt; --cnt >= 0; )
            {
                item--;
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

    if (sp[-1].type == T_STRING)
    {
        string_t *str;
        ptrdiff_t i;

        if (sp->type != T_NUMBER)
            efun_arg_error(2, T_NUMBER, sp->type, sp);
        str = sp[-1].u.str;
        if (!hasStart || (size_t)startpos >= mstrsize(str))
            startpos = mstrsize(str);
        i = sp->u.number;
        if ((i & ~0xff) != 0)
        {
            i = -1;
        }
        else
        {
            char * cp, *start, *str2;
            start = get_txt(str);
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

            i = str2 ? (str2 - get_txt(str)) : -1;
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
    Bool changeInPlace = MY_FALSE;

    /* If the argument is passed in by reference, make sure that it is
     * an array, note the fact, and place it directly into the stack.
     * TODO: Allow protected ranges here.
     */
    if (sp->type == T_LVALUE || sp->type == T_PROTECTED_LVALUE)
    {
        svalue_t * svp = sp;
        vector_t * vec = NULL;

        while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
        {
            svp = svp->u.lvalue;
        }

        if (svp->type != T_POINTER)
        {
            inter_sp = sp;
            errorf("Bad arg 1 to reverse(): got '%s &', "
                  "expected 'string/mixed */mixed * &'.\n"
                 , typename(svp->type));
            /* NOTREACHED */
            return sp;
        }

        changeInPlace = MY_TRUE;

        vec = ref_array(svp->u.vec);
        free_svalue(sp);
        put_array(sp, vec);
    }

    if (sp->type == T_NUMBER)
    {
        p_int res;

        /* Try to use a fast bit swapping algorithm.
         * The slow fallback default is a loop swapping bit-by-bit.
         */

#if SIZEOF_PINT == 8

        res = sp->u.number;

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

        res = sp->u.number;

        res = ((res & 0xaaaaaaaa) >> 1) | ((res & 0x55555555) << 1);
	res = ((res & 0xcccccccc) >> 2) | ((res & 0x33333333) << 2);
	res = ((res & 0xf0f0f0f0) >> 4) | ((res & 0x0f0f0f0f) << 4);
	res = ((res & 0xff00ff00) >> 8) | ((res & 0x00ff00ff) << 8);
	res = (res >> 16) | (res << 16);

#else

        unsigned char * from, * to;
        int num;

        from = (unsigned char *)&sp->u.number;
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
    else if (sp->type == T_STRING)
    {
        size_t len = mstrsize(sp->u.str);

        /* If the length of the string is less than 2, there nothing to do */
        if (len > 1)
        {
            char *h, *str;
            string_t *res;

            memsafe(res = alloc_mstring(len), len, "reversed string");
            h = get_txt(res);
            h += len - 1;
            str = get_txt(sp->u.str);

            while (len--)
                *h-- = *str++;
            free_string_svalue(sp);
            put_string(sp, res);
        }
    }
    else if (sp->type == T_POINTER)
    {
        mp_int v_size;
        vector_t *vec = NULL;

        /* If we change in place, the 'new' vector is the old one
         * with just one reference added. Same if the vector has only
         * one reference to begin with, or is the null vector.
         */
        if (changeInPlace
         || sp->u.vec->ref == 1
         || sp->u.vec == &null_vector)
        {
            vec = ref_array(sp->u.vec);
        }
        else
        {
            vector_t *old;
            size_t size, i;

            old = sp->u.vec;
            size = VEC_SIZE(old);
            vec = allocate_uninit_array((int)size);
            if (!vec)
                errorf("(reverse) Out of memory: array[%lu] for copy.\n"
                     , (unsigned long) size);
            for (i = 0; i < size; i++)
                assign_svalue_no_free(&vec->item[i], &old->item[i]);
        }

        /* If the length of the array is less than 2, there nothing to do */
        if ((v_size = (mp_int)VEC_SIZE(vec)) > 1)
        {
            mp_int half, i;

            DYN_ARRAY_COST(v_size);

            i = 0;
            half = v_size / 2;
            while (i < half)
            {
                svalue_t tmp;
                tmp   = *(vec->item + i);
                *(vec->item + i) = *(vec->item + (v_size - 1) - i);
                *(vec->item + (v_size - 1) - i) = tmp;
                i++;
            }
        }

        /* Replace the old array by the new one. */
        free_svalue(sp);
        put_array(sp, vec);
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
 * 
 * <data> is dependent on <what>:
 *   DC_MEMORY_LIMIT:        ({soft-limit, hard-limit}) both <int>, given in Bytes.
 *   DC_ENABLE_HEART_BEATS:  0/1 (int)
 *   DC_LONG_EXEC_TIME:      0 - __INT_MAX__ (int), given in microseconds.
 *
 */

{

    // Check for privilege_violation.
    if (!privilege_violation2(STR_CONFIGURE_DRIVER, sp-1, sp, sp))
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
                efun_arg_error(1, T_POINTER, sp->type, sp);
            if (VEC_SIZE(sp->u.vec) != 2)
                errorf("Bad arg 1 to configure_driver(): Invalid array size %"PRIdPINT
                       ", expected 2.\n"
                       , VEC_SIZE(sp->u.vec));
            if (sp->u.vec->item[0].type != T_NUMBER)
                errorf("Bad arg 1 to configure_driver(): Element 0 is '%s', expected 'int'.\n"
                       , typename(sp->u.vec->item[0].type));
            if (sp->u.vec->item[1].type != T_NUMBER)
                errorf("Bad arg 1 to configure_driver(): Element 1 is '%s', expected 'int'.\n"
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
                efun_arg_error(1, T_NUMBER, sp->type, sp);
            heart_beats_enabled = sp->u.number != 0 ? MY_TRUE : MY_FALSE;
            break;
            
        case DC_LONG_EXEC_TIME:
            if (sp->type != T_NUMBER)
                efun_arg_error(1, T_NUMBER, sp->type, sp);
            if (!set_profiling_time_limit(sp->u.number))
                errorf("Could not set the profiling time limit for long executions "
                       "(%"PRIdPINT") in configure_driver()\n",
                       sp->u.vec->item[0].u.number);
            break;
    }

    // free arguments
    return pop_n_elems(2, sp);
} /* f_configure_driver() */
/*-------------------------------------------------------------------------*/
svalue_t *
v_debug_info (svalue_t *sp, int num_arg)

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
 * DINFO_OBJLIST (2): Objects from the global object list are
 *     returned.  If the optional <arg2> is omitted, the first
 *     element(s) (numbered 0) is returned. If the <arg2> is a
 *     number n, the n'th element(s) of the object list returned. If the
 *     <arg2> is an object, it's successor(s) in the object list is
 *     returned.
 *     The optional <arg3> specifies the maximum number of objects
 *     returned. If it's 0, a single object is returned. If it is
 *     a positive number m, an array with at max 'm' objects is
 *     returned. This way, by passing __INT_MAX__ as <arg3> it is
 *     possible to create an array of all objects in the game
 *     (given a suitable maximum array size).
 *
 * DINFO_MALLOC: Equivalent to typing ``malloc'' at the command line.
 *     No second arg must be given. Returns 0.
 *
 * DINFO_STATUS (4): Collect the status information of the driver.  The
 *     optional second arg can be 0, "tables", "swap", "malloc", "malloc
 *     extstats" or any other argument accepted by the actual driver.  The
 *     result is a printable string with the status information, or 0 if an
 *     invalid argument was given.
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
 *     <arg2> == "memory": dump a list of all allocated memory blocks (if
 *       the allocator supports this).
 *       Default filename is '/MEMORY_DUMP', the valid_write()
 *       will read 'memdump' for the function, and the new data
 *       will be appended to the end of the file.
 *
 *       If the allocator doesn't support memory dumps, this call will
 *       always return 0, and nothing will be written.
 *
 *       This works best if the allocator is compiled with
 *       MALLOC_TRACE and/or MALLOC_LPC_TRACE.
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
 *        int DID_ST_BOOT_TIME
 *            The time() when the mud was started.
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
 *        int DID_ST_CALLOUT_SIZE
 *            Number and total size of pending call_outs.
 *
 *        int DID_ST_ARRAYS
 *        int DID_ST_ARRAYS_SIZE
 *            Number and size of all arrays.
 *
 *        int DID_ST_MAPPINGS
 *        int DID_ST_MAPPINGS_SIZE
 *            Number and size of all mappings.
 *
 *        int DID_ST_HYBRID_MAPPINGS
 *        int DID_ST_HASH_MAPPINGS
 *            Number of hybrid (hash+condensed) and hash mappings.
 *
 *        int DID_ST_STRUCTS
 *        int DID_ST_STRUCTS_SIZE
 *            Number and size of all struct instances.
 *
 *        int DID_ST_STRUCT_TYPES
 *        int DID_ST_STRUCT_TYPES_SIZE
 *            Number and size of all struct type instances.
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
 *        int DID_ST_UNTABLED
 *        int DID_ST_UNTABLED_SIZE
 *            Total number and size of existing untabled strings.
 *
 *        int DID_ST_TABLED
 *        int DID_ST_TABLED_SIZE
 *            Total number and size of existing directly tabled strings.
 *
 *        int DID_ST_STR_CHAINS
 *            Number of hash chains in the string table.
 *
 *        int DID_ST_STR_ADDED
 *            Number of distinct strings added to the table so far.
 *
 *        int DID_ST_STR_DELETED
 *            Number of distinct strings removed from the table so far.
 *
 *        int DID_ST_STR_COLLISIONS
 *            Number of distinct strings added to an existing hash chain
 *            so far.
 *
 *        int DID_ST_STR_SEARCHES
 *        int DID_ST_STR_SEARCHLEN
 *            Number and accumulated length of string searches by address.
 *
 *        int DID_ST_STR_SEARCHES_BYVALUE
 *        int DID_ST_STR_SEARCHLEN_BYVALUE
 *            Number and accumulated length of string searches by value.
 *
 *        int DID_ST_STR_FOUND
 *        int DID_ST_STR_FOUND_BYVALUE
 *            Number of successful searches by address resp. by value.
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
 *        int DID_ST_MB_FILE
 *            The size of the 'File' memory buffer.
 *
 *        int DID_ST_MB_SWAP
 *            The size of the 'Swap' memory buffer.
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
 *            The name of the allocator: "sysmalloc", "smalloc",
 *            "slaballoc"
 *  
 *        int DID_MEM_SBRK          (slaballoc, smalloc)
 *        int DID_MEM_SBRK_SIZE     (slaballoc, smalloc)
 *            Number and size of memory blocks requested from the
 *            operating system (non-mmapped memory).
 *  
 *        int DID_MEM_LARGE         (slaballoc, smalloc)
 *        int DID_MEM_LARGE_SIZE    (slaballoc, smalloc)
 *        int DID_MEM_LFREE         (slaballoc, smalloc)
 *        int DID_MEM_LFREE_SIZE    (slaballoc, smalloc)
 *            Number and size of large allocated resp. free blocks.
 *            smalloc: The large allocated blocks include the
 *            small chunk blocks.
 *  
 *        int DID_MEM_LWASTED       (slaballoc, smalloc)
 *        int DID_MEM_LWASTED_SIZE  (slaballoc, smalloc)
 *            Number and size of unusable large memory fragments.
 *  
 *        int DID_MEM_CHUNK         (smalloc)
 *        int DID_MEM_CHUNK_SIZE    (smalloc)
 *            Number and size of small chunk blocks.
 *  
 *        int DID_MEM_SLAB          (slaballoc)
 *        int DID_MEM_SLAB_SIZE     (slaballoc)
 *            Number and size of slabs (including fully free slabs).
 *  
 *        int DID_MEM_SLAB_FREE      (slaballoc)
 *        int DID_MEM_SLAB_FREE_SIZE (slaballoc)
 *            Number and size of free slabs (part of DID_MEM_SLAB).
 *  
 *        int DID_MEM_SMALL         (slaballoc, smalloc)
 *        int DID_MEM_SMALL_SIZE    (slaballoc, smalloc)
 *        int DID_MEM_SFREE         (slaballoc, smalloc)
 *        int DID_MEM_SFREE_SIZE    (slaballoc, smalloc)
 *            Number and size of small allocated resp. free blocks.
 *  
 *        int DID_MEM_SWASTED       (smalloc)
 *        int DID_MEM_SWASTED_SIZE  (smalloc)
 *            Number and size of unusably small memory fragments.
 *  
 *        int DID_MEM_SMALL_OVERHEAD_SIZE  (slaballoc)
 *            Size of the slab management overhead (not including
 *            the overhead incurred by each allocated small block).
 *  
 *        int DID_MEM_MINC_CALLS    (slaballoc, smalloc)
 *        int DID_MEM_MINC_SUCCESS  (slaballoc, smalloc)
 *        int DID_MEM_MINC_SIZE     (slaballoc, smalloc)
 *            Number of calls to malloc_increment(), the number
 *            of successes and the size of memory allocated this
 *            way.
 *  
 *        int DID_MEM_PERM         (slaballoc, smalloc)
 *        int DID_MEM_PERM_SIZE    (slaballoc, smalloc)
 *            Number and size of permanent (non-GCable) allocations.
 *  
 *        int DID_MEM_CLIB         (slaballoc, smalloc)
 *        int DID_MEM_CLIB_SIZE    (slaballoc, smalloc)
 *            Number and size of allocations done through the
 *            clib functions (if supported by the allocator).
 *  
 *        int DID_MEM_OVERHEAD     (slaballoc, smalloc)
 *            Overhead for every allocation.
 *  
 *        int DID_MEM_ALLOCATED    (slaballoc, smalloc)
 *            The amount of memory currently allocated from the
 *            allocator, including the overhead for the allocator.
 *  
 *        int DID_MEM_USED         (slaballoc, smalloc)
 *            The amount of memory currently used for driver data,
 *            excluding the overhead from the allocator.
 *  
 *        int DID_MEM_TOTAL_UNUSED (slaballoc, smalloc)
 *            The amount of memory allocated from the system, but
 *            not used by the driver.
 *  
 *        int DID_MEM_DEFRAG_CALLS       (smalloc)
 *            Total number of calls to defragment_small_lists().
 *   
 *        int DID_MEM_DEFRAG_CALLS_REQ   (smalloc)
 *            Number of calls to defragment_small_lists() with a
 *            desired size.
 *   
 *        int DID_MEM_DEFRAG_REQ_SUCCESS (smalloc)
 *            Number of times, a defragmentation for a desired
 *            size was successful.
 *   
 *        int DID_MEM_BLOCKS_INSPECTED   (smalloc)
 *            Number of blocks inspected during defragmentations.
 *   
 *        int DID_MEM_BLOCKS_MERGED      (smalloc)
 *            Number of blocks merged during defragmentations.
 *   
 *        int DID_MEM_BLOCKS_RESULT      (smalloc)
 *            Number of defragmented blocks (ie. merge results).
 *
#ifdef USE_AVL_FREELIST
 *        int DID_MEM_AVL_NODES          (slaballoc, smalloc)
 *            Number of AVL nodes used to manage the large free
 *            blocks. This value might go away again.
#endif
#ifdef MALLOC_EXT_STATISTICS
 *        mixed * DID_MEM_EXT_STATISTICS (slaballoc, smalloc)
 *            If the driver was compiled with extended smalloc
 *            statistics, they are returned in this entry; if the
 *            driver was compiled without the statistics, 0 is
 *            returned.
 *
 *            This value might go away again.
 *
 *            The array contains NUM+2 entries, where NUM is the
 *            number of distinct small block sizes. Entry [NUM]
 *            describes the statistics of oversized small blocks
 *            (smalloc) resp. for all slabs (slaballoc),
 *            entry [NUM+1] summarizes all large blocks. Each
 *            entry is an array of these fields:
 *
 *              int DID_MEM_ES_MAX_ALLOC:
 *                Max number of allocated blocks of this size.
 *
 *              int DID_MEM_ES_CUR_ALLOC:
 *                Current number of allocated blocks of this size.
 *
 *              int DID_MEM_ES_MAX_FREE:
 *                Max number of allocated blocks of this size.
 *
 *              int DID_MEM_ES_CUR_FREE:
 *                Current number of allocated blocks of this size.
 *
 *              float DID_MEM_ES_AVG_XALLOC:
 *                Number of explicit allocation requests per
 *                second.
 *
 *              float DID_MEM_ES_AVG_XFREE:
 *                Number of explicit deallocation requests per
 *                second.
 *
 *              int DID_MEM_ES_FULL_SLABS:
 *                Number of fully used slabs (slaballoc only).
 *
 *              int DID_MEM_ES_FREE_SLABS:
 *                Number of fully free slabs (slaballoc only).
 *
 *              int DID_MEM_ES_TOTAL_SLABS:
 *                Total number of slabs: partially used, fully used
 *                and fully free (slaballoc only).
 *
 *            The allocation/deallocation-per-second statistics do
 *            not cover internal shuffling of the freelists.
 *
 *            The slab statistics (entry [NUM], slaballoc only)
 *            shows in the AVG statistics the frequence with which 
 *            slabs were allocated from resp. returned to the large
 *            memory pool.
#endif
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
 * DINFO_EVAL_NUMBER (8): Return the current evaluation number. It's
 *     incremented for each top-level call. Some examples are: commands,
 *     calls to heart_beat, reset, or clean_up, and calls generated by
 *     call_out or input_to.
 *     The counter may overflow.
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
            errorf("bad number of arguments to debug_info\n");
        if (arg[1].type != T_OBJECT)
            vefun_arg_error(2, T_OBJECT, arg[1].type, sp);
        ob = arg[1].u.ob;
        flags = ob->flags;
        add_message("O_HEART_BEAT      : %s\n",
          flags&O_HEART_BEAT      ?"TRUE":"FALSE");
#ifdef USE_SET_IS_WIZARD
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
#ifdef USE_SET_LIGHT
        add_message("total light : %d\n", (int)ob->total_light);
#endif
        add_message("time_reset  : %"PRIdMPINT"\n", ob->time_reset);
        add_message("time_of_ref : %"PRIdMPINT"\n", ob->time_of_ref);
        add_message("ref         : %"PRIdPINT"\n", ob->ref);
#ifdef DEBUG
        add_message("extra_ref   : %"PRIdPINT"\n", ob->extra_ref);
#endif
        if (ob->gigaticks)
            add_message("evalcost   :  %"PRIuMPINT"%09"PRIuMPINT"\n", 
                        (mp_uint)ob->gigaticks, (mp_uint)ob->ticks);
        else
            add_message("evalcost   :  %"PRIdMPINT"\n", (mp_uint)ob->ticks);
        add_message("swap_num    : %"PRIdPINT"\n", O_SWAP_NUM(ob));
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
            errorf("bad number of arguments to debug_info\n");
        if (sp->type != T_OBJECT)
            vefun_arg_error(2, T_OBJECT, sp->type, sp);
        if ((sp->u.ob->flags & O_SWAPPED) && load_ob_from_swap(sp->u.ob) < 0)
            errorf("Out of memory: unswap object '%s'\n", get_txt(sp->u.ob->name));
        pg = sp->u.ob->prog;
        add_message("program ref's %3"PRIdPINT"\n", pg->ref);
        add_message("Name: '%s'\n",                get_txt(pg->name));
        add_message("program size    %6"PRIuPINT"\n"
          ,(p_uint)(PROGRAM_END(*pg) - pg->program));
        add_message("num func's:  %3u (%4"PRIuPINT")\n", 
                    (unsigned int)pg->num_functions,
                    (p_uint)(pg->num_functions * sizeof(uint32) +
                              pg->num_function_names * sizeof(short)));
        add_message("num vars:    %3u (%4"PRIuPINT")\n", 
                    (unsigned int)pg->num_variables,
                    (p_uint)(pg->num_variables * sizeof(variable_t)));

        v1 = program_string_size(pg, &v0, &v2);
        add_message("num strings: %3u (%4"PRIdMPINT") : overhead %"PRIdMPINT
                    "+ data %"PRIdMPINT" (%"PRIdMPINT")\n"
                   , (unsigned int)pg->num_strings
                   , v0 + v1
                   , v0
                   , v1
                   , v2
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
            add_message("num inherits %3d (%4"PRIuPINT")\n", cnt
                , (p_uint)(pg->num_inherited * sizeof(inherit_t)));
        }
        add_message("total size      %6"PRIdPINT"\n"
          ,pg->total_size);

        v1 = data_size(sp->u.ob, &v2);
        add_message("data size       %6"PRIdMPINT" (%6"PRIdMPINT")\n",
                    v1, v2);
        break;
      }

    case DINFO_OBJLIST:  /* --- DINFO_OBJLIST --- */
      {
        /* Get the first/next object in the object list */

        int i, m;
        ob = obj_list;
        i = 0;
        m = 0;

        if (num_arg > 2)
        {
            if (arg[2].type != T_NUMBER)
                vefun_exp_arg_error(3, (1 << T_NUMBER)
                                     , arg[2].type, sp);
            m = arg[2].u.number;
            if (m < 0)
                errorf("Bad arg3 to debug_info(DINFO_OBJLIST): %ld, "
                      "expected a number >= 0.\n"
                     , (long)m);
        }
 
        if (num_arg > 1)
        {
            if (arg[1].type == T_NUMBER)
            {
                i = arg[1].u.number;
            }
            else
            {
                if (arg[1].type != T_OBJECT)
                    vefun_exp_arg_error(2, (1 << T_OBJECT)|(1 << T_NUMBER)
                                         , arg[1].type, sp);
                ob = arg[1].u.ob;
                i = 1;
            }
        }

        while (ob && --i >= 0) ob = ob->next_all;
        if (ob)
        {
            if (m < 1)
                put_ref_object(&res, ob, "debug_info");
            else
            {
                /* Caller expects an array of at max m objects. */
                object_t * obj_start = ob;
                size_t len;
                vector_t * rc;

                /* First count how many objects we have. */
                for (len = 0; ob && len < (size_t)m; len++, ob = ob->next_all)
                    NOOP;

                rc = allocate_uninit_array(len);
                if (!rc)
                    outofmemory("result array");

                /* Now transfer all the objects into the array. */
                for ( len = 0, ob = obj_start
                    ; ob && len < (size_t)m
                    ; len++, ob = ob->next_all)
                    put_ref_object(rc->item+len, ob, "debug_info");

                put_array(&res, rc);
            }
        }
        else if (m > 0)
        {
            /* No object found, but caller expects an array */
            put_array(&res, allocate_array(0));
        }
        /* else: no object found, and no array expected: just return 0 */
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
            errorf("bad number of arguments to debug_info\n");
        if (num_arg == 1
         || (sp->type == T_NUMBER && sp->u.number == 0)) {
            sp->u.str = STR_EMPTY; /* Just for status_parse() */
        } else {
            if (arg[1].type != T_STRING)
                vefun_exp_arg_error(2, (1 << T_STRING)|(1 << T_NULL)
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
            errorf("bad number of arguments to debug_info\n");

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

        if (mstreq(arg[1].u.str, STR_OBJECTS))
        {
            res.u.number = dumpstat(fname ? fname : STR_OBJDUMP_FNAME) ? 1 : 0;
            break;
        }

        if (mstreq(arg[1].u.str, STR_DESTRUCTED))
        {
            res.u.number = dumpstat_dest(fname ? fname : STR_DESTOBJDUMP_FNAME) ? 1 : 0;
            break;
        }

        if (mstreq(arg[1].u.str, STR_OPCODES))
        {
#ifdef OPCPROF
            res.u.number = opcdump(fname ? fname : STR_OPCDUMP) ? 1 : 0;
#endif
            break;
        }

        if (mstreq(arg[1].u.str, STR_MEMORY))
        {
            if (mem_dump_memory(-1))
            {
                int fd;

                if (!fname)
                    fname = STR_MEMDUMP_FNAME;
                fname = check_valid_path(fname, current_object, STR_MEMDUMP, MY_TRUE);
                if (fname)
                {
                    fd = open(get_txt(fname), O_CREAT|O_APPEND|O_WRONLY, 0664);
                    if (fd < 0)
                    {
                        perror("open memdump file");
                    }
                    else
                    {
                        writes(fd, "------------------------------------"
                                   "--------------\n");
                        dprintf1(fd, "Date: %s\n", (p_int)time_stamp());
                        res.u.number = mem_dump_memory(fd) ? 1 : 0;
                        writes(fd, "\n");
                        close(fd);
                    }

                    free_mstring(fname);
                }
            }
            break;
        }

        errorf("Bad argument '%s' to debug_info(DINFO_DUMP).\n", get_txt(arg[1].u.str));
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
            errorf("bad number of arguments to debug_info\n");
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
                    errorf("Out of memory: array[%d] for result.\n" \
                         , which); \
                dinfo_arg = v->item; \
            } else { \
                v = NULL; \
                if (value < 0 || value >= which) \
                    errorf("Illegal index for debug_info(): %d, " \
                          "expected 0..%d\n", value, which-1); \
                dinfo_arg = &res; \
            }

        case DID_STATUS:
#define ST_NUMBER(which,code) \
    if (value == -1) dinfo_arg[which].u.number = code; \
    else if (value == which) dinfo_arg->u.number = code

            PREP(DID_STATUS_MAX)

            ST_NUMBER(DID_ST_BOOT_TIME, boot_time);
            dinfo_data_status(dinfo_arg, value);
            otable_dinfo_status(dinfo_arg, value);
            hbeat_dinfo_status(dinfo_arg, value);
            callout_dinfo_status(dinfo_arg, value);
            string_dinfo_status(dinfo_arg, value);
#ifdef USE_STRUCTS
            struct_dinfo_status(dinfo_arg, value);
#endif /* USE_STRUCTS */
            rxcache_dinfo_status(dinfo_arg, value);
            mb_dinfo_status(dinfo_arg, value);

            if (value == -1)
                put_array(&res, v);
            break;
#undef ST_NUMBER

        case DID_SWAP:
            PREP(DID_SWAP_MAX)

            swap_dinfo_data(dinfo_arg, value);
            if (value == -1)
                put_array(&res, v);
            break;

        case DID_MEMORY:
            PREP(DID_MEMORY_MAX)

            mem_dinfo_data(dinfo_arg, value);
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
            errorf("bad number of arguments to debug_info\n");

        if (num_arg == 2 && sp->type != T_NUMBER)
            errorf("bad arg 2 to debug_info(): not a number.\n");

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
            put_string(&res, new_mstring(sbuf.buf));
            strbuf_free(&sbuf);
        }
        else if (sp->u.number == DIT_CURRENT_DEPTH)
        {
            put_number(&res, control_stack_depth());
        }
        else
            errorf("bad arg 2 to debug_info(): %"PRIdPINT", expected 0..2\n"
                 , sp->u.number);
        break;
      }

    case DINFO_EVAL_NUMBER:
        /* return current eval number */
        if (num_arg != 1)
            errorf("bad number of arguments to debug_info\n");
        put_number(&res, eval_number);
        break;

    default:
        errorf("Bad debug_info() request value: %"PRIdPINT"\n", 
               arg[0].u.number);
        /* NOTREACHED */
        break;
    }

    /* Clean up the stack and return the result */

    sp = pop_n_elems(num_arg, sp);

    sp++;
    *sp = res;
    return sp;
} /* v_debug_info() */

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
        if (VEC_SIZE(sp->u.vec) != 2)
            errorf("Bad arg 1 to %s(): Invalid array size %"PRIdPINT
                   ", expected 2.\n"
                 , localTime ? "localtime" : "gmtime"
                 , VEC_SIZE(sp->u.vec));
        if (sp->u.vec->item[0].type != T_NUMBER)
            errorf("Bad arg 1 to %s(): Element 0 is '%s', expected 'int'.\n"
                 , localTime ? "localtime" : "gmtime"
                 , efun_arg_typename(sp->u.vec->item[0].type));
        if (sp->u.vec->item[1].type != T_NUMBER)
            errorf("Bad arg 1 to %s(): Element 1 is '%s', expected 'int'.\n"
                 , localTime ? "localtime" : "gmtime"
                 , efun_arg_typename(sp->u.vec->item[1].type));
        clk = sp->u.vec->item[0].u.number;
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
        if (VEC_SIZE(sp->u.vec) != 2)
            errorf("Bad arg 1 to ctime(): Invalid array size %"PRIdPINT
                   ", expected 2.\n", VEC_SIZE(sp->u.vec));
        if (sp->u.vec->item[0].type != T_NUMBER)
            errorf("Bad arg 1 to ctime(): Element 0 is '%s', expected 'int'.\n"
                 , efun_arg_typename(sp->u.vec->item[0].type));
        if (sp->u.vec->item[1].type != T_NUMBER)
            errorf("Bad arg 1 to ctime(): Element 1 is '%s', expected 'int'.\n"
                 , efun_arg_typename(sp->u.vec->item[1].type));

        ts = utime_string( sp->u.vec->item[0].u.number
                         , sp->u.vec->item[1].u.number);
        if (!ts)
            errorf("Bad time in ctime(): ({%"PRIdPINT", %"PRIdPINT
                "}) can't be represented by host system. Maybe too large?\n",
                sp->u.vec->item[0].u.number,
                sp->u.vec->item[1].u.number);

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
    }
    else
    {
      /* second-precision case */
        // test if string for this time is cached
        if (last_time != sp->u.number)
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
                memsafe(rc = new_n_tabled(ts, len), len,
                        "ctime() result");
            }
            else
            {
                memsafe(rc = new_tabled(ts), strlen(ts),
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

    v = sp->u.vec;
    if (VEC_SIZE(v) != 9)
        errorf("Bad arg 1 to mktime(): Invalid array size %ld, expected 9.\n"
                 , (long)VEC_SIZE(v));
    // all elements must be ints.
    for(i=0; i<VEC_SIZE(v); i++) 
    {
        if ( v->item[i].type != T_NUMBER)
            errorf("Bad arg 1 to ctime(): Element %d is '%s', expected 'int'.\n"
                 ,i, efun_arg_typename(v->item[0].type));
    }

    // create the time structure
    xallocate(pTm, sizeof(*pTm), "broken-down time structure for mktime()");
    pTm->tm_sec   = v->item[TM_SEC].u.number;
    pTm->tm_min   = v->item[TM_MIN].u.number;
    pTm->tm_hour  = v->item[TM_HOUR].u.number;
    pTm->tm_mday  = v->item[TM_MDAY].u.number;
    pTm->tm_mon   = v->item[TM_MON].u.number;
    pTm->tm_year  = v->item[TM_YEAR].u.number - 1900;
    pTm->tm_isdst = v->item[TM_ISDST].u.number;
    
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

    memsafe(rc = new_tabled(ts), strlen(ts)+sizeof(string_t), "strftime() result");
    
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

