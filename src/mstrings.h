#ifndef MSTRINGS_H_
#define MSTRINGS_H_ 1

#include "driver.h"
#include "svalue.h"
#include "typedefs.h"

#include "hash.h"

/* --- Types --- */

/* --- struct string_s : String structure ---
 *
 * This is the data structure referenced by svalues and constitutes 'the'
 * internal string structure. It is used for both tabled (shared) and
 * untabled (free) strings, and allocated to size.
 *
 * Untabled strings can be constant (.type == STRING_UNTABLED) or
 * mutable (.type == STRING_MUTABLE). The later have char or string
 * range lvalues on them and thus can be modified anytime (and
 * therefore may not be tabled). But also for mutable strings their
 * length stays constant.
 *
 * A mutable string has one (or zero) rvalue reference to it and at
 * least one lvalue reference. (If the lvalue reference vanishes,
 * the string can be made constant again.)
 *
 * Tabled strings are the ones managed in the string table and have .type
 * == STRING_TABLED. The table uses the .next pointer for the table handling.
 * The table reference is not counted.
 *
 * (While the .next pointer is 'ballast' in untabled strings, in practice the
 * majority of strings in a Mud end up being tabled, so not having the
 * overhead of a separate table link structure outweights the overhead
 * for untabled strings.)
 *
 * .refs counts the number of direct references by svalues. A stored refcount of 0 means that the
 * refcounter rolled over and that the string has to be considered a constant.
 * A refcount of 0 after decrementing means that the last reference
 * for this string_t has been removed.
 *
 * .size is the length of the string (excluding the terminating '\0').
 *
 * .hash is the hash code of the string, computed on demand.
 *
 * The string text itself is held at the end of the structure starting
 * at .txt. The structure is allocated large enough to hold the whole
 * string plus an extra terminating '\0' (which is not counted in the size).
 *
 * Regarding unicode we differentiate between 7-bit ASCII strings,
 * UTF8 encoded strings and byte sequences. 7-bit ASCII strings are also
 * considered to be UTF8 encoded strings, but we keep them distinct to optimize
 * string operations (because this would be the majority of strings).
 */

enum string_type
{
    STRING_UNTABLED,    /* Constant string, not in the string table */
    STRING_TABLED,      /* Constant string, entered in the string table */
    STRING_MUTABLE,     /* Mutable string (therefore not in the string table) */
};

enum unicode_type
{
    STRING_ASCII,       /* A string that does only contain chars <= 0x7f. */
    STRING_UTF8,        /* A unicode string encoded in UTF8. */
    STRING_BYTES,       /* Sequence of 8-bit characters. */
};

struct string_s
{
    struct
    {
        enum string_type  type    :  2;
        enum unicode_type unicode :  2;
        unsigned int ref          : 28;
    } info;

    union /* Additional information according to the string type. */
    {
        /* tabled.hash is also used for STRING_UNTABLED. */
        struct
        {
            string_t * next;    /* Linkpointer in string table. */
            hash32_t   hash;    /* 0, or the hash of the string */
        } tabled;

        struct
        {
            /* These are not ref-counted pointers to lvalues
             * of this string.
             */
            struct protected_char_lvalue* char_lvalues;
            struct protected_range_lvalue* range_lvalues;
        } mutable;
    } u;

    size_t     size;    /* Length of the string */
    char       txt[];   /* In fact .size characters plus one '\0' */
      /* The string text follows here */
};

/* --- Constants --- */

#define MSTRING_HASH_LENGTH (256)
  /* The maximum hashed length of a string. */

/* --- Variables --- */

extern mp_uint mstr_used;
extern mp_uint mstr_used_size;
extern string_t *empty_byte_string;

/* --- Prototypes --- */

extern void mstring_init (void);
extern hash32_t   mstring_get_hash (string_t * pStr);
extern hash32_t   hash_string (const char * const s, size_t size);
extern hash32_t   hash_string_chained (const char * const s, size_t size, hash32_t chainhash);
extern string_t * mstring_alloc_string (size_t iSize MTRACE_DECL);
extern string_t * mstring_new_string (const char * const pTxt, enum unicode_type unicode MTRACE_DECL);
extern string_t * mstring_new_n_string (const char * const pTxt, size_t len, enum unicode_type unicode MTRACE_DECL);
extern string_t * mstring_new_tabled (const char * const pTxt, enum unicode_type unicode MTRACE_DECL);
extern string_t * mstring_new_n_tabled (const char * const pTxt, size_t size, enum unicode_type unicode MTRACE_DECL);
extern string_t * mstring_new_unicode_string (const char * const pTxt MTRACE_DECL);
extern string_t * mstring_new_n_unicode_string (const char * const pTxt, size_t len MTRACE_DECL);
extern string_t * mstring_new_unicode_tabled (const char * const pTxt MTRACE_DECL);
extern string_t * mstring_new_n_unicode_tabled (const char * const pTxt, size_t len MTRACE_DECL);
extern string_t * mstring_make_tabled (string_t * pStr, Bool deref_arg MTRACE_DECL);
extern string_t * mstring_make_constant (string_t * pStr, bool in_place_only MTRACE_DECL);
extern string_t * mstring_make_mutable (string_t * pStr MTRACE_DECL);
extern string_t * mstring_dup (string_t * pStr MTRACE_DECL);
extern string_t * mstring_unshare (string_t * pStr MTRACE_DECL);
extern string_t * mstring_resize (string_t * pStr, size_t n MTRACE_DECL);
extern string_t * mstring_find_tabled (string_t * pStr);
extern string_t * mstring_find_tabled_str (const char * const pTxt, size_t size, enum unicode_type unicode);
extern int        mstring_order( string_t * const pStr1
                               , string_t * const pStr2);
extern int        mstring_compare( string_t * const pStr1
                                 , string_t * const pStr2);
extern Bool       mstring_equal( string_t * const pStr1
                               , string_t * const pStr2);
extern void mstring_free (string_t *s);
extern const char * mstring_mstr_n_str(const string_t * const pStr, size_t start, const char * const pTxt, size_t len);
extern const char * mstring_mstr_rn_str(const string_t * const pStr, size_t start, const char * const pTxt, size_t len);
extern string_t * mstring_add_slash (const string_t *str MTRACE_DECL);
extern string_t * mstring_del_slash (string_t *str MTRACE_DECL);
extern string_t * mstring_cvt_progname (const string_t *str MTRACE_DECL);
extern string_t * mstring_del_dotc (string_t *str MTRACE_DECL);
extern string_t * mstring_add (const string_t *left, const string_t *right MTRACE_DECL);
extern string_t * mstring_add_txt (const string_t *left, const char *right, size_t len MTRACE_DECL);
extern string_t * mstring_add_to_txt (const char *left, size_t len, const string_t *right MTRACE_DECL);
extern string_t * mstring_append (string_t *left, const string_t *right MTRACE_DECL);
extern string_t * mstring_append_txt (string_t *left, const char *right, size_t len MTRACE_DECL);
extern string_t * mstring_repeat(const string_t *base, size_t num MTRACE_DECL);
extern string_t * mstring_extract (const string_t *str, size_t start, long end MTRACE_DECL);
extern long       mstring_chr (const string_t *p, char c, long pos);
extern Bool       mstring_prefixed (const string_t *p, const string_t *s);

#ifdef GC_SUPPORT

extern void mstring_clear_refs (void);
extern void mstring_note_refs (void);
extern void mstring_walk_table (void (*func)(string_t *));
extern void mstring_gc_table (void);

#endif /* GC_SUPPORT */

extern mp_int add_string_status (strbuf_t *sbuf, Bool verbose);
extern void   string_driver_info (svalue_t *svp, int value) __attribute__((nonnull(1)));


/* --- Inline functions and macros --- */
static INLINE size_t mstr_mem_size(const string_t * const s) 
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE size_t mstr_mem_size(const string_t * const s)
  /*   The amount of memory used to hold all this strings' data.
   *   Used only to keep the statistics up to date.
   */
{
    return sizeof(string_t) + s->size + 1;
}

static INLINE Bool mstr_untabled(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE Bool mstr_untabled(const string_t * const s)
  /* Return TRUE if string <s> is not tabled.
   */
{
    return s->info.type != STRING_TABLED;
}

static INLINE Bool mstr_tabled(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE Bool mstr_tabled(const string_t * const s)
  /* Return TRUE if string <s> is tabled - directly or indirectly.
   */
{
    return s->info.type == STRING_TABLED;
}

static INLINE Bool mstr_mutable(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE Bool mstr_mutable(const string_t * const s)
  /* Return TRUE if string <s> is mutable.
   */
{
    return s->info.type == STRING_MUTABLE;
}

static INLINE size_t mstrsize(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE size_t mstrsize(const string_t * const s)
  /*   Return the size (length) of the string <s>.
   */
{
    return s->size;
}

static INLINE string_t *ref_mstring(string_t *const s) __attribute__((nonnull(1)));
static INLINE string_t *ref_mstring(string_t *const s)
  /* Increment the refcount for string <s> and return the ref'ed string.
   */
{
    mstr_used++;
    mstr_used_size += mstr_mem_size(s);
    if (s->info.ref)
        ++(s->info.ref);

    return s;
}

static INLINE unsigned int deref_mstring(string_t *const s) __attribute__((nonnull(1)));
static INLINE unsigned int deref_mstring(string_t *const s)
  /*   Decrement the refcount for string <s> and return the new count.
   */
{
    mstr_used--;
    mstr_used_size -= mstr_mem_size(s);
    if (s->info.ref)
        --(s->info.ref);

    return s->info.ref;
}

//static INLINE void free_mstring(string_t *const s) __attribute__((nonnull(1)));
static INLINE void free_mstring(string_t *const s)
  /* Decrement the refcount for string <s>, and if it reaches 0,
   * deallocate <s> altogether.
   * TODO: check if s can really be NULL or should be allowed to be.
   */
{
    //assert(s != NULL);
    if (s != NULL)
    {
        if (s->info.ref == 1)
        {
            mstring_free(s);
        }
        else
            deref_mstring(s);
    }
}

static INLINE char *get_txt(string_t *const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE char *get_txt(string_t *const s)
  /* Return a pointer to the actual string text of string <s>.
   * There is at least one '\0' terminating the string text.
   * BTW: It is a pity that it can't be const char *get_txt().
   */
{
    return s->txt;
}

static INLINE void extract_cstr(char *d, const string_t *const s, size_t l)
                                __attribute__((nonnull(1,2)));
static INLINE void extract_cstr(char *d, const string_t *const s, size_t l)
  /* Extract the C string from <s> (that is: all characters up to the
   * first '\0' resp the end of the string) and copy it into buffer <d>
   * of size <l>. The macro makes sure that the string is terminated
   * with a '\0'
   */
{
    strncpy(d, get_txt((string_t*)s), l-1);
    if (l > mstrsize(s))
        d[mstrsize(s)] = '\0';
    else
        d[l-1] = '\0';
}

static INLINE void put_ref_string(svalue_t * const dest, string_t * const str)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_ref_string(svalue_t * const dest, string_t * const str)
/* Put the string <str> into <dest>, which is considered empty,
 * and increment the refcount of <str>.
 */
{
    *dest = svalue_string(ref_mstring(str));
}

static INLINE void put_ref_bytes(svalue_t * const dest, string_t * const str)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_ref_bytes(svalue_t * const dest, string_t * const str)
/* Put the byte sequence <str> into <dest>, which is considered empty,
 * and increment the refcount of <str>.
 */
{
    *dest = svalue_bytes(ref_mstring(str));
}

static INLINE void put_ref_symbol(svalue_t * const dest, string_t * const str, const ph_int numquotes)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_ref_symbol(svalue_t * const dest, string_t * const str, const ph_int numquotes)
/* Put the symbol <str> with <numquotes> number of quotes into <dest>,
 * which is considered empty, and increment the refcount of <str>.
 */
{
    *dest = svalue_symbol(ref_mstring(str), numquotes);
}

/* A handful of shorthands for commonly used functions */

#define alloc_mstring(iSize)               mstring_alloc_string(iSize MTRACE_ARG)
#define new_mstring(pTxt,unicode)          mstring_new_string(pTxt,unicode MTRACE_ARG)
#define new_n_mstring(pTxt,len,unicode)    mstring_new_n_string(pTxt,len,unicode MTRACE_ARG)
#define new_tabled(pTxt,unicode)           mstring_new_tabled(pTxt,unicode MTRACE_ARG)
#define new_n_tabled(pTxt,len,unicode)     mstring_new_n_tabled(pTxt,len,unicode MTRACE_ARG)
#define new_unicode_mstring(pTxt)          mstring_new_unicode_string(pTxt MTRACE_ARG)
#define new_n_unicode_mstring(pTxt,len)    mstring_new_n_unicode_string(pTxt,len MTRACE_ARG)
#define new_unicode_tabled(pTxt)           mstring_new_unicode_tabled(pTxt MTRACE_ARG)
#define new_n_unicode_tabled(pTxt,len)     mstring_new_n_unicode_tabled(pTxt,len MTRACE_ARG)
#define make_tabled(pStr)                  mstring_make_tabled(pStr, MY_TRUE MTRACE_ARG)
#define make_tabled_from(pStr)             mstring_make_tabled(pStr, MY_FALSE MTRACE_ARG)
#define make_constant(pStr)                mstring_make_constant(pStr, false MTRACE_ARG)
#define try_make_constant(pStr)            mstring_make_constant(pStr, true MTRACE_ARG)
#define make_mutable(pStr)                 mstring_make_mutable(pStr MTRACE_ARG)
#define dup_mstring(pStr)                  mstring_dup(pStr MTRACE_ARG)
#define unshare_mstring(pStr)              mstring_unshare(pStr MTRACE_ARG)
#define resize_mstring(pStr,n)             mstring_resize(pStr,n MTRACE_ARG)
#define find_tabled(pStr)                  mstring_find_tabled(pStr)
#define find_tabled_str(pTxt,unicode)      mstring_find_tabled_str(pTxt, strlen(pTxt), unicode)
#define find_tabled_str_n(pTxt,n,unicode)  mstring_find_tabled_str(pTxt,n,unicode)
#define mstr_get_hash(s)                   (((s)->info.type != STRING_MUTABLE && (s)->u.tabled.hash) ? (s)->u.tabled.hash : mstring_get_hash(s))
#define mstrcmp(pStr1,pStr2)               mstring_compare(pStr1, pStr2)
#define mstr_order(pStr1,pStr2)            mstring_order(pStr1, pStr2)
#define mstreq(pStr1,pStr2)                mstring_equal(pStr1, pStr2)
#define mstrstr(pStr,pTxt)                 mstring_mstr_n_str(pStr, 0, pTxt, strlen(pTxt))
#define mstrrstr(pStr,pTxt)                mstring_mstr_rn_str(pStr, mstrsize(pStr)-1, pTxt, strlen(pTxt))
#define mstr_add(pStr1,pStr2)              mstring_add(pStr1,pStr2 MTRACE_ARG)
#define mstr_add_txt(pStr1,pTxt2,len)      mstring_add_txt(pStr1,pTxt2,len MTRACE_ARG)
#define mstr_add_to_txt(pTxt1,len,pStr2)   mstring_add_to_txt(pTxt1, len, pStr2 MTRACE_ARG)
#define mstr_append(pStr1,pStr2)           mstring_append(pStr1,pStr2 MTRACE_ARG)
#define mstr_append_txt(pStr1,pTxt2,len)   mstring_append_txt(pStr1,pTxt2,len MTRACE_ARG)
#define mstr_repeat(pStr,num)              mstring_repeat(pStr,num MTRACE_ARG)
#define mstr_extract(pStr,start,end)       mstring_extract (pStr,start,end MTRACE_ARG)
#define add_slash(pStr)                    mstring_add_slash(pStr MTRACE_ARG)
#define del_slash(pStr)                    mstring_del_slash(pStr MTRACE_ARG)
#define del_dotc(pStr)                     mstring_del_dotc(pStr MTRACE_ARG)
#define cvt_progname(pStr)                 mstring_cvt_progname(pStr MTRACE_ARG)
#define mstrchr(pStr,c)                    mstring_chr(pStr, c, 0)
#define mstrchrpos(pStr,c,pos)             mstring_chr(pStr, c, pos)
#define mstrprefixed(pStr1, pStr2)         mstring_prefixed(pStr1, pStr2)

#endif /* MSTRINGS_H_ */
