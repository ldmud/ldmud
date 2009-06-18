#ifndef MSTRINGS_H_
#define MSTRINGS_H_ 1

#include "driver.h"
#include "typedefs.h"

#include "hash.h"

/* --- Types --- */

/* --- struct string_s : String structure ---
 *
 * This is the data structure referenced by svalues and constitutes 'the'
 * internal string structure. It is used for both tabled (shared) and
 * untabled (free) strings, and allocated to size.
 *
 * Untabled strings are marked by .tabled == MY_FALSE.
 *
 * Tabled strings are the ones managed in the string table and have .tabled
 * = TRUE. The table uses the .next pointer for the table handling.
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
 */

struct string_s
{
    struct {
        Bool tabled      :  1;
        unsigned int ref : 31;
    } info;
    string_t * next;    /* Linkpointer in string table. */
    size_t     size;    /* Length of the string */
    hash32_t   hash;    /* 0, or the hash of the string */
    char       txt[1];  /* In fact .size characters plus one '\0' */
      /* The string text follows here */
};

/* --- Constants --- */

#define MSTRING_HASH_LENGTH (256)
  /* The maximum hashed length of a string. */

/* --- Variables --- */

extern mp_uint mstr_used;
extern mp_uint mstr_used_size;

/* --- Prototypes --- */

extern void mstring_init (void);
extern hash32_t   mstring_get_hash (string_t * pStr);
extern hash32_t   hash_string (const char * const s, size_t size);
extern hash32_t   hash_string_chained (const char * const s, size_t size, hash32_t chainhash);
extern string_t * mstring_alloc_string (size_t iSize MTRACE_DECL);
extern string_t * mstring_new_string (const char * const pTxt MTRACE_DECL);
extern string_t * mstring_new_n_string (const char * const pTxt, size_t len MTRACE_DECL);
extern string_t * mstring_new_tabled (const char * const pTxt MTRACE_DECL);
extern string_t * mstring_new_n_tabled (const char * const pTxt, size_t size MTRACE_DECL);
extern string_t * mstring_make_tabled (string_t * pStr, Bool deref_arg MTRACE_DECL);
extern string_t * mstring_dup (string_t * pStr MTRACE_DECL);
extern string_t * mstring_unshare (string_t * pStr MTRACE_DECL);
extern string_t * mstring_resize (string_t * pStr, size_t n MTRACE_DECL);
extern string_t * mstring_find_tabled (string_t * pStr);
extern string_t * mstring_find_tabled_str (const char * const pTxt, size_t size);
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
extern long       mstring_chr (const string_t *p, char c);
extern Bool       mstring_prefixed (const string_t *p, const string_t *s);

#ifdef GC_SUPPORT

extern void mstring_clear_refs (void);
extern void mstring_note_refs (void);
extern void mstring_walk_table (void (*func)(string_t *));
extern void mstring_gc_table (void);

#endif /* GC_SUPPORT */

extern mp_int add_string_status (strbuf_t *sbuf, Bool verbose);
extern void   string_dinfo_status(svalue_t *svp, int value);


/* --- Inline functions and macros --- */
static INLINE size_t mstr_mem_size(const string_t * const s) 
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE size_t mstr_mem_size(const string_t * const s)
  /*   The amount of memory used to hold all this strings' data.
   *   Used only to keep the statistics up to date.
   */
{
    return sizeof(string_t) + s->size;
}

static INLINE hash32_t mstr_hash(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE hash32_t mstr_hash(const string_t * const s)
  /*   Return the hash value of string <s>, which is 0 if the
   *   hash hasn't been computed yet.
   */
{
    return s->hash;
}

static INLINE Bool mstr_singular(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE Bool mstr_singular(const string_t * const s)
  /* Return FALSE if string<s> has multiple users, ie. is tabled
   * or has more than one reference.
   */
{
    return ! (s->info.tabled || s->info.ref != 1);
}

static INLINE Bool mstr_untabled(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE Bool mstr_untabled(const string_t * const s)
  /* Return TRUE if string <s> is not tabled.
   */
{
    return !(s->info.tabled);
}

static INLINE Bool mstr_tabled(const string_t * const s)
                                   __attribute__((nonnull(1)))
                                   __attribute__((pure));
static INLINE Bool mstr_tabled(const string_t * const s)
  /* Return TRUE if string <s> is tabled - directly or indirectly.
   */
{
    return s->info.tabled;
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

static INLINE void free_mstring(string_t *const s) __attribute__((nonnull(1)));
static INLINE void free_mstring(string_t *const s)
  /* Decrement the refcount for string <s>, and if it reaches 0,
   * deallocate <s> altogether.
   * TODO: check if s can really be NULL or should be allowed to be.
   */
{
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

/* A handful of shorthands for commonly used functions */

#define alloc_mstring(iSize)     mstring_alloc_string(iSize MTRACE_ARG)
#define new_mstring(pTxt)        mstring_new_string(pTxt MTRACE_ARG)
#define new_n_mstring(pTxt,len)  mstring_new_n_string(pTxt,len MTRACE_ARG)
#define new_tabled(pTxt)         mstring_new_tabled(pTxt MTRACE_ARG)
#define new_n_tabled(pTxt,len)   mstring_new_n_tabled(pTxt,len MTRACE_ARG)
#define make_tabled(pStr)        mstring_make_tabled(pStr, MY_TRUE MTRACE_ARG)
#define make_tabled_from(pStr)   mstring_make_tabled(pStr, MY_FALSE MTRACE_ARG)
#define dup_mstring(pStr)        mstring_dup(pStr MTRACE_ARG)
#define unshare_mstring(pStr)    mstring_unshare(pStr MTRACE_ARG)
#define resize_mstring(pStr,n)   mstring_resize(pStr,n MTRACE_ARG)
#define find_tabled(pStr)          mstring_find_tabled(pStr)
#define find_tabled_str(pTxt)      mstring_find_tabled_str(pTxt, strlen(pTxt))
#define find_tabled_str_n(pTxt,n)  mstring_find_tabled_str(pTxt,n)
#define mstr_get_hash(s)         ((s)->hash ? (s)->hash : mstring_get_hash(s))
#define mstrcmp(pStr1,pStr2)     mstring_compare(pStr1, pStr2)
#define mstr_order(pStr1,pStr2)  mstring_order(pStr1, pStr2)
#define mstreq(pStr1,pStr2)      mstring_equal(pStr1, pStr2)
#define mstrstr(pStr,pTxt)       mstring_mstr_n_str(pStr, 0, pTxt, strlen(pTxt))
#define mstrrstr(pStr,pTxt)      mstring_mstr_rn_str(pStr, mstrsize(pStr)-1, pTxt, strlen(pTxt))
#define mstr_add(pStr1,pStr2)     mstring_add(pStr1,pStr2 MTRACE_ARG)
#define mstr_add_txt(pStr1,pTxt2,len) mstring_add_txt(pStr1,pTxt2,len MTRACE_ARG)
#define mstr_add_to_txt(pTxt1,len,pStr2) mstring_add_to_txt(pTxt1, len, pStr2 MTRACE_ARG)
#define mstr_append(pStr1,pStr2)  mstring_append(pStr1,pStr2 MTRACE_ARG)
#define mstr_append_txt(pStr1,pTxt2,len) mstring_append_txt(pStr1,pTxt2,len MTRACE_ARG)
#define mstr_repeat(pStr,num)    mstring_repeat(pStr,num MTRACE_ARG)
#define mstr_extract(pStr,start,end) mstring_extract (pStr,start,end MTRACE_ARG)
#define add_slash(pStr)          mstring_add_slash(pStr MTRACE_ARG)
#define del_slash(pStr)          mstring_del_slash(pStr MTRACE_ARG)
#define del_dotc(pStr)           mstring_del_dotc(pStr MTRACE_ARG)
#define cvt_progname(pStr)       mstring_cvt_progname(pStr MTRACE_ARG)
#define mstrchr(pStr,c)          mstring_chr(pStr, c)
#define mstrprefixed(pStr1, pStr2) mstring_prefixed(pStr1, pStr2)

#endif /* MSTRINGS_H_ */
