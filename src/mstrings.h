#ifndef MSTRINGS_H_
#define MSTRINGS_H_ 1

#include "driver.h"
#include "typedefs.h"

/* --- Types --- */

/* --- struct string_data_s : String text structure ---
 *
 * The string_data structure holds the actual string text, and just
 * the string text. The string may contain any character, including '\0'.
 * .size holds the length of the string, the allocation size is unknown but
 * big enough to hold both the structure, the string text, and a terminating
 * '\0' which is not part of the string data proper (and not counted in
 * .size).
 * TODO: Add the allocation size?
 */

struct string_data_s
{
    size_t size;    /* Length of the string */
    char   txt[1];  /* In fact .size characters plus one '\0' */
      /* The string text follows here */
};

typedef struct string_data_s string_data_t;


/* --- struct string_s : String management structure ---
 *
 * This is the data structure referenced by svalues and constitutes 'the'
 * internal string structure. It is used for both tabled (shared) and
 * untabled (free) strings.
 *
 * Untabled strings are marked by .tabled == MY_FALSE and .shared == NULL.
 *
 * Tabled strings are the ones managed in the string table. .tabled
 * == TRUE, .link links the string_ts within their hash chain, and string
 * points to the actual string.
 *
 * Indirectly tabled strings are not in the string table themselves, but
 * reference to one: .tabled == FALSE, .link points to the string_t
 * in the string table (this counts as 1 reference), and .str points
 * to the same string_data_t the tabled string_t uses.
 *
 * .refs counts the number of direct references by svalues and (for tabled
 * strings) by other string_ts. A stored refcount of 0 means that the
 * refcounter rolled over and that the string has to be considered a constant.
 * A refcount of 0 after decrementing means that the last reference
 * for this string_t has been removed.
 */

struct string_s
{
    string_t *link;
    struct {
        Bool tabled      :  1;
        unsigned int ref : 31;
    } info;
    string_data_t *str;
};

/* --- Variables --- */

extern mp_uint mstr_used;
extern mp_uint mstr_used_size;

/* --- Prototypes --- */

extern void mstring_init (void);
extern string_t * mstring_alloc_string (size_t iSize MTRACE_DECL);
extern string_t * mstring_new_string (const char * const pTxt MTRACE_DECL);
extern string_t * mstring_new_n_string (const char * const pTxt, size_t len MTRACE_DECL);
extern string_t * mstring_new_tabled (const char * const pTxt MTRACE_DECL);
extern string_t * mstring_make_tabled (string_t * pStr, Bool deref_arg MTRACE_DECL);
extern string_t * mstring_table_inplace (string_t * pStr MTRACE_DECL);
extern string_t * mstring_dup (string_t * pStr MTRACE_DECL);
extern string_t * mstring_resize (string_t * pStr, size_t n MTRACE_DECL);
extern string_t * mstring_find_tabled (const string_t * pStr);
extern string_t * mstring_find_tabled_str (const char * const pTxt, size_t size);
extern int        mstring_compare(const string_t * const pStr1
                                 , const string_t * const pStr2); 
extern Bool       mstring_equal(const string_t * const pStr1
                               , const string_t * const pStr2); 
extern void mstring_free (string_t *s);
extern char *     mstring_mstr_n_str(const string_t * const pStr, size_t start, const char * const pTxt, size_t len);
extern string_t * mstring_add_slash (const string_t *str MTRACE_DECL);
extern string_t * mstring_add (const string_t *left, const string_t *right MTRACE_DECL);
extern string_t * mstring_add_txt (const string_t *left, const char *right, size_t len MTRACE_DECL);
extern string_t * mstring_add_to_txt (const char *left, size_t len, const string_t *right MTRACE_DECL);
extern string_t * mstring_repeat(const string_t *base, size_t num MTRACE_DECL);
extern string_t * mstring_extract (const string_t *str, size_t start, long end MTRACE_DECL);
extern Bool       mstring_prefixed (const string_t *p, const string_t *s);

#ifdef GC_SUPPORT

extern void mstring_clear_refs (void);
extern void mstring_note_refs (void);
extern void mstring_walk_strings (void (*func)(string_t *));

#endif /* GC_SUPPORT */

extern mp_int add_string_status (strbuf_t *sbuf, Bool verbose);
extern void string_dinfo_status (svalue_t *svp);


/* --- Inline functions and macros --- */

#define mstr_mem_size(s) \
    (sizeof(string_t) + sizeof(string_data_t) + (s)->str->size)

  /* size_t mstr_mem_size(string_t * s)
   *   The amount of memory used to hold all this strings' data.
   *   Used only to keep the statistics up to date.
   */

#define mstr_untabled(s) \
    (!(s)->info.tabled && !(s)->link)

  /* Bool mstr_untabled (string_t *s)
   *   Return TRUE if string <s> is not tabled.
   */

#define mstr_tabled(s) \
    ((s)->info.tabled || (s)->link != NULL)

  /* Bool mstr_tabled (string_t *s)
   *   Return TRUE if string <s> is tabled - directly or indirectly.
   */

#define mstr_d_tabled(s) \
    ((s)->info.tabled)

  /* Bool mstr_d_tabled (string_t *s)
   *   Return TRUE if string <s> is tabled directly.
   */

#define mstr_i_tabled(s) \
    (!(s)->info.tabled && (s)->link != NULL)

  /* Bool mstr_i_tabled (string_t *s)
   *   Return TRUE if string <s> is tabled indirectly.
   */

#define mstrsize(s) \
    ((s)->str->size)

  /* size_t mstrsize(string_t *s)
   *   Return the size (length) of the string <s>.
   */

#define ref_mstring(s) \
    (mstr_used++, mstr_used_size += mstr_mem_size(s), (s)->info.ref ? ++((s)->info.ref) : 0, (s))

  /* string_t * ref_mstring (string_t *s)
   *   Increment the refcount for string <s> and return the ref'ed string.
   */


#define deref_mstring(s) \
    (mstr_used--, mstr_used_size -= mstr_mem_size(s), (s)->info.ref ? --((s)->info.ref) : (s)->info.ref)

  /* int deref_mstring (string_t *s)
   *   Decrement the refcount for string <s> and return the new count.
   */


#define free_mstring(s) \
    MACRO(if ((s)->info.ref == 1) mstring_free(s); else deref_mstring(s); )

  /* void free_mstring(s)
   *
   *   Decrement the refcount for string <s>, and if it reaches 0, 
   *   deallocate <s> altogether.
   */

#define get_txt(s) \
    ((s)->str->txt)

  /* char * get_txt (string_t *s)
   *
   *   Return a pointer to the actual string text of string <s>.
   *   There is at least one '\0' terminating the string text.
   */

#define extract_cstr(d,s,l) \
    MACRO(strncpy((d), get_txt(s), (l)-1); \
          if ((l) >= mstrsize(s)) \
            d[mstrsize(s)-1] = '\0'; \
          else \
            d[(l)-1] = '\0'; \
         )

  /* void extract_str (char * d, string_t *s, size_t l)
   *
   *   Extract the C string from <s> (that is: all characters up to the
   *   first '\0' resp the end of the string) and copy it into buffer <d>
   *   of size <l>. The macro makes sure that the string is terminated
   *   with a '\0'
   */

/* A handful of shorthands for commonly used functions */

#define alloc_mstring(iSize)     mstring_alloc_string(iSize MTRACE_ARG)
#define new_mstring(pTxt)        mstring_new_string(pTxt MTRACE_ARG)
#define new_n_mstring(pTxt,len)  mstring_new_n_string(pTxt,len MTRACE_ARG)
#define new_tabled(pTxt)         mstring_new_tabled(pTxt MTRACE_ARG)
#define make_tabled(pStr)        mstring_make_tabled(pStr, MY_TRUE MTRACE_ARG)
#define make_tabled_from(pStr)   mstring_make_tabled(pStr, MY_FALSE MTRACE_ARG)
#define table_inplace(pStr)      mstring_table_inplace(pStr MTRACE_ARG)
#define dup_mstring(pStr)        mstring_dup(pStr MTRACE_ARG)
#define resize_mstring(pStr,n)   mstring_resize(pStr,n MTRACE_ARG)
#define find_tabled(pStr)          mstring_find_tabled(pStr)
#define find_tabled_str(pTxt)      mstring_find_tabled_str(pTxt, strlen(pTxt))
#define find_tabled_str_n(pTxt,n)  mstring_find_tabled_str(pTxt,n)
#define mstrcmp(pStr1,pStr2)     mstring_compare(pStr1, pStr2)
#define mstreq(pStr1,pStr2)      mstring_equal(pStr1, pStr2)
#define mstrstr(pStr,pTxt)       mstring_mstr_n_str(pStr, 0, pTxt, strlen(pTxt))
#define mstr_add(pStr1,pStr2)     mstring_add(pStr1,pStr2 MTRACE_ARG)
#define mstr_add_txt(pStr1,pTxt2,len) mstring_add_txt(pStr1,pTxt2,len MTRACE_ARG)
#define mstr_add_to_txt(pTxt1,len,pStr2) mstring_add_to_txt(pTxt1, len, pStr2 MTRACE_ARG)
#define mstr_repeat(pStr,num)    mstring_repeat(pStr,num MTRACE_ARG)
#define mstr_extract(pStr,start,end) mstring_extract (pStr,start,end MTRACE_ARG)
#define add_slash(pStr)          mstring_add_slash(pStr MTRACE_ARG)
#define mstrprefixed(pStr1, pStr2) mstring_prefixed(pStr1, pStr2)

#endif /* MSTRINGS_H_ */
