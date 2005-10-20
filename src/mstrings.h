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
 * big enough to hold both the structure and the string text.
 * TODO: Add the allocation size?
 */

struct string_data_s
{
    size_t size;    /* Length of the string */
    char   txt[1];  /* In fact .size characters */
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
 * strings) by other string_ts. A stored refcount of 0 means that the refcounter
 * rolled over and that the string has to be considered a constant.
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
extern string_t * mstring_alloc_string (size_t iSize);
extern string_t * mstring_new_string (const char * const pTxt);
extern string_t * mstring_new_tabled (const char * const pTxt);
extern string_t * mstring_make_tabled (string_t * pStr);
extern string_t * mstring_table_inplace (string_t * pStr);
extern string_t * mstring_dup (string_t * pStr);
extern string_t * mstring_find_tabled (const string_t * pStr);
extern string_t * mstring_find_tabled_str (const char * const pTxt);
extern void mstring_free (string_t *s);

#ifdef GC_SUPPORT

extern void mstring_clear_refs (void);
extern void mstring_note_refs (void);
extern void mstring_walk_strings (void (*func)(string_t *));

#endif /* GC_SUPPORT */

extern mp_int add_string_status (strbuf_t *sbuf, Bool verbose);
extern void string_dinfo_status (svalue_t *svp);


/* --- Inline functions and macros --- */

#define mstr_mem_size(s) \
    (sizeof(string_t) + sizeof(string_data_t) + (s)->str->size - 1)

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
    (mstr_used++, mstr_used_size += mstr_mem_size(s), (s)->info.ref ? (++((s)->info.ref), (s)) : (s))

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

#define get_cstr(s) \
    ((s)->str->txt)

  /* char * get_cstr (string_t *s)
   *
   *   Return a pointer to the actual string text of string <s>.
   *   CAVEAT: the string text is not terminated with '\0'!
   */

/* A handful of shorthands for commonly used functions */

#define alloc_mstring(iSize)   mstring_alloc_string(iSize)
#define new_mstring(pTxt)      mstring_new_string(pTxt)
#define new_tabled(pTxt)       mstring_new_tabled(pTxt)
#define make_tabled(pStr)      mstring_make_tabled(pStr)
#define table_inplace(pStr)    mstring_table_inplace(pStr)
#define dup_mstring(pStr)      mstring_dup(pStr)
#define find_tabled(pStr)      mstring_find_tabled(pStr)
#define find_tabled_str(pStr)  mstring_find_tabled_str(pTxt)

#endif /* MSTRINGS_H_ */
