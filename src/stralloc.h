#ifndef STRALLOC_H_
#define STRALLOC_H_ 1

#include "driver.h"
#include "strfuns.h"
#ifdef MALLOC_smalloc
#include "smalloc.h"
#endif
#if defined(DEBUG) && !defined(NO_REF_STRING)
#include "main.h" /* for dprintf*() */
#endif

/* --- Macros --- */

#define SHSTR_OVERHEAD  (sizeof(unsigned short) + sizeof(char *))
  /* Overhead of a shared string, used in interpret:apply_low()
   * for a heuristic.
   */

#define SHSTR_NEXT(str) \
  (*(char **)((char *) (str) - sizeof(unsigned short) - sizeof(char *)))

  /* char* SHSTR_NEXT(char*): return the pointer to the next string
   * in the same hash chain.
   */

#define SHSTR_REFS(str) \
  (*(unsigned short *)((char *) (str) - sizeof(unsigned short)))

  /* ushort SHSTR_REFS(char*): return the number of refs to this string.
   */

#define SHSTR_BLOCK(str) \
  ((char *)(str) - sizeof(unsigned short) - sizeof(char *))

  /* char* SHSTR_BLOCK(char*): return a pointer to the first byte
   * of the memory area of this string.
   */

#ifdef MALLOC_smalloc

#define shstr_malloced_length(str) ( *( \
        (p_uint *)(str-sizeof(char*)-sizeof(unsigned short))\
        - SMALLOC_OVERHEAD) - SMALLOC_OVERHEAD )
#else

#define malloc_size_mask() (~0)

#define shstr_malloced_length(str) (\
        (sizeof(char*) + sizeof(char *) + sizeof(short) +\
        strlen(str) + 1 + sizeof(char *) - 1) / sizeof(char *) - 1)

#endif

/* --- Prototypes --- */
extern void init_shared_strings(void);

extern char  *findstring(char *s);
extern char  *make_shared_string(char *str);
extern void   deref_string(char *str);
extern void   free_string(char *str);
extern mp_int add_string_status(strbuf_t *sbuf, Bool verbose);
extern void   string_dinfo_status(svalue_t *svp);

#ifdef GC_SUPPORT

extern void clear_shared_string_refs(void);
extern void note_shared_string_table_ref(void);
extern void walk_shared_strings(void (*func)(char *, char *) );

#endif /* GC_SUPPORT */

#ifdef CHECK_STRINGS

extern void check_string_table (void);
extern void ref_shadow_string (char *s);

#ifdef GC_SUPPORT

extern void mark_shadow_string_ref (char *s);
extern void inc_shadow_string_ref (char *s);

#endif /* GC_SUPPORT */

#endif /* CHECK_STRINGS */


/* --- Inline functions --- */

#if !defined(NO_REF_STRING)

extern mp_uint stralloc_allocd_strings;
extern mp_uint stralloc_allocd_bytes;

static INLINE char *ref_string(char * str) /* TODO: UNUSED */;

static INLINE char *
ref_string(char *str)
{
    stralloc_allocd_strings++;
    stralloc_allocd_bytes += shstr_malloced_length(str);
    if (SHSTR_REFS(str))
    {
        SHSTR_REFS(str)++;
#ifdef DEBUG
        if (!SHSTR_REFS(str))
            dprintf2(2, "DEBUG: ref_string(): %x '%s' refcount reaches max!\n"
                    , (p_int)str, (p_int)str);
    }
    else
    {
        dprintf2(2, "DEBUG: ref_string(): %x '%s' has 0 refs.\n"
                , (p_int)str, (p_int)str);
#endif
    }
#ifdef CHECK_STRINGS
    ref_shadow_string(str);
#endif
    return str;
}
#endif

#endif /* STRALLOC_H_ */
