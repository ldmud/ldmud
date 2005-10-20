/*---------------------------------------------------------------------------
 * Shared String Management
 *
 *---------------------------------------------------------------------------
 * To reduce the memory used for string storage, the driver implements
 * string sharing: a particular string is allocated once, and a refcount
 * keeps track how many users it has. If the refcount reaches 0, the string
 * safely be deallocated again.
 *
 * To convert non-shared strings into shared strings quickly, the shared
 * strings are hashed over their first 100 characters and arranged in
 * a hash table with HTABLE_SIZE entries. Every entry points to the head
 * of a chain of strings with equal hash. To make access even faster,
 * the chains are rearranged after every search so that the recently found
 * string is right at the head.
 *
 * The disadvantage is of course that it is not simply possible the strings
 * once they have been shared.
 *
 * The strings are allocated together with a header structure :
 *
 *    struct shared_string
 *    {
 *        char        * next;
 *        StrRefCount   refs;
 *        char          str[];
 *    }
 *
 * and all shared string pointers, including the .next, point to
 * the address of .str[]. To not waste memory on pad bytes, the structure
 * elements are accessed implicite through pointer arithmetic.
 *
 * .next is the pointer to the next element in the hash chain (NULL for
 * the last element), and .refs is the number of references. If .refs
 * accumulates enough references to overflow into value 0, the string is
 * considered constant and excepted from refcounting until the next
 * garbage collection.
 *---------------------------------------------------------------------------
 */

#define NO_REF_STRING
#include "driver.h"

#include <stdio.h>

#include "stralloc.h"

#include "gcollect.h"
#include "hash.h"
#include "main.h"
#include "simulate.h"
#include "smalloc.h"
#include "stdstrings.h"
#include "strfuns.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"

#define NEXT(p) SHSTR_NEXT(p)
#define REFS(p) SHSTR_REFS(p)

/*-------------------------------------------------------------------------*/

static char ** base_table = NULL;
  /* The string hash table: an array of pointers to the heads of
   * the string chains.
   */

static int hash_index;
  /* Hash index computed last in findstring().
   */

/* Statistics */

mp_uint stralloc_allocd_strings = 0;
  /* Number of virtually allocated strings - every reference counts
   * as separate copy.
   */

mp_uint stralloc_allocd_bytes = 0;
  /* Total virtual size of allocated strings counted in multiples
   * of sizeof(char*) - every reference counts as separate copy.
   */

static mp_uint num_distinct_strings = 0;
  /* Number of distinct strings in the string table.
   */

static mp_uint bytes_distinct_strings = 0;
  /* Total memory held in the string table.
   */

static mp_uint search_len = 0;
  /* Number of search steps along hash chains.
   */

static mp_uint num_str_searches = 0;
  /* Number of searches in the string table.
   */

/* TODO: Add these statistics to debug_info() */
static mp_uint num_found = 0;       /* Number of successful searches */
static mp_uint num_added = 0;       /* Number of strings added */
static mp_uint num_deleted = 0;     /* Number of strings deleted */
static mp_uint num_collisions = 0;  /* Number of hashcollisions when adding */
static mp_uint num_entries = 0;     /* Number of used cache entries */

/*-------------------------------------------------------------------------*/

/* Hash function, adapted to our table size.
 */

#if !( (HTABLE_SIZE) & (HTABLE_SIZE)-1 )
#    define StrHash(s) (whashstr((s), 100) & ((HTABLE_SIZE)-1))
#else
#    define StrHash(s) (whashstr((s), 100) % HTABLE_SIZE)
#endif

/*-------------------------------------------------------------------------*/

#ifdef KEEP_STRINGS

/* As another debugging aid, this defined keeps the strings in the
 * table if if their refcount is zero.
 */

#endif

#ifdef CHECK_STRINGS

/* As a debug measure, the structures of the string table are mirrored
 * in a second table, and every access to the strings checks if the
 * mirrored data still matches the original.
 *
 * The idea is to catch wild memory writes, for which the string table
 * seems to be the prime target, as soon as possible after they occured.
 *
 * Define VERBOSE_CS to get lots of debug output.
 */

#undef VERBOSE_CS

/* --- struct str_shadow_s: Descriptor for one shared string */

typedef struct str_shadow_s str_shadow_t;

struct str_shadow_s {
    str_shadow_t   *next;  /* Next shadow in hash chain */
    char           *str;   /* The shadowed string */
    StrRefCount     ref;   /* The shadowed refcount */
    char           *snext; /* The "next" pointer of the shadowed string */
};

static str_shadow_t ** shadow_table = NULL;
  /* The shadow hash table: an array of pointers to the heads of
   * the shadow chains.
   */

static str_shadow_t * last_shadow = NULL;
  /* Last shadow found in findstring() or alloc_new_string().
   */

/*-------------------------------------------------------------------------*/
static INLINE void
check_string (char * s, str_shadow_t *sh)

/* Check the shared string <s> against its shadow <sh>
 */

{
    if (!s && !sh)
        return;

    if (s && !sh)
        fatal("check_string: String %p vs shadow %p\n", s, sh);

    if (!s && sh)
        fatal("check_string: String %p vs shadow %p (s %p, r%lu, n %p)\n"
             , s, sh, sh->str, (unsigned long)sh->ref, sh->snext);

#ifdef VERBOSE_CS
    printf("DEBUG: compare sh %p (s %p, r %lu, n %p)\n"
          , sh, sh->str, (unsigned long)sh->ref, sh->snext);
    printf("DEBUG:   with  s  %p (r %lu, n %p)\n"
          , s, (unsigned long)REFS(s), NEXT(s));
    fflush(stdout);
#endif
    if (sh->str != s)
        fatal("check_string: String %p '%s', shadow expected string %p '%s'\n"
             , s, s, sh->str, sh->str);

    if (sh->ref != REFS(s))
        fatal("check_string: String %p '%s' has %lu refs, shadow expected %lu refs\n", s, s, (unsigned long)REFS(s), (unsigned long)sh->ref);

    if (sh->snext != NEXT(s))
        fatal("check_string: String %p '%s's next is %p, shadow expected %p\n", s, s, NEXT(s), sh->snext);

} /* check_string() */

/*-------------------------------------------------------------------------*/
static str_shadow_t *
find_shadow (char *s)

/* Find the shadow for shared string <s>, return its pointer and also store
 * the pointer in last_shadow().
 * All but the string searched are checked.
 */

{
    char * curr, *prev;
    int h;
    str_shadow_t *scurr, *sprev;


    h = StrHash(s);

    curr = base_table[h];
    prev = NULL;
    scurr = shadow_table[h];
    sprev = NULL;

    while (curr)
    {
        if (curr == s)
        {
            /* found it */
            last_shadow = scurr;
            return scurr;
        }
        check_string(curr, scurr);
        prev = curr;
        curr = NEXT(curr);
        sprev = scurr;
        scurr = scurr->next;
    }
    check_string(curr, scurr);

    return NULL;
} /* find_shadow() */

/*-------------------------------------------------------------------------*/
void
check_string_table (void)

/* Check the whole string table against the shadow table.
 */

{
    int           h;
    char         *curr;
    str_shadow_t *scurr;

#ifdef VERBOSE_CS
    printf("DEBUG: checking string table\n");
    fflush(stdout);
#endif
    for (h = 0; h < HTABLE_SIZE; h++)
    {
        curr = base_table[h];
        scurr = shadow_table[h];
        while (curr)
        {
            check_string(curr, scurr);
            curr = NEXT(curr);
            scurr = scurr->next;
        }
        check_string(curr, scurr);
    }
#ifdef VERBOSE_CS
    printf("DEBUG: checking string table done\n");
    fflush(stdout);
#endif
} /* check_string_table() */

/*-------------------------------------------------------------------------*/
void
ref_shadow_string (char *s)

/* Increment the refcount for string <s> in the shadow table.
 */

{
    str_shadow_t *sh;

    sh = find_shadow(s);
    if (sh->ref)
        sh->ref++;
    check_string(s, sh);
#ifdef VERBOSE_CS
    printf("DEBUG: ref'd s %p (%lu, %p) and sh %p (%p, %lu, %p)\n"
          , s, (unsigned long)REFS(s), NEXT(s)
          , sh, sh->str, (unsigned long)sh->ref, sh->snext);
    fflush(stdout);
#endif
} /* ref_shadow_string() */

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
mark_shadow_string_ref (char *s)

/* GC Support: Mark the reference for string <s> in the shadow table.
 * This function is to be used for yet-unreferenced strings.
 */

{
    str_shadow_t *sh;

    sh = find_shadow(s);
    sh->ref++;
    if (!sh->ref)
    {
        dprintf2(gcollect_outfd, "String shadow (mark): %x '%s' refcount reaches max!\n"
                , (p_int)sh, (p_int)sh->str);
        sh->ref--;
    }
} /* mark_shadow_string_ref() */

/*-------------------------------------------------------------------------*/
void
inc_shadow_string_ref (char *s)

/* GC Support: Increment the refcound for string <s> in the shadow table.
 * This function is to be used for already referenced strings.
 */

{
    str_shadow_t *sh;

    sh = find_shadow(s);
    if (sh->ref)
    {
        sh->ref++;
        if (!sh->ref)
        {
            dprintf2(gcollect_outfd, "String shadow (inc): %x '%s' refcount reaches max!\n"
                    , (p_int)sh, (p_int)sh->str);
        }
    }
} /* inc_shadow_string_ref() */

#endif /* GC_SUPPORT */

#endif /* CHECK_STRINGS */

/*-------------------------------------------------------------------------*/
void
init_shared_strings (void)

/* Initialize all datastructures and the common strings.
 */

{
    int x;

    base_table = xalloc(sizeof(char *) * HTABLE_SIZE);

    if (!base_table)
        fatal("Out of memory (%lu bytes) for shared string table\n"
             , (unsigned long) sizeof(char*)*HTABLE_SIZE);

    for (x = 0; x < HTABLE_SIZE; x++)
        base_table[x] = NULL;

#ifdef CHECK_STRINGS
    shadow_table = xalloc(sizeof(str_shadow_t *) * HTABLE_SIZE);

    if (!shadow_table)
        fatal("Out of memory (%lu bytes) for shared string shadow table\n"
             , (unsigned long) sizeof(char*)*HTABLE_SIZE);

    for (x = 0; x < HTABLE_SIZE; x++)
        shadow_table[x] = NULL;
#endif

    init_standard_strings();
} /* init_shared_strings() */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

static void checked (char * s, char *str) NORETURN;

static void
checked (char * s, char *str)

/* Print the error message "<s> (<str>)" and abort.
 * For brutal debugging.
 */

{
    fprintf(stderr, "%s (\"%s\")\n", s, str);
    fatal(s);
    /* NOTREACHED */
} /* checked() */

#endif

/*-------------------------------------------------------------------------*/
char *
findstring (const char *s)

/* Find the shared version of string <s> (which might be <s> itself) in
 * the string table and return its pointer (not counted as reference!).
 * Return NULL if not found.
 *
 * If found, also move the string to the head of its chain and leave
 * the hashindex in the global hash_index.
 */

{
    char * curr, *prev;
    int h;
#ifdef CHECK_STRINGS
    str_shadow_t *scurr, *sprev;
#endif


    h = StrHash(s);
    hash_index = h;

    curr = base_table[h];
    prev = NULL;
    num_str_searches++;
#ifdef CHECK_STRINGS
    scurr = shadow_table[h];
    sprev = NULL;
#endif

    while (curr)
    {
#ifdef CHECK_STRINGS
        check_string(curr, scurr);
        last_shadow = scurr;
#endif
        search_len++;
        if (curr == s || (*curr == *s && !strcmp(curr, s)))
        {
            /* found it */
            if (prev)
            {
                /* not at head of list */
                NEXT(prev) = NEXT(curr);
                NEXT(curr) = base_table[h];
                base_table[h] = curr;
#ifdef CHECK_STRINGS
                sprev->next = scurr->next;
                sprev->snext = NEXT(prev);
                scurr->next = shadow_table[h];
                scurr->snext = NEXT(curr);
                shadow_table[h] = scurr;
#endif
            }
#ifdef VERBOSE_CS
            printf("DEBUG: found s %p (%lu, %p) and sh %p (%p, %lu, %p)\n"
                 , curr, (unsigned long)REFS(curr), NEXT(curr)
                 , scurr, scurr->str, (unsigned long)scurr->ref, scurr->snext);
            fflush(stdout);
#endif
            num_found++;
            return curr;
        }
        prev = curr;
        curr = NEXT(curr);
#ifdef CHECK_STRINGS
        sprev = scurr;
        scurr = scurr->next;
#endif
    }
#ifdef CHECK_STRINGS
    check_string(curr, scurr);
#endif

    return NULL;
} /* findstring() */

/*-------------------------------------------------------------------------*/
static INLINE char *
alloc_new_string (const char *str)

/* Enter new string <str> into the hash table at index <str> computed
 * by a previous call to findstring(). Return the pointer to the
 * new shared string with one refcount.
 *
 * Return NULL when out of memory.
 */

{
    size_t length;
    char *s;
    int h;

    h = hash_index;

    /* Get the memory */
    length = strlen(str);
    s = xalloc(1 + length + SHSTR_OVERHEAD);
    if (!s)
        return NULL;
    s += SHSTR_OVERHEAD;

    /* Copy the string and add it to the string table */
    if (base_table[h] == NULL)
        num_entries++;
    else
        num_collisions++;
    num_added++;

    strcpy(s, str);
    NEXT(s) = base_table[h];
    base_table[h] = s;

#ifdef CHECK_STRINGS
    {
        str_shadow_t *sh;

        sh = xalloc(sizeof(*sh));
        if (!sh)
            return NULL;
        sh->str = s;
        sh->ref = 1;
        sh->snext = NEXT(s);
        sh->next = shadow_table[h];
        shadow_table[h] = sh;
        last_shadow = sh;
#ifdef VERBOSE_CS
        printf("DEBUG: new s  %p (r 1, n %p) '%s'\n", s, NEXT(s), s);
        printf("DEBUG:  -> sh %p (s %p, r 1, n %p)\n", sh, sh->str, sh->snext);
        fflush(stdout);
#endif
    }
#endif

    num_distinct_strings++;
    bytes_distinct_strings += (
            (SHSTR_OVERHEAD + length + 1 + (sizeof(char *)-1)))
            & (~(sizeof(char *)-1));

    REFS(s) = 1;

    stralloc_allocd_strings++;
    stralloc_allocd_bytes += shstr_malloced_length(s);
    return s;
} /* alloc_new_string() */

/*-------------------------------------------------------------------------*/
char *
make_shared_string (const char *str)

/* Find or make a shared string for <str> and return its pointer
 * with one (more) refcount.
 *
 * Use this function when <str> may be shared or unshared.
 * If you know that a string is shared, ref_string() is the
 * better and faster function.
 */

{
    char * s;

    s = findstring(str);
    if (!s)
    {
        return alloc_new_string(str);
    }
    else
    {
#ifndef KEEP_STRINGS
        if (REFS(s))
        {
            REFS(s)++;
            if (!REFS(s))
            {
                printf("DEBUG: make_shared_string(): found %p '%s' "
                       "refcount reaches max!\n", s, s);
                fflush(stdout);
            }
        }
        else
        {
            printf("DEBUG: make_shared_string(): found %p '%s' has 0 refs.\n"
                  , s, s);
            fflush(stdout);
        }

#ifdef CHECK_STRINGS
        if (last_shadow->ref)
        {
            last_shadow->ref++;
            if (!last_shadow->ref)
            {
                printf("DEBUG: make_shared_string(): shadow %p: "
                       "refcount reaches max!\n", last_shadow);
                fflush(stdout);
            }
        }
#endif

#else /* KEEP_STRINGS */
        if (REFS(s))
        {
            REFS(s)++;
            if (!REFS(s))
            {
                printf("DEBUG: make_shared_string(): found %p '%s' "
                       "refcount reaches max!\n", s, s);
                fflush(stdout);
            }
        }
        else
        {
            REFS(s)++;
        }

#ifdef CHECK_STRINGS
            last_shadow->ref++;
            if (!last_shadow->ref)
            {
                printf("DEBUG: make_shared_string(): shadow %p: "
                       "refcount reaches max!\n", last_shadow);
                fflush(stdout);
            }
#endif

#endif /* KEEP_STRINGS */

#ifdef VERBOSE_CS
        printf("DEBUG: made old s %p (%lu, %p) and sh %p (%p, %lu, %p)\n"
              , s, (unsigned long)REFS(s), NEXT(s)
              , last_shadow, last_shadow->str, (unsigned long)last_shadow->ref
              , last_shadow->snext);
        fflush(stdout);
#endif
    }
    stralloc_allocd_strings++;
    stralloc_allocd_bytes += shstr_malloced_length(s);
    return(s);
} /* make_shared_string() */

/*-------------------------------------------------------------------------*/
static INLINE void
_deref_string (char *str)

/* Decrement the refcount of shared <str>, but don't check for refcount 0.
 */

{
    stralloc_allocd_strings--;
    stralloc_allocd_bytes -= shstr_malloced_length(str);
    if (REFS(str))
    {
        REFS(str)--;
        if (!REFS(str))
        {
            printf("DEBUG: deref_string(): %p '%s' fell to 0 refs!\n", str, str);
            fflush(stdout);
        }
    }
    else
    {
        printf("DEBUG: deref_string(): %p '%s' has 0 refs.\n", str, str);
        fflush(stdout);
    }

#ifdef CHECK_STRINGS
    {
        str_shadow_t *sh;

        sh = find_shadow(str);
        if (sh->ref)
            sh->ref--;
        check_string(str, sh);
#ifdef VERBOSE_CS
        printf("DEBUG: deref'd s %p (%lu, %p) and sh %p (%p, %lu, %p)\n"
              , str, (unsigned long)REFS(str), NEXT(str)
              , sh, sh->str, (unsigned long)sh->ref, sh->snext);
        fflush(stdout);
#endif
    }
#endif
} /* _deref_string() */

void
deref_string (char *str)
  { _deref_string(str); }

#define deref_string(str) _deref_string(str)

/*-------------------------------------------------------------------------*/
void
free_string (char *str)

/* Decrement the refcount of the shared string <str> and free it
 * if the count reaches zero.
 *
#ifdef DEBUG
 * The function applies various sanity checks.
#endif
 */

{
    char * s;

    stralloc_allocd_strings--;
    stralloc_allocd_bytes -= shstr_malloced_length(str);

#ifdef DEBUG
    if (!REFS(str))
    {
        printf("DEBUG: free_string(): %p '%s' has 0 refs.\n"
              , str, str);
        fflush(stdout);
    }

#endif

#ifdef CHECK_STRINGS
    {
        str_shadow_t *sh;

        sh = find_shadow(str);
        check_string(str, sh);
#ifdef VERBOSE_CS
        printf("DEBUG: free s %p (%lu-1, %p) and sh %p (%p, %lu, %p)\n"
              , str, (unsigned long)REFS(str), NEXT(str)
              , sh, sh->str, (unsigned long)sh->ref, sh->snext);
        fflush(stdout);
#endif
    }
#endif

#ifdef DEBUG
    s = findstring(str); /* moves it to head of table if found */
    if (!s)
    {
        checked("Free string: not found in string table!", str);
        return;
    }

    if (s != str)
    {
        /* Either a duplicate entry in the table or the table
         * pointers are messed up. In any case, something fatal.
         */
        checked("Free string: string didnt hash to the same spot!", str);
        return;
    }

    if (REFS(s) <= 0 && d_flag)
    {
        fprintf(stderr
               , "%s Free String: String refs zero or negative! (\"%s\")\n"
               , time_stamp(), str
               );
    }
#endif

#ifdef CHECK_STRINGS
    {
        if (last_shadow->ref)
            last_shadow->ref--;
    }
#endif

    if (!REFS(str) || --REFS(str) > 0)
        return; /* Don't deallocate yet */

#ifndef DEBUG
    s = findstring(str); /* moves it to head of table if found */
#endif

    num_deleted++;
    if (NEXT(str) == NULL)
        num_entries--;

#ifndef KEEP_STRINGS
    base_table[hash_index] = NEXT(str);
#ifdef CHECK_STRINGS
    shadow_table[hash_index] = last_shadow->next;
    xfree(last_shadow);
#endif

    num_distinct_strings--;
    /* We know how much overhead malloc has */
    bytes_distinct_strings -= (shstr_malloced_length(str) & malloc_size_mask())* sizeof(char *);
    xfree(str - SHSTR_OVERHEAD);
#endif

} /* free_string() */

/*-------------------------------------------------------------------------*/
static INLINE mp_uint
overhead_bytes (void)

/* For add_string_status(): the overhead of the string table.
 */

{
    return (sizeof(char *) * HTABLE_SIZE) +
#ifdef CHECK_STRINGS
      sizeof(str_shadow_t *) * HTABLE_SIZE +
      num_distinct_strings * sizeof(str_shadow_t) +
#endif
      num_distinct_strings * SHSTR_OVERHEAD;
} /* overhead_bytes() */

/*-------------------------------------------------------------------------*/
mp_int
add_string_status (strbuf_t *sbuf, Bool verbose)
{
    mp_uint net_bytes_distinct_strings, net_allocd_bytes;

    if (verbose)
    {
        strbuf_add(sbuf, "\nShared string hash table:\n");
        strbuf_add(sbuf, "-------------------------\t Strings    Bytes\n");
    }

    net_bytes_distinct_strings
      =   (bytes_distinct_strings & (malloc_size_mask() * sizeof (char *)))
        - num_distinct_strings * SHSTR_OVERHEAD;
    strbuf_addf(sbuf, "Strings malloced\t\t%8lu %9lu + %lu overhead\n",
                num_distinct_strings, net_bytes_distinct_strings, overhead_bytes());

    if (verbose)
    {
        mp_uint num_x_searches;

        num_x_searches = num_str_searches ? num_str_searches : 1;
        stralloc_allocd_bytes &= malloc_size_mask();
        net_allocd_bytes =   (stralloc_allocd_bytes * sizeof(char*))
                           - stralloc_allocd_strings * SHSTR_OVERHEAD;
        strbuf_addf(sbuf, "Total asked for\t\t\t%8lu %8lu\n",
                    stralloc_allocd_strings, net_allocd_bytes );
        strbuf_addf(sbuf, "Space actually required/total string bytes %lu%%\n",
                    (net_bytes_distinct_strings + overhead_bytes())*100L /
                            net_allocd_bytes );
        strbuf_addf(sbuf, "Searches: %d - Found: %lu (%.1f%%) - Average search length:%7.3f\n"
                   , num_str_searches
                   , num_found, 100.0 * (float)num_found / (float) num_x_searches
                   , (float)search_len / (float)num_x_searches);
        strbuf_addf(sbuf, "Number of chains: %lu (%.1f%%)\n"
                        , num_entries, 100.0 * (float)num_entries/(float)HTABLE_SIZE
                   );
        strbuf_addf(sbuf, "Strings added: %lu - Deleted: %lu - Coll: %lu (%.1f%% added/%.1f%% chains)\n"
                        , num_added
                        , num_deleted
                        , num_collisions
                        , 100.0 * (float)num_collisions/(float)num_added
                        , 100.0 * (float)num_collisions/(float)num_entries
                   );
    }

    return net_bytes_distinct_strings + overhead_bytes();
} /* add_string_status() */

/*-------------------------------------------------------------------------*/
void
string_dinfo_status (svalue_t *svp, int value)

/* Return the string table information for debug_info(DINFO_DATA, DID_STATUS).
 * <svp> points to the svalue block for the result, this function fills in
 * the spots for the object table.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */

{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    mp_uint net_bytes_distinct_strings, net_allocd_bytes;

    net_bytes_distinct_strings
      =   (bytes_distinct_strings & (malloc_size_mask() * sizeof (char *)))
        - num_distinct_strings * SHSTR_OVERHEAD;

    stralloc_allocd_bytes &= malloc_size_mask();
    net_allocd_bytes = (stralloc_allocd_bytes * sizeof(char*))
                       - stralloc_allocd_strings * SHSTR_OVERHEAD;

    ST_NUMBER(DID_ST_STRINGS, num_distinct_strings);
    ST_NUMBER(DID_ST_STRING_SIZE, net_bytes_distinct_strings);
    ST_NUMBER(DID_ST_STR_TABLE_SIZE, overhead_bytes());

    stralloc_allocd_bytes &= malloc_size_mask();
    ST_NUMBER(DID_ST_STR_REQ, stralloc_allocd_strings);
    ST_NUMBER(DID_ST_STR_REQ_SIZE, net_allocd_bytes);

    ST_NUMBER(DID_ST_STR_SEARCHES, num_str_searches);
    ST_NUMBER(DID_ST_STR_SEARCH_LEN, search_len);
    ST_NUMBER(DID_ST_STR_FOUND, num_found);

    ST_NUMBER(DID_ST_STR_ENTRIES, num_entries);
    ST_NUMBER(DID_ST_STR_ADDED, num_added);
    ST_NUMBER(DID_ST_STR_DELETED, num_deleted);
    ST_NUMBER(DID_ST_STR_COLLISIONS, num_collisions);

#undef ST_NUMBER
} /* string_dinfo_status() */

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
clear_shared_string_refs (void)

/* GC support: clear all refs of memory in the string table.
 */

{
    int x;
    char *p;

    for (x=0; x<HTABLE_SIZE; x++)
        for (p = base_table[x]; p; p = NEXT(p) )
            REFS(p) = 0;

#ifdef CHECK_STRINGS
    for (x = 0; x<HTABLE_SIZE; x++)
    {
        str_shadow_t *sh;
        for (sh = shadow_table[x]; sh; sh = sh->next )
            sh->ref = 0;
    }
#endif
} /* clear_shared_string_refs() */

/*-------------------------------------------------------------------------*/
void
note_shared_string_table_ref (void)

/* GC support: count all refs of memory in the string table.
 */

{
    int i;

    note_malloced_block_ref(base_table);
    for (i = 0; i < SHSTR_NOSTRINGS; i++)
        count_ref_from_string(shstring[i]);

#ifdef CHECK_STRINGS
    note_malloced_block_ref(shadow_table);
    for (i = 0; i < HTABLE_SIZE; i++)
    {
        str_shadow_t *sh;
        for (sh = shadow_table[i]; sh; sh = sh->next )
            note_malloced_block_ref(sh);
    }
#endif
} /* note_shared_string_refs() */

/*-------------------------------------------------------------------------*/
void
walk_shared_strings (void (*func) (char *, char *))

/* GC support: Call (*func)(str-SHSTR_OVERHEAD, str) for all shared
 * strings in the string table.
 *
 * Usually the function is "remove_unreferenced_strings()" which removes
 * unref'd strings from the table.
 */

{
    int x;
    char *p, *n;

    for (x = 0; x < HTABLE_SIZE; x++)
        for (n = base_table[x]; NULL != (p = n); )
        {
            n = NEXT(p); /* p may be freed by (*func)() . */
            (*func)(p-SHSTR_OVERHEAD, p);
        }
} /* walk_shared_strings() */

#endif /* GC_SUPPORT */

/***************************************************************************/

