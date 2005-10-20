--- Into hash.c: ---
Modify washstr() to ignore \0 and instead just go for the size.
--- Into gcollect.c: ---
Adapt the string handling.
Adapt the memblock interpretation in MALLOC_TRACE.
New fun: count_ref_from_mstring(string_t *s);
New fun: remove_unreferenced_mstring(string_t *s);
--- Into stdstrings.c: ---
Adapt it.
--- Other ---
CHECKSTRINGS, KEEPSTRINGS gone
Adapt debug_info() and includefile
--- ---

/*---------------------------------------------------------------------------
 * String Management
 *
 *---------------------------------------------------------------------------
 * To reduce the memory used for string storage, the driver implements 
 * string sharing: for every string the driver keeps track in a refcount
 * how many users it has. If the refcount falls back to 0, the string can
 * be safely deallocated again. On the other hand, if a refcount overflows
 * to 0, the string is considered a constant.
 *
 * To reduce memory usage even further, most strings are held in a
 * table. So, when the creation of a new string is requested, the driver
 * first looks in the table for an already existing instance, before it
 * allocates a new string instance. The table is organized as a hash table
 * with HTABLE_SIZE entries.
 *
 * Strings are sequences of chars, stored in an array of known size. The
 * size itself is stored separately, allowing the string to contain every
 * possible character. CAVEAT: In contrast to C, these strings are not
 * terminated by '\0'!
 *
 * Strings are managed using two structures: string_data_t and string_t.
 *
 * string_data_t holds the raw string data as explained above:
 *
 *    struct string_data_s
 *    {
 *        size_t size;            Length of the string
 *        char   txt[1.. .size]; 
 *    }
 *
 * This structure is hardly ever used by other parts of the driver
 * directly; the 'string' datatype is instead represented by
 * the string_t structure:
 *
 *     struct string_s
 *     {
 *         string_t *link;
 *         struct {
 *             Bool tabled       :  1;
 *             unsigned long ref : 31;
 *         } info;
 *         string_data_t *str;
 *     };
 *
 * This string_t value is the one referenced by svalues and the like.
 * It allows the following string types:
 *
 * Untabled, freely allocated strings:
 *   .tabled is FALSE, .link is NULL.
 *
 * Tabled (shared) strings:
 *   .tabled is TRUE, .link is used as link pointer in the hash table.
 *   This .link does not count as reference.
 *
 * Indirectly tabled strings:
 *   .tabled is FALSE, .link points to the corresponding string_t
 *   structure in the hash table (this counts as one reference),
 *   and .str = .link->str.
 * TODO: Are these needed?
 *
 * In all cases .str points to the string_data_t for this string.
 *
 *
 * The distinction between string_data_t and string_t exists for the
 * following reasons:
 *
 *  - svalues do not need to know how the string is implemented.
 *  - string_t's can take advantage of smallocs fast small block
 *    allocator.
 *  - untabled strings can easily be made (indirectly) tabled even
 *    if they have many active refs pending.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stdio.h>

#include "mstrings.h"

#include "gcollect.h"
#include "hash.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/

/* Hash function, adapted to our table size.
 */

extern int wlhashstr(const char * const,  size_t, size_t);
#if !( (HTABLE_SIZE) & (HTABLE_SIZE)-1 )
#    define StrHash(s,siz) (wlhashstr((s), 100, siz) & ((HTABLE_SIZE)-1))
#else
#    define StrHash(s,siz) (wlhashstr((s), 100, siz) % HTABLE_SIZE)
#endif

/*-------------------------------------------------------------------------*/

static string_t ** stringtable = NULL;
  /* The hashed string table: an array of pointers to the heads of
   * the string chains.
   */

/* Statistics */

       mp_uint mstr_used = 0;
  /* Number of virtually allocated strings - every reference counts
   * as separate copy.
   */
   
       mp_uint mstr_used_size = 0;
  /* Total virtual size of allocated strings counted 
   * - every reference counts as separate copy.
   * This does include the memory by the string management structures.
   */

static mp_uint mstr_tabled = 0;
  /* Number of distinct strings in the string table.
   */
   
static mp_uint mstr_tabled_size = 0;
  /* Total memory held in the string table.
   */

static mp_uint mstr_itabled = 0;
  /* Number of indirectly tabled strings.
   */
   
static mp_uint mstr_itabled_size = 0;
  /* Amount of memory held in itabled strings, of which only
   * <mstr_itabled> * sizeof(string_t) is not shared with the string
   * table.
   */

static mp_uint mstr_untabled = 0;
  /* Number of distinct untabled strings.
   */
   
static mp_uint mstr_untabled_size = 0;
  /* Total memory held in untabled strings.
   */

static mp_uint mstr_searchlen_byvalue = 0;
  /* Number of search steps along hash chains with content comparisons.
   */

static mp_uint mstr_searches_byvalue = 0;
  /* Number of searches in the string table with content comparison.
   */

static mp_uint mstr_searchlen = 0;
  /* Number of search steps along hash chains without content comparisons.
   */

static mp_uint mstr_searches = 0;
  /* Number of searches in the string table without content comparisons.
   */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
find_and_move (const char * const s, size_t size, int index)

/* If <s> is a tabled string of length <size> in the stringtable[<index>]
 * chain: find it, move it to the head of the chain and return its string_t*.
 *
 * If <s> is not tabled, return NULL.
 */

{
    string_t *prev, *rover;

    mstr_searches_byvalue++;

    /* Find the string in the table */

    for ( prev = NULL, rover = stringtable[index]
        ;    rover != NULL
          && get_cstr(rover) != s && memcmp(get_cstr(rover), s, size)
        ; prev = rover, rover = rover->link
        )
        mstr_searchlen_byvalue++;

    /* If the string is in the table (rover != NULL), but not at the beginning
     * of the chain, move it there.
     */
    if (rover && prev)
    {
        prev->link = rover->link;
        rover->link = prev;
        stringtable[index] = rover;
    }

    return rover;
} /* find_and_move() */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
move_to_head (string_t *s, int index)

/* If <s> is a tabled string in the stringtable[<index>] chain: move it to
 * the head of the chain and return its pointer.
 * If <s> is not found in that chain, return NULL.
 */

{
    string_t *prev, *rover;

    mstr_searches++;

    /* Find the string in the table */

    for ( prev = NULL, rover = stringtable[index]
        ; rover != NULL && rover != s
        ; prev = rover, rover = rover->link
        )
        mstr_searchlen++;

    /* If s is found (rover != NULL), but not at the beginning of the chain,
     * move it there
     */

    if (rover && prev)

    {
        prev->link = s->link;
        s->link = prev;
        stringtable[index] = s;
    }

    return rover;
} /* move_to_head() */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
make_new_tabled (const char * const pTxt, size_t size, int index)

/* Helper function for mstring_new_tabled(), mstring_make_tabled() and
 * mstring_table_inplace().
 *
 * Create a new tabled string by copying the data string <pTxt> of length
 * <size> and return it counting the result as one reference. The string
 * MUST NOT yet exist in the table, nor are the statistics updated.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_data_t *sdata;
    string_t      *string;

    /* Get the memory for a new one */

    sdata = xalloc(size + sizeof(*sdata) - 1);
    if (!sdata)
        return NULL;

    string = xalloc(sizeof(*string));
    if (!string)
    {
        xfree(sdata);
        return NULL;
    }

    /* Set up the structures and table the string */

    sdata->size = size;
    memcpy(sdata->txt, pTxt, size);

    string->str = sdata;
    string->info.tabled = MY_TRUE;
    string->info.ref = 1;

    string->link = stringtable[index];
    stringtable[index] = string;

    {
        size_t msize;

        msize = mstr_mem_size(string);
        mstr_used++;
        mstr_used_size += msize;
        mstr_tabled++;
        mstr_tabled_size += msize;
    }

    return string;
} /* make_new_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_alloc_string (size_t iSize)

/* Aliased to: alloc_mstring(iSize)
 * Also called by mstring_new_string().
 *
 * Create a new untabled string with space for <iSize> characters and
 * return it, counting the result as one reference.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_data_t *sdata;
    string_t      *string;

    /* Get the memory */

    sdata = xalloc(iSize + sizeof(*sdata) - 1);
    if (!sdata)
        return NULL;

    string = xalloc(sizeof(*string));
    if (!string)
    {
        xfree(sdata);
        return NULL;
    }

    /* Set up the structures */
    sdata->size = iSize;
    string->link = NULL;
    string->str = sdata;
    string->info.tabled = MY_FALSE;
    string->info.ref = 1;

    {
        size_t msize;

        msize = mstr_mem_size(string);
        mstr_used++;
        mstr_used_size += msize;
        mstr_untabled++;
        mstr_untabled_size += msize;
    }

    return string;
} /* mstring_alloc_string() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_new_string (const char * const pTxt)

/* Aliased to: new_mstring(pTxt)
 *
 * Create a new untabled string by copying the C string <pTxt> and
 * return it, counting the result as one reference.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_t      *string;
    size_t         size;

    size = strlen(pTxt);

    string = mstring_alloc_string(size);
    if (string && size)
    {
        memcpy(string->str->txt, pTxt, size);
    }

    return string;
} /* mstring_new_string() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_new_tabled (const char * const pTxt)

/* Aliased to: new_tabled(pTxt)
 *
 * Create a new tabled string by copying the C string <pTxt> and
 * return it counting the result as one reference. If a tabled string
 * for the same <pTxt> already exists, a reference to that one is returned.
 *
 * If memory runs out, NULL is returned.
 */

{
    int        index;
    size_t     size;
    string_t * string;

    size = strlen(pTxt);
    index = StrHash(pTxt, size);

    /* Check if the string has already been tabled */
    string = find_and_move(pTxt, size, index);
    if (string)
    {
        return ref_mstring(string);
    }

    /* No: create a new one */
    return make_new_tabled(pTxt, size, index);
} /* mstring_new_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_make_tabled (string_t * pStr)

/* Aliased to: make_tabled(pStr)
 *
 * Take the string <pStr> and create resp. find a tabled instance of it.
 * Return the counted reference to the tabled instance, and dereference
 * the <pStr> once.
 *
 * If memory runs out, NULL is returned.
 */

{
    int            index;
    size_t         size;
    string_t      *string;

    /* If the string is already tabled directly, our work is done */
    if (pStr->info.tabled)
        return pStr;

    /* If the string is tabled indirectly, return the directly tabled
     * instance.
     */
    if (pStr->link != NULL)
    {
        string = ref_mstring(pStr->link); /* Must come first! */
        free_mstring(pStr);
        return string;
    }

    /* Create a completely new tabled string from the old one */

    size = pStr->str->size;
    index = StrHash(pStr->str->txt, size);

    if (pStr->info.ref == 1)
    {
        /* We can simply reuse the string_t we already have */

        string = pStr;
        string->info.tabled = MY_TRUE;
        string->link = stringtable[index];
        stringtable[index] = string;
    }
    else
    {
        /* We need a completely new table string */

        string = make_new_tabled(pStr->str->txt, size, index);
        free_mstring(pStr);
    }

    return string;
} /* mstring_make_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_table_inplace (string_t * pStr)

/* Aliased to: table_inplace(pStr)
 *
 * If <pStr> is an untabled string, it is morphed into an indirectly tabled
 * one.
 *
 * Result is <pStr>, the refcount doesn't change.
 * If memory runs out, NULL is returned.
 */

{
    string_t *string;
    int        index;
    size_t     size;
    size_t     msize;

    /* If the string is already tabled, our work is done */
    if (pStr->info.tabled || pStr->link != NULL)
        return pStr;

    msize = mstr_mem_size(pStr);

    /* Get or create the tabled string for this untabled one */

    size = pStr->str->size;
    index = StrHash(pStr->str->txt, size);

    /* Check if the string has already been tabled */
    string = find_and_move(pStr->str->txt, size, index);

    if (!string)
    {
        /* No: create a new string structure, table it,
         * and then give it pStr's string_data pointer.
         */
        string = xalloc(sizeof(*string));
        if (!string)
        {
            return NULL;
        }

        string->str = pStr->str;
        string->info.tabled = MY_TRUE;
        string->info.ref = 1;

        string->link = stringtable[index];
        stringtable[index] = string;

        mstr_used++;
        mstr_used_size += msize;
        mstr_tabled++;
        mstr_tabled_size += msize;
    }
    else
    {
        /* Yes: increment the refcount of the found string
         * and get rid of pStr's string_data.
         */
        ref_mstring(string);
        xfree(pStr->str);
        pStr->str = string->str;
    }

    /* string is now the directly tabled string, and both string
     * and pStr share the same string_data structure.
     *
     * Complete the morphing of pStr into an indirectly tabled
     * string.
     */

    pStr->link = string;

    mstr_itabled++;
    mstr_itabled_size += msize;
    mstr_untabled--;
    mstr_untabled_size -= msize;

    /* That's all */

    return pStr;
} /* mstring_table_inplace() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_dup (string_t * pStr)

/* Aliased to: dup_mstring(pStr)
 *
 * If <pStr> is a tabled string, an untabled string with more than one
 * reference, create and return a new untabled string
 * with the same text but just one reference, and remove one reference from
 * <pStr>. Otherwise, just return <pStr>.
 * If memory runs out, NULL is returned.
 *
 * Purpose is to create an instance of a string which an be freely modified.
 */

{
    string_t *string;

    /* Check for the easy case */
    if (!pStr->info.tabled && pStr->info.ref == 1 && pStr->link == NULL)
        return pStr;

    /* Otherwise create a new untabled string from the tabled one */

    string = mstring_alloc_string(pStr->str->size);
    if (string)
    {
        memcpy(string->str->txt,  pStr->str->txt, pStr->str->size);
    }

    free_mstring(pStr);

    return string;
} /* mstring_dup() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_find_tabled (const string_t * pStr)

/* Aliased to: find_tabled(pStr)
 *
 * Find the tabled string with the same content as <pStr> and return it.
 * If <pStr> is a tabled string, it will be the result itself.
 * If there is no such tabled string, NULL is returned.
 *
 * The function does not change refcounts.
 */

{
    string_t *string;
    int       index;
    size_t    size;

    /* If pStr is tabled, our work is done */
    if (pStr->info.tabled)
        return (string_t *)pStr;

    /* If pStr is indirectly tabled, return the tabled instance */
    if (pStr->link != NULL)
        return pStr->link;

    /* Worst case: an untabled string we have to look for */

    size = mstrsize(pStr);
    index = StrHash(pStr->str->txt, size);

    return find_and_move(pStr->str->txt, size, index);
} /* mstring_find_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_find_tabled_str (const char * const pTxt)

/* Aliased to: find_tabled_str(pTxt)
 *
 * Find the tabled string with the same content as the C string <pTxt> and
 * return it.
 * If there is no such tabled string, NULL is returned.
 *
 * The function does not change refcounts.
 */

{
    string_t *string;
    int       index;
    size_t    size;

    size = strlen(pTxt);
    index = StrHash(pTxt, size);

    return find_and_move(pTxt, size, index);
} /* mstring_find_tabled_str() */

/*-------------------------------------------------------------------------*/
void
mstring_free (string_t *s)

/* Decrement the refcount of string <s>. If it reaches 0, deallocate it
 * altogether.
 */

{
    int    index;
    size_t msize;

    if (!s->info.ref)
        return;

    msize = mstr_mem_size(s);

    if (--(s->info.ref))
    {
        mstr_used--;
        mstr_used_size -=  sizeof(*s) + sizeof(*(s->str))
                         + s->str->size - 1;
        return;
    }

    /* String has no refs left - deallocate it */

    if (s->info.tabled)
    {
        /* A tabled string */

        int index;

        mstr_tabled--;
        mstr_tabled_size -= msize;

        index = StrHash(s->str->txt, mstrsize(s));
        if (NULL == move_to_head(s, index))
        {
            fatal("String %p (%s) doesn't hash to the same spot.\n"
                 , s, s->str ? s->str->txt : "<null>"
                 );
        }
        stringtable[index] = s->link;
        xfree(s->str);
        xfree(s);
    }
    else if (s->link == NULL)
    {
        /* A free string */

        mstr_untabled--;
        mstr_untabled_size -= msize;

        xfree(s->str);
        xfree(s);
    }
    else
    {
        /* An indirectly tabled string */

        mstr_itabled--;
        mstr_itabled_size -= msize;

        free_mstring(s->link);
        free(s);
    }
} /* mstring_free() */

/*-------------------------------------------------------------------------*/
void
mstring_init (void)

/* Initialize all datastructures and the common strings.
 */

{
    int x;

    stringtable = xalloc(sizeof(char *) * HTABLE_SIZE);

    if (!stringtable)
        fatal("(mstring_init) Out of memory (%lu bytes) for string table\n"
             , (unsigned long) sizeof(char*)*HTABLE_SIZE);

    for (x = 0; x < HTABLE_SIZE; x++)
        stringtable[x] = NULL;

    init_standard_strings();
} /* mstring_init() */

/*=========================================================================*/

#ifdef GC_SUPPORT

void
mstring_clear_refs (void)

/* GC support: clear all refs of memory in the string table.
 */

{
    int x;
    string_t *p;

    for (x = 0; x < HTABLE_SIZE; x++)
        for (p = stringtable[x]; p; p = p->link )
            p->info.ref = 0;

} /* mstring_clear_refs() */

/*-------------------------------------------------------------------------*/
void
mstring_note_refs (void)

/* GC support: note all refs of memory in the string table.
 */

{
    int i;

    note_malloced_block_ref(stringtable);
    for (i = 0; i < SHSTR_NOSTRINGS; i++)
        count_ref_from_mstring(shstring[i]);
} /* mstring_note_refs() */

/*-------------------------------------------------------------------------*/
void
mstring_walk_strings (void (*func) (string_t *))

/* GC support: Call (*func)(str) for all tabled strings in the string table.
 *
 * Usually the function is "remove_unreferenced_strings()" which removes
 * unref'd strings from the table.
 */

{
    int x;
    string_t *p, *n;

    for (x = 0; x < HTABLE_SIZE; x++)
        for (n = stringtable[x]; NULL != (p = n); )
        {
            n = p->link; /* p may be freed by (*func)() . */
            (*func)(p);
        }
} /* mstring_walk_strings() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
mp_int
add_string_status (strbuf_t *sbuf, Bool verbose)

/* Add the string handler status suitable for printing to <sbuf>.
 * Result is the amount of memory held by the string handler.
 */

{
#   define STR_OVERHEAD (sizeof(string_t)+sizeof(string_data_t)-1)

    mp_uint stringtable_size;
    mp_uint distinct_strings;
    mp_uint distinct_size;
    mp_uint distinct_overhead;

    stringtable_size = HTABLE_SIZE * sizeof(string_t *);
    distinct_strings = mstr_tabled + mstr_itabled + mstr_untabled;
    distinct_size = mstr_tabled_size + mstr_untabled_size
                  + mstr_itabled * sizeof(string_t);
    distinct_overhead = mstr_tabled * STR_OVERHEAD
                      + mstr_untabled * STR_OVERHEAD
                      + mstr_itabled * sizeof(string_t);

    if (!verbose)
    {
        strbuf_addf("Strings alloced\t\t%8lu %8lu + %lu overhead\n"
                   , distinct_strings, distinct_size - distinct_overhead
                   , distinct_overhead + stringtable_size
                   );
    }
    else
    {
        strbuf_add(sbuf, "\nString handler:\n");
        strbuf_add(sbuf,   "---------------\t Strings    Bytes (Data+Overhead)\n");
        strbuf_addf(sbuf,  "Total asked for\t%8lu %8lu (%8lu+%8lu)\n"
                        , mstr_used
                        , mstr_used_size
                        , mstr_used_size - mstr_used * STR_OVERHEAD
                        , mstr_used * STR_OVERHEAD
                        );
        strbuf_addf(sbuf,  "Total allocated\t%8lu %8lu (%8lu+%8lu)\n"
                        , distinct_strings
                        , distinct_size + stringtable_size
                        , distinct_size - distinct_overhead
                        , distinct_overhead + stringtable_size
                        );
        strbuf_addf(sbuf,  " - tabled\t%8lu %8lu (%8lu+%8lu)\n"
                        , mstr_tabled
                        , mstr_tabled_size + stringtable_size
                        , mstr_tabled_size - mstr_tabled * STR_OVERHEAD
                        , mstr_tabled * STR_OVERHEAD + stringtable_size
                        );
        strbuf_addf(sbuf,  " - ind. tabled\t%8lu %8lu (%8lu+%8lu)\n"
                        , mstr_itabled
                        , mstr_itabled_size
                        , mstr_itabled_size - mstr_tabled * STR_OVERHEAD
                        , mstr_itabled * sizeof(string_t)
                        );
        strbuf_addf(sbuf,  " - untabled\t%8lu %8lu (%8lu+%8lu)\n"
                        , mstr_untabled
                        , mstr_untabled_size
                        , mstr_untabled_size - mstr_untabled * STR_OVERHEAD
                        , mstr_untabled * STR_OVERHEAD
                        );
        strbuf_addf(sbuf, "\nSpace required / total string bytes: %lu%%\n"
                        , ((distinct_size + stringtable_size) * 100L) 
                          / (mstr_used_size - mstr_used * STR_OVERHEAD)
                        );
        strbuf_addf(sbuf, "Searches by address: %lu - average length: %7.3f\n"
                        , mstr_searches
                        , (float)mstr_searchlen / (float)mstr_searches
                        );
        strbuf_addf(sbuf, "Searches by content: %lu - average length: %7.3f\n"
                        , mstr_searches_byvalue
                        , (float)mstr_searchlen_byvalue / (float)mstr_searches_byvalue
                        );
    }

    return stringtable_size + distinct_size;
#   undef STR_OVERHEAD
} /* add_string_status() */

/*-------------------------------------------------------------------------*/
void
string_dinfo_status (svalue_t *svp)

/* Return the string table information for debug_info(DINFO_DATA, DID_STATUS).
 * <svp> points to the svalue block for the result, this function fills in
 * the spots for the object table.
 */

{
    svp[DID_ST_STRINGS].u.number = mstr_used;
    svp[DID_ST_STRING_SIZE].u.number = mstr_used_size;

    svp[DID_ST_STR_TABLE_SIZE].u.number = HTABLE_SIZE * sizeof(string_t *);
    svp[DID_ST_STR_OVERHEAD].u.number = sizeof(string_t)+sizeof(string_data_t)-1;
    svp[DID_ST_STR_IT_OVERHEAD].u.number = sizeof(string_t);
    
    svp[DID_ST_UNTABLED].u.number      = mstr_untabled;
    svp[DID_ST_UNTABLED_SIZE].u.number = mstr_untabled_size;
    svp[DID_ST_ITABLED].u.number       = mstr_tabled;
    svp[DID_ST_ITABLED_SIZE].u.number  = mstr_tabled_size;
    svp[DID_ST_TABLED].u.number        = mstr_tabled;
    svp[DID_ST_TABLED_SIZE].u.number  = mstr_tabled_size;

    svp[DID_ST_STR_SEARCHES].u.number          = mstr_searches;
    svp[DID_ST_STR_SEARCHLEN].u.number         = mstr_searchlen;
    svp[DID_ST_STR_SEARCHES_BYVALUE].u.number  = mstr_searches_byvalue;
    svp[DID_ST_STR_SEARCHLEN_BYVALUE].u.number = mstr_searchlen_byvalue;
} /* string_dinfo_status() */

/***************************************************************************/
