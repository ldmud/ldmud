/*---------------------------------------------------------------------------
 * String Management
 *
 *---------------------------------------------------------------------------
 * TODO:: Optimize for short strings to reduce overhead?
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
 * possible character. Internally the module appends a '\0' character
 * to the string data to make it somewhat compatible with C system
 * functions; however, this character itself is not counted in the size
 * of the string, and the module itself doesn't rely on it.
 *
 * Strings are managed using two structures: string_data_t and string_t.
 *
 * string_data_t holds the raw string data as explained above:
 *
 *    struct string_data_s
 *    {
 *        size_t  size;            Length of the string
 *        whash_t hash;            0, or the hash of the string
 *        char    txt[1.. .size];
 *        char    null             Gratuituous terminator
 *    }
 *
 * The hash of the string is computed on-demand. Should the string hash
 * to value 0, the value 0x8000 is used instead - this way the usual
 * calculation (hash % tablesize) won't be affected.
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
 *    if they have many active refs pending and/or are of large size.
 * TODO: Make functions mstr_append() resilient to receiving NULL
 * TODO:: pointers as args. This way stuff like rc =
 * TODO:: mstr_append(rc,...) will always work and we need to check
 * TODO:: for rc != NULL only at the end.
 * TODO: Distinguish between the allocated size of a string and the
 * TODO:: used size. To use this efficiently, functions like mstr_insert()...
 * TODO:: might become necessary.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stdio.h>

#include "mstrings.h"

#include "gcollect.h"
#include "hash.h"
#include "main.h"
#include "simulate.h"
#include "stdstrings.h"
#include "strfuns.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"

/*-------------------------------------------------------------------------*/

/* Adapt a hash value to our table size.
 */

#if !( (HTABLE_SIZE) & (HTABLE_SIZE)-1 )
#    define HashToIndex(h) ((h) & ((HTABLE_SIZE)-1))
#else
#    define HashToIndex(h) ((h) % HTABLE_SIZE)
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

static mp_uint mstr_chains = 0;
  /* Number of hash chains in the string table.
   */

static mp_uint mstr_added = 0;
  /* Number of distinct strings added to the string table.
   */

static mp_uint mstr_deleted = 0;
  /* Number of distinct strings delete from the string table.
   */

static mp_uint mstr_collisions = 0;
  /* Number of collisions when adding a new distinct string.
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

static mp_uint mstr_found_byvalue = 0;
  /* Number of successful searches in the string table with content comparison.
   */

static mp_uint mstr_searchlen = 0;
  /* Number of search steps along hash chains without content comparisons.
   */

static mp_uint mstr_searches = 0;
  /* Number of searches in the string table without content comparisons.
   */

static mp_uint mstr_found = 0;
  /* Number of successful searches in the string table with content comparison.
   */

/*-------------------------------------------------------------------------*/
static INLINE whash_t
hash_string (const char * const s, size_t size)

/* Compute the hash for string <s> of length <size> and return it.
 * The result will always be non-zero.
 */

{
    whash_t hash;

    hash = whashmem(s, size, 256);
    if (!hash)
        hash = 1 << (sizeof (hash) * CHAR_BIT - 1);
    return hash;
} /* hash_string() */

/*-------------------------------------------------------------------------*/
static INLINE whash_t
get_hash (const string_t * pStr)

/* Return the hash of string <pStr>, computing it if necessary.
 */

{
    string_data_t *sdata = pStr->str;

    if (!sdata->hash)
        sdata->hash = hash_string(sdata->txt, sdata->size);

    return sdata->hash;
} /* get_hash() */

/*-------------------------------------------------------------------------*/
whash_t
mstring_get_hash (const string_t * pStr)

/* Aliased to: mstr_get_hash()
 *
 * Return the hash value of <pStr>, computing it if necessary.
 */

{
    return get_hash(pStr);
} /* mstring_get_hash() */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
find_and_move (const char * const s, size_t size, whash_t hash)

/* If <s> is a tabled string of length <size> and <hash> in the related
 * stringtable chain: find it, move it to the head of the chain and return its
 * string_t*.
 *
 * If <s> is not tabled, return NULL.
 */

{
    string_t *prev, *rover;

    int idx = HashToIndex(hash);

    mstr_searches_byvalue++;

    /* Find the string in the table */

    for ( prev = NULL, rover = stringtable[idx]
        ;    rover != NULL
          && get_txt(rover) != s
          && !(   size == mstrsize(rover)
               && hash == get_hash(rover)
               && 0 == memcmp(get_txt(rover), s, size)
              )
        ; prev = rover, rover = rover->link
        )
        mstr_searchlen_byvalue++;

    /* If the string is in the table (rover != NULL), but not at the beginning
     * of the chain, move it there.
     */
    if (rover && prev)
    {
        prev->link = rover->link;
        rover->link = stringtable[idx];
        stringtable[idx] = rover;
    }

    if (rover)
        mstr_found_byvalue++;

    return rover;
} /* find_and_move() */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
move_to_head (string_t *s, int idx)

/* If <s> is a tabled string in the stringtable[<index>] chain: move it to
 * the head of the chain and return its pointer.
 * If <s> is not found in that chain, return NULL.
 */

{
    string_t *prev, *rover;

    mstr_searches++;

    /* Find the string in the table */

    for ( prev = NULL, rover = stringtable[idx]
        ; rover != NULL && rover != s
        ; prev = rover, rover = rover->link
        )
    {
        mstr_searchlen++;
    }

    /* If s is found (rover != NULL), but not at the beginning of the chain,
     * move it there
     */

    if (rover && prev)
    {
        prev->link = s->link;
        s->link = stringtable[idx];
        stringtable[idx] = s;
    }

    if (rover)
        mstr_found++;

    return rover;
} /* move_to_head() */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
make_new_tabled (const char * const pTxt, size_t size, whash_t hash MTRACE_DECL)

/* Helper function for mstring_new_tabled() and mstring_new_n_tabled().
 *
 * Create a new tabled string by copying the data string <pTxt> of length
 * <size> and <hash> and return it counting the result as one reference. The
 * string MUST NOT yet exist in the table.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_data_t *sdata;
    string_t      *string;
    int            idx = HashToIndex(hash);

    /* Get the memory for a new one */

    sdata = xalloc_pass(size + sizeof(*sdata));
      /* sizeof(*sdata) includes the extra data byte */
    if (!sdata)
        return NULL;

    string = xalloc_pass(sizeof(*string));
    if (!string)
    {
        xfree(sdata);
        return NULL;
    }

    /* Set up the structures and table the string */

    sdata->size = size;
    sdata->hash = hash;
    memcpy(sdata->txt, pTxt, size);
    sdata->txt[size] = '\0';

    string->str = sdata;
    string->info.tabled = MY_TRUE;
    string->info.ref = 1;
      /* An uninitialized memory read at this point is ok: it's because
       * the bitfield is initialized in parts.
       */

    mstr_added++;
    if (NULL == stringtable[idx])
        mstr_chains++;
    else
        mstr_collisions++;

    string->link = stringtable[idx];
    stringtable[idx] = string;

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
mstring_alloc_string (size_t iSize MTRACE_DECL)

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

    sdata = xalloc_pass(iSize + sizeof(*sdata));
    if (!sdata)
        return NULL;

    string = xalloc_pass(sizeof(*string));
    if (!string)
    {
        xfree(sdata);
        return NULL;
    }

    /* Set up the structures */
    sdata->size = iSize;
    sdata->hash = 0;
    sdata->txt[iSize] = '\0';
    string->link = NULL;
    string->str = sdata;
    string->info.tabled = MY_FALSE;
    string->info.ref = 1;
      /* An uninitialized memory read at this point is ok: it's because
       * the bitfield is initialized in parts.
       */

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
mstring_realloc_string (string_t *string, size_t iSize MTRACE_DECL)

/* Aliased to: realloc_mstring(string, iSize)
 *
 * Change the space of the <string> to <iSize> characters, keeping
 * the old content as far as possible. Return the pointer to the
 * <string> on success, or NULL if memory runs out.
 *
 * The string must not be tabled.
 */

{
    string_data_t *sdata;
    size_t         old_msize;

    if (string->info.tabled)
    {
        fatal("mstring_realloc_string: String %p (%s) must not be tabled.\n"
             , string, string->str ? string->str->txt : "<null>"
             );
    }

    old_msize = mstr_mem_size(string);

    /* Get the memory */

    if (string->str && !string->link)
        sdata = rexalloc_pass(string->str,  iSize + sizeof(*sdata));
    else
    {
        sdata = xalloc_pass(iSize + sizeof(*sdata));
        
        /* If it's an indirectly tabled string, copy the data over so
         * as not to destroy the data of the directly tabled instance.
         */
        if (string->str && string->link)
        {
            memcpy(sdata, string->str, sizeof(*sdata) + string->str->size);

            mstr_untabled++;
            mstr_untabled_size += old_msize;
            mstr_itabled--;
            mstr_itabled_size -= old_msize;
        }
    }
    if (!sdata)
        return NULL;

    /* Set up the structure */
    sdata->size = iSize;
    sdata->hash = 0;
    sdata->txt[iSize] = '\0';

    string->str = sdata;

    /* If it was an indirectly tabled string, remove the link. */

    if (string->link)
    {
        free_mstring(string->link);
        string->link = NULL;
    }

    {
        size_t msize;

        msize = mstr_mem_size(string);
        mstr_used_size += msize - old_msize;
        mstr_untabled_size += msize - old_msize;
    }

    return string;
} /* mstring_realloc_string() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_new_string (const char * const pTxt MTRACE_DECL)

/* Aliased to: new_mstring(pTxt)
 *
 * Create a new untabled string by copying the C string <pTxt> and
 * return it, counting the result as one reference.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_t *string;
    size_t    size;

    size = strlen(pTxt);

    string = mstring_alloc_string(size MTRACE_PASS);
    if (string && size)
    {
        memcpy(string->str->txt, pTxt, size);
    }

    return string;
} /* mstring_new_string() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_new_n_string (const char * const pTxt, size_t len MTRACE_DECL)

/* Aliased to: new_n_mstring(pTxt, len)
 *
 * Create a new untabled string by copying the <len> characters at <pTxt>
 * and return it, counting the result as one reference.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_t *string;

    string = mstring_alloc_string(len MTRACE_PASS);
    if (string && len)
    {
        memcpy(string->str->txt, pTxt, len);
    }

    return string;
} /* mstring_new_n_string() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_new_tabled (const char * const pTxt MTRACE_DECL)

/* Aliased to: new_tabled(pTxt)
 *
 * Create a new tabled string by copying the C string <pTxt> and
 * return it counting the result as one reference. If a tabled string
 * for the same <pTxt> already exists, a reference to that one is returned.
 *
 * If memory runs out, NULL is returned.
 */

{
    whash_t    hash;
    size_t     size;
    string_t * string;

    size = strlen(pTxt);
    hash = hash_string(pTxt, size);

    /* Check if the string has already been tabled */
    string = find_and_move(pTxt, size, hash);
    if (string)
    {
        return ref_mstring(string);
    }

    /* No: create a new one */
    return make_new_tabled(pTxt, size, hash MTRACE_PASS);
} /* mstring_new_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_new_n_tabled (const char * const pTxt, size_t size MTRACE_DECL)

/* Aliased to: new_n_tabled(pTxt, len)
 *
 * Create a new tabled string by copying the C string <pTxt> of length <size>
 * and return it counting the result as one reference. If a tabled string
 * for the same <pTxt> already exists, a reference to that one is returned.
 *
 * If memory runs out, NULL is returned.
 */

{
    whash_t    hash;
    string_t * string;

    hash = hash_string(pTxt, size);

    /* Check if the string has already been tabled */
    string = find_and_move(pTxt, size, hash);
    if (string)
    {
        return ref_mstring(string);
    }

    /* No: create a new one */
    return make_new_tabled(pTxt, size, hash MTRACE_PASS);
} /* mstring_new_n_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_make_tabled (string_t * pStr, Bool deref_arg MTRACE_DECL)

/* Aliased to: make_tabled(pStr)      : deref_arg = MY_TRUE
 *             make_tabled_from(pStr) : deref_arg = MY_FALSE
 *
 * Take the string <pStr> and convert it into an tabled string if not already
 * tabled (see mstring_table_inplace()).
 * Return the counted reference to the _directly_ tabled instance (pStr might
 * be converted into an indirectly tabled string), and, if <deref_arg>
 * is TRUE, dereference the <pStr> once.
 */

{
    string_t *string;

    /* Table the string one way or the other (always succeeds) */
    mstring_table_inplace(pStr MTRACE_PASS);

    if (pStr->info.tabled)
    {
        /* The string is tabled directly, our work is done */
        if (!deref_arg)
            (void)ref_mstring(pStr);
        string = pStr;
    }
    else
    {
        /* The string is tabled indirectly, return the directly tabled
         * instance.
         */
        string = ref_mstring(pStr->link); /* Must come first! */
        if (deref_arg) free_mstring(pStr); /* Might delete *pStr */
    }

    /* That's all */

    return string;
} /* mstring_make_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_table_inplace (string_t * pStr MTRACE_DECL)

/* Aliased to: table_inplace(pStr)
 *
 * If <pStr> is an untabled string, it is morphed into an tabled one.
 * If the string does not yet exist in the string table, the result
 * will be a directly tabled string, otherwise an indirectly tabled one.
 *
 * Result is <pStr>, the refcount doesn't change.
 *
 * Also used by mstring_make_tabled().
 */

{
    string_t *string;
    whash_t    hash;
    int        idx;
    size_t     size;
    size_t     msize;

    /* If the string is already tabled, our work is done */
    if (pStr->info.tabled || pStr->link != NULL)
        return pStr;

    msize = mstr_mem_size(pStr);

    /* Get or create the tabled string for this untabled one */

    size = pStr->str->size;
    hash = get_hash(pStr);
    idx = HashToIndex(hash);

    /* Check if the string has already been tabled */
    string = find_and_move(pStr->str->txt, size, hash);

    if (!string)
    {
        /* No: change the given string structure into a directly
         * tabled one and link it into the string table.
         */
        pStr->info.tabled = MY_TRUE;

        mstr_added++;
        if (NULL == stringtable[idx])
            mstr_chains++;
        else
            mstr_collisions++;

        pStr->link = stringtable[idx];
        stringtable[idx] = pStr;

        mstr_tabled++;
        mstr_tabled_size += msize;
    }
    else
    {
        /* Yes: increment the refcount of the found string
         * and get rid of pStr's string_data.
         */
        ref_mstring(string);  /* Increments statistics */
        xfree(pStr->str);
        pStr->str = string->str;

        /* Complete the morphing of pStr into an indirectly tabled
         * string.
         */

        pStr->link = string;

        mstr_itabled++;
        mstr_itabled_size += msize;
    }

    mstr_untabled--;
    mstr_untabled_size -= msize;

    /* That's all */

    return pStr;
} /* mstring_table_inplace() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_dup (string_t * pStr MTRACE_DECL)

/* Aliased to: dup_mstring(pStr)
 *
 * Create and return a new untabled string with the same text as <pStr> but
 * just one reference.
 * If memory runs out, NULL is returned.
 *
 * Purpose is to create an instance of a string which an be freely modified
 * (which is why .hash is cleared).
 *
 * See also: mstring_unshare().
 */

{
    string_t *string;

    /* Create a new untabled string from the tabled one */

    string = mstring_alloc_string(pStr->str->size MTRACE_PASS);
    if (string)
    {
        memcpy(string->str->txt,  pStr->str->txt, pStr->str->size);
    }

    return string;
} /* mstring_dup() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_unshare (string_t * pStr MTRACE_DECL)

/* Aliased to: unshare_mstring(pStr)
 *
 * Like mstring_dup(), this function creates and returns an untabled string
 * with the same text as <pStr>, and with just one reference. In contrast
 * to mstring_dup(), this function also dereferences <pStr> on success (which
 * allows it to optimize certain cases).
 * If memory runs out, NULL is returned.
 *
 * Purpose is to create an instance of a string which an be freely modified
 * (which is why .hash is cleared).
 */

{
    string_t *string;

    /* Check for the easy cases where the argument string can be
     * the result: untabled and just one reference.
     */
    if (!pStr->info.tabled && pStr->info.ref == 1 && pStr->link == NULL)
    {
        pStr->str->hash = 0;
        return pStr;
    }

    /* Otherwise create a new untabled string from the tabled one */

    string = mstring_alloc_string(pStr->str->size MTRACE_PASS);
    if (string)
    {
        memcpy(string->str->txt,  pStr->str->txt, pStr->str->size);
        free_mstring(pStr);
    }

    return string;
} /* mstring_unshare() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_resize (string_t * pStr, size_t newlen MTRACE_DECL)

/* Aliased to: resize_mstring(pStr,newlen)
 *
 * Create an untabled copy of <pStr> with just one reference and space
 * for <newlen> bytes, remove one reference from <pStr>, and then return
 * the new string.
 * If memory runs out, NULL is returned, but the original string is still
 * dereferenced.
 */

{
    string_t *string;

    /* Check for the easy case */
    if (!pStr->info.tabled && pStr->info.ref == 1 && pStr->link == NULL
     && pStr->str->size == newlen)
    {
        pStr->str->hash = 0;
        return pStr;
    }

    /* Otherwise create a new untabled string from the tabled one */

    string = mstring_alloc_string(newlen MTRACE_PASS);
    if (string)
    {
        if (newlen > pStr->str->size)
            memcpy(string->str->txt,  pStr->str->txt, pStr->str->size);
        else
            memcpy(string->str->txt,  pStr->str->txt, newlen);
    }

    free_mstring(pStr);

    return string;
} /* mstring_resize() */

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
    whash_t hash;
    size_t  size;

    /* If pStr is tabled, our work is done */
    if (pStr->info.tabled)
        return (string_t *)pStr;

    /* If pStr is indirectly tabled, return the tabled instance */
    if (pStr->link != NULL)
        return pStr->link;

    /* Worst case: an untabled string we have to look for */

    size = mstrsize(pStr);
    hash = get_hash(pStr);

    return find_and_move(pStr->str->txt, size, hash);
} /* mstring_find_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_find_tabled_str (const char * const pTxt, size_t size)

/* Aliased to: find_tabled_str(pTxt), find_tabled_str_n(pTxt)
 *
 * Find the tabled string with the same content as the C string <pTxt> and
 * return it.
 * If there is no such tabled string, NULL is returned.
 *
 * The function does not change refcounts.
 */

{
    whash_t hash;

    hash = hash_string(pTxt, size);

    return find_and_move(pTxt, size, hash);
} /* mstring_find_tabled_str() */

/*-------------------------------------------------------------------------*/
void
mstring_free (string_t *s)

/* Aliased to: free_mstring(pStr)
 *
 * Decrement the refcount of string <s>. If it reaches 0, deallocate it
 * altogether.
 */

{
    size_t msize;

    if (!s || !s->info.ref)
        return;

    msize = mstr_mem_size(s);

    mstr_used--;
    mstr_used_size -= msize;

    if (--(s->info.ref))
    {
        return;
    }

    /* String has no refs left - deallocate it */

    if (s->info.tabled)
    {
        /* A tabled string */

        int idx;

        mstr_tabled--;
        mstr_tabled_size -= msize;

        idx = HashToIndex(get_hash(s));
        if (NULL == move_to_head(s, idx))
        {
            fatal("String %p (%s) doesn't hash to the same spot.\n"
                 , s, s->str ? s->str->txt : "<null>"
                 );
        }
        stringtable[idx] = s->link;
        xfree(s->str);
        xfree(s);

        mstr_deleted++;
        if (NULL == stringtable[idx])
            mstr_chains--;

    }
    else if (s->link == NULL)
    {
        /* An untabled string */

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
        xfree(s);
    }
} /* mstring_free() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_ref ( string_t * str)

/* Aliased to: ref_mstring_safe(s)
 *
 * Increment the refcount for string <str> and return the ref'ed string.
 * In contrast to macro ref_mstring(), this function can handle arguments
 * with sideeffects.
 */

{
    return ref_mstring(str);
} /* mstring_ref() */

/*-------------------------------------------------------------------------*/
unsigned long
mstring_deref ( string_t * str)

/* Aliased to: deref_mstring_safe(s)
 *
 * Decrement the refcount for string <str> and return the new refcount.
 * In contrast to macro deref_mstring(), this function can handle arguments
 * with sideeffects.
 */

{
    return deref_mstring(str);
} /* mstring_deref() */

/*-------------------------------------------------------------------------*/
Bool
mstring_equal(const string_t * const pStr1, const string_t * const pStr2)

/* Aliased to: mstreq(pStr1, pStr2)
 *
 * Compare the two strings <pStr1> and <pStr2> and return TRUE if they
 * have the same content, FALSE otherwise.
 */

{
    if (pStr1 == pStr2 || get_txt(pStr1) == get_txt(pStr2))
        return MY_TRUE;
    if (mstr_d_tabled(pStr1) && mstr_i_tabled(pStr2) && pStr2->link == pStr1)
        return MY_TRUE;
    if (mstr_i_tabled(pStr1) && mstr_d_tabled(pStr2) && pStr1->link == pStr2)
        return MY_TRUE;
    if (mstrsize(pStr1) != mstrsize(pStr2))
        return MY_FALSE;
    if (mstr_hash(pStr1) != 0
     && mstr_hash(pStr2) != 0
     && mstr_hash(pStr1) != mstr_hash(pStr2)
       )
        return MY_FALSE;

    return (memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr1)) == 0);
} /* mstring_equal() */

/*-------------------------------------------------------------------------*/
int
mstring_compare (const string_t * const pStr1, const string_t * const pStr2)

/* Aliased to: mstrcmp(pStr1, pStr2)
 *
 * Compare the two strings <pStr1> and <pStr2> and return
 *   -1 if <pStr1> < <pStr2>
 *    0 if <pStr1> == <pStr2>
 *   +1 if <pStr1> > <pStr2>
 */

{
    int rc;

    if (pStr1 == pStr2 || get_txt(pStr1) == get_txt(pStr2))
        return 0;
    if (mstr_d_tabled(pStr1) && mstr_i_tabled(pStr2) && pStr2->link == pStr1)
        return 0;
    if (mstr_i_tabled(pStr1) && mstr_d_tabled(pStr2) && pStr1->link == pStr2)
        return 0;

    /* We have to compare two strings by byte.
     * Remember to take the difference in length into account when the
     * leading parts match.
     */
    if (mstrsize(pStr1) == mstrsize(pStr2))
    {
        rc = memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr1));
        return rc;
    }
    if (mstrsize(pStr1) < mstrsize(pStr2))
    {
        rc = memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr1));
        return rc != 0 ? rc : -1;
    }

    rc = memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr2));
    return rc != 0 ? rc : 1;
} /* mstring_compare() */

/*-------------------------------------------------------------------------*/
char *
mstring_mstr_n_str ( const string_t * const pStr, size_t start
                   , const char * const pTxt, size_t len)

/* Aliased to: mstrstr(pStr, pTxt)
 *
 * Find the partial string <pTxt> of <len> bytes (which may contain '\0' as
 * part of the data to be found) inside of <pStr> starting at position <start>
 * and return a pointer to the location found.
 * If not found, return NULL.
 */

{
    char * cp;
    size_t left;
    char   first;

    if (start >= mstrsize(pStr))
        return NULL;

    /* Initialize 'characters remaining' and 'current position' */
    left = mstrsize(pStr) - start;
    cp = get_txt(pStr)+start;

    /* Special case: strstr("text", "") */
    if (len == 0)
        return cp;

    first = *pTxt;

    while (left >= len)
    {
        char * next;

        next = memchr(cp, first, left);
        if (NULL == next)
            break;
        left -= next - cp;
        if (left >= len && 0 == memcmp(next, pTxt, len))
            return next;
        if (left > 0)
        {
            cp = next+1;
            left--;
        }
    }

    return NULL;
} /* mstring_mstr_n_str() */

/*-------------------------------------------------------------------------*/
char *
mstring_mstr_rn_str ( const string_t * const pStr, size_t start
                   , const char * const pTxt, size_t len)

/* Aliased to: mstrrstr(pStr, pTxt)
 *
 * Find the partial string <pTxt> of <len> bytes (which may contain '\0' as
 * part of the data to be found) inside of <pStr> up to position <start>
 * and return a pointer to the location found.
 * If not found, return NULL.
 */

{
    char * cp;
    size_t left;
    char   first;

    if (start >= mstrsize(pStr))
        return NULL;

    /* Initialize 'characters remaining' and 'current position' */
    left = mstrsize(pStr) - start;
    cp = get_txt(pStr)+start;

    /* Special case: strrstr("text", "") */
    if (len == 0)
        return cp;

    first = *pTxt;

    cp++; /* Offset the first decrement */
    do {
        cp--;
        if (*cp == first
         && 0 == memcmp(cp, pTxt, len)
           )
            return cp;
    } while (cp != get_txt(pStr));

    return NULL;
} /* mstring_mstr_n_str() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_add_slash (const string_t *str MTRACE_DECL)

/* Aliased to: add_slash(str)
 *
 * Create and return a new string with the data of <str> prepended
 * by a slash ('/'). The result string is untabled and has one reference,
 * the old string <str> is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_t *tmp;
    char * txt;

    tmp = mstring_alloc_string(mstrsize(str)+1 MTRACE_PASS);
    if (tmp)
    {
        txt = get_txt(tmp);
        *txt = '/';
        memcpy(txt+1, get_txt(str), mstrsize(str));
    }
    return tmp;
} /* mstring_add_slash() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_del_slash (string_t *str MTRACE_DECL)

/* Aliased to: del_slash(str)
 *
 * Remove any given leading slash from the string <str> and return the
 * resulting string. If <str> has no slashed to begin with, the result
 * is a new reference to <str>.
 *
 * If memory runs out, NULL is returned.
 */

{
    char * txt;

    txt = get_txt(str);
    while (*txt == '/')
        txt++;
    if (txt == get_txt(str))
        return ref_mstring(str);

    return mstring_new_string(txt MTRACE_PASS);
} /* mstring_del_slash() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_del_dotc (string_t *str MTRACE_DECL)

/* Aliased to: del_dotc(str)
 *
 * If <str> ends in a trailing ".c", create a new untabled string without
 * the suffix and return it. Otherwise return a new reference to <str>.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_t *tmp;
    size_t len;
    char * txt, *p;

    txt = get_txt(str);
    len = mstrsize(str);

    p = strrchr(txt, '.');

    if (p && (size_t)(p - txt) + 2 == len && p[1] == 'c')
        len = (size_t)(p - txt);
    else
        return ref_mstring(str);

    tmp = mstring_alloc_string(len MTRACE_PASS);
    if (tmp)
    {
        memcpy(get_txt(tmp), txt, len);
    }

    return tmp;
} /* mstring_del_dotc() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_cvt_progname (const string_t *str MTRACE_DECL)

/* Aliased to: cvt_progname(str)
 *
 * <str> is a program name: no leading slash, but a trailing '.c'.
 * Create and return a new string with the '.c' removed, and a leading slash
 * added if compat_mode is not set.
 *
 * The result string is untabled and has one reference, the old string <str>
 * is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_t *tmp;
    size_t len;
    char * txt, *txt2, *p;

    txt = get_txt(str);
    len = mstrsize(str);

    p = strrchr(txt, '.');

    if (p)
        len = (size_t)(p - txt);

    if (!compat_mode)
        len++;

    tmp = mstring_alloc_string(len MTRACE_PASS);
    if (tmp)
    {
        txt2 = get_txt(tmp);
        if (!compat_mode)
        {
            *txt2 = '/';
            txt2++;
            len--;
        }
        memcpy(txt2, txt, len);
    }

    return tmp;
} /* mstring_cvt_progname() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_add (const string_t *left, const string_t *right MTRACE_DECL)

/* Aliased to: mstr_add(left,right)
 *
 * Create and return a new string with the data of <left> concatenated
 * with the data of <right>.
 * The result string is untabled and has one reference,
 * the old strings <left> and <right> are not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t lleft, lright;
    string_t *tmp;

    lleft = mstrsize(left);
    lright = mstrsize(right);
    tmp = mstring_alloc_string(lleft+lright MTRACE_PASS);
    if (tmp)
    {
        char * txt;

        txt = get_txt(tmp);
        memcpy(txt, get_txt(left), lleft);
        memcpy(txt+lleft, get_txt(right), lright);
    }
    return tmp;
} /* mstring_add() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_append (string_t *left, const string_t *right MTRACE_DECL)

/* Aliased to: mstr_append(left,right)
 *
 * If <left> is singular (ie. untabled with only one reference):
 * Append the data from <right> to the string <left>, and return the
 * modified <left> with an additional reference.
 *
 * If <left> is not singular:
 * Create and return a new string with the data of <left> concatenated
 * with the data of <right>.
 * The result string is untabled and has one reference,
 * the old strings <left> and <right> are not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t lleft, lright;

    if (!mstr_singular(left))
        return mstring_add(left, right MTRACE_PASS);

    lleft = mstrsize(left);
    lright = mstrsize(right);
    left = mstring_realloc_string(left, lleft+lright MTRACE_PASS);
    if (left)
    {
        char * txt;

        txt = get_txt(left);
        memcpy(get_txt(left)+lleft, get_txt(right), lright);
        ref_mstring(left);
    }
    return left;
} /* mstring_append() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_add_txt (const string_t *left, const char *right, size_t len MTRACE_DECL)

/* Aliased to: mstr_add_txt(left,right,len)
 *
 * Create and return a new string with the data of <left> concatenated
 * with the <len> bytes of data in buffer <right>.
 * The result string is untabled and has one reference,
 * the old string <left> is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t lleft;
    string_t *tmp;
    char * txt;

    lleft = mstrsize(left);
    tmp = mstring_alloc_string(lleft+len MTRACE_PASS);
    if (tmp)
    {
        txt = get_txt(tmp);
        memcpy(txt, get_txt(left), lleft);
        memcpy(txt+lleft, right, len);
    }
    return tmp;
} /* mstring_add_txt() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_append_txt (string_t *left, const char *right, size_t len MTRACE_DECL)

/* Aliased to: mstr_append_txt(left,right,len)
 *
 * If <left> is singular (ie. untabled with only one reference):
 * Append the <len> bytes of data in buffer <right> to the string <left>, and
 * return the modified <left> with an additional reference.
 *
 * If <left> is not singular:
 * Create and return a new string with the data of <left> concatenated
 * with the <len> bytes of data in buffer <right>.
 * The result string is untabled and has one reference,
 * the old string <left> is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t lleft;

    if (!mstr_singular(left))
        return mstring_add_txt(left, right, len MTRACE_PASS);

    lleft = mstrsize(left);
    left = mstring_realloc_string(left, lleft+len MTRACE_PASS);
    if (left)
    {
        char * txt;

        txt = get_txt(left);
        memcpy(get_txt(left)+lleft, right, len);
        ref_mstring(left);
    }
    return left;
} /* mstring_append_txt() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_add_to_txt (const char *left, size_t len, const string_t *right MTRACE_DECL)

/* Aliased to: mstr_add_to_txt(left,len,right)
 *
 * Create and return a new string with the <len> bytes of data in buffer <left>
 * concatenated with the string <right>.
 * The result string is untabled and has one reference,
 * the old string <right> is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t lright;
    string_t *tmp;
    char * txt;

    lright = mstrsize(right);
    tmp = mstring_alloc_string(lright+len MTRACE_PASS);
    if (tmp)
    {
        txt = get_txt(tmp);
        memcpy(txt, left, len);
        memcpy(txt+len, get_txt(right), lright);
    }
    return tmp;
} /* mstring_add_to_txt() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_repeat (const string_t *base, size_t num MTRACE_DECL)

/* Aliased to: mstr_repeat(base,num)
 *
 * Create and return a new string which is the <base> string repeated <num>
 * times.
 * The result string is untabled and has one reference,
 * the old string <base> is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t len, reslen;
    string_t *result;

    len = mstrsize(base);
    reslen = len * num;
    result = mstring_alloc_string(reslen MTRACE_PASS);

    if (result && reslen)
    {
        size_t   curlen;
        char   * txt = get_txt(result);

        /* Seed result[] with one copy of the string */
        memcpy(txt, get_txt(base), len);

        /* Repeatedly double the string in result */
        curlen = len;
        while (2*curlen < reslen)
        {
            memcpy(txt+curlen, txt, curlen);
            curlen *= 2;
        }

        /* Fill up result to the full length */
        if (reslen > curlen)
            memcpy(txt+curlen, txt, reslen-curlen);
    }
    return result;
} /* mstring_repeat() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_extract (const string_t *str, size_t start, long end MTRACE_DECL)

/* Aliased to: mstr_extract(str,start,end)
 *
 * Create and return a new string made of <str>[<start>..<end>].
 * If <end> is negative, the result is made of <str>[<start>..].
 * The result string is untabled and has one reference,
 * the old string <str> is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t len, reslen;
    string_t *result;

    len = mstrsize(str);
    if (!len)
    {
        error("(mstring_extract) Can't extract from empty string.\n");
        /* NOTREACHED */
        return NULL;
    }

    if (end < 0)
        end = (long)len-1;

    if (end >= (long)len)
    {
        error("(mstring_extract) end %ld >= len %lu\n"
             , end, (unsigned long) len);
        /* NOTREACHED */
        return NULL;
    }

    if (end < (long)start)
    {
        error("(mstring_extract) end %ld < start %lu\n"
             , end, (unsigned long) start);
        /* NOTREACHED */
        return NULL;
    }

    if (start >= len)
    {
        error("(mstring_extract) start %lu >= string length %lu\n"
             , (unsigned long) start, (unsigned long)len);
        /* NOTREACHED */
        return NULL;
    }

    reslen = (size_t)end - start + 1;
    result = mstring_alloc_string(reslen MTRACE_PASS);
    if (result && reslen)
    {
        memcpy(get_txt(result), get_txt(str)+start, reslen);
    }
    return result;
} /* mstring_extract() */

/*-------------------------------------------------------------------------*/
Bool
mstring_prefixed (const string_t *p, const string_t *s)

/* Aliased to: mstrprefixed(p,s)
 *
 * Return TRUE if string <s> begins with string <p>, FALSE if not.
 */

{
    char *pp, *ps;
    size_t lp, ls;

    lp = mstrsize(p); pp = get_txt(p);
    ls = mstrsize(s); ps = get_txt(s);

    for (; lp > 0 && ls > 0; lp--, ls--)
    {
        if (*pp++ != *ps++)
            return MY_FALSE;
    }

    return (lp == 0) ? MY_TRUE : MY_FALSE;
} /* mstring_prefixed() */

/*-------------------------------------------------------------------------*/
long
mstring_chr (const string_t *p, char c)

/* Aliased to: mstrchr(p,c)
 *
 * Search character <c> in string <s> and return its position.
 * Return -1 if not found.
 */

{
    char *pp;

    pp = memchr(get_txt(p), c, mstrsize(p));
    if (pp != NULL)
        return pp - get_txt(p);
    return -1;
} /* mstring_chr() */

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
        count_ref_from_string(shstring[i]);
} /* mstring_note_refs() */

/*-------------------------------------------------------------------------*/
void
mstring_walk_strings (void (*func) (string_t *))

/* GC support: Call (*func)(str) for all tabled strings in the string table.
 *
 * Usually the function is "remove_unreferenced_string()" which removes
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
        strbuf_addf(sbuf
                   , "Strings alloced\t\t\t%8lu %9lu (%lu + %lu overhead)\n"
                   , distinct_strings, distinct_size + stringtable_size
                   , distinct_size - distinct_overhead
                   , distinct_overhead + stringtable_size
                   );
    }
    else
    {
        strbuf_add(sbuf, "\nString handler:\n");
        strbuf_add(sbuf,   "---------------\t  Strings     Bytes (Data+Overhead)\n");
        strbuf_addf(sbuf,  "Total asked for\t%9lu %9lu (%9lu+%9lu)\n"
                        , mstr_used
                        , mstr_used_size
                        , mstr_used_size
                          ? mstr_used_size - mstr_used * STR_OVERHEAD
                          : 0
                        , mstr_used * STR_OVERHEAD
                        );
        strbuf_addf(sbuf,  "Total allocated\t%9lu %9lu (%9lu+%9lu)\n"
                        , distinct_strings
                        , distinct_size + stringtable_size
                        , distinct_size - distinct_overhead
                        , distinct_overhead + stringtable_size
                        );
        strbuf_addf(sbuf,  " - tabled\t%9lu %9lu (%9lu+%9lu)\n"
                        , mstr_tabled
                        , mstr_tabled_size + stringtable_size
                        , mstr_tabled_size
                          ? mstr_tabled_size - mstr_tabled * STR_OVERHEAD
                          : 0
                        , mstr_tabled * STR_OVERHEAD + stringtable_size
                        );
        strbuf_addf(sbuf,  " - ind. tabled\t%9lu %9lu (%9lu+%9lu)\n"
                        , mstr_itabled
                        , mstr_itabled_size
                        , mstr_itabled_size
                          ? mstr_itabled_size - mstr_itabled * STR_OVERHEAD
                          : 0
                        , mstr_itabled * sizeof(string_t)
                        );
        strbuf_addf(sbuf,  " - untabled\t%9lu %9lu (%9lu+%9lu)\n"
                        , mstr_untabled
                        , mstr_untabled_size
                        , mstr_untabled_size
                          ? mstr_untabled_size - mstr_untabled * STR_OVERHEAD
                          : 0
                        , mstr_untabled * STR_OVERHEAD
                        );
        strbuf_addf(sbuf, "\nSpace required / naive string implementation: "
                          "%lu%% with, %lu%% without overhead.\n"
                        , ((distinct_size + stringtable_size) * 100L)
                          / (mstr_used_size - mstr_used * sizeof(string_t))
                        , ((distinct_size + stringtable_size
                                          - distinct_overhead) * 100L)
                          / (mstr_used_size - mstr_used * STR_OVERHEAD)
                        );
        strbuf_addf(sbuf, "Searches by address: %lu - found: %lu (%.1f%%) - avg length: %7.3f\n"
                        , mstr_searches
                        , mstr_found, 100.0 * (float)mstr_found / (float)mstr_searches
                        , (float)mstr_searchlen / (float)mstr_searches
                        );
        strbuf_addf(sbuf, "Searches by content: %lu - found: %lu (%.1f%%) - avg length: %7.3f\n"
                        , mstr_searches_byvalue
                        , mstr_found_byvalue, 100.0 * (float)mstr_found_byvalue / (float)mstr_searches_byvalue
                        , (float)mstr_searchlen_byvalue / (float)mstr_searches_byvalue
                        );
        strbuf_addf(sbuf, "Hash chains: %lu (%.1f%%)\n"
                        , mstr_chains
                        , 100.0 * (float)mstr_chains / (float)HTABLE_SIZE
                        );
        strbuf_addf(sbuf, "Distinct strings added: %lu "
                          "- deleted: %lu\n"
                        , mstr_added, mstr_deleted
                        );
        strbuf_addf(sbuf, "Collisions: %lu (%.1f%% added/%1.f%% chains)\n"
                        , mstr_collisions
                        , 100.0 * (float)mstr_collisions / (float)mstr_added
                        , 100.0 * (float)mstr_collisions / (float)mstr_chains
                        );
    }

    return stringtable_size + distinct_size;
#   undef STR_OVERHEAD
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

    ST_NUMBER(DID_ST_STRINGS, mstr_used);
    ST_NUMBER(DID_ST_STRING_SIZE, mstr_used_size);

    ST_NUMBER(DID_ST_STR_TABLE_SIZE, HTABLE_SIZE * sizeof(string_t *));
    ST_NUMBER(DID_ST_STR_OVERHEAD, sizeof(string_t)+sizeof(string_data_t)-1);
    ST_NUMBER(DID_ST_STR_IT_OVERHEAD, sizeof(string_t));

    ST_NUMBER(DID_ST_STR_CHAINS,     mstr_chains);
    ST_NUMBER(DID_ST_STR_ADDED,      mstr_added);
    ST_NUMBER(DID_ST_STR_DELETED,    mstr_deleted);
    ST_NUMBER(DID_ST_STR_COLLISIONS, mstr_collisions);

    ST_NUMBER(DID_ST_UNTABLED,      mstr_untabled);
    ST_NUMBER(DID_ST_UNTABLED_SIZE, mstr_untabled_size);
    ST_NUMBER(DID_ST_ITABLED,       mstr_itabled);
    ST_NUMBER(DID_ST_ITABLED_SIZE,  mstr_itabled_size);
    ST_NUMBER(DID_ST_TABLED,        mstr_tabled);
    ST_NUMBER(DID_ST_TABLED_SIZE,   mstr_tabled_size);

    ST_NUMBER(DID_ST_STR_SEARCHES,          mstr_searches);
    ST_NUMBER(DID_ST_STR_SEARCHLEN,         mstr_searchlen);
    ST_NUMBER(DID_ST_STR_SEARCHES_BYVALUE,  mstr_searches_byvalue);
    ST_NUMBER(DID_ST_STR_SEARCHLEN_BYVALUE, mstr_searchlen_byvalue);
    ST_NUMBER(DID_ST_STR_FOUND,             mstr_found);
    ST_NUMBER(DID_ST_STR_FOUND_BYVALUE,     mstr_found_byvalue);

#undef ST_NUMBER
} /* string_dinfo_status() */

/***************************************************************************/
