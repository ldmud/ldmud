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
 * To reduce memory usage even further, strings can be entered into a table.
 * On the creation of a new string the driver can lookup the table for
 * an already existing copy and return a reference to a string held therein.
 * This is used mainly for function names in programs, but also for
 * mapping keys. The table is organized as a hash table
 * with HTABLE_SIZE entries.
 *
 * Strings are sequences of chars, stored in an array of known size. The
 * size itself is stored separately, allowing the string to contain every
 * possible character. Internally the module appends a '\0' character
 * to the string data to make it somewhat compatible with C system
 * functions; however, this character itself is not counted in the size
 * of the string, and the module itself doesn't rely on it.
 *
 * Strings are managed using a single structure: string_t.
 *
 *    struct string_s
 *    {
 *        struct {
 *            Bool tabled      :  1;
 *            unsigned int ref : 31;
 *        } info;
 *        string_t * next;            String table pointer.
 *        size_t     size;            Length of the string 
 *        hash32_t    hash;            0, or the hash of the string
 *        char       txt[1.. .size];
 *        char       null             Gratuituous terminator
 *    }
 *
 * The hash of the string is computed on-demand. Should the string hash
 * to value 0, the value 0x8000 is used instead - this way the usual
 * calculation (hash % tablesize) won't be affected.
 *
 * This string_t value is the one referenced by svalues and the like.
 * It allows the following string types:
 *
 * Untabled, freely allocated strings:
 *   .tabled is FALSE
 *
 * Tabled (shared) strings:
 *   .tabled is TRUE
 *   .next is the hash chain pointer, the reference from the
 *   table is not counted.
 *
 * TODO: Make functions mstr_add() resilient to receiving NULL
 * TODO:: pointers as args. This way stuff like rc =
 * TODO:: mstr_add(rc,...) will always work and we need to check
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
#if HTABLE_SIZE > MAX_HASH32
#error The hash table size must not be larger then MAX_HASH32.
The end.
#endif

static INLINE hash32_t
HashToIndex(hash32_t hash)
/* Adapt a hash value to our table size.
 */
{
#if !( (HTABLE_SIZE) & (HTABLE_SIZE)-1 )
    // use faster masking in case of HTABLE_SIZE being a power of 2.
    return hash & ((HTABLE_SIZE)-1);
#else
    // fallback to slow modulo.
    return hash % HTABLE_SIZE;
#endif
}

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

static mp_uint mstr_tabled_count = 0;
  /* Number of distinct strings in the string table.
   */

static mp_uint mstr_tabled_size = 0;
  /* Total memory held in the string table.
   */

static mp_uint mstr_chains = 0;
  /* Number of hash chains in the string table.
   */

static statcounter_t mstr_added = 0;
  /* Number of distinct strings added to the string table.
   */

static statcounter_t mstr_deleted = 0;
  /* Number of distinct strings deleted from the string table.
   */

static statcounter_t mstr_collisions = 0;
  /* Number of collisions when adding a new distinct string.
   */

static mp_uint mstr_untabled_count = 0;
  /* Number of distinct untabled strings.
   */

static mp_uint mstr_untabled_size = 0;
  /* Total memory held in untabled strings.
   */

static mp_uint mstr_searchlen_byvalue = 0;
  /* Number of search steps along hash chains with content comparisons.
   */

static statcounter_t mstr_searches_byvalue = 0;
  /* Number of searches in the string table with content comparison.
   */

static statcounter_t mstr_found_byvalue = 0;
  /* Number of successful searches in the string table with content comparison.
   */

static mp_uint mstr_searchlen = 0;
  /* Number of search steps along hash chains without content comparisons.
   */

static statcounter_t mstr_searches = 0;
  /* Number of searches in the string table without content comparisons.
   */

static statcounter_t mstr_found = 0;
  /* Number of successful searches in the string table with content comparison.
   */

#ifdef EXT_STRING_STATS
statcounter_t stNumEqual = 0;
statcounter_t stNumHashEqual = 0;
statcounter_t stNumTabledEqual = 0;
statcounter_t stNumComp = 0;
statcounter_t stNumTabledComp = 0;
statcounter_t stNumTabledChecked = 0;
statcounter_t stNumTabledCheckedTable = 0;
statcounter_t stNumTabledCheckedSearch = 0;
#endif /* EXT_STRING_STATS */

/*-------------------------------------------------------------------------*/
static INLINE hash32_t
hash_string_inl (const char * const s, size_t size)

/* Compute the hash for string <s> of length <size> and return it.
 * The result will always be non-zero.
 */

{
    if (size > MSTRING_HASH_LENGTH)
        size = MSTRING_HASH_LENGTH;

    hash32_t hash = hashmem32(s, size);
    if (!hash)
        hash = 1 << (sizeof (hash) * CHAR_BIT - 1);
    return hash;
} /* hash_string_inl() */

hash32_t
hash_string (const char * const s, size_t size)
{
    return hash_string_inl(s, size);
}

hash32_t
hash_string_chained (const char * const s, size_t size, hash32_t chainhash)
/* Compute the hash for string <s> of length <size> and use <chainhash> as
 * initial value. Used to hash several strings into one hash value.
 * The result will always be non-zero.
 */
{
    if (size > MSTRING_HASH_LENGTH)
        size = MSTRING_HASH_LENGTH;
    
    hash32_t hash = hashmem32_chained(s, size, chainhash);
    if (!hash)
        hash = 1 << (sizeof (hash) * CHAR_BIT - 1);
    return hash;    
} // hash_string_chained

/*-------------------------------------------------------------------------*/
static INLINE hash32_t
get_hash (string_t * pStr)
/* Return the hash of string <pStr>, computing it if necessary.
 */

{
    if (!pStr->hash)
        pStr->hash = hash_string_inl(pStr->txt, pStr->size);

    return pStr->hash;
} /* get_hash() */

/*-------------------------------------------------------------------------*/
hash32_t
mstring_get_hash (string_t * pStr)

/* Aliased to: mstr_get_hash()
 *
 * Return the hash value of <pStr>, computing it if necessary.
 */

{
    return get_hash(pStr);
} /* mstring_get_hash() */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
find_and_move (const char * const s, size_t size, hash32_t hash)
/* If <s> is a tabled string of length <size> and <hash> in the related
 * stringtable chain: find it, move it to the head of the chain and return its
 * string_t*.
 *
 * If <s> is not tabled, return NULL.
 */

{
    string_t *prev, *rover;

    hash32_t idx = HashToIndex(hash);

    mstr_searches_byvalue++;

    /* Find the string in the table */

    mstr_searchlen_byvalue++;
    for ( prev = NULL, rover = stringtable[idx]
        ;    rover != NULL
          && get_txt(rover) != s
          && !(   size == mstrsize(rover)
               && hash == get_hash(rover)
               && 0 == memcmp(get_txt(rover), s, size)
              )
        ; prev = rover, rover = rover->next
        )
        mstr_searchlen_byvalue++;

    /* If the string is in the table (rover != NULL), but not at the beginning
     * of the chain, move it there.
     */
    if (rover && prev)
    {
        prev->next = rover->next;
        rover->next = stringtable[idx];
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

    mstr_searchlen++;
    for ( prev = NULL, rover = stringtable[idx]
        ; rover != NULL && rover != s
        ; prev = rover, rover = rover->next
        )
    {
        mstr_searchlen++;
    }

    /* If s is found (rover != NULL), but not at the beginning of the chain,
     * move it there
     */

    if (rover && prev)
    {
        prev->next = rover->next;
        rover->next = stringtable[idx];
        stringtable[idx] = rover;
    }

    if (rover)
        mstr_found++;

    return rover;
} /* move_to_head() */

/*-------------------------------------------------------------------------*/
static INLINE string_t *
make_new_tabled (const char * const pTxt, size_t size, hash32_t hash MTRACE_DECL)
/* Helper function for mstring_new_tabled() and mstring_new_n_tabled().
 *
 * Create a new tabled string by copying the data string <pTxt> of length
 * <size> and <hash> and return it counting the result as one reference. The
 * string MUST NOT yet exist in the table.
 *
 * If memory runs out, NULL is returned.
 */

{
    string_t * string;
    hash32_t   idx = HashToIndex(hash);

    /* Get the memory for a new one */

    string = xalloc_pass(size + sizeof(*string));
      /* sizeof(*string) includes the extra data byte */
    if (!string)
        return NULL;

    /* Set up the structures and table the string */

    string->size = size;
    string->hash = hash;
    memcpy(string->txt, pTxt, size);
    string->txt[size] = '\0';
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

    string->next = stringtable[idx];
    stringtable[idx] = string;

    {
        size_t msize;

        msize = mstr_mem_size(string);
        mstr_used++;
        mstr_used_size += msize;
        mstr_tabled_count++;
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
    string_t *string;

    /* Get the memory */

    string = xalloc_pass(iSize + sizeof(*string));
    if (!string)
        return NULL;

    /* Set up the structures */
    string->size = iSize;
    string->next = NULL;
    string->hash = 0;
    string->txt[iSize] = '\0';
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
        mstr_untabled_count++;
        mstr_untabled_size += msize;
    }

    return string;
} /* mstring_alloc_string() */

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
        memcpy(string->txt, pTxt, size);
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
        memcpy(string->txt, pTxt, len);
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
    hash32_t   hash;
    size_t     size;
    string_t * string;

    size = strlen(pTxt);
    hash = hash_string_inl(pTxt, size);

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
    hash32_t   hash;
    string_t * string;

    hash = hash_string_inl(pTxt, size);

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
static string_t *
table_string (string_t * pStr MTRACE_DECL)

/* Called by: mstring_make_tabled()
 *
 * Table the string <pStr> and return a pointer to the tabled string.
 * If <pStr> is already tabled, it will also be the result.
 * If <pStr> is not tabled, but a string of this content already exist,
 * the reference to the tabled string will be the result.
 * Otherwise, <pStr> is added to the table and returned.
 *
 * Return NULL when out of memory.
 */

{
    string_t *string;
    hash32_t   hash;
    hash32_t   idx;
    size_t     size;
    size_t     msize;

    /* If the string is already tabled, our work is done */
    if (pStr->info.tabled)
        return pStr;

    msize = mstr_mem_size(pStr);

    /* Get or create the tabled string for this untabled one */

    size = pStr->size;
    hash = get_hash(pStr);
    idx = HashToIndex(hash);

    /* Check if the string has already been tabled */
    string = find_and_move(pStr->txt, size, hash);

    if (!string)
    {
        /* No: add the string into the table.
         */
        pStr->info.tabled = MY_TRUE;

        mstr_added++;
        if (NULL == stringtable[idx])
            mstr_chains++;
        else
            mstr_collisions++;

        pStr->next = stringtable[idx];
        stringtable[idx] = pStr;

        mstr_tabled_count++;
        mstr_tabled_size += msize;

        mstr_untabled_count--;
        mstr_untabled_size -= msize;

        string = pStr;
    }

    /* That's all */

    return string;
} /* table_string() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_make_tabled (string_t * pStr, Bool deref_arg MTRACE_DECL)

/* Aliased to: make_tabled(pStr)      : deref_arg = MY_TRUE
 *             make_tabled_from(pStr) : deref_arg = MY_FALSE
 *
 * Take the string <pStr> and convert it into an tabled string if not already
 * tabled.
 * Return the counted reference to the tabled instance, and, if <deref_arg> is
 * TRUE, dereference the <pStr> once.
 *
 * Return NULL when out of memory.
 */

{
    string_t *string;

    /* Table the string one way or the other (always succeeds) */
    string = table_string(pStr MTRACE_PASS);
    if (!string)
        return NULL;

    (void)ref_mstring(string);
    if (deref_arg)
        free_mstring(pStr);

    return string;
} /* mstring_make_tabled() */

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

    string = mstring_alloc_string(pStr->size MTRACE_PASS);
    if (string)
    {
        memcpy(string->txt,  pStr->txt, pStr->size);
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
    if (!pStr->info.tabled && pStr->info.ref == 1)
    {
        pStr->hash = 0;
        return pStr;
    }

    /* Otherwise create a new untabled string from the tabled one */

    string = mstring_alloc_string(pStr->size MTRACE_PASS);
    if (string)
    {
        memcpy(string->txt,  pStr->txt, pStr->size);
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
    if (!pStr->info.tabled && pStr->info.ref == 1
     && pStr->size == newlen)
    {
        pStr->hash = 0;
        return pStr;
    }

    /* Otherwise create a new untabled string from the tabled one */

    string = mstring_alloc_string(newlen MTRACE_PASS);
    if (string)
    {
        if (newlen > pStr->size)
            memcpy(string->txt,  pStr->txt, pStr->size);
        else
            memcpy(string->txt,  pStr->txt, newlen);
    }

    free_mstring(pStr);

    return string;
} /* mstring_resize() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_find_tabled (string_t * pStr)

/* Aliased to: find_tabled(pStr)
 *
 * Find the tabled string with the same content as <pStr> and return it.
 * If <pStr> is a tabled string, it will be the result itself.
 * If there is no such tabled string, NULL is returned.
 *
 * The function does not change refcounts.
 */

{
    hash32_t hash;
    size_t  size;

#ifdef EXT_STRING_STATS
    stNumTabledChecked++;
#endif /* EXT_STRING_STATS */

    /* If pStr is tabled, our work is done */
    if (pStr->info.tabled)
    {
#ifdef EXT_STRING_STATS
        stNumTabledCheckedTable++;
#endif /* EXT_STRING_STATS */
        return (string_t *)pStr;
    }

    /* Worst case: an untabled string we have to look for */

#ifdef EXT_STRING_STATS
    stNumTabledCheckedSearch++;
#endif /* EXT_STRING_STATS */
    size = mstrsize(pStr);
    hash = get_hash(pStr);

    return find_and_move(pStr->txt, size, hash);
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
    hash32_t   hash;

    hash = hash_string_inl(pTxt, size);

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

        hash32_t idx;

        mstr_tabled_count--;
        mstr_tabled_size -= msize;

        idx = HashToIndex(get_hash(s));
        if (NULL == move_to_head(s, idx))
        {
            fatal("String %p (%s) doesn't hash to the same spot.\n"
                 , s, s->txt
                 );
        }

        stringtable[idx] = s->next;

        if (NULL == stringtable[idx])
            mstr_chains--;
        mstr_deleted++;

    }
    else
    {
        /* An untabled string */

        mstr_untabled_count--;
        mstr_untabled_size -= msize;
    }

    /* The deallocation of the string itself is the same in either case. */
    xfree(s);
} /* mstring_free() */

/*-------------------------------------------------------------------------*/
Bool
mstring_equal(string_t * const pStr1, string_t * const pStr2)

/* Aliased to: mstreq(pStr1, pStr2)
 *
 * Compare the two strings <pStr1> and <pStr2> and return TRUE if they
 * have the same content, FALSE otherwise.
 */

{
#ifdef EXT_STRING_STATS
    stNumEqual++;
#endif /* EXT_STRING_STATS */
    if (pStr1 == pStr2 || get_txt(pStr1) == get_txt(pStr2))
    {
#ifdef EXT_STRING_STATS
        if (mstr_tabled(pStr1))
            stNumTabledEqual++;
#endif /* EXT_STRING_STATS */
        return MY_TRUE;
    }

    if (mstrsize(pStr1) != mstrsize(pStr2))
        return MY_FALSE;

    if (get_hash(pStr1) != get_hash(pStr2))
    {
#ifdef EXT_STRING_STATS
        stNumHashEqual++;
#endif /* EXT_STRING_STATS */
        return MY_FALSE;
    }

    return (memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr1)) == 0);
} /* mstring_equal() */

/*-------------------------------------------------------------------------*/
int
mstring_compare (string_t * const pStr1, string_t * const pStr2)

/* Aliased to: mstrcmp(pStr1, pStr2)
 *
 * Compare the two strings <pStr1> and <pStr2> and return
 *   -1 if <pStr1> < <pStr2>
 *    0 if <pStr1> == <pStr2>
 *   +1 if <pStr1> > <pStr2>
 */

{
    int rc;

#ifdef EXT_STRING_STATS
    stNumComp++;
#endif /* EXT_STRING_STATS */

    /* Compare for direct equality */
    if (pStr1 == pStr2 || get_txt(pStr1) == get_txt(pStr2))
    {
#ifdef EXT_STRING_STATS
        if (mstr_tabled(pStr1))
            stNumTabledComp++;
#endif /* EXT_STRING_STATS */
        return 0;
    }

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
int
mstring_order (string_t * const pStr1, string_t * const pStr2)

/* Aliased to: mstr_order(pStr1, pStr2)
 *
 * Compare the two strings <pStr1> and <pStr2> and return
 *   -1 if <pStr1> < <pStr2>
 *    0 if <pStr1> == <pStr2>
 *   +1 if <pStr1> > <pStr2>
 *
 * Other than mstring_compare() this function does not implement
 * a lexicographic order, but instead a faster hash-centric order.
 * It is thus more useful for sorted arrays and mapping indices.
 */

{
    int rc;

#ifdef EXT_STRING_STATS
    stNumComp++;
#endif /* EXT_STRING_STATS */

    /* Compare for direct equality */
    if (pStr1 == pStr2 || get_txt(pStr1) == get_txt(pStr2))
    {
#ifdef EXT_STRING_STATS
        if (mstr_tabled(pStr1))
            stNumTabledComp++;
#endif /* EXT_STRING_STATS */
        return 0;
    }

    /* Shorter strings are 'less' than longer strings */
    {
        size_t size1 = mstrsize(pStr1);
        size_t size2 = mstrsize(pStr2);
        if (size1 != size2)
            return size1 < size2 ? -1 : 1;
    }

    /* Strings with a smaller hash also count as 'less'. */
    {
        hash32_t hash1 = get_hash(pStr1);
        hash32_t hash2 = get_hash(pStr2);
        if (hash1 != hash2)
            return  hash1 < hash2 ? -1 : 1;
    }

    /* Length and hash are identical - we have to compare byte by byte. */
    rc = memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr1));
    return rc;
} /* mstring_order() */

/*-------------------------------------------------------------------------*/
const char *
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
    const char * cp;
    size_t left;
    char   first;

    if (start > mstrsize(pStr))
        return NULL;

    /* Initialize 'characters remaining' and 'current position' */
    left = mstrsize(pStr) - start;
    /* remove the const qualifier temporarily when calling get_txt(). */
    cp = get_txt((string_t *const)pStr)+start;

    /* Special case: strstr("text", "") */
    if (len == 0)
        return cp;

    first = *pTxt;

    while (left >= len)
    {
        const char * next;

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
const char *
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
    const char * cp;
    size_t left;
    char   first;

    if (start > mstrsize(pStr))
        return NULL;

    /* Initialize 'characters remaining' and 'current position' */
    left = mstrsize(pStr) - start;
    cp = get_txt((string_t *const)pStr)+start;

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
    } while (cp != get_txt((string_t *const)pStr));

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
        memcpy(txt+1, get_txt((string_t *const)str), mstrsize(str));
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
    const char * txt, *p;
    char *txt2;

    txt = get_txt((string_t *const)str);
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
        memcpy(txt, get_txt((string_t *const)left), lleft);
        memcpy(txt+lleft, get_txt((string_t *const)right), lright);
    }
    return tmp;
} /* mstring_add() */

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
        memcpy(txt, get_txt((string_t *const)left), lleft);
        memcpy(txt+lleft, right, len);
    }
    return tmp;
} /* mstring_add_txt() */

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
        memcpy(txt+len, get_txt((string_t *const)right), lright);
    }
    return tmp;
} /* mstring_add_to_txt() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_append (string_t *left, const string_t *right MTRACE_DECL)

/* Aliased to: mstr_append(left,right)
 *
 * Create and return a new string with the data of <left> concatenated
 * with the data of <right>.
 * The result string is untabled and has one reference,
 * <left> is dereferenced once (if not NULL).
 * the old strings <right> is not changed.
 *
 * If memory runs out or if <left> is already NULL, NULL is returned.
 */

{
    string_t *tmp;

    if (left == NULL)
        return NULL;

    tmp = mstring_add(left, right MTRACE_PASS);
    free_mstring(left);
    return tmp;
} /* mstring_append() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_append_txt (string_t *left, const char *right, size_t len MTRACE_DECL)

/* Aliased to: mstr_append_txt(left,right,len)
 *
 * Create and return a new string with the data of <left> concatenated
 * with the <len> bytes of data in buffer <right>.
 * The result string is untabled and has one reference,
 * <left> is dereferenced once (if not NULL).
 *
 * If memory runs out or if <left> is already NULL, NULL is returned.
 */

{
    string_t *tmp;

    if (left == NULL)
        return NULL;

    tmp = mstring_add_txt(left, right, len MTRACE_PASS);
    free_mstring(left);
    return tmp;
} /* mstring_append_txt() */

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
        memcpy(txt, get_txt((string_t *const)base), len);

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
        errorf("(mstring_extract) Can't extract from empty string.\n");
        /* NOTREACHED */
        return NULL;
    }

    if (end < 0)
        end = (long)len-1;

    if (end >= (long)len)
    {
        errorf("(mstring_extract) end %ld >= len %lu\n"
             , end, (unsigned long) len);
        /* NOTREACHED */
        return NULL;
    }

    if (end < (long)start)
    {
        errorf("(mstring_extract) end %ld < start %lu\n"
             , end, (unsigned long) start);
        /* NOTREACHED */
        return NULL;
    }

    if (start >= len)
    {
        errorf("(mstring_extract) start %lu >= string length %lu\n"
             , (unsigned long) start, (unsigned long)len);
        /* NOTREACHED */
        return NULL;
    }

    reslen = (size_t)end - start + 1;
    result = mstring_alloc_string(reslen MTRACE_PASS);
    if (result && reslen)
    {
        memcpy(get_txt(result), get_txt((string_t *const)str)+start, reslen);
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
    const char *pp, *ps;
    size_t lp, ls;

    lp = mstrsize(p); pp = get_txt((string_t *const)p);
    ls = mstrsize(s); ps = get_txt((string_t *const)s);

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

    pp = memchr(get_txt((string_t *const)p), c, mstrsize(p));
    if (pp != NULL)
        return pp - get_txt((string_t *const)p);
    return -1;
} /* mstring_chr() */

/*-------------------------------------------------------------------------*/
void
mstring_init (void)

/* Initialize all datastructures and the common strings.
 */

{
    int x;

    stringtable = xalloc(sizeof(*stringtable) * HTABLE_SIZE);

    if (!stringtable)
        fatal("(mstring_init) Out of memory (%lu bytes) for string table\n"
             , (unsigned long) sizeof(*stringtable)*HTABLE_SIZE);

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

    for (x = 0; x < HTABLE_SIZE; x++)
    {
        string_t *p;
        for (p = stringtable[x]; p; p = p->next )
        {
            p->info.ref = 0;
        }
    }

} /* mstring_clear_refs() */

/*-------------------------------------------------------------------------*/
void
mstring_note_refs (void)

/* GC support: note all refs of memory in the string table.
 */

{
    int x;

    note_malloced_block_ref(stringtable);

    for (x = 0; x < SHSTR_NOSTRINGS; x++)
    {
        count_ref_from_string(shstring[x]);
    }
} /* mstring_note_refs() */

/*-------------------------------------------------------------------------*/
void
mstring_walk_table (void (*func) (string_t *))

/* GC support: Call (*func)(str) for all tabled strings in the string table.
 *
 * Usually the function is "mark_unreferenced_string()" which marks
 * unref'd strings in the table, followed by a call to mstring_gc_table().
 */

{
    int x;

    for (x = 0; x < HTABLE_SIZE; x++)
    {
        string_t * p;
        for (p = stringtable[x]; NULL != p; p = p->next)
        {
            (*func)(p);
        }
    }
} /* mstring_walk_table() */

/*-------------------------------------------------------------------------*/
void
mstring_gc_table (void)

/* GC support: Remove all strings from the table which have a refcount
 * of 0.
 *
 * This can only happen in the last stage of a GC.
 */

{
    int x;

    for (x = 0; x < HTABLE_SIZE; x++)
    {
        string_t * prev, * next;
        for (prev = NULL, next = stringtable[x]; next != NULL; )
        {
            if (next->info.ref == 0)
            {
                string_t * this = next;

                /* Unlink the string from the table, then free it. */
                if (prev == NULL)
                {
                    stringtable[x] = this->next;
                    next = this->next;
                }
                else
                {
                    prev->next = this->next;
                    next = this->next;
                }

                mstr_untabled_count++;
                mstr_untabled_size += mstr_mem_size(this);
                mstr_tabled_count--;
                mstr_tabled_size += mstr_mem_size(this);
                mstr_deleted++;

                this->info.ref = 1;
                this->info.tabled = MY_FALSE;
                free_mstring(this);
            }
            else
            {
                /* Step to next string */
                prev = next;
                next = next->next;
            }
        }
    } /* for (x) */
} /* mstring_gc_table() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
mp_int
add_string_status (strbuf_t *sbuf, Bool verbose)

/* Add the string handler status suitable for printing to <sbuf>.
 * Result is the amount of memory held by the string handler.
 */

{
#   define STR_OVERHEAD (sizeof(string_t)+1)

    statcounter_t stringtable_size;
    statcounter_t distinct_strings;
    statcounter_t distinct_size;
    statcounter_t distinct_overhead;

    stringtable_size = HTABLE_SIZE * sizeof(string_t *);
    distinct_strings = mstr_tabled_count + mstr_untabled_count;
    distinct_size = mstr_tabled_size + mstr_untabled_size;
    distinct_overhead = mstr_tabled_count * STR_OVERHEAD
                      + mstr_untabled_count * STR_OVERHEAD;

    if (!verbose)
    {
        strbuf_addf(sbuf
                   , "Strings alloced\t\t\t%8"PRIuSTATCOUNTER" %9"PRIuSTATCOUNTER
                     " (%"PRIuSTATCOUNTER" + %"PRIuSTATCOUNTER" overhead)\n"
                   , distinct_strings, distinct_size + stringtable_size
                   , distinct_size - distinct_overhead
                   , distinct_overhead + stringtable_size
                   );
    }
    else
    {
        strbuf_add(sbuf, "\nString handler:\n");
        strbuf_add(sbuf,   "---------------\t  Strings     Bytes (Data+Overhead)\n");
        strbuf_addf(sbuf,  "Total asked for\t%9"PRIuMPINT" %9"PRIuMPINT
                           " (%9"PRIuMPINT"+%9"PRIuMPINT")\n"
                        , mstr_used
                        , mstr_used_size
                        , mstr_used_size
                          ? mstr_used_size - mstr_used * STR_OVERHEAD
                          : 0
                        , mstr_used * STR_OVERHEAD
                        );
        strbuf_addf(sbuf,  "Total allocated\t%9"PRIuSTATCOUNTER" %9"PRIuSTATCOUNTER
                           " (%9"PRIuSTATCOUNTER"+%9"PRIuSTATCOUNTER")\n"
                        , distinct_strings
                        , distinct_size + stringtable_size
                        , distinct_size - distinct_overhead
                        , distinct_overhead + stringtable_size
                        );
        strbuf_addf(sbuf,  " - tabled\t%9"PRIuMPINT" %9"PRIuSTATCOUNTER" (%9"PRIuMPINT"+%9"PRIuSTATCOUNTER")\n"
                        , mstr_tabled_count
                        , mstr_tabled_size + stringtable_size
                        , mstr_tabled_size
                          ? mstr_tabled_size - mstr_tabled_count * STR_OVERHEAD
                          : 0
                        , mstr_tabled_count * STR_OVERHEAD + stringtable_size
                        );
        strbuf_addf(sbuf,  " - untabled\t%9"PRIuMPINT" %9"PRIuMPINT" (%9"PRIuMPINT"+%9"PRIuMPINT")\n"
                        , mstr_untabled_count
                        , mstr_untabled_size
                        , mstr_untabled_size
                          ? mstr_untabled_size - mstr_untabled_count * STR_OVERHEAD
                          : 0
                        , mstr_untabled_count * STR_OVERHEAD
                        );
        strbuf_addf(sbuf, "\nSpace required vs. 'regular C' string implementation: "
                          "%"PRIuSTATCOUNTER"%% with, %"PRIuSTATCOUNTER"%% without overhead.\n"
                        , ((distinct_size + stringtable_size) * 100L)
                          / (mstr_used_size - mstr_used * sizeof(string_t))
                        , ((distinct_size + stringtable_size
                                          - distinct_overhead) * 100L)
                          / (mstr_used_size - mstr_used * STR_OVERHEAD)
                        );
        strbuf_addf(sbuf, "Searches by address: %"PRIuSTATCOUNTER" - found: %"PRIuSTATCOUNTER" (%.1f%%) - avg length: %7.3f\n"
                        , mstr_searches
                        , mstr_found, 100.0 * (float)mstr_found / (float)mstr_searches
                        , (float)mstr_searchlen / (float)mstr_searches
                        );
        strbuf_addf(sbuf, "Searches by content: %"PRIuSTATCOUNTER" - found: %"PRIuSTATCOUNTER" (%.1f%%) - avg length: %7.3f\n"
                        , mstr_searches_byvalue
                        , mstr_found_byvalue, 100.0 * (float)mstr_found_byvalue / (float)mstr_searches_byvalue
                        , (float)mstr_searchlen_byvalue / (float)mstr_searches_byvalue
                        );
        strbuf_addf(sbuf, "Hash chains used: %"PRIuMPINT" of %lu (%.1f%%)\n"
                        , mstr_chains, (unsigned long)HTABLE_SIZE
                        , 100.0 * (float)mstr_chains / (float)HTABLE_SIZE
                        );
        strbuf_addf(sbuf, "Distinct strings added: %"PRIuSTATCOUNTER" "
                          "- deleted: %"PRIuSTATCOUNTER"\n"
                        , mstr_added, mstr_deleted
                        );
        strbuf_addf(sbuf, "Collisions: %"PRIuSTATCOUNTER" (%.1f%% added)\n"
                        , mstr_collisions
                        , 100.0 * (float)mstr_collisions / (float)mstr_added
                        );
#ifdef EXT_STRING_STATS
        strbuf_addf(sbuf, "Equality tests: %"PRIuSTATCOUNTER" total, %"PRIuSTATCOUNTER" by table (%.1f%%), %"PRIuSTATCOUNTER" by hash (%.1lf%%)\n"
                        , stNumEqual, stNumTabledEqual
                        , stNumEqual ? 100.0 * ((float)stNumTabledEqual/stNumEqual) : 0.0
                        , stNumHashEqual
                        , stNumEqual ? 100.0 * ((float)stNumHashEqual/stNumEqual) : 0.0
                        );
        strbuf_addf(sbuf, "Comparisons:    %"PRIuSTATCOUNTER" total, %"PRIuSTATCOUNTER" by table (%.1f%%)\n"
                        , stNumComp, stNumTabledComp
                        , stNumComp ? 100.0 * ((float)stNumTabledComp/stNumComp) : 0.0
                        );
        strbuf_addf(sbuf, "Table lookups for existence: %"PRIuSTATCOUNTER","
                          " %"PRIuSTATCOUNTER" by table (%.1f%%),"
                          " %"PRIuSTATCOUNTER" by content (%.1f%%)\n"
                        , stNumTabledChecked
                        , stNumTabledCheckedTable
                        , stNumTabledChecked ? 100.0 * ((float)stNumTabledCheckedTable/stNumTabledChecked) : 0.0
                        , stNumTabledCheckedSearch
                        , stNumTabledChecked ? 100.0 * ((float)stNumTabledCheckedSearch/stNumTabledChecked) : 0.0
                        );
#endif /* EXT_STRING_STATS */
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
    ST_NUMBER(DID_ST_STR_OVERHEAD, sizeof(string_t)-1);

    ST_NUMBER(DID_ST_STR_CHAINS,     mstr_chains);
    ST_NUMBER(DID_ST_STR_ADDED,      mstr_added);
    ST_NUMBER(DID_ST_STR_DELETED,    mstr_deleted);
    ST_NUMBER(DID_ST_STR_COLLISIONS, mstr_collisions);

    ST_NUMBER(DID_ST_UNTABLED,      mstr_untabled_count);
    ST_NUMBER(DID_ST_UNTABLED_SIZE, mstr_untabled_size);
    ST_NUMBER(DID_ST_TABLED,        mstr_tabled_count);
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
