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
 *        size_t size;            Length of the string
 *        char   txt[1.. .size];
 *        char   null             Gratuituous terminator
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
 * TODO: If indirectly tabled strings are not used, think about merging
 * TODO:: string_data_t and string_t.
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
#    define StrHash(s,siz) (whashmem((s), siz, 100) & ((HTABLE_SIZE)-1))
#else
#    define StrHash(s,siz) (whashmem((s), siz, 100) % HTABLE_SIZE)
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
make_new_tabled (const char * const pTxt, size_t size, int index MTRACE_DECL)

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

    sdata = xalloc_traced(size + sizeof(*sdata) MTRACE_PASS);
    if (!sdata)
        return NULL;

    string = xalloc_traced(sizeof(*string) MTRACE_PASS);
    if (!string)
    {
        xfree(sdata);
        return NULL;
    }

    /* Set up the structures and table the string */

    sdata->size = size;
    memcpy(sdata->txt, pTxt, size);
    sdata.txt[size] = '\0';

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

    sdata = xalloc_traced(iSize + sizeof(*sdata) MTRACE_PASS);
    if (!sdata)
        return NULL;

    string = xalloc_traced(sizeof(*string) MTRACE_PASS);
    if (!string)
    {
        xfree(sdata);
        return NULL;
    }

    /* Set up the structures */
    sdata->size = iSize;
    sdata->txt[iSize] = '\0';
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
mstring_new_string (const char * const pTxt MTRACE_DECL)

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
        memcpy(string->str->txt, pTxt, size);
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
    return make_new_tabled(pTxt, size, index MTRACE_PASS);
} /* mstring_new_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_make_tabled (string_t * pStr, Bool deref_arg MTRACE_DECL)

/* Aliased to: make_tabled(pStr), make_tabled_from(pStr)
 *
 * Take the string <pStr> and create resp. find a tabled instance of it.
 * Return the counted reference to the tabled instance, and, if <deref_arg>
 * is TRUE, dereference the <pStr> once.
 *
 * The usage should be obvious:
 *   make_tabled() to convert a given string into a tabled one
 *   make_tabled_from() to get a new tabled 'copy' of a given string
 *     without losing the original.
 *
 * If memory runs out, NULL is returned (but, with <deref_arg> set, the
 * original string is still dereferenced).
 */

{
    int            index;
    size_t         size;
    string_t      *string;

    /* If the string is already tabled directly, our work is done */
    if (pStr->info.tabled)
    {
        if (!deref_arg)
            pStr->info.ref += 1;
        return pStr;
    }

    /* If the string is tabled indirectly, return the directly tabled
     * instance.
     */
    if (pStr->link != NULL)
    {
        string = ref_mstring(pStr->link); /* Must come first! */
        if (deref_arg) free_mstring(pStr);
        return string;
    }

    /* Create a completely new tabled string from the old one */

    size = pStr->str->size;
    index = StrHash(pStr->str->txt, size);

    if (pStr->info.ref == 1 && !make_new)
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

        string = make_new_tabled(pStr->str->txt, size, index MTRACE_PASS);
        if (deref_arg) free_mstring(pStr);
    }

    return string;
} /* mstring_make_tabled() */

/*-------------------------------------------------------------------------*/
string_t *
mstring_table_inplace (string_t * pStr MTRACE_DECL)

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
        string = xalloc_traced(sizeof(*string) MTRACE_PASS);
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
mstring_dup (string_t * pStr MTRACE_DECL)

/* Aliased to: dup_mstring(pStr)
 *
 * If <pStr> is a tabled string or an untabled string with more than one
 * reference, create and return a new untabled string with the same text but
 * just one reference. Otherwise, just return <pStr> with one more reference.
 * If memory runs out, NULL is returned.
 *
 * Purpose is to create an instance of a string which an be freely modified.
 */

{
    string_t *string;

    /* Check for the easy case */
    if (!pStr->info.tabled && pStr->info.ref == 1 && pStr->link == NULL)
        return ref_mstring(pStr);

    /* Otherwise create a new untabled string from the tabled one */

    string = mstring_alloc_string(pStr->str->size MTRACE_PASS);
    if (string)
    {
        memcpy(string->str->txt,  pStr->str->txt, pStr->str->size);
    }

    return string;
} /* mstring_dup() */

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
        return pStr;

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
    string_t *string;
    int       index;

    index = StrHash(pTxt, size);

    return find_and_move(pTxt, size, index);
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
Bool
mstring_equal(const string_t * const pStr1, const string_t * const pStr2) 

/* Aliased to: mstreq(pStr1, pStr2)
 *
 * Compare the two strings <pStr1> and <pStr2> and return TRUE if they
 * have the same content, FALSE otherwise.
 */

{
    if (pStr1 == pStr2)
        return MY_TRUE;
    if (mstr_d_tabled(pStr1) && mstr_i_tabled(pStr2) && pStr2->link == pStr1)
        return MY_TRUE;
    if (mstr_i_tabled(pStr1) && mstr_d_tabled(pStr2) && pStr1->link == pStr2)
        return MY_TRUE;
    if (mstrsize(pStr1) != mstrsize(pStr2))
        return MY_FALSE;

    return (memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr1)) != 0);
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
    if (pStr1 == pStr2)
        return MY_TRUE;
    if (mstr_d_tabled(pStr1) && mstr_i_tabled(pStr2) && pStr2->link == pStr1)
        return MY_TRUE;
    if (mstr_i_tabled(pStr1) && mstr_d_tabled(pStr2) && pStr1->link == pStr2)
        return MY_TRUE;
    if (mstrsize(pStr1) <= mstrsize(pStr2))
        return memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr1));

    return memcmp(get_txt(pStr1), get_txt(pStr2), mstrsize(pStr2));
} /* mstring_compare() */

/*-------------------------------------------------------------------------*/
char *
mstring_mstr_n_str ( const string_t * const pStr, size_t start
                   , const char * const pTxt, size_t len)

/* Aliased to: mstrstr(pStr, pTxt)
 *
/* Find the partial string <pTxt> of <len> bytes (which my contain '\0' as
 * part of the data to be found) inside of <pStr> starting at position <start>
 * and return a pointer to the location found.
 * If not found, return NULL.
 */

{
    char * cp;
    size_t left;
    char   first;

    if (len < 1 || start >= mstrsize(pStr))
        return NULL;

    left = mstrsize(pStr);
    cp = get_txt(pStr)+start;
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
string_t *
mstring_add_slash (const string_t *str MTRACE_DECL)

/* Aliased to: add_slash(str)
 *
 * Create and return a new string with the data of <str> prepended
 * by a slash ('/'). The result string is untabled and has one reference,
 * the old string <str> is not changed.
 */

{
    string_t *tmp;
    char * txt;

    tmp = mstring_alloc_string(mstrsize(str)+1 MTRACE_PASS);
    if (tmp)
    {
        txt = get_txt(tmp);
        *tmp = '/';
        memcpy(tmp+1, get_txt(str), mstrsize(str));
    }
    return tmp;
} /* mstring_add_slash() */

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
    char * txt;

    lleft = mstrsize(left);
    lright = mstrsize(right);
    tmp = mstring_alloc_string(lleft+lright MTRACE_PASS);
    if (tmp)
    {
        txt = get_txt(tmp);
        memcpy(tmp, get_txt(left), lleft);
        memcpy(tmp+lleft, get_text(right), lright);
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
        memcpy(tmp, get_txt(left), lleft);
        memcpy(tmp+lleft, right, len);
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
        memcpy(tmp, left, len);
        memcpy(tmp+len, get_txt(right), lright);
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
    if (result && len)
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

/* Aliased to: mstr_extract(str,start,len)
 *
 * Create and return a new string made of <str>[<start>..<end>].
 * If <end> is negative, the result is made of <str>[<start>..].
 * The result string is untabled and has one reference,
 * the old string <str> is not changed.
 *
 * If memory runs out, NULL is returned.
 */

{
    size_t len;
    string_t *result;

    len = mstrsize(str);
    if (len)
    {
        error("(mstring_extract) Can't extract from empty string.\n");
        /* NOTREACHED */
        return NULL;
    }

    if (end < 0)
        end = (long)len-1;

    if (end < (long)start)
    {
        error("(mstring_extract) end %ld < start %lu\n"
             , end, (unsigned long) start);
        /* NOTREACHED */
        return NULL;
    }

    if (start >= len)
    {
        error("(mstring_extract) start %lu > string length %lu\n"
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
    svp[DID_ST_ITABLED].u.number       = mstr_itabled;
    svp[DID_ST_ITABLED_SIZE].u.number  = mstr_itabled_size;
    svp[DID_ST_TABLED].u.number        = mstr_tabled;
    svp[DID_ST_TABLED_SIZE].u.number   = mstr_tabled_size;

    svp[DID_ST_STR_SEARCHES].u.number          = mstr_searches;
    svp[DID_ST_STR_SEARCHLEN].u.number         = mstr_searchlen;
    svp[DID_ST_STR_SEARCHES_BYVALUE].u.number  = mstr_searches_byvalue;
    svp[DID_ST_STR_SEARCHLEN_BYVALUE].u.number = mstr_searchlen_byvalue;
} /* string_dinfo_status() */

/***************************************************************************/
