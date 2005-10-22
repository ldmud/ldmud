/*---------------------------------------------------------------------------
 * Mapping handling functions.
 *
 *---------------------------------------------------------------------------
 * TODO: Rewrite the low-level functions (like allocate_mapping()) to return
 * TODO:: failure codes (errno like) instead of throwing errors. In addition,
 * TODO:: provide wrapper functions which do throw error()s, so that every
 * TODO:: caller can handle the errors himself (like the swapper).
 * Mappings, or 'associative arrays', are similar to normal arrays, with
 * the principal difference that they can use every value to index their
 * stored data, whereas arrays only index with integer values. On the
 * other hand this means that in mappings the data is not stored in any
 * particular order, whereas arrays imply an order through their indexing.
 *
 * LPMud mappings in extension allow to store several values for each
 * index value. This behaviour is functionally equivalent to a 'normal'
 * mapping holding arrays as data, but its direct implementation allows
 * certain optimisations.
 *
 * NB: Where strings are used as index value, they are made shared strings.
 *
 *
 * A mapping consists of several structures (defined in mapping.h):
 *
 *  - the mapping_t is the base of all mappings.
 *  - mapping_cond_t holds the condensed entries
 *  - mapping_hash_t holds the hashed entries added since the
 *      creation of the mapping_cond_t block.
 *
 * Using this approach, mappings manage to combine a low memory overhead
 * with fast operation. Both the hashed and the condensed part may
 * be absent.
 *
 * The key values are sorted in principle by (.type, .u.number >> 1,
 * .x.generic), with the exception of closures which have their own sorting
 * order within their .type. For values which don't have a secondary
 * information, x.generic is set to .u.number << 1 - which also makes sure
 * that the lowest bit of T_NUMBERs is considered.
 *
 * The mapping_cond_t block holds mapping entries in sorted order.
 * Deleted entries are signified by a T_INVALID key value and can appear
 * out of order. The data values for a deleted entry are set to svalue-0.
 *
 * The mapping_hash_t block is used to record all the new additions to
 * the mapping since the last compaction. The new entries' data is kept
 * directly in the hash entries. The hash table grows with the
 * number of hashed entries, so that the average chain length is
 * no more than 2. For easier computations,the number of buckets
 * is always a power of 2.
 *
 * All mappings with a mapping_hash_t structure are considered 'dirty'
 * (and vice versa, only 'dirty' mappings have a mapping_hash_t)
 * and kept in a singly-linked list. The backend (or the garbage collector)
 * calls in regular intervals the function compact_mappings(), which
 * traverses the dirty list and 'cleans' the mappings by sorting the
 * hashed entries into the condensed part, removing the hashed part by
 * this.
 *
 * Mappings maintain two refcounts: the main refcount for all references,
 * and in the hash structure a protector refcount for references as
 * PROTECTED_MAPPING. The latter references are used for mappings which are
 * passed fully or in part as a reference to a function. As long as the
 * protector refcount is not 0, all entry deletions are not executed
 * immediately. Instead, the 'deleted' entries are kept in a separate list
 * until all protective references are removed.
 *
 *
 * -- mapping_t --
 *
 *   mapping_t {
 *       p_int           ref;
 *       wiz_list_t    * user;
 *       int             num_values;
 *       p_int           num_entries;
 *
 *       mapping_cond_t * cond;
 *       mapping_hash_t * hash;
 *   }
 *
 *   .ref is the number of references, as usual.
 *
 *   .user is, as usual, the wizlist entry of the owner object.
 *
 *   .num_values and .num_entries give the width (excluding the key!)
 *   and number of valid entries in the mapping.
 *
 *   .cond and .hash are the condensed resp. hashed data blocks.
 *   .hash also serves as indicator if the mapping is 'dirty',
 *   and therefore contains all the information about the dirtyness.
 *   During the garbage collection, .hash is used for a temporary list
 *   of mappings with 'stale' keys (ie keys referencing destructed objects
 *   or lambdas).
 *
 * -- mapping_cond_t --
 *
 *   mapping_cond_t {
 *       size_t    size;
 *       svalue_t *data[(mapping->num_values+1) * .size];
 *   }
 *
 *   This structure holds the .size compacted entries for a mapping (.size
 *   includes the deleted entries as well, if any).
 *
 *   The first .size svalues in .data[] are the keys. Follwing are the
 *   actual data values, the values for one entry each in one row.
 *
 *   If a key is .data[ix], its data values are in
 *   .data[.size + ix * mapping->num_values] through
 *   .data[.size + (ix+1) * mapping->num_values - 1].
 *
 *   If an entry is deleted, the key's .type is set to T_INVALID and
 *   the data values are zeroed out (and mapping->hash->cond_deleted is
 *   incremented), but the entry is otherwise left in place.
 *
 * -- mapping_hash_t --
 *
 *   hash_mapping_t {
 *       p_int        mask;
 *       p_int        used;
 *       p_int        cond_deleted;
 *       p_int        ref;
 *       map_chain_t *deleted;
 *       mapping_t   *next_dirty;
 *       map_chain_t *chains[ 1 +.mask ];
 *   }
 *
 *   This structure keeps track of the changes to a mapping. Every mapping
 *   with a hash part is considered 'dirty' and kept in the dirty mapping
 *   list. The list is linked through the .next_dirty pointer.
 *
 *   New entries to the mapping are kept in the hashtable made up by
 *   .chains[]. There are .mask+1 different chains, with .mask+1 always
 *   being a power of two. This way, .mask can be used in a binary-&
 *   operation to convert a hash value into a chain index. The number
 *   of entries in the hashtable is listed in .used.
 *
 *   The driver imposes an upper limit onto the average length of the
 *   chains: if the average length exceeds two elements, the size of
 *   the hashtable is doubled (by reallocating the hash_mapping structure).
 *   This is the reason why you can allocate a mapping with a given 'size':
 *   it reduces the number of reallocations in the long run.
 *
 *   .condensed_deleted gives the number of deleted entries in
 *   the mappings condensed_part.
 *
 *   .ref and .deleted come into use when the mapping is used as
 *   protector mapping. Protector mappings are necessary whenever
 *   single values of the mapping are used as lvalues, in order to
 *   protect them against premature deletion ( map[0] = ({ map=0 })
 *   being the classic case). .ref counts the number of such
 *   protective references, and is always <= mapping.ref. .deleted
 *   is the list of entries deleted from the mapping while the
 *   protection is in effect. If the .ref falls back to 0, all
 *   the pending deletions of the .deleted entries are performed.
 *
 * -- map_chain_t --
 *
 *   This structure is used to keep single entries in the hash chains
 *   of hash_mapping, and occasionally, in the .deleted list of
 *   protector mappings.
 *
 *   map_chain_t {
 *       map_chain_t *next;
 *       svalue_t data[ mapping->num_values+1 ];
 *   }
 *
 *   .next is the next struct map_chain in the hash chain (or .deleted list).
 *   .data holds the key and it's data values.
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stdio.h>

#include "mapping.h"

#include "array.h"
#include "backend.h"
#include "closure.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "smalloc.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/
/* Types */

/* The local typedefs */
typedef struct map_chain_s    map_chain_t;


/* --- struct map_chain_s: one entry in a hash chain ---
 *
 * The hashed mapping entries.
 */

struct map_chain_s {
    map_chain_t * next;  /* next entry */
    svalue_t      data[1 /* +mapping->num_values */];
      /* [0]: the key, [1..]: the data */
};

#define SIZEOF_MCH(mch, nv) ( \
    sizeof(*mch) + (nv) * sizeof(svalue_t) \
                            )
  /* Allocation size of a map_chain_t for <nv> values per key.
   */


/*-------------------------------------------------------------------------*/

#define EMPTY_MAPPING_THRESHOLD 2000
  /* Number of 'freed' empty mappings allowed in the dirty list
   * at any time. This way the dirty list can be single-linked only
   * and still allow fast 'freeing' of unused mappings.
   * TODO: Make the list doubly-linked - the memory savings are irrelevant.
   */

static mapping_hash_t dirty_mapping_head_hash;
  /* Auxiliary structure dirty_mapping_head can reference
   */

static mapping_t dirty_mapping_head
  = {
    /* ref         */ 1,
    /* user        */ NULL,
    /* num_values  */ 0,
    /* num_entries */ 0,
    /* cond        */ NULL,
    /* hash        */ &dirty_mapping_head_hash,
    };
  /* Head of the list of (potentially) dirty mappings, ie. mappings
   * with a hash_mapping part.
   */

static mapping_t *last_dirty_mapping = &dirty_mapping_head;
  /* Last dirty mapping in the list.
   */

mp_int num_dirty_mappings = 0;
  /* Number of dirty mappings (excluding the head) in the list.
   */

mp_int num_mappings = 0;
  /* Number of allocated mappings.
   */

static mp_int empty_mapping_load = 2*(-EMPTY_MAPPING_THRESHOLD);
  /* The load of the dirty mapping list with empty mappings, computed
   * as the number of empty dirty mappings minus the number of non-empty
   * dirty mappings. If the difference reaches EMPTY_MAPPING_THRESHOLD,
   * the empty mappings are removed by a call to remove_empty_mappings().
   *
   * The true value is computed only when needed - for the most time
   * it is just incremented by 2 whenever a mapping becomes empty+dirty.
   * This is reason for the negative base value: when the value becomes
   * positive, it is time to recompute the true value.
   */

static mp_int empty_mapping_base = 0;
  /* The number of dirty mappings at the time of the last call
   * to remove_empty_mappings(). This value is used to compute
   * the proper value of empty_mapping_load.
   */

mapping_t *stale_mappings;
  /* During a garbage collection, this is a list of mappings with
   * keys referencing destructed objects/lambdas. Since during this
   * phase all mappings are compacted, the list is linked through
   * the .hash pointers.
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void remove_empty_mappings (void);

#if 0

/* TODO: Remove these defines when the statistics prove to be correct */

#define LOG_ALLOC(where,add,alloc) \
printf("DEBUG: %s: m %p user %p total %ld + %ld (alloc %ld) = %ld\n", where, m, m->user, m->user->mapping_total, add, alloc, m->user->mapping_total + (add))

#define LOG_ADD(where,add) \
printf("DEBUG: %s: m %p user %p total %ld + %ld = %ld\n", where, m, m->user, m->user->mapping_total, add, m->user->mapping_total + (add))

#define LOG_SUB(where,sub) \
printf("DEBUG: %s: m %p user %p total %ld - %ld = %ld\n", where, m, m->user, m->user->mapping_total, sub, m->user->mapping_total - (sub))

#define LOG_SUB_M(where,m,sub) \
printf("DEBUG: %s: m %p user %p total %ld - %ld = %ld\n", where, (m), (m)->user, (m)->user->mapping_total, sub, (m)->user->mapping_total - (sub))

#else

#define LOG_ALLOC(where,add,alloc)
#define LOG_ADD(where,add)
#define LOG_SUB(where,add)
#define LOG_SUB_M(where,m,add)

#endif

/*-------------------------------------------------------------------------*/
static INLINE map_chain_t *
new_map_chain (mapping_t * m)

/* Return a fresh map_chain_t for mapping <m>.
 * The .data[] values are not initialised.
 *
 * Return NULL if out of memory.
 */

{
    map_chain_t *rc;

    rc = xalloc(SIZEOF_MCH(rc, m->num_values));
    if (rc)
    {
        LOG_ALLOC("new_map_chain", SIZEOF_MCH(rc, m->num_values), SIZEOF_MCH(rc, m->num_values));
        m->user->mapping_total += SIZEOF_MCH(rc, m->num_values);
    }

    return rc;
} /* new_map_chain() */

/*-------------------------------------------------------------------------*/
static INLINE void
free_map_chain (mapping_t * m, map_chain_t *mch, Bool no_data)

/* Free the map_chain <mch> of mapping <m>.
 * If <no_data> is TRUE, the svalues themselves are supposed to be empty.
 */

{
    p_int ix;

    if (!no_data)
    {
        for (ix = m->num_values; ix >= 0; ix--)
        {
            free_svalue(mch->data+ix);
        }
    }

    LOG_SUB("free_map_chain", SIZEOF_MCH(mch, m->num_values));
    m->user->mapping_total -= SIZEOF_MCH(mch, m->num_values);
    xfree(mch);
} /* free_map_chain() */

/*-------------------------------------------------------------------------*/
static INLINE mapping_hash_t *
get_new_hash ( mapping_t *m, mp_int hash_size)

/* Allocate a new hash structure for mapping <m>, prepared to take
 * <hash_size> entries. The hash structure is NOT linked into <m>.
 *
 * Return the new structure, or NULL when out of memory.
 */

{
    mapping_hash_t *hm;
    map_chain_t **mcp;

    /* Compute the number of hash buckets to 2**k, where
     * k is such that 2**(k+1) > size >= 2**k.
     *
     * To do this, compute 'size' to (2**k)-1 by first setting
     * all bits after the leading '1' and then shifting the
     * number right once. The result is then also the mask
     * required for indexing.
     */
    hash_size |= hash_size >> 1;
    hash_size |= hash_size >> 2;
    hash_size |= hash_size >> 4;
    if (hash_size & ~0xff)
    {
        hash_size |= hash_size >> 8;
        hash_size |= hash_size >> 16;
    }
    hash_size >>= 1;

    /* Allocate the hash_mapping big enough to hold (size+1) hash
     * buckets.
     * size must not exceed the accessible indexing range. This is
     * a possibility because size as a mp_int may have a different
     * range than array indices which are size_t.
     * TODO: The 0x100000 seems to be a safety offset, but is it?
     */
    if (hash_size > (mp_int)((MAXINT - sizeof *hm - 0x100000) / sizeof *mcp)
     || !(hm = xalloc(sizeof *hm + sizeof *mcp * hash_size) ) )
    {
        return NULL;
    }

    hm->mask = hash_size;
    hm->used = hm->cond_deleted = hm->ref = 0;

    /* These members don't really need a default initialisation
     * but it's here to catch bogies.
     */
    hm->next_dirty = NULL;
    hm->deleted = NULL;

    /* Initialise the hashbuckets (there is at least one) */
    mcp = hm->chains;
    do *mcp++ = NULL; while (--hash_size >= 0);

    LOG_ALLOC("get_new_hash", SIZEOF_MH(hm), sizeof *hm + sizeof *mcp * hm->mask);
    m->user->mapping_total += SIZEOF_MH(hm);

    return hm;
} /* get_new_hash() */

/*-------------------------------------------------------------------------*/
static INLINE void
new_dirty_mapping (mapping_t *m)

/* This mapping <m> just became dirty - insert it into the dirty
 * mapping list.
 */

{
    /* With this hash_mapping structure, the mapping counts
     * as potentially dirty.
     */
    last_dirty_mapping->hash->next_dirty = m;
    last_dirty_mapping = m;

    num_dirty_mappings++;

    /* Inform backend that there is a new mapping to condense */
    extra_jobs_to_do = MY_TRUE;
} /* new_dirty_mapping() */

/*-------------------------------------------------------------------------*/
static mapping_t *
get_new_mapping ( wiz_list_t * user, mp_int num_values
                , mp_int hash_size, mp_int cond_size)

/* Allocate a basic mapping with <num_values> values per key, and set it
 * up to have an initial datablock of <data_size> entries, a hash
 * suitable for <hash_size> entries, and a condensed block for <cond_size>
 * entries.
 *
 * The .user is of the mapping is set to <user>.
 *
 * Return the new mapping, or NULL when out of memory.
 */

{
    mapping_cond_t *cm;
    mapping_hash_t *hm;
    mapping_t *m;
/* DEBUG: */  size_t cm_size;

    /* Check if the new size is too big */
    if (num_values > 0)
    {
        if (num_values > SSIZE_MAX /* TODO: SIZET_MAX, see port.h */
         || (   num_values != 0 
             && (SSIZE_MAX - sizeof(map_chain_t)) / num_values < sizeof(svalue_t))
           )
            return NULL;
    }

    /* Allocate the structures */
    m = xalloc(sizeof *m);
    if (!m)
        return NULL;

    m->user = user; /* Already needed for statistics */

    /* Set up the key block for <cond_size> entries */

    cm = NULL;
    if (cond_size > 0)
    {

        /* size_t */ cm_size = (size_t)cond_size;
        cm = xalloc(sizeof(*cm) + sizeof(svalue_t) * cm_size * (num_values+1) - 1);
        if (!cm)
        {
            xfree(m);
            return NULL;
        }

        cm->size = cm_size;
    }

    /* Set up the hash block for <hash_size> entries.
     * Do this last because get_new_hash() modifies the statistics.
     */

    hm = NULL;
    if (hash_size > 0)
    {
        hm = get_new_hash(m, hash_size);
        if (!hm)
        {
            if (cm) xfree(cm);
            xfree(m);
            return NULL;
        }

        /* With this hash_mapping structure, the mapping counts
         * as potentially dirty.
         */
        new_dirty_mapping(m);
    }

    /* Initialise the mapping */

    m->cond = cm;
    m->hash = hm;
    m->num_values = num_values;
    m->num_entries = 0;
    m->ref = 1;

    /* Statistics */
    LOG_ADD("get_new_mapping - base", sizeof *m);
    m->user->mapping_total += sizeof *m;
    if (cm)
    {
        LOG_ALLOC("get_new_mapping - cond", SIZEOF_MC(cm, num_values), sizeof(*cm) + sizeof(svalue_t) * cm_size * (num_values+1) - 1);
        m->user->mapping_total += SIZEOF_MC(cm, num_values);
    }
    /* hm has already been counted */

    num_mappings++;

    return m;

} /* get_new_mapping() */

/*-------------------------------------------------------------------------*/
mapping_t *
allocate_mapping (mp_int size, mp_int num_values)

/* Allocate a mapping with <num_values> values per key, and setup the
 * hash part for (initially) <size> entries. The condensed part will
 * not be allocated.
 *
 * Return the new mapping, or NULL when out of memory.
 */

{
    return get_new_mapping(current_object->user, num_values, size, 0);
} /* allocate_mapping() */

/*-------------------------------------------------------------------------*/
mapping_t *
allocate_cond_mapping (wiz_list_t * user, mp_int size, mp_int num_values)

/* Allocate for <user> a mapping with <num_values> values per key, and
 * setup the condensed part for <size> entries. The hash part will not be
 * allocated.
 *
 * The swapper uses this function.
 *
 * Return the new mapping, or NULL when out of memory.
 */

{
    return get_new_mapping(user, num_values, 0, size);
} /* allocate_cond_mapping() */

/*-------------------------------------------------------------------------*/
void
_free_mapping (mapping_t *m, Bool no_data)

/* Aliases: free_mapping(m)       -> _free_mapping(m, FALSE)
 *          free_empty_mapping(m) -> _free_mapping(m, TRUE)
 *
 * The mapping and all associated memory is deallocated resp. dereferenced.
 *
 * If <no_data> is TRUE, all the svalues are assumed to be freed already
 * (the swapper uses this after swapping out a mapping). The function still
 * will deallocate any map_chain entries, if existing.
 *
 * If the mapping is 'dirty' (ie. contains a hash_mapping part), it
 * is not deallocated immediately, but instead counts 1 to the empty_mapping-
 * _load (with regard to the threshold).
 */

{
    mapping_hash_t *hm;  /* Hashed part of <m> */

#ifdef DEBUG
    if (!m)
        fatal("NULL pointer passed to free_mapping().\n");

    if (!m->user)
        fatal("No wizlist pointer for mapping");

    if (!no_data && m->ref > 0)
        fatal("Mapping with %ld refs passed to _free_mapping().\n", m->ref);
#endif

    num_mappings--;

    /* Free the condensed data */
    if (m->cond != NULL)
    {
        p_int left = m->cond->size * (m->num_values + 1);
        svalue_t *data = &(m->cond->data[0]);

        for (; !no_data && left > 0; left--, data++)
            free_svalue(data);

        LOG_SUB("free_mapping cond", SIZEOF_MC(m->cond, m->num_values));
        m->user->mapping_total -= SIZEOF_MC(m->cond, m->num_values);
        xfree(m->cond);
        m->cond = NULL;
    }

    /* If there is a hashed part, free that one, but keep the mapping
     * itself allocated (to not disrupt the dirty-mapping list).
     * Otherwise, just free the mapping.
     */
    if ( NULL != (hm = m->hash) )
    {
        map_chain_t **mcp, *mc, *next;
        mapping_t *next_dirty;
        int i;

#ifdef DEBUG
        if (hm->ref)
            fatal("Ref count in freed hash mapping: %ld\n", hm->ref);
#endif
        LOG_SUB("free_mapping hash", SIZEOF_MH(hm));
        m->user->mapping_total -= SIZEOF_MH(hm);

        mcp = hm->chains;

        /* Loop through all chains */

        i = hm->mask + 1;
        do {

            /* Free this chain */

            for (next = *mcp++; NULL != (mc = next); )
            {
                next = mc->next;
                free_map_chain(m, mc, no_data);
            }
        } while (--i);

        /* Replace this hash_mapping with an empty one and
         * mark the mapping itself as empty.
         */
        next_dirty = hm->next_dirty;
        xfree(hm);

        hm = get_new_hash(m, 0);
        hm->next_dirty = next_dirty;

        m->hash = hm;
        m->num_entries = 0;

        /* Count this new empty mapping. If the load becomes positive,
         * we have freed 200 mappings since the last call to
         * remove_empty_mappings(). If that happens, call the function
         * to at least recompute the true _load.
         *
         * '+2' in order to offset implicite '-1' caused by
         * the mere existance of this dirty mapping.
         */
        if ( (empty_mapping_load += 2) > 0)
            remove_empty_mappings();
    }
    else
    {
        /* No hash: free the base structure.
         */

        LOG_SUB("free_mapping base", sizeof(*m));
        m->user->mapping_total -= sizeof(*m);
        xfree(m);
    }
} /* free_mapping() */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

static void
check_dirty_mapping_list (void)

/* Check the list of dirty mappings for consistency, generating a fatal()
 * error if not.
 */

{
    int i;
    mapping_t *m;

    for (m = &dirty_mapping_head, i = num_dirty_mappings; m && --i >= 0; )
    {
        m = m->hash->next_dirty;
    }
    if (!m)
        fatal("expected %ld dirty mappings, found only %ld\n"
             , (long)num_dirty_mappings, (long)num_dirty_mappings - i
             );
    if (m != last_dirty_mapping)
        fatal("last_dirty_mapping not at end of list of %ld dirty mappings\n"
             , (long)num_dirty_mappings
             );
    if (m->hash->next_dirty)
        fatal("list of %ld dirty mapping list\n"
             , (long)num_dirty_mappings
             );
}

#endif

/*-------------------------------------------------------------------------*/
static void
remove_empty_mappings (void)

/* Weight the changes in the number of dirty mappings against
 * the number of empty mappings. If it crosses the threshhold, remove
 * the empty mappings from the list.
 */

{
    mapping_t **mp, *m, *last;
    mapping_hash_t *hm;

    /* Since the last call, only empty dirty mappings were counted in
     * the empty_mapping_load. Offset this with the total number
     * of dirty mappings in order to get the real load.
     */
    empty_mapping_load += empty_mapping_base - num_dirty_mappings;
    empty_mapping_base = num_dirty_mappings;

    if (empty_mapping_load <= -EMPTY_MAPPING_THRESHOLD)
        return;

    /* At this point we know that we have EMPTY_MAPPING_THRESHOLD
     * empty free mappings in the dirty list.
     */

#ifdef DEBUG
    /* We have stored all these superflous zeroes.
     * Now check that there is one in the proper place.
     */
    if (last_dirty_mapping->hash->next_dirty != 0)
        fatal("Dirty mapping list not terminated\n");
#endif

    last_dirty_mapping->hash->next_dirty = 0;

    /* Walk the dirty mapping list, deallocating pending
     * empty mappings
     */
    last = &dirty_mapping_head;
    mp = &dirty_mapping_head_hash.next_dirty;
    m = *mp;
    do {
        hm = m->hash;
        if (!m->ref && !m->num_entries)
        {
            LOG_SUB("remove_empty_mappings", (sizeof(*m) + SIZEOF_MH(hm)));
            m->user->mapping_total -= sizeof(*m) + SIZEOF_MH(hm);
            xfree(m);
            *mp = m = hm->next_dirty;
            xfree(hm);
            num_dirty_mappings--;
            continue;
        }
        last = m;
        mp = &hm->next_dirty;
        m = *mp;
    } while (m);
    last_dirty_mapping = last;


    /* Adjust the counters */

    empty_mapping_load = 2*(-EMPTY_MAPPING_THRESHOLD) - empty_mapping_base;

#ifdef DEBUG
    check_dirty_mapping_list();
#endif

} /* remove_empty_mappings() */

/*-------------------------------------------------------------------------*/
void
free_protector_mapping (mapping_t *m)

/* Free the mapping <m> which is part of a T_PROTECTOR_MAPPING svalue.
 * Such svalues are created only for mappings with a hashed part, and
 * have the ref of the hashed part incremented at creation.
 *
 * This function is a wrapper around free_mapping() and takes care
 * to free m->hash->deleted if m->hash->ref reaches zero due to this
 * call.
 */

{
    mapping_hash_t *hm;

#ifdef DEBUG
    /* This type of mapping must have a hash part */

    if (!m->hash || m->hash->ref <= 0)
    {
        /* This shouldn't happen */
        printf("%s free_protector_mapping() : no hash %s\n"
              , time_stamp(), m->hash ? "reference" : "part");
#ifdef TRACE_CODE
        {
            last_instructions(TOTAL_TRACE_LENGTH, MY_TRUE, NULL);
        }
#endif
        dump_trace(MY_FALSE, NULL);
        printf("%s free_protector_mapping() : no hash %s\n"
              , time_stamp(), m->hash ? "reference" : "part");
        free_mapping(m);
    }
#endif /* DEBUG */


    /* If this was the last protective reference, execute
     * the pending deletions.
     */

    if (!--(hm = m->hash)->ref)
    {
        map_chain_t *mc, *next;

        for (mc = hm->deleted; mc; mc = next)
        {
            next = mc->next;
            free_map_chain(m, mc, MY_FALSE);
        }

        hm->deleted = NULL;
    }

    /* Call free_mapping() if appropriate */

    free_mapping(m);

} /* free_protector_mapping() */

/*-------------------------------------------------------------------------*/
static INLINE p_int
mcompare (svalue_t * left, svalue_t * right)

/* Compare the two svalues *<left> and *<right>, using the mapping
 * sorting order, and return:
 *   -1: <left> is smaller than <right>
 *    0: <left> and <right> are equal
 *    1: <left> is bigger than <right>
 */

{
    int rc;

    rc = left->type - right->type;
    if (rc)
        return rc;

    if (left->type == T_CLOSURE)
        return closure_cmp(left, right);

    /* This comparison works for T_NUMBERS because the missing bit
     * from u.number is stored in x.generic.
     */

    rc = (left->u.number >> 1) - (right->u.number >> 1);
    if (rc)
        return rc;

    return left->x.generic - right->x.generic;
} /* mcompare() */

/*-------------------------------------------------------------------------*/
static INLINE mp_int
mhash (svalue_t * svp)

/* Compute and return the hash value for svalue *<svp>.
 */

{
    mp_int i;

    if (svp->type != T_CLOSURE
     || (   svp->x.closure_type != CLOSURE_LFUN
         && svp->x.closure_type != CLOSURE_ALIEN_LFUN
         && svp->x.closure_type != CLOSURE_IDENTIFIER )
       )
    {
        i = svp->u.number ^ *SVALUE_FULLTYPE(svp);
    }
    else
    {
        i = (p_int)(svp->u.lambda->ob) ^ *SVALUE_FULLTYPE(svp);
    }

    i = i ^ i >> 16;
    i = i ^ i >> 8;

    return i;
} /* mhash() */

/*-------------------------------------------------------------------------*/
static svalue_t *
find_map_entry ( mapping_t *m, svalue_t *map_index
               , p_int * pKeys, map_chain_t ** ppChain
               )

/* Index mapping <m> with key value <map_index> and if found, return a pointer
 * to the entry block for this key (ie. the result pointer will point to
 * the stored key value).
 * If the key was found in the condensed data, *<pKeys> will be set
 * to key index; otherwise *<ppChain> will point to the hash map chain entry.
 * The 'not found' values for the two variables are -1 and NULL resp.
 *
 * If the key is not found, NULL is returned.
 *
 * Sideeffect: if <map_index> is an unshared string, it is made shared.
 *   Also, <map_index>.x.generic information is generated for types
 *   which usually have none.
 */

{
    *pKeys = -1;
    *ppChain = NULL;

    /* If the key is a string, make it tabled */
    if (map_index->type == T_STRING && !mstr_tabled(map_index->u.str))
    {
        map_index->u.str = make_tabled(map_index->u.str);
    }

    /* Generate secondary information for types which usually
     * have none.
     */
    if (map_index->type == T_STRING
     || (   map_index->type != T_CLOSURE
         && map_index->type != T_FLOAT
         && map_index->type != T_SYMBOL
         && map_index->type != T_QUOTED_ARRAY
        )
       )
        map_index->x.generic = (short)(map_index->u.number << 1);

    /* Search in the condensed part first.
     */

    if (m->cond && m->cond->size != 0)
    {
        mapping_cond_t *cm = m->cond;
        mp_int size = cm->size;
        svalue_t *key, * keystart, * keyend;

        keystart = &cm->data[0];
        keyend = keystart + size;

        /* Skip eventual deleted entries at start or end */
        while (size > 0 && keystart->type == T_INVALID)
        {
            keystart++;
            size--;
        }

        while (size > 0 && keyend[-1].type == T_INVALID)
        {
            keyend--;
            size--;
        }

        while (keyend > keystart)
        {
            int cmp;

            key = (keyend - keystart) / 2 + keystart;

            while (key > keystart && key->type == T_INVALID)
                key--;

            cmp = mcompare(map_index, key);
            
            if (cmp == 0)
            {
                /* Found it */
                *pKeys = (p_int)(key - &(cm->data[0]));
                return key;
            }

            if (cmp > 0)
            {
                /* The map_index value is after key */
                for ( keystart = key+1
                    ; keystart < keyend && keystart->type == T_INVALID
                    ; keystart++)
                  NOOP;
            }
            else
            {
                /* The map_index value is before key */
                for ( keyend = key
                    ; keystart < keyend && keyend[-1].type == T_INVALID
                    ; keyend--)
                  NOOP;
            }
        }
    }
    
    /* At this point, the key was not found in the condensed index
     * of the mapping. Try the hashed index next.
     */

    if (m->hash && m->hash->used)
    {
        mapping_hash_t *hm = m->hash;
        map_chain_t *mc;

        mp_int idx = mhash(map_index) & hm->mask;

        /* Look for the value in the chain determined by index */

        for (mc = hm->chains[idx]; mc != NULL; mc = mc->next)
        {
            if (0 == mcompare(&(mc->data[0]), map_index))
            {
                /* Found it */
                *ppChain = mc;
                return &(mc->data[0]);
            }
        }
    }

    /* Not found at all */

    return NULL;
} /* find_map_entry() */

/*-------------------------------------------------------------------------*/
svalue_t *
_get_map_lvalue (mapping_t *m, svalue_t *map_index
                , Bool need_lvalue, Bool check_size)

/* Index mapping <m> with key value <map_index> and return a pointer to the
 * array of values stored for this key.
 *
 * If the mapping does not contains the given index, and <need_lvalue> is
 * false, &const0 is returned. If <need_lvalue> is true, a new key/value
 * entry is created and returned (map_index is assigned for this).
 *
 * If check_size is true and the extension of the mapping would increase
 * its size over max_mapping_size, a runtime error is raised.
 *
 * Return NULL when out of memory.
 *
 * Sideeffect: if <map_index> is an unshared string, it is made shared.
 *   Also, <map_index>.x.generic information is generated for types
 *   which usually have none.
 *
 * For easier use, mapping.h defines the following macros:
 *   get_map_value(m,x)            -> _get_map_lvalue(m,x,false,true)
 *   get_map_lvalue(m,x)           -> _get_map_lvalue(m,x,true,true)
 *   get_map_lvalue_unchecked(m,x) -> _get_map_lvalue(m,x,true,false)
 */

{
    map_chain_t    * mc;
    mapping_hash_t * hm;
    svalue_t       * entry;
    mp_int           idx;

    entry = find_map_entry(m, map_index, (p_int *)&idx, &mc);

    /* If we found the entry, return the values */
    if (entry != NULL)
    {
        if (mc != NULL)
            return entry+1;

        return COND_DATA(m->cond, idx, m->num_values);
    }

    if (!need_lvalue)
        return &const0;

    /* We didn't find key and the caller wants the data.
     * So create a new entry and enter it into the hash index (also
     * created if necessary).
     */

    /* Size limit exceeded? */
    if (check_size && max_mapping_size)
    {
        mp_int msize;

        msize = (mp_int)MAP_SIZE(m);
        if (msize >= max_mapping_size)
        {
            check_map_for_destr(m);
            msize = (mp_int)MAP_SIZE(m);
        }
        if (msize >= max_mapping_size)
        {
            error("Illegal mapping size: %ld\n", msize+1);
            return NULL;
        }
    }

    /* Get the new entry svalues, but don't assign the key value
     * yet - further steps might still fail.
     */
    mc = new_map_chain(m);
    if (NULL == mc)
        return NULL;

    /* If the mapping has no hashed index, create one with just one
     * chain and put the new entry in there.
     */

    if ( !(hm = m->hash) )
    {
        /* Create the hash part of the mapping and put
         * it into the dirty list.
         */

        hm = get_new_hash(m, 1);
        if (!hm)
        {
            free_map_chain(m, mc, MY_TRUE);
            return NULL; /* Oops */
        }
        m->hash = hm;
        new_dirty_mapping(m);

        /* Now insert the map_chain structure into its chain */
        hm->chains[0] = mc;
        mc->next = NULL;
    }
    else
    {

        /* The hashed index exists, so we can insert the new entry there.
         *
         * However, if the average number of map_chains per chain exceeds 2,
         * double the size of the bucket array first.
         */
        if (hm->used & ~hm->mask<<1)
        {
            mapping_hash_t *hm2;
            mp_int size, mask, j;
            map_chain_t **mcp, **mcp2, *next;

            hm2 = hm;

            /* Compute new size and mask, and allocate the structure */

            size = (hm->mask << 1) + 2;
            mask = size - 1;

            hm = xalloc(sizeof *hm - sizeof *mcp + sizeof *mcp * size);
            if (!hm)
            {
                free_map_chain(m, mc, MY_TRUE);
                return NULL;
            }

            /* Initialise the new structure except for the chains */

            *hm = *hm2;
            hm->mask = mask;
            mcp = hm->chains;
            do *mcp++ = NULL; while (--size);

            /* Copy the old chains into the new buckets by rehashing
             * them.
             */
            mcp = hm->chains;
            mcp2 = hm2->chains;
            for (j = hm2->mask + 1; --j >= 0; )
            {
                map_chain_t *mc2;

                for (mc2 = *mcp2++; mc2; mc2 = next)
                {
                    next = mc2->next;
                    idx = mhash(&(mc2->data[0])) & mask;
                    mc2->next = mcp[idx];
                    mcp[idx] = mc2;
                }
            }
            m->hash = hm;

            LOG_ALLOC("get_map_lvalue - existing hash", SIZEOF_MH(hm) - SIZEOF_MH(hm2), sizeof *hm - sizeof *mcp + sizeof *mcp * size);
            m->user->mapping_total += SIZEOF_MH(hm) - SIZEOF_MH(hm2);

            /* Away, old data! */

            xfree(hm2);
        }

        /* Finally, insert the new entry into its chain */

        idx = mhash(map_index) & hm->mask;
        mc->next = hm->chains[idx];
        hm->chains[idx] = mc;
    }

    /* With the new map_chain structure inserted, we can adjust
     * the statistics and copy the key value into the structure.
     */

    assign_svalue_no_free(&(mc->data[0]), map_index);
    for (idx = m->num_values, entry = &(mc->data[1]); idx > 0
        ; idx--, entry++)
        put_number(entry, 0);

    hm->used++;
    m->num_entries++;

    return &(mc->data[1]);
} /* _get_map_lvalue() */

/*-------------------------------------------------------------------------*/
void
check_map_for_destr (mapping_t *m)

/* Check the mapping <m> for references to destructed objects.
 * Where they appear as keys, both key and associated values are
 * deleted from the mapping. Where they appear as values, they are
 * replaced by svalue-0.
 */

{
    int             num_values;
    mapping_cond_t *cm;
    mapping_hash_t *hm;

    num_values = m->num_values;

    /* Scan the condensed part for destructed object references used as keys.
     */

    if (NULL != (cm = m->cond))
    {
        size_t ix;
        svalue_t * entry;

        /* First, scan the keys */
        for (ix = 0, entry = &(cm->data[0]); ix < cm->size; ++ix, ++entry)
        {
            if (T_INVALID == entry->type)
                continue;

            if (destructed_object_ref(entry))
            {
                int i;
                svalue_t * data = COND_DATA(cm, ix, num_values);

                /* Destructed key: remove the whole entry */
                m->num_entries--;

                free_svalue(entry);
                entry->type = T_INVALID;

                for (i = num_values; i > 0; --i, data++)
                {
                    free_svalue(data);
                    put_number(data, 0);
                }

                /* Count the deleted entry in the hash part.
                 * Create it if necessary.
                 */
                if ( !(hm = m->hash) )
                {
                    hm = get_new_hash(m, 0);
                    if (!hm)
                    {
                        outofmem(sizeof *hm, "hash mapping");
                        /* NOTREACHED */
                        return;
                    }
                    m->hash = hm;
                    new_dirty_mapping(m);
                }

                hm->cond_deleted++;

                continue;
            }
        } /* for (all keys) */

        /* Second, scan the values */
        for ( ix = 0, entry = &(cm->data[cm->size])
            ; ix < num_values * cm->size; ++ix, ++entry)
        {
            if (destructed_object_ref(entry))
            {
                assign_svalue(entry, &const0);
            }
        } /* for (all values) */
    } /* if (m->cond) */

    /* If it exists, scan the hash part for destructed objects.
     */

    if ( NULL != (hm = m->hash) )
    {
        map_chain_t **mcp, **mcp2, *mc;
        p_int i, j;

        /* Walk all chains */

        for (mcp = hm->chains, i = hm->mask + 1; --i >= 0;)
        {
            /* Walk this chain */

            for (mcp2 = mcp++; NULL != (mc = *mcp2); )
            {
                /* Destructed object as key: remove entry */

                svalue_t * entry = &(mc->data[0]);

                if (destructed_object_ref(entry))
                {
                    m->num_entries--;

                    *mcp2 = mc->next;

                    /* If the mapping is a protector mapping, move
                     * the entry into the 'deleted' list, else
                     * just deallocate it.
                     */
                    if (hm->ref)
                    {
                        mc->next = hm->deleted;
                        hm->deleted = mc;
                    }
                    else
                    {
                        free_map_chain(m, mc, MY_FALSE);
                    }
                    hm->used--;
                    continue;
                }

                /* Scan the values of this entry (not reached
                 * if the entry was removed above
                 */
                for (entry++, j = num_values; j > 0; --j, ++entry)
                {
                    if (destructed_object_ref(entry))
                    {
                        assign_svalue(entry, &const0);
                    }
                }

                mcp2 = &mc->next;

            } /* walk this chain */
        } /* walk all chains */
    } /* if (hash part exists) */

} /* check_map_for_destr() */

/*-------------------------------------------------------------------------*/
static void
remove_mapping (mapping_t *m, svalue_t *map_index)

/* Remove from mapping <m> that entry which is index by key value
 * <map_index>. Nothing happens if it doesn't exist.
 *
 * Sideeffect: if <map_index> is an unshared string, it is made shared.
 *   Also, <map_index>.x.generic information is generated for types
 *   which usually have none.
 */

{
    p_int            key_ix;
    svalue_t       * entry;
    map_chain_t    * mc;
    mapping_hash_t * hm;
    p_int            num_values;

    num_values = m->num_values;

    entry = find_map_entry(m, map_index, &key_ix, &mc);

    if (NULL != entry)
    {
        /* The entry exists - now remove it */

        m->num_entries--;

        if (key_ix >= 0)
        {
            /* The entry is in the condensed part */
            p_int i;

            free_svalue(entry); entry->type = T_INVALID;
            entry = COND_DATA(m->cond, key_ix, num_values);
            for (i = num_values; i > 0; i--, entry++)
            {
                free_svalue(entry);
                put_number(entry, 0);
            }

            /* Count the deleted entry in the hash part.
             * Create it if necessary.
             */
            if ( !(hm = m->hash) )
            {
                hm = get_new_hash(m, 0);
                if (!hm)
                {
                    outofmem(sizeof *hm, "hash mapping");
                    /* NOTREACHED */
                    return;
                }
                m->hash = hm;
                new_dirty_mapping(m);
            }

            hm->cond_deleted++;
        }
        else if (mc != NULL && NULL != (hm = m->hash))
        {
            /* The key is in the hash mapping */

            map_chain_t *prev, *mc2;
            mp_int idx = mhash(entry) & hm->mask;

            for ( prev = 0, mc2 = hm->chains[idx]
                ; mc2 != NULL && mc2 != mc
                ; prev = mc2, mc2 = mc2->next)
                NOOP;

            if (mc2 == NULL)
                fatal("Mapping entry didn't hash to the same spot.\n");

            /* If the mapping is a protector mapping, move
             * the entry into the 'deleted' list, else
             * just deallocate it.
             */
            if (hm->ref)
            {
                mc->next = hm->deleted;
                hm->deleted = mc;
            }
            else
            {
                if (prev)
                    prev->next = mc->next;
                else
                    hm->chains[idx] = mc->next;

                free_map_chain(m, mc, MY_FALSE);
            }
            hm->used--;
        }
        else
            fatal("Mapping entry found in neither condensed nor hash index.\n");
    }
    /* else the entry wasn't found */

} /* remove_mapping() */

/*-------------------------------------------------------------------------*/
mapping_t *
resize_mapping (mapping_t *m, mp_int new_width)

/* Produce a shallow copy of mapping <m>, adjusted to have
 * <new_width> values per key, and return it.
 * The copy of a protector mapping is a normal mapping.
 *
 * check_map_for_destr(m) should be called before.
 */

{
    mapping_t      * m2;
    mapping_hash_t * hm, *hm2 = NULL;
    mapping_cond_t * cm, *cm2 = NULL;
    mp_int common_width;  /* == min(num_values, new_width) */

    /* Set the width variables */
    if (m->num_values >= new_width)
    {
        common_width = new_width;
    }
    else
    {
        common_width = m->num_values;
    }

    /* Check if the new size is too big */
    if (new_width > 0)
    {
        if (new_width > SSIZE_MAX /* TODO: SIZET_MAX, see port.h */
         || (   new_width != 0
             && (SSIZE_MAX - sizeof(map_chain_t)) / new_width < sizeof(svalue_t))
           )
        {
            error("Mapping width too big (%ld)\n", new_width);
            /* NOTREACHED */
            return NULL;
        }
          
    }

    /* Get the target mapping without a hash, but with a condensed block
     * big enough to hold all entries.
     */
    {
        p_int cm_size = 0;
        if (m->cond)
        {
            cm_size = m->cond->size;
            if (m->hash)
                cm_size -= m->hash->cond_deleted;
        }
        m2 = get_new_mapping(current_object->user, new_width, 0, cm_size);
        if (!m2)
        {
            outofmem(sizeof *m2 + sizeof(svalue_t) * m->num_entries * new_width
                    , "result mapping base structure");
            /* NOTREACHED */
            return NULL;
        }
    }

    /* --- Copy the hash part, if existent ---
     */

    if ( NULL != (hm = m->hash) )
    {
        map_chain_t **mcp, **mcp2;
        mp_int size;

        /* Allocate and initialize the hash structure */

        size = hm->mask + 1;
        hm2 = xalloc(sizeof *hm - sizeof *mcp + sizeof *mcp * size);
        if (!hm2)
        {
            outofmem(sizeof *hm - sizeof *mcp + sizeof *mcp * size, "hash structure");
            /* NOTREACHED */
            return NULL;
        }

        hm2->mask = hm->mask;
        hm2->used = hm->used;
        hm2->cond_deleted = 0;
        hm2->next_dirty = NULL;
        hm2->deleted = NULL;
        hm2->ref = 0;

        /* Now copy the hash chains */

        mcp = hm->chains;
        mcp2 = hm2->chains;
        do {
            map_chain_t *last = NULL, *mc, *mc2;

            for (mc = *mcp++; mc; mc = mc->next)
            {
                svalue_t *src, *dest;
                p_int i;

                mc2 = new_map_chain(m2);
                if (!mc2)
                {
                    xfree(hm2);
                    outofmem(SIZEOF_MCH(mc, new_width), "hash link");
                    /* NOTREACHED */
                    return NULL;
                }

                /* Copy the key and the common values */
                for (src = &(mc->data[0]), dest = &(mc2->data[0]), i = common_width
                    ; i >= 0
                    ; --i, src++, dest++)
                {
                    assign_svalue_no_free(dest, src);
                }

                /* Zero out any extraneous values */
                for (dest = &(mc2->data[common_width+1]), i = new_width - common_width
                    ; i > 0
                    ; --i, dest++)
                {
                    put_number(dest, 0);
                }


                mc2->next = last;
                last = mc2;
            }
            *mcp2++ = last;
        } while (--size);

        /* Plug the new hash into the new mapping */
        m2->hash = hm2;
        LOG_ALLOC("copy_mapping - hash", SIZEOF_MH(hm2), sizeof *hm - sizeof *mcp + sizeof *mcp * size);
        m->user->mapping_total += SIZEOF_MH(hm2);
    }


    /* --- Copy the condensed part ---
     */

    if (NULL != (cm = m->cond) && NULL != (cm2 = m2->cond))
    {
        size_t src_ix;
        svalue_t * src_key, * src_data;
        svalue_t * dest_key, * dest_data;

        for (   src_ix = 0
              , src_key = &(cm->data[0])
              , dest_key = &(cm2->data[0])
              , dest_data = COND_DATA(cm2, 0, new_width)
            ; src_ix < cm->size
            ; src_ix++, src_key++)
        {
            if (src_key->type != T_INVALID)
            {
                p_int i;

                src_data = COND_DATA(cm, src_ix, m->num_values);

                /* Copy the key and the common data */
                assign_svalue_no_free(dest_key++, src_key);
                for (i = common_width; i > 0; i--)
                    assign_svalue_no_free(dest_data++, src_data++);

                /* Zero out any extraneous values */
                for (i = new_width - common_width; i > 0; i--, dest_data++)
                    put_number(dest_data, 0);
            }
        } /* for (all keys) */
    }

    /* --- Finalize the basis structure ---
     */

    if ( NULL != m2->hash )
        new_dirty_mapping(m2);

    m2->num_entries = m->num_entries;

    /* That's it. */
    return m2;
} /* resize_mapping() */

/*-------------------------------------------------------------------------*/
mapping_t *
add_mapping (mapping_t *m1, mapping_t *m2)

/* Merge mappings <m1> and <m2> into a new mapping and return it.
 * Entries from <m2> effectively overwrite entries <m1> if their key
 * matches.
 *
 * If <m1> and <m2> differ in the number of values per entry, return
 * a copy of <m1> if non-empty, else return a copy of <m2>.
 *
 * Return NULL if out of memory.
 *
 * To keep the function fast, the condensed part of m3 is always
 * the sum of the condensed parts of m1 and m2: this allows to operate
 * with static limits. To achieve this, entries from m1
 * overwritten by m2 are counted as cond_deleted entries in m3.
 * We leave it to the later compaction phase to get rid of all these
 * entries - if the mapping is still alive then.
 *
 * Note: The mappings (or at least mapping m2) should not contain destructed
 * objects, ie.  check_map_for_destr() should be called on both mappings
 * before the addition. If this is not done, strange things may happen to your
 * mappings, though the exact reasons are unclear (b-001204).
 */

{
    mp_int      num_values = m1->num_values;
    mapping_t * m3;       /* The result mapping */
    mapping_hash_t * hm;
    p_int cm3size;

    /* Special case: number of values per entry differs.
     * If one of the mappings is empty, the other one is returned.
     * If both mappings contain data, an error is thrown.
     */

    if (m2->num_values != num_values)
    {
        if (!m1->num_entries)
        {
            return copy_mapping(m2);
        }

        if (!m2->num_entries)
        {
            return copy_mapping(m1);
        }

        error("Mappings to be added are of different width: %ld vs. %ld\n"
             , (long)num_values, (long)m2->num_values);
    }


    /* Allocate the result mapping *m3 and initialise it.
     */

    {
        p_int hsize = 1; /* Force the creation of the hash */

        if (m1->hash) hsize += m1->hash->used;
        if (m2->hash) hsize += m2->hash->used;

        cm3size = 0;
        if (m1->cond) cm3size += m1->cond->size;
        if (m2->cond) cm3size += m2->cond->size;

        m3 = get_new_mapping(current_object->user, num_values, hsize, cm3size);

        if (!m3)
        {
            outofmem(sizeof *m3 + sizeof(svalue_t) * hsize * cm3size * num_values
                    , "result mapping base structure");
            /* NOTREACHED */
            return NULL;
        }
    }

    /* Merge the condensed entries.
     * Since the keys are sorted, a simple walk through both mappings
     * in parallel with proper selection does the trick.
     */

    if (NULL != m3->cond)
    {
        mapping_cond_t *cm1, *cm2, *cm3;
        svalue_t *src1_key, *src2_key, *dest_key, *dest_data;
        size_t cm1size, cm2size;
        size_t cm1_ix, cm2_ix, num_entries;

        cm1 = m1->cond;
        cm1size = cm1 ? cm1->size : 0;

        cm2 = m2->cond;
        cm2size = cm2 ? cm2->size : 0;

        cm3 = m3->cond;

        /* Loop over the mappings in parallel */
        for (   cm1_ix = cm2_ix = 0
              , src1_key = cm1 ? &(cm1->data[0]) : NULL
              , src2_key = cm2 ? &(cm2->data[0]) : NULL
              , dest_key = &(cm3->data[0])
              , dest_data = COND_DATA(cm3, 0, num_values)
              , num_entries = 0
            ; cm1_ix < cm1size && cm2_ix < cm2size
            ; NOOP )
        {
            int cmp, i;

            if (src1_key->type == T_INVALID
             || destructed_object_ref(src1_key)
               )
            {
                cm1_ix++;
                src1_key++;
                continue;
            }

            if (src2_key->type == T_INVALID
             || destructed_object_ref(src2_key)
               )
            {
                cm2_ix++;
                src2_key++;
                continue;
            }

            /* Ok, it's a new entry */
            m3->num_entries++;

            cmp = mcompare(src1_key, src2_key);

            if (cmp < 0)
            {
                svalue_t *src_data = COND_DATA(cm1, cm1_ix, num_values);

                /* Copy the key and the values */
                assign_svalue_no_free(dest_key++, src1_key);
                for (i = num_values; i > 0; i--)
                    assign_svalue_no_free(dest_data++, src_data++);

                num_entries++;
                cm1_ix++;
                src1_key++;
            }
            else if (cmp >= 0)
            {
                svalue_t *src_data = COND_DATA(cm2, cm2_ix, num_values);

                /* Copy the key and the values */
                assign_svalue_no_free(dest_key++, src2_key);
                for (i = num_values; i > 0; i--)
                    assign_svalue_no_free(dest_data++, src_data++);

                num_entries++;
                cm2_ix++;
                src2_key++;

                if (cmp == 0)
                {
                    cm1_ix++;
                    src1_key++;
                }
            }
        } /* for(mappings in parallel) */

        /* Copy remaining values from m1 */
        for ( ; cm1_ix < cm1size; cm1_ix++, src1_key++)
        {
            svalue_t *data = COND_DATA(cm1, cm1_ix, num_values);
            int i;

            if (src1_key->type != T_INVALID
             && !destructed_object_ref(src1_key))
            {
                /* Copy the key and the values */
                assign_svalue_no_free(dest_key++, src1_key);
                for (i = num_values; i > 0; i--)
                    assign_svalue_no_free(dest_data++, data++);

                num_entries++;
            }
        } /* for (remaining values in m1) */

        /* Copy remaining values from m2 */
        for ( ; cm2_ix < cm2size; cm2_ix++, src2_key++)
        {
            svalue_t *data = COND_DATA(cm2, cm2_ix, num_values);
            int i;

            if (src2_key->type != T_INVALID
             && !destructed_object_ref(src2_key))
            {
                /* Copy the key and the values */
                assign_svalue_no_free(dest_key++, src2_key);
                for (i = num_values; i > 0; i--)
                    assign_svalue_no_free(dest_data++, data++);

                num_entries++;
            }
        } /* for (remaining values in m2) */

        /* We have now num_entries entries in m3.
         * Any remaining space in cm3 counts as 'deleted', so
         * initialise it accordingly.
         */
        m3->num_entries = num_entries;
        m3->hash->cond_deleted = cm3size - num_entries;

        for ( ; num_entries < cm3size; num_entries++)
        {
            int i;

            dest_key->type = T_INVALID; dest_key++;

            for (i = num_values; i > 0; i--, dest_data++)
            {
                put_number(dest_data, 0);
            }

        }
    } /* Merge condensed entries */

    /* Now copy the two hash parts, using get_map_lvalue() to create
     * the new hashed entries
     *
     * First m1...
     */
    if ( NULL != (hm = m1->hash) )
    {
        map_chain_t **mcp;
        p_int size;

        size = hm->mask + 1;
        mcp = hm->chains;
        do {
            map_chain_t *mc;

            for (mc = *mcp++; mc; mc = mc->next)
            {
                svalue_t * src, * dest;
                int i;

                src = &(mc->data[0]);
                dest = get_map_lvalue_unchecked(m3, src);
                for (src++, i = num_values; --i >= 0; )
                    assign_svalue(dest++, src++);
            }
        } while (--size);
    }

    /* ...now m2, potentially overwriting the entries from m1.
     */
    if ( NULL != (hm = m2->hash) )
    {
        map_chain_t **mcp;
        p_int size;

        size = hm->mask + 1;
        mcp = hm->chains;
        do {
            map_chain_t *mc;

            for (mc = *mcp++; mc; mc = mc->next)
            {
                svalue_t * src, * dest;
                int i;

                src = &(mc->data[0]);
                dest = get_map_lvalue_unchecked(m3, src);
                for (src++, i = num_values; --i >= 0; )
                    assign_svalue(dest++, src++);
            }
        } while (--size);
    }

    /* And that's it :-) */
    return m3;

} /* add_mapping() */

/*-------------------------------------------------------------------------*/
void
walk_mapping ( mapping_t *m
             , void (*func) (svalue_t *key, svalue_t *val, void *extra)
             , void *extra)

/* Generic function to perform a mapping walk. The function visits every
 * valid entry of <m> and for each entry calls <func>, passing the
 * current key, the current value(s) and the parameter <extra> to the
 * function.
 *
 * <func> may modify the value(s), but not the key.
 */

{
    mapping_cond_t *cm;
    mapping_hash_t *hm;
    svalue_t *key, *data;
    mp_int num_values;

    num_values = m->num_values;

    /* Walk through the condensed data */

    if (NULL != (cm = m->cond))
    {
        size_t ix; 
        
        for ( ix = 0, key = &(cm->data[0]), data = COND_DATA(cm, 0, num_values)
            ; ix < cm->size
            ; ix++, key++, data += num_values
            )
        {
            if (key->type != T_INVALID
             && !destructed_object_ref(key)
               )
              (*func)(key, data, extra);
        }
    }

    /* Walk through the hashed data */

    if (NULL != (hm = m->hash))
    {
        mp_int size;

        for (size = hm->mask; size >= 0; size--)
        {
            map_chain_t *mc;

            for (mc = hm->chains[size]; mc != NULL; )
            {
                map_chain_t *next = mc->next;
                if (!destructed_object_ref(&(mc->data[0])))
                    (*func)(&(mc->data[0]), &(mc->data[1]), extra);
                mc = next;
            }
        }
    }

} /* walk_mapping() */

/*-------------------------------------------------------------------------*/
void
compact_mappings (mp_int num)

/* Compact the first <num> mappings in the dirty-mapping list.
 * Compaction means: removal of all deleted entries from the condensed
 * and hashed part, merge of all hashed entries into the condensed part,
 * reduction of the memory held by the condensed part to the
 * minimum.
 *
 * The merger is a two step process: first, all hashed entries are
 * sorted, then the sorted entries are merged with the condensed part.
 * The sort itself is done using Mergesort, with special treatment for those
 * portions that don't make up the current power of 2.
 *
 * The function is big, but functionally simple: there is only so
 * much complexity in a Mergesort.
 */

{
    mapping_t *m;  /* The current mapping to compact */

    malloc_privilege = MALLOC_SYSTEM;
      /* compact_mappings() is called in very low memory situations,
       * so it has to be allowed to use the system reserve.
       * Neat sideeffect: all allocations are guaranteed to work (or
       * the driver terminates).
       */

    if (last_indexing_protector.type == T_PROTECTOR_MAPPING)
    {
        /* There is a slight chance that free_protector_mapping causes
         * remove_empty_mappings(), and thus changes num_dirty_mappings.
         */
        free_protector_mapping(last_indexing_protector.u.map);
        last_indexing_protector.type = T_NUMBER;
    }

    /* Restrict num to the number of dirty mappings */

    if (num >= num_dirty_mappings)
    {
        num = num_dirty_mappings;
        last_dirty_mapping = &dirty_mapping_head;
    }
    else
    {
        extra_jobs_to_do = MY_TRUE;
    }

    num_dirty_mappings -= num;

    m = dirty_mapping_head_hash.next_dirty;
    while (--num >= 0)
    {
        
        mapping_hash_t *hm;
          /* The hash part of m (guaranteed to exist!) */
        mapping_cond_t *cm;
          /* The condensed part of m */
        int num_values;
          /* Number of values per entry */

        mapping_t *m2;
          /* Temporary holder for the compacted result mapping */
        mapping_cond_t *cm2;
          /* The new condensed part of the mapping */

        map_chain_t *hook1, *hook2;
          /* All hashed entries in two long chains.
           */

        mp_int count1, count2;
        map_chain_t **mcpp, *mcp, *next;
        map_chain_t *last_hash;
          /* Auxiliaries */

        mp_int runlength;
          /* Current Mergesort partition length */

#ifdef DEBUG
        if (!m->user)
            fatal("No wizlist pointer for mapping\n");
#endif
        m->ref++; /* prevent freeing while using in case of recursive
                   * mappings referenced by a deleted value
                   */

        check_map_for_destr(m);

        hm = m->hash;
        cm = m->cond;

        if (hm->ref) {
            fatal("compact_mappings(): remaining ref count %ld!\n", hm->ref);
        }

        /* If this is an empty mapping awaiting deallocation,
         * just do that (remember that we incremented the refcount!).
         */
        if (1 == m->ref && !m->num_entries)
        {
            LOG_SUB("compact_mappings: empty mapping", sizeof(*m) + SIZEOF_MH(hm));
            m->user->mapping_total -= sizeof(*m) + SIZEOF_MH(hm);
            xfree(m);
            m = hm->next_dirty;
            xfree(hm);
            empty_mapping_load -= 2;
            continue;
        }

        /* Make sure that we remove all destructed entries */
        check_map_for_destr(m);

        /* Test if the mapping needs compaction at all.
         * If not, just delete the hash part.
         */
        if (!hm->used && !hm->cond_deleted)
        {
            LOG_SUB("compact_mappings: no need to", SIZEOF_MH(hm));
            m->user->mapping_total -= SIZEOF_MH(hm);
            m->hash = NULL;

            /* the ref count has been incremented above; on the other
             * hand, the last real reference might have gone with the
             * deleted keys. If that is the case, free_mapping() will
             * deallocate it (since we NULL out the .hash).
             */
            free_mapping(m);

            m = hm->next_dirty;
            xfree(hm);
            continue;
        }

        /* Nope, this one needs compaction.
         * Get the temporary result mapping (we need the condensed block
         * anyway, and this way it's simple to keep the statistics
         * straight).
         */

        num_values = m->num_values;

        m2 = get_new_mapping(m->user, num_values, 0, m->num_entries);
        cm2 = m2->cond;

        if (cm2 != NULL)
        {
            /* --- Setup Mergesort ---
             *
             * Unravel all hash chains into two chains, dangling from hook1
             * and hook2.
             *
             * The chains differ in length by at most 1 element. Within
             * the chains, the elements are pairwise sorted.
             *
             * In this loop, hook1 is always the next chain to add to,
             * and last_hash is the first element of the next pair to add.
             */
            mcpp = hm->chains;
            count1 = hm->mask;
            hook1 = hook2 = NULL;
            last_hash = NULL;

            do {
                mcp = *mcpp;
                *mcpp++ = NULL; /* m no longer owns this chain */
                while (mcp)
                {
                    next = mcp->next;

                    if (last_hash)
                    {
                        p_int d = mcompare(&(mcp->data[0]), &(last_hash->data[0]));

                        if (d < 0) {
                            last_hash->next = hook1;
                            mcp->next = last_hash;
                            hook1 = hook2;
                            hook2 = mcp;
                        } else {
                            mcp->next = hook1;
                            last_hash->next = mcp;
                            hook1 = hook2;
                            hook2 = last_hash;
                        }
                        last_hash = NULL;
                    }
                    else
                    {
                        last_hash = mcp;
                    }
                    mcp = next;
                }
            } while (--count1 >= 0);

            /* Add the remaining odd element */
            if (last_hash)
            {
                last_hash->next = hook1;
                hook1 = last_hash;
            }


            /* --- Mergesort the hashed entries ---
             *
             * Sort hook1 and hook2 into hook1.
             */
            for (runlength = 2; runlength < hm->used; runlength <<= 1)
            {
                map_chain_t *out_hook1, *out_hook2, **out1, **out2;
                  /* The output chains, which serve as input chains in
                   * the next pass
                   */

                count1 = hm->used & (runlength-1);
                count2 = hm->used & runlength;
                if (!count1)
                {
                    out2 = &out_hook1;
                    *out2 = hook2;
                    while (--count2 >= 0) {
                        out2 = &(*out2)->next;
                    }
                    hook2 = *out2;
                    count1 = count2 = runlength;
                    out1 = &out_hook2;
                }
                else if (!count2)
                {
                    out2 = &out_hook1;
                    *out2 = hook1;
                    do {
                        out2 = &(*out2)->next;
                    } while (--count1);
                    hook1 = *out2;
                    count1 = count2 = runlength;
                    out1 = &out_hook2;
                }
                else
                {
                    out1 = &out_hook1;
                    out2 = &out_hook2;
                }

                while (hook1)
                {
                    /* Sort the next runlength elements onto out1 */
                    while (1) {
                        p_int d = mcompare(&(hook1->data[0]), &(hook2->data[0]));

                        if (d > 0)
                        {
                            *out1 = hook2;
                            out1 = &hook2->next;
                            hook2 = *out1;
                            if (!--count2)
                            {
                                *out1 = hook1;
                                do {
                                    out1 = &(*out1)->next;
                                } while (--count1);
                                hook1 = *out1;
                                break;
                            }
                        }
                        else
                        {
                            *out1 = hook1;
                            out1 = &hook1->next;
                            hook1 = *out1;
                            if (!--count1)
                            {
                                *out1 = hook2;
                                do {
                                    out1 = &(*out1)->next;
                                } while (--count2);
                                hook2 = *out1;
                                break;
                            }
                        }
                    }

                    /* Now switch the chains */
                    {
                        map_chain_t **temp;

                        temp = out1;
                        out1 = out2;
                        out2 = temp;
                    }
                    count1 = count2 = runlength;
                }

                /* Terminate the out-chains and set them up
                 * as next input chains.
                 */
                *out1 = NULL;
                *out2 = NULL;
                hook1 = out_hook1;
                hook2 = out_hook2;
            }
            if (!hook1)
                hook1 = hook2;


            /* --- Merge the old condensed part with the sorted lists ---
             */
            {
                size_t src_ix;  /* Index into the old keys */
                svalue_t *src_key, *src_data;
                svalue_t *dest_key, *dest_data;

                src_ix = 0;
                src_key = cm ? &(cm->data[0]) : NULL;
                src_data = cm ? COND_DATA(cm, 0, num_values) : NULL;
                dest_key = &(cm2->data[0]);
                dest_data = COND_DATA(cm2, 0, num_values);

                /* Do the actual merge.
                 */
                while (hook1 && cm != NULL && src_ix < cm->size)
                {
                    int d;

                    if (src_key->type == T_INVALID)
                    {
                        src_ix++;
                        src_key++;
                        src_data += num_values;
                        continue;
                    }

                    d = mcompare(src_key, &(hook1->data[0]));

                    if (d > 0)
                    {
                        /* Take entry from hook1 */

                        map_chain_t *temp;
                        svalue_t    *src;
                        int i;

                        *dest_key++ = hook1->data[0];

                        for (src = &(hook1->data[1]), i = num_values; i > 0; --i)
                            *dest_data++ = *src++;

                        temp = hook1;
                        hook1 = temp->next;
                        free_map_chain(m, temp, MY_TRUE);
                    }
                    else
                    {
                        /* Take entry from the old condensed part */

                        int i;

                        *dest_key++ = *src_key++;

                        for (i = num_values; i > 0; --i)
                            *dest_data++ = *src_data++;

                        src_ix++;
                    }
                } /* if (hook1 && src_ix < cm->size) */

                /* Copy any remaining entries from the old condensed part
                 * or the misc_hook1
                 */
                if (cm != NULL && src_ix < cm->size)
                {
                    /* Copy from the old condensed part */

                    while (src_ix < cm->size)
                    {
                        if (src_key->type != T_INVALID)
                        {
                            int i;

                            *dest_key++ = *src_key++;

                            for (i = num_values; i > 0; --i)
                                *dest_data++ = *src_data++;
                        }
                        else
                        {
                            src_key++;
                            src_data += num_values;
                        }
                        src_ix++;
                    }
                }
                else
                {
                    /* Copy from hook1 */

                    while (hook1)
                    {
                        map_chain_t *temp;
                        svalue_t    *src;
                        int i;

                        *dest_key++ = hook1->data[0];

                        for (src = &(hook1->data[1]), i = num_values; i > 0; --i)
                            *dest_data++ = *src++;

                        temp = hook1;
                        hook1 = temp->next;
                        free_map_chain(m, temp, MY_TRUE);
                    }
                }
            } /* --- End of Merge --- */
        } /* --- if (cm2 != NULL) --- */

        /* Switch the new key and data blocks from m2 to m, and
         * vice versa for the old ones. We don't assign the hash block
         * as we already deleted all the map_chain structures.
         */
        m->cond = cm2;
        m2->cond = cm;

        m->hash = NULL; /* Since we compacted it away */

        LOG_SUB("compact_mappings - remove old hash", SIZEOF_MH(hm));
        m->user->mapping_total -= SIZEOF_MH(hm);
          /* The memorysize for the map_chain_t structure has already been
           * subtracted.
           */

        free_mapping(m);
          /* Undo the initial m->ref++; if there was a recursive
           * reference which is now gone, the mapping will be deallocated
           * now.
           */

        free_empty_mapping(m2);
          /* Get rid of the temporary mapping and the old cond block.
           */

        m = hm->next_dirty;

        xfree(hm);
    } /* while (num >= 0) */

    /* m is now the first of the remaining uncompacted mappings, or
     * the head itself if all dirty mappings have been processed.
     */

    dirty_mapping_head_hash.next_dirty = m;

} /* compact_mappings() */

/*-------------------------------------------------------------------------*/
mp_int
total_mapping_size (void)

/* Return the amount of memory used by all mappings in the system
 */

{
    wiz_list_t *wl;
    mp_int total;

    total = default_wizlist_entry.mapping_total;
    for (wl = all_wiz; wl; wl = wl->next) {
        total += wl->mapping_total;
    }
    return total;
} /* total_mapping_size() */

/*-------------------------------------------------------------------------*/
size_t
mapping_overhead (mapping_t *m)

/* Return the memory overhead size of the given mapping <m>.
 */

{
    size_t rc = 0;

    rc = sizeof(*m);
    if (m->cond)
        rc += sizeof(m->cond) - sizeof(svalue_t);
    if (m->hash)
        rc += SIZEOF_MH(m->hash)
              + m->hash->used * (sizeof(map_chain_t) - sizeof(svalue_t))
           ;

    return rc;
} /* mapping_overhead() */

/*-------------------------------------------------------------------------*/

/* Structure used by set_mapping_user() to communicate with ..._filter()
 */
struct set_mapping_user_locals
{
    int        num_values;  /* Number of values per key */
    object_t  *owner;       /* Owner to set */
    svalue_t **hairy;
      /* Next free entry in the array of keys which need manual tweaking */
};


static void
set_mapping_user_filter (svalue_t *key, svalue_t *data, void *extra)

/* walk_mapping-callback function used by set_mapping_user().
 * <extra> points in fact to a struct set_mapping_user_locals.
 *
 * Set the owner of <key> and all <data> to extra.owner (this might call
 * set_mapping_user() recursively).
 *
 * If the key needs special treatment (ie. changing the owner would change
 * its sort index), it is left unchanged and a memory copy of it is stored in
 * extra.hairy++.
 */

{
    int i;
    struct set_mapping_user_locals *locals;
    object_t *owner;

    locals = (struct set_mapping_user_locals *)extra;
    owner = locals->owner;

    if (key->type == T_CLOSURE)
    {
        *(locals->hairy++) = key;
    }
    else
    {
        set_svalue_user(key, owner);
    }
    for (i = locals->num_values; --i > 0;)
    {
        set_svalue_user(data++, owner);
    }
}

void
set_mapping_user (mapping_t *m, object_t *owner)

/* Set the <owner> as the user of mapping <m> and all its contained
 * keys and values, and update the wizlist entry for <owner>.
 *
 * As this function is called only for variables in newly compiled
 * objects, there is no need to guard against recursive
 * calls for this particular mapping.
 */

{
    int num_values;
    mp_int total;
    wiz_list_t *user;
    struct set_mapping_user_locals locals;
    svalue_t **first_hairy;
    mp_int i;

    num_values = m->num_values;

    /* Move the total size in the wizlist from the old owner
     * to the new one
     */
    total = (mp_int)( sizeof(*m)
                     + ((m->cond) ? SIZEOF_MC(m->cond, m->num_values) : 0)
                    );
    LOG_SUB("set_mapping_user", total);
    m->user->mapping_total -= total;
    user = owner->user;
    m->user = user;
    LOG_ADD("set_mapping_user", total);
    m->user->mapping_total += total;


    /* Walk the mapping to set all owners */

    locals.owner = owner;
    locals.num_values = num_values;
    first_hairy = alloca(((m->cond) ? m->cond->size : 1) * sizeof(svalue_t *)); 
    if (!first_hairy)
    {
        error("Stack overflow.\n");
        /* NOTREACHED */
        return;
    }
    locals.hairy = first_hairy;
    walk_mapping(m, set_mapping_user_filter, &locals);

    /* All 'hairy' keys are changed by reassignment to the mapping.
     * Be aware that changing the user might not change the search order.
     */
    for (i = locals.hairy - first_hairy; --i >= 0; first_hairy++)
    {
        svalue_t new_key, *dest, *source;
        mp_int j;

        /* Create the new key by changing its owner */
        assign_svalue_no_free(&new_key, *first_hairy);
        set_svalue_user(&new_key, owner);

        /* Create a new entry in the mapping for the new key */
        dest = get_map_lvalue_unchecked(m, &new_key);
        free_svalue(&new_key);

        /* Move the values from the old entry to the new one, invalidating
         * the old ones by this.
         */
        source = get_map_value(m, *first_hairy);
        if (source != dest)
        {
            if (num_values)
                memcpy((char *)dest, (char *)source, num_values * sizeof *dest);
            for (j = num_values; --j > 0; source++)
                source->type = T_INVALID;

            /* Remove the old entry */
            remove_mapping(m, *first_hairy);
        }
    }
} /* set_mapping_user() */


#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
clear_mapping_size (void)

/* Clear the statistics about the number and memory usage of all mappings
 * in the game.
 */

{
    wiz_list_t *wl;

    num_mappings = 0;
    default_wizlist_entry.mapping_total = 0;
    for (wl = all_wiz; wl; wl = wl->next)
        wl->mapping_total = 0;
} /* clear_mapping_size(void) */

/*-------------------------------------------------------------------------*/
void
count_mapping_size (mapping_t *m)

/* Add the mapping <m> to the statistics.
 */

{
    mp_int total;

    num_mappings++;

    total = sizeof(*m);
    if (m->cond != NULL)
        total += SIZEOF_MC(m->cond, m->num_values);
    if (m->hash != NULL)
        total += SIZEOF_MH_ALL(m->hash, m->num_values);

    m->user->mapping_total += total;
} /* count_mapping_size(void) */

/*-------------------------------------------------------------------------*/
void
count_ref_in_mapping (mapping_t *m)

/* GC support: Count all references by the mapping <m>.
 * The GC will call this function only for compacted mappings.
 *
 * If the mapping contains keys referencing destructed objects/lambdas,
 * it is added to the list of stale mappings.
 */

{
    mp_int size;
    mp_int num_values;
    Bool any_destructed = MY_FALSE;

    num_values = m->num_values;

    /* Count references by condensed keys and their data.
     * Take special care of keys referencing destructed objects/lambdas.
     */

    size = m->cond ? m->cond->size : 0;
    while ( --size >= 0)
    {
        svalue_t * key = &(m->cond->data[size]);
        svalue_t * data = COND_DATA(m->cond, size, num_values);

        if (destructed_object_ref(key))
        {
            /* This key is a destructed object, resp. is bound to a destructed
             * object. The entry has to be deleted.
             */

            if (key->type == T_CLOSURE &&
                key->x.closure_type == CLOSURE_BOUND_LAMBDA)
            {
                /* We don't want changing keys, even if they are still valid
                 * unbound closures
                 */
                lambda_t *l = key->u.lambda;

                key->x.closure_type = CLOSURE_LAMBDA;
                key->u.lambda = l->function.lambda;
                if (!l->ref) {
                    l->function.lambda->ob = l->ob;
                    l->ref = -1;
                    l->ob = (object_t *)stale_misc_closures;
                    stale_misc_closures = l;
                } else {
                    l->function.lambda->ob = gc_obj_list_destructed;
                }
            }
            count_ref_in_vector(key, 1);
            if (key->type == T_CLOSURE) {
                /* *key has been transformed by count_ref_in_vector()
                 * into an efun closure bound to the master
                 */
                key->u.ob->ref--;
            }

            /* Don't bother freeing the svalues - this is the GC after all,
             * and freeing them might even confuse the memory allocator.
             */
            m->num_entries--;
            key->type = T_INVALID;

            if (!any_destructed)
            {
                any_destructed = MY_TRUE;
                /* Might be a small mapping. Don't malloc, it might get too
                 * much due to the global scope of garbage_collection.
                 * Since there was a previous
                 * compact_mappings(num_dirty_mappings), the hash field is
                 * known to be NULL.
                 */
                m->hash = (mapping_hash_t *)stale_mappings;
                stale_mappings = m;
                /* We are going to use free_svalue() later to get rid of the
                 * data asscoiated with the keys. This data might reference
                 * mappings with destructed keys... Thus, we must prevent
                 * free_mapping() to look at the hash field.
                 */
                m->ref++;
                /* Ref for the stale-mapping link. */
            }
        }
        else
        {
            count_ref_in_vector(key, 1);
            count_ref_in_vector(data, num_values);
        }
    }
} /* count_ref_in_mapping() */

/*-------------------------------------------------------------------------*/
void
clean_stale_mappings (void)

/* GC support: After count_ref_in_mapping(), the gc will free all
 * unreferenced destructed objects and lambdas. This may have removed
 * several keys in the stale_mappings. Since the objective
 * is to recover memory, we try to compact these mappings now.
 * Be aware that the mappings might be empty now.
 */

{
    mapping_t *m, *next;

    for (m = stale_mappings; m; m = next)
    {
        mapping_cond_t *cm;
        size_t size;
        mp_int num_values;
        mp_int i;

        /* Unlink from the stale_mapping list */
        next = (mapping_t *)m->hash;
        m->hash = NULL;

        num_values = m->num_values;
        cm = m->cond;

        /* Try to reallocate a new condensed block */

        if (m->num_entries)
        {
            mapping_cond_t *cm2;
            size_t ix;
            svalue_t *src_key, *src_data;
            svalue_t *dest_key, *dest_data;

            size = sizeof(*cm2) + sizeof(svalue_t) * (m->num_entries*(num_values+1) - 1);
            cm2 = xalloc(size);
            if (!cm2)
            {
                fprintf(stderr, "%s Unable to compact stale mapping: Out of memory "
                                "for new condensed block (%ld bytes).\n"
                              , time_stamp(), (long)size);
                debug_message("%s Unable to compact stale mapping: Out of memory "
                              "for new condensed block (%ld bytes).\n"
                             , time_stamp(), (long)size);

                /* No use in even trying to compact the much bigger data
                 * block either.
                 */
                continue;
            }

            cm2->size = m->num_entries;

            /* Copy the data */
            for (   ix = 0
                  , src_key = &(cm->data[0])
                  , src_data = COND_DATA(cm, 0, num_values)
                  , dest_key = &(cm2->data[0])
                  , dest_data = COND_DATA(cm2, 0, num_values)
                ; ix < cm->size
                ; ix++, src_key++)
            {
                if (src_key->type != T_INVALID)
                {
                    *dest_key++ = *src_key;
                    for (i = num_values; i > 0; i--)
                        *dest_data++ = *src_data++;
                }
                else
                    src_data += num_values;
            }

            /* Replace the old keyblock by the new one. */
            LOG_ALLOC("clean_stale - new keyblock", SIZEOF_MC(cm2, num_values), size);
            m->user->mapping_total += SIZEOF_MC(cm2, num_values);
            m->cond = cm2;
        }
        else
        {
            /* Mapping is empty - no condensed block needed. */
            m->cond = NULL;
        }

        /* Delete the old condensed block, if any */
        if (cm)
        {
            LOG_SUB("clean_state - old keyblock", SIZEOF_MC(cm, num_values));
            m->user->mapping_total -= SIZEOF_MC(cm, num_values);
            xfree(cm);
        }

        free_mapping(m); /* Undo the ref held by the stale-mapping list */
    }
} /* clean_stale_mappings() */

#endif /* GC_SUPPORT */

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
svalue_t *
f_m_allocate (svalue_t *sp)

/* EFUN m_allocate()
 *
 *   mapping m_allocate(int size, int width)
 *
 * Reserve memory for a mapping.
 *
 * size is the number of entries (i.e. keys) to reserve, width is
 * the number of data items per entry. If the optional width is
 * omitted, 1 is used as default.
 */

{
    if (sp[-1].u.number < 0)
        error("Illegal mapping size: %ld\n", sp[-1].u.number);
    if (sp->u.number < 0)
        error("Illegal mapping width: %ld\n", sp->u.number);

    if (max_mapping_size && sp[-1].u.number > max_mapping_size)
        error("Illegal mapping size: %ld\n", sp[-1].u.number);

    sp--;

    if (!(sp->u.map = allocate_mapping(sp->u.number, sp[1].u.number)))
    {
        sp++;
        /* sp points to a number-typed svalue, so freeing won't
         * be a problem.
         */
        error("Out of memory\n");
    }
    sp->type = T_MAPPING;

    return sp;
} /* f_m_allocate() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_m_delete (svalue_t *sp)

/* EFUN m_delete()
 *
 *   mapping m_delete(mapping map, mixed key)
 *
 * Remove the entry with index 'key' from mapping 'map'. The
 * changed mapping 'map' is also returned as result.
 * If the mapping does not have an entry with index 'key',
 * nothing is changed.
 */

{
    mapping_t *m;

    m = (sp-1)->u.map;
    remove_mapping(m, sp);
    free_svalue(sp--);
    /* leave the modified mapping on the stack */
    return sp;
} /* f_m_delete() */

/*-------------------------------------------------------------------------*/
vector_t *
m_indices (mapping_t *m)

/* Create a vector with all keys from mapping <m> and return it.
 * If the mapping contains destructed objects, m_indices() will remove
 * them.
 *
 * The helper function m_indices_filter() is located in interpret.c
 * to take advantage of inlined assign_svalue_no_free().
 *
 * The function is used for efuns m_indices(), map_mapping(), and for
 * the loop construct foreach().
 */

{
    vector_t *v;
    svalue_t *svp;
    mp_int size;

    check_map_for_destr(m);
    size = (mp_int)MAP_SIZE(m);
    v = allocate_array(size); /* might cause error */
    svp = v->item;
    walk_mapping(m, m_indices_filter, &svp);
    return v;
} /* m_indices() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_m_indices (svalue_t *sp)

/* EFUN m_indices()
 *
 *   mixed *m_indices(mapping map)
 *
 * Returns an array containing the indices of mapping 'map'.
 */

{
    mapping_t *m;
    vector_t *v;

    m = sp->u.map;
    v = m_indices(m);

    free_mapping(m);
    put_array(sp,v);

    return sp;
} /* f_m_indices() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_m_values (svalue_t *sp)

/* EFUN m_values()
 *
 *   mixed *m_values(mapping map)
 *   mixed *m_values(mapping map, int index)
 *
 * Returns an array with the values of mapping 'map'.
 * If <index> is given as a number between 0 and the width of
 * the mapping, the values from the given column are returned,
 * else the values of the first column.
 *
 * The called filter function m_values_filter() is in interpret.c
 * to take advantage of inline expansion.
 */

{
    mapping_t *m;
    vector_t *v;
    struct mvf_info vip;
    mp_int size;
    int num;

    /* Get and check the arguments */
    num = sp->u.number;
    sp--;
    inter_sp = sp;

    m = sp->u.map;
    if (num < 0 || num >= m->num_values)
        error("Illegal index %d to m_values(): should be in 0..%ld.\n"
             , num, (long)m->num_values-1);

    /* Get the size of the mapping */
    check_map_for_destr(m);
    size = (mp_int)MAP_SIZE(m);

    if (size > 0 && m->num_values < 1)
        error("m_values() applied on mapping with no values.\n");

    v = allocate_array(size);

    /* Extract the desired column from the mapping */
    vip.svp = v->item;
    vip.num = num;
    walk_mapping(m, m_values_filter, &vip);
    free_mapping(m);

    /* Push the result */
    put_array(sp,v);

    return sp;
} /* f_m_values() */

/*-------------------------------------------------------------------------*/
static void
add_to_mapping_filter (svalue_t *key, svalue_t *data, void *extra)

/* Auxiliary function to add_to_mapping():
 * Add/overwrite (key:data) to mapping <extra>.
 */

{
    svalue_t *data2;
    int i;

    data2 = get_map_lvalue_unchecked((mapping_t *)extra, key);
    if (data2 != data) /* this should always be true */
    {
        for (i = ((mapping_t *)extra)->num_values; --i >= 0;)
        {
            assign_svalue(data2++, data++);
        }
    }
} /* add_to_mapping_filter() */

/*-------------------------------------------------------------------------*/
void
add_to_mapping (mapping_t *m1, mapping_t *m2)

/* Add the data from mapping <m2> to mapping <m1>, overwriting existing
 * entries.
 *
 * If the values per entry differ, and one of the mappings is empty,
 * the empty mapping's width is set to that of the non-empy one.
 * Otherwise (different width, no mapping empty) the function returns
 * immediately.
 *
 * Called by interpret.c as part of F_ADD_EQ and F_VOID_ADD_EQ.
 */

{
    /* Adding a mapping to itself doesn't change its content. */
    if (m1 == m2)
        return;

    if (m2->num_values != m1->num_values)
    {
        /* If one of the two mappings is empty, we can adjust its width
         * after getting rid of all pending data blocks.
         */
        if (0 == m2->num_entries && NULL == m2->hash)
        {
            if (m2->cond != NULL)
            {
                LOG_SUB_M("add_to_mapping - m2 no cond", m2, SIZEOF_MC(m2->cond, m2->num_values));
                m2->user->mapping_total -= SIZEOF_MC(m2->cond, m2->num_values);
                xfree(m2->cond);
                m2->cond = NULL;
            }
            m2->num_values = m1->num_values;
        }
        else if (0 == m1->num_entries && NULL == m1->hash)
        {
            if (m1->cond != NULL)
            {
                LOG_SUB_M("add_to_mapping - m1 no cond", m1, SIZEOF_MC(m2->cond, m2->num_values));
                m1->user->mapping_total -= SIZEOF_MC(m1->cond, m1->num_values);
                xfree(m1->cond);
                m1->cond = NULL;
            }
            m1->num_values = m2->num_values;
        }
        else
        {
            return;
        }
    }
    walk_mapping(m2, add_to_mapping_filter, m1);
} /* add_to_mapping() */

/*-------------------------------------------------------------------------*/
void
sub_from_mapping_filter ( svalue_t *key, svalue_t *data UNUSED
                        , void *extra)

/* Auxiliary to subtract_mapping(): Delete <key> from mapping <extra>.
 * Also called by interpret.c as part of F_SUB_EQ (which then makes sure
 * that subtrahend and minuend are not identical).
 */

{
#ifdef __MWERKS__
#    pragma unused(data)
#endif
    remove_mapping((mapping_t *)extra, key);
} /* sub_from_mapping_filter() */

/*-------------------------------------------------------------------------*/
mapping_t *
subtract_mapping (mapping_t *minuend, mapping_t *subtrahend)

/* Create a copy of <minuend> minus all entries which are also in
 * <subtrahend>.
 *
 * Called by interpret.c as part of F_SUBTRACT.
 */

{
    /* TODO: This could be done faster, especially if there the mappings are
     * mainly condensed. On the other hand, the priority of fast mapping
     * subtraction is unknown.
     * Also, by providing a copy of the minuend it is safe to subtract
     * a mapping from itself.
     */
    minuend = copy_mapping(minuend);
    walk_mapping(subtrahend, sub_from_mapping_filter, minuend);
    return minuend;
} /* subtract_mapping() */

/*-------------------------------------------------------------------------*/
static void
f_walk_mapping_filter (svalue_t *key, svalue_t *data, void *extra)

/* Auxiliary to efuns {walk,filter}_mapping(): callback for walk_mapping().
 *
 * <extra> is a pointer to a (svalue_t *) to an array of 2 svalues.
 * The first of these gets to hold the <key>, the second is an lvalue
 * pointing to <data>.
 */

{
    svalue_t *svp;

    svp = *(svalue_t **)extra;
    assign_svalue_no_free(svp, key);
    (++svp)->u.lvalue = data;
    *(svalue_t **)extra = ++svp;
} /* f_walk_mapping_filter() */

/*-------------------------------------------------------------------------*/
static void
f_walk_mapping_cleanup (svalue_t *arg)

/* Auxiliary to efuns {walk,filter}_walk_mapping(): Cleanup.
 *
 * This function is called during the stackcleanup after a mapping walk.
 * <arg> is the array of svalue allocated by walk_mapping_prologue().
 * See walk_mapping_prologue() for details.
 */

{
    svalue_t *svp;
    mapping_t *m;
    mp_int i;

    svp = arg + 1;

    if (svp->u.cb)
        free_callback(svp->u.cb);
    svp++;
    
    m = svp[1].u.map;

    /* If the mapping had a hash part prior to the f_walk_mapping(),
     * it was protected by the prologue and we have to lift that
     * protection.
     */
    if (svp[1].x.generic)
    {
        mapping_hash_t *hm;

        hm = m->hash;

        if (!--hm->ref)
        {
            /* Last ref gone: deallocated the pending deleted entries */

            map_chain_t *mc, *next;

            for (mc = hm->deleted; mc; mc = next)
            {
                free_map_chain(m, mc, MY_FALSE);
                next = mc->next;
            }
        }
    }

    /* Free the key svalues in the block */
    i = svp->u.number;
    if (i) do
    {
        svp += 2;
        free_svalue(svp);
    } while (--i > 0);

    /* Deallocate the block */
    xfree(arg);

} /* f_walk_mapping_cleanup() */

/*-------------------------------------------------------------------------*/
static svalue_t *
walk_mapping_prologue (mapping_t *m, svalue_t *sp, callback_t *cb)

/* Auxiliary to efuns {walk,filter}_walk_mapping(): Setup.
 *
 * The function creates an svalue array of the keys and (as lvalues) the
 * data values of mapping <m>. The head of the array holds organisational
 * information; the array as a whole is put as lvalue onto the stack
 * at <sp>+1.
 *
 * The result configuration of the array is:
 *
 *    sp+1  ->  [0] { lvalue } -> { T_ERROR_HANDLER: f_walk_mapping_cleanup }
 *              [1] { u.cb: callback structure }
 *              [2] { u.number: number of mapping entries }
 *              [3] { u.map: <m>, x.generic: <m> has hash part }
 *    result -> [4] { key1 }
 *              [5] { lvalue } -> values of key1
 *              [6] { key2 }
 *              [7] { lvalue } -> values of key2
 *                etc
 *
 * Storing the array as error handler allows a simple cleanup in course
 * of the free_svalue()s done by f_walk_mapping().
 *
 * If <m> at call time has a hash part, it is protected by incrementing
 * hash->ref.
 */

{
    mapping_hash_t *hm;
    svalue_t *pointers;
    svalue_t *write_pointer, *read_pointer;

    if ( NULL != (hm = m->hash) ) {
        if (m->num_values == 1)
        {
            hm = NULL; /* Flag: no values per key */
        }
        else if (!hm->ref++)
        {
            hm->deleted = NULL;
        }
    }
    pointers = (svalue_t *)xalloc( (m->num_entries * 2 + 4) * sizeof(svalue_t) );
    pointers[0].type = T_ERROR_HANDLER;
    pointers[0].u.error_handler = f_walk_mapping_cleanup;
    pointers[1].type = T_CALLBACK;
    pointers[1].u.cb = cb;
    pointers[2].u.number = m->num_entries;
    pointers[3].u.map = m;
    pointers[3].x.generic = hm != NULL;
    (++sp)->type = T_LVALUE;
    sp->u.lvalue = pointers;
    read_pointer = write_pointer = pointers + 4;
    walk_mapping(m, f_walk_mapping_filter, &write_pointer);
    return read_pointer;
} /* walk_mapping_prologue() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_walk_mapping (svalue_t *sp, int num_arg)

/* EFUN walk_mapping()
 *
 *   void walk_mapping(mapping m, string func, string|object ob, mixed extra,...)
 *   void walk_mapping(mapping m, closure cl, mixed extra,...)
 *
 * Calls ob->func(key, value1, ..., valueN, extra,...) resp. applies
 * the closure to every entry in the mapping. The keys are passed
 * by value, the values are passed by reference and can be
 * changed in the function.
 * Any number of extra arguments is accepted and passed.
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 */

{
    svalue_t *arg;           /* Begin of the args on the stack */
    callback_t cb;
    int error_index;
    mapping_t *m;            /* Mapping to walk */
    int num_values;               /* Number of values per entry */
    svalue_t *read_pointer;  /* Prepared mapping values */
    mp_int i;

    /* Locate the arguments on the stack and extract them */
    arg = sp - num_arg + 1;
    inter_sp = sp;

    error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
    inter_sp = sp = arg;
    num_arg = 1;

    if (error_index >= 0)
    {
        vefun_bad_arg(error_index+2, sp);
        /* NOTREACHED */
        return sp;
    }

    m = arg[0].u.map;


    /* Preparations */

    check_map_for_destr(m);
    assign_eval_cost();

    read_pointer = walk_mapping_prologue(m, sp, &cb);
    i = read_pointer[-2].u.number;
    sp++; /* walk_mapping_prologue() pushed one value */

    num_values = m->num_values;

    /* For every key:values pair in read_pointer[], set up
     * the stack for a call to the walk function.
     */
    while (--i >= 0)
    {
        int j;
        svalue_t *sp2, *data;

        if (!callback_object(&cb))
            error("Object used by walk_mapping destructed");

        /* Push the key */
        assign_svalue_no_free( (sp2 = sp+1), read_pointer++ );

        /* Push the values as lvalues */
        for (j = num_values, data = (read_pointer++)->u.lvalue; --j >= 0; )
        {
             (++sp2)->type = T_LVALUE;
             sp2->u.lvalue = data++;
        }

        /* Call the function */
        inter_sp = sp2;
        (void)apply_callback(&cb, 1 + num_values);
    }

    /* This frees the whole array allocated by the prologue,
     * including the data held by the callback.
     */
    free_svalue(sp);

    /* Free the arguments */
    i = num_arg;
    do
        free_svalue(--sp);
    while (--i > 0);

    return sp-1;
} /* f_walk_mapping() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_filter_mapping (svalue_t *sp, int num_arg, Bool bFull)

/* EFUN filter() on mappings, filter_mapping() == filter_indices()
 *
 *   mapping filter_mapping(mapping, string func, string|object ob, ...)
 *   mapping filter_mapping(mapping, closure cl, ...)
 *
 *   mapping filter(mapping, string func, string|object ob, ...)
 *   mapping filter(mapping, closure cl, ...)
 *
 * ob->func() is called resp. cl applied to every element in the
 * mapping, with the key of the element as first argument, optionally
 * the data for the key as second argument (if bFull is TRUE), and
 * then the extra args that were given to the efun. If the function
 * returns true, the element is added to the result mapping.
 *
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 *
 * If the data for the key is passed, it can take one of the following
 * forms:
 *    widthof(m) == 0:  nothing is passed
 *    widthof(m) == 1:  m[key] is passed
 *    widthof(m) >  1:  ({ m[key,0] .. m[key,width-1] }) is passed
 */

{
    svalue_t *arg;           /* Start of arguments on the stack */
    mapping_t *m;            /* Mapping to filter */
    int         error_index;
    callback_t  cb;
    int num_values;               /* Width of the mapping */
    vector_t *dvec;          /* Values of one key */
    svalue_t *dvec_sp;       /* Stackentry of dvec */
    svalue_t *read_pointer;  /* Prepared mapping values */
    svalue_t *v;
    int i, j;

    /* Locate the arguments on the stack and extract them */
    arg = sp - num_arg + 1;

    error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
    inter_sp = sp = arg;
    num_arg = 1;

    if (error_index >= 0)
    {
        vefun_bad_arg(error_index+2, sp);
        /* NOTREACHED */
        return sp;
    }

    m = arg[0].u.map;

    /* Preparations */

    check_map_for_destr(m);
    assign_eval_cost();

    num_values = m->num_values;

    /* Prepare the vector for the values of each element */
    dvec = NULL;
    dvec_sp = NULL;
    bFull = bFull ? 1 : 0;
      /* So we can use it as the number of extra arguments */

    if (bFull && num_values > 1)
    {
        dvec = allocate_array(num_values);
        if (!dvec)
        {
            inter_sp = sp;
            error("Out of memory\n");
        }
        ++sp;
        put_array(sp, dvec);
        dvec_sp = sp;
    }

    read_pointer = walk_mapping_prologue(m, sp, &cb);

    m = allocate_mapping(read_pointer[-2].u.number, num_values);
    if (!m)
    {
        inter_sp = sp + 1;
        error("Out of memory\n");
    }
    sp += 2;
    put_mapping(sp, m);

      /* m and dvec are kept referenced on the stack so that
       * in case of an error it is properly dereferenced.
       * At a normal termination however, m will not be dereferenced.
       */

    /* For every (key:values) in read_pointer[], set up the stack for
     * a call to the filter function. If it returns true, assign the
     * pair to the new mapping.
     */
    for (i = read_pointer[-2].u.number; --i >= 0; read_pointer += 2)
    {
        svalue_t *data;

        /* Push the key */
        assign_svalue_no_free((inter_sp = sp + 1), read_pointer);

        if (bFull) /* Push the data */
        {
            if (num_values == 0)
            {
                push_number(inter_sp, 0);
            }
            else if (1 == num_values)
            {
                push_svalue(read_pointer[1].u.lvalue);
            }
            else
            {
                svalue_t *svp;

                v = read_pointer[1].u.lvalue;
                for (j = 0, svp = dvec->item
                    ; j < num_values
                    ; j++, svp++, v++)
                    assign_svalue(svp, v);
                push_svalue(dvec_sp);
            }
        }

        if (!callback_object(&cb))
            error("Object used by %s destructed"
                 , bFull ? "filter" : "filter_mapping");


        v = apply_callback(&cb, 1 + bFull);

        /* Did the filter return TRUE? */
        if (!v || (v->type == T_NUMBER && !v->u.number) )
            continue;

        /* If we come here, the filter function returned 'true'.
         * Therefore assign the pair to the new mapping.
         */
        v = get_map_lvalue_unchecked(m, read_pointer);
        for (j = num_values, data = read_pointer[1].u.lvalue; --j >= 0; )
        {
            assign_svalue_no_free(v++, data++);
        }
    }

    /* Cleanup the temporary data except for the reference to m.
     * The arguments have been removed before already.
     */
    free_callback(&cb);
    i = num_arg + (dvec != NULL ? 1 : 0);
    do
    {
        free_svalue(--sp);
    }
    while (--i >= 0);

    /* Return the result mapping in place of the argument mapping.
     */
    put_mapping(sp, m);

    return sp;
} /* x_filter_mapping() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_filter_indices (svalue_t *sp, int num_arg)

/* EFUN filter_indices()
 *
 *   mapping filter_indices(mapping, string func, string|object ob, ...)
 *   mapping filter_indices(mapping, closure cl, ...)
 *
 * ob->func() is called resp. cl applied to every element in the
 * mapping, with first argument being the key of the
 * element, and then the extra args that were given to
 * filter_mapping. If the function returns true, the element is
 * added to the result mapping. ob can also be a file_name of an
 * object.
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 */

{
    return x_filter_mapping(sp, num_arg, MY_FALSE);
}  /* f_filter_indices() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_map_mapping (svalue_t *sp, int num_arg, Bool bFull)

/* EFUN map() on mappings, map_indices()
 *
 *   mapping map_mapping(mapping m, string func, object ob, ...)
 *   mapping map_mapping(mapping m, closure cl, ...)
 *
 *   mapping map(mapping m, string func, string|object ob, ...)
 *   mapping map(mapping m, closure cl, ...)
 *
 * ob->func() is called resp. cl applied to every element in the
 * mapping, with the key of the element as first argument, optionally
 * the data for the key as second argument (if bFull is TRUE), and
 * then the extra args that were given to the efun.
 *
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 *
 * If the data for the key is passed, it can take one of the following
 * forms:
 *    widthof(m) == 0:  nothing is passed
 *    widthof(m) == 1:  m[key] is passed
 *    widthof(m) >  1:  ({ m[key,0] .. m[key,width-1] }) is passed
 *
 * The data item in the result mapping is set to the return value
 * of the function. ob can also be a file_name of an object.
 * If the second arg is a string and the third is not an
 * object, this_object() will be used as default.
 *
 * Note that if mapping m has more than one value per key, these
 * are ignored: the resulting mapping always has one value per key.
 *
 * Also note that the behaviour of this function is different from
 * map_array().
 */

{
    svalue_t *arg;           /* Begin of arguments on the stack */
    mapping_t *arg_m;        /* Mapping to map */
    mapping_t *m;            /* Result mapping */
    int num_values;          /* Width of the mapping */
    vector_t *vec;           /* Indices of m */
    svalue_t *dvec_sp;       /* Stackentry of dvec */
    vector_t *dvec;          /* Values of one key */
    long i;
    svalue_t *key;
    callback_t cb;
    int error_index;

    /* Locate and extract arguments */
    arg = sp - num_arg + 1;
    inter_sp = sp;

    error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
    inter_sp = sp = arg;
    num_arg = 1;

    if (error_index >= 0)
    {
        vefun_bad_arg(error_index+2, sp);
        /* NOTREACHED */
        return sp;
    }

    arg_m = arg[0].u.map;


    /* Preparations */

    assign_eval_cost();

    num_values = arg_m->num_values;

    /* Get the indices of arg_m */
    vec = m_indices(arg_m); /* might cause error */
    ++sp;
    put_array(sp, vec);

    /* Prepare the vector for the values of each element */
    dvec = NULL;
    dvec_sp = NULL;
    bFull = bFull ? 1 : 0;
      /* So we can use it as the number of extra arguments */

    if (bFull && num_values > 1)
    {
        dvec = allocate_array(num_values);
        if (!dvec)
        {
            inter_sp = sp;
            error("Out of memory\n");
        }
        ++sp;
        put_array(sp, dvec);
        dvec_sp = sp;
    }

    m = allocate_mapping((i = (long)VEC_SIZE(vec)), 1);
    if (!m)
    {
        inter_sp = sp;
        error("Out of memory\n");
    }
    ++sp;
    put_mapping(sp, m);

      /* Both vec, dvec and m are kept referenced on the stack so that
       * in case of an error they are properly dereferenced.
       * At a normal termination however, m will not be dereferenced
       * but vec and dvec will.
       */

    key = vec->item;
    for (; --i >= 0; key++) {
        svalue_t *v;
        svalue_t *data;

        /* Push the key */
        assign_svalue_no_free((inter_sp = sp + 1), key);

        if (bFull) /* Push the data */
        {
            if (0 == num_values)
                push_number(inter_sp, 0);
            else if (1 == num_values)
            {
                v = get_map_value(arg_m, key);
                push_svalue(v);
            }
            else
            {
                int j;
                svalue_t *svp;

                v = get_map_value(arg_m, key);
                for (j = 0, svp = dvec->item; j < num_values; j++, svp++, v++)
                    assign_svalue(svp, v);
                push_svalue(dvec_sp);
            }
        }

        /* Call the filter function */
        v = get_map_lvalue_unchecked(m, key);

        if (!callback_object(&cb))
            error("Object used by %s destructed"
                 , bFull ? "map" : "map_mapping");

        data = apply_callback(&cb, 1 + bFull);
        if (data)
        {
            transfer_svalue_no_free(v, data);
            data->type = T_INVALID;
        }
    }

    /* Cleanup the temporary data except for the reference to m.
     * The arguments have been removed before already.
     */
    free_callback(&cb);
    i = num_arg + (dvec != NULL ? 1 : 0);
    do
    {
        free_svalue(--sp);
    }
    while (--i >= 0);

    /* Return the result mapping in place of the argument mapping.
     */
    put_mapping(sp, m);
    return sp;
} /* x_map_mapping() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_map_indices (svalue_t *sp, int num_arg)

/* VEFUN map_indices()
 *
 *   mapping map_indices(mapping m, string func, object ob, ...)
 *   mapping map_indices(mapping m, closure cl, ...)
 *
 * ob->func() is called resp. cl applied to every element in the
 * mapping, with the key of the element as first argument, and
 * then the extra args that were given to map_mapping.
 * The data item in the mapping is replaced by the return value
 * of the function. ob can also be a file_name of an object.
 *
 * If <ob> is omitted, or neither an object nor a string, then
 * this_object() is used.
 */

{
    return x_map_mapping(sp, num_arg, MY_FALSE);
}  /* f_map_indices() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_m_contains (svalue_t *sp, int num_arg)

/* EFUN m_contains()
 *
 *   int m_contains(mixed &data1, ..., &dataN, map, key)
 *
 * If the mapping contains the key map, the corresponding values
 * are assigned to the data arguments, which massed be passed by
 * reference, and 1 is returned. If key is not in map, 0 is
 * returned and the data args are left unchanged.
 * It is possible to use this function for a 0-value mapping, in
 * which case it has the same effect as member(E).
 */

{
    svalue_t *item;
    int i;

    /* Test the arguments */
    for (i = -num_arg; ++i < -1; )
        if (sp[i].type != T_LVALUE)
            vefun_arg_error(num_arg + i, T_LVALUE, sp[i].type, sp);
    if (sp[-1].type != T_MAPPING)
        vefun_arg_error(num_arg-1, T_MAPPING, sp[-1].type, sp);
    if (sp[-1].u.map->num_values != num_arg - 2)
        error("Not enough lvalues: given %ld, required %ld.\n"
             , (long)num_arg-2, (long)sp[-1].u.map->num_values);

    item = get_map_value(sp[-1].u.map, sp);
    if (item == &const0)
    {
        /* Not found */
        sp = pop_n_elems(num_arg-1, sp);
        free_svalue(sp);
        put_number(sp, 0);
        return sp;
    }

    free_svalue(sp--); /* free key */

    /* Copy the elements */
    for (i = -num_arg + 1; ++i < 0; )
    {
        /* get_map_lvalue() may return destructed objects. */
        /* TODO: May this cause problems elsewhere, too? */
        if (destructed_object_ref(item))
        {
            assign_svalue(sp[i].u.lvalue, &const0);
            item++;
        }
        else
            /* mapping must not have been freed yet */
            assign_svalue(sp[i].u.lvalue, item++);
        free_svalue(&sp[i]);
    }

    free_svalue(sp--); /* free mapping */
    sp += 3 - num_arg;
    put_number(sp, 1);

    return sp;
} /* f_m_contains() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_m_reallocate (svalue_t *sp)

/* EFUN m_reallocate()
 *
 *    mapping m_reallocate(mapping m, int width)
 *
 * Create a new mapping of width <width> and fill it with the values
 * of mapping <m>. If <m> is narrower than <width>, the extra values
 * in the result will be 0; if <m> is wider, the extra values of <m>
 * will be omitted.
 */

{
    int        new_width;  /* Requested width of the target mapping */
    mapping_t *m;          /* Argument mapping */
    mapping_t *new_m;      /* New mapping */

    /* Test and get arguments */
    new_width = sp->u.number;
    if (new_width < 0)
    {
        error("Illegal width to m_reallocate(): %ld\n", (long)new_width);
        /* NOTREACHED */
        return sp;
    }

    inter_sp = --sp;

    m = sp->u.map;

    /* Resize the mapping */
    check_map_for_destr(m);
    new_m = resize_mapping(m, new_width);
    if (!new_m)
    {
        error("Out of memory.\n");
        /* NOTREACHED */
        return sp;
    }

    /* Assign and return the result */
    free_svalue(sp);
    put_mapping(sp, new_m);

    return sp;
} /* f_m_reallocate() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_mkmapping (svalue_t *sp, int num_arg)

/* EFUN mkmapping()
 *
 *   mapping mkmapping(mixed *arr1, mixed *arr2,...)
 *
 * Returns a mapping with indices from 'arr1' and values from
 * 'arr2'... . arr1[0] will index arr2...[0], arr1[1] will index
 * arr2...[1], etc. If the arrays are of unequal size, the mapping
 * will only contain as much elements as are in the smallest
 * array.
 */

{
    long i, length, num_values;
    mapping_t *m;
    svalue_t *key;

    /* Check the arguments and set length to the size of
     * the shortest array.
     */
    length = LONG_MAX;
    for (i = -num_arg; ++i <= 0; )
    {
        if ( sp[i].type != T_POINTER )
            vefun_arg_error(i+num_arg, T_POINTER, sp[i].type, sp);
        if (length > (long)VEC_SIZE(sp[i].u.vec))
            length = (long)VEC_SIZE(sp[i].u.vec);
    }

    if (max_mapping_size && length > max_mapping_size)
        error("Illegal mapping size: %ld\n", length);

    /* Allocate the mapping */
    num_values = num_arg - 1;
    m = allocate_mapping(length, num_values);
    if (!m)
        error("Out of memory\n");

    /* Shift key through the first array and assign the values
     * from the others.
     */
    key = &(sp-num_values)->u.vec->item[length];
    while (--length >= 0)
    {
        svalue_t *dest;

        dest = get_map_lvalue_unchecked(m, --key);
        for (i = -num_values; ++i <= 0; )
        {
            /* If a key value appears multiple times, we have to free
             * a previous assigned value to avoid a memory leak
             */
            assign_svalue(dest++, &sp[i].u.vec->item[length]);
        }
    }

    /* Clean up the stack and push the result */
    sp = pop_n_elems(num_arg, sp);
    push_mapping(sp, m);

    return sp;
} /* f_mkmapping() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_unmkmapping (svalue_t *sp)

/* EFUN unmkmapping()
 *
 *   mixed* unmkmapping(mapping map)
 *
 * Take mapping <map> and return an array of arrays with the keys
 * and values from the mapping.
 *
 * The return array has the form ({ keys[], data0[], data1[], ... }).
 */

{
    svalue_t *svp;
    mapping_t *m;
    vector_t *v;
    struct mvf_info vip;
    mp_int size;
    int i;

    /* Get the arguments */
    m = sp->u.map;

    /* Determine the size of the mapping and allocate the result vector */
    check_map_for_destr(m);
    size = (mp_int)MAP_SIZE(m);
    v = allocate_array(m->num_values+1);

    /* Allocate the sub vectors */
    for (i = 0, svp = v->item; i <= m->num_values; i++, svp++)
    {
        vector_t *v2;

        v2 = allocate_array(size);
        put_array(svp, v2);
    }

    /* Copy the elements from the mapping into the vector brush */
    vip.svp = v->item;
    vip.num = 0;
    vip.width = m->num_values;
    walk_mapping(m, m_unmake_filter, &vip);

    /* Clean up the stack and push the result */
    free_mapping(m);
    put_array(sp,v);

    return sp;
} /* f_unmkmapping() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_widthof (svalue_t *sp)

/* EFUN widthof()
 *
 *   int widthof (mapping map)
 *
 * Returns the number of values per key of mapping <map>.
 * If <map> is 0, the result is 0.
 */

{
    int width;

    if (sp->type == T_NUMBER && sp->u.number == 0)
        return sp;

    if (sp->type != T_MAPPING)
        efun_arg_error(1, T_MAPPING, sp->type, sp);

    width = sp->u.map->num_values;
    free_mapping(sp->u.map);
    put_number(sp, width);

    return sp;
} /* f_widthof() */

/***************************************************************************/
