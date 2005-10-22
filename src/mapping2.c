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
 *  - mapping_data_t holds the svalues for keys and associated values
 *  - mapping_keys_t is a condensed index of older keys
 *  - mapping_hash_t is a hashed index of all entries added since the
 *      creation of the mapping_keys_t index.
 *
 * Using this approach, mappings manage to combine a low memory overhead
 * with fast operation. Both the hashed and the condensed index may
 * be absent.
 *
 * Within the data blocks (mapping_data_t), a key and its values forms
 * an entity called 'entry', and the datablocks are always allocated to
 * hold whole entries. The datablocks are chained together in a list,
 * with the most recently allocated one at the head. When entries are
 * deleted, their key svalues are chained together in a freelist, so 
 * that subsequent allocations can reused the space. If the freelist
 * is empty, a new entry is allocated from the yet unused space in the
 * head block.
 *
 * A mapping can have only one mapping_keys_t block, which is a block
 * of pointers to the key-svalues of the indexed data and is created
 * when a mapping is compacted. The pointers are kept in sorted order,
 * with the exeception that deleted entries receive a NULL pointer in
 * the key block. 
 *
 * A mapping can have only one mapping_hash_t block,  which is used
 * to record all the new additions to the mapping since the last
 * compaction. While the new entries' data is added directly to the 
 * mappings data blocks, the key pointers are kept in a hash table
 * in the mapping_hash_t structure. The hash table grows with the
 * number of hashed entries, so that the average chain length is
 * no more than 2. For easier computations,the number of buckets
 * is always a power of 2.
 *
 * All mappings with a hash_mapping structure are considered 'dirty'
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
 *       mapping_data_t * data;
 *       svalue_t       * free;
 *       mapping_keys_t * keys;
 *       mapping_hash_t * hash;
 *   }
 *
 *   .ref is the number of references, as usual.
 *
 *   .user is, as usual, the wizlist entry of the owner object.
 *
 *   .num_values and .num_entries give the width (including the key!)
 *   and number of valid entries in the mapping.
 *
 *   .data is a list of of data blocks holding all the svalues for keys
 *   and values. Only the first block in this list may have unused
 *   entries outside the freelist.
 *   If .data is NULL, the mapping is factually empty, but
 *   not yet deallocated because it is member of the dirtylist.
 *   Free entries in .data are chained together in a freelist and anchored
 *   at .free.
 *
 *   .keys and .hash are the condensed resp. hashed index to the mapping
 *   keys. .hash also serves as indicator if the mapping is 'dirty',
 *   and therefore contains all the information about the dirtyness.
 *   During the garbage collection, .hash is used for a temporary list
 *   of mappings with 'stale' keys (ie keys referencing destructed objects
 *   or lambdas).
 *
 * -- mapping_data_t --
 *
 *   mapping_data_t {
 *       mapping_data_s * next;
 *       size_t           size;
 *       size_t           first;
 *       svalue_t         data[.size]
 *   }
 *
 *   This structure keeps the actual mapping data in .data. The list of
 *   datablocks is linked together through the .next pointer.
 *
 *   .size is the number of svalues in this block, which is always
 *   a multiple of mapping->num_values;
 *
 *   .first is the first used(!) svalue in the .data[]. The svalues
 *   are allocated from the end downwards, and only the first block
 *   in the mapping's chain can have a .first != 0.
 *
 *   Deleted entries in the blocks are kept in the freelist anchored
 *   in mapping->free. The list is chained through the former key svalues'
 *   .u.lvalue pointer, the key svalues have a type of T_INVALID. The
 *   data values associated with such a deleted key are set to svalue 0.
 *
 * -- mapping_keys_t --
 *
 *   mapping_keys_t {
 *       size_t    size;
 *       svalue_t *data[.size];
 *   }
 *
 *   This structure holds the compacted key indices ("keys") for a mapping.
 *   Every 'key' (entry in .data[]) points to the key svalue in one
 *   of the mappings' datablocks. The keys are kept in sorted order so
 *   that a binary search is possible. If an entry is deleted, its
 *   key is set to NULL (and mapping->hash->condensed_deleted is incremented),
 *   but otherwise left in place.
 *
 *   .size is the number of entries in the .data[] array, valid and deleted.
 *
 * -- mapping_hash_t --
 *
 *   hash_mapping_t {
 *       p_int        mask;
 *       p_int        used;
 *       p_int        condensed_deleted;
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
 *       svalue_t *key;
 *   }
 *
 *   .next is the next struct map_chain in the hash chain (or .deleted list).
 *   .key points to the key value of the entry, with the data svalue following
 *   right behind.
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
typedef struct mapping_data_s mapping_data_t;
typedef struct mapping_keys_s mapping_keys_t;
typedef struct mapping_hash_s mapping_hash_t;
typedef struct map_chain_s    map_chain_t;


/* --- struct mapping_data_s: the mapping data ---
 * 
 * This structure holds the svalues of keys and associated values.
 * It is always allocated to hold a multiple of (mapping->num_values+1)
 * svalues.
 *
 * The svalues are used from the end, .first gives the index of the first
 * used entry. All entries before .first are free, but not in the
 * mappings freelist. Free entries in the middle of the used area have
 * their key value set to T_INVALID and are chained through its .lvalue
 * pointer, the value svalues are set to svalue-0.
 */

struct mapping_data_s {
    mapping_data_s * next;    /* Next data block */
    size_t           size;    /* Number of svalues allocated */
    size_t           first;   /* Index of first used svalue, counting down */
    svalue_t         data[1]; /* The .size svalues */
};

#define SIZEOF_MD(dm) ( \
    sizeof(*(dm)) + sizeof(svalue_t) * ((dm)->size - 1) \
                      )
  /* Allocation size of a given mapping_data_t structure. */

#define MAPPING_DATA_SIZE  16
  /* Default number of entries for a new mapping_data_t block. */


/* --- struct mapping_keys_s: the condensed index ---
 *
 * When a mapping is compacted, pointers to the key svalues are stored
 * in sorted order in this structure (allocated to size).
 * If later a key is deleted, the associated index pointer is set to NULL.
 */

struct mapping_keys_s {
    size_t    size;                 /* Number of keys indices */
    svalue_t *data[1 /* .size */];  /* The key pointers */
};

#define SIZEOF_MK(km) ( \
    sizeof(*(km)) + sizeof(svalue_t*) * ((km)->size - 1) \
                      )
  /* Allocation size of a given mapping_keys_t structure. */


/* --- struct mapping_hash_s: the hashed index ---
 *
 * New entries in a mapping are stored in a hash structure for fast
 * access. The hash grows dynamically with the number of entries - 
 * the structure is then reallocated to fit - the goal is to keep
 * on average two entries per hash bucket.
 */

struct mapping_hash_s {
    p_int         used;        /* Number of entries in the hash */
    p_int         mask;
      /* Index mask for chains[], converting the raw hash value into
       * the valid index number using a bit-and operation.
       * Incremented by one, it's the number of chains.
       */
    p_int         ref;
      /* Refcount if this mapping is part of a T_PROTECTOR_MAPPING svalue.
       * The value is <= the mappings main refcount.
       */
    mapping_t   * next_dirty;  /* Next dirty mapping in the list */
    p_int condensed_deleted;
      /* Number of entries deleted from the condensed part
       */
    p_int ref;
      /* Refcount if this mapping is part of a T_PROTECTOR_MAPPING svalue.
       * The value is <= the mappings main refcount.
       */
    map_chain_t *deleted;
      /* Protector mappings only: list of deleted entries, which are kept
       * pending because the they may still be used as destination for
       * a lvalue.
       */
    map_chain_t * chains[ 1 /* +.mask */ ];
      /* The hash chain heads ('hash buckets')
       */
};

#define SIZEOF_MH(hm) ( \
    sizeof(*(hm)) + sizeof(map_chain_t *) * ((hm)->mask + 1) \
                  + sizeof(map_chain_t) * ((hm)->used) \
                      )
  /* Allocation size of a given mapping_hash_t structure. */


/* --- struct map_chain_s: one entry in a hash chain ---
 *
 * Like the mapping_keys_t, this structure only holds a pointer to
 * the key value in the mapping_data_t.
 */

struct map_chain_s {
    map_chain_t * next;  /* next entry */
    svalue_t    *key;    /* the key value */
};


/*-------------------------------------------------------------------------*/

#define EMPTY_MAPPING_THRESHOLD 2000
  /* Number of 'freed' empty mappings allowed in the dirty list
   * at any time. This way the dirty list can be single-linked only
   * and still allow fast 'freeing' of unused mappings.
   */

static mapping_hash dirty_mapping_head_hash;
  /* Auxiliary structure dirty_mapping_head can reference
   */

static mapping_t dirty_mapping_head
  = {
    /* ref         */ 1,
    /* user        */ NULL,
    /* num_values  */ 0,
    /* num_entries */ 0,
    /* data        */ NULL,
    /* free        */ NULL,
    /* keys        */ NULL,
    /* hash        */ &dirty_mapping_head_hash,
    /* condensed  */ 0,
    /* user       */ 0,
    /* num_values */ 0
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

static mp_int empty_mapping_load = 2*-EMPTY_MAPPING_THRESHOLD;
  /* The load of the dirty mapping list with empty mappings, weighted
   * against the number of non-empty mappings. If the load raises
   * over -EMPTY_MAPPING_THRESHOLD, the empty mappings are removed.
   */

static mp_int empty_mapping_base = 0;
  /* The number of dirty mappings at the time of the last call
   * to remove_empty_mappings(). This value is used to compute
   * empty_mapping_load.
   */

mapping_t *stale_mappings;
  /* During a garbage collection, this is a list of mappings with
   * keys referencing destructed objects/lambdas. Since during this
   * phase all mappings are compacted, the list is linked through
   * the .hash pointers.
   */

static svalue_t walk_mapping_string_svalue
  = { T_STRING };
  /* Stand-in svalue for string-keys, to be passed to the callback
   * function when doing a walk_mapping().
   */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
new_map_entry (mapping_t * m)

/* Return a fresh block of svalues for a new entry in mapping <m>, allocating
 * a new mapping_data_t if necessary.
 *
 * Return NULL if out of memory.
 */

{
    svalue_t *rc;

    if (m->free)
    {
        /* Can take one of the freelist */

        rc = m->free;
        m->free = rc->u.lvalue;
        put_number(rc, 0); /* The values are already 0 */
    }
    else
    {
        /* Have to take one off the freespace in the first block */

        int i;

        if (!m->data || m->data->first == 0)
        {
            /* Well, first we need a first block with free space */
            mapping_data_t * mdata;

            size_t size = (size_t)m->num_values * MAPPING_DATA_SIZE;

            mdata = xalloc(  sizeof(*mdata)
                           + sizeof(svalue_t) * (size - 1)
                          );
            if (!mdata)
                return NULL;

            mdata->size = size;
            mdata->first = size;
            mdata->next = m->data;
            m->data = mdata;

            /* Statistics */
            m->user->mapping_total += SIZEOF_MD(dm);
        }

        m->data->first -= m->num_values;
        rc = &m->data->data[m->data->first];

        for (i = m->num_values-1; i >= 0; i--)
            put_number(rc+i, 0);
    }

    return rc;
} /* new_map_entry() */

/*-------------------------------------------------------------------------*/
static INLINE void
free_map_entry (mapping_t * m, svalue_t *entry, Bool no_data)

/* Free the svalue entry block <entry> of mapping <m>.
 * If <no_data> is TRUE, the svalues themselves are supposed to be empty.
 */

{
    size_t ix;

    for (ix = m->num_values-1; ix >= 0; ix--)
    {
        if (!no_data)
            free_svalue(entry+ix);
        put_number(entry+ix, 0);
    }

    /* If this entry is the most recently allocated one, return it
     * to its block, otherwise into the freelist.
     */
    if (m->data && m->data->data+m->data->first == entry)
    {
        m->data->first += m->num_values;
    }
    else
    {
        entry->type = T_INVALID;
        entry->u.lvalue = m->free;
        m->free = entry;
    }
} /* free_map_entry() */

/*-------------------------------------------------------------------------*/
static mapping_t *
get_new_mapping ( wiz_list_t * user, mp_int num_values
                , mp_int data_size, mp_int hash_size, mp_int key_size)

/* Allocate a basic mapping with <num_values> values per key, and set it
 * up to have an initial datablock of <data_size> entries, a hash
 * suitable for <hash_size> entries, and a keyblock for <key_size> entries.
 * The .user is of the mapping is set to <user>.
 *
 * Return the new mapping, or NULL when out of memory.
 */

{
    mapping_data_t *dm;
    mapping_hash_t *hm;
    mapping_keys_t *km;
    mapping_t *m;

    /* Allocate the structures */
    m = xalloc(sizeof *m);
    if (!m)
        return NULL;

    /* Set up the data block for <data_size> entries */

    dm = NULL;
    if (data_size > 0)
    {
        size_t dm_size = (size_t)data_size;

        dm_size *= num_values + 1;

        dm = xalloc(sizeof(*dm) + sizeof(svalue_t) * (dm_size - 1));
        if (!dm)
        {
            xfree(m);
            return NULL;
        }

        dm->next = NULL;
        dm->size = dm_size;
        dm->first = dm_size;
    }

    /* Set up the key block for <key_size> entries */

    km = NULL;
    if (key_size > 0)
    {
        size_t km_size = (size_t)key_size;

        km = xalloc(sizeof(*km) + sizeof(svalue_t *) * (km_size - 1));
        if (!km)
        {
            if (dm) xfree(dm);
            xfree(m);
            return NULL;
        }

        km->size = km_size;
    }

    /* Set up the hash block for <hash_size> entries */

    hm = NULL;
    if (hash_size > 0)
    {
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
        if (size & ~0xff)
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
            if (km) xfree(km);
            if (dm) xfree(dm);
            xfree(m);
            return NULL;
        }

        hm->mask = hash_size;
        hm->used = hm->condensed_deleted = hm->ref = 0;

        /* With this hash_mapping structure, the mapping counts
         * as potentially dirty.
         */
        last_dirty_mapping->hash->next_dirty = m;
        last_dirty_mapping = m;

        /* These members don't really need a default initialisation
         * but it's here to catch bogies.
         */
        hm->next_dirty = NULL;
        hm->deleted = NULL;

        num_dirty_mappings++;

        /* Inform backend that there is a new mapping to condense */
        extra_jobs_to_do = MY_TRUE;

        /* Initialise the hashbuckets */
        mcp = hm->chains;
        do *mcp++ = NULL; while (--size >= 0);
    }

    /* Initialise the mapping */

    m->data = dm;
    m->keys = km;
    m->hash = hm;
    m->num_values = num_values+1;
    m->num_entries = 0;
    m->ref = 1;

    /* Statistics */
    (m->user = user)->mapping_total += sizeof *m;
    if (dm)
        m->user->mapping_total += SIZEOF_MD(dm);
    if (hm)
        m->user->mapping_total += SIZEOF_MH(hm);
    if (km)
        m->user->mapping_total += SIZEOF_MK(km);

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
    mp_int dm_size;
    mapping_t *m;

    dm_size = size;
    if (dm_size)
        dm_size = MAPPING_DATA_SIZE;

    return get_new_mapping(current_object->user, num_values, dm_size, size, 0);
} /* allocate_mapping() */

/*-------------------------------------------------------------------------*/
void
_free_mapping (mapping_t *m, Bool no_data)

/* Aliases: free_mapping(m)       -> _free_mapping(m, FALSE)
 *          free_empty_mapping(m) -> _free_mapping(m, TRUE)
 *
 * The mapping and all associated memory is deallocated resp. dereferenced.
 * If <no_data> is TRUE, all the svalues are assumed to be freed already
 * (the swapper uses this after swapping out a mapping).
 *
 * If the mapping is 'dirty' (ie. contains a hash_mapping part), it
 * is not deallocated immediately, but instead counts 2 to the empty_mapping-
 * _load (with regard to the threshold).
 */

{
    struct mapping_hash *hm;  /* Hashed part of <m> */

#ifdef DEBUG
    if (!m)
        fatal("NULL pointer passed to free_mapping().\n");

    if (!m->user)
        fatal("No wizlist pointer for mapping");

    if (!no_data && m->ref > 0)
        fatal("Mapping with %ld refs passed to _free_mapping().\n", m->ref);
#endif

    num_mappings--;

    /* Free all svalues and datablocks of this mapping */
    while (m->data != NULL)
    {
        mapping_data_t *ptr;
        size_t ix;

        ptr = m->data; m->data = ptr->next;
        for (ix = ptr->first; !no_data && ix < ptr->size; ix++)
        {
            free_svalue(&(ptr->data[ix]));
        }

        /* Statistics */
        m->user->mapping_total -= SIZEOF_MD(ptr);

        xfree(ptr);
    }

    /* Free the condensed key indices */
    if (m->keys != NULL)
    {
        m->user->mapping_total -= SIZEOF_MK(m->keys);
        xfree(m->keys);
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
        m->user->mapping_total -= SIZEOF_MH(hm);

        mcp = hm->chains;

        /* Loop through all chains */

        i = hm->mask + 1;
        do {

            /* Free this chain */

            for (next = *mcp++; NULL != (mc = next); )
            {
                next = mc->next;
                xfree(mc);
            }
        } while (--i);

        /* Replace this hash_mapping with an empty one and
         * mark the mapping itself as empty.
         */
        next_dirty = hm->next_dirty;
        xfree(hm);

        hm = xalloc(sizeof *hm);
        hm->mask = hm->used = hm->condensed_deleted = hm->ref = 0;
        hm->chains[0] = NULL;
        hm->next_dirty = next_dirty;
        m->user->mapping_total += SIZEOF_MH(hm);

        m->hash = hm;
        m->data = NULL;
        m->keys = NULL;
        m->num_entries = 0;

        /* Count this new empty mapping, removing all empty
         * mappings if necessary
         */
        if ( (empty_mapping_load += 2) > 0)
            remove_empty_mappings();
    }
    else
    {
        /* No hash: free the base structure.
         */

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

    for (m = &dirty_mapping_head, i = num_dirty_mappings; --i >= 0; )
    {
        m = m->hash->next_dirty;
    }
    if (m != last_dirty_mapping)
        fatal("last_dirty_mapping not at end of dirty list\n");
    if (m->hash->next_dirty)
        fatal("dirty mapping list not terminated\n");
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

    empty_mapping_load += empty_mapping_base - num_dirty_mappings;
    empty_mapping_base = num_dirty_mappings;
    if (empty_mapping_load <= -EMPTY_MAPPING_THRESHOLD)
        return;

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
        if (!m->data)
        {
            m->user->mapping_total -= sizeof(*m) - SIZEOF_MH(hm);
            xfree(m);
            *mp = m = hm->next_dirty;
            xfree(hm);
            continue;
        }
        last = m;
        mp = &hm->next_dirty;
        m = *mp;
    } while (m);
    last_dirty_mapping = last;


    /* Adjust the counters */

    num_dirty_mappings -=
      (empty_mapping_load + 2*EMPTY_MAPPING_THRESHOLD + empty_mapping_base) >> 1;
    empty_mapping_load = 2*-EMPTY_MAPPING_THRESHOLD - empty_mapping_base;

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
        svalue_t *svp2;

        for (mc = hm->deleted; mc; mc = next)
        {
            free_map_entry(m, mc->key, MY_FALSE);
            next = mc->next;
            xfree(mc);
        }
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

    return left->x.generic - right.x.generic;
} /* mcompare() */

/*-------------------------------------------------------------------------*/
static INLINE mp_int
mhash (svalue_t * svp)

/* Compute and return the hash value for svalue *<svp>.
 */

{
    mp_int i;

    if (svp->type != T_CLOSURE
     || (   map_index->x.closure_type != CLOSURE_LFUN
         && map_index->x.closure_type != CLOSURE_ALIEN_LFUN
         && map_index->x.closure_type != CLOSURE_IDENTIFIER )
       )
    {
        i = svp->u.number ^ *SVALUE_FULLTYPE(svp);
    }
    else
    {
        i = (p_int)(map_index->u.lambda->ob) ^ *SVALUE_FULLTYPE(svp);
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
 * If the key was found in the condensed key index, *<pKeys> will be set
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
    mapping_keys_t *km = m->keys;

    *pKeys = -1;
    *ppChain = NULL;

    /* If the key is a string, make it tabled */
    if (!mstr_tabled(map_index->u.str))
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

    closure_index = (map_index->type == T_CLOSURE);

    /* Search in the condensed part first.
     */

    if (m->keys && km->size != 0)
    {
        mapping_keys_t *km = m->keys;
        mp_int size = km->size;
        svalue_t **key, ** keystart, ** keyend;

        keystart = &km->data[0];
        keyend = keystart + size;

        /* Skip eventual deleted entries at start or end */
        while (size > 0 && *keystart == NULL)
        {
            keystart++;
            size--;
        }

        while (size > 0 && keyend[-1] == NULL)
        {
            keyend++;
            size--;
        }

        while (keyend > keystart)
        {
            int cmp;

            key = (keyend - keystart) / 2 + keystart;

            while (key > keystart && *key == NULL)
                key--;

            cmp = compare(map_index, *key);
            
            if (cmp)
            {
                /* Found it */
                *pKeys = (p_int)(key - &(km->data[0]));
                return key;
            }

            if (cmp < 0)
            {
                /* The map_index value is after key */
                for ( keystart = key+1
                    ; keystart < keyend && *keystart == NULL
                    ; keystart++)
                  NOOP;
            }
            else
            {
                /* The map_index value is before key */
                for ( keyend = key
                    ; keystart < keyend && keystart[-1] == NULL
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

        mp_int index = mhash(map_index) & hm->mask;

        /* Look for the value in the chain determined by index */

        for (mc = hm->chains[index]; mc != NULL; mc = mc->next)
        {
            if (0 == compare(mc->key, map_index))j
            {
                /* Found it */
                *ppChain = mc;
                return mc->key;
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
    mp_int           index;
    svalue_t       * entry;

    entry = find_map_entry(map_index, (p_int *)&index, &mc);

    /* If we found the entry, return the values */
    if (entry != NULL)
        return entry+1;

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
    entry = new_map_entry(m);
    if (NULL == entry)
        return NULL;

    /* If the mapping has no hashed index, create one with just one
     * chain and put the new entry in there.
     */

    if ( !(hm = m->hash) )
    {
        /* Create the hash part of the mapping and put
         * it into the dirty list.
         */

        hm = xalloc(sizeof *hm);
        if (!hm)
        {
            free_map_entry(m, entry, MY_FALSE);
            return NULL; /* Oops */
        }

        hm->mask = hm->condensed_deleted = 0;
        hm->ref = 0;
        hm->used = 0;
        last_dirty_mapping->hash->next_dirty = m;
        last_dirty_mapping = m;

        /* These members don't really need a default initialisation
         * but it's here to catch bogies.
         */
        hm->next_dirty = NULL;
        hm->deleted = NULL;

        num_dirty_mappings++;
        extra_jobs_to_do = MY_TRUE;  /* there are mappings to condense! */
        m->hash = hm;

        m->user->mapping_total += SIZEOF_MH(hm);

        /* Now create the hashing structure with one empty entry */
        mc = xalloc(*mc);
        hm->chains[0] = mc;
        if (!mc)
        {
            /* Keep the empty mapping_hash_t around */
            free_map_entry(m, entry, MY_FALSE);
            return NULL;
        }
        mc->next = NULL;
        mc->key = entry;
        assign_svalue_no_free(entry, map_index);


        hm->used++;
        m->num_entries++;
        m->user->mapping_total += sizeof(*mc);

        return entry+1;
    }

    /* The hashed index exists, so we can insert the new entry there.
     *
     * However, if the average number of map_chains per chain exceeds 2,
     * double the size of the bucket array first.
     */
    if (hm->used & ~hm->mask<<1)
    {
        mapping_hash_t *hm2;
        mp_int mask, j;
        map_chain_t **mcp, **mcp2, *next;

        hm2 = hm;

        /* Compute new size and mask, and allocate the structure */

        size = (hm->mask << 1) + 2;
        mask = size - 1;

        hm = xalloc(sizeof *hm - sizeof *mcp + sizeof *mcp * size);
        if (!hm)
        {
            free_map_entry(m, entry, MY_FALSE);
            return NULL;
        }

        m->user->mapping_total += SIZEOF_MH(hm) - SIZEOF(hm2);

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
            for (mc = *mcp2++; mc; mc = next)
            {
                next = mc->next;
                index = mhash(mc->key) & mask;
                mc->next = mcp[index];
                mcp[index] = mc;
            }
        }
        m->hash = hm;

        /* Away, old data! */

        xfree(hm2);
    }

    /* Finally, insert the new entry into its chain */

    index = mhash(map_index) & hm->mask;
    mc = xalloc(sizeof(*mc));
    if (!mc)
    {
        free_map_entry(m, entry, MY_FALSE);
        return NULL;
    }

    hm->used++;
    mc->next = hm->chains[index];
    mc->key = entry;
    hm->chains[index] = mc;
    assign_svalue_no_free(entry, map_index);
    
    m->user->mapping_total += sizeof(*mc);
    m->num_entries++;

    return entry+1;
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
    mapping_keys_t *km;
    mapping_hash_t *hm;

    num_values = m->num_values;

    /* Scan the condensed part for destructed object references used as keys.
     */

    if (NULL != (km = m->keys))
    {
        size_t ix;

        for (ix = 0; ix < km->size; ++ix)
        {
            svalue_t *entry = km->data[ix];
            int i;

            if (NULL == entry)
                continue;

            if (destructed_object_ref(entry))
            {
                /* Destructed key: remove the whole entry */
                m->num_entries--;
                free_map_entry(m, entry, MY_FALSE);
                km->data[ix] = NULL;

                /* Count the deleted entry in the hash part.
                 * Create it if necessary.
                 */
                if ( !(hm = m->hash) )
                {
                    hm = xalloc(sizeof *hm);
                    if (!hm)
                    {
                        outofmem(sizeof *hm, "hash mapping");
                        /* NOTREACHED */
                        return;
                    }
                    m->hash = hm;
                    hm->mask = hm->used = hm->condensed_deleted = hm->ref = 0;
                    hm->chains[0] = 0;
                    last_dirty_mapping->hash->next_dirty = m;
                    last_dirty_mapping = m;
#ifdef DEBUG
                    hm->next_dirty = NULL;
                    hm->deleted = NULL;
#endif
                    num_dirty_mappings++;
                    extra_jobs_to_do = MY_TRUE;

                    m->user->mapping_total += sizeof(*hm);
                }

                hm->condensed_deleted++;

                continue;
            }

            /* Scan the values for destructed object refs and zero
             * those found out.
             */
            for (i = num_values-1; i > 0; --i)
            {
                ++entry;
                if (destructed_object_ref(entry))
                {
                    assign_svalue(entry, &const0);
                }
            }
        } /* for (all keys) */
    } /* if (m->keys) */

    /* If it exists, scan the hash part for destructed objects.
     */

    if ( NULL != (hm = m->hash) )
    {
        map_chain_t **mcp, **mcp2, *mc;

        /* Walk all chains */

        for (mcp = hm->chains, i = hm->mask + 1; --i >= 0;)
        {
            /* Walk this chain */

            for (mcp2 = mcp++; NULL != (mc = *mcp2); )
            {
                /* Destructed object as key: remove entry */

                if (destructed_object_ref(mc->key))
                {
                    svalue_t * entry = mc->key;
                    int i;

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
                        free_map_entry(m, entry, MY_FALSE);
                        xfree(mc);
                    }
                    m->user->mapping_total -= sizeof(*mc);
                    hm->used--;
                    continue;
                }

                /* Scan the values of this entry (not reached
                 * if the entry was removed above
                 */
                for (entry++, i = num_values-1; i >= 0; --i, ++entry)
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
    p_int       * key_ix;
    svalue_t    * entry;
    map_chain_t * mc;

    entry = find_map_entry(m, &key_ix, &mc);

    if (NULL != entry)
    {
        /* The entry exists - now remove it */

        m->num_entries--;

        if (key_ix >= 0)
        {
            /* The entry is in the condensed part */

            free_map_entry(m, entry, MY_FALSE);
            km->data[key_ix] = NULL;

            /* Count the deleted entry in the hash part.
             * Create it if necessary.
             */
            if ( !(hm = m->hash) )
            {
                hm = xalloc(sizeof *hm);
                if (!hm)
                {
                    outofmem(sizeof *hm, "hash mapping");
                    /* NOTREACHED */
                    return;
                }
                m->hash = hm;
                hm->mask = hm->used = hm->condensed_deleted = hm->ref = 0;
                hm->chains[0] = 0;
                last_dirty_mapping->hash->next_dirty = m;
                last_dirty_mapping = m;
#ifdef DEBUG
                hm->next_dirty = NULL;
                hm->deleted = NULL;
#endif
                num_dirty_mappings++;
                extra_jobs_to_do = MY_TRUE;

                m->user->mapping_total += sizeof(*hm);
            }

            hm->condensed_deleted++;
        }
        else if (mc != NULL && NULL != (hm = m->hash))
        {
            /* The key is in the hash mapping */

            map_chain_t prev;
            mp_int index = mhash(entry) & hm->mask;

            for ( prev = 0, mc = hm->chains[index]
                ; mc != NULL && mc->key != entry
                ; prev = mc, mc = mc->next)
                NOOP;

            if (mc == NULL)
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
                free_map_entry(m, entry, MY_FALSE);
                xfree(mc);
            }
            m->user->mapping_total -= sizeof(*mc);
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
    mapping_keys_t * km, *km2 = NULL;
    mp_int common_width;  /* == min(num_values, new_width+1) */

    /* Set the width variables */
    new_width++;
    if (m->num_values >= new_width)
    {
        common_width = new_width;
    }
    else
    {
        common_width = m->num_values;
    }

    /* Get the target mapping without a hash, but with a data block
     * big enough to hold all entries.
     */

    {
        p_int km_size = 0;
        if (m->keys)
        {
            km_size = m->keys->size;
            if (m->hash)
                km_size -= m->hash->condensed_deleted;
        }
        m2 = get_new_mapping(m->user, new_width-1, m->num_entries, 0, km_size);
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
        hm2->condensed_deleted = 0;
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
                svalue_t * entry, *svp;

                mc2 = (struct map_chain *)xalloc((size_t)linksize);
                if (!mc2)
                {
                    xfree(hm2);
                    outofmem(sizeof(*mc), "hash link");
                    /* NOTREACHED */
                    return NULL;
                }

                /* Copy the key and the common values */
                entry = new_map_entry(m2);
                if (entry == NULL)
                {
                    xfree(hm2);
                    outofmem(m->num_values * sizeof(*entry), "data block");
                    /* NOTREACHED */
                    return NULL;
                }
                mc2->key = entry;

                i = common_width;
                svp = mc->key;
                do {
                    assign_svalue_no_free(entry++, svp++);
                } while (--i > 0);

                mc2->next = last;
                last = mc2;
            }
            *mcp2++ = last;
        } while (--size);

        /* Plug the new hash into the new mapping */
        m2->hash = hm2;
        m->user->mapping_total += SIZEOF_MH(hm2);
    }


    /* --- Copy the condensed part ---
     */

    if (NULL != (km = m->keys))
    {
        size_t ix, ix2;

        /* The new key block is already in the mapping */
        km2 = m2->km;

        for (ix = ix2 = 0; ix < km->size; ix++)
        {
            svalue_t * key = km->data[ix];

            if (key != NULL)
            {
                svalue_t * entry = new_map_entry(m2);
                mp_int i;

                if (entry == NULL)
                {
                    xfree(km2);
                    outofmem(m->num_values * sizeof(*entry), "data block");
                    /* NOTREACHED */
                    return NULL;
                }

#if DEBUG
                if (ix2 >= km2->size)
                    fatal("Destination index out of range.\n");
#endif
                km2->data[ix2++] = entry;

                /* Copy the data from the original entry */
                i = common_width;
                do {
                    assign_svalue_no_free(entry++, key++);
                } while (--i > 0);
            }
        } /* for (all keys) */
    }


    /* --- Finalize the basis structure ---
     */

    if ( NULL != m2->hash )
    {
        num_dirty_mappings++;
        last_dirty_mapping->hash->next_dirty = m2;
        last_dirty_mapping = m2;
    }
    m2->num_entries = m->num_entries;

    num_mappings++;

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
 * with static limits. To achieve this, all deleted entries are copied
 * together with the live entries, furthermore, entries from m1
 * overwritten by m2 are given virtually deleted entries in m3.
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
    mapping_t * m3;
      /* The result mapping */
    mapping_hash_t * hm;
    p_int km3size;

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
             , (long)num_values-1, (long)m2->num_values-1);
    }


    /* Allocate the result mapping *m3 and initialise it.
     */

    {
        p_int hsize = 0;

        if (m1->hash) hsize += m1->hash->used;
        if (m2->hash) hsize += m2->hash->used;

        km3size = 0;
        if (m1->keys) km3size += m1->keys->size;
        if (m2->keys) km3size += m2->keys->size;

        m3 = get_new_mapping( m->user, num_values-1
                            , m1->num_entries + m2->num_entries
                            , hsize, km3size);

        if (!m3)
        {
            outofmem(sizeof *m3 + sizeof(svalue_t) * m->num_entries * new_width
                    , "result mapping base structure");
            /* NOTREACHED */
            return NULL;
        }
    }

    /* Merge the condensed entries.
     * Since the keys are sorted, a simple walk through both mappings
     * in parallel with proper selection does the trick.
     */

    {
        mapping_keys_t *km1, *km2, *km3;
        size_t km1size, km2size;
        size_t ix1, ix2, ix3;

        km1 = m1->keys;
        km1size = km1 ? km1->size : 0;

        km2 = m2->keys;
        km2size = km2 ? km2->size : 0;

        km3 = m3->keys;

        /* Loop over the mappings in parallel */
        for ( ix1 = ix2 = ix3 = 0
            ; ix1 < km1size && ix2 < km2size
            ; NOOP )
        {
            svalue_t *entry1, *entry2, *entry3;
            int cmp, i;

            entry1 = km1->data[ix1];
            if (!entry1)
            {
                ix1++;
                continue;
            }

            entry2 = km2->data[ix2];
            if (!entry2)
            {
                ix2++;
                continue;
            }

            cmp = compare(entry1, entry2);

            /* Get the new entry block */
            entry3 = new_map_entry(m3);
            if (!entry3)
            {
                outofmem(sizeof(entry*) * num_values, "new entry block");
                /* NOTREACHED */
                return NULL;
            }
            km3->data[ix3++] = entry3;
            m3->num_entries++;

            if (cmp < 0)
            {
                /* Get the new entry from m1 */
                for (i = 0; i < num_values; i++)
                    assign_svalue_no_free(entry3+i, entry1+i);
                ix1++;
            }
            else if (cmp > 0)
            {
                /* Get the new entry from m2 */
                for (i = 0; i < num_values; i++)
                    assign_svalue_no_free(entry3+i, entry2+i);
                ix2++;
            }
            else
            {
                /* Get the new entry from m2, but pretend it overwrote
                 * the entry from m1.
                 */
                for (i = 0; i < num_values; i++)
                    assign_svalue_no_free(entry3+i, entry2+i);
                ix1++;
                ix2++;
                m3->hash->condensed_deleted++;
            }
        } /* for(mappings in parallel) */

        /* Copy remaining values from m1 */
        for ( ; ix1 < km1size; ix1++)
        {
            svalue_t *entry1, *entry3;

            entry1 = km1->data[ix1];
            if (entry1)
            {
                entry3 = new_map_entry(m3);
                if (!entry3)
                {
                    outofmem(sizeof(entry*) * num_values, "new entry block");
                    /* NOTREACHED */
                    return NULL;
                }
                km3->data[ix3++] = entry3;
                m3->num_entries++;

                for (i = 0; i < num_values; i++)
                    assign_svalue_no_free(entry3+i, entry1+i);
            }
        } /* for (remaining values in m1) */

        /* Copy remaining values from m2 */
        for ( ; ix2 < km2size; ix2++)
        {
            svalue_t *entry2, *entry3;

            entry2 = km2->data[ix2];
            if (entry2)
            {
                entry3 = new_map_entry(m3);
                if (!entry3)
                {
                    outofmem(sizeof(entry*) * num_values, "new entry block");
                    /* NOTREACHED */
                    return NULL;
                }
                km3->data[ix3++] = entry3;
                m3->num_entries++;

                for (i = 0; i < num_values; i++)
                    assign_svalue_no_free(entry3+i, entry2+i);
            }
        } /* for (remaining values in m2) */

        /* NULL out any remaming entries in km3 */
        for ( ; ix3 < km3size; ix3++)
            km3->data[ix3] = NULL;
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
                svalue_t * entry, * entry3;
                entry = mc->key;
                entry3 = get_map_lvalue_unchecked(m3, entry);
                for (i = num_values; --i > 0; )
                    assign_svalue(entry3++, entry++);
                }
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
                svalue_t * entry, * entry3;
                entry = mc->key;
                entry3 = get_map_lvalue_unchecked(m3, entry);
                for (i = num_values; --i > 0; )
                    assign_svalue(entry3++, entry++);
                }
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
    string_t **str;
    svalue_t *svp, *data;
    mp_int size;
    mp_int num_values;
    mapping_data_t *dm;

    num_values = m->num_values;

    /* Just walk through all the data blocks */
    for (dm = m->data; dm != NULL; dm = dm->next)
    {
        size_t ix;

        for (ix = dm->first; ix < dm->size; ix += num_values)
        {
            svalue_t *entry = &(dm->data[ix]);

            if (entry->type != T_INVALID)
                (*func)(entry, entry+1, extra);
        }
    }

} /* walk_mapping() */


/*-------------------------------------------------------------------------*/
void
compact_mappings (mp_int num)

/* Compact the first <num> mappings in the dirty-mapping list.
 * Compaction means: removal of all deleted entries from the condensed
 * part, merge of all hashed entries into the condensed part,
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
       * Neat sideeffect: all allocations are guaranteed to work (else
       * the driver would terminate).
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
        mapping_keys_t *km;
          /* The condensed part of m */
        int num_values;
          /* Number of values per entry */

        mapping_t *m2;
          /* Temporary holder for the compacted result mapping */
        mapping_keys_t *km2;
          /* The new condensed part of the mapping */

        map_chain_t *hook1, *hook2;
          /* All hashed entries in two long chains.
           */

        map_chain_t **mcpp, *mcp, *next;
        map_chain_t *last_hash;
          /* Auxiliaries */

        mp_int count1, count2;
          /* The condensed part of m */
        mapping_keys_t *cm2;
          /* The new condensed part of the mapping */

        mp_int string_used, misc_used;
          /* Number of string/misc entries in the hash */

        mp_int runlength;
          /* Current Mergesort partition length */

#ifdef DEBUG
        if (!m->user)
            fatal("No wizlist pointer for mapping\n");
#endif
        m->ref++; /* prevent freeing while using in case of recursive
                   * mappings referenced by a deleted value
                   */

        hm = m->hash;
        km = m->keys;

        if (hm->ref) {
            fatal("compact_mappings(): remaining ref count %ld!\n", hm->ref);
        }

        /* If this is an empty mapping awaiting deallocation,
         * just do that.
         */
        if (NULL == m->data)
        {
            m->user->mapping_total -= sizeof(*m) + SIZEOF_MH(hm);
            xfree(m);
            m = hm->next_dirty;
            xfree(hm);
            empty_mapping_load -= 2;
            continue;
        }

        /* Test if the mapping needs compaction at all.
         * If not, just delete the hash part.
         */
        if (!hm->user && !hm->condensed_deleted && !m->free)
        {
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
         * Get the temporary result mapping (we need it for the
         * datablock).
         */

        num_values = m->num_values;

        m2 = get_new_mapping(m->user, num_values-1, m->num_entries, 0,
                             m->num_entries);
        km2 = m2->keys;

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
            mcp = *mcpp++;
            while (mcp)
            {
                next = mcp->next;

                if (last_hash)
                {
                    p_int d = mcompare(mcp->key, last_hash->key);

                    if (d > 0) {
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
                    p_int d = mcompare(hook1->key, hook2->key);

                    if (d < 0)
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
            size_t ix1;  /* Index into the old keys */
            size_t ix2;  /* Index into the new keys */

            ix1 = ix2 = 0;

            /* Do the actual merge.
             */
            while (hook1 && km && ix1 < km->size)
            {
                svalue_t * key, * entry;
                int d;

                key = km->data[ix1];
                if (!key)
                {
                    ix1++;
                    continue;
                }

                d = mcompare(key, hook1->key);

                entry = new_map_entry(m2);
                km2->data[ix2++] = entry;

                if (d < 0)
                {
                    /* Take entry from hook1 */

                    map_chain_t *temp;
                    int i;

                    for (key = hook1->key, i = num_values; i > 0; --i)
                        *entry++ = *key++;

                    temp = hook1;
                    hook1 = temp->next;
                    xfree(temp);
                }
                else
                {
                    /* Take entry from the old condensed part */

                    int i;

                    for (i = num_values; i > 0; --i)
                        *entry++ = *key++;

                    ix1++;
                }
            } /* if (hook1 && ix1 < km->size) */

            /* Copy any remaining entries from the old condensed part
             * or the misc_hook1
             */
            if (km && ix1 < km->size)
            {
                /* Copy from the old condensed part */

                while (ix1 < km->size)
                {
                    svalue_t * key = km->data[ix1++];
                    
                    if (key != NULL)
                    {
                        svalue_t * entry;
                        int i;

                        entry = new_map_entry(m2);
                        km2->data[ix2++] = entry;
                        for (i = num_values; i > 0; --i)
                            *entry++ = *key++;
                    }
                }
            }
            else
            {
                /* Copy from hook1 */

                while (hook1)
                {
                    map_chain_t *temp;
                    svalue_t *key, *data;
                    int i;

                    entry = new_map_entry(m2);
                    km2->data[ix2++] = entry;
                    for (key = hook1->key,i = num_values; i > 0; --i)
                        *entry++ = *key++;

                    temp = hook1;
                    hook1 = temp->next;
                    xfree(temp);
                }
            }
        } /* --- End of Merge --- */

        /* Switch the new key and data blocks from m2 to m, and
         * vice versa for the old ones. We don't assign the hash block
         * as we already deleted all the map_chain structures.
         */
        m->keys = km2;
        m2->keys = km;
        {
            mapping_data_t * dm2 = m2->data;
            m2->data = m1->data;
            m->data = dm2;
        }
        m->free = NULL; /* We just compacted it */
        m->hash = NULL; /* dito */

        m->user->mapping_total -= SIZEOF_MH(hm);
          /* This also takes into account the deallocation of the
           * map_chain_t structures.
           */

        free_mapping(m);
          /* Undo the initial m->ref++; if there was a recursive
           * reference which is now gone, the mapping will be deallocated
           * now.
           */

        free_empty_mapping(m2);
          /* Get rid of the temporary mapping and the old key
           * and data blocks.
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

/* Structure used by set_mapping_user() to communicate with ..._filter()
 */
struct set_mapping_user_locals
{
    int        num_values;  /* Number of values per entry */
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
                     + (m->keys) ? SIZEOF_MK(m->keys) : 0
                     + (m->data) ? SIZEOF_MD(m->data) : 0
                    );
    m->user->mapping_total -= total;
    user = owner->user;
    m->user = user;
    m->user->mapping_total += total;


    /* Walk the mapping to set all owners */

    locals.owner = owner;
    locals.num_values = num_values;
    first_hairy = alloca(((m->keys) ? m->keys->size : 1) * sizeof(svalue_t *)); 
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
        source = get_map_value(m, first_hairy);
        if (source != dest)
        {
            if (num_values)
                memcpy((char *)dest, (char *)source, (num_values-1) * sizeof *dest);
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

    size = m->keys->size;
    while ( --size >= 0)
    {
        svalue_t * key = m->keys->data[size];

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

            /* Free the entry block, but don't bother freeing the
             * svalues - this is the GC after all, and freeing them
             * might even confuse the memory allocator.
             */
            free_map_entry(m, key, MY_TRUE);
            m->num_entries--;
            m->keys->data[size] = NULL;

            if (!any_destructed)
            {
                any_destructed = MY_TRUE;
                /* Might be a small mapping. Don't malloc, it might get too
                 * much due to the global scope of garbage_collection.
                 * Since there was a previous
                 * compact_mappings(num_dirty_mappings) , the hash field is
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
            count_ref_in_vector(key, num_values);
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
        mapping_keys_t *km;
        mapping_data_t *dm;
        size_t size;
        mp_int num_values;
        mp_int i;

        /* Unlink from the stale_mapping list */
        next = (mapping_t *)m->hash;
        m->hash = NULL;

        num_values = m->num_values;
        km = m->keys;
        dm = m->data;

        /* Try to reallocate a new key block */

        if (m->num_entries)
        {
            mapping_keys_t *km2;
            size_t ix1, ix2;

            size = sizeof(*km2) + sizeof(svalue_t*) * (m->num_entries-1);
            km2 = xalloc(size);
            if (!km2)
            {
                fprintf(stderr, "%s Unable to compact stale mapping: Out of memory "
                                "for new key block (%ld bytes).\n"
                              , time_stamp(), (long)size);
                debug_message("%s Unable to compact stale mapping: Out of memory "
                              "for new key block (%ld bytes).\n"
                             , time_stamp(), (long)size);

                /* No use in even trying to compact the much bigger data
                 * block either.
                 */
                continue;
            }

            km2->size = m->num_entries;

            /* Copy the keys */
            for (ix1 = ix2 = 0; ix1 < km->size; ix1++)
            {
                svalue_t * key = km->size[ix1];

                if (key != NULL)
                    km2->data[ix2++] = key;
            }

            /* Replace the old keyblock by the new one. */
            m->user->mapping_total += SIZEOF_MK(km2);
            m->keys = km2;
        }
        else
        {
            /* Mapping is empty - no keyblock needed. */
            m->keys = NULL;
        }

        /* Delete the old keyblock */
        m->user->mapping_total -= SIZEOF_MK(km);
        xfree(km);

        km = m->keys;

        /* Now try to compact the data block */

        if (m->num_entries)
        {
            mapping_data_t * dm2;
            size_t ixk, ixd;
            svalue_t * dest;

            size = sizeof(*dm2) + sizeof(svalue_t) * (m->num_entries * num_values - 1);
            dm2 = xalloc(size);

            if (!dm2)
            {
                fprintf(stderr, "%s Unable to compact stale mapping: Out of memory "
                                "for new data block (%ld bytes).\n"
                              , time_stamp(), (long)size);
                debug_message("%s Unable to compact stale mapping: Out of memory "
                              "for new data block (%ld bytes).\n"
                             , time_stamp(), (long)size);

                /* No use in even trying to compact the much bigger data
                 * block either.
                 */
                continue;
            }

            dm2->size = m->num_entries * num_values;
            dm2->first = 0;

            /* Transfer the values from the old data block, key by key */
            for (dest = &(dm2->data[0]), ix = 0 ; ix < km->size ; ix++)
            {
                svalue_t *src = km->data[ix];

                for (i = num_values; i > 0; i--)
                {
                    *dest++ = *src++;
                }
            }

            /* Replace the old datablock by the new one */
            m->user->mapping_total += SIZEOF_MD(dm2);
            m->data = dm2;
            m->free = NULL;
        }
        else
        {
            /* Mapping is empty - no datablock needed. */
            m->data = NULL;
            m->free = NULL;
        }

        /* Delete the old datablocks (though there should
         * be only one)
         */
        while (dm != NULL)
        {
            mapping_data_t *next = dm->next;

            m->user->mapping_total -= SIZEOF_MD(dm);
            xfree(dm);
            dm = next;
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
    if (m->num_values < 2)
        error("m_values() applied on mapping with no values.\n");
    if (num < 0 || num >= m->num_values-1)
        error("Illegal index %d to m_values(): should be in 0..%d.\n"
             , num, m->num_values-2);

    /* Get the size of the mapping */
    check_map_for_destr(m);
    size = (mp_int)MAP_SIZE(m);

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
    for (i = ((mapping_t *)extra)->num_values; --i > 0;)
    {
        assign_svalue(data2++, data++);
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
    if (m2->num_values != m1->num_values)
    {
        /* If one of the two mappings is empty, we can adjust its width
         * after getting rid of all pending data blocks.
         */
        if (0 == m2->num_entries)
        {
            while (m2->data)
            {
                mapping_data_t *dm = m2->data;
                m2->data = dm->next;
                m2->user->mapping_total -= SIZEOF_MD(dm);
                xfree(dm);
            }
            m2->free = NULL;
            m2->num_values = m1->num_values;
        }
        else if (0 == m1->num_entries)
        {
            while (m1->data)
            {
                mapping_data_t *dm = m1->data;
                m1->data = dm->next;
                m1->user->mapping_total -= SIZEOF_MD(dm);
                xfree(dm);
            }
            m1->free = NULL;
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
 * Also called by interpret.c as part of F_SUB_EQ.
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
 * <extra> is a pointer to a (svalue_t *) to an array of (numvalues+1)
 * svalues. The first of these gets to hold the <key>, the others are lvalues
 * to get the <data>.
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
                free_map_entry(m, mc->key, MY_FALSE);
                next = mc->next;
                xfree(mc);
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
    pointers = (svalue_t *)xalloc( (i * 2 + 4) * sizeof(svalue_t) );
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
        for (j = num_values - 1, data = (read_pointer++)->u.lvalue; --j >= 0; )
        {
             (++sp2)->type = T_LVALUE;
             sp2->u.lvalue = data++;
        }

        /* Call the function */
        inter_sp = sp2;
        (void)apply_callback(&cb, 1 + num_values);
    }

    /* This frees the whole array allocated by the prologue,
     * including the data help by the callback.
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

    if (bFull && num_values > 2)
    {
        dvec = allocate_array(num_values-1);
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

    m = allocate_mapping(read_pointer[-2].u.number, num_values-1);
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
            if (num_values == 1)
            {
                push_number(inter_sp, 0);
            }
            else if (2 == num_values)
            {
                push_svalue(read_pointer[1].u.lvalue);
            }
            else
            {
                svalue_t *svp;

                v = read_pointer[1].u.lvalue;
                for (j = 0, svp = dvec->item
                    ; j < num_values-1
                    ; j++, svp++, v++)
                    assign_svalue(svp, v);
                push_svalue(dvec_sp);
            }
        }

        if (!callback_object(&cb))
            error("Object used by %s destructed"
                 , bFull ? "map" : "map_mapping");


        v = apply_callback(&cb, 1 + bFull);

        /* Did the filter return TRUE? */
        if (!v || (v->type == T_NUMBER && !v->u.number) )
            continue;

        /* If we come here, the filter function returned 'true'.
         * Therefore assign the pair to the new mapping.
         */
        v = get_map_lvalue_unchecked(m, read_pointer);
        for (j = num_values-1, data = read_pointer[1].u.lvalue; --j >= 0; )
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

/* EFUN map() on mappings, map_mapping() == map_indices()
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

    if (bFull && num_values > 2)
    {
        dvec = allocate_array(num_values-1);
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
            if (1 == num_values)
                push_number(inter_sp, 0);
            else if (2 == num_values)
            {
                v = get_map_value(arg_m, key);
                push_svalue(v);
            }
            else
            {
                int j;
                svalue_t *svp;

                v = get_map_value(arg_m, key);
                for (j = 0, svp = dvec->item; j < num_values-1; j++, svp++, v++)
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
    if (sp[-1].u.map->num_values-1 != num_arg -2)
        error("Not enough lvalues: given %ld, required %ld.\n"
             , (long)num_arg-2, (long)sp[-1].u.map->num_values-1);

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
    v = allocate_array(m->num_values);

    /* Allocate the sub vectors */
    for (i = 0, svp = v->item; i < m->num_values; i++, svp++)
    {
        vector_t *v2;

        v2 = allocate_array(size);
        put_array(svp, v2);
    }

    /* Copy the elements from the mapping into the vector brush */
    vip.svp = v->item;
    vip.num = 0;
    vip.width = m->num_values-1;
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

    width = sp->u.map->num_values-1;
    free_mapping(sp->u.map);
    put_number(sp, width);

    return sp;
} /* f_widthof() */

/***************************************************************************/

