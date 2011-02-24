/*---------------------------------------------------------------------------
 * Mapping handling functions.
 *
 *---------------------------------------------------------------------------
 * TODO: Rewrite the low-level functions (like allocate_mapping()) to return
 * TODO:: failure codes (errno like) instead of throwing errors. In addition,
 * TODO:: provide wrapper functions which do throw errorf()s, so that every
 * TODO:: caller can handle the errors himself (like the swapper).
 *
 * TODO: A better mapping implementation would utilise the to-be-written
 * TODO:: small block pools. The mapping entries would be unified to
 * TODO:: (hash header:key:values) tuples and stored in a pool.
 * TODO:: The 'compacted' part of the mapping would obviously go away,
 * TODO:: and all indexing would be done through hash table.
 * TODO:: The pool is not absolutely required, but would reduce overhead if
 * TODO:: MALLOC_TRACE is in effect.
 *
 * TODO: Check if the use of mp_int is reasonable for values for num_values
 * TODO::and num_entries (which are in the struct p_int). And check as
 * TODO::the wild mixture of mp_int, p_int, size_t (and maybe still int?)
 * TODO::used for iterating over mapping structures.
 *
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
 * The key values are sorted according to svalue_cmp(), that is in principle
 * by (.type, .u.number >> 1, .x.generic), with the exception of closures and
 * strings which have their own sorting order within their .type.
 *
 * Since the x.generic information is also used to generate the hash value for
 * hashing, for values which don't have a secondary information, x.generic is
 * set to .u.number << 1.
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
 * (and vice versa, only 'dirty' mappings have a mapping_hash_t).
 * During the regular object cleanup, the backend will find and 'clean'
 * the dirty mappings by sorting the hashed entries into the condensed part,
 * removing the hashed part by this.
 *
 * To be compacted, a mapping has to conform to a number of conditions:
 *  - it has been at least TIME_TO_COMPACT seconds (typical 10 minutes)
 *    since the last addition or deletion of an entry
 * and
 *     - it was to be at least 2*TIME_TO_COMPACT seconds (typical 20 minutes)
 *       since the last addition or deletion of an entry
 *  or - the number of condensed-deleted entries is at least half the capacity
 *       of the condensed part
 *  or - the number of hashed entries exceeds the number non-deleted condensed
 *       entries.
 *
 * The idea is to minimize reallocations of the (potentially large) condensed
 * block, as it easily runs into fragmentation of the large block heap.
 *
 * A garbage collection however compacts all mappings unconditionally.
 *
 *
 * Mappings maintain two refcounts: the main refcount for all references,
 * and in the hash structure a protector refcount for references as
 * PROTECTED_MAPPING. The latter references are used for 'dirty' mappings
 * (ie. mappings with a hash part) which are passed fully or in part as a
 * reference to a function. As long as the protector refcount is not 0, all
 * entry deletions are not executed immediately. Instead, the 'deleted'
 * entries are kept in a separate list until all protective references
 * are removed. PROTECTED_MAPPINGs don't need to protect the condensed
 * part of a mapping as that changes only during compact_mapping()s
 * in the backend.
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
 *
 *       mapping_t      * next;
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
 *
 *   The .next pointer is not used by the mapping module itself,
 *   but is provided as courtesy for the cleanup code and the GC, to
 *   avoid additional memory allocations during a low memory situation.
 *   The cleanup code uses it to keep its list of dirty mappings; the
 *   GC uses it to keep its list of stale mappings (ie. mappings with
 *   keys referencing destructed objects).
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
 *       mp_int       last_used;
 *       map_chain_t *deleted;
 *       map_chain_t *chains[ 1 +.mask ];
 *   }
 *
 *   This structure keeps track of the changes to a mapping. Every mapping
 *   with a hash part is considered 'dirty'.
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
 *   .last_used holds the time (seconds since the epoch) of the last addition
 *   or removal of an entry. It is used by the compaction algorithm to
 *   determine whether the mapping should be compacted or not.
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
#ifdef USE_STRUCTS
#include "structs.h"
#endif /* USE_STRUCTS */
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-svalue_cmp.h"

#define TIME_TO_COMPACT (600) /* 10 Minutes */
   /* TODO: Make this configurable.
    * TODO:: When doing so, also implement the shrinking of the hashtable
    */

/*-------------------------------------------------------------------------*/
/* Types */

/* The local typedefs */
typedef struct map_chain_s    map_chain_t;
typedef struct walk_mapping_s walk_mapping_t;

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


/* --- struct walk_mapping_s: contains all walk_mapping pointers */

struct walk_mapping_s {
    error_handler_t head;      /* The error handler: f_walk_mapping_cleanup */
    p_int           entries;   /* Number of mapping entries. */
    mapping_t     * map;       /* The mapping. */
    Bool            has_hash;  /* The mapping has a hash part (refcount +1) */
    callback_t    * callback;  /* The callback structure */
    svalue_t        pointers[1 /* + .entries-1*/];
};

/*-------------------------------------------------------------------------*/

mp_int num_mappings = 0;
  /* Number of allocated mappings.
   */

mp_int num_hash_mappings = 0;
  /* Number of allocated mappings with only a hash part.
   */

mp_int num_dirty_mappings = 0;
  /* Number of allocated mappings with a hash and a condensed part.
   */

mapping_t *stale_mappings;
  /* During a garbage collection, this is a list of mappings with
   * keys referencing destructed objects/lambdas, linked through
   * the .next pointers. Since th GC performs a global cleanup first,
   * this list is normally empty, but having it increases the robustness
   * of the GC.
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

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
/* TODO: hash_size of mp_int seems unnecessarily large to me, because 
 * TODO::mappings can only have p_int values? */
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
    if (hash_size > (mp_int)((SIZE_MAX - sizeof *hm - 0x100000) / sizeof *mcp)
     || !(hm = xalloc(sizeof *hm + sizeof *mcp * hash_size) ) )
    {
        return NULL;
    }

    hm->mask = hash_size;
    hm->used = hm->cond_deleted = hm->ref = 0;
    hm->last_used = current_time;

    /* These members don't really need a default initialisation
     * but it's here to catch bogies.
     */
    hm->deleted = NULL;

    /* Initialise the hashbuckets (there is at least one) */
    mcp = hm->chains;
    do *mcp++ = NULL; while (--hash_size >= 0);

    LOG_ALLOC("get_new_hash", SIZEOF_MH(hm), sizeof *hm + sizeof *mcp * hm->mask);
    m->user->mapping_total += SIZEOF_MH(hm);

    return hm;
} /* get_new_hash() */

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
/* TODO: hash_size of mp_int seems unnecessarily large to me, because 
 * TODO::mappings can only have p_int values? */
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

        /* !DEBUG: size_t */ cm_size = (size_t)cond_size;
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
    }

    /* Initialise the mapping */

    m->cond = cm;
    m->hash = hm;
    m->next = NULL;
    m->num_values = num_values;
    m->num_entries = 0;
    // there can't be a destructed object in the mapping now, record the
    // current counter.
    m->last_destr_check = destructed_ob_counter;
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
    if (m->cond && m->hash)
        num_dirty_mappings++;
    else if (m->hash)
        num_hash_mappings++;
    check_total_mapping_size();

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
Bool
_free_mapping (mapping_t *m, Bool no_data)

/* Aliases: free_mapping(m)       -> _free_mapping(m, FALSE)
 *          free_empty_mapping(m) -> _free_mapping(m, TRUE)
 *
 * The mapping and all associated memory is deallocated resp. dereferenced.
 * Always return TRUE (for use within the free_mapping() macro).
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
        fatal("Mapping with %"PRIdPINT" refs passed to _free_mapping().\n", 
              m->ref);
#endif

    num_mappings--;
    if (m->cond && m->hash)
        num_dirty_mappings--;
    else if (m->hash)
        num_hash_mappings--;

    m->ref = 0;
      /* In case of free_empty_mapping(), this is neither guaranteed nor a
       * precondition, but in case this mapping needs to be entered into the
       * dirty list the refcount needs to be correct.
       */

    /* Free the condensed data */
    if (m->cond != NULL)
    {
        p_int left = m->cond->size * (m->num_values + 1);
        svalue_t *data = &(m->cond->data[0]);

        for (; !no_data && left > 0; left--, data++)
            free_svalue(data);

        LOG_SUB("free_mapping cond", SIZEOF_MC(m->cond, m->num_values));
        m->user->mapping_total -= SIZEOF_MC(m->cond, m->num_values);
        check_total_mapping_size();
        xfree(m->cond);
        m->cond = NULL;

    }

    /* Free the hashed data */
    if ( NULL != (hm = m->hash) )
    {
        map_chain_t **mcp, *mc, *next;
        p_int i;

#ifdef DEBUG
        if (hm->ref)
            fatal("Ref count in freed hash mapping: %"PRIdPINT"\n", hm->ref);
#endif
        LOG_SUB("free_mapping hash", SIZEOF_MH(hm));
        m->user->mapping_total -= SIZEOF_MH(hm);
        check_total_mapping_size();

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

        xfree(hm);
    }

    /* Free the base structure.
     */

    LOG_SUB("free_mapping base", sizeof(*m));
    m->user->mapping_total -= sizeof(*m);
    check_total_mapping_size();
    xfree(m);

    return MY_TRUE;
} /* _free_mapping() */

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
/*        printf("%s free_protector_mapping() : no hash %s\n"
              , time_stamp(), m->hash ? "reference" : "part");
 */
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
static INLINE mp_int
mhash (svalue_t * svp)

/* Compute and return the hash value for svalue *<svp>.
 * The function requires that x.generic is valid even for types without
 * a secondary type information.
 */

{
    mp_int i;

    switch (svp->type)
    {
    case T_STRING:
        i = mstr_get_hash(svp->u.str);
        break;

    case T_CLOSURE:
        if (CLOSURE_REFERENCES_CODE(svp->x.closure_type))
        {
            i = (p_int)(svp->u.lambda) ^ *SVALUE_FULLTYPE(svp);
        }
        else if (CLOSURE_MALLOCED(svp->x.closure_type))
        {
            i = (p_int)(svp->u.lambda->ob) ^ *SVALUE_FULLTYPE(svp);
        }
        else /* Efun, Simul-Efun, Operator closure */
        {
            i = *SVALUE_FULLTYPE(svp);
        }
        break;

    default:
        i = svp->u.number ^ *SVALUE_FULLTYPE(svp);
        break;
    }

    i = i ^ i >> 16;
    i = i ^ i >> 8;

    return i;
} /* mhash() */

/*-------------------------------------------------------------------------*/
static svalue_t *
find_map_entry ( mapping_t *m, svalue_t *map_index
               , p_int * pKeys, map_chain_t ** ppChain
               , Bool bMakeTabled
               )

/* Index mapping <m> with key value <map_index> and if found, return a pointer
 * to the entry block for this key (ie. the result pointer will point to
 * the stored key value).
 * If the key was found in the condensed data, *<pKeys> will be set
 * to key index; otherwise *<ppChain> will point to the hash map chain entry.
 * The 'not found' values for the two variables are -1 and NULL resp.
 *
 * If <bMakeTabled> is TRUE and <map_index> is a string, it is made tabled.
 *
 * If the key is not found, NULL is returned.
 *
 * Sideeffect: <map_index>.x.generic information is generated for types
 *   which usually have none (required for hashing).
 */

{
    *pKeys = -1;
    *ppChain = NULL;

    /* If the key is a string, make it tabled */
    if (map_index->type == T_STRING && !mstr_tabled(map_index->u.str)
     && bMakeTabled)
    {
        map_index->u.str = make_tabled(map_index->u.str);
    }

    /* Check if it's a destructed object.
     */
    if (destructed_object_ref(map_index))
        assign_svalue(map_index, &const0);

    /* Generate secondary information for types which usually
     * have none (required for hashing).
     */
    if (map_index->type != T_CLOSURE
     && map_index->type != T_FLOAT
     && map_index->type != T_SYMBOL
     && map_index->type != T_QUOTED_ARRAY
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

            cmp = svalue_cmp(map_index, key);

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
            if (!svalue_eq(&(mc->data[0]), map_index))
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
 * array of values stored for this key. If the mapping has no values for a
 * key, a pointer to const1 is returned.
 *
 * If the mapping does not contains the given index, and <need_lvalue> is
 * false, &const0 is returned. If <need_lvalue> is true, a new key/value
 * entry is created and returned (map_index is assigned for this). If the
 * mapping doesn't have values for a key, a pointer to a local static
 * instance of svalue-0 is returned.
 *
 * If check_size is true and the extension of the mapping would increase
 * its size over max_mapping_size, a runtime error is raised.
 *
 * Return NULL when out of memory.
 *
 * Sideeffect: if <map_index> is an unshared string, it is made shared.
 *   Also, <map_index>.x.generic information is generated for types
 *   which usually have none (required for hashing).
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

static svalue_t local_const0;
  /* Local svalue-0 instance to be returned if a lvalue
   * for a 0-width was requested.
   */

    entry = find_map_entry(m, map_index, (p_int *)&idx, &mc, need_lvalue);

    /* If we found the entry, return the values */
    if (entry != NULL)
    {
        if (!m->num_values)
            return &const1;

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
    if (check_size && (max_mapping_size || max_mapping_keys))
    {
        mp_int msize;

        msize = (mp_int)MAP_TOTAL_SIZE(m) + m->num_values + 1;
        if (   (max_mapping_size && msize > (mp_int)max_mapping_size)
            || (max_mapping_keys && MAP_SIZE(m)+1 > max_mapping_keys)
           )
        {
            check_map_for_destr_keys(m);
            msize = (mp_int)MAP_TOTAL_SIZE(m) + m->num_values + 1;
        }
        if (max_mapping_size && msize > (mp_int)max_mapping_size)
        {
            errorf("Illegal mapping size: %"PRIdMPINT" elements (%"
                PRIdPINT" x %"PRIdPINT")\n"
                 , msize, MAP_SIZE(m)+1, m->num_values);
            return NULL;
        }
        if (max_mapping_keys && MAP_SIZE(m) > (mp_int)max_mapping_keys)
        {
            errorf("Illegal mapping size: %"PRIdMPINT" entries\n", msize+1);
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

        /* Now insert the map_chain structure into its chain */
        hm->chains[0] = mc;
        mc->next = NULL;

        if (m->cond)
            num_dirty_mappings++;
        else
            num_hash_mappings++;
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
            check_total_mapping_size();

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

    hm->last_used = current_time;
    hm->used++;
    m->num_entries++;

    if (m->num_values)
        return &(mc->data[1]);

    /* Return a reference to the local static svalue-0 instance, so that
     * buggy code doesn't accidentally changes the global const0.
     */
    put_number(&local_const0, 0);
    return &local_const0;
} /* _get_map_lvalue() */

/*-------------------------------------------------------------------------*/
Bool
mapping_references_objects (mapping_t *m)

/* Check if the mapping <m> references objects (directly or through
 * closures) as keys.
 * Return TRUE if it does, FALSE if it doesn't.
 *
 * The swapper uses this function to determine whether or not to
 * swap a mapping.
 */

{
    mapping_cond_t *cm;
    mapping_hash_t *hm;

    /* Scan the condensed part for object references used as keys.
     */

    if (NULL != (cm = m->cond))
    {
        size_t ix;
        svalue_t * entry;

        for (ix = 0, entry = &(cm->data[0]); ix < cm->size; ++ix, ++entry)
        {
            if (T_OBJECT == entry->type || T_CLOSURE == entry->type)
                return MY_TRUE;
        } /* for (all keys) */

    } /* if (m->cond) */

    /* If it exists, scan the hash part for object references.
     */

    if ( NULL != (hm = m->hash) )
    {
        map_chain_t **mcp, *mc;
        p_int i;

        /* Walk all chains */

        for (mcp = hm->chains, i = hm->mask + 1; --i >= 0;)
        {
            /* Walk this chain */

            for (mc = *mcp++; NULL != mc; mc = mc->next)
            {
                svalue_t * entry = &(mc->data[0]);

                if (T_OBJECT == entry->type || T_CLOSURE == entry->type)
                    return MY_TRUE;
            } /* walk this chain */
        } /* walk all chains */
    } /* if (hash part exists) */

    return MY_FALSE;
} /* mapping_references_objects() */

/*-------------------------------------------------------------------------*/
void
check_map_for_destr_keys (mapping_t *m)

/* Check the mapping <m> for keys referencing destructed objects
 * If one is found, both key and associated values are deleted from the mapping.
 */

{
    p_int             num_values;
    mapping_cond_t *cm;
    mapping_hash_t *hm;

    // If no object was destructed since the last check, there can't be a
    // key referencing a destructed object in the mapping.
    if (m->last_destr_check == destructed_ob_counter)
        return;
    
    num_values = m->num_values;
    
    /* Scan the condensed part for destructed object references used as keys.
     */
    if (NULL != (cm = m->cond))
    {
        size_t ix;
        svalue_t * entry;
        
        /* Scan the keys */
        for (ix = 0, entry = &(cm->data[0]); ix < cm->size; ++ix, ++entry)
        {
            if (T_INVALID == entry->type)
                continue;
            
            if (destructed_object_ref(entry))
            {
                p_int i;
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
                    num_dirty_mappings++;
                }
                
                hm->cond_deleted++;
                
                continue;
            }
        } /* for (all keys) */
        
    } /* if (m->cond) */
    
    /* If it exists, scan the hash part for destructed objects.
     */
    
    if ( NULL != (hm = m->hash) )
    {
        map_chain_t **mcp, **mcp2, *mc;
        p_int i;
        
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

                mcp2 = &mc->next;
                
            } /* walk this chain */
        } /* walk all chains */
    } /* if (hash part exists) */

    // finally, record the current counter of destructed objects.
    m->last_destr_check = destructed_ob_counter;

} /* check_map_for_destr_keys() */

/*-------------------------------------------------------------------------*/
void
check_map_for_destr_values (mapping_t *m)

/* Check the mapping <m> for values referencing destructed objects.
 * Any such values replaced by svalue-0.
 */

{
    p_int             num_values;
    mapping_cond_t *cm;
    mapping_hash_t *hm;
        
    num_values = m->num_values;
    
    /* Scan the condensed part for destructed object references in values
     */
    if (NULL != (cm = m->cond))
    {
        size_t ix;
        svalue_t * entry;
                
        /* Scan the values */
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

                svalue_t * entry = &(mc->data[0]); // this is the key
                                
                /* Scan the values of this entry */
                for (++entry, j = num_values; j > 0; --j, ++entry)
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
        
} /* check_map_for_destr_values() */

/*-------------------------------------------------------------------------*/
void
check_map_for_destr (mapping_t *m)

/* Check the mapping <m> for references to destructed objects.
 * Where they appear as keys, both key and associated values are
 * deleted from the mapping. Where they appear as values, they are
 * replaced by svalue-0.
 */

{
    check_map_for_destr_keys(m);
    check_map_for_destr_values(m);
} /* check_map_for_destr() */

/*-------------------------------------------------------------------------*/
static void
remove_mapping (mapping_t *m, svalue_t *map_index)

/* Remove from mapping <m> that entry which is index by key value
 * <map_index>. Nothing happens if it doesn't exist.
 *
 * Sideeffect: if <map_index> is an unshared string, it is made shared.
 *   Also, <map_index>.x.generic information is generated for types
 *   which usually have none (required for hashing).
 */

{
    p_int            key_ix;
    svalue_t       * entry;
    map_chain_t    * mc;
    mapping_hash_t * hm;
    p_int            num_values;

    num_values = m->num_values;

    entry = find_map_entry(m, map_index, &key_ix, &mc, MY_FALSE);

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

                if (m->cond)
                    num_dirty_mappings++;
                else
                    num_hash_mappings++;
            }

            hm->last_used = current_time;
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

            /* Unlink the found entry */
            if (prev)
                prev->next = mc->next;
            else
                hm->chains[idx] = mc->next;

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

            hm->last_used = current_time;
            hm->used--;
            /* TODO: Reduce the size of the hashtable if the average
             * TODO:: number of entries per chain is <= 1 (or better <= 0.5
             * TODO:: to provide some breathing space for new entries).
             */
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
    p_int  num_entries;

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
            errorf("Mapping width too big (%"PRIdMPINT")\n", new_width);
            /* NOTREACHED */
            return NULL;
        }

    }
    
    num_entries = m->num_entries;

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
            outofmem(sizeof *m2 + (mp_int)sizeof(svalue_t) * m->num_entries * new_width
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
        hm2->last_used = current_time;
        hm2->cond_deleted = 0;
        hm2->deleted = NULL;
        hm2->ref = 0;

        /* Now copy the hash chains */

        mcp = hm->chains;
        mcp2 = hm2->chains;
        do {
            map_chain_t *last = NULL, *mc, *mc2;

            for (mc = *mcp++; mc; mc = mc->next)
                if(destructed_object_ref(&(mc->data[0])))
                    num_entries--;
                else
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
        check_total_mapping_size();
        if (m->cond)
            num_dirty_mappings++;
        else
            num_hash_mappings++;
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
            if (src_key->type == T_INVALID)
                ; /* Do nothing */
            else if (destructed_object_ref(src_key))
            {
                /* We have to fill the space.
                 * (Alternatively we could decrease m->cond->size.)
                 */
                p_int i;

                num_entries--;

                dest_key->type = T_INVALID;
                dest_key++;

                for (i = new_width; i > 0; i--, dest_data++)
                    put_number(dest_data, 0);
            }
            else
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

    m2->num_entries = num_entries;

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

        errorf("Mappings to be added are of different width: %"PRIdMPINT
               " vs. %"PRIdPINT"\n",
               num_values, m2->num_values);
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
            int cmp;
            p_int i;

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

            cmp = svalue_cmp(src1_key, src2_key);

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
            p_int i;

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
            p_int i;

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

        for ( ; (p_int)num_entries < cm3size; num_entries++)
        {
            p_int i;

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
                p_int i;

                src = &(mc->data[0]);
                dest = get_map_lvalue_unchecked(m3, src);
                if (!dest)
                {
                    free_mapping(m3);
                    return NULL;
                }
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
                p_int i;

                src = &(mc->data[0]);
                dest = get_map_lvalue_unchecked(m3, src);
                if (!dest)
                {
                    free_mapping(m3);
                    return NULL;
                }
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
Bool
compact_mapping (mapping_t *m, Bool force)

/* Compact the mapping <m>.
 *
 * If <force> is TRUE, always compact the mapping.
 * If <force> is FALSE, the mappings is compacted if
 *   - have a .last_used time of 2*TIME_TO_COMPACT or more seconds earlier,
 *   - or have to have at least half of their condensed entries deleted
 *     and have a .last_used time of TIME_TO_COMPACT or more seconds earlier.
 *
 * Return TRUE if the mapping has been freed altogether in the function
 * (ie. <m> is now invalid), or FALSE if it still exists.
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
    int old_malloc_privilege = malloc_privilege;
      /* Since it will be set temporarily to MALLOC_SYSTEM */

    Bool checked_map_for_destr = MY_FALSE;
      /* Flag if check_map_for_destr() has been called. */

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

    malloc_privilege = MALLOC_SYSTEM;
      /* compact_mappings() may be called in very low memory situations,
       * so it has to be allowed to use the system reserve.
       * Neat sideeffect: all allocations are guaranteed to work (or
       * the driver terminates).
       */

    if (last_indexing_protector.type == T_PROTECTOR_MAPPING)
    {
        /* There is a slight chance that free_protector_mapping causes
         * remove_empty_mappings().
         */
        free_protector_mapping(last_indexing_protector.u.map);
        last_indexing_protector.type = T_NUMBER;
    }

#ifdef DEBUG
    if (!m->user)
        fatal("No wizlist pointer for mapping\n");
#endif

    m->ref++; /* prevent freeing while using in case of recursive
               * mappings referenced by a deleted value
               */

    hm = m->hash;
    cm = m->cond;

    if (hm && hm->ref) {
        fatal("compact_mapping(): remaining protector ref count %"
              PRIdPINT"!\n", hm->ref);
    }

    /* Test if the mapping is dirty at all.
     */
    if (!hm)
    {
        check_map_for_destr(m); /* may create a hash part */
        checked_map_for_destr = MY_TRUE;
        hm = m->hash;
        cm = m->cond;
    }

    if (!hm)
    {
        LOG_SUB("compact_mapping(): no hash part", 0);
        malloc_privilege = old_malloc_privilege;
        check_total_mapping_size();

        return free_mapping(m);
    }

    /* Test the compaction criterium.
     * By testing it before check_map_for_destr(), the size related
     * criterias might trigger later than desired, but the time criterium
     * makes sure that we won't miss one.
     */
    if (!force
     && !(   current_time - hm->last_used >= TIME_TO_COMPACT
          && (   hm->cond_deleted * 2 >= m->num_entries - hm->used 
              || hm->used >= m->num_entries - hm->used - hm->cond_deleted
              || current_time - hm->last_used >= 2*TIME_TO_COMPACT
             )
         )
       )
    {
        /* This mapping doesn't qualify for compaction.
         */
        m->ref--; /* undo the ref increment from above */
        malloc_privilege = old_malloc_privilege;
        return MY_FALSE;
    }

    /* Detect all destructed entries - the compaction algorithm
     * relies on it.
     */
    if (!checked_map_for_destr)
    {
        check_map_for_destr(m);
        checked_map_for_destr = MY_TRUE;
        hm = m->hash;
        cm = m->cond;
    }

    /* Test if the mapping needs compaction at all.
     * If not, just delete the hash part (if any).
     */
    if (!hm->used && !hm->cond_deleted)
    {
        LOG_SUB("compact_mapping(): no need to", SIZEOF_MH(hm));
        malloc_privilege = old_malloc_privilege;
        m->user->mapping_total -= SIZEOF_MH(hm);
        m->hash = NULL;

        if (m->cond)
            num_dirty_mappings--;
        else
            num_hash_mappings--;
        check_total_mapping_size();

        xfree(hm);

        /* the ref count has been incremented above; on the other
         * hand, the last real reference might have gone with the
         * deleted keys. If that is the case, free_mapping() will
         * deallocate it (since we NULLed out the .hash).
         */
        return free_mapping(m);
    }

    /* This mapping can be compacted, and there is something to compact. */

    /* Get the temporary result mapping (we need the condensed block
     * anyway, and this way it's simple to keep the statistics
     * straight).
     */

    if (m->cond && m->hash)
        num_dirty_mappings--;
    else if (m->hash)
        num_hash_mappings--;

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
                    int d = svalue_cmp(&(mcp->data[0]), &(last_hash->data[0]));

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
                    int d = svalue_cmp(&(hook1->data[0]), &(hook2->data[0]));

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

                d = svalue_cmp(src_key, &(hook1->data[0]));

                if (d > 0)
                {
                    /* Take entry from hook1 */

                    map_chain_t *temp;
                    svalue_t    *src;
                    p_int i;

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

                    p_int i;

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
                        p_int i;

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
                    p_int i;

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

    LOG_SUB("compact_mapping() - remove old hash", SIZEOF_MH(hm));
    malloc_privilege = old_malloc_privilege;
    m->user->mapping_total -= SIZEOF_MH(hm);
    check_total_mapping_size();
      /* The memorysize for the map_chain_t structure has already been
       * subtracted.
       */

    xfree(hm);

    free_empty_mapping(m2);
      /* Get rid of the temporary mapping and the old cond block.
       */

    return free_mapping(m);
      /* Undo the initial m->ref++; if there was a recursive
       * reference which is now gone, the mapping will be deallocated
       * now.
       */

} /* compact_mapping() */

/*-------------------------------------------------------------------------*/
#ifdef CHECK_MAPPING_TOTAL
void
m_check_total_mapping_size (const char * file, int line)

/* Check the sanity of the total amount of memory recorded for all
 * mappings in the system. If the value becomes bogus, log a message.
 */

{
    static mp_int last_size = 0;
    static Bool last_size_ok = MY_TRUE;
    wiz_list_t *wl;
    mp_int total;
    Bool this_size_ok = MY_TRUE;
#if defined(MALLOC_smalloc) || defined(MALLOC_slaballoc)
    mp_int mem_in_use = xalloc_used();
#endif
    
    total = default_wizlist_entry.mapping_total;
    for (wl = all_wiz; wl; wl = wl->next)
    {
        total += wl->mapping_total;
    }

    if (total < 0
#if defined(MALLOC_smalloc) || defined(MALLOC_slaballoc)
     || total > mem_in_use
#endif
       )
        this_size_ok = MY_FALSE;

    if (last_size_ok && !this_size_ok)
    {
        dprintf3(gcollect_outfd, "DEBUG: (%s : %d) Invalid total mapping size %d"
                  , (p_int)file, (p_int)line, (p_int)total);
#if defined(MALLOC_smalloc) || defined(MALLOC_slaballoc)
        dprintf1(gcollect_outfd, " (memory usage: %d)", (p_int)mem_in_use);
#endif
        dprintf1(gcollect_outfd, ", was %d\n", (p_int)last_size);
    }

    last_size_ok = this_size_ok;
    last_size = total;
}
#endif /* CHECK_MAPPING_TOTAL */

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
    p_int        num_values;  /* Number of values per key */
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
    p_int i;
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
    p_int num_values;
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
    check_total_mapping_size();
    user = owner->user;
    m->user = user;
    LOG_ADD("set_mapping_user", total);
    m->user->mapping_total += total;
    check_total_mapping_size();


    /* Walk the mapping to set all owners */

    locals.owner = owner;
    locals.num_values = num_values;
    first_hairy = alloca(((m->cond) ? m->cond->size : 1) * sizeof(svalue_t *));
    if (!first_hairy)
    {
        errorf("Stack overflow.\n");
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
        if (!dest)
        {
            outofmemory("key with new owner");
            /* NOTREACHED */
            return;
        }
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
    check_total_mapping_size();
} /* clear_mapping_size(void) */

/*-------------------------------------------------------------------------*/
void
count_mapping_size (mapping_t *m)

/* Add the mapping <m> to the statistics.
 * This method is called from the garbage collector only, at which point
 * the .hash member is either NULL or used as link pointer for a list
 * of stale mappings.
 */

{
    mp_int total;

    num_mappings++;

    total = sizeof(*m);
#if 0 && defined(CHECK_MAPPING_TOTAL)
    dprintf3(gcollect_outfd, "DEBUG: map '%s' %d (num values %d)"
                           , (p_int)(m->user->name ? get_txt(m->user->name) : "<0>")
                           , (p_int)total, (p_int)m->num_values);
#endif

    if (m->cond != NULL)
    {
        mp_int subtotal;

        subtotal = SIZEOF_MC(m->cond, m->num_values);
        total += subtotal;
#if 0 && defined(CHECK_MAPPING_TOTAL)
        dprintf2(gcollect_outfd, " + %d (size %d)"
                               , (p_int)subtotal
                               , (p_int)m->cond->size
                               );
#endif
    }

    /* m->hash does not point to a hash structure at this time */

#if 0 && defined(CHECK_MAPPING_TOTAL)
    dprintf1(gcollect_outfd, " = %d\n", (p_int)total);
#endif

    m->user->mapping_total += total;
    check_total_mapping_size();
} /* count_mapping_size(void) */

/*-------------------------------------------------------------------------*/
static void
handle_destructed_key (svalue_t *key)

/* GC support: <key> has been found to be a key referencing a destructed
 * object. This function modifies it so that the GC wont choke.
 */

{
    if (key->type == T_CLOSURE &&
        key->x.closure_type == CLOSURE_BOUND_LAMBDA)
    {
        /* Avoid changing keys: collapse the bound/unbound combination
         * into a single lambda closure bound to the destructed
         * object. This way the GC will treat it correctly.
         */
        lambda_t *l = key->u.lambda;

        key->x.closure_type = CLOSURE_LAMBDA;
        key->u.lambda = l->function.lambda;
        if (!l->ref)
        {
            /* This would have been the first reference to the
             * lambda closure: add it to the stale list and mark
             * it as 'stale'.
             */
            l->function.lambda->ob = l->ob;
            l->ref = -1;
            l->ob = (object_t *)stale_misc_closures;
            stale_misc_closures = l;
        }
        else
        {
            /* Closure is already been marked as 'stale': no need
             * to do anything about it, but but since l->ob is no
             * longer a valid object, we need to use a known
             * destructed object as stand-in for remaining lambda.
             * TODO: Having a type CLOSURE_DESTRUCTED_LAMBDA
             * TODO:: might be safer? After all,
             * TODO:: gc_obj_list_destructed might be NULL.
             */
#ifdef DEBUG
            if (gc_obj_list_destructed)
                fatal("gc_obj_list_destructed is NULL\n");
#endif
            l->function.lambda->ob = gc_obj_list_destructed;
        }
    }
    count_ref_in_vector(key, 1);
    if (key->type == T_CLOSURE)
    {
        /* *key has been transformed by count_ref_in_vector()
         * into an efun closure bound to the master.
         */
        key->u.ob->ref--;
    }

    /* Don't bother freeing the svalues - this is the GC after all,
     * and freeing them might even confuse the memory allocator.
     */
    key->type = T_INVALID;
} /* handle_destructed_key() */

/*-------------------------------------------------------------------------*/
void
count_ref_in_mapping (mapping_t *m)

/* GC support: Count all references by the mapping <m>.
 *
 * If the mapping contains keys referencing destructed objects/lambdas,
 * it is added to the list of stale mappings.
 */

{
    mp_int size;
    mp_int num_values;
    Bool any_destructed = MY_FALSE;

    num_values = m->num_values;

    /* Mark the blocks as referenced */
    if (m->cond)
        note_malloced_block_ref(m->cond);
    if (m->hash)
        note_malloced_block_ref(m->hash);

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
            handle_destructed_key(key);
            m->num_entries--;

            any_destructed = MY_TRUE;
        }
        else
        {
            count_ref_in_vector(key, 1);
            count_ref_in_vector(data, num_values);
        }
    }

    /* Count references by hashed keys and their data.
     * Take special care of keys referencing destructed objects/lambdas.
     */
    size = m->hash ? m->hash->mask+1 : 0;
    while ( --size >= 0)
    {
        map_chain_t * mc = m->hash->chains[size];

        for ( ; mc != NULL; mc = mc->next)
        {
            note_malloced_block_ref(mc);
            if (destructed_object_ref(mc->data))
            {
                /* This key is a destructed object, resp. is bound to a
                 * destructed object. The entry has to be deleted.
                 */
                handle_destructed_key(mc->data);

                any_destructed = MY_TRUE;
            }
            else
            {
                count_ref_in_vector(mc->data, 1);
                count_ref_in_vector(mc->data+1, num_values);
            }
        }
    }

    /* If any stale key was found, link the mapping into the 
     * stale mapping list.
     */
    if (any_destructed)
    {
        m->next = stale_mappings;
        stale_mappings = m;
        /* We are going to use free_svalue() later to get rid of the
         * data asscoiated with the keys. This data might reference
         * mappings with destructed keys... Thus, we must prevent
         * free_mapping() to look at the hash field.
         */
        m->ref++;
        /* Ref for the stale-mapping link. */
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
        mapping_hash_t *hm;
        size_t size;
        mp_int num_cond_entries;
        mp_int num_values;
        mp_int i;

        /* Unlink from the stale_mapping list */
        next = m->next;
        m->next = NULL;

        num_values = m->num_values;
        cm = m->cond;
        hm = m->hash;

        /* Try to reallocate a new condensed block */

        num_cond_entries = m->num_entries - (hm ? hm->used : 0);
        if (num_cond_entries)
        {
            mapping_cond_t *cm2;
            size_t ix;
            svalue_t *src_key, *src_data;
            svalue_t *dest_key, *dest_data;

            size = sizeof(*cm2) + sizeof(svalue_t) * (num_cond_entries * (num_values+1) - 1);
            cm2 = xalloc(size);
            if (!cm2)
            {
                fprintf(stderr, "%s Unable to compact stale mapping: Out of memory "
                                "for new condensed block (%zu bytes).\n"
                              , time_stamp(), size);
                debug_message("%s Unable to compact stale mapping: Out of memory "
                              "for new condensed block (%zu bytes).\n"
                             , time_stamp(), size);

                /* No use in even trying to compact the much bigger data
                 * block either.
                 */
                continue;
            }

            cm2->size = num_cond_entries;

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
            /* No condensed block needed. */
            m->cond = NULL;
        }

        /* Delete the old condensed block, if any */
        if (cm)
        {
            LOG_SUB("clean_state - old keyblock", SIZEOF_MC(cm, num_values));
            m->user->mapping_total -= SIZEOF_MC(cm, num_values);
            xfree(cm);
        }

        /* Removed all invalid keys from the hash part, if any */
        if (hm && hm->used)
        {
            size_t ix;

            for (ix = 0; ix <= (size_t)hm->mask; ix++)
            {
                map_chain_t * mc, * mcp;

                for (mcp = NULL, mc = hm->chains[ix]; mc != NULL ; )
                {
                    if (mc->data[0].type == T_INVALID)
                    {
                        /* This key has been marked for deletion,
                         * now remove it altogether.
                         */
                        map_chain_t * this = mc;

                        if (mcp == NULL)
                        {
                            hm->chains[ix] = this->next;
                        }
                        else
                        {
                            mcp->next = this->next;
                        }
                        mc = this->next;

                        m->num_entries--;
                        hm->used--;
                        m->user->mapping_total -= SIZEOF_MCH(this, num_values);
                        xfree(this);
                    }
                    else
                    {
                        /* Valid key - just step forward */
                        mcp = mc;
                        mc = mc->next;
                    }
                } /* for(mc) */
            } /* for(ix) */
        } /* hash part */

        check_total_mapping_size();
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
    p_int size = sp[-1].u.number;
    p_int width = sp[0].u.number;

    if (size < 0)
        errorf("Illegal mapping size: %"PRIdPINT"\n", size);
    if (width < 0)
        errorf("Illegal mapping width: %"PRIdPINT"\n", width);

    if (max_mapping_size
     && size * (1 + width) > (p_int)max_mapping_size)
        errorf("Illegal mapping size: %"PRIdMPINT
               " elements (%"PRIdPINT" x %"PRIdPINT").\n",
               (mp_int)size * (1+width),
               size, width);

    if (max_mapping_keys
     && size > (p_int)max_mapping_keys)
        errorf("Illegal mapping size: %"PRIdPINT" entries.\n", size);

    sp--;

    if (!(sp->u.map = allocate_mapping(size, width)))
    {
        sp++;
        /* sp points to a number-typed svalue, so freeing won't
         * be a problem.
         */
        errorf("Out of memory for mapping[%"PRIdPINT",%"PRIdPINT"].\n", 
               size, width);
        /* NOTREACHED */
    }
    sp->type = T_MAPPING;

    return sp;
} /* f_m_allocate() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_m_add (svalue_t *sp, int num_arg)

/* EFUN m_allocate()
 *
 *   mapping m_add(mapping map, mixed key, [mixed data...])
 *
 * Add (or replace) an entry with index <key> in mapping <map>.
 * The modified mapping is also returned as result.
 *
 * The values for the entry are taken from the <data> arguments.
 * Unassigned entry values default to 0, extraneous <data> arguments
 * are ignore.
 */

{
    mapping_t *m;
    svalue_t *argp;
    svalue_t *entry;
    p_int num_values;

    argp = sp - num_arg + 1;
    m = argp->u.map;

    /* Get (or create) the mapping entry */
    entry = get_map_lvalue(m, argp+1);

    /* Transfer the given values from the stack into the mapping
     * entry.
     */
    num_values = m->num_values;
    if (num_values > num_arg - 2)
        num_values = num_arg - 2;
    for ( argp += 2
        ; num_values > 0 && argp <= sp
        ; num_values--, argp++, entry++
        )
    {
        transfer_svalue(entry, argp);
        /* And since we take out values from under sp, play it
         * safe:
         */
        put_number(argp, 0);
    }

    /* We leave the reference to the mapping on the stack as result,
     * but pop everything else.
     */
    sp = pop_n_elems(num_arg-1, sp);

    return sp;
} /* v_m_add() */

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

    check_map_for_destr_keys(m);
    size = MAP_SIZE(m);
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
    p_int size;
    p_int num;

    /* Get and check the arguments */
    num = sp->u.number;
    sp--;
    inter_sp = sp;

    m = sp->u.map;
    if (num < 0 || num >= m->num_values)
        errorf("Illegal index %"PRIdPINT" to m_values(): should be in 0..%"
               PRIdPINT".\n", num, m->num_values-1);

    /* Get the size of the mapping */
    check_map_for_destr(m);
    size = MAP_SIZE(m);

    if (size > 0 && m->num_values < 1)
        errorf("m_values() applied on mapping with no values.\n");

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
    p_int i;

    data2 = get_map_lvalue_unchecked((mapping_t *)extra, key);
    if (!data2)
    {
        outofmemory("entry added to mapping");
        /* NOTREACHED */
        return;
    }
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
            errorf("Mappings to be added are of different width: %"PRIdPINT
                   " vs. %"PRIdPINT"\n",
                   m1->num_values, m2->num_values);
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
struct map_intersect_s
{
    mapping_t * m;   /* Mapping to be intersected */
    mapping_t * rc;  /* Result mapping */
};


static void
map_intersect_filter (svalue_t *key, svalue_t *data UNUSED, void *extra)

/* Auxiliary function to map_intersect():
 * If <key> is in <extra>->m, add the data to <extra>->rc.
 */

{
#ifdef __MWERKS__
#    pragma unused(data)
#endif
    mapping_t * m  = ((struct map_intersect_s *)extra)->m;
    mapping_t * rc = ((struct map_intersect_s *)extra)->rc;

    svalue_t * src;

    src = get_map_value(m, key);
    if (src != &const0)
    {
        p_int num_values = m->num_values;
        svalue_t * dest;
        p_int j;

        dest = get_map_lvalue(rc, key);
        if (!dest)
        {
            outofmemory("result mapping entry");
            /* NOTREACHED */
        }
        for (j = 0; j < num_values; j++)
        {
            assign_svalue(dest+j, src+j);
        }
    } /* if found element */
} /* map_intersect_filter() */


mapping_t *
map_intersect (mapping_t *m, svalue_t * val)

/* Intersect mapping <m> with vector/mapping <val>.
 *
 * The result is a new mapping with all those elements of <m> which index
 * can be found in vector <val>->u.vector resp. as index in mapping
 * <val>->u.map. Both <m> and <val> are freed on return.
 *
 * Called by interpret to implement F_AND.
 */

{
    mapping_t *rc = NULL;

    if (val->type == T_POINTER)
    {
        vector_t * vec = val->u.vec;
        p_int      vecsize = VEC_SIZE(vec);
        p_int      num_values = m->num_values;
        p_int      i;

        rc = allocate_mapping(vecsize, num_values);
        if (!rc)
        {
            outofmemory("result mapping");
            /* NOTREACHED */
        }

        for (i = 0; i < vecsize; i++)
        {
            svalue_t * src;

            src = get_map_value(m, &vec->item[i]);
            if (src != &const0)
            {
                svalue_t * dest;
                p_int j;

                dest = get_map_lvalue(rc, &vec->item[i]);
                if (!dest)
                {
                    outofmemory("result mapping entry");
                    /* NOTREACHED */
                }
                for (j = 0; j < num_values; j++)
                {
                    assign_svalue(dest+j, src+j);
                }
            } /* if found element */
        } /* for (i) */
    }
    else if (val->type == T_MAPPING)
    {
        mapping_t              * map = val->u.map;
        p_int                    num_values = m->num_values;
        struct map_intersect_s   data;

        rc = allocate_mapping(MAP_SIZE(map), num_values);
        if (!rc)
        {
            outofmemory("result mapping");
            /* NOTREACHED */
        }

        data.m = m;
        data.rc = rc;
        walk_mapping(map, map_intersect_filter, &data);
    }
    else
        fatal("(map_intersect) Illegal type to arg2: %d, "
              "expected array/mapping."
             , val->type);

    free_mapping(m);
    free_svalue(val);
    return rc;
} /* map_intersect() */
 
/*-------------------------------------------------------------------------*/
vector_t *
map_intersect_array (vector_t *vec, mapping_t *map)

/* OPERATOR & (array/map intersection)
 *
 * Perform an intersection of the vectors <vec> with the indices of
 * mapping <map>.
 *
 * The result is a new vector with all elements which are present in both
 * input vectors.
 *
 * Both <vec> and <map> are freed.
 */

{
    Bool     *flags;       /* The result from match_arrays() */
    p_int    result_size; /* Size of the result array */
    vector_t *result;      /* Result array */
    svalue_t *dest;        /* Pointer for storing the result elements */
    p_int i;

    p_int vec_size = VEC_SIZE(vec);

    /* Handle empty arrays */

    if (vec_size == 0)
    {
        free_mapping(map);
        free_array(vec);
        return ref_array(&null_vector);
    }

    /* Non-trivial arrays: match them up */

    xallocate(flags, vec_size * sizeof(Bool), "flag vector");
    memset(flags, 0, vec_size * sizeof(Bool));

    /* Walk through the vector and check for each element
     * if it exists in the mapping.
     * If it does, set the corresponding flag and count the
     * result size.
     */
    result_size = 0;
    for (i = 0; i < vec_size; ++i)
    {
        if (get_map_value(map, vec->item+i) != &const0)
        {
            flags[i] = MY_TRUE;
            result_size++;
        }
    }

    if (result_size == vec_size)
    {
        /* No elements to remove */
        xfree(flags);
        free_mapping(map);
        return vec;
    }

    if (max_array_size && result_size > max_array_size)
    {
        xfree(flags);
        free_mapping(map);
        free_array(vec);
        errorf("Illegal array size: %"PRIdPINT".\n", result_size);
    }

    result = allocate_array(result_size);

    /* Copy the elements to keep from vec into result.
     * We count down result_size to be able to stop as early
     * as possible.
     */
    for ( dest = result->item, i = 0
        ; i < vec_size && result_size != 0
        ; i++
        )
    {
        if (flags[i])
        {
            assign_svalue_no_free(dest, vec->item+i);
            dest++;
            result_size--;
        }
    }

    /* Cleanup and return */
    xfree(flags);
    free_array(vec);
    free_mapping(map);

    return result;
} /* map_intersect_array() */

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
f_walk_mapping_cleanup (error_handler_t *arg)

/* Auxiliary to efuns {walk,filter}_walk_mapping(): Cleanup.
 *
 * This function is called during the stackcleanup after a mapping walk.
 * <arg> is the array of svalue allocated by walk_mapping_prologue().
 * See walk_mapping_prologue() for details.
 */
{
    walk_mapping_t *data;
    svalue_t *svp;
    mapping_t *m;
    mp_int i;

    data = (walk_mapping_t*) arg;

    if (data->callback)
        free_callback(data->callback);

    m = data->map;

    /* If the mapping had a hash part prior to the f_walk_mapping(),
     * it was protected by the prologue and we have to lift that
     * protection.
     */
    if (data->has_hash)
    {
        mapping_hash_t *hm;

        hm = m->hash;

        if (!--hm->ref)
        {
            /* Last ref gone: deallocated the pending deleted entries */

            map_chain_t *mc, *next;

            for (mc = hm->deleted; mc; mc = next)
            {
                next = mc->next;
                free_map_chain(m, mc, MY_FALSE);
            }

            hm->deleted = NULL;
        }
    }

    /* Free the key svalues in the block */
    i = data->entries;
    svp = data->pointers;
    if (i) do
    {
        free_svalue(svp);
        svp += 2;
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
    walk_mapping_t *pointers;
    svalue_t *write_pointer, *read_pointer;

    if ( NULL != (hm = m->hash) ) {
        if (m->num_values == 0)
        {
            hm = NULL; /* Flag: no values per key */
        }
        else if (!hm->ref++)
        {
            hm->deleted = NULL;
        }
    }
    xallocate(pointers, sizeof(walk_mapping_t)
                      + (m->num_entries * 2 - 1) * sizeof(svalue_t)
                      , "walk_mapping prologue" );

    pointers->callback = cb;
    pointers->entries = m->num_entries;
    pointers->map = m;
    pointers->has_hash = hm != NULL;

    inter_sp = sp;
    push_error_handler(f_walk_mapping_cleanup, &(pointers->head));
    read_pointer = write_pointer = pointers->pointers;
    walk_mapping(m, f_walk_mapping_filter, &write_pointer);
    return read_pointer;
} /* walk_mapping_prologue() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_walk_mapping (svalue_t *sp, int num_arg)

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
    p_int num_values;        /* Number of values per entry */
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
    i = m->num_entries;
    inter_sp = ++sp; /* walk_mapping_prologue() pushed one value */

    num_values = m->num_values;

    /* For every key:values pair in read_pointer[], set up
     * the stack for a call to the walk function.
     */
    while (--i >= 0)
    {
        p_int j;
        svalue_t *sp2, *data;

        if (!callback_object(&cb))
            errorf("Object used by walk_mapping destructed\n");

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
} /* v_walk_mapping() */

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
    p_int num_values;        /* Width of the mapping */
    p_int num_entries;       /* Size of the mapping */
    vector_t *dvec;          /* Values of one key */
    svalue_t *dvec_sp;       /* Stackentry of dvec */
    svalue_t *read_pointer;  /* Prepared mapping values */
    svalue_t *v;
    p_int i, j;

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
    num_entries = m->num_entries;

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
            free_callback(&cb);
            errorf("Out of memory\n");
        }
        ++sp;
        put_array(sp, dvec);
        dvec_sp = sp;
    }

    read_pointer = walk_mapping_prologue(m, sp, &cb);

    m = allocate_mapping(num_entries, num_values);
    if (!m)
    {
        inter_sp = sp + 1;
        errorf("Out of memory\n");
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
    for (i = num_entries; --i >= 0; read_pointer += 2)
    {
        svalue_t *data;

        /* Check if somebody took a reference to the old dvec.
         * If yes, we need to create a new one.
         */
        if (dvec != NULL && dvec->ref > 1)
        {
            free_array(dvec);
            dvec = allocate_array(num_values);
            if (!dvec)
            {
                put_number(dvec_sp, 0);
                inter_sp = sp;
                free_callback(&cb);
                errorf("Out of memory\n");
            }
            else
                put_array(dvec_sp, dvec);
        }

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
            errorf("Object used by %s destructed"
                 , bFull ? "filter" : "filter_mapping");


        v = apply_callback(&cb, 1 + bFull);

        /* Did the filter return TRUE? */
        if (!v || (v->type == T_NUMBER && !v->u.number) )
            continue;

        /* If we come here, the filter function returned 'true'.
         * Therefore assign the pair to the new mapping.
         */
        v = get_map_lvalue_unchecked(m, read_pointer);
        if (!v)
        {
            outofmemory("filtered entry");
            /* NOTREACHED */
            return NULL;
        }
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
v_filter_indices (svalue_t *sp, int num_arg)

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
}  /* v_filter_indices() */

/*-------------------------------------------------------------------------*/
svalue_t *
x_map_mapping (svalue_t *sp, int num_arg, Bool bFull)

/* EFUN map() on mappings, map_indices()
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
    p_int num_values;        /* Width of the mapping */
    vector_t *vec;           /* Indices of m */
    svalue_t *dvec_sp;       /* Stackentry of dvec */
    vector_t *dvec;          /* Values of one key */
    p_int i;
    svalue_t *key;
    callback_t cb;
    int error_index;

    /* Locate and extract arguments */
    arg = sp - num_arg + 1;
    inter_sp = sp;

    error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
    inter_sp = sp = arg;
    num_arg = 2;

    if (error_index >= 0)
    {
        vefun_bad_arg(error_index+2, sp);
        /* NOTREACHED */
        return sp;
    }

    sp++;
    inter_sp = sp;
    put_callback(sp, &cb);

    /* Preparations */

    arg_m = arg[0].u.map;

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
            errorf("Out of memory\n");
        }
        ++sp;
        put_array(sp, dvec);
        dvec_sp = sp;
    }

    m = allocate_mapping((i = VEC_SIZE(vec)), 1);
    if (!m)
    {
        inter_sp = sp;
        errorf("Out of memory\n");
    }
    ++sp;
    put_mapping(sp, m);

      /* Both cb, vec, dvec and m are kept referenced on the stack so that
       * in case of an error they are properly dereferenced.
       * At a normal termination however, m will not be dereferenced
       * but cb, vec and dvec will.
       */

    key = vec->item;
    for (; --i >= 0; key++) {
        svalue_t *v;
        svalue_t *data;

        /* Check if somebody took a reference to the old dvec.
         * If yes, we need to create a new one.
         */
        if (dvec != NULL && dvec->ref > 1)
        {
            free_array(dvec);
            dvec = allocate_array(num_values);
            if (!dvec)
            {
                put_number(dvec_sp, 0);
                inter_sp = sp;
                errorf("Out of memory\n");
            }
            else
                put_array(dvec_sp, dvec);
        }

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
                p_int j;
                svalue_t *svp;

                v = get_map_value(arg_m, key);
                for (j = 0, svp = dvec->item; j < num_values; j++, svp++, v++)
                    assign_svalue(svp, v);
                push_svalue(dvec_sp);
            }
        }

        /* Call the filter function */
        v = get_map_lvalue_unchecked(m, key);
        if (!v)
        {
            outofmemory("mapped entry");
            /* NOTREACHED */
            return NULL;
        }

        if (!callback_object(&cb))
            errorf("Object used by %s destructed"
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
v_map_indices (svalue_t *sp, int num_arg)

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
}  /* v_map_indices() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_m_contains (svalue_t *sp, int num_arg)

/* EFUN m_contains()
 *
 *   int m_contains(mixed &data1, ..., &dataN, map, key)
 *
 * If the mapping contains the key map, the corresponding values
 * are assigned to the data arguments, which must be passed by
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
        errorf("Not enough lvalues: given %d, required %"PRIdPINT".\n",
               num_arg-2, sp[-1].u.map->num_values);

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
} /* v_m_contains() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_m_entry (svalue_t *sp)

/* TEFUN m_entry()
 *
 *    mixed * m_entry (mapping m, mixed key)
 *
 * Query the mapping <m> for key <key> and return all values for this
 * key as array.
 * If the mapping does not contain an entry for <key>, svalue-0 is
 * returned.
 */

{
    svalue_t * data;
    vector_t * rc;

    data = get_map_value(sp[-1].u.map, sp);
    if (&const0 != data)
    {
        p_int num_values = sp[-1].u.map->num_values;
        p_int i;

        rc = allocate_array(num_values);

        for (i = 0; i < num_values; i++)
        {
            assign_svalue(rc->item+i, data+i);
        }
    }
    else
        rc = NULL;

    free_svalue(sp); sp--;
    free_svalue(sp);

    if (rc)
        put_array(sp, rc);
    else
        put_number(sp, 0);

    return sp;
} /* f_m_entry() */

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
    p_int      new_width;  /* Requested width of the target mapping */
    mapping_t *m;          /* Argument mapping */
    mapping_t *new_m;      /* New mapping */

    /* Test and get arguments */
    new_width = sp->u.number;
    if (new_width < 0)
    {
        errorf("Illegal width to m_reallocate(): %"PRIdPINT"\n", new_width);
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
        errorf("Out of memory.\n");
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
v_mkmapping (svalue_t *sp, int num_arg)

/* EFUN mkmapping()
 *
 *   mapping mkmapping(mixed *arr1, mixed *arr2,...)
 *
 * Returns a mapping with indices from 'arr1' and values from
 * 'arr2'... . arr1[0] will index arr2...[0], arr1[1] will index
 * arr2...[1], etc. If the arrays are of unequal size, the mapping
 * will only contain as much elements as are in the smallest
 * array.
 *
#ifdef USE_STRUCTS
 *   mapping mkmapping(struct st)
 *
 * Return a mapping with all values from struct <st>, indexed by
 * the struct's member names.
#endif
 */

{
    mapping_t *m;

    m = NULL;

#ifdef USE_STRUCTS
    if (sp[-num_arg+1].type == T_STRUCT)
    {
        struct_t * st;
        long i, length;

        /* Check the arguments and determine the mapping length.
         */
        if (num_arg > 1)
            errorf("Too many arguments to mkmapping(): expected struct\n");

        st = sp->u.strct;
        length = struct_size(st);

        if (max_mapping_size && length > (p_int)max_mapping_size)
            errorf("Illegal mapping size: %ld elements\n", length);
        if (max_mapping_keys && length > (p_int)max_mapping_keys)
            errorf("Illegal mapping size: %ld entries\n", length);

        /* Allocate the mapping and assign the values */
        m = allocate_mapping(length, 1);
        if (!m)
            errorf("Out of memory\n");

        for (i = 0; i < length; i++)
        {
            svalue_t   key;
            svalue_t * data;

            put_string(&key, st->type->member[i].name);
            data = get_map_lvalue_unchecked(m, &key);
            assign_svalue(data, &st->member[i]);
        }
    }
#endif

    if (sp[-num_arg+1].type == T_POINTER)
    {
        int i, num_values;   /* contains num_arg, which is int */
        p_int length;     /* VEC_SIZE, array sizes */
        svalue_t *key;

        /* Check the arguments and set length to the size of
         * the shortest array.
         */
        length = PINT_MAX;
        for (i = -num_arg; ++i <= 0; )
        {
            if ( sp[i].type != T_POINTER )
                vefun_arg_error(i+num_arg, T_POINTER, sp[i].type, sp);
            if (length > VEC_SIZE(sp[i].u.vec))
                length = VEC_SIZE(sp[i].u.vec);
        }

        if (max_mapping_size && (mp_int)length * num_arg > (mp_int)max_mapping_size)
            errorf("Illegal mapping size: %"PRIdMPINT
                   " elements (%"PRIdPINT" x %d)\n"
                 , (mp_int)length * num_arg, length, num_arg);
        if (max_mapping_keys && length > (p_int)max_mapping_keys)
            errorf("Illegal mapping size: %"PRIdPINT" entries\n", length);

        /* Allocate the mapping */
        num_values = num_arg - 1;
        m = allocate_mapping(length, num_values);
        if (!m)
            errorf("Out of memory\n");

        /* Shift key through the first array and assign the values
         * from the others.
         */
        key = &(sp-num_values)->u.vec->item[length];
        while (--length >= 0)
        {
            svalue_t *dest;

            dest = get_map_lvalue_unchecked(m, --key);
            if (!dest)
            {
                outofmemory("new mapping entry");
                /* NOTREACHED */
                return NULL;
            }
            for (i = -num_values; ++i <= 0; )
            {
                /* If a key value appears multiple times, we have to free
                 * a previous assigned value to avoid a memory leak
                 */
                assign_svalue(dest++, &sp[i].u.vec->item[length]);
            }
        }
    }

    /* If m is NULL at this point, we got an illegal argument */
    if (m == NULL)
    {
        fatal("Illegal argument to mkmapping(): %s, expected array/struct.\n"
             , typename(sp[-num_arg+1].type));
    }

    /* Clean up the stack and push the result */
    sp = pop_n_elems(num_arg, sp);
    push_mapping(sp, m);

    return sp;
} /* v_mkmapping() */

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
    p_int i;

    /* Get the arguments */
    m = sp->u.map;

    /* Determine the size of the mapping and allocate the result vector */
    check_map_for_destr(m);
    size = MAP_SIZE(m);
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
    p_int width;

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
