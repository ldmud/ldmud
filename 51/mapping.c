/*---------------------------------------------------------------------------
 * Mapping handling functions.
 *
 *---------------------------------------------------------------------------
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
 * A mapping consists of three structures (defined in datatypes.h):
 *
 *  - the struct mapping is the base of all mappings.
 *  - the struct hash_mapping keeps track of the recent changes
 *    to the mapping.
 *  - the struct condensed_mapping holds all older data in a
 *    memory effective format.
 *
 * Using this approach, mappings manage to combine a low memory overhead
 * with fast operation. Both the hashed and the condensed part may
 * be absent.
 *
 * All mappings with a hash_mapping structure are considered 'dirty'
 * and kept in a singly-linked list. The backend (or the garbage collector)
 * calls in regular intervals the function compact_mappings(), which
 * traverses the dirty list and 'cleans' the mappings by sorting the
 * hashed entries into the condensed part, removing the hashed part by
 * this.
 *
 *
 * -- struct mapping --
 *
 *   struct mapping {
 *       p_int                     ref;
 *       struct hash_mapping      *hash;
 *       struct condensed_mapping *condensed;
 *       struct wiz_list          *user;
 *       int                       num_values;
 *   }
 *
 *   .ref is the number of references, as usual.
 *   .hash and .condensed point to the hashed resp. condensed part of
 *     the mapping. If .condensed is NULL, the mapping is factually empty,
 *     but not yet deallocated because it is member of the dirtylist.
 *     During the garbage collection, .hash is used for a temporary list
 *     of mappings with 'stale' keys (ie keys referencing destructed objects
 *     or lambdas).
 *   .user is, as usual, the wizlist entry of the owner object.
 *   .num_values denotes the 'width' of the mapping, ie. how many values
 *     are stored for every key.
 *
 *
 * -- struct condensed_mapping --
 *
 *   struct condensed_mapping {
 *       "struct svalue m_values[ ... ];"
 *       "struct svalue misc[ ... ];"
 *       p_int misc_size;
 *       p_int string_size;
 *       "char *string[ ... ];"
 *       "struct svalue s_values[ ... ];"
 *   }
 *
 *   Well, things are a bit more complicated than that: the actual
 *   struct condensed_mapping consists only of the members .misc_size
 *   and .string_size, with the struct mapping.condensed pointing
 *   to struct condensed_mapping.misc_size. However, on creation the
 *   structure is always embedded in a memory block big enough to
 *   hold the implied members string[], s_values[], misc[] and m_values[]
 *   as well.
 *
 *   This condensed part of a mapping distinguishes between entries
 *   index by strings and entries indexed by other 'misc' values.
 *   For each type, the data is kept in two areas. The first area
 *   is an array of the key values (string[] resp. misc[]), the second
 *   area is an array of values for each key (s_values[] resp. m_values[]).
 *   Within the value arrays, the values for one entry are always stored
 *   consecutive. The association between the key and value areas is
 *   (assuming 'num' values per key):
 *
 *     .string[x] -> .s_values[num * x .. num * x + num - 1]
 *     .misc[x]   -> .m_values[num * x .. num * x + num - 1]
 *
 *   The keys are stored in sorted order (so that indexing operations
 *   can use a fast binary search). The sorting order for string keys
 *   is given by their address (unique because they are shared), the
 *   sorting order for the misc keys is given by the tuple
 *   (.u.number, .x.generic, .type).
 *
 *   Deleted or otherwise invalid string keys have an odd pointer
 *   value, invalid misc keys have the .type T_INVALID. Both are
 *   still kept in proper sorting order (or course!). The values for
 *   invalid keys are usually set to svalue-0.
 *
 *   .misc_size and .string_size give the size of the .misc[] resp.
 *   .string[] arrays in byte (for speed reasons).
 *
 *   A few macros help with this structure:
 *
 *     CM_MISC(cm)
 *       For condensed_mapping <cm>, return a pointer to the svalue
 *       after the last misc key in that mapping.
 *
 *     CM_STRING(cm)
 *       For condensed_mapping <cm>, return a pointer to the first
 *       string key in that mapping.
 *
 *
 * -- struct hash_mapping --
 *
 *   struct hash_mapping {
 *       p_int mask;
 *       p_int used;
 *       p_int condensed_deleted;
 *       p_int ref;
 *       struct map_chain *deleted;
 *       struct mapping   *next_dirty;
 *       struct map_chain *chains[ 1 +.mask ]; 
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
 * -- struct map_chain --
 *
 *   This structure is used to keep single entries in the hash chains
 *   of hash_mapping, and occasionally, in the .deleted list of
 *   protector mappings.
 *
 *   struct map_chain {
 *       struct map_chain *next;
 *       struct svalue key;
 *       struct svalue data[map.num_values];
 *   }
 *
 *   .next is the next struct map_chain in the hash chain (or .deleted list).
 *   .key is the key value of the entry, .data[] are the data values.
 *   Some places in the module assume that .data[-1] == .key.
 *
 *   The structure is allocated big enough to hold all the values.
 *   This macro helps with the computation:
 *
 *     MAP_CHAIN_SIZE(n)
 *       Size of a map_chain structure for <n> values
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include "my-alloca.h"
#include <stdio.h>

#include "mapping.h"

#include "array.h"
#include "backend.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "object.h"
#include "regexp.h"
#include "simulate.h"
#include "smalloc.h"
#include "stralloc.h"
#include "wiz_list.h"

#define MIN_P_INT  ( (p_int)-1  << (sizeof(p_int)  * 8 - 1) )
#define MIN_PH_INT ( (ph_int)-1 << (sizeof(ph_int) * 8 - 1) )
  /* Smallest value a p_int/ph_int variable can hold.
   */

/*-------------------------------------------------------------------------*/

#define EMPTY_MAPPING_THRESHOLD 2000
  /* Number of 'freed' empty mappings allowed in the dirty list
   * at any time. This way the dirty list can be single-linked only
   * and still allow fast 'freeing' of unused mappings.
   */

static struct hash_mapping dirty_mapping_head_hash;
  /* Auxiliary structure dirty_mapping_head can reference
   */

static struct mapping dirty_mapping_head
  = {
    /* ref        */ 1,
    /* hash       */ &dirty_mapping_head_hash,
    /* condensed  */ 0,
    /* user       */ 0,
    /* num_values */ 0
    };
  /* Head of the list of (potentially) dirty mappings, ie. mappings
   * with a hash_mapping part.
   */

static struct mapping *last_dirty_mapping = &dirty_mapping_head;
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

struct mapping *stale_mappings;
  /* During a garbage collection, this is a list of mappings with
   * keys referencing destructed objects/lambdas. Since during this
   * phase all mappings are compacted, the list is linked through
   * the .hash pointers.
   */

static struct svalue walk_mapping_string_svalue
  = { T_STRING };
  /* Stand-in svalue for string-keys, to be passed to the callback
   * function when doing a walk_mapping().
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void remove_empty_mappings(void);

/*-------------------------------------------------------------------------*/
struct mapping *
allocate_mapping (mp_int size, mp_int num_values)

/* Allocate a mapping with <num_values> values per key, and setup the
 * hash part for (initially) <size> entries. The condensed part will
 * contain 0 entries.
 *
 * Return the new mapping, or NULL when out of memory.
 */

{
    struct hash_mapping *hm;
    struct condensed_mapping *cm;
    struct mapping *m;

    /* Allocate the structures */
    m = xalloc(sizeof *m);
    if (!m)
        return NULL;
    cm = xalloc(sizeof *cm);
    if (!cm)
    {
        xfree(m);
        return NULL;
    }

    /* If <size> is given, create a hash_mapping structure <hm> and
     * setup it up to hold that many entries.
     */

    hm = NULL;
    if (size)
    {
        struct map_chain **mcp;

        /* Compute the number of hash buckets to 2**k, where
         * k is such that 2**(k+1) > size >= 2**k.
         *
         * To do this, compute 'size' to (2**k)-1 by first setting
         * all bits after the leading '1' and then shifting the
         * number right once. The result is then also the mask
         * required for indexing.
         */
        size |= size >> 1;
        size |= size >> 2;
        size |= size >> 4;
        if (size & ~0xff)
        {
            size |= size >> 8;
            size |= size >> 16;
        }
        size >>= 1;

        /* Allocate the hash_mapping big enough to hold (size+1) hash
         * buckets.
         * size must not exceed the accessible indexing range. This is
         * a possibility because size as a mp_int may have a different
         * range than array indices which are size_t.
         * TODO: The 0x100000 seems to be a safety offset, but is it?
         */
        if (size > (mp_int)((MAXINT - sizeof *hm - 0x100000) / sizeof *mcp)
         || !(hm = xalloc(sizeof *hm + sizeof *mcp * size) ) )
        {
            xfree(cm);
            xfree(m);
            return NULL;
        }

        hm->mask = size;
        hm->used = hm->condensed_deleted = hm->ref = 0;

        /* With this hash_mapping structure, the mapping counts
         * as potentially dirty.
         */
        last_dirty_mapping->hash->next_dirty = m;
        last_dirty_mapping = m;
#ifdef DEBUG
        /* These members don't really need a default initialisation
         * but it's here to catch bogies.
         */
        hm->next_dirty = NULL;
        hm->deleted = NULL;
#endif
        num_dirty_mappings++;

        /* Inform backend that there is a new mapping to condense */
        extra_jobs_to_do = MY_TRUE;

        /* Initialise the hashbuckets */
        mcp = hm->chains;
        do *mcp++ = NULL; while (--size >= 0);
    }

    /* Initialise the mapping */
    cm->string_size = 0;
    cm->misc_size = 0;
    m->hash = hm;
    m->condensed = cm;
    m->num_values = num_values;
    m->ref = 1;

    /* Statistics */
    (m->user = current_object->user)->mapping_total +=
      sizeof *m + sizeof(char*) + sizeof *cm + sizeof(char*);
    num_mappings++;

    return m;

} /* allocate_mapping() */

/*-------------------------------------------------------------------------*/
void
free_mapping (struct mapping *m)

/* Decrement the refcount of mapping <m>. If it reaches 0, the mapping
 * and all associated memory is deallocated resp. dereferenced.
 *
 * If the mapping is 'dirty' (ie. contains a hash_mapping part), it
 * is not deallocated immediately, but instead counts 2 to the empty_mapping-
 * _load (with regard to the threshold).
 */

{
    struct hash_mapping *hm;       /* Hashed part of <m> */
    struct condensed_mapping *cm;  /* Condensed part of <m> */
    char **str;                    /* First/next string key in <cm> */
    struct svalue *svp;            /* Last+1 misc key in <cm> */
    int num_values;                /* Number of values in <m> */
    int i, j;

#ifdef DEBUG
    if (!m->user)
        fatal("No wizlist pointer for mapping");
#endif

    if (--m->ref)
        return;

    /* Yup, it's a real one */


    num_mappings--;

    num_values = m->num_values;

    cm = m->condensed;


    /* Dereference all valid key strings */
    
    str = CM_STRING(cm);
    i = cm->string_size;
    while ( (i -= sizeof *str) >= 0)
    {
        if ( !((p_int)*str & 1) )
            free_string(*str);
        str++;
    }

    /* Dereference the values for the string keys */

    svp = (struct svalue *)str;
    i = cm->string_size * num_values;
    while ( (i -= sizeof *str) >= 0)
    {
        free_svalue(svp++);
    }


    /* Dereference all misc keys and their values */

    svp = CM_MISC(cm);
    i = cm->misc_size * (num_values + 1);
    while ( (i -= sizeof *svp) >= 0)
        free_svalue(--svp);


    /* Subtract the memory allocated by the condensed part from
     * the users account.
     */
    m->user->mapping_total -=   sizeof *m + sizeof(char *)
                              + sizeof *cm + sizeof(char *)
                              +    (cm->string_size * (sizeof *svp/sizeof *str)
                                    + cm->misc_size)
                                *  (1 + num_values)
                              - cm->string_size * (sizeof *svp/sizeof *str - 1)
                            ;

    xfree( (char *)svp); /* free the condensed mapping part */


    /* If there is a hashed part, free that one, but keep the mapping
     * itself allocated (to not disrupt the dirty-mapping list).
     */

    if ( NULL != (hm = m->hash) )
    {
        struct map_chain **mcp, *mc, *next;
        struct mapping *next_dirty;

#ifdef DEBUG
        if (hm->ref)
            fatal("Ref count in freed hash mapping: %ld\n", hm->ref);
#endif
        mcp = hm->chains;

        /* Loop through all chains */
        
        i = hm->mask + 1;
        do {

            /* Free this chain */

            for (next = *mcp++; NULL != (mc = next); )
            {
                svp = &mc->key;
                j = num_values;
                do {
                    free_svalue(svp++);
                } while (--j >= 0);
                next = mc->next;
                xfree( (char *)mc );
            }
        } while (--i);

        /* Replace this hash_mapping with an empty one and
         * mark the mapping itself as empty.
         */
        next_dirty = hm->next_dirty;
        xfree( (char *)hm );

        hm = (struct hash_mapping *)xalloc(sizeof *hm);
        hm->mask = hm->used = hm->condensed_deleted = hm->ref = 0;
        hm->chains[0] = 0;
        hm->next_dirty = next_dirty;

        m->condensed = 0;
        m->hash = hm;

        /* Count this new empty mapping, removing all empty
         * mappings if necessary
         */
        if ( (empty_mapping_load += 2) > 0)
            remove_empty_mappings();

        return;
    }


    /* Finally free the base structure (not reached for dirty mappings).
     */

    xfree( (char *)m );

} /* free_mapping() */

/*-------------------------------------------------------------------------*/
void
free_empty_mapping (struct mapping *m)

/* Free a mapping <m> which is known to not contain any valid keys or
 * values. The ref-count is assumed to be 0, too.
 *
 * If the mapping is 'dirty' (ie. contains a hash_mapping part), it
 * is not deallocated immediately, but instead counts 2 to the empty_mapping-
 * _load (with regard to the threshold).
 */

{
    struct hash_mapping *hm;
    struct condensed_mapping *cm;
    mp_int  num_values;

#ifdef DEBUG
    if (!m->user)
        fatal("No wizlist pointer for mapping");
#endif

    num_mappings--;

    num_values = m->num_values;
    
    cm = m->condensed;

    /* Subtract the memory allocated by the condensed part from
     * the users account.
     */
    m->user->mapping_total -=   sizeof *m + sizeof(char *) + sizeof *cm
                              + sizeof(char *)
                              +   (   cm->string_size
                                   * (sizeof(struct svalue)/sizeof(char*))
                                   + cm->misc_size)
                                * (1 + num_values)
                              -    cm->string_size
                                * (sizeof(struct svalue)/sizeof(char*) - 1)
                            ;

    /* free the condensed mapping part */
    xfree( (char *)CM_MISC(cm) - cm->misc_size * (num_values + 1) );


    /* If there is a hashed part, free that one, but keep the mapping
     * itself allocated (to not disrupt the dirty-mapping list).
     */

    if ( NULL != (hm = m->hash) )
    {
        struct map_chain **mcp, *mc, *next;
        struct mapping *next_dirty;
        mp_int i;

#ifdef DEBUG
        if (hm->ref)
            fatal("Ref count in freed hash mapping: %ld\n", hm->ref);
#endif
        mcp = hm->chains;

        /* Loop through all chains */

        i = hm->mask + 1;
        do {
            /* Free this chain */

            for (next = *mcp++; NULL != (mc = next); )
            {
                next = mc->next;
                xfree( (char *)mc );
            }
        } while (--i);

        /* Replace this hash_mapping with an empty one and
         * mark the mapping itself as empty.
         */
        next_dirty = hm->next_dirty;
        xfree( (char *)hm );

        hm = (struct hash_mapping *)xalloc(sizeof *hm);
        hm->mask = hm->used = hm->condensed_deleted = hm->ref = 0;
        hm->chains[0] = 0;
        hm->next_dirty = next_dirty;

        m->condensed = 0;
        m->hash = hm;

        /* Count this new empty mapping, removing all empty
         * mappings if necessary
         */
        if ( (empty_mapping_load += 2) > 0)
            remove_empty_mappings();

        return;
    }

    /* Finally free the base structure (not reached for dirty mappings).
     */

    xfree( (char *)m );

} /* free_empty_mapping() */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

void
check_dirty_mapping_list (void)

/* Check the list of dirty mappings for consistency, generating a fatal()
 * error if not.
 */

{
    int i;
    struct mapping *m;

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
 * the number empty mappings. If it crosses the threshhold, remove
 * the empty mappings from the list.
 */

{
    struct mapping **mp, *m, *last;
    struct hash_mapping *hm;

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
        if (!m->condensed)
        {
            xfree((char *)m);
            *mp = m = hm->next_dirty;
            xfree( (char *)hm );
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
free_protector_mapping (struct mapping *m)

/* Free the mapping <m> which is part of a T_PROTECTOR_MAPPING svalue.
 * Such svalues are created only for mappings with a hashed part, and
 * have the ref of the hashed part incremented at creation.
 *
 * This function is a wrapper around free_mapping() and takes care
 * to free m->hash->deleted if m->hash->ref reaches zero due to this
 * call.
 */

{
    struct hash_mapping *hm;

#ifdef DEBUG
    /* This type of mapping must have a hash part */

    if (!m->hash || m->hash->ref <= 0)
    {
        /* This shouldn't happen */
        printf("free_protector_mapping() : no hash %s\n"
              , m->hash ? "reference" : "part");
#ifdef TRACE_CODE
        {
            last_instructions(TOTAL_TRACE_LENGTH, 1, 0);
        }
#endif
        dump_trace(0);
        printf("free_protector_mapping() : no hash %s\n",
                m->hash ? "reference" : "part");
        free_mapping(m);
    }
#endif /* DEBUG */


    /* If this was the last protective reference, execute
     * the pending deletions.
     */

    if (!--(hm = m->hash)->ref)
    {
        int num_values = m->num_values;
        struct map_chain *mc, *next;
        struct svalue *svp2;

        for (mc = hm->deleted; mc; mc = next)
        {
            mp_int j;

            svp2 = &mc->key;
            j = num_values;
            do {
                free_svalue(svp2++);
            } while (--j >= 0);
            next = mc->next;
            xfree( (char *)mc );
        }
    }

    /* Call free_mapping() if appropriate */

    if (--m->ref)
        return;
    m->ref++;
    free_mapping(m);
    
} /* free_protector_mapping() */

/*-------------------------------------------------------------------------*/
struct svalue *
get_map_lvalue (struct mapping *m, struct svalue *map_index, /* TODO: BOOL */ short need_lvalue)

/* Index mapping <m> with key value <map_index> and return a pointer to the
 * array of values stored for this key.
 *
 * If the mapping does not contains the given index, and <need_lvalue> is
 * false, &const0 is returned. If <need_lvalue> is true, a new key/value
 * entry is created and returned.
 *
 * Return NULL when out of memory.
 *
 * Sideeffect: if <map_index> is an unshared string, it is made shared.
 */

{
    mp_int size;
    struct condensed_mapping *cm = m->condensed;
    struct hash_mapping *hm;
    int num_values = m->num_values;
    struct svalue *svp;


    /* Search in the condensed part first.
     */

    switch (map_index->type)
    {
    /* ----- String Indexing ----- */
     
    case T_STRING:
      {
        char *str;
        char *key; /* means a char **, but pointer arithmetic wants char * */
        char *keystart, *keyend;

        /* We need a shared string for the search */

        if (map_index->x.string_type != STRING_SHARED)
        {
            char *tmpstr;

            tmpstr = make_shared_string(map_index->u.string);
            if (map_index->x.string_type == STRING_MALLOC)
                xfree(map_index->u.string);
            map_index->x.string_type = STRING_SHARED;
            map_index->u.string = tmpstr;
        }

        str = map_index->u.string;
        keystart = (char *)CM_STRING(cm);
        size = cm->string_size;
        if (size)
        {
            p_int offset;

            keyend = keystart + size;
            key = keystart;

            /* Set offset to the highest power of two which is still
             * less than size. This value is then used for the first
             * partition operation.
             */
            offset = size-1;
            offset |= offset >> 1;
            offset |= offset >> 2;
            offset |= offset >> 4;
            if (offset & ~0xff)
            {
                offset |= offset >> 8;
                offset |= offset >> 16;
            }

            /* Binary search for the key string unless/until offset
             * would denote a partition smaller than a string pointer.
             */
            if ( (offset = (offset+1) >> 1) >= (p_int)sizeof str)
                do {
                    if (key + offset >= keyend) continue;
                    if ( str >= *(char **)(key+offset) ) key += offset;
                } while ( (offset >>= 1) >= (p_int)sizeof str);

            /* If the correct string key was found, return the values */
            
            if ( str == *(char **)key )
            {
#ifndef FAST_MULTIPLICATION
                if (num_values == 1) /* speed up this case */
                    return (struct svalue *)
                      (keyend + (key - keystart ) *
                        (sizeof(struct svalue)/sizeof str) );
                else
#endif/*FAST_MULTIPLICATION*/
                    return (struct svalue *)
                      (keyend + (key - keystart ) *
                        ( num_values * (sizeof(struct svalue)/sizeof str) ));
            }

            /* If we come here, we didn't find it */
        }
        /* don't search if there are no string keys */
        break;
      }

    /* ----- Misc Indexing ----- */

    default: /* All types without secondary information */
      
        map_index->x.generic = map_index->u.number << 1;
        /* FALL THROUGH */

    case T_FLOAT:
    case T_CLOSURE:
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
      {
        p_int offset;
        char *key; /* means a char **, but pointer arithmetic wants char * */
        char *keystart, *keyend;
        ph_int index_type = map_index->type;
        ph_int index_x = map_index->x.generic;
        p_int index_u = map_index->u.number;
        p_int u_d;

        /* Setup the binary search
         */

        keyend = (char *)CM_MISC(cm);
        size = cm->misc_size;
        keystart = keyend - size;

        /* Set offset to the highest power of two which is still
         * less than size. This value is then used for the first
         * partition operation.
         */
        offset = size | size >> 1;
        offset |= offset >> 2;
        offset |= offset >> 4;
        if (offset & ~0xff)
        {
            offset |= offset >> 8;
            offset |= offset >> 16;
        }
        offset = (offset+1) >> 1;

        /* Binary search for the key string unless/until offset
         * would denote a partition smaller than a svalue.
         */
        key = keyend - offset;
        while ( (offset >>= 1) >= (p_int)(sizeof svp)/2)
        {
            if ( !(u_d = (((struct svalue *)key)->u.number >> 1) -
                       (index_u >> 1)) )
            {
                if ( !(u_d = ((struct svalue *)key)->x.generic - index_x) )
                  if ( !(u_d = ((struct svalue *)key)->type - index_type) )
                  {
                      /* found */
#ifndef FAST_MULTIPLICATION
                      if (num_values == 1) /* speed up this case */
                          return (struct svalue *) (key - size);
                      else
#endif/*FAST_MULTIPLICATION*/
                          return (struct svalue *)
                            (keystart - ( num_values * (keyend - key) ) );
                  }
            }
            if (u_d > 0)
            {
                key += offset;
            } else
            {
                /* u_d < 0 */
                key -= offset;
                while (key < keystart)
                {
                    if ( (offset >>= 1) < (p_int)(sizeof svp) )
                        break;
                    key += offset;
                }
            }
        }

        /* If we come here, we didn't find it */
        break;
      }

    }  /* switch(map_index->type) */


    /* At this point, the key was not found in the condensed part
     * of the mapping. If the mapping has a hash part, it is now
     * searched there, if not and if need_lvalue is true, it is
     * created.
     */
     
    if ( !(hm = m->hash) )
    {
        /* --- No Hash Part: create it if desired --- */
        
        struct map_chain *mc;
        mp_int i;

        /* No lvalue needed -> just return */

        if (!need_lvalue)
            return &const0;

        /* Create the hash part of the mapping and put
         * it into the dirty list.
         */

        hm = (struct hash_mapping *)xalloc(sizeof *hm);
        if (!hm)
            return NULL; /* Oops */

        hm->mask = hm->condensed_deleted = 0;
        hm->ref = 0;
        hm->used = 1;
        last_dirty_mapping->hash->next_dirty = m;
        last_dirty_mapping = m;
#ifdef DEBUG
        /* These members don't really need a default initialisation
         * but it's here to catch bogies.
         */
        hm->next_dirty = NULL;
        hm->deleted = NULL;
#endif
        num_dirty_mappings++;
        extra_jobs_to_do = MY_TRUE;  /* there are mappings to condense! */
        m->hash = hm;

        /* Now create the hashing structure with one empty entry */
        mc = (struct map_chain *)xalloc(MAP_CHAIN_SIZE(num_values));
        hm->chains[0] = mc;
        if (!mc)
            return NULL;
        mc->next = NULL;
        assign_svalue_no_free(&mc->key, map_index);
        svp = mc->data;
        for (i = num_values; --i >= 0; svp++)
        {
            svp->type = T_NUMBER;
            svp->u.number = 0;
        }
        return mc->data;
    }
    else
    {
        struct map_chain *mc;
        p_int index_type = *SVALUE_FULLTYPE(map_index);
        p_int index_u = map_index->u.number;
        mp_int i;

        /* Compute the hash value and make it a valid index */

        i = index_u ^ index_type;
        i = i ^ i >> 16;
        i = i ^ i >> 8;
        i &= hm->mask;

        /* Look for the value in the chain determined by i */
        
        for (mc = hm->chains[i];mc; mc = mc->next)
        {
            if (mc->key.u.number != index_u ||
                *SVALUE_FULLTYPE(&mc->key) != index_type)
                continue;
            return mc->data;
        }

        /* Not found and no lvalue needed -> return */

        if (!need_lvalue)
            return &const0;

        /* If the average number of map_chains per chain exceeds 2,
         * double the size of the bucket array.
         */
        if (hm->used & ~hm->mask<<1)
        {
            struct hash_mapping *hm2;
            mp_int mask, j;
            struct map_chain **mcp, **mcp2, *next;

            hm2 = hm;

            /* Compute new size and mask, and allocate the structure */
            
            size = (hm->mask << 1) + 2;
            mask = size - 1;

            hm = (struct hash_mapping *)
              xalloc(sizeof *hm - sizeof *mcp + sizeof *mcp * size);
            if (!hm)
                return NULL;

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
                    i = mc->key.u.number ^ *SVALUE_FULLTYPE(&mc->key);
                    i = i ^ i >> 16;
                    i = i ^ i >> 8;
                    i &= mask;
                    mc->next = mcp[i];
                    mcp[i] = mc;
                }
            }
            m->hash = hm;

            /* Away, old data! */
            
            xfree((char *)hm2);

            /* Update the hashed index i to the new structure */
            
            i = map_index->u.number ^ *SVALUE_FULLTYPE(map_index);
            i = i ^ i >> 16;
            i = i ^ i >> 8;
            i &= mask;
        }

        /* Create a new, empty entry for the index chain */
        
        mc = (struct map_chain *)xalloc(MAP_CHAIN_SIZE(num_values));
        if (!mc)
            return NULL;
        hm->used++;
        mc->next = hm->chains[i];
        hm->chains[i] = mc;
        assign_svalue_no_free(&mc->key, map_index);
        svp = mc->data;
        for (i = num_values; --i >= 0; svp++) {
            svp->type = T_NUMBER;
            svp->u.number = 0;
        }

        return mc->data;
    }

    /* NOTREACHED */
} /* get_map_lvalue() */

/*-------------------------------------------------------------------------*/
void
check_map_for_destr (struct mapping *m)

/* Check the mapping <m> for references to destructed objects.
 * Where they appear as keys, both key and associated values are
 * deleted from the mapping. Where they appear as values, they are
 * replace by svalue-0.
 */

{
    struct condensed_mapping *cm;
    struct hash_mapping *hm;
    struct svalue *svp;
    mp_int i, j;
    int num_values;

    num_values = m->num_values;
    
    cm = m->condensed;

    /* Scan the condensed part for destructed objects used as keys.
     */

    for (svp = CM_MISC(cm),i = cm->misc_size; (i -= sizeof *svp) >= 0; )
    {
        --svp;
        if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED)
        {
            struct svalue *data = NULL;

            /* Clear all associated values */
            
            if ( 0 != (j = num_values) )
            {
                data = (struct svalue *)((char *)svp - i -
                  num_values * ((char *)CM_MISC(cm) - (char *)svp));
                do {
                    free_svalue(data);
                    data->type = T_NUMBER;
                    data->u.number = 0;
                    data++;
                } while (--j);
            }

            /* If the following keys have a matching (.u.number, x.generic)
             * move them forward to replace this no longer valid entry.
             * This is necessary to keep the sorting relation intact.
             *
             * Afterwards, svp might no longer point to a T_OBJECT svalue,
             * but .u.number interpreted as .u.ob still points to object
             * causing all this trouble.
             */
            while ( &svp[1] < CM_MISC(cm)
             &&      svp[1].u.number == svp[0].u.number
             &&      svp[1].x.generic == svp[0].x.generic)
            {
                *SVALUE_FULLTYPE(&svp[0]) =  *SVALUE_FULLTYPE(&svp[1]);
                svp++;
                i += sizeof *svp;
                for (j = num_values; --j >= 0; data++)
                    data[-num_values] = data[0];
                    data->type = T_NUMBER;
                    data->u.number = 0;
            }

            /* We know that svp->u.ob is an object here, even if
             * svp->type disagrees.
             */
            free_object_svalue(svp);

            svp[0].type = T_INVALID;

            /* Count the deleted entry in the hash part.
             * Create it if necessary.
             */
            if ( !(hm = m->hash) )
            {
                hm = (struct hash_mapping *)xalloc(sizeof *hm);
                if (!hm)
                {
                    error("Out of memory\n");
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
            }

            hm->condensed_deleted++;
        } /* if (destructed object) */
    } /* for (all condensed keys) */


    /* Scan the misc-key values in the condensed part,
     * replacing with svalue-0s where appropriate.
     */

    for (i = cm->misc_size * num_values; (i -= sizeof *svp) >= 0; )
    {
        svp--;
        if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED)
        {
            assign_svalue(svp, &const0);
        }
    }

    /* Scan the string-key values in the condensed part,
     * replacing with svalue-0s where appropriate.
     */

    svp = (struct svalue *)( (char *)CM_STRING(cm) + cm->string_size );

    for (i = cm->string_size * num_values; (i -= sizeof(char *)) >= 0; svp++)
    {
        if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED)
        {
            assign_svalue(svp, &const0);
        }
    }


    /* If it exists, scan the hash part for destructed objects.
     */

    if ( NULL != (hm = m->hash) )
    {
        struct map_chain **mcp, **mcp2, *mc;

        /* Walk all chains */
        
        for (mcp = hm->chains, i = hm->mask + 1; --i >= 0;)
        {
            /* Walk this chain */
            
            for (mcp2 = mcp++; NULL != (mc = *mcp2); )
            {
                /* Destructed object as key: remove entry */
                
                if (mc->key.type == T_OBJECT
                 && mc->key.u.ob->flags & O_DESTRUCTED)
                {
                    svp = &mc->key;
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
                        j = num_values;
                        do {
                            free_svalue(svp++);
                        } while (--j >= 0);
                        xfree( (char *)mc );
                    }
                    hm->used--;
                    continue;
                }

                /* Scan the values of this entry (not reached
                 * if the entry was removed above
                 */
                for (svp = mc->data, j = num_values; --j >= 0; )
                {
                    if (svp->type == T_OBJECT
                     && svp->u.ob->flags & O_DESTRUCTED)
                    {
                        assign_svalue(svp, &const0);
                    }
                }

                mcp2 = &mc->next;

            } /* walk this chain */
        } /* walk all chains */
    } /* if (hash part exists) */

} /* check_map_for_destr() */

/*-------------------------------------------------------------------------*/
void
remove_mapping (struct mapping *m, struct svalue *map_index)

/* Remove from mapping <m> that entry which is index by key value
 * <map_index>. Nothing happens if it doesn't exist.
 *
 * Sideeffect: if <map_index> is an unshared string, it is made shared.
 *
 * TODO: This function could be combined with get_map_lvalue().
 */

{
    mp_int size;
    struct condensed_mapping *cm = m->condensed;
    struct hash_mapping *hm;
    int num_values = m->num_values;
    struct svalue *svp;

    /* Search in the condensed part first.
     */

    switch (map_index->type)
    {
    /* ----- String Indexing ----- */
     
    case T_STRING:
      {
        char *str;
        char *key; /* means a char **, but pointer arithmetic wants char * */
        char *keystart, *keyend;

        /* We need a shared string for the search */

        if (map_index->x.string_type != STRING_SHARED)
        {
            char *tmpstr;

            tmpstr = findstring(map_index->u.string);
            if (!tmpstr) {
                return;
            }
            if (map_index->x.string_type == STRING_MALLOC)
                xfree(map_index->u.string);
            map_index->x.string_type = STRING_SHARED;
            map_index->u.string = tmpstr;
            increment_string_ref(tmpstr);
        }

        str = map_index->u.string;
        keystart = (char *)CM_STRING(cm);
        size = cm->string_size;
        if (size) {
            p_int offset;

            keyend = keystart + size;
            key = keystart;

            /* Set offset to the highest power of two which is still
             * less than size. This value is then used for the first
             * partition operation.
             */
            offset = size-1;
            offset |= offset >> 1;
            offset |= offset >> 2;
            offset |= offset >> 4;
            if (offset & ~0xff) {
                offset |= offset >> 8;
                offset |= offset >> 16;
            }

            /* Binary search for the key string unless/until offset
             * would denote a partition smaller than a string pointer.
             */
            if ( (offset = (offset+1) >> 1) >= (p_int)sizeof str)
                do {
                    if (key + offset >= keyend) continue;
                    if ( str >= *(char **)(key+offset) ) key += offset;
                } while ( (offset >>= 1) >= (p_int)sizeof str);


            /* If the correct string key was found, remove the entry */
            
            if ( str == *(char **)key )
            {
                int i;

                /* Deallocate the string and mark the pointer as 'invalid'
                 */
                free_string(str);
                (*(char **)key)++;

                /* Zero out all associated values */

                svp = (struct svalue *)
                  (keyend + (key - keystart ) *
                    ( num_values * (sizeof(struct svalue)/sizeof str) ));
                for (i = num_values; --i >= 0 ;svp++)
                {
                    free_svalue(svp);
                    svp->type = T_NUMBER;
                    svp->u.number = 0;
                }

                /* Count the deleted entry in the hash part.
                 * Create it if necessary.
                 */
                if ( !(hm = m->hash) )
                {
                    hm = (struct hash_mapping *)xalloc(sizeof *hm);
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
                }

                hm->condensed_deleted++;

                return;
            }

            /* If we come here, we didn't find it */
        }
        /* don't search if there are no string keys */
        break;
      }

    /* ----- Misc Indexing ----- */

    default: /* All types without secondary information */
      
        map_index->x.generic = map_index->u.number << 1;
        /* FALL THROUGH */

    case T_FLOAT:
    case T_CLOSURE:
    case T_SYMBOL:
    case T_QUOTED_ARRAY:
      {
        /* map_index->type != T_STRING */

        p_int offset;
        char *key; /* means a char **, but pointer arithmetic wants char * */
        char *keystart, *keyend;
        ph_int index_type = map_index->type;
        ph_int index_x = map_index->x.generic;
        p_int index_u = map_index->u.number, u_d;

        /* Setup the binary search
         */

        keyend = (char *)CM_MISC(cm);
        size = cm->misc_size;
        keystart = keyend - size;

        /* Set offset to the highest power of two which is still
         * less than size. This value is then used for the first
         * partition operation.
         */
        offset = size | size >> 1;
        offset |= offset >> 2;
        offset |= offset >> 4;
        if (offset & ~0xff) {
            offset |= offset >> 8;
            offset |= offset >> 16;
        }
        offset = (offset+1) >> 1;

        /* Binary search for the key string unless/until offset
         * would denote a partition smaller than a svalue.
         */
        key = keyend - offset;
        while ( (offset >>= 1) >= (p_int)(sizeof svp)/2) {
            if ( !(u_d = (((struct svalue *)key)->u.number >> 1) -
                         (index_u >> 1)) )
            {
              if ( !(u_d = ((struct svalue *)key)->x.generic - index_x) )
                if ( !(u_d = ((struct svalue *)key)->type - index_type) )
                {
                    int i;

                    /* Deallocate the found key and zero out
                     * its associated values
                     */
                    
                    free_svalue( (struct svalue *)key );
                    svp = (struct svalue *)
                      (keystart - ( num_values * (keyend - key) ) );
                    for (i = num_values; --i >= 0 ;svp++) {
                        free_svalue(svp);
                        svp->type = T_NUMBER;
                        svp->u.number = 0;
                    }

                    /* If the following keys have a matching (.u.number,
                     * x.generic) move them forward to replace this no
                     * longer valid entry.
                     * This is necessary to keep the sorting relation
                     * intact.
                     */
                    while ( ((struct svalue *)key+1)->u.number == index_u
                     &&     ((struct svalue *)key+1)->x.generic == index_x
                     &&     key + sizeof(struct svalue) < keyend)
                    {
                        struct svalue *svp2;

                        *((struct svalue *)key) = *((struct svalue *)key+1);
                        key += sizeof(struct svalue);
                        svp2 = svp - num_values;
                        for (i = num_values; --i >= 0 ;svp++, svp2++)
                        {
                            *svp2 = *svp;
                            svp->type = T_NUMBER;
                            svp->u.number = 0;
                        }
                    }

                    /* key is now the last of its (.u.number, x.generic)
                     * kind: make it invalid
                     */
                    ((struct svalue *)key)->type = T_INVALID;

                    /* Count the deleted entry in the hash part.
                     * Create it if necessary.
                     */
                    if ( !(hm = m->hash) )
                    {
                        hm = (struct hash_mapping *)xalloc(sizeof *hm);
                        if (!hm)
                        {
                            error("Out of memory\n");
                            /* NOTREACHED */
                            return;
                        }
                        m->hash = hm;
                        hm->mask = hm->used = hm->condensed_deleted = 0;
                        hm->chains[0] = 0;
                        last_dirty_mapping->hash->next_dirty = m;
                        last_dirty_mapping = m;
#ifdef DEBUG
                        hm->next_dirty = 0;
                        hm->deleted = 0;
#endif
                        hm->ref = 0;
                        num_dirty_mappings++;
                        extra_jobs_to_do = MY_TRUE;
                    }

                    hm->condensed_deleted++;
                    return;
                }
            }

            if (u_d > 0) {
                key += offset;
            } else {
                /* u_d < 0 */
                key -= offset;
                while (key < keystart) {
                    if ( (offset >>= 1) < (p_int)(sizeof svp) )
                        break;
                    key += offset;
                }
            }
        }

        /* If we come here, we didn't find it */
        break;
      }

    } /* switch(map_index->type) */

    /* At this point, the key was not found in the condensed part
     * of the mapping. If the mapping has a hash part, it is now
     * searched there.
     */
     
    if ( NULL != (hm = m->hash) )
    {
        struct map_chain **mcp, *mc;
        p_int index_type = *SVALUE_FULLTYPE(map_index);
        p_int index_u = map_index->u.number;
        mp_int i;

        /* Index and walk the proper chain */

        i = index_u ^ index_type;
        i = i ^ i >> 16;
        i = i ^ i >> 8;
        i &= hm->mask;

        for(mcp = &hm->chains[i]; NULL != (mc = *mcp); )
        {
            if (mc->key.u.number == index_u
             && *SVALUE_FULLTYPE(&mc->key) == index_type)
            {
                int j;

                *mcp = mc->next;

                /* If the mapping is a protector mapping, move
                 * the entry into the 'deleted' list, else
                 * just deallocate it.
                 */
                if (hm->ref)
                {
                    mc->next = hm->deleted;
                    hm->deleted = mc;
                } else {
                    svp = &mc->key;
                    j = num_values;
                    do {
                        free_svalue(svp++);
                    } while (--j >= 0);
                    xfree( (char *)mc );
                }
                hm->used--;
                return;
            }
            mcp = &mc->next;
        }
    }

    /* Here, the key was not found at all. Just return. */

} /* remove_mapping() */

/*-------------------------------------------------------------------------*/
struct mapping *
copy_mapping (struct mapping *m)

/* Produce a shallow copy of mapping <m> and return it.
 * The copy of a protector mapping is a normal mapping.
 */

{
    struct mapping *m2;
    struct hash_mapping *hm, *hm2 = 0;
    struct condensed_mapping *cm, *cm2;
    mp_int num_values = m->num_values;
    mp_int size;
    mp_int i;
    char **str, **str2;
    struct svalue *svp, *svp2;

    /* --- Copy the hash part, if existent ---
     */

    if ( NULL != (hm = m->hash) )
    {
        struct map_chain **mcp, **mcp2;
        mp_int linksize;

        /* Allocate and initialize the hash structure */

        size = hm->mask + 1;
        hm2 = (struct hash_mapping *)
          xalloc(sizeof *hm - sizeof *mcp + sizeof *mcp * size);
        if (!hm2)
        {
            error("Out of memory.\n");
            /* NOTREACHED */
            return NULL;
        }

        hm2->mask = hm->mask;
        hm2->used = hm->used;
        hm2->condensed_deleted = hm->condensed_deleted;
#ifdef DEBUG
        hm2->next_dirty = NULL;
        hm2->deleted = NULL;
#endif
        hm2->ref = 0;

        /* Now copy the hash chains */

        mcp = hm->chains;
        mcp2 = hm2->chains;
        linksize = MAP_CHAIN_SIZE(num_values);
        do {
            struct map_chain *last = 0, *mc, *mc2;

            for(mc = *mcp++; mc; mc = mc->next) {
                mc2 = (struct map_chain *)xalloc(linksize);
                if (!mc2)
                {
                    error("Out of memory.\n");
                    /* NOTREACHED */
                    return NULL;
                }
                i = num_values;
                svp = &mc->key;
                svp2 = &mc2->key;
                do {
                    assign_svalue_no_free(svp2++, svp++);
                } while (--i >= 0);
                mc2->next = last;
                last = mc2;
            }
            *mcp2++ = last;
        } while (--size);
    }


    /* --- Copy the condensed part ---
     */
    
    cm = m->condensed;

    /* Allocate the new condensed structure with enough space
     * to hold all values, and let cm2 point to it.
     */
#ifdef MALLOC_smalloc
    size =
      (malloced_size(
        (char *)cm - cm->misc_size * (1 + num_values)
      ) - SMALLOC_OVERHEAD) * sizeof (p_int);
#else
    size = sizeof *cm2 +
      (cm->string_size  * (sizeof *svp/sizeof(char *)) + cm->misc_size) *
        (1 + num_values) -
      cm->string_size * (sizeof *svp/sizeof(char *) - 1);
#endif
    cm2 = (struct condensed_mapping *)
      ( (char *)xalloc(size) + cm->misc_size * (1 + num_values) );
    if (!cm2)
    {
        error("Out of memory.\n");
        /* NOTREACHED */
        return NULL;
    }


    /* Copy the base information */
    
    *cm2 = *cm;


    /* Copy the string key pointers, upping their refs */
    
    str = CM_STRING(cm);
    str2 = CM_STRING(cm2);
    for(i = cm->string_size; (i -= sizeof *str) >= 0; str++, str2++)
    {
        *str2 = *str;
        if ( !((p_int)*str & 1) )
            increment_string_ref(*str);
    }


    /* Copy the values associated with the string keys */
    
    svp = (struct svalue *)str;
    svp2 = (struct svalue *)str2;
    for(i = cm->string_size*num_values; (i -= sizeof *str) >= 0; ) {
        assign_svalue_no_free(svp2++, svp++);
    }


    /* Copy the misc keys and their associated values */
    
    svp = CM_MISC(cm);
    svp2 = CM_MISC(cm2);
    i = cm->misc_size*(num_values+1);
    while ( (i -= sizeof *svp) >= 0)
        assign_svalue_no_free(--svp2, --svp);


    /* --- Create the basis mapping structure and initialise it ---
     */

    m2 = (struct mapping *)xalloc(sizeof *m2);
    if ( NULL != (m2->hash = hm2) )
    {
        num_dirty_mappings++;
        last_dirty_mapping->hash->next_dirty = m2;
        last_dirty_mapping = m2;
    }
    m2->condensed = cm2;
    m2->num_values = num_values;
    m2->ref = 1;

    (m2->user = current_object->user)->mapping_total +=
      sizeof *m2 + sizeof(char*) + size + sizeof(char*);

    num_mappings++;

    /* That's it. */
    return m2;

} /* copy_mapping() */

/*-------------------------------------------------------------------------*/
struct mapping *
add_mapping (struct mapping *m1, struct mapping *m2)

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
 */

{
    struct mapping *m3;
      /* The result mapping */
    struct condensed_mapping *cm1, *cm2, *cm3;
      /* Condensed parts of m1, m2 and m3 */
    struct svalue *condensed_start, *condensed_end;
      /* Start and end of the memory block *cm3 is embedded in */
    mp_int string_size, misc_size;
      /* string- and misc- size of cm3 */
    mp_int num_values = m1->num_values;

    struct hash_mapping *hm;
    struct svalue *svp1, *svp2, *svp3;
    struct svalue *data1, *data2, *data3;
    char **str1, **str2, **str3;
    mp_int size, size1, size2;
    mp_int i;
    mp_int u_d;
    mp_int dirty;
      /* Number of condensed-deleted entries in cm3 */


    cm1 = m1->condensed;
    cm2 = m2->condensed;

    /* Special case: number of values per entry differs */

    if (m2->num_values != num_values)
    {
        if (!cm1->string_size && !cm1->misc_size && !m1->hash) {
            return copy_mapping(m2);
        } else {
            return copy_mapping(m1);
        }
    }


    /* Allocate the result mapping *m3 and initialise it.
     */
     
    string_size = cm1->string_size + cm2->string_size;
    misc_size = cm1->misc_size + cm2->misc_size;
    size = sizeof *cm3 +
      (string_size * (sizeof *svp3/sizeof *str1) + misc_size) *
        (1 + num_values) -
      string_size * (sizeof *svp3/sizeof *str1 - 1);
    if ( !(condensed_start  = (struct svalue *)xalloc(size)) )
        return NULL;

    condensed_end = (struct svalue *)((char *)condensed_start + size);
    cm3 = (struct condensed_mapping *)
      ( (char *)condensed_start + misc_size * (1 + num_values) );
    cm3->string_size = string_size;
    cm3->misc_size = misc_size;


    /* Merge the string-keyed entries.
     * Since the keys are sorted, a simple walk through both mappings
     * in parallel with proper selection does the trick.
     */
    dirty = 0;
    size1 = cm1->string_size;
    size2 = cm2->string_size;
    str1 = CM_STRING(cm1);
    data1 = (struct svalue *)( (char *)str1 + size1 );
    str2 = CM_STRING(cm2);
    data2 = (struct svalue *)( (char *)str2 + size2 );
    str3 = CM_STRING(cm3);
    data3 = (struct svalue *)( (char *)str3 + string_size );

    for(;size1 && size2; str3++)
    {
        if (*str1 < *str2)
        {
            /* Copy from m1 */
            
            *str3 = *str1++;
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(data3++, data1++);
            size1 -= sizeof *str1;
        }
        else
        {
            /* Copy from cm2 */
            
            if (*str1 == *str2)
            {
                /* Create a 'deleted' entry for the overwritten cm1-entry */
                
                if( (p_int)*str1 & 1 )
                {
                    *str3++ = *str1++;
                } else {
                    dirty++;
                    *str3++ = *str1++  - 1;
                }
                for (i = num_values; --i >= 0; )
                    (data3++)->type = T_INVALID;
                data1 += num_values;
                size1 -= sizeof *str1;
            }

            /* Now copy the data from cm2 */

            *str3 = *str2++;
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(data3++, data2++);
            size2 -= sizeof *str2;
        }

        /* Don't forget the refcount */

        if ( !((p_int)*str3 & 1) )
            increment_string_ref(*str3);
    }

    /* If there is data left uncopied in cm1, copy it now. */
    
    if (!size1) {
        str1 = str2;
        size1 = size2;
        data1 = data2;
    }
    for (;(size1 -= sizeof *str1) >= 0;)
    {
        if ( !( (p_int)(*str3 = *str1++) & 1) )
            increment_string_ref(*str3);
        str3++;
        for (i = num_values; --i >= 0; )
            assign_svalue_no_free(data3++, data1++);
    }


    /* Merge the misc-keyed entries.
     * Again, since the keys are sorted, a simple walk through both
     * mappings in parallel with proper selection does the trick.
     */
    size1 = cm1->misc_size;
    size2 = cm2->misc_size;
    svp1 = CM_MISC(cm1) - 1;
    data1 = (struct svalue *)( (char *)svp1 - size1 );
    svp2 = CM_MISC(cm2) - 1;
    data2 = (struct svalue *)( (char *)svp2 - size2 );
    svp3 = CM_MISC(cm3);
    data3 = (struct svalue *)( (char *)svp3 - misc_size );
    for(;size1 && size2; ) {
        if ( !(u_d = (svp1->u.number >> 1) - (svp2->u.number >> 1)) )
          if ( !(u_d = svp1->x.generic - svp2->x.generic) )
            if ( !(u_d = svp1->type - svp2->type) )
            {
                /* Create a 'deleted' entry for the overwritten cm1-entry */
                dirty += svp1->type != T_INVALID;
                svp1--;
                data1 -= num_values;
                size1 -= sizeof *svp1;
            }
        if (u_d < 0)
        {
            /* Copy from cm1 */
            assign_svalue_no_free(--svp3, svp1--);
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(--data3, data1--);
            size1 -= sizeof *svp1;
        }
        else
        {
            /* Copy from cm2 */
            assign_svalue_no_free(--svp3, svp2--);
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(--data3, data2--);
            size2 -= sizeof *svp2;
        }
    }

    /* If there is data left uncopied in cm1, copy it now. */
    
    if (!size1) {
        svp1 = svp2;
        size1 = size2;
        data1 = data2;
    }
    while ( (size1 -= sizeof *svp1) >= 0) {
        assign_svalue_no_free(--svp3, svp1--);
        for (i = num_values; --i >= 0; )
            assign_svalue_no_free(--data3, data1--);
    }
    while (data3 > condensed_start) {
        (--svp3)->type = T_INVALID;
        svp3->x.generic = MIN_PH_INT;
        svp3->u.number = MIN_P_INT;
        for (i = num_values; --i >= 0; )
            (--data3)->type = T_INVALID;
    }


    /* Increment dirty by the number of condensed_deleted elements in
     * cm1 and cm2.
     *
     * In parallel, set size1 to the total number of entries in the hash
     * parts of m1 and m2.
     *
     * If the final dirty is non-zero, size1 will be set to at least 1,
     * even if there are no entries in the hash parts.
     */
    size1 =
      (m1->hash ? dirty += m1->hash->condensed_deleted, m1->hash->used : 0)
    + (m2->hash ? dirty += m2->hash->condensed_deleted, m2->hash->used : 0) ;
    size1 += !size1 && dirty;

    /* Use size1 to allocate the result mapping and (that's the real reason
     * for the size1 trickery) the hash structures.
     * The also allocated condensed part will be replaced by the
     * condensed part created above.
     */
    if ( !(m3 = allocate_mapping(size1, num_values)) ) {
        xfree((char *)condensed_start);
        /* There's a value leak here, well, gcollect will take care of this */
        return NULL;
    }
    xfree( (char *)m3->condensed );
    m3->condensed = cm3;

    if (size1)
        m3->hash->condensed_deleted = dirty;

    (m3->user = current_object->user)->mapping_total += size - sizeof *cm3;
      /*  allocate_mapping has already accounted most of the total size
       *  sizeof *m3 + sizeof(char*) + size + sizeof(char*);
       */

    /* Now copy the two hash parts, using get_map_lvalue() to create
     * the new hashed entries
     *
     * First m1...
     */
    if ( NULL != (hm = m1->hash) )
    {
        struct map_chain **mcp;

        size = hm->mask + 1;
        mcp = hm->chains;
        do {
            struct map_chain *mc;

            for(mc = *mcp++; mc; mc = mc->next) {
                data1 = mc->data;
                data3 = get_map_lvalue(m3, &mc->key, MY_TRUE);
                if (data3 < condensed_start || data3 >= condensed_end) {
                    for (i = num_values; --i >= 0; )
                        assign_svalue(data3++, data1++);
                }
            }
        } while (--size);
    }

    /* ...now m2, potentially overwriting the entries from m1.
     */
    if ( NULL != (hm = m2->hash) )
    {
        struct map_chain **mcp;

        size = hm->mask + 1;
        mcp = hm->chains;
        do {
            struct map_chain *mc;

            for(mc = *mcp++; mc; mc = mc->next)
            {
                data1 = mc->data;
                data2 = get_map_lvalue(m3, &mc->key, MY_TRUE);
                for (i = num_values; --i >= 0; )
                    assign_svalue(data2++, data1++);
            }
        } while (--size);
    }


    /* That's it. */
    return m3;

} /* add_mapping() */

/*-------------------------------------------------------------------------*/
void
walk_mapping ( struct mapping *m
             , void (*func) (struct svalue *key, struct svalue *val, char *extra)
             , char *extra)

/* Generic function to perform a mapping walk. The function visits every
 * valid entry of <m> and for each entry calls <func>, passing the
 * current key, the current value(s) and the parameter <extra> to the
 * function.
 *
 * <func> may modify the value(s), but not the key.
 */

{
    char **str;
    struct svalue *svp, *data;
    mp_int size;
    mp_int num_values;
    struct hash_mapping *hm;

    walk_mapping_string_svalue.x.string_type = STRING_SHARED;
      /* Needed only the first time, but who cares */

    num_values = m->num_values;

    /* Walk the condensed string-key entries,
     * using walk_mapping_string_svalue to pass the key to <func>.
     */
    str = CM_STRING(m->condensed);
    size = m->condensed->string_size;
    data = (struct svalue *)((char *)str + size);
    while ( (size -= sizeof(char *)) >= 0)
    {
        if ( !( (p_int)(walk_mapping_string_svalue.u.string = *str++) & 1 ) )
            (*func)(&walk_mapping_string_svalue, data, extra);
        data += num_values;
    }

    /* Walk the condensed misc-key entries.
     */
    svp = CM_MISC(m->condensed);
    size = m->condensed->misc_size;
    data = (struct svalue *)((char *)svp - size);
    while ( (size -= sizeof(struct svalue)) >= 0)
    {
        data -= num_values;
        if ( (--svp)->type != T_INVALID )
            (*func)(svp, data, extra);
    }

    /* Walk the hashed entries
     */
    if ( NULL != (hm = m->hash) )
    {
        struct map_chain **mcp, *mc;

        mcp = hm->chains;
        size = hm->mask;
        do {
            if ( NULL != (mc = *mcp++) ) {
                do {
                    (*func)(&mc->key, mc->data, extra);
                } while ( NULL != (mc = mc->next) );
            }
        } while (--size >= 0);
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
 * separated into string and misc key entries and sorted, then
 * the sorted entries are merged with the condensed part. The sort
 * itself is done using Mergesort, with special treatment for those
 * portions that don't make up the current power of 2.
 *
 * The function is big, but functionally simple: there is only so
 * much complexity in a Mergesort.
 */

{
    struct mapping *m;  /* The current mapping to compact */

    malloc_privilege = MALLOC_SYSTEM;
      /* compact_mappings() is called in very low memory situations,
       * so it has to be allowed to use the system reserve.
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
        struct hash_mapping *hm;
          /* The hash part of m (guaranteed to exist!) */
        struct condensed_mapping *cm;
          /* The condensed part of m */
        int num_values;
          /* Number of values per entry */

        struct condensed_mapping *cm2;
          /* The new condensed part of the mapping */

        mp_int string_used, misc_used;
          /* Number of string/misc entries in the hash */

        mp_int runlength;
          /* Current Mergesort partition length */

        struct map_chain *string_hook1, *misc_hook1;
        struct map_chain *string_hook2, *misc_hook2;
          /* All string-/misc-keyed entries in two long chains.
           * Two chains each, since this is what Mergesort expects.
           */

        mp_int count1, count2;
        struct map_chain **mcpp, *mcp, *next;
        struct map_chain *last_string, *last_misc;
          /* Auxiliaries */

#ifdef DEBUG
        if (!m->user)
            fatal("No wizlist pointer for mapping\n");
#endif
        m->ref++; /* prevent freeing while using in case of recursive
                   * mappings referenced by a deleted value
                   */

        cm = m->condensed;
        hm = m->hash;

#ifdef DEBUG
        if (hm->ref) {
            fatal("compact_mappings(): remaining ref count %ld!\n", hm->ref);
        }
#endif /* DEBUG */

        /* Test if there are any hashed values to merge or
         * deleted values to compact
         */
        if (!hm->used && !hm->condensed_deleted)
        {
            if (!cm)
            {
                /* It's an empty mapping awaiting its deallocation */
                xfree((char *)m);
                empty_mapping_load -= 2;
            }
            else
            {
                m->hash = NULL;
                /* the ref count has been incremented above; on the other
                 * hand, the last real reference might have gone with the
                 * deleted keys. If that is the case, free_mapping() will
                 * deallocate it.
                 */
                free_mapping(m);
            }

            /* No hashed keys, condensed part is compact: just deallocate
             * the hash part.
             */
            m = hm->next_dirty;
            xfree( (char *)hm );
            continue;
        }

        num_values = m->num_values;


        /* --- Setup Mergesort ---
         *
         * Unravel all hash chains into four chains: two for string
         * keyed entries, two for misc keyed. These two chains
         * dangle from string_hook{1,2} resp. misc_hook{1,2}.
         *
         * The chains differ in length by at most 1 element. Within
         * the chains, the elements are pairwise sorted.
         *
         * In this loop, *_hook1 is always the next chain to add to,
         * and last_* is the first element of the next pair to add.
         */
        mcpp = hm->chains;
        count1 = hm->mask;
        string_hook1 = string_hook2 = NULL;
        misc_hook1 = misc_hook2 = NULL;
        misc_used = 0;
        last_string = last_misc = NULL;
        do {
            mcp = *mcpp++;
            while (mcp)
            {
                next = mcp->next;

                if (mcp->key.type != T_STRING)
                {
                    if (last_misc)
                    {
                        p_int d;

                        if ( !(d = (last_misc->key.u.number >> 1) -
                                   (mcp->key.u.number >> 1) ) )
                          if ( !(d = last_misc->key.x.generic -
                                     mcp->key.x.generic ) )
                            d = last_misc->key.type - mcp->key.type;
                        if (d > 0) {
                            last_misc->next = misc_hook1;
                            mcp->next = last_misc;
                            misc_hook1 = misc_hook2;
                            misc_hook2 = mcp;
                        } else {
                            mcp->next = misc_hook1;
                            last_misc->next = mcp;
                            misc_hook1 = misc_hook2;
                            misc_hook2 = last_misc;
                        }
                        misc_used += 2;
                        last_misc = NULL;
                    }
                    else
                    {
                        last_misc = mcp;
                    }
                }
                else
                {
                    if (last_string)
                    {
                        if (last_string->key.u.string > mcp->key.u.string)
                        {
                            last_string->next = string_hook1;
                            mcp->next = last_string;
                            string_hook1 = string_hook2;
                            string_hook2 = mcp;
                        }
                        else
                        {
                            mcp->next = string_hook1;
                            last_string->next = mcp;
                            string_hook1 = string_hook2;
                            string_hook2 = last_string;
                        }
                        last_string = 0;
                    }
                    else
                    {
                        last_string = mcp;
                    }
                }
                mcp = next;
            }
        } while (--count1 >= 0);

        /* Add the remaining odd element */
        if (last_string)
        {
            last_string->next = string_hook1;
            string_hook1 = last_string;
        }
        if (last_misc)
        {
            misc_used++;
            last_misc->next = misc_hook1;
            misc_hook1 = last_misc;
        }

        string_used = hm->used - misc_used;


        /* --- Mergesort the string-key entries ---
         *
         * Sort string_hook1 and string_hook2 into string_hook1.
         */
        for (runlength = 2; runlength < string_used; runlength <<= 1)
        {
            struct map_chain *out_hook1, *out_hook2, **out1, **out2;
              /* The output chains, which serve as input chains in
               * the next pass
               */

            count1 = string_used & (runlength-1);
            count2 = string_used & runlength;
            if (!count1)
            {
                out2 = &out_hook1;
                *out2 = string_hook2;
                while (--count2 >= 0)
                {
                    out2 = &(*out2)->next;
                }
                string_hook2 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            }
            else if (!count2)
            {
                out2 = &out_hook1;
                *out2 = string_hook1;
                do {
                    out2 = &(*out2)->next;
                } while (--count1);
                string_hook1 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            }
            else
            {
                out1 = &out_hook1;
                out2 = &out_hook2;
            }
            while (string_hook1)
            {
                /* Sort the next runlength elements onto out1 */
                while (1)
                {
                    if (string_hook2->key.u.string <
                        string_hook1->key.u.string)
                    {
                        *out1 = string_hook2;
                        out1 = &string_hook2->next;
                        string_hook2 = *out1;
                        if (!--count2)
                        {
                            *out1 = string_hook1;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count1);
                            string_hook1 = *out1;
                            break;
                        }
                    }
                    else
                    {
                        *out1 = string_hook1;
                        out1 = &string_hook1->next;
                        string_hook1 = *out1;
                        if (!--count1)
                        {
                            *out1 = string_hook2;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count2);
                            string_hook2 = *out1;
                            break;
                        }
                    }
                }

                /* Now switch the chains */
                {
                    struct map_chain **temp;

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
            string_hook1 = out_hook1;
            string_hook2 = out_hook2;
        }
        if (!string_hook1)
            string_hook1 = string_hook2;


        /* --- Mergesort the misc-key entries ---
         *
         * Sort misc_hook1 and misc_hook2 into misc_hook1.
         */
        for (runlength = 2; runlength < misc_used; runlength <<= 1)
        {
            struct map_chain *out_hook1, *out_hook2, **out1, **out2;
              /* The output chains, which serve as input chains in
               * the next pass
               */

            count1 = misc_used & (runlength-1);
            count2 = misc_used & runlength;
            if (!count1)
            {
                out2 = &out_hook1;
                *out2 = misc_hook2;
                while (--count2 >= 0) {
                    out2 = &(*out2)->next;
                }
                misc_hook2 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            }
            else if (!count2)
            {
                out2 = &out_hook1;
                *out2 = misc_hook1;
                do {
                    out2 = &(*out2)->next;
                } while (--count1);
                misc_hook1 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            }
            else
            {
                out1 = &out_hook1;
                out2 = &out_hook2;
            }
            while (misc_hook1)
            {
                /* Sort the next runlength elements onto out1 */
                while (1) {
                    p_int d;

                    if (!(d = (misc_hook2->key.u.number >> 1) -
                              (misc_hook1->key.u.number >> 1) ))
                      if (!(d = misc_hook2->key.x.generic -
                                misc_hook1->key.x.generic))
                          d = misc_hook2->key.type -
                              misc_hook1->key.type;
                    if (d < 0)
                    {
                        *out1 = misc_hook2;
                        out1 = &misc_hook2->next;
                        misc_hook2 = *out1;
                        if (!--count2)
                        {
                            *out1 = misc_hook1;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count1);
                            misc_hook1 = *out1;
                            break;
                        }
                    }
                    else
                    {
                        *out1 = misc_hook1;
                        out1 = &misc_hook1->next;
                        misc_hook1 = *out1;
                        if (!--count1)
                        {
                            *out1 = misc_hook2;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count2);
                            misc_hook2 = *out1;
                            break;
                        }
                    }
                }

                /* Now switch the chains */
                {
                    struct map_chain **temp;

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
            misc_hook1 = out_hook1;
            misc_hook2 = out_hook2;
        }
        if (!misc_hook1)
            misc_hook1 = misc_hook2;


        /* --- Merge the old condensed part with the sorted lists ---
         */
        {
            mp_int misc_deleted;
              /* Number deleted misc-keyed entries */
            mp_int string_total, misc_total;
              /* Total number of valid string-/misc-keyed entries */
            char *condensed_start;
              /* Begin of memory allocated for cm2 */
            char *cm1_end, *cm2_end;
              /* End of string-keyed value areas in cm resp. cm2 */

            char **str1, **str2;
            struct svalue *key1, *key2;
            struct svalue *data1, *data2;
              /* Auxiliaries */


            /* Count the number of deleted misc-keyed entries */

            misc_deleted = 0;
            if (hm->condensed_deleted)
            {
                struct svalue *svp;
                mp_int size;

                svp = CM_MISC(cm);
                size = cm->misc_size;
                while ( (size -= sizeof(struct svalue)) >= 0)
                {
                    if ( (--svp)->type == T_INVALID )
                        misc_deleted++;
                }
            }


            /* Compute the total number of entries */

            string_total = string_used + cm->string_size/sizeof(char *) -
                        (hm->condensed_deleted - misc_deleted);
            misc_total = misc_used + cm->misc_size/sizeof(struct svalue) -
                        misc_deleted;


            /* Allocate and initialise the new condensed structure */

            condensed_start = xalloc(sizeof *cm2 +
                (string_total+misc_total)*sizeof(struct svalue)*(num_values+1)-
                string_total * (sizeof(struct svalue)-sizeof(char *))
            );
            cm2 = (struct condensed_mapping *)
                   (condensed_start +
                    misc_total * (num_values+1) * sizeof(struct svalue) );
            cm2->string_size = string_total * sizeof(char*);
            cm2->misc_size = misc_total * sizeof(struct svalue);


            /* Merge the string-keyed entries from cm with the sorted
             * hash entries dangling from string_hook1 into cm2.
             */

            str1 = CM_STRING(cm);
            data1 = (struct svalue *)((char *)str1 + cm->string_size);
            str2 = CM_STRING(cm2);
            data2 = (struct svalue *)((char *)str2 + cm2->string_size);
            count1 = cm->string_size;

            /* For all leading invalid keys, free the associated
             * values (this is more a safety measure as the values should
             * be svalue-0 anyway).
             */
            while (count1 && (p_int)*str1 & 1)
            {
                int i;

                i = num_values;
                while (--i >= 0) {
                    free_svalue(data1++);
                }
                str1++;
                count1 -= sizeof(char *);
            }

            /* Do the actual merge */
            if (string_hook1 && count1)
            {
                while (1)
                {
                    if (string_hook1->key.u.string < *str1)
                    {
                        /* Take entry from string_hook1 */
                        
                        struct map_chain *temp;
                        struct svalue *data;
                        int i;

                        temp = string_hook1;
                        *str2++ = temp->key.u.string;
                        data = temp->data;
                        i = num_values;
                        while (--i >= 0)
                        {
                            *data2++ = *data++;
                        }
                        string_hook1 = temp->next;
                        xfree( (char *)temp );
                        if (!string_hook1)
                            break;
                    }
                    else
                    {
                        /* Take entry from old condensed part */
                        
                        int i;

                        *str2++ = *str1++;
                        i = num_values;
                        while (--i >= 0)
                        {
                            *data2++ = *data1++;
                        }
                        if ( !(count1 -= sizeof(char*)) )
                            break;

                        /* Skip eventual following invalid entries in cm */
                        if ((p_int)*str1 & 1)
                        {
                            do {
                                i = num_values;
                                while (--i >= 0) {
                                    free_svalue(data1++);
                                }
                                str1++;
                                if ( !(count1 -= sizeof(char*)) )
                                    break;
                            } while ((p_int)*str1 & 1);
                            if (!count1)
                                break;
                        }
                    }
                } /* while(1) */
            } /* if (string_hook1 && count1) */

            /* Copy any remaining entries from the old condensed part
             * or the string_hook1
             */
            if (count1)
            {
                /* Copy from the condensed part */
                
                while (1)
                {
                    int i;

                    *str2++ = *str1++;
                    i = num_values;
                    while (--i >= 0)
                    {
                        *data2++ = *data1++;
                    }
                    if ( !(count1 -= sizeof(char*)) )
                        break;

                    /* Skip eventual following invalid entries in cm */
                    if ((p_int)*str1 & 1)
                    {
                        do {
                            i = num_values;
                            while (--i >= 0) {
                                free_svalue(data1++);
                            }
                            str1++;
                            if ( !(count1 -= sizeof(char*)) )
                                break;
                        } while ((p_int)*str1 & 1);
                        if (!count1)
                            break;
                    }
                }
            }
            else
            {
                /* Copy from string_hook1 */

                while (string_hook1)
                {
                    struct map_chain *temp;
                    struct svalue *data;
                    int i;

                    temp = string_hook1;
                    *str2++ = temp->key.u.string;
                    data = temp->data;
                    i = num_values;
                    while (--i >= 0) {
                        *data2++ = *data++;
                    }
                    string_hook1 = temp->next;
                    xfree( (char *)temp );
                }
            }


            /* Remember the actual end of the areas used */
            
            cm1_end = (char *)data1;
            cm2_end = (char *)data2;


            /* Merge the misc-keyed entries from cm with the sorted
             * hash entries dangling from misc_hook1 into cm2.
             */

            key1 = CM_MISC(cm);
            data1 = (struct svalue *)((char *)key1 - cm->misc_size);
            key2 = CM_MISC(cm2);
            data2 = (struct svalue *)((char *)key2 - cm2->misc_size);
            count1 = cm->misc_size;

            /* For all leading invalid keys, free the associated
             * values (this is more a safety measure as the values should
             * be svalue-0 anyway).
             */
            while (count1 && key1[-1].type == T_INVALID)
            {
                int i;

                key1--;
                i = num_values;
                while (--i >= 0) {
                    free_svalue(--data1);
                }
                count1 -= sizeof(struct svalue);
            }

            /* Do the actual merge */
            if (misc_hook1 && count1)
            {
                while (1)
                {
                    p_int d;

                    if (!(d = (misc_hook1->key.u.number >> 1) -
                              (key1[-1].u.number >> 1) ))
                      if (!(d = misc_hook1->key.x.generic - key1[-1].x.generic))
                          d = misc_hook1->key.type - key1[-1].type;
                    if (d < 0)
                    {
                        /* Take entry from misc_hook1 */
                        
                        struct map_chain *temp;
                        struct svalue *data;
                        int i;

                        temp = misc_hook1;
                        *--key2 = temp->key;
                        data = temp->data + num_values;
                        i = num_values;
                        while (--i >= 0) {
                            *--data2 = *--data;
                        }
                        misc_hook1 = temp->next;
                        xfree( (char *)temp );
                        if (!misc_hook1)
                            break;
                    }
                    else
                    {
                        /* Take entry from the old condensed part */
                        
                        int i;

                        *--key2 = *--key1;
                        i = num_values;
                        while (--i >= 0) {
                            *--data2 = *--data1;
                        }
                        if (! (count1 -= sizeof(struct svalue)) )
                            break;

                        /* Skip eventual following invalid entries in cm */
                        if (key1[-1].type == T_INVALID)
                        {
                            do {
                                key1--;
                                i = num_values;
                                while (--i >= 0) {
                                    free_svalue(--data1);
                                }
                                if (! (count1 -= sizeof(struct svalue)) )
                                    break;
                            } while (key1[-1].type == T_INVALID);
                            if (!count1)
                                break;
                        }
                    }
                } /* while(1) */
            } /* if (misc_hook1 && count1) */

            /* Copy any remaining entries from the old condensed part
             * or the misc_hook1
             */
            if (count1)
            {
                /* Copy from the old condensed part */

                while (1)
                {
                    int i;

                    *--key2 = *--key1;
                    i = num_values;
                    while (--i >= 0) {
                        *--data2 = *--data1;
                    }
                    if (! (count1 -= sizeof(struct svalue)) )
                        break;

                    /* Skip eventual following invalid entries in cm */
                    if (key1[-1].type == T_INVALID)
                    {
                        do {
                            key1--;
                            i = num_values;
                            while (--i >= 0) {
                                free_svalue(--data1);
                            }
                            if (! (count1 -= sizeof(struct svalue)) )
                                break;
                        } while (key1[-1].type == T_INVALID);
                        if (!count1)
                            break;
                    }
                }
            }
            else
            {
                /* Copy from misc_hook1 */

                while (misc_hook1)
                {
                    struct map_chain *temp;
                    struct svalue *data;
                    int i;

                    temp = misc_hook1;
                    *--key2 = temp->key;
                    data = temp->data + num_values;
                    i = num_values;
                    while (--i >= 0) {
                        *--data2 = *--data;
                    }
                    misc_hook1 = temp->next;
                    xfree( (char *)temp );
                }
            }


            /* Adjust the accounting in the users wizlist entry */
            m->user->mapping_total +=
                (cm2_end - (char *)data2) -
                (cm1_end - (char *)data1);

            xfree( (char *)(data1) ); /* free old condensed mapping part */

        } /* --- End of Merge --- */

        m->condensed = cm2;
        m->hash = NULL;

        free_mapping(m);
          /* Undo the initial m->ref++; if there was a recursive
           * reference which is now gone, the mapping will be deallocated
           * now.
           */

        m = hm->next_dirty;

        xfree( (char *)hm );
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
    struct wiz_list *wl;
    mp_int total;

    total = default_wizlist_entry.mapping_total;
    for (wl = all_wiz; wl; wl = wl->next) {
        total += wl->mapping_total;
    }
    return total;
}

/*-------------------------------------------------------------------------*/

/* Structure used by set_mapping_user() to communicate with ..._filter()
 */
struct set_mapping_user_locals
{
    int num_values;        /* Number of values per key */
    struct object *owner;  /* Owner to set */
    struct svalue *hairy;
      /* Next free entry in the array of keys which need manual tweaking */
};


static void
set_mapping_user_filter (struct svalue *key, struct svalue *data, char *extra)

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
    struct object *owner;

    locals = (struct set_mapping_user_locals *)extra;
    owner = locals->owner;

    if (key->type == T_CLOSURE && !CLOSURE_MALLOCED(key->x.closure_type))
    {
        *locals->hairy++ = *key;
    }
    else
    {
        set_svalue_user(key, owner);
    }
    for (i = locals->num_values; --i >= 0;)
    {
        set_svalue_user(data++, owner);
    }
}

void
set_mapping_user (struct mapping *m, struct object *owner)

/* Set the <owner> as the user of mapping <m> and all its contained
 * keys and values, and update the wizlist entry for <owner>.
 *
 * As this function is called only for variables in newly compiled
 * objects, there is no need to guard against recursive
 * calls for this particular mapping.
 */

{
    struct condensed_mapping *cm;
    int num_values;
    mp_int total;
    struct wiz_list *user;
    struct set_mapping_user_locals locals;
    struct svalue *first_hairy;
    mp_int i;

    num_values = m->num_values;
    cm = m->condensed;

    /* Move the total size in the wizlist from the old owner
     * to the new one
     */
    total =
      sizeof *m + sizeof(char *) + sizeof *cm + sizeof(char *) +
        ( cm->string_size * (sizeof(struct svalue)/sizeof(char*)) +
          cm->misc_size) * (1 + num_values) -
        cm->string_size * (sizeof(struct svalue)/sizeof(char *) - 1);
    m->user->mapping_total -= total;
    user = owner->user;
    m->user = user;
    m->user->mapping_total += total;


    /* Walk the mapping to set all owners */
    
    locals.owner = owner;
    locals.num_values = num_values;
    locals.hairy = first_hairy = (struct svalue *)alloca(cm->misc_size);
    if (!first_hairy)
    {
        error("Stack overflow.\n");
        /* NOTREACHED */
        return;
    }
    walk_mapping(m, set_mapping_user_filter, (char *)&locals);

    /* All 'hairy' keys are changed by reassignment to the mapping
     */
    for (i = locals.hairy - first_hairy; --i >= 0; first_hairy++)
    {
        struct svalue new_key, *dest, *source;
        mp_int j;

        /* Create the new key by changing its owner */
        assign_svalue_no_free(&new_key, first_hairy);
        set_svalue_user(&new_key, owner);

        /* Create a new entry in the mapping for the new key */
        dest = get_map_lvalue(m, &new_key, MY_TRUE);
        free_svalue(&new_key);

        /* Move the values from the old entry to the new one, invalidating
         * the old ones by this.
         */
        source = get_map_lvalue(m, first_hairy, MY_FALSE);
        if (num_values)
            memcpy((char *)dest, (char *)source, num_values * sizeof *dest);
        for (j = num_values; --j >= 0; source++)
            source->type = T_INVALID;

        /* Remove the old entry */
        remove_mapping(m, first_hairy);
    }
} /* set_mapping_user() */

#ifdef MALLOC_smalloc

/*-------------------------------------------------------------------------*/
void
count_ref_in_mapping (struct mapping *m)

/* GC support: Count all references by the mapping <m>.
 * The GC will call this function only for compacted mappings.
 *
 * If the mapping contains keys referencing destructed objects/lambdas,
 * it is added to the list of stale mappings.
 */

{
    char **str;
    struct svalue *svp, *data;
    mp_int size;
    mp_int num_values;
    /* TODO: BOOL */ int any_destructed = MY_FALSE;

    num_values = m->num_values;


    /* Count references by condensed string keys and their data */
    
    str = CM_STRING(m->condensed);
    size = m->condensed->string_size;
    while ( (size -= sizeof(char *)) >= 0)
    {
        count_ref_from_string(*str++);
    }

    data = (struct svalue *)str;
    count_ref_in_vector(
      (struct svalue *)str,
      m->condensed->string_size / sizeof *str * num_values
    );


    /* Count references by condensed misc keys and their data.
     * Take special care of keys referencing destructed objects/lambdas.
     */

    svp = CM_MISC(m->condensed);
    size = m->condensed->misc_size;
    while ( (size -= sizeof(struct svalue)) >= 0)
    {
        --svp;
        if ( (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED)
         ||  (   svp->type == T_CLOSURE
              && ( CLOSURE_MALLOCED(svp->x.closure_type) ?
                  ( svp->x.closure_type != CLOSURE_UNBOUND_LAMBDA &&
                    svp->u.lambda->ob->flags & O_DESTRUCTED ) :
                  svp->u.ob->flags & O_DESTRUCTED ) ) )
        {
            /* This key is / is bound to a destructed object. The entry has to
             * be deleted (later).
             */

            if (svp->type == T_CLOSURE &&
                svp->x.closure_type == CLOSURE_BOUND_LAMBDA)
            {
                /* We don't want changing keys, even if they are still valid
                 * unbound closures
                 */
                struct lambda *l = svp->u.lambda;

                svp->x.closure_type = CLOSURE_LAMBDA;
                svp->u.lambda = l->function.lambda;
                if (!l->ref) {
                    l->function.lambda->ob = l->ob;
                    l->ref = -1;
                    l->ob = (struct object *)stale_misc_closures;
                    stale_misc_closures = l;
                } else {
                    l->function.lambda->ob = gc_obj_list_destructed;
                }
            }
            count_ref_in_vector(svp, 1);
            if (svp->type == T_CLOSURE) {
                /* *svp has been transformed into an efun closure bound
                 * to the master
                 */
                svp->u.ob->ref--;
            }
            svp->type = T_INVALID;
            if (!any_destructed)
            {
                any_destructed = MY_TRUE;
                /* Might be a small mapping. Don't malloc, it might get too
                 * much due to the global scope of garbage_collection.
                 * Since there was a previous
                 * compact_mappings(num_dirty_mappings) , the hash field is
                 * known to be NULL.
                 */
                m->hash = (struct hash_mapping *)stale_mappings;
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
            count_ref_in_vector(svp, 1);
        }
    }

    size = m->condensed->misc_size * num_values;
    count_ref_in_vector(
      (struct svalue *)((char *)svp - size),
      size / sizeof *svp
    );
} /* count_ref_in_mapping() */

/*-------------------------------------------------------------------------*/
void
clean_stale_mappings (void)

/* GC support: After count_ref_in_mapping(), the gc will free all
 * unreferenced destruct objects and lambdas. This may have changed
 * several keys in the stale_mappings to T_INVALID. Since the objective
 * is to recover memory, these mappings are now compacted.
 */

{
    struct mapping *m, *next;

    for (m = stale_mappings; m; m = next)
    {
        struct condensed_mapping *cm, *cm2;
        char *cm2_start;
        mp_int size;
        mp_int data_size;
          /* Size of the misc-keyed values of cm */
        mp_int deleted_size;
          /* Total size deleted from the misc-part of cm */
        mp_int preserved_size;
          /* Preserved size from the string-part of cm */
        mp_int i, num_values;
        struct svalue *svp, *svp2, *data, *data2;
        mp_int num_deleted = 0;
          /* Number of deleted misc-key entries */

        next = (struct mapping *)m->hash;
        m->hash = NULL;

        num_values = m->num_values;
        cm = m->condensed;

        /* Count the number of invalid misc keys */
        
        svp = CM_MISC(cm);
        i = size = cm->misc_size;
        while ( (i -= sizeof(struct svalue)) >= 0)
        {
            if ( (--svp)->type == T_INVALID)
                num_deleted++;
        }

        /* Compute the various sizes and update the wizlist total */
        
        data_size = size * num_values;
        deleted_size = num_deleted * sizeof(struct svalue) * (num_values + 1);
        preserved_size = sizeof(*cm2) +
          cm->string_size *
          (1 + (sizeof(struct svalue)/sizeof(char *)) * num_values);
        m->user->mapping_total -= deleted_size;

        /* Allocate the new condensed part and initialise it */
        
        cm2_start = xalloc(data_size + size - deleted_size + preserved_size);
        if (!cm2_start)
        {
            fatal("Out of memory.\n");
            /* NOTREACHED */
            continue;
        }
        cm2 = (struct condensed_mapping *)
          (cm2_start + data_size + size - deleted_size);
        memcpy((char *)cm2, (char *)cm, preserved_size);
        cm2->misc_size = size - num_deleted * sizeof(struct svalue);

        /* Copy the date for all valid misc-keys into the new
         * condensed part.
         */
        data = svp;
        svp2 = CM_MISC(cm2);
        data2 = (struct svalue *)((char *)svp2 - size) + num_deleted;
        svp = CM_MISC(cm);
        i = size;
        while ( (i -= sizeof(struct svalue)) >= 0)
        {
            if ( (--svp)->type == T_INVALID) {
                mp_int j;

                for (j = num_values; --j >= 0; ) {
                    free_svalue(--data);
                }
                continue;
            }
            *--svp2 = *svp;
            data -= num_values;
            data2 -= num_values;
            memcpy(data2, data, num_values * sizeof(struct svalue));
        }
        m->condensed = cm2;

        xfree((char *)cm - data_size - size); /* No longer needed */
        
        free_mapping(m); /* Undo the ref held by the stale-mapping list */
    }
} /* clean_stale_mappings() */

#endif /* MALLOC_smalloc */

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
struct vector *
m_indices (struct mapping *m)

/* Create a vector with all keys from mapping <m> and return it.
 *
 * The function is used by interpret.c for M_INDICES and by map_mapping().
 */

{
    struct vector *v;
    struct svalue *svp;
    mp_int size;

    check_map_for_destr(m);
    size = m->condensed->string_size / sizeof(char *) +
           m->condensed->misc_size   / sizeof(struct svalue) +
          (m->hash ? m->hash->used  - m->hash->condensed_deleted : 0);
    v = allocate_array(size); /* might cause error */
    svp = v->item;
    walk_mapping(m, m_indices_filter, (char *)&svp);
    return v;
}

/*-------------------------------------------------------------------------*/
static void
add_to_mapping_filter (struct svalue *key, struct svalue *data, char *extra)

/* Auxiliary function to add_to_mapping():
 * Add/overwrite (key:data) to mapping <extra>.
 */

{
    struct svalue *data2;
    int i;

    data2 = get_map_lvalue((struct mapping *)extra, key, MY_TRUE);
    for (i = ((struct mapping *)extra)->num_values; --i >= 0;)
    {
        if (data2->type != T_NUMBER)
            free_svalue(data2);
        assign_svalue_no_free(data2++, data++);
    }
}

/*-------------------------------------------------------------------------*/
void
add_to_mapping (struct mapping *m1, struct mapping *m2)

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
        struct condensed_mapping *cm1, *cm2;

        cm2 = m2->condensed;
        if (!cm2->string_size && !cm2->misc_size && !m2->hash)
        {
            m2->num_values = m1->num_values;
        }
        else
        {
            cm1 = m1->condensed;
            if (!cm1->string_size && !cm1->misc_size && !m1->hash)
            {
                m1->num_values = m2->num_values;
            }
            else
            {
                return;
            }
        }
    }
    walk_mapping(m2, add_to_mapping_filter, (char *)m1);
}

/*-------------------------------------------------------------------------*/
void
sub_from_mapping_filter ( struct svalue *key, struct svalue *data UNUSED
                        , char *extra)

/* Auxiliary to subtract_mapping(): Delete <key> from mapping <extra>.
 * Also called by interpret.c as part of F_SUB_EQ.
 */

{
#ifdef __MWERKS__
#    pragma unused(data)
#endif
    remove_mapping((struct mapping *)extra, key);
}

/*-------------------------------------------------------------------------*/
struct mapping *
subtract_mapping (struct mapping *minuend, struct mapping *subtrahend)

/* Create a copy of <minuend> minus all entries which are also in
 * <subtrahend>.
 *
 * Called by interpret.c as part of F_SUBTRACT.
 */

{
    /* this could be done faster, especially if there the mappings are
     * mainly condensed. On the other hand, the priority of fast mapping
     * subtraction is unknown.
     */
    minuend = copy_mapping(minuend);
    walk_mapping(subtrahend, sub_from_mapping_filter, (char *)minuend);
    return minuend;
}

/*-------------------------------------------------------------------------*/
static void
f_walk_mapping_filter (struct svalue *key, struct svalue *data, char *extra)

/* Auxiliary to efuns {walk,filter}_mapping(): callback for walk_mapping().
 *
 * <extra> is a pointer to a (struct svalue *) to an array of (numvalues+1)
 * svalues. The first of these gets to hold the <key>, the others are lvalues
 * to get the <data>.
 */

{
    struct svalue *svp;

    svp = *(struct svalue **)extra;
    assign_svalue_no_free(svp, key);
    (++svp)->u.lvalue = data;
    *(struct svalue **)extra = ++svp;
}

/*-------------------------------------------------------------------------*/
static void
f_walk_mapping_cleanup (struct svalue *arg)

/* Auxiliary to efuns {walk,filter}_walk_mapping(): Cleanup.
 *
 * This function is called during the stackcleanup after a mapping walk.
 * <arg> is the array of svalue allocated by walk_mapping_prologue().
 * See walk_mapping_prologue() for details.
 */

{
    struct svalue *svp;
    struct mapping *m;
    mp_int i;

    svp = arg + 1;
    m = svp[1].u.map;

    /* If the mapping had a hash part prior to the f_walk_mapping(),
     * it was protected by the prologue and we have to lift that
     * protection.
     */
    if (svp[1].x.generic)
    {
        struct hash_mapping *hm;
        int num_values;

        hm = m->hash;
        num_values = m->num_values;

        if (!--hm->ref)
        {
            /* Last ref gone: deallocated the pending deleted entries */

            struct map_chain *mc, *next;
            struct svalue *svp2;

            for (mc = hm->deleted; mc; mc = next)
            {
                mp_int j;

                svp2 = &mc->key;
                j = num_values;
                do {
                    free_svalue(svp2++);
                } while (--j >= 0);
                next = mc->next;
                xfree( (char *)mc );
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
    xfree((char *)arg);

} /* f_walk_mapping_cleanup() */

/*-------------------------------------------------------------------------*/
static struct svalue *
walk_mapping_prologue (struct mapping *m, struct svalue *sp)

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
 *              [1] { u.number: number of mapping entries }
 *              [2] { u.map: <m>, x.generic: <m> has hash part }
 *    result -> [3] { key1 }
 *              [4] { lvalue } -> values of key1
 *              [5] { key2 }
 *              [6] { lvalue } -> values of key2
 *                etc
 *
 * Storing the array as error handler allows a simple cleanup in course
 * of the free_svalue()s done by f_walk_mapping().
 *
 * If <m> at call time has a hash part, it is protected by incrementing
 * hash->ref.
 */

{
    struct hash_mapping *hm;
    struct svalue *pointers;
    struct svalue *write_pointer, *read_pointer;
    mp_int i;

    i = m->condensed->string_size/sizeof(char *) +
        m->condensed->misc_size/sizeof(struct svalue);
    if ( NULL != (hm = m->hash) ) {
        i += hm->used - hm->condensed_deleted;
        if (!m->num_values) {
            hm = 0;
        } else if (!hm->ref++) {
            hm->deleted = NULL;
        }
    }
    pointers = (struct svalue *)xalloc( (i * 2 + 3) * sizeof(struct svalue) );
    pointers[0].type = T_ERROR_HANDLER;
    pointers[0].u.error_handler = f_walk_mapping_cleanup;
    pointers[1].u.number = i;
    pointers[2].u.map = m;
    pointers[2].x.generic = hm != 0;
    (++sp)->type = T_LVALUE;
    sp->u.lvalue = pointers;
    read_pointer = write_pointer = pointers + 3;
    walk_mapping(m, f_walk_mapping_filter, (char*)&write_pointer);
    return read_pointer;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_walk_mapping (struct svalue *sp, int num_arg)

/* EFUN walk_mapping()
 *
 *   void walk_mapping(mapping m, string func, object ob, mixed extra,...)
 *   void walk_mapping(mapping m, closure cl, mixed extra,...)
 *
 * Calls ob->func(key, value1, ..., valueN, extra,...) resp. applies
 * the closure to every entry in the mapping. The keys are passed
 * by value, the values are passed by reference and can be
 * changed in the function.
 * Any number of extra arguments is accepted and passed.
 */

{
    struct svalue *arg;           /* Begin of the args on the stack */
    struct svalue *extra = NULL;  /* Begin of the 'extra' args on the stack */
    int extra_num;                /* Number of 'extra' args */
    struct mapping *m;            /* Mapping to walk */
    struct object *ob;            /* Object to call */
    char *func;                   /* Function to call */
    int num_values;               /* Number of values per entry */
    struct svalue *read_pointer;  /* Prepared mapping values */
    mp_int i;

    /* Locate the arguments on the stack and extract them */
    arg = sp - num_arg + 1;
    inter_sp = sp;
    if (arg[0].type != T_MAPPING)
        bad_xefun_vararg(1, sp);
    if (arg[1].type == T_CLOSURE)
    {
        ob = NULL;
        func = (char *)&arg[1];
        extra_num = num_arg - 2;
        extra = &arg[2];
    }
    else if (arg[1].type != T_STRING)
    {
        bad_xefun_vararg(2, sp);
    }
    else
    {
        if (num_arg >= 3 /* && arg[1].type == T_STRING */)
        {
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type != T_STRING
             ||      !(ob = get_object(arg[2].u.string)) )
                bad_xefun_vararg(3, sp);
            extra_num = num_arg - 3;
            extra = &arg[3];
        }
        else
        {
            ob = current_object;
            extra_num = 0;
        }
        func = arg[1].u.string;
    }
    m = arg[0].u.map;


    /* Preparations */
    
    check_map_for_destr(m);
    assign_eval_cost();

    read_pointer = walk_mapping_prologue(m, sp);
    i = read_pointer[-2].u.number;
    sp++; /* walk_mapping_prologue() pushed on value */

    num_values = m->num_values;

    /* For every key:values pair in read_pointer[], set up
     * the stack for a call to the walk function.
     */
    while (--i >= 0)
    {
        int j;
        struct svalue *sp2, *data;

        /* Push the key */
        assign_svalue_no_free( (sp2 = sp+1), read_pointer++ );

        /* Push the values as lvalues */
        for (j = num_values, data = (read_pointer++)->u.lvalue; --j >= 0; )
        {
             (++sp2)->type = T_LVALUE;
             sp2->u.lvalue = data++;
        }

        /* Push the 'extra' arguments */
        inter_sp = sp2;
        push_svalue_block(extra_num, extra);

        /* Call the function */
        if (ob)
        {
            if (ob->flags & O_DESTRUCTED)
                error("Object used by walk_mapping destructed");
            apply( func, ob, 1 + num_values + extra_num);
        } else {
            call_lambda( (struct svalue *)func, 1 + num_values + extra_num);
            free_svalue(inter_sp--);
        }
    }

    /* This frees the whole array allocated by the prologue */
    free_svalue(sp);

    /* Free the arguments */
    i = num_arg;
    do
        free_svalue(--sp);
    while (--i > 0);

    return sp-1;

} /* f_walk_mapping() */

/*-------------------------------------------------------------------------*/
struct svalue *
filter_mapping (struct svalue *sp, int num_arg)

/* EFUN filter_mapping()
 *
 *   mapping filter_mapping(mapping, string func, object ob, ...)
 *   mapping filter_mapping(mapping, closure cl, ...)
 *
 * ob->func() is called resp. cl applied to every element in the
 * mapping, with first argument the first data item of the
 * element, and then the extra args that were given to
 * filter_mapping. If the function returns true, the element is
 * added to the result mapping. ob can also be a file_name of an
 * object. If the second arg is a string and the third is not an
 * object, this_object() will be used as default.
 */

{
    struct svalue *arg;           /* Start of arguments on the stack */
    struct mapping *m;            /* Mapping to filter */
    char *func;                   /* Function to call */
    struct object *ob;            /* Object to call */
    struct svalue *extra = NULL;  /* Start of 'extra' args on the stack */
    int extra_num;                /* Number of 'extra' args */
    int num_values;               /* Width of the mapping */
    struct svalue *read_pointer;  /* Prepared mapping values */
    struct svalue *v;
    int i, j;

    /* Locate the arguments on the stack and extract them */
    arg = sp - num_arg + 1;
    inter_sp = sp;
    if (arg[0].type != T_MAPPING)
        bad_xefun_vararg(1, sp);
    if (arg[1].type == T_CLOSURE)
    {
        ob = NULL;
        func = (char *)&arg[1];
        extra = &arg[2];
        extra_num = num_arg - 2;
    }
    else if (arg[1].type != T_STRING)
    {
        bad_xefun_vararg(2, sp);
    }
    else
    {
        if (num_arg < 3)
        {
            ob = current_object;
            /* let the compiler mourn: we need no extra. */
            extra_num = 0;
        }
        else
        {
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type != T_STRING
             ||      !(inter_sp = sp, ob = get_object(arg[2].u.string)) )
                bad_xefun_vararg(3, sp);
            extra = &arg[3];
            extra_num = num_arg - 3;
        }
        func = arg[1].u.string;
    }
    m = arg[0].u.map;


    /* Preparations */
    
    check_map_for_destr(m);
    assign_eval_cost();

    num_values = m->num_values;
    read_pointer = walk_mapping_prologue(m, sp);
    m = allocate_mapping(read_pointer[-2].u.number , num_values);
    if (!m)
    {
        inter_sp = sp + 1;
        error("Out of memory\n");
    }
    (sp += 2)->type = T_MAPPING;
    sp->u.map = m;

      /* M is kept referenced on the stack so that
       * in case of an error it is properly dereferenced.
       * At a normal termination however, m will not be dereferenced.
       */

    /* For every (key:values) in read_pointer[], set up the stack for
     * a call to the filter function. If it returns true, assign the
     * pair to the new mapping.
     */
    for (i = read_pointer[-2].u.number; --i >= 0; read_pointer += 2)
    {
        struct svalue *data;

        if (ob)
        {
            if (ob->flags & O_DESTRUCTED)
                break;

            /* Push the key */
            assign_svalue_no_free((inter_sp = sp + 1), read_pointer);

            /* Push the 'extra' args */
            push_svalue_block( extra_num, extra);

            /* Call the function */
            v = apply( func, ob, 1 + extra_num);

            /* Did the filter return TRUE? */
            if (!v || (v->type == T_NUMBER && !v->u.number) )
                continue;
        }
        else
        {
            /* Push the key */
            assign_svalue_no_free((inter_sp = sp + 1), read_pointer);

            /* Push the 'extra' args */
            push_svalue_block( extra_num, extra);

            /* Call the function */
            call_lambda( (struct svalue *)func, 1 + extra_num);

            /* Did the filter return TRUE? */
            v = inter_sp--;
            if (v->type == T_NUMBER) {
                if (!v->u.number)
                    continue;
            }
            else
            {
                free_svalue(v);
            }
        }

        /* If we come here, the filter function returned 'true'.
         * There for assign the pair to the new mapping.
         */
        v = get_map_lvalue(m, read_pointer, MY_TRUE);
        for (j = num_values, data = read_pointer[1].u.lvalue; --j >= 0; )
        {
            assign_svalue_no_free(v++, data++);
        }
    }

    /* Cleanup the read_pointer block and the arguments except
     * for the initial mapping svalue and the reference to m.
     */
    i = num_arg;
    do
        free_svalue(--sp);
    while (--i >= 0);

    /* Return the result mapping in place of the argument mapping.
     */
    sp->type = T_MAPPING;
    sp->u.map = m;

    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
map_mapping (struct svalue *sp, int num_arg)

/* EFUN map_mapping()
 *
 *   mapping map_mapping(mapping m, string func, object ob, ...)
 *   mapping map_mapping(mapping m, closure cl, ...)
 *
 * ob->func() is called resp. cl applied to every element in the
 * mapping, with the key of the element as first argument, and
 * then the extra args that were given to map_mapping.
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
    struct svalue *arg;           /* Begin of arguments on the stack */
    struct mapping *m;            /* Mapping to map */
    char *func;                   /* Function to cal */
    struct object *ob;            /* Object to call */
    struct svalue *extra = NULL;  /* Begin of 'extra' args on the stack */
    int extra_num;                /* Number of 'extra' args */
    int num_values;               /* Width of the mapping */
    struct vector *vec;           /* Indices of m */
    int i;
    struct svalue *key;

    /* Locate and extract arguments */
    arg = sp - num_arg + 1;
    inter_sp = sp;
    if (arg[0].type != T_MAPPING)
        bad_xefun_vararg(1, sp);
    if (arg[1].type == T_CLOSURE)
    {
        ob = NULL;
        func = (char *)(arg + 1);
        extra = &arg[2];
        extra_num = num_arg - 2;
    }
    else if (arg[1].type != T_STRING)
    {
        bad_xefun_vararg(2, sp);
    }
    else
    {
        if (num_arg < 3)
        {
            ob = current_object;
            /* let the compiler mourn: we need no extra. */
            extra_num = 0;
        }
        else
        {
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type != T_STRING
             ||      !(inter_sp = sp, ob = get_object(arg[2].u.string)) )
                bad_xefun_vararg(3, sp);
            extra = &arg[3];
            extra_num = num_arg - 3;
        }
        func = arg[1].u.string;
    }
    m = arg[0].u.map;


    /* Preparations */
    
    check_map_for_destr(m);
    assign_eval_cost();

    num_values = m->num_values;

    /* Get the indices of m */
    vec = m_indices(m); /* might cause error */
    (++sp)->type = T_POINTER;
    sp->u.vec = vec;

    m = allocate_mapping((i = VEC_SIZE(vec)), 1);
    if (!m)
    {
        inter_sp = sp;
        error("Out of memory\n");
    }
    (++sp)->type = T_MAPPING;
    sp->u.map = m;

      /* Both vec and m are kept referenced on the stack so that
       * in case of an error they are properly dereferenced.
       * At a normal termination however, m will not be dereferenced
       * but vec will.
       */
       
    key = vec->item;
    for (; --i >= 0; key++) {
        struct svalue *v;

        assign_svalue_no_free((inter_sp = sp + 1), key);
        push_svalue_block( extra_num, extra);
        v = get_map_lvalue(m, key, MY_TRUE);
        if (ob) {
            struct svalue *data;

            if (ob->flags & O_DESTRUCTED)
                error("Object used by map_mapping destructed");
            data = apply( func, ob, 1 + extra_num);
            if (data) {
                transfer_svalue_no_free(v, data);
                data->type = T_INVALID;
            }
        } else {
            call_lambda( (struct svalue *)func, 1 + extra_num);
            transfer_svalue_no_free(v, inter_sp--);
        }
    }

    /* Cleanup the read_pointer block and the arguments except
     * for the initial mapping svalue and the reference to m.
     */
    i = num_arg;
    do
        free_svalue(--sp);
    while (--i >= 0);

    /* Return the result mapping in place of the argument mapping.
     */
    sp->type = T_MAPPING;
    sp->u.map = m;
    return sp;
}

/***************************************************************************/

