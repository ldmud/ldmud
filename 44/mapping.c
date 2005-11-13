/*---------------------------------------------------------------------------
 * Mapping handling functions.
 *
 *---------------------------------------------------------------------------
 * The structure of an array ("vector") is defined in datatypes.h as this:
 *
 *   struct vector {
 *       p_int size;               (ifndef MALLOC_smalloc)
 *       p_int ref;
 *       p_int extra_ref;          (ifdef DEBUG)
 *       struct wiz_list *user;
 *       struct svalue item[1...];
 *   };
 *
 * .size is the number of elements in the vector. If smalloc is used,
 * this number can be deduced from the memory block size, the entry
 * itself is therefore omitted.
 * TODO: document me.
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

struct mapping *stale_mappings; /* for garbage_collection */
  /* TODO: ??? */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void remove_empty_mappings(void);

/*-------------------------------------------------------------------------*/
struct mapping *
allocate_mapping (mp_int size, mp_int num_values)

/* Allocate a mapping with <num_values> values per key, and setup the
 * structures for (initially) <size> entries
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
}

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
    struct svalue *svp;            /* Last+1 non-string key in <cm> */
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


    /* Dereference all non-string keys and their values */

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
}

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
}

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
}

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
}

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
                /* found it */
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
            /* not found */
        }
        /* don't search if there are no string keys */
        break;
      }

      /* ----- Non-String Indexing ----- */

      default:
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
          p_int index_u = map_index->u.number, u_d;

        keyend = (char *)CM_MISC(cm);
        size = cm->misc_size;
        keystart = keyend - size;
        offset = size | size >> 1;
        offset |= offset >> 2;
        offset |= offset >> 4;
        if (offset & ~0xff) {
            offset |= offset >> 8;
            offset |= offset >> 16;
        }
        offset = (offset+1) >> 1;
        key = keyend - offset;
        while ( (offset >>= 1) >= (p_int)(sizeof svp)/2) {
            if ( !(u_d = (((struct svalue *)key)->u.number >> 1) -
                         (index_u >> 1)) ) {
              if ( !(u_d = ((struct svalue *)key)->x.generic - index_x) )
                if ( !(u_d = ((struct svalue *)key)->type - index_type) ) {
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
        /* not found */
        break;
      }
    }  /* switch(map_index->type) */


    if ( !(hm = m->hash) ) {
        struct map_chain *mc;
        mp_int i;

        if (!need_lvalue)
            return &const0;
        hm = (struct hash_mapping *)xalloc(sizeof *hm);
        if (!hm)
            return 0;
        hm->mask = hm->condensed_deleted = 0;
        hm->ref = 0;
        hm->used = 1;
        last_dirty_mapping->hash->next_dirty = m;
        last_dirty_mapping = m;
#ifdef DEBUG
        hm->next_dirty = 0;
        hm->deleted = 0;
#endif
        num_dirty_mappings++;
        extra_jobs_to_do = MY_TRUE;
        m->hash = hm;
        mc = (struct map_chain *)xalloc(MAP_CHAIN_SIZE(num_values));
        hm->chains[0] = mc;
        if (!mc)
            return 0;
        mc->next = 0;
        assign_svalue_no_free(&mc->key, map_index);
        svp = mc->data;
        for (i = num_values; --i >= 0; svp++) {
            svp->type = T_NUMBER;
            svp->u.number = 0;
        }
        return mc->data;
    } else {
        struct map_chain *mc;
        p_int index_type = *SVALUE_FULLTYPE(map_index);
        p_int index_u = map_index->u.number;
        mp_int i;

        i = index_u ^ index_type;
        i = i ^ i >> 16;
        i = i ^ i >> 8;
        i &= hm->mask;
        for (mc = hm->chains[i];mc; mc = mc->next){
            if (mc->key.u.number != index_u ||
                *SVALUE_FULLTYPE(&mc->key) != index_type)
                continue;
            return mc->data;
        }
        if (!need_lvalue)
            return &const0;
        if (hm->used & ~hm->mask<<1) {
            /* Avoid average map_chains lengths above 2 by reallocating the
             * array of pointers
             */
            struct hash_mapping *hm2;
            mp_int mask, j;
            struct map_chain **mcp, **mcp2, *next;

            hm2 = hm;
            size = (hm->mask << 1) + 2;
            mask = size - 1;
            hm = (struct hash_mapping *)
              xalloc(sizeof *hm - sizeof *mcp + sizeof *mcp * size);
            if (!hm)
                return 0;
            *hm = *hm2;
            hm->mask = mask;
            mcp = hm->chains;
            do *mcp++ = 0; while (--size);
            mcp = hm->chains;
            mcp2 = hm2->chains;
            for (j = hm2->mask + 1; --j >= 0; ) {
                for (mc = *mcp2++; mc; mc = next) {
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
            xfree((char *)hm2);
            i = map_index->u.number ^ *SVALUE_FULLTYPE(map_index);
            i = i ^ i >> 16;
            i = i ^ i >> 8;
            i &= mask;
        }
        mc = (struct map_chain *)xalloc(MAP_CHAIN_SIZE(num_values));
        if (!mc)
            return 0;
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
}

void check_map_for_destr(m)
    struct mapping *m;
{
    struct condensed_mapping *cm;
    struct hash_mapping *hm;
    struct svalue *svp;
    mp_int i, j;
    int num_values;

    num_values = m->num_values;
    cm = m->condensed;
    for (svp = CM_MISC(cm),i = cm->misc_size; (i -= sizeof *svp) >= 0; ) {
        --svp;
        if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED) {
            struct svalue *data = NULL;

            if ( 0 != (j = num_values) ) {
                data = (struct svalue *)((char *)svp - i -
                  num_values * ((char *)CM_MISC(cm) - (char *)svp));
                do {
                    free_svalue(data);
                    data->type = T_NUMBER;
                    data->u.number = 0;
                    data++;
                } while (--j);
            }
            while ( &svp[1] < CM_MISC(cm) &&
              svp[1].u.number == svp[0].u.number &&
              svp[1].x.generic == svp[0].x.generic)
            {
                *SVALUE_FULLTYPE(&svp[0]) =  *SVALUE_FULLTYPE(&svp[1]);
                svp++;
                i += sizeof *svp;
                for (j = num_values; --j >= 0; data++)
                    data[-num_values] = data[0];
                    data->type = T_NUMBER;
                    data->u.number = 0;
            }
            free_object_svalue(svp);
            svp[0].type = T_INVALID;
            if ( !(hm = m->hash) ) {
                hm = (struct hash_mapping *)xalloc(sizeof *hm);
                m->hash = hm;
                hm->mask = hm->used = hm->condensed_deleted = hm->ref = 0;
                hm->chains[0] = 0;
                last_dirty_mapping->hash->next_dirty = m;
                last_dirty_mapping = m;
#ifdef DEBUG
                hm->next_dirty = 0;
                hm->deleted = 0;
#endif
                num_dirty_mappings++;
                extra_jobs_to_do = MY_TRUE;
            }
            hm->condensed_deleted++;
        }
    }
    for (i = cm->misc_size * num_values; (i -= sizeof *svp) >= 0; ) {
        svp--;
        if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED) {
            assign_svalue(svp, &const0);
        }
    }
    svp = (struct svalue *)( (char *)CM_STRING(cm) + cm->string_size );
    for (i = cm->string_size * num_values; (i -= sizeof(char *)) >= 0; svp++) {
        if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED) {
            assign_svalue(svp, &const0);
        }
    }
    if ( NULL != (hm = m->hash) ) {
        struct map_chain **mcp, **mcp2, *mc;

        for (mcp = hm->chains, i = hm->mask + 1; --i >= 0;) {
            for (mcp2 = mcp++; NULL != (mc = *mcp2); ) {
                if (mc->key.type == T_OBJECT &&
                  mc->key.u.ob->flags & O_DESTRUCTED)
                {
                    svp = &mc->key;
                    *mcp2 = mc->next;

                    if (hm->ref) {
                        mc->next = hm->deleted;
                        hm->deleted = mc;
                    } else {
                        j = num_values;
                        do {
                            free_svalue(svp++);
                        } while (--j >= 0);
                        xfree( (char *)mc );
                    }
                    hm->used--;
                    continue;
                }
                for (svp = mc->data, j = num_values; --j >= 0; ) {
                    if (svp->type == T_OBJECT &&
                      svp->u.ob->flags & O_DESTRUCTED)
                    {
                        assign_svalue(svp, &const0);
                    }
                }
                mcp2 = &mc->next;
            }
        }
    }
}

void remove_mapping(m, map_index)
    struct mapping *m;
    struct svalue *map_index;
{
    mp_int size;
    struct condensed_mapping *cm = m->condensed;
    struct hash_mapping *hm;
    int num_values = m->num_values;
    struct svalue *svp;

    switch (map_index->type) {
      case T_STRING:
      {
        char *str;
        char *key; /* means a char **, but pointer arithmetic wants char * */
        char *keystart, *keyend;

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
            offset = size-1;
            offset |= offset >> 1;
            offset |= offset >> 2;
            offset |= offset >> 4;
            if (offset & ~0xff) {
                offset |= offset >> 8;
                offset |= offset >> 16;
            }
            if ( (offset = (offset+1) >> 1) >= (p_int)sizeof str)
                do {
                    if (key + offset >= keyend) continue;
                    if ( str >= *(char **)(key+offset) ) key += offset;
                } while ( (offset >>= 1) >= (p_int)sizeof str);
            if ( str == *(char **)key ) {
                /* found it */

                int i;

                free_string(str);
                (*(char **)key)++;
                svp = (struct svalue *)
                  (keyend + (key - keystart ) *
                    ( num_values * (sizeof(struct svalue)/sizeof str) ));
                for (i = num_values; --i >= 0 ;svp++) {
                    free_svalue(svp);
                    svp->type = T_NUMBER;
                    svp->u.number = 0;
                }
                if ( !(hm = m->hash) ) {
                    hm = (struct hash_mapping *)xalloc(sizeof *hm);
                    m->hash = hm;
                    hm->mask = hm->used = hm->condensed_deleted = hm->ref = 0;
                    hm->chains[0] = 0;
                    last_dirty_mapping->hash->next_dirty = m;
                    last_dirty_mapping = m;
#ifdef DEBUG
                    hm->next_dirty = 0;
                    hm->deleted = 0;
#endif
                    num_dirty_mappings++;
                    extra_jobs_to_do = MY_TRUE;
                }
                hm->condensed_deleted++;
                return;
            }
            /* not found */
        }
        /* don't search if there are no string keys */
        break;
      }
      default:
        map_index->x.generic = map_index->u.number << 1;
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

        keyend = (char *)CM_MISC(cm);
        size = cm->misc_size;
        keystart = keyend - size;
        offset = size | size >> 1;
        offset |= offset >> 2;
        offset |= offset >> 4;
        if (offset & ~0xff) {
            offset |= offset >> 8;
            offset |= offset >> 16;
        }
        offset = (offset+1) >> 1;
        key = keyend - offset;
        while ( (offset >>= 1) >= (p_int)(sizeof svp)/2) {
            if ( !(u_d = (((struct svalue *)key)->u.number >> 1) -
                         (index_u >> 1)) ) {
              if ( !(u_d = ((struct svalue *)key)->x.generic - index_x) )
                if ( !(u_d = ((struct svalue *)key)->type - index_type) ) {
                    /* found */

                    int i;

                    free_svalue( (struct svalue *)key );
                    svp = (struct svalue *)
                      (keystart - ( num_values * (keyend - key) ) );
                    for (i = num_values; --i >= 0 ;svp++) {
                        free_svalue(svp);
                        svp->type = T_NUMBER;
                        svp->u.number = 0;
                    }

                    /* it's harmless to read misc/string_size as an svalue */
                    while ( ((struct svalue *)key+1)->u.number == index_u &&
                            ((struct svalue *)key+1)->x.generic == index_x &&
                            key + sizeof(struct svalue) < keyend)
                    {
                        struct svalue *svp2;

                        *((struct svalue *)key) = *((struct svalue *)key+1);
                        key += sizeof(struct svalue);
                        svp2 = svp - num_values;
                        for (i = num_values; --i >= 0 ;svp++, svp2++) {
                            *svp2 = *svp;
                            svp->type = T_NUMBER;
                            svp->u.number = 0;
                        }
                    }
                    ((struct svalue *)key)->type = T_INVALID;
                    if ( !(hm = m->hash) ) {
                        hm = (struct hash_mapping *)xalloc(sizeof *hm);
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
        /* not found */
        break;
      }
    }
    if ( NULL != (hm = m->hash) ) {
        struct map_chain **mcp, *mc;
        p_int index_type = *SVALUE_FULLTYPE(map_index);
        p_int index_u = map_index->u.number;
        mp_int i;

        i = index_u ^ index_type;
        i = i ^ i >> 16;
        i = i ^ i >> 8;
        i &= hm->mask;
        for(mcp = &hm->chains[i]; NULL != (mc = *mcp); ) {
            if (mc->key.u.number == index_u &&
                *SVALUE_FULLTYPE(&mc->key) == index_type)
            {
                int j;

                *mcp = mc->next;
                if (hm->ref) {
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
}

struct mapping *copy_mapping(m)
    struct mapping *m;
{
    struct mapping *m2;
    struct hash_mapping *hm, *hm2 = 0;
    struct condensed_mapping *cm, *cm2;
    mp_int num_values = m->num_values;
    mp_int size;
    mp_int i;
    char **str, **str2;
    struct svalue *svp, *svp2;

    if ( NULL != (hm = m->hash) ) {
        struct map_chain **mcp, **mcp2;
        mp_int linksize;

        size = hm->mask + 1;
        hm2 = (struct hash_mapping *)
          xalloc(sizeof *hm - sizeof *mcp + sizeof *mcp * size);
        hm2->mask = hm->mask;
        hm2->used = hm->used;
        hm2->condensed_deleted = hm->condensed_deleted;
#ifdef DEBUG
        hm2->next_dirty = 0;
        hm->deleted = 0;
#endif
        hm2->ref = 0;
        num_dirty_mappings++;
        mcp = hm->chains;
        mcp2 = hm2->chains;
        linksize = MAP_CHAIN_SIZE(num_values);
        do {
            struct map_chain *last = 0, *mc, *mc2;

            for(mc = *mcp++; mc; mc = mc->next) {
                mc2 = (struct map_chain *)xalloc(linksize);
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
    cm = m->condensed;
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
    *cm2 = *cm;
    str = CM_STRING(cm);
    str2 = CM_STRING(cm2);
    for(i = cm->string_size; (i -= sizeof *str) >= 0; str++, str2++) {
        *str2 = *str;
        if ( !((p_int)*str & 1) )
            increment_string_ref(*str);
    }
    svp = (struct svalue *)str;
    svp2 = (struct svalue *)str2;
    for(i = cm->string_size*num_values; (i -= sizeof *str) >= 0; ) {
        assign_svalue_no_free(svp2++, svp++);
    }
    svp = CM_MISC(cm);
    svp2 = CM_MISC(cm2);
    i = cm->misc_size*(num_values+1);
    while ( (i -= sizeof *svp) >= 0)
        assign_svalue_no_free(--svp2, --svp);
    m2 = (struct mapping *)xalloc(sizeof *m2);
    if ( NULL != (m2->hash = hm2) ) {
        last_dirty_mapping->hash->next_dirty = m2;
        last_dirty_mapping = m2;
    }
    m2->condensed = cm2;
    m2->num_values = num_values;
    m2->ref = 1;
    (m2->user = current_object->user)->mapping_total +=
      sizeof *m2 + sizeof(char*) + size + sizeof(char*);
    num_mappings++;
    return m2;
}

struct mapping *add_mapping(m1, m2)
    struct mapping *m1, *m2;
{
    struct mapping *m3;
    struct hash_mapping *hm;
    struct condensed_mapping *cm1, *cm2, *cm3;
    struct svalue *condensed_start, *condensed_end;
    mp_int num_values = m1->num_values;
    mp_int size, size1, size2;
    mp_int string_size, misc_size;
    mp_int i;
    char **str1, **str2, **str3;
    struct svalue *svp1, *svp2, *svp3;
    mp_int u_d;
    struct svalue *data1, *data2, *data3;
    mp_int dirty;

    cm1 = m1->condensed;
    cm2 = m2->condensed;
    if (m2->num_values != num_values) {
        if (!cm1->string_size && !cm1->misc_size && !m1->hash) {
            return copy_mapping(m2);
        } else {
            return copy_mapping(m1);
        }
    }
    string_size = cm1->string_size + cm2->string_size;
    misc_size = cm1->misc_size + cm2->misc_size;
    size = sizeof *cm3 +
      (string_size * (sizeof *svp3/sizeof *str1) + misc_size) *
        (1 + num_values) -
      string_size * (sizeof *svp3/sizeof *str1 - 1);
    if ( !(condensed_start  = (struct svalue *)xalloc(size)) )
        return 0;
    condensed_end = (struct svalue *)((char *)condensed_start + size);
    cm3 = (struct condensed_mapping *)
      ( (char *)condensed_start + misc_size * (1 + num_values) );
    cm3->string_size = string_size;
    cm3->misc_size = misc_size;
    dirty = 0;
    size1 = cm1->string_size;
    size2 = cm2->string_size;
    str1 = CM_STRING(cm1);
    data1 = (struct svalue *)( (char *)str1 + size1 );
    str2 = CM_STRING(cm2);
    data2 = (struct svalue *)( (char *)str2 + size2 );
    str3 = CM_STRING(cm3);
    data3 = (struct svalue *)( (char *)str3 + string_size );
    for(;size1 && size2; str3++) {
        if (*str1 < *str2) {
            *str3 = *str1++;
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(data3++, data1++);
            size1 -= sizeof *str1;
        } else {
            if (*str1 == *str2) {
                if( (p_int)*str1 & 1 ) {
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
            *str3 = *str2++;
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(data3++, data2++);
            size2 -= sizeof *str2;
        }
        if ( !((p_int)*str3 & 1) )
            increment_string_ref(*str3);
    }
    if (!size1) {
        str1 = str2;
        size1 = size2;
        data1 = data2;
    }
    for (;(size1 -= sizeof *str1) >= 0;) {
        if ( !( (p_int)(*str3 = *str1++) & 1) )
            increment_string_ref(*str3);
        str3++;
        for (i = num_values; --i >= 0; )
            assign_svalue_no_free(data3++, data1++);
    }
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
            if ( !(u_d = svp1->type - svp2->type) ) {
                dirty += svp1->type != T_INVALID;
                svp1--;
                data1 -= num_values;
                size1 -= sizeof *svp1;
            }
        if (u_d < 0) {
            assign_svalue_no_free(--svp3, svp1--);
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(--data3, data1--);
            size1 -= sizeof *svp1;
        } else {
            assign_svalue_no_free(--svp3, svp2--);
            for (i = num_values; --i >= 0; )
                assign_svalue_no_free(--data3, data2--);
            size2 -= sizeof *svp2;
        }
    }
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
    size1 =
      (m1->hash ? dirty += m1->hash->condensed_deleted, m1->hash->used : 0) +
      (m2->hash ? dirty += m2->hash->condensed_deleted, m2->hash->used : 0) ;
    size1 += !size1 && dirty;
    if ( !(m3 = allocate_mapping(size1, num_values)) ) {
        xfree((char *)condensed_start);
        /* There's a value leak here, well, gcollect will take care of this */
        return 0;
    }
    xfree( (char *)m3->condensed );
    m3->condensed = cm3;
    if (size1)
        m3->hash->condensed_deleted = dirty;
    (m3->user = current_object->user)->mapping_total += size - sizeof *cm3;
      /*  allocate_mapping has already accounted most of the total size
       *  sizeof *m3 + sizeof(char*) + size + sizeof(char*);
       */

    if ( NULL != (hm = m1->hash) ) {
        struct map_chain **mcp;

        size = hm->mask + 1;
        mcp = hm->chains;
        do {
            struct map_chain *mc;

            for(mc = *mcp++; mc; mc = mc->next) {
                data1 = mc->data;
                data3 = get_map_lvalue(m3, &mc->key, 1);
                if (data3 < condensed_start || data3 >= condensed_end) {
                    for (i = num_values; --i >= 0; )
                        assign_svalue(data3++, data1++);
                }
            }
        } while (--size);
    }
    if ( NULL != (hm = m2->hash) ) {
        struct map_chain **mcp;

        size = hm->mask + 1;
        mcp = hm->chains;
        do {
            struct map_chain *mc;

            for(mc = *mcp++; mc; mc = mc->next) {
                data1 = mc->data;
                data2 = get_map_lvalue(m3, &mc->key, 1);
                for (i = num_values; --i >= 0; )
                    assign_svalue(data2++, data1++);
            }
        } while (--size);
    }
    return m3;
}

struct svalue walk_mapping_string_svalue = { T_STRING };

void walk_mapping(m, func, extra)
    struct mapping *m;
    void (*func) PROT((struct svalue *, struct svalue *, char *));
    char *extra;
{
    char **str;
    struct svalue *svp, *data;
    mp_int size;
    mp_int num_values;
    struct hash_mapping *hm;

    num_values = m->num_values;
    str = CM_STRING(m->condensed);
    size = m->condensed->string_size;
    data = (struct svalue *)((char *)str + size);
    while ( (size -= sizeof(char *)) >= 0) {
        if ( !( (p_int)(walk_mapping_string_svalue.u.string = *str++) & 1 ) )
            (*func)(&walk_mapping_string_svalue, data, extra);
        data += num_values;
    }
    svp = CM_MISC(m->condensed);
    size = m->condensed->misc_size;
    data = (struct svalue *)((char *)svp - size);
    while ( (size -= sizeof(struct svalue)) >= 0) {
        data -= num_values;
        if ( (--svp)->type != T_INVALID )
            (*func)(svp, data, extra);
    }
    if ( NULL != (hm = m->hash) ) {
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
}

void f_walk_mapping_filter(key, data, extra)
    struct svalue *key, *data;
    char *extra;
{
    struct svalue *svp;

    svp = *(struct svalue **)extra;
    assign_svalue_no_free(svp, key);
    (++svp)->u.lvalue = data;
    *(struct svalue **)extra = ++svp;
}

void f_walk_mapping_cleanup(arg)
    struct svalue *arg;
{
    struct svalue *svp;
    struct mapping *m;
    mp_int i;

    svp = arg + 1;
    m = svp[1].u.map;
    if (svp[1].x.generic) {
        struct hash_mapping *hm;
        int num_values;

        hm = m->hash;
        num_values = m->num_values;
        if (!--hm->ref) {
            struct map_chain *mc, *next;
            struct svalue *svp2;

            for (mc = hm->deleted; mc; mc = next) {
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
    i = svp->u.number;
    if (i) do {
        svp += 2;
        free_svalue(svp);
    } while (--i > 0);
    xfree((char *)arg);
}

struct svalue *walk_mapping_prologue(m, sp)
    struct mapping *m;
    struct svalue *sp;
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
            hm->deleted = 0;
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

struct svalue *f_walk_mapping(sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    struct svalue *arg, *extra = NULL;
    int extra_num;
    struct mapping *m;
    struct object *ob;
    char *func;
    int num_values;
    struct svalue *read_pointer;
    mp_int i;

    arg = sp - num_arg + 1;
    inter_sp = sp;
    if (arg[0].type != T_MAPPING)
        bad_xefun_vararg(1, sp);
    if (arg[1].type == T_CLOSURE) {
        ob = 0;
        func = (char *)&arg[1];
        extra_num = num_arg - 2;
        extra = &arg[2];
    } else if (arg[1].type != T_STRING) {
        bad_xefun_vararg(2, sp);
    } else {
        if (num_arg >= 3 /* && arg[1].type == T_STRING */) {
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type != T_STRING ||
                     !(ob = get_object(arg[2].u.string)) )
                bad_xefun_vararg(3, sp);
            extra_num = num_arg - 3;
            extra = &arg[3];
        } else {
            ob = current_object;
            extra_num = 0;
        }
        func = arg[1].u.string;
    }
    m = arg[0].u.map;
    check_map_for_destr(m);
    assign_eval_cost();

    read_pointer = walk_mapping_prologue(m, sp);
    i = read_pointer[-2].u.number;
    sp++;
    num_values = m->num_values;
    while (--i >= 0) {
        int j;
        struct svalue *sp2, *data;

        assign_svalue_no_free( (sp2 = sp+1), read_pointer++ );
        for (j = num_values, data = (read_pointer++)->u.lvalue; --j >= 0; ) {
             (++sp2)->type = T_LVALUE;
             sp2->u.lvalue = data++;
        }
        inter_sp = sp2;
        push_svalue_block(extra_num, extra);
        if (ob) {
            if (ob->flags & O_DESTRUCTED)
                error("Object used by walk_mapping destructed");
            apply( func, ob, 1 + num_values + extra_num);
        } else {
            call_lambda( (struct svalue *)func, 1 + num_values + extra_num);
            free_svalue(inter_sp--);
        }
    }
    free_svalue(sp);
    i = num_arg;
    do
        free_svalue(--sp);
    while (--i > 0);
    return sp-1;
}

struct vector *m_indices(m)
    struct mapping *m;
{
    struct vector *v;
    struct svalue *svp;
    mp_int size;

    size = m->condensed->string_size / sizeof(char *) +
           m->condensed->misc_size   / sizeof(struct svalue) +
          (m->hash ? m->hash->used  - m->hash->condensed_deleted : 0);
    v = allocate_array(size); /* might cause error */
    svp = v->item;
    walk_mapping(m, m_indices_filter, (char *)&svp);
    return v;
}

static void add_to_mapping_filter(key, data, extra)
    struct svalue *key, *data;
    char *extra;
{
    struct svalue *data2;
    int i;

    data2 = get_map_lvalue((struct mapping *)extra, key, 1);
    for (i = ((struct mapping *)extra)->num_values; --i >= 0;) {
        if (data2->type != T_NUMBER)
            free_svalue(data2);
        assign_svalue_no_free(data2++, data++);
    }
}

void sub_from_mapping_filter(key, data, extra)
    struct svalue *key;
    struct svalue *data UNUSED;
    char *extra;
{
    remove_mapping((struct mapping *)extra, key);
}

void add_to_mapping(m1, m2)
    struct mapping *m1, *m2;
{
    if (m2->num_values != m1->num_values) {
        struct condensed_mapping *cm1, *cm2;

        cm2 = m2->condensed;
        if (!cm2->string_size && !cm2->misc_size && !m2->hash) {
            m2->num_values = m1->num_values;
        } else {
            cm1 = m1->condensed;
            if (!cm1->string_size && !cm1->misc_size && !m1->hash) {
                m1->num_values = m2->num_values;
            } else {
                return;
            }
        }
    }
    walk_mapping(m2, add_to_mapping_filter, (char *)m1);
}

struct mapping *subtract_mapping(minuend, subtrahend)
    struct mapping *minuend, *subtrahend;
{
    /* this could be done faster, especially if there the mappings are
     * mainly condensed. On the other hand, the priority of fast mapping
     * subtraction is unknown.
     */
    minuend = copy_mapping(minuend);
    walk_mapping(subtrahend, sub_from_mapping_filter, (char *)minuend);
    return minuend;
}

/* leave a filtered mapping on the stack */
struct svalue *filter_mapping (sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    struct svalue *arg;
    struct mapping *m;
    char *func;
    struct object *ob;
    struct svalue *extra = NULL;
    int num_values;
    struct svalue *v;
    int extra_num;
    int i, j;
    struct svalue *read_pointer;

    arg = sp - num_arg + 1;
    inter_sp = sp;
    if (arg[0].type != T_MAPPING)
        bad_xefun_vararg(1, sp);
    if (arg[1].type == T_CLOSURE) {
        ob = 0;
        func = (char *)&arg[1];
        extra = &arg[2];
        extra_num = num_arg - 2;
    } else if (arg[1].type != T_STRING) {
        bad_xefun_vararg(2, sp);
    } else {
        if (num_arg < 3) {
            ob = current_object;
            /* let the compiler mourn: we need no extra. */
            extra_num = 0;
        } else {
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type != T_STRING ||
                     !(inter_sp = sp, ob = get_object(arg[2].u.string)) )
                bad_xefun_vararg(3, sp);
            extra = &arg[3];
            extra_num = num_arg - 3;
        }
        func = arg[1].u.string;
    }
    m = arg[0].u.map;
    check_map_for_destr(m);
    assign_eval_cost();

    num_values = m->num_values;
    read_pointer = walk_mapping_prologue(m, sp);
    m = allocate_mapping(read_pointer[-2].u.number , num_values);
    if (!m) {
        inter_sp = sp + 1;
        error("Out of memory\n");
    }
    (sp += 2)->type = T_MAPPING;
    sp->u.map = m;
    for (i = read_pointer[-2].u.number; --i >= 0; read_pointer += 2) {
        struct svalue *data;

        if (ob) {
            if (ob->flags & O_DESTRUCTED)
                break;
            assign_svalue_no_free((inter_sp = sp + 1), read_pointer);
            push_svalue_block( extra_num, extra);
            v = apply( func, ob, 1 + extra_num);
            if (!v || (v->type == T_NUMBER && !v->u.number) )
                continue;
        } else {
            assign_svalue_no_free((inter_sp = sp + 1), read_pointer);
            push_svalue_block( extra_num, extra);
            call_lambda( (struct svalue *)func, 1 + extra_num);
            v = inter_sp--;
            if (v->type == T_NUMBER) {
                if (!v->u.number)
                    continue;
            } else {
                free_svalue(v);
            }
        }
        v = get_map_lvalue(m, read_pointer, 1);
        for (j = num_values, data = read_pointer[1].u.lvalue; --j >= 0; ) {
            assign_svalue_no_free(v++, data++);
        }
    }
    i = num_arg;
    do
        free_svalue(--sp);
    while (--i >= 0);
    sp->type = T_MAPPING;
    sp->u.map = m;
    return sp;
}

/* leave result mapping on the stack */
struct svalue *map_mapping (sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    struct svalue *arg;
    struct mapping *m;
    char *func;
    struct object *ob;
    struct svalue *extra = NULL;
    struct vector *vec;
    int num_values;
    int extra_num;
    int i;
    struct svalue *key;

    arg = sp - num_arg + 1;
    inter_sp = sp;
    if (arg[0].type != T_MAPPING)
        bad_xefun_vararg(1, sp);
    if (arg[1].type == T_CLOSURE) {
        ob = 0;
        func = (char *)(arg + 1);
        extra = &arg[2];
        extra_num = num_arg - 2;
    } else if (arg[1].type != T_STRING) {
        bad_xefun_vararg(2, sp);
    } else {
        if (num_arg < 3) {
            ob = current_object;
            /* let the compiler mourn: we need no extra. */
            extra_num = 0;
        } else {
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type != T_STRING ||
                     !(inter_sp = sp, ob = get_object(arg[2].u.string)) )
                bad_xefun_vararg(3, sp);
            extra = &arg[3];
            extra_num = num_arg - 3;
        }
        func = arg[1].u.string;
    }
    m = arg[0].u.map;
    check_map_for_destr(m);
    assign_eval_cost();

    num_values = m->num_values;
    vec = m_indices(m); /* might cause error */
    (++sp)->type = T_POINTER;
    sp->u.vec = vec;
    m = allocate_mapping((i = VEC_SIZE(vec)), 1);
    if (!m) {
        inter_sp = sp;
        error("Out of memory\n");
    }
    (++sp)->type = T_MAPPING;
    sp->u.map = m;
    key = vec->item;
    for (; --i >= 0; key++) {
        struct svalue *v;

        assign_svalue_no_free((inter_sp = sp + 1), key);
        push_svalue_block( extra_num, extra);
        v = get_map_lvalue(m, key, 1);
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
    i = num_arg;
    do
        free_svalue(--sp);
    while (--i >= 0);
    sp->type = T_MAPPING;
    sp->u.map = m;
    return sp;
}

/* Used mergesort to sort hashed string and misc keys, respectively.
 * Portions that won't make up the current power of two are processed
 * first, to save tests.
 * When all previously hashed keys are sorted, they are merged with the
 * already condensed part.
 */
void compact_mappings(num)
    mp_int num;
{
    struct mapping *m;

    malloc_privilege = MALLOC_SYSTEM;
    if (last_indexing_protector.type == T_PROTECTOR_MAPPING) {
        /* There is a slight chance that free_protector_mapping causes
         * remove_empty_mappings(), and thus changes num_dirty_mappings.
         */
        free_protector_mapping(last_indexing_protector.u.map);
        last_indexing_protector.type = T_NUMBER;
    }
    if (num >= num_dirty_mappings) {
        num = num_dirty_mappings;
        last_dirty_mapping = &dirty_mapping_head;
    } else {
        extra_jobs_to_do = MY_TRUE;
    }
    num_dirty_mappings -= num;
    m = dirty_mapping_head_hash.next_dirty;
    while (--num >= 0) {
        mp_int count1, count2;
        struct hash_mapping *hm;
        struct condensed_mapping *cm, *cm2;
        int num_values;
        /* hooks to hang the chains on  :-) */
        struct map_chain *string_hook1, *string_hook2,
                         *misc_hook1,   *misc_hook2;
        mp_int string_used, misc_used, runlength;
        struct map_chain **mcpp, *mcp, *next;
        struct map_chain *last_string, *last_misc;

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
#if 1
            fatal("compact_mappings(): remaining ref count %ld!\n", hm->ref);
#else
            struct svalue *svp;
            int i;

            printf("compact_mappings(): remaining ref count %ld!\n", hm->ref);
#ifdef TRACE_CODE
            {
                last_instructions(TOTAL_TRACE_LENGTH, 1, 0);
            }
#endif
            printf("compact_mappings(): remaining ref count! %ld\n", hm->ref);
            if (hm->ref > 0) {
                for (mcp = hm->deleted; mcp; mcp = next) {
                    next = mcp->next;
                    svp = &mcp->key;
                    i = num_values;
                    do {
                        free_svalue(svp++);
                    } while (--i >= 0);
                    xfree( (char *)mcp );
                }
            }
#endif
        }
#endif /* DEBUG */
        if (!hm->used && !hm->condensed_deleted) {
            if (!m->condensed) {
                xfree((char *)m);
                empty_mapping_load -= 2;
            } else {
                m->hash = 0;
                /* the ref count has been incremented above; on the other
                 * hand, the last real reference might have gone with the
                 * deleted keys.
                 */
                free_mapping(m);
            }
            m = hm->next_dirty;
            xfree( (char *)hm );
            continue;
        }
        num_values = m->num_values;
        mcpp = hm->chains;
        count1 = hm->mask;
        string_hook1 = string_hook2 = 0;
        misc_hook1 = misc_hook2 = 0;
        misc_used = 0;
        last_string = last_misc = 0;
        do {
            mcp = *mcpp++;
            while (mcp) {
                next = mcp->next;

                if (mcp->key.type != T_STRING) {
                    if (last_misc) {
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
                        last_misc = 0;
                    } else {
                        last_misc = mcp;
                    }
                } else {
                    if (last_string) {
                        if (last_string->key.u.string > mcp->key.u.string) {
                            last_string->next = string_hook1;
                            mcp->next = last_string;
                            string_hook1 = string_hook2;
                            string_hook2 = mcp;
                        } else {
                            mcp->next = string_hook1;
                            last_string->next = mcp;
                            string_hook1 = string_hook2;
                            string_hook2 = last_string;
                        }
                        last_string = 0;
                    } else {
                        last_string = mcp;
                    }
                }
                mcp = next;
            }
        } while (--count1 >= 0);
        if (last_string) {
            last_string->next = string_hook1;
            string_hook1 = last_string;
        }
        if (last_misc) {
            misc_used++;
            last_misc->next = misc_hook1;
            misc_hook1 = last_misc;
        }
        string_used = hm->used - misc_used;
        for (runlength = 2; runlength < string_used; runlength <<= 1) {
            struct map_chain *out_hook1, *out_hook2, **out1, **out2;

            count1 = string_used & (runlength-1);
            count2 = string_used & runlength;
            if (!count1) {
                out2 = &out_hook1;
                *out2 = string_hook2;
                while (--count2 >= 0) {
                    out2 = &(*out2)->next;
                }
                string_hook2 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            } else if (!count2) {
                out2 = &out_hook1;
                *out2 = string_hook1;
                do {
                    out2 = &(*out2)->next;
                } while (--count1);
                string_hook1 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            } else {
                out1 = &out_hook1;
                out2 = &out_hook2;
            }
            while (string_hook1) {
                while (1) {
                    if (string_hook2->key.u.string <
                        string_hook1->key.u.string)
                    {
                        *out1 = string_hook2;
                        out1 = &string_hook2->next;
                        string_hook2 = *out1;
                        if (!--count2) {
                            *out1 = string_hook1;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count1);
                            string_hook1 = *out1;
                            break;
                        }
                    } else {
                        *out1 = string_hook1;
                        out1 = &string_hook1->next;
                        string_hook1 = *out1;
                        if (!--count1) {
                            *out1 = string_hook2;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count2);
                            string_hook2 = *out1;
                            break;
                        }
                    }
                }
                {
                    struct map_chain **temp;

                    temp = out1;
                    out1 = out2;
                    out2 = temp;
                }
                count1 = count2 = runlength;
            }
            *out1 = 0;
            *out2 = 0;
            string_hook1 = out_hook1;
            string_hook2 = out_hook2;
        }
        if (!string_hook1)
            string_hook1 = string_hook2;
        for (runlength = 2; runlength < misc_used; runlength <<= 1) {
            struct map_chain *out_hook1, *out_hook2, **out1, **out2;

            count1 = misc_used & (runlength-1);
            count2 = misc_used & runlength;
            if (!count1) {
                out2 = &out_hook1;
                *out2 = misc_hook2;
                while (--count2 >= 0) {
                    out2 = &(*out2)->next;
                }
                misc_hook2 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            } else if (!count2) {
                out2 = &out_hook1;
                *out2 = misc_hook1;
                do {
                    out2 = &(*out2)->next;
                } while (--count1);
                misc_hook1 = *out2;
                count1 = count2 = runlength;
                out1 = &out_hook2;
            } else {
                out1 = &out_hook1;
                out2 = &out_hook2;
            }
            while (misc_hook1) {
                while (1) {
                    p_int d;

                    if (!(d = (misc_hook2->key.u.number >> 1) -
                              (misc_hook1->key.u.number >> 1) ))
                      if (!(d = misc_hook2->key.x.generic -
                                misc_hook1->key.x.generic))
                          d = misc_hook2->key.type -
                              misc_hook1->key.type;
                    if (d < 0) {
                        *out1 = misc_hook2;
                        out1 = &misc_hook2->next;
                        misc_hook2 = *out1;
                        if (!--count2) {
                            *out1 = misc_hook1;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count1);
                            misc_hook1 = *out1;
                            break;
                        }
                    } else {
                        *out1 = misc_hook1;
                        out1 = &misc_hook1->next;
                        misc_hook1 = *out1;
                        if (!--count1) {
                            *out1 = misc_hook2;
                            do {
                                out1 = &(*out1)->next;
                            } while (--count2);
                            misc_hook2 = *out1;
                            break;
                        }
                    }
                }
                {
                    struct map_chain **temp;

                    temp = out1;
                    out1 = out2;
                    out2 = temp;
                }
                count1 = count2 = runlength;
            }
            *out1 = 0;
            *out2 = 0;
            misc_hook1 = out_hook1;
            misc_hook2 = out_hook2;
        }
        if (!misc_hook1)
            misc_hook1 = misc_hook2;
        {
            /* merge old condensed part with sorted lists */
            mp_int misc_deleted;
            mp_int string_total, misc_total;
            char *condensed_start;
            char **str1, **str2;
            struct svalue *key1, *key2;
            struct svalue *data1, *data2;
            char *cm1_end, *cm2_end;

            misc_deleted = 0;
            if (hm->condensed_deleted) {
                struct svalue *svp;
                mp_int size;

                svp = CM_MISC(cm);
                size = cm->misc_size;
                while ( (size -= sizeof(struct svalue)) >= 0) {
                    if ( (--svp)->type == T_INVALID )
                        misc_deleted++;
                }
            }
            string_total = string_used + cm->string_size/sizeof(char *) -
                        (hm->condensed_deleted - misc_deleted);
            misc_total = misc_used + cm->misc_size/sizeof(struct svalue) -
                        misc_deleted;
            condensed_start = xalloc(sizeof *cm2 +
                (string_total+misc_total)*sizeof(struct svalue)*(num_values+1)-
                string_total * (sizeof(struct svalue)-sizeof(char *))
            );
            cm2 = (struct condensed_mapping *)
                   (condensed_start +
                    misc_total * (num_values+1) * sizeof(struct svalue) );
            cm2->string_size = string_total * sizeof(char*);
            cm2->misc_size = misc_total * sizeof(struct svalue);
            str1 = CM_STRING(cm);
            data1 = (struct svalue *)((char *)str1 + cm->string_size);
            str2 = CM_STRING(cm2);
            data2 = (struct svalue *)((char *)str2 + cm2->string_size);
            count1 = cm->string_size;
            while (count1 && (p_int)*str1 & 1) {
                int i;

                i = num_values;
                while (--i >= 0) {
                    free_svalue(data1++);
                }
                str1++;
                count1 -= sizeof(char *);
            }
            if (string_hook1 && count1) {
                while (1) {
                    if (string_hook1->key.u.string < *str1) {
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
                        if (!string_hook1)
                            break;
                    } else {
                        int i;

                        *str2++ = *str1++;
                        i = num_values;
                        while (--i >= 0) {
                            *data2++ = *data1++;
                        }
                        if ( !(count1 -= sizeof(char*)) )
                            break;
                        if ((p_int)*str1 & 1) {
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
            }
            if (count1) {
                while (1) {
                    int i;

                    *str2++ = *str1++;
                    i = num_values;
                    while (--i >= 0) {
                        *data2++ = *data1++;
                    }
                    if ( !(count1 -= sizeof(char*)) )
                        break;
                    if ((p_int)*str1 & 1) {
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
            } else {
                while (string_hook1) {
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
            cm1_end = (char *)data1;
            cm2_end = (char *)data2;
            key1 = CM_MISC(cm);
            data1 = (struct svalue *)((char *)key1 - cm->misc_size);
            key2 = CM_MISC(cm2);
            data2 = (struct svalue *)((char *)key2 - cm2->misc_size);
            count1 = cm->misc_size;
            while (count1 && key1[-1].type == T_INVALID) {
                int i;

                key1--;
                i = num_values;
                while (--i >= 0) {
                    free_svalue(--data1);
                }
                count1 -= sizeof(struct svalue);
            }
            if (misc_hook1 && count1) {
                while (1) {
                    p_int d;

                    if (!(d = (misc_hook1->key.u.number >> 1) -
                              (key1[-1].u.number >> 1) ))
                      if (!(d = misc_hook1->key.x.generic - key1[-1].x.generic))
                          d = misc_hook1->key.type - key1[-1].type;
                    if (d < 0) {
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
                    } else {
                        int i;

                        *--key2 = *--key1;
                        i = num_values;
                        while (--i >= 0) {
                            *--data2 = *--data1;
                        }
                        if (! (count1 -= sizeof(struct svalue)) )
                            break;
                        if (key1[-1].type == T_INVALID) {
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
            }
            if (count1) {
                while (1) {
                    int i;

                    *--key2 = *--key1;
                    i = num_values;
                    while (--i >= 0) {
                        *--data2 = *--data1;
                    }
                    if (! (count1 -= sizeof(struct svalue)) )
                        break;
                    if (key1[-1].type == T_INVALID) {
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
            } else {
                while (misc_hook1) {
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
            m->user->mapping_total +=
                (cm2_end - (char *)data2) -
                (cm1_end - (char *)data1);
            xfree( (char *)(data1) ); /* free old condensed mapping part */
        } /* end of merge block */
        m->condensed = cm2;
        m->hash = 0;
        free_mapping(m);
        m = hm->next_dirty;
        xfree( (char *)hm );
    }
    dirty_mapping_head_hash.next_dirty = m;
}

mp_int total_mapping_size() {
    struct wiz_list *wl;
    mp_int total;

    total = default_wizlist_entry.mapping_total;
    for (wl = all_wiz; wl; wl = wl->next) {
        total += wl->mapping_total;
    }
    return total;
}

#if 0
void check_dirty(avoid)
    int avoid;
{
    struct mapping *m;
    mp_int i;

    m = dirty_mapping_head_hash.next_dirty;
    for (i = num_dirty_mappings - avoid; --i >= 0; m = m->hash->next_dirty) {
        struct hash_mapping *hm = m->hash;
        mp_int j;
        struct map_chain **mcp, *mc;

        j = hm->mask;
        mcp = hm->chains;
        do {
            mc = *mcp++;
            while (mc) {
                if ((p_int)mc & 0xff000003) /* The mask is machine dependent. */
                    fatal("check_dirty\n");
                mc = mc->next;
            }
        } while (--j >= 0);
    }
}
#endif

struct set_mapping_user_locals {
    int num_values;
    struct object *owner;
    struct svalue *hairy; /* changing keys */
};

static void set_mapping_user_filter(key, data, extra)
    struct svalue *key, *data;
    char *extra;
{
    int i;
    struct set_mapping_user_locals *locals;
    struct object *owner;

    locals = (struct set_mapping_user_locals *)extra;
    owner = locals->owner;
    if (key->type == T_CLOSURE && !CLOSURE_MALLOCED(key->x.closure_type)) {
        *locals->hairy++ = *key;
    } else {
        set_svalue_user(key, owner);
    }
    for (i = locals->num_values; --i >= 0;) {
        set_svalue_user(data++, owner);
    }
}

void set_mapping_user(m, owner)
    struct mapping *m;
    struct object *owner;
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
    total =
      sizeof *m + sizeof(char *) + sizeof *cm + sizeof(char *) +
        ( cm->string_size * (sizeof(struct svalue)/sizeof(char*)) +
          cm->misc_size) * (1 + num_values) -
        cm->string_size * (sizeof(struct svalue)/sizeof(char *) - 1);
    m->user->mapping_total -= total;
    user = owner->user;
    m->user = user;
    m->user->mapping_total += total;
    locals.owner = owner;
    locals.num_values = num_values;
    locals.hairy = first_hairy = (struct svalue *)alloca(cm->misc_size);
    walk_mapping(m, set_mapping_user_filter, (char *)&locals);
    for (i = locals.hairy - first_hairy; --i >= 0; first_hairy++) {
        struct svalue new_key, *dest, *source;
        mp_int j;

        assign_svalue_no_free(&new_key, first_hairy);
        set_svalue_user(&new_key, owner);
        dest = get_map_lvalue(m, &new_key, 1);
        free_svalue(&new_key);
        source = get_map_lvalue(m, first_hairy, 0);
        if (num_values)
            memcpy((char *)dest, (char *)source, num_values * sizeof *dest);
        for (j = num_values; --j >= 0; source++)
            source->type = T_INVALID;
        remove_mapping(m, first_hairy);
    }
}

#ifdef MALLOC_smalloc
void count_ref_in_mapping(m)
    struct mapping *m;
{
    char **str;
    struct svalue *svp, *data;
    mp_int size;
    mp_int num_values;
    int any_destructed = 0;

    num_values = m->num_values;
    str = CM_STRING(m->condensed);
    size = m->condensed->string_size;
    while ( (size -= sizeof(char *)) >= 0) {
        count_ref_from_string(*str++);
    }
    data = (struct svalue *)str;
    count_ref_in_vector(
      (struct svalue *)str,
      m->condensed->string_size / sizeof *str * num_values
    );
    svp = CM_MISC(m->condensed);
    size = m->condensed->misc_size;
    while ( (size -= sizeof(struct svalue)) >= 0) {
        --svp;
        if ( (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED) ||
            ( svp->type == T_CLOSURE &&
              ( CLOSURE_MALLOCED(svp->x.closure_type) ?
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
            if (!any_destructed) {
                any_destructed = 1;
                /* Might be a small mapping. Don't malloc, it might get too
                 * much due to the global scope of garbage_collection.
                 * Since there was a previous
                 * compact_mappings(num_dirty_mappings) , the hash field is
                 * known to be 0.
                 */
                m->hash = (struct hash_mapping *)stale_mappings;
                stale_mappings = m;
                /* We are going to use free_svalue() later to get rid of the
                 * data asscoiated with the keys. This data might reference
                 * mappings with destructed keys... Thus, we must prevent
                 * free_mapping() to look at the hash field.
                 */
                m->ref++;
            }
        } else {
            count_ref_in_vector(svp, 1);
        }
    }
    size = m->condensed->misc_size * num_values;
    count_ref_in_vector(
      (struct svalue *)((char *)svp - size),
      size / sizeof *svp
    );
}

void clean_stale_mappings() {
    struct mapping *m, *next;

    for (m = stale_mappings; m; m = next) {
        struct condensed_mapping *cm, *cm2;
        char *cm2_start;
        mp_int size, data_size, deleted_size, preserved_size;
        mp_int i, num_deleted = 0, num_values;
        struct svalue *svp, *svp2, *data, *data2;

        next = (struct mapping *)m->hash;
        m->hash = 0;
        num_values = m->num_values;
        cm = m->condensed;
        svp = CM_MISC(cm);
        i = size = cm->misc_size;
        while ( (i -= sizeof(struct svalue)) >= 0) {
            if ( (--svp)->type == T_INVALID)
                num_deleted++;
        }
        data_size = size * num_values;
        deleted_size = num_deleted * sizeof(struct svalue) * (num_values + 1);
        preserved_size = sizeof(*cm2) +
          cm->string_size *
          (1 + (sizeof(struct svalue)/sizeof(char *)) * num_values);
        m->user->mapping_total -= deleted_size;
        cm2_start = xalloc(data_size + size - deleted_size + preserved_size);
        cm2 = (struct condensed_mapping *)
          (cm2_start + data_size + size - deleted_size);
        memcpy((char *)cm2, (char *)cm, preserved_size);
        cm2->misc_size = size - num_deleted * sizeof(struct svalue);
        data = svp;
        svp2 = CM_MISC(cm2);
        data2 = (struct svalue *)((char *)svp2 - size) + num_deleted;
        svp = CM_MISC(cm);
        i = size;
        while ( (i -= sizeof(struct svalue)) >= 0) {
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
        xfree((char *)cm - data_size - size);
        free_mapping(m);
    }
}
#endif /* MALLOC_smalloc */

/***************************************************************************/

