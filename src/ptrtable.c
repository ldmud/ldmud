/*---------------------------------------------------------------------------
 * Pointer Registration Table
 *
 *---------------------------------------------------------------------------
 * Several actions in the driver involve a depth search through all
 * mappings and arrays referenced from one object, including the detection
 * of cyclic references.
 *
 * This is solved with a two-dimension hash structure, recording every
 * mapping/array pointer encountered. As the same functionality
 * is needed in some other places of the driver, too, the whole bunch
 * of functions are made public.
 *
 * The table is used to create a mirror of the array/mapping relations:
 * every mapping/array is represented by one record, recording an
 * ID number and how often the represented array/mapping was encountered.
 *
 * The entries of the top-level hashtable may be unused, contain plain
 * records or one of the second-level hashtables. The bit vector hash_usage
 * keeps track of which entry is what.
 *
 * TODO: A generic walk_this_svalue() function should go in here, with
 * TODO:: a callback foundthis(svalue, ptable, extra), returning
 * TODO:: true if svalue shall be further descended into.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <sys/types.h>

#include "ptrtable.h"
#include "mempools.h"
#include "simulate.h"

/*-------------------------------------------------------------------------*/

#define PTABLE_SIZE 256
  /* Sice of a pointer table. The value must be a power of 2.
   */

/*-------------------------------------------------------------------------*/
/* Structure definitions
 */

/* One sub table.
 */
struct sub_table
{
    struct pointer_record *records[PTABLE_SIZE];
      /* The table of hash chains */
    char used[PTABLE_SIZE / CHAR_BIT];
      /* Bitvector denoting which record[] entries are valid */
};


/* The pointer table base structure.
 */
struct pointer_table
{
    Mempool pool;
      /* The memory pool from which the table and all structures are
       * allocated.
       */

    struct pointer_record *table[PTABLE_SIZE];
      /* The top-level hashtable.
       */

    char hash_usage[ PTABLE_SIZE * 2 / CHAR_BIT ];
     /* Bit vector describing the state of every entry in table[],
      * two bits each.
      *
      * The state of entry table[hash] is described
      * in hash_usage[(hash/8 ) * 2] and hash_usage[(hash/8) * 2 + 1],
      * bit hash%8 each. The first bit is true if the associated entry
      * is in use, the second if the entry holds a sub table.
      */
};

/*-------------------------------------------------------------------------*/
struct pointer_table *
new_pointer_table (void)

/* Allocate and initialise a new pointer table and return it.
 */

{
    Mempool pool;
    struct pointer_table *ptable;

    pool = new_mempool(size_mempool(sizeof(struct pointer_record)));
    if (!pool)
        return NULL;
    ptable = mempool_alloc(pool, sizeof(*ptable));
    if (!ptable)
    {
        mempool_delete(pool);
        return NULL;
    }
    memset(ptable->hash_usage, 0, sizeof ptable->hash_usage);
    memset(ptable->table, 0, sizeof ptable->table);
    ptable->pool = pool;

    return ptable;
} /* new_pointer_table() */

/*-------------------------------------------------------------------------*/
void
free_pointer_table (struct pointer_table *ptable)

/* Deallocate the table <ptable> and all referenced memory.
 */

{
    mempool_delete(ptable->pool);
} /* free_pointer_table() */

/*-------------------------------------------------------------------------*/
struct pointer_record *
find_add_pointer (struct pointer_table *ptable, void *pointer, Bool bAdd)

/* Lookup the <pointer> in the <ptable> and return the pointer to its
 * pointer_record. If the <pointer> is not registered yet and <bAdd> is
 * false, NULL is returned. Otherwise, a new entry with .ref_count set to -1,
 * .id_number and .data cleared, is generated and returned.
 */

{
    p_uint key;     /* The <pointer> as a normal int */
    int hash;       /* hash computed from <key> aka <pointer> */
    int mask;       /* mask for <hash> in to <usage_p> */
    char *usage_p;  /* First usage vector byte for entry <hash> */
    struct pointer_record *old;      /* Record to add a new one after */
    struct pointer_record *new;      /* New record to add */
    struct pointer_record **insert;  /* Pointer to hashed table entry */

    key = (p_uint)pointer;

    /* Compute the hash value, and the index and mask for
     * the usage vector
     */

    /* TODO: This code assumes that a pointer is 32 Bits long */
    hash = key ^ key >> 16;
    hash ^= hash >> 8;
    hash &= (PTABLE_SIZE-1);
    mask = 1 << (hash & (CHAR_BIT-1));
    /* TODO: this statement assumes CHAR_BIT == 8 */
    usage_p = ptable->hash_usage + (hash >> 2 & ~1);

    insert = &(ptable->table[hash]);

    /* Search the pointer among the existing entries.
     *
     * The switch() allows an easy way out of the if() when
     * a new entry has to be generated.
     */

    old = NULL;
    if (usage_p[0] & mask) switch (0) { default:

        /* The place in the main hash table has been used before */

        if (usage_p[1] & mask)
        {
            /* This place is already used for a sub-table.
             * Continue the search in there.
             */
            struct sub_table *table;

            table = *(struct sub_table**)insert;

            hash = (key ^ key >> 16) & (PTABLE_SIZE-1);
            mask = 1 << (hash & (CHAR_BIT-1));
            /* TODO: this statement assumes CHAR_BIT == 8 */
            usage_p = &table->used[hash >> 3];

            insert = &table->records[hash];

            if ( !(usage_p[0] & mask) )
                /* need to insert in free place */
                break;

            old = *insert;
            do {
                if (old->key == key)
                {
                    return old;
                }
            } while ( NULL != (old = old->next) );
            old = *insert;
            /* need to insert at top of sub hash chain */
            break;
        }
        else
        {
            /* The entry is occupied by a record */

            struct sub_table *table;
            int old_hash;

            old = *insert;
            if (old->key == key) {
                return old;
            }

            if (!bAdd)
                return NULL;

            /* Hash collision: create a new sub-table. */

            usage_p[1] |= mask;

            table = mempool_alloc(ptable->pool, sizeof *table);
            if (!table)
                errorf("(pointertable) Out of memory (%zu bytes pooled) "
                      "for subtable.\n", sizeof *table);
            *insert = (struct pointer_record *)table;
            memset(table->used, 0, sizeof table->used);

            /* Put the old entry into the new subtable */

            /* TODO: This code yaddayadda... */
            old_hash = (old->key ^ old->key >> 16) & (PTABLE_SIZE-1);
            table->used[old_hash >> 3] |= 1 << (old_hash & (CHAR_BIT-1));
            table->records[old_hash] = old;

            /* Compute the position for the new entry */

            hash = (key ^ key >> 16) & (PTABLE_SIZE-1);
            if (hash != old_hash)
            {
                old = NULL;
            }
            insert = &table->records[hash];
            mask = 1 << (hash & (CHAR_BIT-1));
            usage_p = &table->used[hash >> 3];
        }
    }


    /* Pointer not found in table: create a new entry? */
    if (!bAdd)
        return NULL;

    usage_p[0] |= mask;
    new = mempool_alloc(ptable->pool, sizeof *new);
    if (!new)
        errorf("(pointertable) Out of memory (%zu bytes pooled) for "
              "new entry.\n", sizeof *new);
    *insert = new;
    new->key = key;
    new->next = old;
    new->ref_count = -1;
    new->id_number = 0;
    new->data = NULL;

    return new;
} /* find_add_pointer() */

/*-------------------------------------------------------------------------*/
struct pointer_record *
register_pointer (struct pointer_table *ptable, void *pointer)

/* Register the <pointer> in the <ptable>. If it is already in there,
 * the number of registrations is incremented and the functions returns NULL.
 * If the pointer is not in the table, a new entry is generated, the
 * .ref_count, .id_number and .data entry are cleared and the function
 * returns the new record.
 */

{
    struct pointer_record * prc;

    prc = find_add_pointer(ptable, pointer, MY_TRUE);
    prc->ref_count++;
    return (!prc->ref_count ? prc : NULL);
} /* register_pointer() */

/***************************************************************************/

