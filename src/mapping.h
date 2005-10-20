#ifndef MAPPING_H__
#define MAPPING_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "svalue.h"

/* --- Types --- */

/* --- struct mapping: the mapping datatypes --- */

/* The main structure of a mapping
 */

struct mapping_s {
    p_int                     ref;         /* Number of references */
    struct hash_mapping      *hash;        /* 'dirty' part of the mapping */
    struct condensed_mapping *condensed;
      /* 'clean' part of the mapping. If this pointer is NULL, the mapping
       * is pending for final deallocation.
       */
    wiz_list_t               *user;        /* Save who made the mapping */
    p_int                     num_values;  /* Number of values per key */
};


/* The 'dirty' part of a mapping.
 *
 * It is allocated big enough for chains[] to hold all (.mask+1) chain
 * heads. The number of chains is chosen, and if necessary increased,
 * so that the average length of a chain is less or equal two entries.
 */

struct hash_mapping {
    p_int mask;
      /* Index mask for chains[], converting the raw hash value into
       * the valid index number using a bit-and operation.
       * Incremented by one, it's the number of chains.
       */
    p_int used;                    /* Number of entries in the hash */
    p_int condensed_deleted;
      /* Number of entries deleted from the condensed part
       */
    p_int ref;
      /* Refcount if this mapping is part of a T_PROTECTOR_MAPPING svalue.
       * The value is <= the mappings main refcount.
       */
    struct map_chain *deleted;
      /* Protector mappings only: list of deleted entries, which are kept
       * pending because the they may still be used as destination for
       * a lvalue.
       */
    mapping_t        *next_dirty;  /* Next dirty mapping in the list */
    struct map_chain *chains[ 1 /* +.mask */ ];
      /* The hash chain heads ('hash buckets')
       */
};


/* One 'dirty' entry in a mapping.
 * It is allocated big enough for data[] to hold all values.
 */

struct map_chain {
    struct map_chain *next;  /* next dirty entry in hash_mapping chain */
    svalue_t key;       /* the key value */
    svalue_t data[1];   /* the data values */
      /* &data == &key+1 is used in some places, like in copy_mapping()
       * or resize_mapping() */
};


/* The 'clean' part of a mapping.
 *
 * The memory layout is a bit complicated...
 */

struct condensed_mapping {

    /* svalue_t m_values[ ... ];
     *
     *   The values associated with the following misc (non-string) keys.
     *   The association is (with num values per key):
     *
     *     misc[x] -> m_values[num * x .. num * x + num - 1]
     *
     * svalue_t misc[ ... ]
     *
     *   The misc keys and their associated values.
     *   Invalid keys have the svalue-type T_INVALID, the associated
     *   values are (usually) all svalue-0; and both are always located at
     *   the end of the array.
     */

    p_int misc_size;    /* Total size of the misc key values in byte(!) */
    p_int string_size;  /* Total size of the string key pointers in byte(!) */
      /* Using the size in Byte for these two values allows a faster setup
       * for the search when indexing
       */

    /* char *string[ ... ];
     *
     *   Pointer to the key strings. Uneven pointer values denote
     *   deleted/invalidated keys and no longer point to valid
     *   memory, the associated values are (usually) all svalue-0.
     *
     * svalue_t s_values[ ... ];
     *
     *   The values associated with the preceeding string keys.
     *   The association is (with num values per key):
     *
     *     string[x] -> s_values[num * x .. num * x + num - 1]
     */
};

/* --- Macros --- */

#define CM_MISC(m) ((svalue_t *)(m))
  /* For condensed_mapping <m>, return a pointer to the svalue
   * after the last misc key in that mapping.
   */

#define CM_STRING(m) ((char **) ((m)+1))
  /* For condensed_mapping <m>, return a pointer to the first
   * string key in that mapping.
   */

#define MAP_CHAIN_SIZE(n) (\
        (sizeof(struct map_chain) - sizeof(svalue_t) ) +\
                sizeof(svalue_t)*(n) )
  /* Size of a map_chain structure for <n> values
   */

#define MAP_SIZE(m) (\
        (m)->condensed->string_size / sizeof(char *) + \
        (m)->condensed->misc_size   / sizeof(svalue_t) + \
        ((m)->hash ? (m)->hash->used - (m)->hash->condensed_deleted : 0) \
                    )
  /* Size of a given mapping <m>.
   */

/* mapping_t *ref_mapping(mapping_t *m)
 *   Add another ref to mapping <m> and return the mapping <m>.
 */

#define ref_mapping(m) ((m)->ref++, (m))

/* void free_mapping(mapping_t *m)
 *   Subtract one ref from mapping <m>, and free the mapping fully if
 *   the refcount reaches zero.
 */

#define free_mapping(m) MACRO( if (--((m)->ref) <= 0) _free_mapping(m); )

/* p_int deref_mapping(mapping_t *m)
 *   Subtract one ref from mapping <m>, but don't check if it needs to
 *   be freed. Result is the number of refs left.
 */

#define deref_mapping(m) (--(m)->ref)

/* --- Variables --- */

extern mp_int num_mappings;
extern mp_int num_dirty_mappings;

/* --- Prototypes --- */

extern mapping_t *allocate_mapping(mp_int size, mp_int num_values);
extern void _free_mapping(mapping_t *m);
extern void free_empty_mapping(mapping_t *m);
extern void free_protector_mapping(mapping_t *m);
extern svalue_t *_get_map_lvalue(mapping_t *m, svalue_t *map_index, Bool need_lvalue, Bool check_size);
#define get_map_value(m,x) _get_map_lvalue(m,x,MY_FALSE, MY_TRUE)
#define get_map_lvalue(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_TRUE)
#define get_map_lvalue_unchecked(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_FALSE)
extern void check_map_for_destr(mapping_t *m);
extern void remove_mapping(mapping_t *m, svalue_t *map_index);
extern mapping_t *copy_mapping(mapping_t *m);
extern mapping_t *resize_mapping(mapping_t *m, mp_int new_width);
extern mapping_t *add_mapping(mapping_t *m1, mapping_t *m2);
extern void walk_mapping(mapping_t *m, void (*func)(svalue_t *key, svalue_t *val, void *extra), void *extra);
extern void compact_mappings(mp_int num);
extern mp_int total_mapping_size(void);
extern void set_mapping_user(mapping_t *m, object_t *owner);

extern vector_t *m_indices(mapping_t *m);
extern void sub_from_mapping_filter(svalue_t *key, svalue_t *data, void *extra);
extern void add_to_mapping(mapping_t *m1, mapping_t *m2);
extern mapping_t *subtract_mapping(mapping_t *minuend, mapping_t *subtrahend);
extern svalue_t *x_filter_mapping(svalue_t *sp, int num_arg, Bool bFull);
extern svalue_t *f_filter_indices (svalue_t *sp, int num_arg);
extern svalue_t *x_map_mapping(svalue_t *sp, int num_arg, Bool bFull);
extern svalue_t *f_map_indices (svalue_t *sp, int num_arg);
extern svalue_t *f_walk_mapping(svalue_t *sp, int num_arg);
extern svalue_t *f_m_reallocate (svalue_t *sp);

#ifdef DEBUG
extern void check_dirty_mapping_list(void);
#endif

#ifdef GC_SUPPORT
extern void count_ref_in_mapping(mapping_t *m);
extern void clean_stale_mappings(void);
extern void clear_mapping_size (void);
extern void count_mapping_size (mapping_t *m);
#endif

#endif /* MAPPING_H__ */

