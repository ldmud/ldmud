#ifndef __MAPPING_H__
#define __MAPPING_H__ 1

#include "driver.h"

#include "interpret.h"  /* struct svalue, mapping, hash_mapping, vector */
#include "object.h"     /* struct object */

/* --- Types --- */

/* --- struct mapping: the mapping datatypes --- */

/* The main structure of a mapping
 */

struct mapping {
    p_int                     ref;         /* Number of references */
    struct hash_mapping      *hash;        /* 'dirty' part of the mapping */
    struct condensed_mapping *condensed;
      /* 'clean' part of the mapping. If this pointer is NULL, the mapping
       * is pending for final deallocation.
       */
    struct wiz_list          *user;        /* Save who made the mapping */
    int                       num_values;  /* Number of values per key */
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
    struct mapping   *next_dirty;  /* Next dirty mapping in the list */
    struct map_chain *chains[ 1 /* +.mask */ ];
      /* The hash chain heads ('hash buckets')
       */
};


/* One 'dirty' entry in a mapping.
 * It is allocated big enough for data[] to hold all values.
 */

struct map_chain {
    struct map_chain *next;  /* next dirty entry in hash_mapping chain */
    struct svalue key;       /* the key value */
    struct svalue data[1];   /* the data values */
      /* &data == &key+1 is used in some places, like in copy_mapping()
       * or resize_mapping() */
};


/* The 'clean' part of a mapping.
 *
 * The memory layout is a bit complicated...
 */

struct condensed_mapping {

    /* struct svalue m_values[ ... ];
     *
     *   The values associated with the following misc (non-string) keys.
     *   The association is (with num values per key):
     *
     *     misc[x] -> m_values[num * x .. num * x + num - 1]
     *
     * struct svalue misc[ ... ]
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
     * struct svalue s_values[ ... ];
     *
     *   The values associated with the preceeding string keys.
     *   The association is (with num values per key):
     *
     *     string[x] -> s_values[num * x .. num * x + num - 1]
     */
};

/* --- Macros --- */

#define CM_MISC(m) ((struct svalue *)(m))
  /* For condensed_mapping <m>, return a pointer to the svalue
   * after the last misc key in that mapping.
   */

#define CM_STRING(m) ((char **)((m)+1))
  /* For condensed_mapping <m>, return a pointer to the first
   * string key in that mapping.
   */

#define MAP_CHAIN_SIZE(n) (\
        (sizeof(struct map_chain) - sizeof(struct svalue) ) +\
                sizeof(struct svalue)*(n) )
  /* Size of a map_chain structure for <n> values
   */

#define MAP_SIZE(m) (\
        (m)->condensed->string_size / sizeof(char *) + \
        (m)->condensed->misc_size   / sizeof(struct svalue) + \
        ((m)->hash ? (m)->hash->used - (m)->hash->condensed_deleted : 0) \
                    )
  /* Size of a given mapping <m>.
   */

/* struct mapping *ref_mapping(struct mapping *m)
 *   Add another ref to array <m> and return the vector <m>.
 */

#define ref_mapping(m) ((m)->ref++, (m))

/* void free_mapping(struct mapping *m)
 *   Subtract one ref from array <m>, and free the array fully if
 *   the refcount reaches zero.
 */

#define free_mapping(m) MACRO( if (--((m)->ref) <= 0) _free_mapping(m); )

/* p_int deref_mapping(struct mapping *m)
 *   Subtract one ref from mapping <m>, but don't check if it needs to
 *   be freed. Result is the number of refs left.
 */

#define deref_mapping(m) (--(m)->ref)

/* --- Variables --- */

extern mp_int num_mappings;
extern mp_int num_dirty_mappings;

/* --- Prototypes --- */

extern struct mapping *allocate_mapping(mp_int size, mp_int num_values);
extern void _free_mapping(struct mapping *m);
extern void free_empty_mapping(struct mapping *m);
extern void free_protector_mapping(struct mapping *m);
extern struct svalue *_get_map_lvalue(struct mapping *m, struct svalue *map_index, Bool need_lvalue, Bool check_size);
#define get_map_value(m,x) _get_map_lvalue(m,x,MY_FALSE, MY_TRUE)
#define get_map_lvalue(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_TRUE)
#define get_map_lvalue_unchecked(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_FALSE)
extern void check_map_for_destr(struct mapping *m);
extern void remove_mapping(struct mapping *m, struct svalue *map_index);
extern struct mapping *copy_mapping(struct mapping *m);
extern struct mapping *resize_mapping(struct mapping *m, mp_int new_width);
extern struct mapping *add_mapping(struct mapping *m1, struct mapping *m2);
extern void walk_mapping(struct mapping *m, void (*func)(struct svalue *key, struct svalue *val, void *extra), void *extra);
extern void compact_mappings(mp_int num);
extern mp_int total_mapping_size(void);
extern void set_mapping_user(struct mapping *m, struct object *owner);

extern struct vector *m_indices(struct mapping *m);
extern void sub_from_mapping_filter(struct svalue *key, struct svalue *data, void *extra);
extern void add_to_mapping(struct mapping *m1, struct mapping *m2);
extern struct mapping *subtract_mapping(struct mapping *minuend, struct mapping *subtrahend);
extern struct svalue *filter_mapping(struct svalue *sp, int num_arg, Bool bFull);
extern struct svalue *map_mapping(struct svalue *sp, int num_arg, Bool bFull);
extern struct svalue *f_walk_mapping(struct svalue *sp, int num_arg);
extern struct svalue *f_m_reallocate (struct svalue *sp);

#ifdef DEBUG
extern void check_dirty_mapping_list(void);
#endif

#ifdef MALLOC_smalloc
extern void count_ref_in_mapping(struct mapping *m);
extern void clean_stale_mappings(void);
#endif

#endif /* __MAPPING_H__ */

