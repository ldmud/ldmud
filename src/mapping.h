#ifndef MAPPING_H__
#define MAPPING_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "svalue.h"

/* --- Types --- */

/* Local typedefs */

typedef struct mapping_hash_s mapping_hash_t;
typedef struct mapping_cond_s mapping_cond_t;

/* --- struct mapping_s: the mapping datatypes --- */

/* The main structure of a mapping
 */

struct mapping_s {
    p_int       ref;               /* Number of references */
    wiz_list_t *user;              /* Who made the mapping */
    p_int       num_values;        /* Number of values for a key */
    p_int       num_entries;       /* Number of valid entries */
    uint32_t    last_destr_check;  /* Last check for destr. object in keys */
    struct mapping_cond_s * cond;  /* Condensed entries */
    struct mapping_hash_s * hash;  /* Hashed entries */
    mapping_t  *next;
      /* Next mapping - for use by the cleanup code and
       * the garbage collector.
       */
};

/* --- struct mapping_hash_s: the hashed index ---
 *
 * New entries in a mapping are stored in a hash structure for fast
 * access. The hash grows dynamically with the number of entries -
 * the structure is then reallocated to fit - the goal is to keep
 * on average two entries per hash bucket.
 *
 * This structure is exported so that interpret.c can use it to
 * build protectors.
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
    mp_int        last_used;
      /* Time of the last change (add or delete) to the mapping.
       */
    p_int         cond_deleted;
      /* Number of entries deleted from the condensed part
       */
    struct map_chain_s *deleted;
      /* Protector mappings only: list of deleted entries, which are kept
       * pending because the they may still be used as destination for
       * a lvalue.
       */
    struct map_chain_s * chains[ 1 /* +.mask */ ];
      /* The hash chain heads ('hash buckets')
       */
};

#define SIZEOF_MH_ALL(hm, nv) ( \
    sizeof(*(hm)) + sizeof(map_chain_t *) * (hm)->mask \
                  + (sizeof(map_chain_t) + sizeof(svalue_t) * (nv)) * ((hm)->used) \
                      )
  /* Allocation size of a given mapping_hash_t structure for a mapping
   * with <nv> values per key, including map_chain structures.
   */

#define SIZEOF_MH(hm) ( \
    sizeof(*(hm)) + sizeof(map_chain_t *) * (hm)->mask \
                      )
  /* Allocation size of a given mapping_hash_t structure, excluding
   * the map_chain structures.
   */


/* --- struct mapping_cond_s: the condensed block ---
 *
 * When a mapping is compacted, the entries are stored in sorted order
 * in this structure (allocated to size).
 * If later a key is deleted, it's .type is set to T_INVALID.
 *
 * The structure is exported so that the swapper can use it.
 */

struct mapping_cond_s {
    size_t   size;                 /* Number of allocated entries */
    svalue_t data[1 /* .size * (m->num_values+1) */];
};

#define SIZEOF_MC(cm, nv) ( \
    sizeof(*(cm)) + sizeof(svalue_t) * (((cm)->size * ((nv)+1)) - 1) \
                      )
  /* Allocation size of a given mapping_cond_t structure for
   * a mapping with <nv> values per key.
   */

#define COND_DATA(cm, ix, nv) ( \
    &((cm)->data[(cm)->size + (ix) * (nv)]) \
                              )
  /* For key entry [<ix>] in a given mapping_cond_t for a mapping
   * with <nv> values per key, return pointer to the first assocated value.
   */


/* --- struct mvf_info: structure used by m_values()/unmkmapping() ---
 *
 * This structure is passed by reference to the filter functions used
 * by the two efuns and passes information from one filter call to the
 * next.
 */
struct mvf_info
{
    svalue_t * svp;
      /* m_values_filter: Pointer to next result vector entry
       * m_unmake_filter: Pointer to result array of svalues
       */
    int             num;
      /* m_values_filter: Column to retrieve.
       * m_unmake_filter: Next row to retrieve
       */
    int             width;
      /* m_unmake_filter: width of the mapping
       */
};


/* --- Macros --- */

#define MAP_SIZE(m) ((m)->num_entries)
  /* Size (number of keys) of a given mapping <m>.
   */

#define MAP_TOTAL_SIZE(m) ((m)->num_entries * (1+(m)->num_values))
  /* Size (number of keys and values) of a given mapping <m>.
   */

/* mapping_t *ref_mapping(mapping_t *m)
 *   Add another ref to mapping <m> and return the mapping <m>.
 */

#define ref_mapping(m) ((m)->ref++, (m))

/* Bool free_mapping(mapping_t *m)
 *   Subtract one ref from mapping <m>, and free the mapping fully if
 *   the refcount reaches zero.
 *   Return TRUE if the mapping is deallocated, and FALSE if not.
 */

#define free_mapping(m) ( (--((m)->ref) <= 0) ? _free_mapping(m, MY_FALSE) : MY_FALSE )

/* p_int deref_mapping(mapping_t *m)
 *   Subtract one ref from mapping <m>, but don't check if it needs to
 *   be freed. Result is the number of refs left.
 */

#define deref_mapping(m) (--(m)->ref)

/* --- Variables --- */

extern mp_int num_mappings;
extern mp_int num_hash_mappings;
extern mp_int num_dirty_mappings;

/* --- Prototypes --- */

extern mapping_t *allocate_mapping(mp_int size, mp_int num_values);
extern mapping_t *allocate_cond_mapping(wiz_list_t * user, mp_int size, mp_int num_values);
extern Bool _free_mapping(mapping_t *m, Bool no_data);
#define free_empty_mapping(m) _free_mapping(m, MY_TRUE)
extern void free_protector_mapping(mapping_t *m);
extern svalue_t *_get_map_lvalue(mapping_t *m, svalue_t *map_index, Bool need_lvalue, Bool check_size);
#define get_map_value(m,x) _get_map_lvalue(m,x,MY_FALSE, MY_TRUE)
#define get_map_lvalue(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_TRUE)
#define get_map_lvalue_unchecked(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_FALSE)
extern Bool mapping_references_objects (mapping_t *m);
extern void check_map_for_destr_keys(mapping_t *m);
extern void check_map_for_destr_values(mapping_t *m);
extern void check_map_for_destr(mapping_t *m);
extern mapping_t *resize_mapping(mapping_t *m, mp_int new_width);
#define copy_mapping(m) resize_mapping((m), (m)->num_values)
extern mapping_t *add_mapping(mapping_t *m1, mapping_t *m2);
extern void walk_mapping(mapping_t *m, void (*func)(svalue_t *key, svalue_t *val, void *extra), void *extra);
extern Bool compact_mapping(mapping_t *m, Bool force);
extern mp_int total_mapping_size(void);
extern size_t mapping_overhead(mapping_t *m);
extern void set_mapping_user(mapping_t *m, object_t *owner);

extern svalue_t *f_m_allocate(svalue_t *sp);
extern svalue_t *v_m_contains(svalue_t *sp, int num_arg);
extern svalue_t *v_m_add(svalue_t *sp, int num_arg);
extern svalue_t *f_m_delete(svalue_t *sp);
extern vector_t *m_indices(mapping_t *m);
extern svalue_t *f_m_indices(svalue_t *sp);
extern svalue_t *f_m_values(svalue_t *sp);
extern svalue_t *v_mkmapping (svalue_t *sp, int num_arg);
extern void sub_from_mapping_filter(svalue_t *key, svalue_t *data, void *extra);
extern void add_to_mapping(mapping_t *m1, mapping_t *m2);
extern mapping_t *subtract_mapping(mapping_t *minuend, mapping_t *subtrahend);
extern mapping_t *map_intersect(mapping_t *m, svalue_t * val);
extern vector_t * map_intersect_array (vector_t *vec, mapping_t *map);
extern svalue_t *x_filter_mapping(svalue_t *sp, int num_arg, Bool bFull);
extern svalue_t *v_filter_indices (svalue_t *sp, int num_arg);
extern svalue_t *x_map_mapping(svalue_t *sp, int num_arg, Bool bFull);
extern svalue_t *v_map_indices (svalue_t *sp, int num_arg);
extern svalue_t *v_walk_mapping(svalue_t *sp, int num_arg);
extern svalue_t * f_m_entry (svalue_t *sp);
extern svalue_t *f_m_reallocate (svalue_t *sp);
extern svalue_t *f_unmkmapping (svalue_t *sp);
extern svalue_t *f_widthof (svalue_t *sp);

#ifdef GC_SUPPORT
extern void count_ref_in_mapping(mapping_t *m);
extern void clean_stale_mappings(void);
extern void clear_mapping_size (void);
extern void count_mapping_size (mapping_t *m);
#endif

#ifdef CHECK_MAPPING_TOTAL
extern void m_check_total_mapping_size (const char * file, int line);
#define check_total_mapping_size() m_check_total_mapping_size(__FILE__, __LINE__)
#else
#define check_total_mapping_size() NOOP
#endif

#endif /* MAPPING_H__ */
