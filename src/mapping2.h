#ifndef MAPPING_H__
#define MAPPING_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "svalue.h"

/* --- Types --- */

/* --- struct mapping_s: the mapping datatypes --- */

/* The main structure of a mapping
 *
 * Note that .num_values includes the key value!
 */

struct mapping_s {
    p_int       ref;               /* Number of references */
    wiz_list_t *user;              /* Who made the mapping */
    int         num_values;        /* Number of values per entry(!) */
    p_int       num_entries;       /* Number of valid entries */ 

    struct mapping_data_s * data;  /* Data blocks */
    svalue_t              * free;  /* Data entry free list */
    struct mapping_keys_s * keys;  /* Condensed key indices */
    struct mapping_hash_s * hash;  /* Hashed key indices */
};

/* --- Macros --- */

#define MAP_SIZE(m) ((m)->num_entries)
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

#define free_mapping(m) MACRO( if (--((m)->ref) <= 0) _free_mapping(m, MY_FALSE); )

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
extern void _free_mapping(mapping_t *m, Bool no_data);
#define free_empty_mapping(m) _free_mapping(m, MY_TRUE)
extern void free_protector_mapping(mapping_t *m);
extern svalue_t *_get_map_lvalue(mapping_t *m, svalue_t *map_index, Bool need_lvalue, Bool check_size);
#define get_map_value(m,x) _get_map_lvalue(m,x,MY_FALSE, MY_TRUE)
#define get_map_lvalue(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_TRUE)
#define get_map_lvalue_unchecked(m,x) _get_map_lvalue(m,x,MY_TRUE, MY_FALSE)
extern void check_map_for_destr(mapping_t *m);
extern mapping_t *resize_mapping(mapping_t *m, mp_int new_width);
#define copy_mapping(m) resize_mapping((m), (m)->num_values-1)

#endif /* MAPPING_H__ */

#if 0
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

#define CM_MISC(m) ((svalue_t *)(m))
  /* For condensed_mapping <m>, return a pointer to the svalue
   * after the last misc key in that mapping.
   */

#define CM_STRING(m) ((string_t **)((m)+1))
  /* For condensed_mapping <m>, return a pointer to the first
   * string key in that mapping.
   */

#define MAP_CHAIN_SIZE(n) (\
        (sizeof(struct map_chain) - sizeof(svalue_t) ) +\
                sizeof(svalue_t)*(n) )
  /* Size of a map_chain structure for <n> values
   */

/* --- Prototypes --- */

extern mapping_t *add_mapping(mapping_t *m1, mapping_t *m2);
extern void walk_mapping(mapping_t *m, void (*func)(svalue_t *key, svalue_t *val, void *extra), void *extra);
extern void compact_mappings(mp_int num);
extern mp_int total_mapping_size(void);
extern void set_mapping_user(mapping_t *m, object_t *owner);

extern svalue_t *f_copy_mapping(svalue_t *sp);
extern svalue_t *f_m_allocate(svalue_t *sp);
extern svalue_t *f_m_contains(svalue_t *sp, int num_arg);
extern svalue_t *f_m_delete(svalue_t *sp);
extern vector_t *m_indices(mapping_t *m);
extern svalue_t *f_m_indices(svalue_t *sp);
extern svalue_t *f_m_values(svalue_t *sp);
extern svalue_t *f_mkmapping (svalue_t *sp, int num_arg);
extern void sub_from_mapping_filter(svalue_t *key, svalue_t *data, void *extra);
extern void add_to_mapping(mapping_t *m1, mapping_t *m2);
extern mapping_t *subtract_mapping(mapping_t *minuend, mapping_t *subtrahend);
extern svalue_t *x_filter_mapping(svalue_t *sp, int num_arg, Bool bFull);
extern svalue_t *f_filter_indices (svalue_t *sp, int num_arg);
extern svalue_t *x_map_mapping(svalue_t *sp, int num_arg, Bool bFull);
extern svalue_t *f_map_indices (svalue_t *sp, int num_arg);
extern svalue_t *f_walk_mapping(svalue_t *sp, int num_arg);
extern svalue_t *f_m_reallocate (svalue_t *sp);
extern svalue_t *f_unmkmapping (svalue_t *sp);
extern svalue_t *f_widthof (svalue_t *sp);

#ifdef DEBUG
extern void check_dirty_mapping_list(void);
#endif

#ifdef GC_SUPPORT
extern void count_ref_in_mapping(mapping_t *m);
extern void clean_stale_mappings(void);
#endif

#endif

