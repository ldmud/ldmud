#ifndef __MAPPING_H__
#define __MAPPING_H__ 1

#include "driver.h"

#include "interpret.h"  /* struct svalue, mapping, hash_mapping, vector */
#include "object.h"     /* struct object */

/* --- Variables --- */

extern mp_int num_mappings;
extern mp_int num_dirty_mappings;

/* --- Prototypes --- */

extern struct mapping *allocate_mapping(mp_int size, mp_int num_values);
extern void free_mapping(struct mapping *m);
extern void free_empty_mapping(struct mapping *m);
extern void free_protector_mapping(struct mapping *m);
extern struct svalue *get_map_lvalue(struct mapping *m, struct svalue *map_index, /* TODO: BOOL */ short need_lvalue);
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
extern struct svalue *filter_mapping(struct svalue *sp, int num_arg, /* TODO: BOOL */ int bFull);
extern struct svalue *map_mapping(struct svalue *sp, int num_arg, /* TODO: BOOL */ int bFull);
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

