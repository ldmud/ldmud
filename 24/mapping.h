#ifndef __MAPPING_H__
#define __MAPPING_H__ 1

#include "driver.h"

#include "interpret.h"  /* struct svalue, mapping, hash_mapping, vector */
#include "object.h"     /* struct object */

/* --- Variables --- */

extern mp_int num_mappings;
extern mp_int num_dirty_mappings;
extern struct svalue walk_mapping_string_svalue;

/* --- Prototypes --- */

extern struct mapping *allocate_mapping PROT((mp_int size, mp_int num_values));
extern void free_mapping PROT((struct mapping *m));
extern void free_empty_mapping PROT((struct mapping *m));
extern void free_protector_mapping PROT((struct mapping *m));
extern struct svalue *get_map_lvalue PROT((struct mapping *m, struct svalue *map_index, int need_lvalue));
extern void check_map_for_destr PROT((struct mapping *m));
extern void remove_mapping PROT((struct mapping *m, struct svalue *map_index));
extern struct mapping *copy_mapping PROT((struct mapping *m));
extern struct mapping *add_mapping PROT((struct mapping *m1, struct mapping *m2));
extern void walk_mapping PROT((struct mapping *m, void (*func)(struct svalue *, struct svalue *, char *), char *extra));
extern void f_walk_mapping_filter PROT((struct svalue *key, struct svalue *data, char *extra));
extern void f_walk_mapping_cleanup PROT((struct svalue *arg));
extern struct svalue *walk_mapping_prologue PROT((struct mapping *m, struct svalue *sp));
extern struct vector *m_indices PROT((struct mapping *m));
extern void sub_from_mapping_filter PROT((struct svalue *key, struct svalue *data, char *extra));
extern void add_to_mapping PROT((struct mapping *m1, struct mapping *m2));
extern struct mapping *subtract_mapping PROT((struct mapping *minuend, struct mapping *subtrahend));
extern struct svalue *filter_mapping PROT((struct svalue *sp, int num_arg));
extern struct svalue *map_mapping PROT((struct svalue *sp, int num_arg));
extern void compact_mappings PROT((mp_int num));
extern mp_int total_mapping_size PROT((void));
extern void set_mapping_user PROT((struct mapping *m, struct object *owner));

#ifdef MAPPINGS
extern struct svalue *f_walk_mapping PROT((struct svalue *sp, int num_arg));
#endif

#ifdef DEBUG
extern void check_dirty_mapping_list PROT((void));
#endif

#ifdef MALLOC_smalloc
extern void count_ref_in_mapping PROT((struct mapping *m));
extern void clean_stale_mappings PROT((void));
#endif

#endif /* __MAPPING_H__ */
