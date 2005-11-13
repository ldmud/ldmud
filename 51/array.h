#ifndef __ARRAY_H__
#define __ARRAY_H__ 1

#include "driver.h"
#include "instrs.h"     /* F_INHERIT_LIST */
#include "datatypes.h"
#ifdef MALLOC_smalloc
#include "smalloc.h"    /* SMALLOC_OVERHEAD */
#endif

/* --- Macros --- */

/* See array.c for a description of what the macros do. */

/* Helper for VEC_INIT() */
#ifdef DEBUG
#    define VEC_DEBUGREF(ref) ref,
#else
#    define VEC_DEBUGREF(ref)
#endif

#ifdef MALLOC_smalloc

#    define VEC_SIZE(v) (\
      ( malloced_size(v) - \
        ( SMALLOC_OVERHEAD + \
          ( sizeof(struct vector) - sizeof(struct svalue) ) / SIZEOF_P_INT \
        ) \
      ) / (sizeof(struct svalue)/SIZEOF_P_INT) \
     )
#    define INIT_VEC_TYPE p_uint s[SMALLOC_OVERHEAD]; struct vector v
#    define VEC_INIT(size, ref, type) \
        { ( size * sizeof(struct svalue) + \
            sizeof(struct vector) - \
            sizeof(struct svalue) \
          ) / SIZEOF_P_INT + SMALLOC_OVERHEAD }, \
        { ref, VEC_DEBUGREF(ref) (struct wiz_list *)NULL, { { type } } }

#else /* !MALLOC_smalloc */

#    define VEC_SIZE(v) ((v)->size)
#    define INIT_VEC_TYPE struct vector v
#    define VEC_INIT(size, ref, type) \
        { size, ref, VEC_DEBUGREF(ref) (struct wiz_list *)NULL, { { type } } }
#endif

#if !defined(MALLOC_smalloc) || !defined(SMALLOC_TRACE)
#    define ALLOC_VECTOR(nelem, file, line) \
        (struct vector *)xalloc(sizeof (struct vector) + \
                                sizeof(struct svalue) * (nelem - 1))
#else
#    define ALLOC_VECTOR(nelem, file, line) \
        (struct vector *)smalloc(sizeof (struct vector) + \
                                sizeof(struct svalue) * (nelem - 1), file, line)
#endif /* SMALLOC_TRACE */


/* --- Types --- */

/* The global null vector is allocated statically and thus needs
 * its own type.
 */

struct null_vector_aggregate_struct { INIT_VEC_TYPE; };


/* --- Variables --- */

extern struct null_vector_aggregate_struct null_vector_aggregate;
#define null_vector null_vector_aggregate.v

extern int num_arrays;
extern char *last_insert_alist_shared_string;
extern void (*allocate_array_error_handler) (char *, ...);
extern struct svalue assoc_shared_string_key;


/* --- Prototypes --- */

#if defined(MALLOC_smalloc) && defined(SMALLOC_TRACE)

#define allocate_array(n) (_allocate_array(n, __FILE__ "::allocate_array", __LINE__))
#define allocate_uninit_array(n) (_allocate_array(n, __FILE__ "::allocate_uninit_array", __LINE__))
#define implode_string(a,d) (_implode_string(a,d, __FILE__ "::implode_string", __LINE__))

extern struct vector *_allocate_array(mp_int, char *, int);
extern struct vector *_allocate_uninit_array(mp_int, char *, int);
extern char *_implode_string(struct vector *, char *, char *, int);

#else

extern struct vector *allocate_array(mp_int);
extern struct vector *allocate_uninit_array(mp_int);
extern char *implode_string(struct vector *, char *);

#endif /* SMALLOC && SMALLOC_TRACE */

extern void free_vector(struct vector *p);
extern void free_empty_vector(struct vector *p);
extern struct vector *explode_string(char *str, char *del);
extern struct vector *slice_array(struct vector *p, int from, int to);
extern struct vector *make_unique(struct vector *arr, char *func, struct svalue *skipnum);
extern struct vector *add_array(struct vector *p, struct vector *q);
extern struct vector *subtract_array(struct vector *minuend, struct vector *subtrahend);
extern struct vector *all_inventory(struct object *ob);
extern void map_array(struct vector *arr, char *func, struct object *ob, int num_extra, struct svalue *extra);
extern struct vector *sort_array(struct vector *data, char *func, struct object *ob);
extern struct vector *deep_inventory(struct object *ob, int take_top);
extern struct vector *order_alist(struct svalue *inlists, int listnum, int reuse);
extern struct svalue *insert_alist(struct svalue *key, struct svalue *key_data, struct vector *list);
extern int assoc(struct svalue *key, struct vector *list);
extern struct vector *intersect_alist(struct vector *a1, struct vector *a2);
extern int is_alist(struct vector *v);
extern struct vector *intersect_array(struct vector *a1, struct vector *a2);
extern struct vector *match_regexp(struct vector *v, char *pattern);

extern struct svalue *f_filter_array(struct svalue *sp, int num_arg);
extern struct svalue *f_regexplode(struct svalue *sp);

#ifdef F_INHERIT_LIST
extern struct svalue *f_inherit_list(struct svalue *sp);
#endif /* F_INHERIT_LIST */

extern struct svalue *f_filter_objects(struct svalue *sp, int num_arg);
extern struct svalue *f_map_objects(struct svalue *sp, int num_arg);
extern struct svalue *f_functionlist(struct svalue *sp);

extern void set_vector_user(struct vector *p, struct object *owner);
extern long total_array_size(void);

#endif /* __ARRAY_H__ */
