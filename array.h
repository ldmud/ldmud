#ifndef ARRAY_H__
#define ARRAY_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "instrs.h"     /* F_FILTER_ARRAY, F_MAP_ARRAY, F_INSER_ALIST */
#include "svalue.h"

/* --- Macros --- */

/* vector_t *ref_array(vector_t *a)
 *   Add another ref to array <a> and return the vector <a>.
 */

#define ref_array(a) ((a)->ref++, (a))

/* void free_array(vector_t *a)
 *   Subtract one ref from array <a>, and free the array fully if
 *   the refcount reaches zero.
 */

#define free_array(a) MACRO( if (--((a)->ref) <= 0) _free_vector(a); )

/* p_int deref_array(vector_t *a)
 *   Subtract one ref from array <a>, but don't check if it needs to
 *   be freed. Result is the number of refs left.
 */

#define deref_array(a) (--(a)->ref)

/* See array.c for a description of what the following macros do. */

/* Helper for LOCAL_VECn() */
#ifdef DEBUG
#    define VEC_DEBUGREF(ref) ref,
#else
#    define VEC_DEBUGREF(ref)
#endif

#define VEC_HEAD(size) size, 1, VEC_DEBUGREF(1) NULL


#if defined(DEBUG) && defined(MALLOC_smalloc)
#    define VEC_SIZE(v) vec_size(v)
     extern p_int vec_size (vector_t *vec);
     extern vector_t *static_vector1, *static_vector2;
#else
#    define VEC_SIZE(v) ((v)->size)
#endif


#define LOCAL_VEC1(name, type1) \
    struct { vector_t v; } name \
      = { { VEC_HEAD(1), { { type1, { 0 } } } } }

#define LOCAL_VEC2(name, type1, type2) \
    struct { vector_t v; svalue_t item[1]; } name \
      = { { VEC_HEAD(2), { { type1, { 0 } } } }, { { type2, { 0 } } } }

#if !defined(MALLOC_TRACE)
#    define ALLOC_VECTOR(nelem, file, line) \
        (vector_t *)xalloc(sizeof (vector_t) + \
                                sizeof(svalue_t) * (nelem - 1))
#else
#    define ALLOC_VECTOR(nelem, file, line) \
        (vector_t *)xalloc_traced(sizeof (vector_t) + \
                                sizeof(svalue_t) * (nelem - 1), file, line)
#endif /* MALLOC_TRACE */


/* --- Types --- */

/* --- struct vector: the array datatype ---
 *
 * When smalloc is used, the number of elements can be deduced from
 * the memory block size, so the .size entry is not needed.
 */

struct vector_s {
    p_int size;        /* Number of contained elements */
    p_int ref;         /* Number of references */
#ifdef DEBUG
    p_int extra_ref;   /* Second refcount, used to check .ref. */
#endif
    wiz_list_t *user;  /* Save who made the vector */
    svalue_t item[1];
};


/* --- Variables --- */

extern vector_t null_vector;

extern int num_arrays;
extern char *last_insert_alist_shared_string;
extern void (*allocate_array_error_handler) (char *, ...);
extern svalue_t assoc_shared_string_key;


/* --- Prototypes --- */

#if defined(MALLOC_TRACE)

#define allocate_array(n) (_allocate_array(n, __FILE__ "::allocate_array", __LINE__))
#define allocate_array_unlimited(n) (_allocate_array_unlimited(n, __FILE__ "::allocate_array", __LINE__))
#define allocate_uninit_array(n) (_allocate_array(n, __FILE__ "::allocate_uninit_array", __LINE__))
#define implode_string(a,d) (_implode_string(a,d, __FILE__ "::implode_string", __LINE__))

extern vector_t *_allocate_array(mp_int, char *, int);
extern vector_t *_allocate_array_unlimited(mp_int, char *, int);
extern vector_t *_allocate_uninit_array(mp_int, char *, int);
extern char *_implode_string(vector_t *, char *, char *, int);

#else

extern vector_t *allocate_array(mp_int);
extern vector_t *allocate_array_unlimited(mp_int);
extern vector_t *allocate_uninit_array(mp_int);
extern char *implode_string(vector_t *, char *);

#endif /* MALLOC_TRACE */

extern void _free_vector(vector_t *p);
extern void free_empty_vector(vector_t *p);
extern void check_for_destr(vector_t *v);
extern vector_t *explode_string(char *str, char *del);
extern vector_t *old_explode_string(char *str, char *del);
extern vector_t *slice_array(vector_t *p, mp_int from, mp_int to);
extern vector_t *make_unique(vector_t *arr, char *func, svalue_t *skipnum);
extern vector_t *add_array(vector_t *p, vector_t *q);
extern vector_t *subtract_array(vector_t *minuend, vector_t *subtrahend);
extern vector_t *all_inventory(object_t *ob);
extern vector_t *deep_inventory(object_t *ob, Bool take_top);
extern vector_t *order_alist(svalue_t *inlists, int listnum, Bool reuse);
extern int assoc(svalue_t *key, vector_t *list);
extern vector_t *intersect_alist(vector_t *a1, vector_t *a2);
extern int is_alist(vector_t *v);
extern vector_t *intersect_array(vector_t *a1, vector_t *a2);
extern vector_t *match_regexp(vector_t *v, char *pattern);

extern svalue_t *x_filter_array(svalue_t *sp, int num_arg);
extern svalue_t *x_map_array(svalue_t *sp, int num_arg);

extern svalue_t *f_sort_array(svalue_t *sp, int num_arg);
extern svalue_t *f_transpose_array(svalue_t *sp);
extern svalue_t *f_regexplode(svalue_t *sp);

#ifdef F_INSERT_ALIST
extern svalue_t *insert_alist(svalue_t *key, svalue_t *key_data, vector_t *list);
#endif

#ifdef F_FILTER_ARRAY
extern svalue_t *f_filter_array(svalue_t *sp, int num_arg);
#endif

#ifdef F_MAP_ARRAY
extern svalue_t *f_map_array(svalue_t *sp, int num_arg);
#endif

extern svalue_t *f_inherit_list (svalue_t *sp, int num_arg);
extern svalue_t *f_include_list (svalue_t *sp, int num_arg);
extern svalue_t *f_filter_objects(svalue_t *sp, int num_arg);
extern svalue_t *f_map_objects(svalue_t *sp, int num_arg);
extern svalue_t *f_functionlist(svalue_t *sp);

extern void set_vector_user(vector_t *p, object_t *owner);
extern long total_array_size(void);

#if defined(GC_SUPPORT)
extern void clear_array_size (void);
extern void count_array_size (vector_t *vec);
#endif

#endif /* ARRAY_H__ */
