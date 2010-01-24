#ifndef ARRAY_H__
#define ARRAY_H__ 1

#include "driver.h"
#include <stddef.h>

#include "typedefs.h"
#include "svalue.h"


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


/* --- Macros --- */
/* See array.c for a description of what the following macros do. */

/* Helper for LOCAL_VECn() */
#ifdef DEBUG
#    define VEC_DEBUGREF(ref) ref,
#else
#    define VEC_DEBUGREF(ref)
#endif


#define VEC_HEAD(size) size, 1, VEC_DEBUGREF(1) NULL

#define VEC_SIZE(v) ((v)->size)

#define LOCAL_VEC1(name, type1) \
    struct { vector_t v; } name \
      = { { VEC_HEAD(1), { { type1, { 0 } } } } }

#define LOCAL_VEC2(name, type1, type2) \
    struct { vector_t v; svalue_t item[1]; } name \
      = { { VEC_HEAD(2), { { type1, { 0 } } } }, { { type2, { 0 } } } }


/* --- Variables --- */

extern vector_t null_vector;

extern int num_arrays;
extern void (*allocate_array_error_handler) (const char *, ...);


/* --- Prototypes --- */

#if defined(MALLOC_TRACE)

#define allocate_array(n) (_allocate_array(n, __FILE__ "::allocate_array", __LINE__))
#define allocate_array_unlimited(n) (_allocate_array_unlimited(n, __FILE__ "::allocate_array", __LINE__))
#define allocate_uninit_array(n) (_allocate_array(n, __FILE__ "::allocate_uninit_array", __LINE__))
#define implode_string(a,d) (arr_implode_string(a,d, __FILE__ "::implode_string", __LINE__))

#else

#define allocate_array(n)           _allocate_array(n)
#define allocate_array_unlimited(n) _allocate_array_unlimited(n)
#define allocate_uninit_array(n)    _allocate_array(n)
#define implode_string(a,d)         arr_implode_string(a,d)

#endif /* MALLOC_TRACE */

extern vector_t *_allocate_array(mp_int MTRACE_DECL);
extern vector_t *_allocate_array_unlimited(mp_int MTRACE_DECL);
extern vector_t *_allocate_uninit_array(mp_int MTRACE_DECL);
extern void _free_vector(vector_t *p);
extern void free_empty_vector(vector_t *p);
extern void check_for_destr(vector_t *v);
extern string_t *arr_implode_string(vector_t *, string_t * MTRACE_DECL);
extern vector_t *explode_string(string_t *str, string_t *del);
extern vector_t *slice_array(vector_t *p, mp_int from, mp_int to);
extern vector_t *add_array(vector_t *p, vector_t *q);
extern ptrdiff_t *get_array_order (vector_t * vec );
extern vector_t *order_array (vector_t *vec);
extern long lookup_key (svalue_t *key, vector_t *vec);
extern vector_t *subtract_array(vector_t *minuend, vector_t *subtrahend);
extern Bool is_ordered(vector_t *v);
extern vector_t *intersect_array(vector_t *vec1, vector_t *vec2);
extern vector_t *join_array(vector_t *vec1, vector_t *vec2);
extern vector_t * symmetric_diff_array (vector_t *vec1, vector_t *vec2);

extern svalue_t *v_allocate(svalue_t *sp, int num_arg);
extern svalue_t *x_filter_array(svalue_t *sp, int num_arg);
extern svalue_t *v_sort_array(svalue_t *sp, int num_arg);
extern svalue_t *x_map_array(svalue_t *sp, int num_arg);
extern svalue_t *f_transpose_array(svalue_t *sp);

extern svalue_t *v_filter_objects(svalue_t *sp, int num_arg);
extern svalue_t *v_map_objects(svalue_t *sp, int num_arg);
extern svalue_t *v_unique_array(svalue_t *sp, int num_arg);

extern void set_vector_user(vector_t *p, object_t *owner);
extern long total_array_size(void);

#ifdef USE_ALISTS
extern vector_t * shrink_array (vector_t *p, mp_int n);
#endif

#if defined(GC_SUPPORT)
extern void clear_array_size (void);
extern void count_array_size (vector_t *vec);
#endif


/* --- static helper functions --- */

/* vector_t *ref_array(vector_t *a)
 *   Add another ref to array <a> and return the vector <a>.
 */
static INLINE vector_t* ref_array(vector_t *a) {
    ++a->ref;
    return a;
}

/* void free_array(vector_t *a)
 *   Subtract one ref from array <a>, and free the array fully if
 *   the refcount reaches zero.
 */
static INLINE void free_array(vector_t *a) {
    if (--(a->ref) <= 0) 
        _free_vector(a); 
}

/* p_int deref_array(vector_t *a)
 *   Subtract one ref from array <a>, but don't check if it needs to
 *   be freed. Result is the number of refs left.
 */
static INLINE p_int deref_array(vector_t *a) {
    return --a->ref;
}


#endif /* ARRAY_H__ */
