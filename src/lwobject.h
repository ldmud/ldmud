/*---------------------------------------------------------------------------
 * Lightweight objects
 *
 *---------------------------------------------------------------------------
 * These is the runtime implementation of lightweight objects.
 */

#ifndef LWOBJECT_H__
#define LWOBJECT_H__

#include "driver.h"
#include "typedefs.h"
#include "svalue.h"

/* --- Lightweight object ---
 *
 * A lightweight object tries to have a minimal memory footprint.
 */
struct lwobject_s
{
    p_int ref;            /* Reference count. */
    program_t *prog;      /* Program code.    */
    wiz_list_t *user;     /* UID.             */
    wiz_list_t *eff_user; /* EUID.            */
    svalue_t variables[]; /* The variables    */
};

extern void _free_lwobject(lwobject_t *lwob);
extern lwobject_t *create_lwobject(object_t *blueprint);
extern lwobject_t *copy_lwobject(lwobject_t *orig, bool copy_variables);

extern svalue_t *v_new_lwobject(svalue_t *sp, int num_arg);
extern svalue_t *f_lwobject_info(svalue_t *sp);
extern svalue_t *f_configure_lwobject(svalue_t *sp);

/* lwobject_t *ref_lwobject(lwobject_t *lwob)
 *   Add another ref to <lwob> and return the lwobject_t <lwob>.
 */
static INLINE lwobject_t *ref_lwobject(lwobject_t *lwob)
{
    if (lwob && lwob->ref)
        lwob->ref++;
    return lwob;
}

/* void free_lwobject(lwobject_t *lwob)
 *   Remove one ref from <lwob>, and free the lwobject fully if
 *   the refcount reaches zero.
 */
static INLINE void free_lwobject(lwobject_t *lwob)
{
    if (lwob && lwob->ref && !--(lwob->ref))
        _free_lwobject(lwob);
}

static INLINE void put_ref_lwobject(svalue_t * const dest, lwobject_t * const lwobj)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_ref_lwobject(svalue_t * const dest, lwobject_t * const lwobj)
/* Put the lightweight object <lwobj> into <dest>, which is considered empty,
 * and increment the refcount of <obj>.
 */
{
    *dest = svalue_lwobject(ref_lwobject(lwobj));
}

#define push_ref_lwobject(sp,val) put_ref_lwobject(++(sp),val)

#ifdef GC_SUPPORT

extern lwobject_t* new_sample_lwobject();
extern void free_sample_lwobject(lwobject_t* lwob);

extern void clear_lwobject_ref(lwobject_t *lwob);
extern void count_lwobject_ref(lwobject_t *lwob);

#endif /* GC_SUPPORT */

#endif /* LWOBJECT_H__ */
