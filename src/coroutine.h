/*---------------------------------------------------------------------------
 * Coroutines
 *
 *---------------------------------------------------------------------------
 * These is the runtime implementation of coroutines.
 */

#ifndef COROUTINE_H__
#define COROUTINE_H__

#include "driver.h"
#include "typedefs.h"
#include "svalue.h"

/* --- Coroutine Configuration --- */
enum
{
    CR_RESERVED_EXTRA_VALUES = 5,
        /* Reserved space for extra values in the
         * coroutine object. Must be at least 2.
         */
};

/* --- Coroutine State ---
 *
 * The current state of a coroutine.
 */
enum coroutine_state
{
    CS_RUNNING,         /* Coroutine is currently running
                         * (i.e. somewhere in the call stack).
                         */
    CS_AWAITING,        /* Coroutine is sleeping and waiting
                         * for another coroutine to finish.
                         */
    CS_SLEEPING,        /* Coroutine is just sleeping.
                         */
    CS_FINISHED,        /* Coroutine has finished,
                         * all references should turn to zero.
                         */
};

/* --- Coroutine ---
 *
 * A coroutine encapsulates the state of a suspended function.
 *
 * Local variables and stack entries are saved in .variables[].
 * There is space for .num_variables + CR_RESERVED_EXTRA_VALUES values.
 * If more values need to be saved (.num_values > CR_RESERVED_EXTRA_VALUES),
 * .variables[.num_variables].u.lvalue will point to an
 * memory block that contains all .num_values values,
 * .variables[.num_variables+1].u.number contains the size
 * of the extra memory block.
 *
 * All waiting coroutines will form a doubly linked list. They will be
 * freed all or none. So even if the reference count of one coroutine
 * drops to zero, it will not be deallocated if it's in a list with
 * other coroutines that have a non-zero reference count. The list fields
 * (.awaiter and .awaitee) are therefore also not counted references.
 *
 * If pragma save_local_names is active, the compiler will store the names
 * of local variables at each suspension point. The number of those is
 * saved in .num_variable_names. So to get these names, you'll have to
 * look at .pc - 2*.num_variable_names to read the indices (short) into
 * the program's strings.
 */
struct coroutine_s
{
    int ref;                        /* Reference count.               */
    enum coroutine_state state;     /* State of the coroutine.        */

    svalue_t ob;                    /* The corresponding object.      */
    program_t *prog;                /* Current program.               */
    lambda_t *closure;              /* Inline closure.                */
    bytecode_p funstart;            /* Start of the function code.    */
    bytecode_p pc;                  /* Program counter.               */
    int function_index_offset;      /* Index of .prog's function block
                                     * within .ob's program.
                                     */
    int variable_index_offset;      /* Same for variables.            */
    int num_variables;              /* Number of local variables.     */
    int num_values;                 /* Number of extra values.        */
    int num_variable_names;         /* Number of variable names.      */
#ifdef DEBUG
    int num_hidden_variables;       /* Number of hidden temporary
                                     * variables (eg. in a foreach.
                                     */
#endif

    coroutine_t *awaiter;           /* The coroutine waiting for me.  */
    coroutine_t *awaitee;           /* The coroutine I'm waiting for. */
    svalue_t *last_frame;           /* Last stack frame.              */
    svalue_t variables[];           /* The variables.                 */
};

extern void _free_coroutine(coroutine_t *cr);
extern coroutine_t *create_coroutine(svalue_t *closure);
extern bool suspend_coroutine(coroutine_t *cr, svalue_t *fp);
extern coroutine_t *get_resumable_coroutine(coroutine_t* cr);
extern bool resume_coroutine(coroutine_t *cr);
extern void await_coroutine(coroutine_t *awaiter, coroutine_t *awaitee);
extern coroutine_t *finish_coroutine(coroutine_t *cr);
extern void abort_coroutine(coroutine_t *cr);
extern bool valid_coroutine(coroutine_t *cr);
extern string_t* coroutine_to_string(coroutine_t *cr);

/* coroutine_t *ref_coroutine(coroutine_t *cr)
 *   Add another ref to <cr> and return the coroutine_t <cr>.
 */
static INLINE coroutine_t *ref_coroutine(coroutine_t *cr)
{
    if (cr && cr->ref >= 0)
        cr->ref++;
    return cr;
}

/* void free_coroutine(coroutine_t *cr)
 *   Remove one ref from <cr>, and free the coroutine fully if
 *   the refcount reaches zero.
 */
static INLINE void free_coroutine(coroutine_t *cr)
{
    if (cr && cr->ref && !--(cr->ref))
        _free_coroutine(cr);
}

static INLINE void put_ref_coroutine(svalue_t * const dest, coroutine_t * const cr)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_ref_coroutine(svalue_t * const dest, coroutine_t * const cr)
/* Put the coroutine <cr> into <dest>, which is considered empty,
 * and increment the refcount of <cr>.
 */
{
    *dest = svalue_coroutine(ref_coroutine(cr));
}

static INLINE void put_ref_valid_coroutine(svalue_t * const dest, coroutine_t * const cr)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_ref_valid_coroutine(svalue_t * const dest, coroutine_t * const cr)
/* Put the coroutine <cr> into <dest>, which is considered empty,
 * and increment the refcount of <cr>. If <cr> is already finished,
 * put the number 0 there instead.
 */
{
    if (valid_coroutine(cr))
        *dest = svalue_coroutine(ref_coroutine(cr));
    else
        put_number(dest, 0);
}

#define push_ref_coroutine(sp,val)       put_ref_coroutine(++(sp),val)
#define push_ref_valid_coroutine(sp,val) put_ref_valid_coroutine(++(sp),val)

#ifdef GC_SUPPORT

extern coroutine_t* new_sample_coroutine();
extern void free_sample_coroutine(coroutine_t* cr);

extern void clear_coroutine_ref(coroutine_t *cr);
extern void count_coroutine_ref(coroutine_t *cr);

#endif /* GC_SUPPORT */

#endif /* COROUTINE_H__ */
