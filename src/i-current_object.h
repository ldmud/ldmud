/*---------------------------------------------------------------------------
 * Helper functions to handle the current object.
 *
 *---------------------------------------------------------------------------
 */

#ifndef I_CURRENT_OBJECT__
#define I_CURRENT_OBJECT__

#include "driver.h"
#include "lwobject.h"
#include "object.h"
#include "simulate.h"

/*-------------------------------------------------------------------------*/
static INLINE object_t *
get_current_object ()

/* Returns the current object if it is indeed an object,
 * otherwise return NULL.
 */

{
    if (current_object.type == T_OBJECT)
        return current_object.u.ob;
    return NULL;
} /* get_current_object() */

/*-------------------------------------------------------------------------*/
static INLINE lwobject_t *
get_current_lwobject ()

/* Returns the current lightweight object if it is indeed a lightweight
 * object, otherwise return NULL.
 */

{
    if (current_object.type == T_LWOBJECT)
        return current_object.u.lwob;
    return NULL;
} /* get_current_lwobject() */

/*-------------------------------------------------------------------------*/
static INLINE bool
is_current_object (svalue_t ob)

/* Returns true if the given object is the same as the current object.
 */

{
    if (ob.type != current_object.type)
        return false;
    switch (ob.type)
    {
        case T_OBJECT:
            return ob.u.ob == current_object.u.ob;
        case T_LWOBJECT:
            return ob.u.lwob == current_object.u.lwob;
        default:
            return false;
    }
} /* is_current_object() */

/*-------------------------------------------------------------------------*/
static INLINE void
clear_current_object ()

/* Set current_object to 0.
 */

{
    current_object = (svalue_t){ T_NUMBER, {}, {.number = 0} };
} /* clear_current_object() */

/*-------------------------------------------------------------------------*/
static INLINE void
set_current_object (object_t *ob)

/* Sets the current object to <ob>.
 */

{
    if (!ob)
        clear_current_object();
    put_object(&current_object, ob);
} /* set_current_object() */

/*-------------------------------------------------------------------------*/
static INLINE void
set_current_lwobject (lwobject_t *lwob)

/* Sets the current object to <lwob>.
 */

{
    if (!lwob)
        clear_current_object();
    put_lwobject(&current_object, lwob);
} /* set_current_lwobject() */

/*-------------------------------------------------------------------------*/
static INLINE bool
is_current_object_destructed ()

/* Returns true if the current object is a regular object that has been
 * destructed, otherwise (for living or lightweight objects) return false.
 */

{
    if (current_object.type == T_OBJECT)
        return (current_object.u.ob->flags & O_DESTRUCTED) != 0;
    return false;
} /* is_current_object_destructed() */

/*-------------------------------------------------------------------------*/
static INLINE program_t *
get_current_object_program ()

/* Returns the program from the current object.
 */

{
    switch (current_object.type)
    {
        case T_OBJECT:
            return current_object.u.ob->prog;

        case T_LWOBJECT:
            return current_object.u.lwob->prog;

        case T_NUMBER:
            return NULL;

        default:
            fatal("Illegal type for current object.\n");
    }
} /* get_current_object_program() */

/*-------------------------------------------------------------------------*/
static INLINE svalue_t *
get_current_object_variables ()

/* Returns the variables from the current object.
 */

{
    switch (current_object.type)
    {
        case T_OBJECT:
            return current_object.u.ob->variables;

        case T_LWOBJECT:
            return current_object.u.lwob->variables;

        case T_NUMBER:
            return NULL;

        default:
            fatal("Illegal type for current object.\n");
    }
} /* get_current_object_variables() */

/*-------------------------------------------------------------------------*/
static INLINE wiz_list_t *
get_current_user ()

/* Returns the user of the current object.
 */

{
    switch (current_object.type)
    {
        case T_OBJECT:
            return current_object.u.ob->user;

        case T_LWOBJECT:
            return current_object.u.lwob->user;

        case T_NUMBER:
            return NULL;

        default:
            fatal("Illegal type for current object.\n");
    }
} /* get_current_user() */

/*-------------------------------------------------------------------------*/
static INLINE wiz_list_t *
get_current_eff_user ()

/* Returns the effective user of the current object.
 */

{
    switch (current_object.type)
    {
        case T_OBJECT:
            return current_object.u.ob->eff_user;

        case T_LWOBJECT:
            return current_object.u.lwob->eff_user;

        case T_NUMBER:
            return NULL;

        default:
            fatal("Illegal type for current object.\n");
    }
} /* get_current_user() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_object_svalue_no_free (svalue_t *dest, svalue_t ob, const char* where)

/* Writes the object <ob> into <dest> and increments the reference count.
 * This is used preferably over assign_svalue_no_free(), when destructed
 * objects should stay and not become zero.
 * <where> is used for debugging output.
 */

{
    *dest = ob;

    switch (ob.type)
    {
        case T_OBJECT:
            ref_object(ob.u.ob, where);
            break;

        case T_LWOBJECT:
            ref_lwobject(ob.u.lwob);
            break;

        case T_NUMBER:
            break;

        default:
            fatal("Illegal type for object.\n");
    }
} /* assign_object_svalue_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_object_svalue (svalue_t *dest, svalue_t ob, const char* where)

/* Writes the object <ob> into <dest> and increments the reference count.
 * The contents of <dest> will be freed before. (Contrary to assign_svalue()
 * here lvalues in <dest> will be ignored.)
 * <where> is used for debugging output.
 */

{
    switch (dest->type)
    {
        case T_OBJECT:
            free_object(dest->u.ob, where);
            break;

        case T_LWOBJECT:
            free_lwobject(dest->u.lwob);
            break;

        case T_NUMBER:
        case T_INVALID:
            break;

        default:
            free_svalue(dest);
            break;
    }

    assign_object_svalue_no_free(dest, ob, where);

} /* assign_object_svalue() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_current_object_no_free (svalue_t *dest, const char* where)

/* Writes the current object into <dest> and increments the reference count.
 * This is used preferably over assign_svalue_no_free(), when destructed
 * objects should stay and not become zero.
 * <where> is used for debugging output.
 */

{
    assign_object_svalue_no_free(dest, current_object, where);
} /* assign_current_object_no_free() */

/*-------------------------------------------------------------------------*/
static INLINE void
assign_current_object (svalue_t *dest, const char* where)

/* Writes the current object into <dest> and increments the reference count.
 * The contents of <dest> will be freed before. (Contrary to assign_svalue()
 * here lvalues in <dest> will be ignored.)
 * <where> is used for debugging output.
 */

{
    assign_object_svalue(dest, current_object, where);
} /* assign_current_object() */

/*-------------------------------------------------------------------------*/
static INLINE bool
object_svalue_eq (svalue_t a, svalue_t b)

/* Compare two object svalues and return true, if they are the same.
 */

{
    if (a.type != b.type)
        return false;
    switch (a.type)
    {
        case T_OBJECT:
            return a.u.ob == b.u.ob;
        case T_LWOBJECT:
            return a.u.lwob == b.u.lwob;
        case T_NUMBER:
            return true;
        default:
            fatal("Illegal type for object value.\n");
    }

} /* object_svalue_eq() */

/*-------------------------------------------------------------------------*/
static INLINE int
object_svalue_cmp (svalue_t a, svalue_t b)

/* Compare two object svalues and returns a value:
 * < 0: if a < b
 * = 0: if a == b
 * > 0: if a > b
 */

{
    if (a.type != b.type)
        return a.type < b.type ? -1 : 1;

    switch (a.type)
    {
        case T_OBJECT:
            return (int)(a.u.ob - b.u.ob);
        case T_LWOBJECT:
            return (int)(a.u.lwob - b.u.lwob);
        case T_NUMBER:
            return 0;
        default:
            fatal("Illegal type for object value.\n");
    }

} /* object_svalue_cmp() */

/*-------------------------------------------------------------------------*/

#define push_current_object(sp,where) assign_current_object_no_free(++(sp),(where))

#endif  /* I_CURRENT_OBJECT__ */
