/*---------------------------------------------------------------------------
 * LPC type handling
 *
 *---------------------------------------------------------------------------
 * Here we define LPC types. It's not to be mistaken for the runtime type
 * system which is defined in svalue.h. LPC types are used when declaring
 * a variable, function parameter or result. So they are mainly a compile
 * time structure but can be saved for use with runtime type checks.
 */

#ifndef TYPES_H__
#define TYPES_H__

#include "driver.h"
#include "typedefs.h"

typedef enum type_classes    type_classes_t;
typedef enum primary_types   primary_types_t;
typedef enum object_types    object_types_t;
typedef struct struct_info_s struct_info_t;
typedef struct object_type_s object_type_t;
typedef struct array_type_s  array_type_t;
typedef struct union_type_s  union_type_t;
typedef uint32               typeflags_t;

/* --- Type Information ---
 *
 * These constants and types are used to encode types and visibility
 * of variables and functions.
 *
 * A type consists of a type class (primary type, struct, array or union),
 * type information (depending on class). The definition of a global
 * variable also contains visibility modifiers.
 *
 * The pieces show up in these types:
 *  - lpctype:  type class + type information
 *  - fulltype: pointer to lpctype + visibility flags
 *
 * lpctype is used as a pointer to a type object. The type object is
 * dynamically allocated and refcounted. For each type there exists
 * at most one object.
 */

enum type_classes
{
    TCLASS_PRIMARY,
    TCLASS_STRUCT,
    TCLASS_OBJECT,
    TCLASS_ARRAY,
    TCLASS_UNION
};

/* --- Primary type values --- */
enum primary_types
{
    TYPE_UNKNOWN      =  0,   /* This type must be casted */
    TYPE_NUMBER       =  1,
    TYPE_STRING       =  2,
    TYPE_VOID         =  3,
    TYPE_MAPPING      =  4,
    TYPE_FLOAT        =  5,
    TYPE_ANY          =  6,   /* Will match any type */
    TYPE_CLOSURE      =  7,
    TYPE_SYMBOL       =  8,
    TYPE_QUOTED_ARRAY =  9,
    TYPE_BYTES        = 10,
    TYPE_COROUTINE    = 11,
};

/* -- Object types -- */
enum object_types
{
    OBJECT_REGULAR,
    OBJECT_LIGHTWEIGHT
};

struct struct_info_s
{
    /* The name of the struct (refcounted).
     * This is the identifying and constant element.
     * If it is NULL it means 'any struct'.
     */
    struct_name_t *name;

    /* The current struct definition (not refcounted).
     * This is only valid at compile time and
     * NULL otherwise.
     */
    struct_type_t *def;
};

struct object_type_s
{
    /* The program name of the object (refcounted).
     * It has a leading '/' and trailing '.c'.
     * If it is NULL it means 'any object'.
     */
    string_t *program_name;

    /* Next entry in the object type table.
     */
    lpctype_t *next;

    /* The type of object.
     */
    object_types_t type;
};

struct array_type_s
{
    /* The type of the array's element.
     */
    lpctype_t *element;

    /* The base type (same as element iff depth == 1)
     * (Not refcounted, already done via .element chain.)
     */
    lpctype_t *base;

    /* The array depth.
     */
    p_int depth;
};

/* A union of types is represented with a list of lpctype_t.
 * To ensure uniqueness the member types are ordered by their address:
 * The .member address is always larger than the types in .head.
 */
struct union_type_s
{
    /* The type representing all the types of this union except
     * the last one (the one with the greatest pointer).
     * If there is more than one type left, it's a TCLASS_UNION again.
     */
    lpctype_t *head;

    /* The last element in the union.
     */
    lpctype_t *member;

    /* The next TCLASS_UNIONs with the same head but another .member.
     * (Not refcounted.)
     */
    lpctype_t *next;
};

/* --- struct lpctype_s: Basic lpc type information
 *
 * This structure holds information about a type of the lpc language.
 * For each distinct lpc type there is exactly one lpctype_s object, so
 * two types are equal if and only if the addresses of their lpctype_s
 * structures are the same.
 *
 * For matching two types there additional rules besides equality:
 * TYPE_ANY matches any type. TCLASS_STRUCT with t_struct == NULL
 * is an arbitrary struct that matches any struct (only for internal
 * use). TCLASS_UNION matches any subset of its members.
 *
 * lpctype_s doesn't hold any visibility information.
 */

struct lpctype_s
{
    p_int ref;

    struct
    {
        type_classes_t t_class  : 31;
        bool           t_static :  1; /* Resides in static memory. */
    };

    union
    {
        primary_types_t  t_primary;     /* TCLASS_PRIMARY */
        struct_info_t    t_struct;      /* TCLASS_STRUCT */
        object_type_t    t_object;      /* TCLASS_OBJECT */
        array_type_t     t_array;       /* TCLASS_ARRAY */
        union_type_t     t_union;       /* TCLASS_UNION */
    };

    lpctype_t *array_of; /* Not refcounted, the type that is the array
                          * of this type (NULL if the type isn't used).
                          */

    lpctype_t *unions_of; /* A list of TCLASS_UNIONs that have us as
                           * their head (not refcounted).
                           */
};


/* Type flags. */
enum type_flags
{
    /* Modifiers: */
    TYPE_MOD_REFERENCE        = 0x00000001,    /* Reference to a type */
    TYPE_MOD_LITERAL          = 0x00000002,    /* A literal value. */
    TYPE_MOD_LVALUE           = 0x00000004,    /* This is only used in efun_arg_types[]
                                                * to automatically generate lvalues.
                                                */
    TYPE_MOD_MASK             = 0x00000007,    /* All non-visibility modifiers. */

    /* Flag set in virtual variables, also interpreted as offset
     * in the variable index for virtual variables. */
    VIRTUAL_VAR_TAG           = 0x00004000,

    /* From function_flags the following may apply:
     *   TYPE_MOD_STATIC      = 0x40000000,  ** Static function or variable    **
     *   TYPE_MOD_NO_MASK     = 0x20000000,  ** The nomask => not redefineable **
     *   TYPE_MOD_PRIVATE     = 0x10000000,  ** Can't be inherited             **
     *   TYPE_MOD_PUBLIC      = 0x08000000,  ** Force inherit through private  **
     *   TYPE_MOD_VARARGS     = 0x04000000,  ** Used for type checking         **
     *   VAR_INITIALIZED      = 0x04000000,  ** Variable is not shared         **
     *   TYPE_MOD_VIRTUAL     = 0x02000000,  ** can be re- and cross- defined  **
     *   TYPE_MOD_PROTECTED   = 0x01000000,  ** cannot be called externally    **
     *   TYPE_MOD_XVARARGS    = 0x00800000,  ** accepts optional arguments     **
     *   TYPE_MOD_NOSAVE      = 0x00400000,  ** vars: can't be saved           **
     *   TYPE_MOD_DEPRECATED  = 0x00100000,  ** lfun is marked deprecated      **
     *   NAME_HIDDEN          = 0x00020000,  ** Not visible for inheritance    **
     */
};


/* --- struct fulltype_s: Full type information
 *
 * This structure holds the pointer to the type object and all flags
 * for type modifiers and visibility.
 *
 * The modifiers are used for
 * 1) global variables (all except TYPE_MOD_REFERENCE)
 * 2) compilation of expressions (only TYPE_MOD_REFERENCE)
 * 3) parsing of function argument definitions (only TYPE_MOD_VARARGS)
 */
struct fulltype_s
{
    lpctype_t      *t_type;
    typeflags_t     t_flags;
};

/* Basic types */
extern lpctype_t *lpctype_int, *lpctype_string, *lpctype_bytes,
                 *lpctype_mapping, *lpctype_float, *lpctype_mixed,
                 *lpctype_closure, *lpctype_symbol, *lpctype_coroutine,
                 *lpctype_quoted_array,
                 *lpctype_any_struct, *lpctype_any_object,
                 *lpctype_any_lwobject, *lpctype_void, *lpctype_unknown;

/* For use in initializers */
extern lpctype_t _lpctype_int, _lpctype_string, _lpctype_bytes,
                 _lpctype_mapping, _lpctype_float, _lpctype_mixed,
                 _lpctype_closure, _lpctype_symbol, _lpctype_coroutine,
                 _lpctype_quoted_array,
                 _lpctype_any_struct, _lpctype_any_object,
                 _lpctype_any_lwobject, _lpctype_void, _lpctype_unknown;


extern lpctype_t *get_struct_type(struct_type_t* def);
extern lpctype_t *get_object_type(string_t* prog);
extern lpctype_t *get_lwobject_type(string_t* prog);
extern lpctype_t *get_array_type(lpctype_t *element);
extern lpctype_t *get_array_type_with_depth(lpctype_t *element, int depth);
extern lpctype_t *get_union_type(lpctype_t *head, lpctype_t* member);
extern lpctype_t *get_common_type(lpctype_t *t1, lpctype_t* t2);
extern bool has_common_type(lpctype_t *t1, lpctype_t* t2);

extern void make_static_type(lpctype_t *src, lpctype_t *dest);
extern void update_struct_type(lpctype_t *t, struct_type_t *def);
extern void clean_struct_type(lpctype_t *t);
extern void _free_lpctype(lpctype_t *t);
extern bool lpctype_contains(lpctype_t* src, lpctype_t* dest);
extern bool is_compatible_object(object_t *ob, lpctype_t *t);
extern bool is_compatible_lwobject(lwobject_t *ob, lpctype_t *t);
extern int get_type_compat_int(lpctype_t *t);
extern void types_driver_info(svalue_t *svp, int value) __attribute__((nonnull(1)));

/* bool is_type_struct(lpctype_t *t)
 *   Returns true, iff <t> represents a specific or any struct type.
 */
static INLINE bool is_type_struct(lpctype_t *t)
{
    if(t == lpctype_unknown || t == NULL)
        return false;

    return lpctype_contains(t, lpctype_any_struct);
}

/* bool is_type_any(lpctype_t *t)
 *   Returns true, iff <t> represents all types.
 */
static INLINE bool is_type_any(lpctype_t *t)
{
    return t == lpctype_mixed;
}

/* lpctype_t *ref_lpctype(lpctype_t *t)
 *   Add another ref to <t> and return the lpctype_t <t>.
 */
static INLINE lpctype_t *ref_lpctype(lpctype_t *t)
{
    if (t && t->ref)
        ++t->ref;
    return t;
}

/* void free_lpctype(lpctype_t *t)
 *   Remove one ref from <t>, and free the lpctype fully if
 *   the refcount reaches zero.
 */
static INLINE void free_lpctype(lpctype_t *t)
{
    if (t && t->ref && !--(t->ref))
        _free_lpctype(t);
}

/* fulltype_t& ref_fulltype(fulltype_t &t)
 *   Add another ref to <t>.
 * void free_lpctype(fulltype_t &t)
 *   Remove one ref from <t>.
 */
#define ref_fulltype(t) (ref_lpctype((t).t_type),(t))
#define free_fulltype(t) free_lpctype((t).t_type)

/* fulltype_t get_fulltype(lpctype_t *t)
 *   Given an lpctype_t return a fulltype_t
 *   structure with <t> in it. No ref is added.
 */
static INLINE fulltype_t get_fulltype(lpctype_t *t)
{
    return ((fulltype_t) { .t_type = t, .t_flags = 0 });
}

/* fulltype_t get_fulltype_flags(lpctype_t *t, typeflags_t f)
 *   Given an lpctype_t return a fulltype_t
 *   structure with <t> and <f> in it. No ref is added.
 */
static INLINE fulltype_t get_fulltype_flags(lpctype_t *t, typeflags_t f)
{
    return ((fulltype_t) { .t_type = t, .t_flags = f });
}

#ifdef GC_SUPPORT

extern void clear_lpctype_ref (lpctype_t *t);
extern void count_lpctype_ref (lpctype_t *t);
extern void clear_object_type_table_refs();
extern void remove_unreferenced_object_types();

/* void clear_fulltype_ref(fulltype_t &t)
 *   Clear all references associated with <t>.
 * void count_fulltype_ref(fulltype_t &t)
 *   Count all references associated with <t>.
 */
#define clear_fulltype_ref(t) clear_lpctype_ref((t).t_type)
#define count_fulltype_ref(t) count_lpctype_ref((t).t_type)

#endif /* GC_SUPPORT */

#endif /* TYPES_H__ */
