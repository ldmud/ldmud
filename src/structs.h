#ifndef STRUCTS_H_
#define STRUCTS_H_ 1

#include "driver.h"

#include "typedefs.h"

#include "exec.h"
#include "hash.h"
#include "svalue.h"
#include "mstrings.h"

/* --- Types --- */

/* --- struct struct_s: a LPC struct instance
 *
 * The LPC struct contains just the actual struct data; the structure
 * of the struct is stored in a separate struct_def_t instance and linked
 * to from the data structure.
 *
 * Like arrays a LPC struct is allocated large enough to hold all member
 * svalues.
 */

struct struct_s
{
    struct_type_t * type;  /* The type object */
    p_int           ref;   /* Number of references */
    wiz_list_t    * user;  /* Who made the struct */
    svalue_t        member[1 /* .type->num_members */ ];
      /* The struct member values */
};


/* --- struct struct_member_s: description of one struct member
 */

struct struct_member_s
{
    string_t * name;  /* Tabled name of the struct member */
    lpctype_t* type;  /* The compile type of the member (see exec.h) */
};


/* --- struct struct_name_s: LPC struct name object
 *
 * All types using structs link to this name structure, as do
 * the struct type objects themselves. The name structures are organized
 * in a hash table for lookups, and thus contain the necessary structures
 * for it.
 */

struct struct_name_s
{
    struct_name_t * next;      /* Next type in hash chain (uncounted) */
    hash32_t        hash;      /* Hash value of this type */

    p_int           ref;       /* Number of references to this structure */
    string_t      * name;      /* Tabled name of the struct */
    string_t      * prog_name; /* Tabled name of the defining program. */

    lpctype_t     * lpctype;   /* The corresponding lpctype.
                                * Read via get_struct_type().
                                */
    struct_type_t * current;   /* The most current definition for this name. */
};

/* --- struct struct_type_s: LPC struct typeobject
 *
 * All programs using structs link to these typestructure, as do
 * the struct instances themselves.
 *
 * The member descriptors are allocated in a separate block to allow
 * the use of struct prototypes.
 */

struct struct_type_s
{
    p_int           ref;       /* Number of references to this structure */
    struct_name_t * name;      /* The name of the struct and its defining
                                * program.
                                */
    struct_type_t * base;      /* Counted link to the base structure,
                                * or NULL if none
                                */
    int32           prog_id;   /* ID number of the defining program */
    string_t      * unique_name; /* The unique name of this struct,
                                  * composed from .name, .prog_name and
                                  * .prog_id. It is created the first
                                  * time it is queried.
                                  */
    unsigned short  num_members;  /* Number of data members */
    struct_member_t * member;
      /* The description of the struct members, including those from the
       * base structure (if any).
       * The descriptors are in order of appearance in the struct; if
       * larger structs become the norm, we might have to think about
       * a fast name->index lookup mechanism.
       * If the struct doesn't have any members, this pointer is NULL.
       */
};

/* --- Prototypes --- */

extern struct_t * struct_new (struct_type_t *pSType);
extern struct_type_t * struct_new_prototype ( string_t *name
                                            , string_t *prog_name );
extern struct_type_t * struct_fill_prototype ( struct_type_t   *type
                                             , int32            prog_id
                                             , struct_type_t   *base
                                             , int              num_members
                                             , struct_member_t *member);
extern struct_type_t * struct_new_type ( string_t        *name
                                       , string_t        *prog_name
                                       , int32            prog_id
                                       , struct_type_t   *base
                                       , int              num_members
                                       , struct_member_t *member);
extern struct_name_t * struct_new_name (string_t *name, string_t *prog_name);
extern struct_t * struct_new_anonymous (int num_members);
extern void struct_free_empty (struct_t *pStruct);
extern void struct_free (struct_t *pStruct);
extern void struct_free_type (struct_type_t *pSType);
extern void struct_free_name (struct_name_t *pSName);
extern struct_type_t * struct_lookup_type ( struct_type_t * pSType );
extern void struct_publish_type ( struct_type_t * pSType );
extern void struct_publish_global_type ( struct_type_t * pSType );
extern Bool struct_type_equivalent (struct_type_t * pSType1, struct_type_t *pSType2);
extern void struct_type_update ( struct_type_t * pSType, struct_type_t * pOld, struct_type_t * pNew);
extern struct_type_t * struct_find (string_t *name, program_t * prog);
extern int struct_find_member ( struct_type_t * ptype, string_t * name );
extern int struct_find_direct_member ( struct_type_t * ptype, string_t * name );
extern void struct_check_for_destr ( struct_t * pStruct );
extern mp_int total_struct_size (strbuf_t *sbuf, Bool verbose);
extern void struct_driver_info(svalue_t *svp, int value) __attribute__((nonnull(1)));
extern string_t * struct_t_unique_name (struct_type_t *pSType);
#define struct_unique_name(pStruct) struct_t_unique_name(pStruct->type)
extern int struct_baseof(struct_type_t *base, struct_type_t *st) __attribute__((nonnull(1,2)));
extern struct_type_t* struct_baseof_name(struct_name_t *base, struct_type_t *st) __attribute__((nonnull(1,2)));
extern void test_efun_arg_struct_type(const char* efun_name, int pos, struct_t *pStruct, struct_type_t *pSType);
extern struct_type_t* create_std_struct_type(int std_struct_idx, lpctype_t *lpctype, const char* name, lpctype_t *member_types[], const char *member_names[]);
extern struct_type_t* get_std_struct_type(int std_struct_idx);

#ifdef GC_SUPPORT

extern void clear_struct_type_ref (struct_type_t * pSType);
extern void clear_struct_name_ref (struct_name_t * pSName);
extern void clear_struct_ref (struct_t * pStruct);
extern void clear_tabled_struct_refs (void);
extern void count_struct_type_ref (struct_type_t * pSType);
extern void count_struct_name_ref (struct_name_t * pSName);
extern void count_struct_ref (struct_t * pStruct);
extern void clear_std_struct_refs();
extern void count_std_struct_refs();
extern void remove_unreferenced_structs (void);

#endif /* GC_SUPPORT */

extern svalue_t * x_map_struct (svalue_t *sp, int num_arg);
extern svalue_t * f_struct_info(svalue_t * sp);
extern svalue_t * f_baseof(svalue_t *sp);


/* --- Static (inline) helpers --- */

/* p_int struct_ref(struct *t)
 * p_int struct_t_ref(struct_type_t *t)
 *   Return the number of references to struct(type) <t>.
 */
static INLINE p_int struct_ref(struct_t *t)
{
    return t->type->ref;
}
static INLINE p_int struct_t_ref(struct_type_t *t)
{
    return t->ref;
}

/* unsigned short struct_size(struct *t)
 * unsigned short struct_t_size(struct_type_t *t)
 *   Return the number of elements in struct(type) <t>.
 */
static INLINE short struct_size(struct_t *t)
{
    return t->type->num_members;
}
static INLINE short struct_t_size(struct_type_t *t)
{
    return t->num_members;
}

/* string_t * struct_name(struct *t)
 * string_t * struct_t_name(struct_type_t *t)
 *   Return an uncounted reference to the struct name.
 */
static INLINE string_t *struct_name(struct_t *t)
{
    return t->type->name->name;
}
static INLINE string_t *struct_t_name(struct_type_t *t)
{
    return t->name->name;
}

/* string_t * struct_pname(struct *t)
 * string_t * struct_t_pname(struct_type_t *t)
 *   Return an uncounted reference to the struct's prog_name.
 */
static INLINE string_t *struct_pname(struct_t *t)
{
    return t->type->name->prog_name;
}
static INLINE string_t *struct_t_pname(struct_type_t *t)
{
    return t->name->prog_name;
}

/* int32 struct_pid(struct *t)
 * int32 struct_t_pid(struct_type_t *t)
 *   Return the ID of the struct's definint program.
 */
static INLINE int32 struct_pid(struct_t *t)
{
    return t->type->prog_id;
}
static INLINE int32 struct_t_pid(struct_type_t *t)
{
    return t->prog_id;
}

/* struct_t *ref_struct(struct_t *t)
 *   Add another ref to struct <t> and return <t>.
 *
 * struct_type_t *ref_struct_type(struct_type_t *t)
 *   Add another ref to struct typeobject <t> and return <t>.
 *
 * struct_name_t *ref_struct_name(struct_name_t *t)
 *   Add another ref to struct name object <t> and return <t>.
 */
static INLINE struct_t *ref_struct(struct_t *t)
{
    ++(t->ref);
    return t;
}
static INLINE struct_type_t *ref_struct_type(struct_type_t *t)
{
    ++(t->ref);
    return t;
}
static INLINE struct_name_t *ref_struct_name(struct_name_t *t)
{
    ++(t->ref);
    return t;
}


/* void free_struct(struct_t *t)
 *   Subtract one ref from struct <t>, and free the struct
 *   fully if the refcount reaches zero.
 *
 * void free_struct_type(struct_type_t *t)
 *   Subtract one ref from struct typeobject <t>, and free the typeobject
 *   fully if the refcount reaches zero.
 *
 * void free_struct_name(struct_name_t *t)
 *   Subtract one ref from struct name object <t>, and free the object
 *   fully if the refcount reaches zero.
 */
static INLINE void free_struct(struct_t *t)
{
    if (--(t->ref) <= 0)
        struct_free(t);
}
static INLINE void free_struct_type(struct_type_t *t)
{
    if (--(t->ref) <= 0)
        struct_free_type(t);
}
static INLINE void free_struct_name(struct_name_t *t)
{
    if (--(t->ref) <= 0)
        struct_free_name(t);
}


/* p_int deref_struct(struct_t *t)
 *   Subtract one ref from struct <t>, but don't check if it needs to
 *   be freed. Result is the number of refs left.
 *
 * p_int deref_struct_type(struct_type_t *t)
 *   Subtract one ref from struct typeobject <t>, but don't check if it needs
 *   to be freed. Result is the number of refs left.
 *
 * p_int deref_struct_name(struct_name_t *t)
 *   Subtract one ref from struct name object <t>, but don't check if it needs
 *   to be freed. Result is the number of refs left.
 */
static INLINE p_int deref_struct(struct_t *t)
{
    return --(t->ref);
}
static INLINE p_int deref_struct_type(struct_type_t *t)
{
    return --(t->ref);
}
static INLINE p_int deref_struct_name(struct_name_t *t)
{
    return --(t->ref);
}


/* void free_struct_member_data(struct_member_t *v)
 *   Free all data associated with struct member <v>.
 */
static INLINE void free_struct_member_data(struct_member_t *v)
{
    if (v->name)
        free_mstring(v->name);
    free_lpctype(v->type);
}

static INLINE void put_ref_struct(svalue_t * const dest, struct_t * const s)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_ref_struct(svalue_t * const dest, struct_t * const s)
/* Put the struct <s> into <dest>, which is considered empty,
 * and increment the refcount of <s>.
 */
{
    *dest = svalue_struct(ref_struct(s));
}

#endif /* STRUCTS_H_ */
