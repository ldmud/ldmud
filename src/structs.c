/*------------------------------------------------------------------
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_STRUCTS

#include "structs.h"

#include "array.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "stdstrings.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"
#include "../mudlib/sys/struct_info.h"

/*-------------------------------------------------------------------------*/

#define STRUCT_TYPE_MEMSIZE \
    (sizeof(struct_type_t))
  /* Allocation memory size of a struct typeobject <t>.
   */

#define STRUCT_TYPE_MEMBER_MEMSIZE(n) \
    (sizeof(struct_member_t) * (n))
  /* Allocation memory size of a <n> member definition block.
   */

#define STRUCT_MEMSIZE(t) \
    (sizeof(struct_t) + ((t)->type->num_members - 1) * sizeof(svalue_t))
  /* Memory size of a given struct <t>.
   */

#define STRUCT_MEMSIZE_FROM_TYPE(t) \
    (sizeof(struct_t) + ((t)->num_members - 1) * sizeof(svalue_t))
  /* Memory size of struct of type <t>.
   */

/*-------------------------------------------------------------------------*/

/* --- Statistics --- */

static mp_int num_struct = 0;
static mp_int num_struct_type = 0;
  /* Number of structs and struct typeobjects.
   */

static mp_int size_struct = 0;
static mp_int size_struct_type = 0;
  /* Allocated size of structs and struct typeobjects.
   */

/*-------------------------------------------------------------------------*/
struct_t *
struct_new (struct_type_t *pSType)

/* Create a new empty struct instance for struct type <pSType>
 * and return it.
 * Return NULL when out of memory.
 *
 * The returned struct will have one reference.
 */

{
    struct_t *pStruct;
    size_t    size;

#ifdef DEBUG
    if (pSType == NULL)
        fatal("NULL typeobject pointer passed to struct_new().\n");
#endif

    size = STRUCT_MEMSIZE_FROM_TYPE(pSType);
    pStruct = xalloc(size);
    if (pStruct != NULL)
    {
        unsigned short  num;
        svalue_t       *svp;

        pStruct->ref = 1;
        pStruct->type = ref_struct_type(pSType);
        if (current_object)
            pStruct->user = current_object->user;
        else
            pStruct->user = &default_wizlist_entry;
        for (num = pSType->num_members, svp = pStruct->member
            ; num-- > 0 ; svp++)
        {
            put_number(svp, 0);
        }

        num_struct++;
        size_struct += size;
        pStruct->user->struct_total += size;
    }

    return pStruct;
} /* struct_new() */

/*-------------------------------------------------------------------------*/
struct_type_t *
struct_new_prototype ( string_t *name )

/* Create a new prototype struct typeobject from the given data.
 * The references from the data are adopted, and the result is the
 * new typeobject with one reference.
 * When an error occurs, NULL is returned and the input data is freed.
 */

{
    struct_type_t * pSType;

    pSType = xalloc(STRUCT_TYPE_MEMSIZE);
    if (pSType != NULL)
    {
        pSType->ref = 1;
        pSType->name = name;
        pSType->unique_name = NULL;
        pSType->num_members = 0;
        pSType->member = NULL;
        pSType->base = NULL;

        num_struct_type++;
        size_struct_type += STRUCT_TYPE_MEMSIZE;
    }
    else
    {
        free_mstring(name);
    }

    return pSType;
} /* struct_new_prototype() */

/*-------------------------------------------------------------------------*/
struct_type_t *
struct_fill_prototype ( struct_type_t   *type
                      , string_t        *unique_name
                      , struct_type_t   *base
                      , int              num_members
                      , struct_member_t *member)

/* Complete the struct prototype <type> with the given data and return
 * its pointer. If <member> is NULL, the member entries are left empty.
 * The references from the data are adopted, and the result is the
 * new typeobject with one reference.
 * When an error occurs, NULL is returned and the input data is freed,
 * including the prototype.
 */

{
    struct_member_t * pMembers;
    unsigned short num;

#ifdef DEBUG
    if (type == NULL)
        fatal("NULL typeobject pointer passed to struct_fill_prototype().\n");
    if (type->unique_name != NULL)
        fatal("Non-prototype typeobject passed to struct_fill_prototype().\n");
#endif

    if (num_members != 0)
        pMembers = xalloc(STRUCT_TYPE_MEMBER_MEMSIZE(num_members));
    else
        pMembers = NULL;

    if (num_members == 0 || pMembers != NULL)
    {
        type->unique_name = unique_name;
        type->base = base;
        type->num_members = num_members;

        if (member != NULL)
        {
            for (num = 0; num < num_members; num++)
            {
                pMembers[num] = member[num];
            }
        }
        else
            memset(pMembers, 0, STRUCT_TYPE_MEMBER_MEMSIZE(num_members));

        type->member = pMembers;

        size_struct_type += STRUCT_TYPE_MEMBER_MEMSIZE(num_members);
    }
    else
    {
        free_mstring(type->name);
        free_mstring(unique_name);

        if (base)
            free_struct_type(base);

        if (member != NULL)
        {
            for (num = 0; num < num_members; num++)
            {
                free_struct_member_data(&member[num]);
            }
        }

        xfree(type);

        num_struct_type--;
        size_struct_type -= STRUCT_TYPE_MEMSIZE;
    }

    return type;
} /* struct_fill_prototype() */

/*-------------------------------------------------------------------------*/
struct_type_t *
struct_new_type ( string_t        *name
                , string_t        *unique_name
                , struct_type_t   *base
                , int              num_members
                , struct_member_t *member)

/* Create a new struct typeobject from the given data and return
 * its pointer. If <member> is NULL, the member entries are left empty.
 * The references from the data are adopted, and the result is the
 * new typeobject with one reference.
 * When an error occurs, NULL is returned and the input data is freed.
 */

{
    struct_type_t * pSType;

    pSType = struct_new_prototype(name);
    if (pSType != NULL)
        pSType = struct_fill_prototype(pSType, unique_name, base, num_members, member);

    return pSType;
} /* struct_new_type() */

/*-------------------------------------------------------------------------*/
struct_t *
struct_new_anonymous (int num_members)

/* Create an empty anonymous struct instance with <num_members>
 * and return its pointer.
 * Return NULL when out of memory.
 *
 * The returned struct will have one reference.
 */

{
    struct_type_t * pType;
    struct_t      * pStruct;
    int             i;
    char            buf[100];
    Bool            gotError;

    (void) ref_mstring(STR_ANONYMOUS);
    (void) ref_mstring(STR_ANONYMOUS);
    pType = struct_new_type( STR_ANONYMOUS
                           , STR_ANONYMOUS
                           , NULL, num_members, NULL);
    if (pType == NULL)
        return NULL;

    pStruct = struct_new(pType);
    free_struct_type(pType);
      /* struct_new() added one ref, but since this is an anonymous
       * struct, the struct instance will hold the only ref.
       */

    if (pStruct == NULL)
    {
        free_struct_type(pType);
        return NULL;
    }


    /* Create default members */
    gotError = MY_FALSE;
    for (i = 0; i < num_members; i++)
    {
        sprintf(buf, "m-%d", i);
        pType->member[i].name = new_tabled(buf);
        if (!pType->member[i].name)
        {
            debug_message("(%s:%d) Out of memory (%lu bytes) for member name\n"
                         , __FILE__, __LINE__, (unsigned long)strlen(buf)
                         );
            gotError = MY_TRUE;
            break;
        }
        pType->member[i].type.type = TYPE_ANY;
        pType->member[i].type.t_struct = NULL;
    }

    if (gotError)
    {
        free_struct(pStruct);
        pStruct = NULL;
    }

    return pStruct;
} /* struct_new_anonymous() */

/*-------------------------------------------------------------------------*/
void
struct_free_empty (struct_t *pStruct)

/* Free the struct <pStruct> (which holds no valid svalues) and all referenced
 * data.
 */

{
#ifdef DEBUG
    if (!pStruct)
        fatal("NULL pointer passed to struct_free_empty().\n");

    if (!pStruct->user)
        fatal("No wizlist pointer for struct in struct_free_empty().");

    if (pStruct->ref != 0)
        fatal("Struct with %ld refs passed to struct_free_empty().\n"
              , pStruct->ref);
#endif

    num_struct--;
    size_struct -= STRUCT_MEMSIZE(pStruct);
    pStruct->user->struct_total -= STRUCT_MEMSIZE(pStruct);

    free_struct_type(pStruct->type);

    xfree(pStruct);
} /* struct_free_empty() */

/*-------------------------------------------------------------------------*/
void
struct_free (struct_t *pStruct)

/* Free the struct <pStruct> and all referenced data.
 */

{
    unsigned short num;

#ifdef DEBUG
    if (!pStruct)
        fatal("NULL pointer passed to struct_free().\n");

    if (!pStruct->user)
        fatal("No wizlist pointer for struct in struct_free().");

    if (pStruct->ref != 0)
        fatal("Struct with %ld refs passed to struct_free().\n"
              , pStruct->ref);
#endif

    for (num = struct_size(pStruct); num-- > 0; )
    {
        free_svalue(&pStruct->member[num]);
    }

    struct_free_empty(pStruct);
} /* struct_free() */

/*-------------------------------------------------------------------------*/
void
struct_free_type (struct_type_t *pSType)

/* Free the struct typeobject <pSType> and all referenced data.
 */

{
    unsigned short num;

#ifdef DEBUG
    if (!pSType)
        fatal("NULL pointer passed to struct_free_type().\n");

    if (pSType->ref != 0)
        fatal("struct typeobject with %ld refs passed to struct_free_type().\n"
             , pSType->ref);
#endif

    num_struct_type--;
    size_struct_type -=   STRUCT_TYPE_MEMSIZE
                        + STRUCT_TYPE_MEMBER_MEMSIZE(pSType->num_members);

    free_mstring(pSType->name);
    if (pSType->unique_name)
        free_mstring(pSType->unique_name);
    if (pSType->base)
        free_struct_type(pSType->base);

    for (num = 0; num < pSType->num_members; num++)
    {
        free_struct_member_data(&pSType->member[num]);
    }

    if (pSType->member)
        xfree(pSType->member);

    xfree(pSType);
} /* struct_free_type() */

/*-------------------------------------------------------------------------*/
struct_type_t *
struct_find ( string_t * name, program_t * prog )

/* Find struct <name> in the given program <prog> and return it's type
 * pointer.
 * Return NULL if not found.
 */

{
    int i;

    for (i = 0; i < prog->num_structs; i++)
    {
        if (mstreq(prog->struct_defs[i].type->name, name))
            return prog->struct_defs[i].type;
    }

    return NULL;
} /* struct_find() */

/*-------------------------------------------------------------------------*/
int
struct_find_member ( struct_type_t * ptype, string_t * name )

/* Find the struct member <name> for struct <ptype> and return its index.
 * Return -1 if not found.
 */

{
    int member;
    struct_member_t * pmember;

    if (struct_t_size(ptype) < 1)
        return -1;

    for (member = 0, pmember = ptype->member
        ; member < struct_t_size(ptype)
        ; member++, pmember++
        )
    {
        if (mstreq(pmember->name, name))
            return member;
    }

    return -1;
} /* struct_find_member() */

/*-------------------------------------------------------------------------*/
void
struct_check_for_destr ( struct_t * pStruct )

/* Remove all references to destructed objects from <pStruct>.
 */

{
    int member, num_members;
    svalue_t * svp;

    num_members = struct_size(pStruct);

    for (member = 0, svp = pStruct->member
        ; member < num_members
        ; member++, svp++
        )
    {
        if (destructed_object_ref(svp))
        {
            free_svalue(svp);
            put_number(svp, 0);
        }
    }
} /* struct_check_for_destr() */

/*-------------------------------------------------------------------------*/
mp_int
total_struct_size (strbuf_t *sbuf, Bool verbose)

/* Add the struct handler status suitable for printing to <sbuf>.
 * Result is the amount of memory held by the struct handler.
 */

{
    if (!verbose)
    {
        strbuf_addf(sbuf, "Structs:\t\t\t%8ld %9ld (%ld types: %ld)\n"
                        , num_struct, size_struct
                        , num_struct_type, size_struct_type
                   );
    }

    return size_struct + size_struct_type;
} /* total_struct_size() */

/*-------------------------------------------------------------------------*/
void
struct_dinfo_status (svalue_t *svp, int value)

/* Return the struct information for debug_info(DINFO_DATA, DID_STATUS).
 * <svp> points to the svalue block for the result, this function fills in
 * the spots for the object table.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */

{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    ST_NUMBER(DID_ST_STRUCTS, num_struct);
    ST_NUMBER(DID_ST_STRUCTS_SIZE, size_struct);

    ST_NUMBER(DID_ST_STRUCT_TYPES, num_struct_type);
    ST_NUMBER(DID_ST_STRUCT_TYPES_SIZE, size_struct_type);

#undef ST_NUMBER
} /* string_dinfo_status() */

/*=========================================================================*/
/*                           GC SUPPORT                                    */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
clear_struct_type_ref (struct_type_t * pSType)

/* Clear all references held by struct typeobject <pSType>
 */

{
    unsigned short num;

    if (pSType->ref != 0)
    {
        clear_memory_reference(pSType);
        if (pSType->member)
            clear_memory_reference(pSType->member);

        pSType->ref = 0;
        pSType->name->info.ref = 0;
        if (pSType->unique_name)
            pSType->unique_name->info.ref = 0;
        if (pSType->base)
            clear_struct_type_ref(pSType->base);

        for (num = struct_t_size(pSType); num-- > 0; )
        {
            pSType->member[num].name->info.ref = 0;
            clear_vartype_ref(&pSType->member[num].type);
        }
    }

} /* clear_struct_type_ref() */

/*-------------------------------------------------------------------------*/
void
clear_struct_ref (struct_t * pStruct)

/* Clear all references held by struct <pStruct>
 */

{
    if (pStruct->ref != 0)
    {
        clear_memory_reference(pStruct);
        pStruct->ref = 0;
        clear_struct_type_ref(pStruct->type);
        if (struct_size(pStruct))
            clear_ref_in_vector(pStruct->member, struct_size(pStruct));
    }
} /* clear_struct_ref() */

/*-------------------------------------------------------------------------*/
void
count_struct_type_ref (struct_type_t * pSType)

/* Count all references held by struct typeobject <pSType>
 */

{
    unsigned short num;

    pSType->ref++;

    if (test_memory_reference(pSType))
    {
        note_malloced_block_ref(pSType);
        if (pSType->member)
            note_malloced_block_ref(pSType->member);

        count_ref_from_string(pSType->name);
        if (pSType->unique_name)
            count_ref_from_string(pSType->unique_name);
        if (pSType->base)
            count_struct_type_ref(pSType->base);

        for (num = struct_t_size(pSType); num-- > 0; )
        {
            count_ref_from_string(pSType->member[num].name);
            count_vartype_ref(&pSType->member[num].type);
        }
    }
} /* count_struct_type_ref() */

/*-------------------------------------------------------------------------*/
void
count_struct_ref (struct_t * pStruct)

/* Count all references held by struct <pStruct>
 */

{
    pStruct->ref++;
    if (test_memory_reference(pStruct))
    {
        note_malloced_block_ref(pStruct);
        count_struct_type_ref(pStruct->type);
        if (struct_size(pStruct))
            count_ref_in_vector(pStruct->member, struct_size(pStruct));
    }
} /* clear_struct_ref() */

#endif /* GC_SUPPORT */

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
svalue_t *
x_map_struct (svalue_t *sp, int num_arg)

/* EFUN map() on structs
 *
 *   mixed * map(struct arg, string func, string|object ob, mixed extra...)
 *   mixed * map(struct arg, closure cl, mixed extra...)
 *   mixed * map(struct arr, mapping map)
 *
 * Map the elements of <arr> through a filter defined by the other
 * arguments, and return an array of the elements returned by the filter.
 *
 * The filter can be a function call:
 *
 *    <obj>-><fun>(elem, <extra>...)
 *
 * or a mapping query:
 *
 *    <map>[elem]
 *
 * In the mapping case, if <map>[elem] does not exist, the original
 * value is returned in the result.
 *
 * <obj> can both be an object reference or a filename. If <ob> is
 * omitted, or neither an object nor a string, then this_object() is used.
 *
 * As a bonus, all references to destructed objects in <arr> are replaced
 * by proper 0es.
 */

{
    struct_t   *st;
    struct_t   *res;
    svalue_t   *arg;
    svalue_t   *v, *w, *x;
    mp_int      cnt;

    inter_sp = sp;
    arg = sp - num_arg + 1;

    st = arg->u.strct;
    cnt = (mp_int)struct_size(st);

    if (arg[1].type == T_MAPPING)
    {
        /* --- Map through mapping --- */

        mapping_t *m;

        if (num_arg > 2) {
            inter_sp = sp;
            error("Too many arguments to map_array()\n");
        }
        m = arg[1].u.map;

        res = struct_new(st->type);
        if (!res)
            error("(map_struct) Out of memory: struct[%ld] for result\n", cnt);
        push_struct(inter_sp, res); /* In case of errors */

        for (w = st->member, x = res->member; --cnt >= 0; w++, x++)
        {
            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            v = get_map_value(m, w);
            if (v == &const0)
                assign_svalue_no_free(x, w);
            else
                assign_svalue_no_free(x, v);
        }

        free_svalue(arg+1); /* the mapping */
        sp = arg;
    }
    else
    {
        /* --- Map through function call --- */

        callback_t  cb;
        int         error_index;

        error_index = setup_efun_callback(&cb, arg+1, num_arg-1);
        if (error_index >= 0)
        {
            vefun_bad_arg(error_index+2, arg);
            /* NOTREACHED */
            return arg;
        }
        inter_sp = sp = arg+1;
        put_callback(sp, &cb);
        num_arg = 2;

        res = struct_new(st->type);
        if (!res)
            error("(map_struct) Out of memory: struct[%ld] for result\n", cnt);
        push_struct(inter_sp, res); /* In case of errors */

        /* Loop through arr and res, mapping the values from arr */
        for (w = st->member, x = res->member; --cnt >= 0; w++, x++)
        {
            if (current_object->flags & O_DESTRUCTED)
                continue;

            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            if (!callback_object(&cb))
                error("object used by map_array destructed");

            push_svalue(w);

            v = apply_callback(&cb, 1);
            if (v)
            {
                transfer_svalue_no_free(x, v);
                v->type = T_INVALID;
            }
        }

        free_callback(&cb);
    }
    
    /* The arguments have been removed already, now just replace
     * the struct on the stack with the result.
     */
    free_struct(st);
    arg->u.strct = res; /* Keep svalue type T_STRUCT */

    return arg;
} /* x_map_struct () */

/*-------------------------------------------------------------------------*/
svalue_t *
f_baseof (svalue_t *sp)

/* EFUN baseof()
 *
 *    int baseof(struct b, struct s)
 *
 * Test if the type of struct <b> is a base of struct <s> (the values of
 * <b> and <s> are irrelevant). Results is:
 *   0: <b> is not a base of <s>, nor is <b> of equal type as <s> (though <s>
 *      might be a base of <b>).
 *   1: <b> is a true base of <s>
 *   2: <b> and <s> are the same struct type
 */

{
    struct_type_t *base, *st;
    int rc;

    /* Get the arguments from the stack */
    base = sp[-1].u.strct->type;
    st = sp[0].u.strct->type;
  
    if (st == base)
        rc = 2;
    else
    {
        rc = 0;

        while ((st = st->base) != NULL)
        {
            if (st == base)
            {
                rc = 1;
                break;
            }
        }
    }

    /* Remove the arguments and push the result */
    free_svalue(sp); sp--;
    free_svalue(sp);

    put_number(sp, rc);

    return sp;
} /* f_baseof() */

/*-------------------------------------------------------------------------*/
static vector_t *
single_struct_info (struct_type_t * st, Bool include_base)

/* Create the struct_info() result array for a single struct and return it.
 * If <include_base> is TRUE, all members defined in a possible base
 * struct are included as if they were top-level members.
 */

{
    vector_t * rc;
    size_t offset;
    size_t i;

    offset = 0;
    if (!include_base && st->base != NULL)
        offset = struct_t_size(st->base);
    rc = allocate_array(struct_t_size(st) - offset + SI_MAX);
    put_ref_string(&rc->item[SI_NAME], struct_t_name(st));
    put_ref_string(&rc->item[SI_UNIQUE_NAME], st->unique_name);
    for (i = offset; i < struct_t_size(st); i++)
    {
        vector_t * member;
        struct_member_t * pMember;

        pMember = &st->member[i];

        member = allocate_array(SIM_MAX);
        put_array(&rc->item[SI_MEMBER+i-offset], member);
        put_ref_string(&member->item[SIM_NAME], pMember->name);
        put_number(&member->item[SIM_TYPE], pMember->type.type);
        if ((pMember->type.type & PRIMARY_TYPE_MASK) == T_STRUCT)
            put_ref_string(&member->item[SIM_EXTRA]
                          , struct_t_name(pMember->type.t_struct)
                          );
    }

    return rc;
} /* single_struct_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_struct_info (svalue_t *sp)

/* EFUN struct_info()
 *
 *    mixed * struct_info(struct s, int type)
 *
 * Return an array with information about the struct <s>. The
 * type of information returned is determined by <type>.
 */

{
    struct_type_t *st;
    int rtype;
    svalue_t result;

    /* Get the arguments from the stack */
    st = sp[-1].u.strct->type;
    rtype = sp[0].u.number;
  
    /* Get the basic result information */
    put_array(&result, single_struct_info(st, rtype != SINFO_NESTED));

    if (st->base != NULL)
    {
        /* Depending on the <rtype> argument, determine the how to handle
         * the base structs (if any).
         */
        switch(rtype)
        {
        case SINFO_FLAT:
            put_ref_string(&result.u.vec->item[SI_BASE], struct_t_name(st->base));
            break;

        case SINFO_NESTED:
          {
            svalue_t *rc;

            for ( rc = &result.u.vec->item[SI_BASE], st = st->base
                ; st != NULL
                ; st = st->base, rc = &rc->u.vec->item[SI_BASE]
                )
            {
                put_array(rc, single_struct_info(st, MY_FALSE));
            }
            break;
                
          }
        default:
            free_svalue(&result);
            error("Bad arg 2 to struct_info(): illegal value %ld\n"
                 , sp->u.number);
            /* NOTREACHED */
            return sp;
        }
    }

    free_svalue(sp); sp--; /* type */
    free_svalue(sp); /* struct */

    /* Assign the result */
    transfer_svalue_no_free(sp, &result);

    return sp;
} /* f_struct_info() */

/***************************************************************************/

#endif /* USE_STRUCTS */
