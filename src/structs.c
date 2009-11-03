/*------------------------------------------------------------------
 * Struct Implementation
 *
 *------------------------------------------------------------------
 * Structs implement an aggregate type in LPC with a fixed size,
 * which allows access to its members by their name. The LPC compiler
 * tries to resolve struct member accesses at compile time, but
 * a runtime access by name is also possible.
 *
 * In contrast to C structs (and in analogy to Oberon-2 RECORDs),
 * structs allow single inheritance as long as no member names are
 * duplicated.
 *
 * Structs are tied by the program (object) defining them: the LPC
 * compiler considers two 'struct data' definitions as different, if
 * they appear in different objects. Consequently the method to
 * use existing struct definitions is to inherit them.
 *------------------------------------------------------------------
 * Structs are implemented by three data structures: struct_type_t,
 * struct_t, and struct_member_t.
 *
 * A struct_type_t defines the structure of a struct, and every
 * struct instance carries a pointer to the struct_type_t for
 * identification. Ditto, typetests at runtime are carried out
 * using the struct_type_t pointers.
 *
 * The members descriptions held by a struct_type_t are made up
 * from struct_member_t instances.
 *
 * A struct_t finally is the actual struct instance. The driver
 * treats it like the other by-reference structures.
 *
 * The struct_type_t's are organized in a hash table, using the
 * struct_type_t's own name and the name of the defining program
 * as key. The purpose of the hash table is to overcome one disadvantage
 * of the close bond between structs and their programs: if an object
 * is re-compiled, all structs in there would be redefined and thus
 * receive new typeobjects even if their structure didn't change. An
 * accidental update of a central object like /std/types.c could
 * break a mud!
 *
 * To avoid this, the LPC compiler looks up every newly defined struct
 * in the hash table, and if it exists, checks the two definitions
 * for conformity. If they are the same, the newly defined struct type
 * is deleted, and the old struct type is reactived for the new program.
 *
 *
 * -- struct_type_t --
 *
 *   struct_type_t
 *   {
 *       struct_type_t * next;
 *       hash32_t        hash;
 * 
 *       string_t      * name;
 *       p_int           ref;
 *       struct_type_t * base;
 *       string_t      * prog_name;
 *       string_t      * unique_name;
 *       int32           prog_id;
 *       unsigned short  num_members;
 *       struct_member_t * member;
 *   }
 *
 *   .next and .hash are used to manage the struct_type_t in the hash
 *   table. .next is the link pointer, .hash is the hash created from .name
 *   and .prog_name.
 *
 *   .name is the name of the struct.
 *
 *   .ref is the number of references to the type. Every struct_t created
 *   from the type holds one reference; so do the program structures and
 *   (if used) the program argument type list.
 *
 *   .base is a counted pointer to the super-struct, or NULL if this struct
 *   does not inherit.
 *
 *   .prog_name and .prog_id are the name and ID of the program defining the
 *   struct. They are required to distinguish between identically named
 *   structs defined in different programs (even different incarnations of
 *   a program). While a new struct definition is compiled, .prog_name is
 *   NULL to identify it as incomplete 'prototype'.
 *
 *   .unique_name is created only on first request and gives a unique
 *   name for the struct composed of .name, .prog_name and .prog_id. It
 *   is used by the driver for diagnostics.
 *
 *   .num_members is the number of data members in this struct, including
 *   inherited members.
 *
 *   .member is allocated to hold the .num_member member descriptions
 *   in the order they appear in the struct.
 *
 *
 * -- struct_type_t --
 *
 *   struct_member_s
 *   {
 *       string_t * name;
 *       vartype_t  type;
 *   }
 *
 *   .name is the name of the member, .type it's compile-time type.
 *
 *
 * -- struct_t --
 *
 *   struct_s
 *   {
 *       struct_type_t * type;
 *       p_int           ref;
 *       wiz_list_t    * user;
 *       svalue_t        member[.type->num_members];
 *   }
 *
 *   .type is the pointer to the struct_type_t for this struct.
 *   .ref is the number of references to this struct instance.
 *   .user is the wizlist entry.
 *   .member are the member values.
 *
 * Concerning the struct hash table and the index calculation: the hash table
 * size is always a power of two (starts with 1 and is always doubled, if the
 * the table grows). So we use hash & (tablesize-1) for the calculation which
 * is significantly faster then % (modulo).
 *------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_STRUCTS

#include "structs.h"

#include <stdio.h>

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

static struct_type_t ** table = NULL;
  /* The hash table of all published struct types.
   */

static size_t num_types = 0;
  /* Number of published struct types in the table.
   */

static size_t table_size = 0;
  /* Number of buckets in the table.
   */

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
static INLINE hash32_t
hash2 (string_t * const pName, string_t * const pProgName)

/* Compute the hash from <pName> combined with <pProgName>.
 */

{
    hash32_t         hash;

    hash = mstr_get_hash(pName);
    hash = hash_string_chained("\n", 1, hash);
    hash = hash_string_chained(get_txt(pProgName), mstrsize(pProgName), hash);

    return hash;
} /* hash2() */

/*-------------------------------------------------------------------------*/
static struct_type_t *
find_by_type (struct_type_t * pSType)

/* Lookup the struct type <pSType> in the hash table. If it's in there,
 * return its pointer <pSType> and move it to the head of its chain.
 * If the type is not in the table, return NULL.
 */

{
    struct_type_t * this, * prev;
    size_t          ix;

    if (!table || !table_size)
        return NULL;

    ix = pSType->hash & (table_size-1);

    prev = NULL;
    this = table[ix];
    while (this != NULL)
    {
        if (this == pSType)
        {
            if (prev != NULL)
            {
                prev->next = this->next;
                this->next = table[ix];
                table[ix] = this;
            }
            break;
        }

        prev = this;
        this = this->next;
    }

    return this;
} /* find_by_type() */

/*-------------------------------------------------------------------------*/
static struct_type_t *
find_by_name (string_t * pName, string_t * pProgName, hash32_t  hash)

/* Lookup the struct type named <pName> and defined in program <pProgName>
 * in the hash table. <hash> is the hash of the two names combined.
 * If found, move it to the head of its chain and return the type pointer.
 * If not found, return NULL.
 *
 * In either case, *<pHash> is set to the hash value of the entry if != NULL.
 */

{
    struct_type_t * this, * prev;
    size_t          ix;

    if (!table || !table_size)
        return NULL;

    ix = hash & (table_size-1);

    prev = NULL;
    this = table[ix];
    while (this != NULL)
    {
        if (this->hash == hash
         && mstreq(this->name, pName)
         && mstreq(this->prog_name, pProgName)
           )
        {
            if (prev != NULL)
            {
                prev->next = this->next;
                this->next = table[ix];
                table[ix] = this;
            }
            break;
        }

        prev = this;
        this = this->next;
    }

    return this;
} /* find_by_name() */

/*-------------------------------------------------------------------------*/
static void
add_type (struct_type_t * pSType)

/* Add the struct type <pSType> to the hash table.
 * The <pSType>->hash must already been computed.
 */

{
     size_t ix;

#ifdef DEBUG
     if (find_by_type(pSType))
         fatal("struct type %s (%s %"PRId32") already in table.\n"
              , get_txt(struct_t_name(pSType))
              , get_txt(struct_t_pname(pSType))
              , struct_t_pid(pSType)
              );
#endif

     if (!table)
     {
         memsafe(table = pxalloc(sizeof(*table)), sizeof(*table)
                , "struct type hash table");
         table_size = 1;
         table[0] = pSType;
         pSType->next = NULL;
         num_types = 1;
         return;
     }

     ix = pSType->hash & (table_size-1);
     pSType->next = table[ix];
     table[ix] = pSType;

     num_types++;

     /* If chain lengths grow too much (more than 2 entries per bucket
      * on average), increase the table size
      */
     if (num_types > 2 * table_size && table_size * 2 < MAX_HASH32)
     {
         size_t new_size = 2 * table_size;
         struct_type_t ** table2;

         table2 = pxalloc(new_size * sizeof(*table2));
         if (table2)
         { 
             memset(table2, 0, new_size * sizeof(*table2));

             /* Rehash all existing entries */
             for (ix = 0; ix < table_size; ix++)
             {
                 struct_type_t * this;

                 while (NULL != (this = table[ix]))
                 {
                     size_t ix2;

                     table[ix] = this->next;
                     ix2 = this->hash & (new_size-1);

                     this->next = table2[ix2];
                     table2[ix2] = this;
                 }
             } /* for() */

             pfree(table);
             table = table2;
             table_size = new_size;
         } /* if (table2) */
     } /* if (check for rehash condition) */

} /* add_type() */

/*-------------------------------------------------------------------------*/
static void
remove_type (struct_type_t * pSType)

/* Remove the struct type <pSType> from the hash table, if it's in there.
 */

{
     size_t ix;

     if (!find_by_type(pSType))
         return;

     /* pSType is now at the head of it's chain */

     ix = pSType->hash & (table_size-1);
     table[ix] = pSType->next;

     num_types--;
} /* remove_type() */

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
        pSType->prog_name = NULL;
        pSType->unique_name = NULL;
        pSType->hash = 0;
        pSType->prog_id = 0;
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
                      , string_t        *prog_name
                      , int32            prog_id
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
    if (type->prog_name != NULL)
        fatal("Non-prototype typeobject passed to struct_fill_prototype().\n");
#endif

    if (num_members != 0)
        pMembers = xalloc(STRUCT_TYPE_MEMBER_MEMSIZE(num_members));
    else
        pMembers = NULL;

    if (num_members == 0 || pMembers != NULL)
    {
        type->prog_name = prog_name;
        type->next = NULL;
        type->hash = hash2(type->name, type->prog_name);
        type->prog_id = prog_id;
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
        free_mstring(prog_name);

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
                , string_t        *prog_name
                , int32            prog_id
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
        pSType = struct_fill_prototype(pSType, prog_name, prog_id, base, num_members, member);

    return pSType;
} /* struct_new_type() */

/*-------------------------------------------------------------------------*/
struct_type_t *
struct_lookup_type ( struct_type_t * pSType )

/* Lookup the hash table if a struct type with the same name and program
 * name as <pSType> has been published already.
 * Return the (uncounted) pointer to the found type, or NULL if not found.
 */

{
#ifdef DEBUG
    if (pSType == NULL)
        fatal("NULL typeobject pointer passed to struct_lookup_type().\n");
    if (pSType->prog_name == NULL)
        fatal("prototype typeobject passed to struct_lookup_type().\n");
#endif

    return find_by_name(pSType->name, pSType->prog_name, pSType->hash);
} /* struct_lookup_type() */

/*-------------------------------------------------------------------------*/
void
struct_publish_type ( struct_type_t * pSType )

/* Add the struct type <pSType> to the hash table ("publish it"), replacing
 * an existing entry if necessary.
 * It is safe to publish the same type multiple times.
 */

{
    struct_type_t * old;

#ifdef DEBUG
    if (pSType == NULL)
        fatal("NULL typeobject pointer passed to struct_publish_type().\n");
    if (pSType->prog_name == NULL)
        fatal("prototype typeobject passed to struct_publish_type().\n");
#endif

    old = find_by_name(pSType->name, pSType->prog_name, pSType->hash);
    if (old)
        remove_type(old);
    add_type(pSType);
} /* struct_publish_type() */

/*-------------------------------------------------------------------------*/
Bool
struct_type_equivalent (struct_type_t * pSType1, struct_type_t *pSType2)

/* Check if the two types <pSType1> and <pSType2> are of equivalent
 * structure.
 */

{
    int num;

    if (pSType1 == pSType2)
        return MY_TRUE;
    if (!mstreq(struct_t_name(pSType1), struct_t_name(pSType2))
     || !mstreq(struct_t_pname(pSType1), struct_t_pname(pSType2))
     || pSType1->base != pSType2->base
       )
        return MY_FALSE;
    if (struct_t_pid(pSType1) == struct_t_pid(pSType2))
        return MY_TRUE;

    /* The basics match, now check the members */
    if (pSType1->num_members != pSType2->num_members)
        return MY_FALSE;

    for (num = 0; num < pSType1->num_members; num++)
    {
        struct_member_t * pMember1 = &pSType1->member[num];
        struct_member_t * pMember2 = &pSType2->member[num];
        if (!mstreq(pMember1->name, pMember2->name)
         || pMember1->type.type != pMember2->type.type
           )
            return MY_FALSE;

        /* For structs members, a deep comparison is necessary.
         */
        if ((pMember1->type.type & PRIMARY_TYPE_MASK) == TYPE_STRUCT)
        {
            Bool rc;
            vartype_t t_member1 = pMember1->type;
            vartype_t t_member2 = pMember2->type;

            /* Prevent recursion by blocking out the structs */
            pMember1->type.type = TYPE_NUMBER;
            pMember2->type.type = TYPE_NUMBER;

            rc = struct_type_equivalent( t_member1.t_struct
                                       , t_member2.t_struct
                                       );

            /* Restore the original member types */
            pMember1->type = t_member1;
            pMember2->type = t_member2;

            if (!rc)
                return MY_FALSE;
        }
    }

    return MY_TRUE;
} /* struct_type_equivalent() */

/*-------------------------------------------------------------------------*/
void
struct_type_update ( struct_type_t * pSType
                   , struct_type_t * pOld
                   , struct_type_t * pNew)

/* In struct type <pSType>, replace all references to <pOld> by <pNew>.
 */

{
    int num;

    if (pOld == pNew)
        return;

    if (pSType->base == pOld)
    {
        free_struct_type(pSType->base);
        pSType->base = ref_struct_type(pNew);
    }

    for (num = 0; num < pSType->num_members; num++)
    {
        struct_member_t * pMember = &pSType->member[num];

        if ((pMember->type.type & PRIMARY_TYPE_MASK) == TYPE_STRUCT
         && pMember->type.t_struct == pOld
           )
        {
            free_struct_type(pMember->type.t_struct);
            pMember->type.t_struct = ref_struct_type(pNew);
        }
    }
} /* struct_type_update() */

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
                           , 0
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
        // the type was not referenced by struct_new() in this case, so it must
        // not be freed.
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
            debug_message("(%s:%d) Out of memory (%zu bytes) for member name\n"
                         , __FILE__, __LINE__, strlen(buf)
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
 * data. However, the refcount of <pStruct>->type is not changed.
 * The refcount of <pStruct> is ignored, as all known callers take care
 * of it (in particular a swap during GC leaves behind strange values).
 */

{
#ifdef DEBUG
    if (!pStruct)
        fatal("NULL pointer passed to struct_free_empty().\n");

    if (!pStruct->user)
        fatal("No wizlist pointer for struct in struct_free_empty().");
#endif

    num_struct--;
    size_struct -= STRUCT_MEMSIZE(pStruct);
    pStruct->user->struct_total -= STRUCT_MEMSIZE(pStruct);

    /* Don't free_struct_type(pStruct->type) */

    xfree(pStruct);
} /* struct_free_empty() */

/*-------------------------------------------------------------------------*/
void
struct_free (struct_t *pStruct)

/* Free the struct <pStruct> and all referenced data.
 */

{
    unsigned short num;
    struct_type_t * pSType;

#ifdef DEBUG
    if (!pStruct)
        fatal("NULL pointer passed to struct_free().\n");

    if (!pStruct->user)
        fatal("No wizlist pointer for struct in struct_free().");

    if (pStruct->ref != 0)
        fatal("Struct with %"PRIdPINT" refs passed to struct_free().\n"
              , pStruct->ref);
#endif

    for (num = struct_size(pStruct); num-- > 0; )
    {
        free_svalue(&pStruct->member[num]);
    }

    pSType = pStruct->type;
    struct_free_empty(pStruct); /* needs a valid .type */
    free_struct_type(pSType);
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
        fatal("struct typeobject with %"PRIdPINT" refs passed to struct_free_type().\n"
             , pSType->ref);
#endif

    remove_type(pSType); /* In case it was published */

    num_struct_type--;
    size_struct_type -=   STRUCT_TYPE_MEMSIZE
                        + STRUCT_TYPE_MEMBER_MEMSIZE(pSType->num_members);

    free_mstring(pSType->name);
    if (pSType->prog_name)
        free_mstring(pSType->prog_name);
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
        strbuf_addf(sbuf, "Structs:\t\t\t%8"PRIdMPINT" %9"PRIdMPINT
                          " (%"PRIdMPINT" types: %"PRIdMPINT")\n"
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

/*-------------------------------------------------------------------------*/
string_t *
struct_t_unique_name (struct_type_t *pSType)

/* Also aliased to: struct_unique_name(struct_t *)
 *
 * Compose and return the unique name of struct type <pSType>.
 * The returned string reference is not counted.
 *
 * The value save/restore routines rely on the format of this string.
 */

{
    char name[MAXPATHLEN+256];

    if (pSType->unique_name)
        return pSType->unique_name;

    snprintf(name, sizeof(name), "%s %s #%"PRId32
                , get_txt(struct_t_name(pSType))
                , get_txt(struct_t_pname(pSType))
                , struct_t_pid(pSType)
           );
    pSType->unique_name = new_mstring(name);

    return pSType->unique_name;
} /* struct_t_unique_name() */

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
        if (pSType->prog_name)
            pSType->prog_name->info.ref = 0;
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
        {
            clear_ref_in_vector(pStruct->member, struct_size(pStruct));
        }
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
        if (pSType->prog_name)
            count_ref_from_string(pSType->prog_name);
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
        {
            count_ref_in_vector(pStruct->member, struct_size(pStruct));
        }
    }
} /* count_struct_ref() */

/*-------------------------------------------------------------------------*/
void
clear_tabled_struct_refs (void)

/* Clear all references held by the struct types in the hash table.
 */

{

    if (table && table_size)
    {
        size_t num;

        for (num = 0; num < table_size; num++)
        {
            struct_type_t * pSType;
            for (pSType = table[num]; pSType != NULL; pSType = pSType->next)
                clear_struct_type_ref(pSType);
        }
    }
} /* clear_tabled_struct_refs() */

/*-------------------------------------------------------------------------*/
void
remove_unreferenced_structs (void)

/* Free all structs in the table which are not marked as referenced.
 * This function must be called before freeing all unreferenced strings!
 */

{
    size_t num;

    if (!table || !table_size)
        return;
    
    for (num = 0; num < table_size; num++)
    {
        struct_type_t * this, * prev;

        for (prev = NULL, this = table[num]; this != NULL; )
        {
            if (!test_memory_reference(this))
            {
                prev = this;
                this = this->next;
            }
            else
            {
                struct_type_t * next = this->next;
                int i;

                if (prev)
                    prev->next = next;
                else
                    table[num] = next;

                num_types--;
                
                /* Now we deallocate the memory for all the struct type
                 * structure.  We rely on the strings (member names) still
                 * being around - other referenced memory we ignore.
                 */
                num_struct_type--;
                size_struct_type -= STRUCT_TYPE_MEMBER_MEMSIZE(this->num_members);

                dprintf2(gcollect_outfd, "struct type %x '%s' was left "
                                         "unreferenced, freeing now.\n"
                                       , (p_int) this
                                       , (p_int) get_txt(this->name)
                        );

                /* Reference all strings and free them, to avoid unnecessary
                 * 'string unreferenced' diagnostics.
                 */
                count_ref_from_string(this->name);
                free_mstring(this->name);
                if (this->prog_name)
                {
                    count_ref_from_string(this->prog_name);
                    free_mstring(this->prog_name);
                }
                if (this->unique_name)
                {
                    count_ref_from_string(this->unique_name);
                    free_mstring(this->unique_name);
                }

                for (i = struct_t_size(this); i-- > 0; )
                {
                    count_ref_from_string(this->member[i].name);
                    free_mstring(this->member[i].name);
                }

                /* Reference the memory (to update its flags) and free it */
                if (this->member)
                {
                    note_malloced_block_ref(this->member);
                    xfree(this->member);
                }
                note_malloced_block_ref(this);
                xfree(this);

                this = next;
            }
        }
    } /* for (num) */

} /* remove_unreferenced_structs() */

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
 *   mixed * map(struct arr, mapping map [, int col])
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
 *    <map>[elem[,idx]]
 *
 * In the mapping case, if <map>[elem[,idx]] does not exist, the original
 * value is returned in the result.
 * [Note: argument type and range checking for idx is done in v_map()]
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
        p_int column = 0; /* mapping column to use */

        m = arg[1].u.map;

        if (num_arg > 2)
            column = arg[2].u.number;

        res = struct_new(st->type);
        if (!res)
            errorf("(map_struct) Out of memory: struct[%"PRIdMPINT"] for result\n", cnt);
        push_struct(inter_sp, res); /* In case of errors */

        for (w = st->member, x = res->member; --cnt >= 0; w++, x++)
        {
            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            v = get_map_value(m, w);
            if (v == &const0)
                assign_svalue_no_free(x, w);
            else
                assign_svalue_no_free(x, v + column);
        }

        if (num_arg > 2)
            free_svalue(arg+2);
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
            errorf("(map_struct) Out of memory: struct[%"PRIdMPINT"] for result\n", cnt);
        push_struct(inter_sp, res); /* In case of errors */

        /* Loop through arr and res, mapping the values from arr */
        for (w = st->member, x = res->member; --cnt >= 0; w++, x++)
        {
            if (current_object->flags & O_DESTRUCTED)
                continue;

            if (destructed_object_ref(w))
                assign_svalue(w, &const0);

            if (!callback_object(&cb))
                errorf("object used by map_array destructed");

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
int
struct_baseof(struct_type_t *base, struct_type_t *st)
/* Test if the struct type <base> is a base of the struct type *st
 * Results are:
 *   0: <base> is not a base of <st>, nor is <base> of equal type as <st> 
 *      (though <st> might be a base of <base>).
 *   1: <base> is a true base of <st>
 *   2: <base> and <st> are the same struct type
 */
{
    if (st == base)
        return 2;
    
    while ((st = st->base) != NULL)
    {
        if (st == base)
            return 1;
    }
    
    return 0;
    
} // baseof

/*-------------------------------------------------------------------------*/
svalue_t *
f_baseof (svalue_t *sp)

/* EFUN baseof()
 *
 *    int baseof(struct b, struct s)
 *
 * Test if the type of struct <b> is a base of struct <s> (the values of
 * <b> and <s> are irrelevant). Results are:
 *   0: <b> is not a base of <s>, nor is <b> of equal type as <s> (though <s>
 *      might be a base of <b>).
 *   1: <b> is a true base of <s>
 *   2: <b> and <s> are the same struct type
 */

{
    int rc;

    /* Get the arguments from the stack */
    rc = struct_baseof(sp[-1].u.strct->type, sp[0].u.strct->type);
    
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
    put_ref_string(&rc->item[SI_PROG_NAME], st->prog_name);
    put_number(&rc->item[SI_PROG_ID], st->prog_id);
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
            errorf("Bad arg 2 to struct_info(): illegal value %"PRIdPINT"\n"
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
