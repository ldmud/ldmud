/*---------------------------------------------------------------------------
 * LPC type handling
 *
 *---------------------------------------------------------------------------
 */

#include <assert.h>
#include <wctype.h>

#include "gcollect.h"
#include "lex.h"
#include "lwobject.h"
#include "main.h"
#include "object.h"
#include "pkg-python.h"
#include "types.h"
#include "simul_efun.h"
#include "simulate.h"
#include "structs.h"
#include "xalloc.h"

#include "i-current_object.h"

#include "../mudlib/sys/driver_info.h"

/* Base types are statically allocated. */
lpctype_t _lpctype_int          = { 0, { TCLASS_PRIMARY, true }, {TYPE_NUMBER},       NULL, NULL };
lpctype_t _lpctype_string       = { 0, { TCLASS_PRIMARY, true }, {TYPE_STRING},       NULL, NULL };
lpctype_t _lpctype_mapping      = { 0, { TCLASS_PRIMARY, true }, {TYPE_MAPPING},      NULL, NULL };
lpctype_t _lpctype_float        = { 0, { TCLASS_PRIMARY, true }, {TYPE_FLOAT},        NULL, NULL };
lpctype_t _lpctype_mixed        = { 0, { TCLASS_PRIMARY, true }, {TYPE_ANY},          NULL, NULL };
lpctype_t _lpctype_closure      = { 0, { TCLASS_PRIMARY, true }, {TYPE_CLOSURE},      NULL, NULL };
lpctype_t _lpctype_symbol       = { 0, { TCLASS_PRIMARY, true }, {TYPE_SYMBOL},       NULL, NULL };
lpctype_t _lpctype_coroutine    = { 0, { TCLASS_PRIMARY, true }, {TYPE_COROUTINE},    NULL, NULL };
lpctype_t _lpctype_lpctype      = { 0, { TCLASS_PRIMARY, true }, {TYPE_LPCTYPE},      NULL, NULL };
lpctype_t _lpctype_quoted_array = { 0, { TCLASS_PRIMARY, true }, {TYPE_QUOTED_ARRAY}, NULL, NULL };
lpctype_t _lpctype_void         = { 0, { TCLASS_PRIMARY, true }, {TYPE_VOID},         NULL, NULL };
lpctype_t _lpctype_bytes        = { 0, { TCLASS_PRIMARY, true }, {TYPE_BYTES},        NULL, NULL };
lpctype_t _lpctype_unknown      = { 0, { TCLASS_PRIMARY, true }, {TYPE_UNKNOWN},      NULL, NULL };
lpctype_t _lpctype_any_struct   = { 0, { TCLASS_STRUCT,  true }, {.t_struct = {NULL, NULL}},  NULL, NULL };
lpctype_t _lpctype_any_object   = { 0, { TCLASS_OBJECT,  true }, {.t_object = {NULL, NULL, OBJECT_REGULAR}},     NULL, NULL };
lpctype_t _lpctype_any_lwobject = { 0, { TCLASS_OBJECT,  true }, {.t_object = {NULL, NULL, OBJECT_LIGHTWEIGHT}}, NULL, NULL };

/* And are used via pointer. */
lpctype_t *lpctype_int          = &_lpctype_int;
lpctype_t *lpctype_string       = &_lpctype_string;
lpctype_t *lpctype_mapping      = &_lpctype_mapping;
lpctype_t *lpctype_float        = &_lpctype_float;
lpctype_t *lpctype_mixed        = &_lpctype_mixed;
lpctype_t *lpctype_closure      = &_lpctype_closure;
lpctype_t *lpctype_symbol       = &_lpctype_symbol;
lpctype_t *lpctype_coroutine    = &_lpctype_coroutine;
lpctype_t *lpctype_lpctype      = &_lpctype_lpctype;
lpctype_t *lpctype_quoted_array = &_lpctype_quoted_array;
lpctype_t *lpctype_any_struct   = &_lpctype_any_struct;
lpctype_t *lpctype_any_object   = &_lpctype_any_object;
lpctype_t *lpctype_any_lwobject = &_lpctype_any_lwobject;
lpctype_t *lpctype_void         = &_lpctype_void;
lpctype_t *lpctype_bytes        = &_lpctype_bytes;
lpctype_t *lpctype_unknown      = &_lpctype_unknown;

static lpctype_t **object_type_table = NULL;
  /* The hash table of all used named object types.
   */

static size_t object_type_table_size = 0;
  /* Number of buckets in the table.
   */

static size_t num_object_types = 0;
  /* Number of named object types in the table.
   */

/*-------------------------------------------------------------------------*/
static lpctype_t *
lpctype_new (void)

/* Create a new lpctype_t instance.
 * .t_class and class dependant members are left uninitialized.
 */

{
    lpctype_t *type = (lpctype_t*) xalloc(sizeof(lpctype_t));
    type->ref = 1;
    type->t_static = false;
    type->array_of = NULL;
    type->unions_of = NULL;
    return type;
} /* lpctype_new() */

/*-------------------------------------------------------------------------*/
static void
lpctype_touch (lpctype_t *uniontype)

/* Reorder the union head so that we are the first element
 * of .unions_of. So afterwards the following holds:
 * uniontype->t_union.head->unions_of == uniontype
 *
 * This is done to speed up recently used lookups.
 */

{
    lpctype_t **headptr = &(uniontype->t_union.head->unions_of);
    lpctype_t **ptr = headptr;

    while (*ptr != uniontype)
        ptr = &((*ptr)->t_union.next);

    if (ptr != headptr)
    {
        *ptr = uniontype->t_union.next;
        uniontype->t_union.next = *headptr;
        *headptr = uniontype;
    }
} /* lpctype_touch() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_struct_name_type (struct_name_t* name)

/* Create an lpctype_t for struct name <name>.
 */

{
    lpctype_t *type = name->lpctype;

    if (type != NULL)
        ref_lpctype(type);
    else
    {
        name->lpctype = type = lpctype_new();
        type->t_class = TCLASS_STRUCT;
        type->t_struct.name = ref_struct_name(name);
        type->t_struct.def_idx = USHRT_MAX;
        type->t_struct.def = NULL;
    }

    return type;
} /* get_struct_name_type() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_struct_type (struct_type_t* def)

/* Create an lpctype_t around <def>, check for already
 * existing definitions.
 */

{
    lpctype_t *type = get_struct_name_type(def->name);

    type->t_struct.def = def;

    return type;
} /* get_struct_type() */

/*-------------------------------------------------------------------------*/
void
update_struct_type (lpctype_t *t, struct_type_t *def)

/* Updates the .t_struct.def field of <t> to point to <def>.
 */

{
    if (t)
        t->t_struct.def = def;

} /* update_struct_type() */

/*-------------------------------------------------------------------------*/
void
clean_struct_type (lpctype_t *t)

/* Clean the .def element in t->t_struct.
 */

{
    if(!t || t->t_class != TCLASS_STRUCT || t->t_static)
        return;

    t->t_struct.def = NULL;
} /* clean_struct_type */

/*-------------------------------------------------------------------------*/
static lpctype_t *
find_object_type (string_t *prog, object_types_t otype)

/* Lookup the object type for <prog>/<otype> in the hash table.
 * <prog> must be tabled string. If found, move it to the head of its chain
 * and return the type pointer. If not found, return NULL.
 */

{
    lpctype_t *this, *prev;
    size_t ix;

    if (!object_type_table || !object_type_table_size)
        return NULL;

    ix = (mstr_get_hash(prog) + otype) & (object_type_table_size-1);

    prev = NULL;
    this = object_type_table[ix];
    while (this != NULL)
    {
        if (this->t_object.program_name == prog && this->t_object.type == otype)
        {
            if (prev != NULL)
            {
                prev->t_object.next = this->t_object.next;
                this->t_object.next = object_type_table[ix];
                object_type_table[ix] = this;
            }
            break;
        }

        prev = this;
        this = this->t_object.next;
    }

    return this;
} /* find_object_type() */

/*-------------------------------------------------------------------------*/
static bool
add_object_type (lpctype_t *type)

/* Add the named object type <type> to the hash table.
 * Returns false when the type could not be entered into the table.
 */

{
    size_t ix;

    if (!object_type_table)
    {
        object_type_table = pxalloc(sizeof(*object_type_table));
        if (object_type_table == NULL)
            return false;

        object_type_table_size = 1;
        object_type_table[0] = type;
        type->t_object.next = NULL;

        num_object_types = 1;
        return true;
    }

    ix = (mstr_get_hash(type->t_object.program_name) + type->t_object.type) & (object_type_table_size-1);
    type->t_object.next = object_type_table[ix];
    object_type_table[ix] = type;
    num_object_types++;

    /* If we have twice the entries than the table size, we resize.
     */
    if (num_object_types > 2 * object_type_table_size && object_type_table_size * 2 <= TTABLE_SIZE)
    {
        size_t new_size = 2 * object_type_table_size;
        lpctype_t **new_table = pxalloc(new_size * sizeof(*new_table));

        if (!new_table)
            return true; /* We were successful nevertheless. */

        memset(new_table, 0, new_size * sizeof(*new_table));

        /* Rehash all existing entries */
        for (ix = 0; ix < object_type_table_size; ix++)
        {
            lpctype_t *this = object_type_table[ix];

            while (this != NULL)
            {
                lpctype_t *next = this->t_object.next;
                size_t new_ix = (mstr_get_hash(this->t_object.program_name) + this->t_object.type) & (new_size-1);

                this->t_object.next = new_table[new_ix];
                new_table[new_ix] = this;

                this = next;
            }
        }

        pfree(object_type_table);
        object_type_table = new_table;
        object_type_table_size = new_size;
     }

     return true;

} /* add_object_type() */

/*-------------------------------------------------------------------------*/
static lpctype_t *
internal_get_object_type (string_t *prog, object_types_t otype)

/* Create an lpctype_t for an (lw)object having <prog> in its program.
 * The name is normalized (removing a leading '/', check for double
 * slashes and add a trailing '.c' if it's not already there), so it
 * will match the program names easily. This name will then looked
 * up in our table, to ensure unique type objects.
 *
 * Returns NULL when out of memory.
 */

{
    const char *normalized;
    string_t *s;
    size_t len = mstrsize(prog);
    lpctype_t *result;

    assert(prog->info.unicode != STRING_BYTES);

    /* Let's normalize the filename. */
    if (!len)
        return (otype == OBJECT_REGULAR) ? lpctype_any_object : lpctype_any_lwobject;

    normalized = make_name_sane(get_txt(prog), false, true);
    if (normalized)
        s = new_tabled(normalized, prog->info.unicode);
    else
        s = make_tabled_from(prog);

    /* Now look at our table. */
    result = find_object_type(s, otype);
    if (result)
    {
        free_mstring(s);
        return ref_lpctype(result);
    }

    result = lpctype_new();
    result->t_class = TCLASS_OBJECT;
    result->t_object.program_name = s;
    result->t_object.type = otype;

    if (!add_object_type(result))
    {
        /* So free_lpctype() won't look into the table. */
        result->t_object.program_name = NULL;
        free_mstring(s);
        free_lpctype(result);

        return NULL;
    }

    return result;
} /* get_object_type() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_object_type (string_t *prog)

/* Create an lpctype_t for an object having <prog> in its program.
 *
 * Returns NULL when out of memory.
 */

{
    return internal_get_object_type(prog, OBJECT_REGULAR);
} /* get_object_type() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_lwobject_type (string_t *prog)

/* Create an lpctype_t for an lwobject having <prog> in its program.
 *
 * Returns NULL when out of memory.
 */

{
    return internal_get_object_type(prog, OBJECT_LIGHTWEIGHT);
} /* get_lwobject_type() */

/*-------------------------------------------------------------------------*/
#ifdef USE_PYTHON
lpctype_t *
get_python_type (int python_type_id)

/* Create an lpctype_t for a Python type.
 *
 * Returns NULL when out of memory.
 */

{
    lpctype_t *type = lookup_python_type(python_type_id);

    if (type == NULL)
    {
        type = lpctype_new();
        type->t_class = TCLASS_PYTHON;
        type->t_python.type_id = python_type_id;
        enter_python_type(python_type_id, type);
    }

    ref_lpctype(type);

    return type;
} /* get_python_type() */

#endif
/*-------------------------------------------------------------------------*/
static void
remove_object_type (lpctype_t *type)

/* Remove the named object type <type> from the hash table,
 * if it's in there.
 */

{
    size_t ix;

    if (type->t_object.program_name == NULL)
        return;

    if (!find_object_type(type->t_object.program_name, type->t_object.type))
         return;

    /* Now that entry should be at the top of the chain. */
    ix = (mstr_get_hash(type->t_object.program_name) + type->t_object.type) & (object_type_table_size-1);
    assert(object_type_table[ix] == type);
    object_type_table[ix] = type->t_object.next;

    num_object_types--;
} /* remove_object_type() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_array_type (lpctype_t *element)

/* Create an array type with <element> as the base type.
 */

{
    lpctype_t *type;

    if (element == NULL)
        return NULL;

    type  = element->array_of;
    if (type != NULL)
        return ref_lpctype(type);

    element->array_of = type = lpctype_new();
    type->t_class = TCLASS_ARRAY;
    type->t_array.element = ref_lpctype(element);
    if (element->t_class == TCLASS_ARRAY)
    {
        type->t_array.base = element->t_array.base;
        type->t_array.depth = element->t_array.depth + 1;
    }
    else
    {
        type->t_array.base = element;
        type->t_array.depth = 1;
    }

    return type;
} /* get_array_type() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_array_type_with_depth (lpctype_t *element, int depth)

/* Create an array whose depth is exactly <depth>.
 */

{
    lpctype_t *type;

    if (element->t_class == TCLASS_ARRAY)
    {
        if (element->t_array.depth == depth)
            return ref_lpctype(element);

        if (element->t_array.depth > depth)
            element = element->t_array.base;
        else
            depth -= element->t_array.depth;
    }

    type = ref_lpctype(element);
    for (; depth > 0; depth--)
    {
        lpctype_t *old = type;
        type = get_array_type(type);
        free_lpctype(old);
    }
    return type;
} /* get_array_type_with_depth() */

/*-------------------------------------------------------------------------*/
static lpctype_t *
make_union_type (lpctype_t *head, lpctype_t* member)

/* Create a union type from <head> and <member>, this function
 * doesn't care about orderung.
 */

{
    /* Look for the member in the union list of <head>. */
    lpctype_t *result;

    result = head->unions_of;
    while (result && result->t_union.member != member)
        result = result->t_union.next;

    if (result)
        return ref_lpctype(result);

    result = lpctype_new();
    result->t_class = TCLASS_UNION;
    result->t_union.head = ref_lpctype(head);
    result->t_union.member = ref_lpctype(member);
    result->t_union.next = head->unions_of;
    head->unions_of = result;

    return result;
} /* make_union_type() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_union_type (lpctype_t *head, lpctype_t* member)

/* Create a union type from <head> (maybe a union type) adding <member>
 * (should not be a union type). <member> is inserted at the correct
 * position. If <member> is a union type, the result is the same
 * as if get_union_type would be called for each member of this union.
 * Creating a union type with TYPE_UNKNOWN or TYPE_ANY will yield
 * TYPE_UNKNOWN resp. TYPE_ANY (and not a union).
 */

{
    lpctype_t *insert = head;
    lpctype_t *result, *next_member;

    if (member == NULL || member == lpctype_void)
        return ref_lpctype(head);
    if (head == NULL || head == lpctype_void)
        return ref_lpctype(member);
    if (head == lpctype_unknown || member == lpctype_unknown)
        return lpctype_unknown;
    if (head == lpctype_mixed || member == lpctype_mixed)
        return lpctype_mixed;

    if (member->t_class == TCLASS_UNION)
    {
        next_member = member;
        result = ref_lpctype(head);

        do
        {
            insert = result;
            result = get_union_type(result, next_member->t_union.member);
            free_lpctype(insert);

            next_member = next_member->t_union.head;
        }
        while (next_member->t_class == TCLASS_UNION);

        insert = result;
        result = get_union_type(result, next_member);
        free_lpctype(insert);

        return result;
    }

    if (lpctype_contains(member, head))
        return ref_lpctype(head);
    else if (lpctype_contains(head, member))
        return ref_lpctype(member);

    if (head->t_class == TCLASS_UNION)
    {
        // Let's go over the members in head, and see if we can remove
        // some of them because they might be contained in member.
        lpctype_t *current = head;
        while (true)
        {
            lpctype_t *elem = current->t_class == TCLASS_UNION ? current->t_union.member : current;

            if (lpctype_contains(elem, member))
            {
                // Okay, build a union type without those inferior members.
                result = ref_lpctype(member);

                while (true)
                {
                    lpctype_t *insert_elem = insert->t_class == TCLASS_UNION ? insert->t_union.member : insert;

                    if (!lpctype_contains(insert_elem, member))
                    {
                        lpctype_t *prev_result = result;
                        result = get_union_type(result, insert_elem);
                        free_lpctype(prev_result);
                    }

                    if (insert->t_class == TCLASS_UNION)
                        insert = insert->t_union.head;
                    else
                        return result;
                }
                /* NOTREACHED */
            }

            if (current->t_class == TCLASS_UNION)
                current = current->t_union.head;
            else
                break;
        }
    }

    while (insert->t_class == TCLASS_UNION && insert->t_union.member > member)
    {
        lpctype_touch(insert); /* Remember the way back in .unions_of. */
        insert = insert->t_union.head;
    }

    if (insert->t_class == TCLASS_UNION ? (insert->t_union.member == member) : (insert == member))
        return ref_lpctype(head); /* Already in there. */

    if (insert->t_class != TCLASS_UNION && insert > member)
    {
        result = make_union_type(member, insert);
        next_member = NULL;
    }
    else
    {
        next_member = insert->unions_of; /* make_union_type will change this. */
        result = make_union_type(insert, member);
    }

    while (insert != head)
    {
        if (next_member)
        {
            insert = next_member;
            next_member = NULL;
        }
        else
            insert = insert->unions_of;
        result = make_union_type(result, insert->t_union.member);
        free_lpctype(result->t_union.head);
    }

    return result;
} /* get_union_type() */

/*-------------------------------------------------------------------------*/
static lpctype_t *
internal_get_common_type(lpctype_t *t1, lpctype_t* t2, bool find_one)

/* Determine the intersection of both types.
 * Returns NULL if there is no common type.
 * If one of both types is TYPE_UNKNOWN, then
 * the result will by TYPE_UNKNOWN, too.
 *
 * If <find_one> is true, then it may finish even if only a part
 * of the result type was found (used for has_common_type()).
 */

{
    /* Hopefully the most common case. */
    if (t1 && t1 == t2)
        return ref_lpctype(t1);

    /* We can't return NULL, as this is an error condition. */
    if (t1 == NULL && t2 == NULL)
        return lpctype_mixed;
    else if (t1 == NULL)
        return ref_lpctype(t2);
    else if (t2 == NULL)
        return ref_lpctype(t1);

    if (t2->t_class == TCLASS_UNION && t1->t_class != TCLASS_UNION)
    {
        /* Switch them, so t2 is not a union unless t1 is one, too. */
        lpctype_t *temp; temp = t1; t1 = t2; t2 = temp;
    }
    /* Some shortcuts, before we're diving into t1.*/
    if (t2->t_class == TCLASS_PRIMARY)
    {
        switch (t2->t_primary)
        {
        case TYPE_UNKNOWN:
            return ref_lpctype(t2);
        case TYPE_ANY:
            return ref_lpctype(t1);
        default:
            break;
        }
    }

    switch (t1->t_class)
    {
    case TCLASS_PRIMARY:
        switch (t1->t_primary)
        {
        case TYPE_UNKNOWN:
            return ref_lpctype(t1);
        case TYPE_ANY:
            return ref_lpctype(t2);
        default:
            /* Primary types besides the above exceptions should
               be identical (checked at the beginning of this function). */
            return NULL;
        }

    case TCLASS_STRUCT:
        if (t2->t_class != TCLASS_STRUCT)
            return NULL;
        else if (t1->t_struct.name == NULL)
            return ref_lpctype(t2);
        else if (t2->t_struct.name == NULL)
            return ref_lpctype(t1);
        /* This is somewhat counterintuitive, but the derived struct
           is more specialized, so it is the result of the intersection. */
        else if (t2->t_struct.def && struct_baseof_name(t1->t_struct.name, t2->t_struct.def))
            return ref_lpctype(t2);
        else if (t1->t_struct.def && struct_baseof_name(t2->t_struct.name, t1->t_struct.def))
            return ref_lpctype(t1);
        else
            return NULL;

    case TCLASS_OBJECT:
        if (t2->t_class != TCLASS_OBJECT)
            return NULL;
        else if (t1->t_object.type != t2->t_object.type)
            return NULL;
        else if (t1->t_object.program_name == NULL)
            return ref_lpctype(t2);
        else if (t2->t_object.program_name == NULL)
            return ref_lpctype(t1);
        else
            return NULL;

#ifdef USE_PYTHON
    case TCLASS_PYTHON:
        return NULL;
#endif

    case TCLASS_ARRAY:
        if (t2->t_class != TCLASS_ARRAY)
            return NULL;
        else
        {
            lpctype_t *common_element = get_common_type(t1->t_array.element, t2->t_array.element);
            lpctype_t *result = get_array_type(common_element);
            free_lpctype(common_element);
            return result;
        }

    case TCLASS_UNION:
        {
            lpctype_t *result = NULL;
            while (true)
            {
                lpctype_t *base = t1->t_class == TCLASS_UNION ? t1->t_union.member : t1;
                lpctype_t *common_base = get_common_type(t2, base);
                lpctype_t *oldresult = result;

                if (find_one && common_base)
                    return common_base;

                result = get_union_type(result, common_base);
                free_lpctype(common_base);
                free_lpctype(oldresult);

                if (t1->t_class == TCLASS_UNION)
                    t1 = t1->t_union.head;
                else
                    break;
            }

            return result;
        }

    default:
        fatal("Unknown type class %d!\n", t1->t_class);
        return NULL;
    }
} /* internal_get_common_type() */

/*-------------------------------------------------------------------------*/
bool
has_common_type(lpctype_t *t1, lpctype_t* t2)

/* Determine whether the intersection of both types is non-empty.
 */

{
    lpctype_t *result = internal_get_common_type(t1, t2, true);
    free_lpctype(result);
    return (result != NULL);
} /* has_common_type() */

/*-------------------------------------------------------------------------*/
lpctype_t *
get_common_type(lpctype_t *t1, lpctype_t* t2)

/* Determine the intersection of both types.
 * Returns NULL if there is no common type.
 * If one of both types is TYPE_UNKNOWN, then
 * the result will by TYPE_UNKNOWN, too.
 */

{
    return internal_get_common_type(t1, t2, false);
} /* get_common_type() */

/*-------------------------------------------------------------------------*/
void
make_static_type (lpctype_t *src, lpctype_t *dest)

/* Takes the type data of <src> and moves it to <dest>, which is
 * a static memory block. Changes all pointers accordingly.
 * <src> is freed afterwards.
 */

{
    lpctype_t *t;

    assert(!src->t_static);

    *dest = *src;
    dest->t_static = true;
    dest->ref = 0;

    switch (src->t_class)
    {
    case TCLASS_PRIMARY:
        /* This shouldn't happen, as there can't be no non-static primary types. */
        break;

    case TCLASS_STRUCT:
        if (src->t_struct.name)
            src->t_struct.name->lpctype = dest;
        break;

    case TCLASS_OBJECT:
        remove_object_type(src);
        add_object_type(dest);
        break;

#ifdef USE_PYTHON
    case TCLASS_PYTHON:
        /* Can't happen, Python types are not part of internal types. */
        break;
#endif

    case TCLASS_ARRAY:
        src->t_array.element->array_of = dest;
        break;

    case TCLASS_UNION:
        {
            lpctype_t **tref = &(src->t_union.head->unions_of);
            while (*tref != NULL)
            {
                if (*tref == src)
                {
                    *tref = dest;
                    break;
                }

                tref = &((*tref)->t_union.next);
            }
            break;
        }
    }

    /* References to <src> can come also from arrays or unions of it. */
    t = src->array_of;
    while (t != NULL)
    {
        if (t->t_array.element == src)
            t->t_array.element = dest;
        if (t->t_array.base == src)
            t->t_array.base = dest;
        else
            /* If we are not a base (i.e. <src> is an array itself)
             * then there is no need to go deeper. */
            break;

        t = t->array_of;
    }

    /* Unions are more tricky, because the member list is sorted
     * by their pointer, so theoretically we have to resort every
     * union made of <src>. But for now, we assume, that they don't
     * exist at the time make_static_type() is called.
     * The same assumption could be made for arrays, but they
     * weren't difficult to handle...
     */
    assert(src->unions_of == NULL);
    xfree(src);
} /* make_static_type() */

/*-------------------------------------------------------------------------*/
void
_free_lpctype (lpctype_t *t)

/* Deallocate the vector <p>, properly freeing the contained elements.
 */

{
    /* There can't be any arrays of us,
     * otherwise our refcount wouldn't be 0.
     */
    assert(t->array_of == NULL);
    /* Same for unions. */
    assert(t->unions_of == NULL);

    switch(t->t_class)
    {
    case TCLASS_PRIMARY:
        break; /* Nothing to do. */

    case TCLASS_STRUCT:
        if (t->t_struct.name)
        {
            t->t_struct.name->lpctype = NULL;
            free_struct_name(t->t_struct.name);
        }
        break;

    case TCLASS_OBJECT:
        remove_object_type(t);
        if (t->t_object.program_name)
            free_mstring(t->t_object.program_name);
        break;

#ifdef USE_PYTHON
    case TCLASS_PYTHON:
        /* Nothing to do. */
        break;
#endif

    case TCLASS_ARRAY:
        t->t_array.element->array_of = NULL;
        free_lpctype(t->t_array.element);
        break;

    case TCLASS_UNION:
        {
            /* Remove this union from the union list of .head. */
            lpctype_t **ptr = &(t->t_union.head->unions_of);
            while(*ptr && *ptr != t)
                ptr = &((*ptr)->t_union.next);

            assert(ptr != NULL);
            *ptr = t->t_union.next;
        }
        free_lpctype(t->t_union.member);
        free_lpctype(t->t_union.head);
        break;
    }

    xfree(t);
} /* _free_lpctype() */

/*-------------------------------------------------------------------------*/
bool
lpctype_contains (lpctype_t* src, lpctype_t* dest)

/* Returns true when a variable of type <src> can be
 * stored in a variable of type <dest>.
 *
 * TYPE_ANY as <src> needs also TYPE_ANY as <dest>.
 * TYPE_UNKNOWN as <src> always yields true.
 */

{
    /* We have no type information? Then anything is possible. */
    if (src == NULL || dest == NULL)
        return true;

    /* Walk through the src union (until src isn't a union anymore). */
    while (true)
    {
        lpctype_t *srcbase = src->t_class == TCLASS_UNION ? src->t_union.member : src;
        bool found = false;

        /* Walk through the dest union. */
        while (true)
        {
            lpctype_t *destbase = dest->t_class == TCLASS_UNION ? dest->t_union.member : dest;

            if (destbase->t_class == TCLASS_PRIMARY && destbase->t_primary == TYPE_ANY)
            {
                /* TYPE_ANY matches everything. */
                found = true;
                break;
            }

            switch (srcbase->t_class)
            {
            case TCLASS_PRIMARY:
                if ((destbase->t_class == TCLASS_PRIMARY && destbase->t_primary == srcbase->t_primary)
                 || (srcbase->t_primary == TYPE_UNKNOWN))
                    found = true;
                break;

            case TCLASS_STRUCT:
                if(destbase->t_class == TCLASS_STRUCT)
                {
                    /* Check if <dest> is a base struct of <src>. */

                    if (destbase->t_struct.name == NULL) /* Matches any struct */
                        found = true;
                    else if (destbase->t_struct.name && srcbase->t_struct.def)
                    {
                        if(struct_baseof_name(destbase->t_struct.name, srcbase->t_struct.def))
                            found = true;
                    }
                }
                break;

            case TCLASS_OBJECT:
                if (destbase->t_class == TCLASS_OBJECT)
                {
                    if (srcbase == destbase
                     || (destbase->t_object.program_name == NULL  /* Matches any (lw)object */
                      && destbase->t_object.type == srcbase->t_object.type))
                        found = true;
                }
                break;

#ifdef USE_PYTHON
            case TCLASS_PYTHON:
                if (destbase == srcbase)
                    found = true;
                break;
#endif

            case TCLASS_ARRAY:
                if (destbase->t_class == TCLASS_ARRAY)
                {
                    if (destbase->t_array.depth == srcbase->t_array.depth)
                        found = lpctype_contains(srcbase->t_array.base, destbase->t_array.base);
                    else if (destbase->t_array.depth > srcbase->t_array.depth)
                    {
                        lpctype_t *destelem = destbase->t_array.element;
                        int i = srcbase->t_array.depth - 1; /* We already did one in the line above. */

                        for (;i;i--)
                            destelem = destelem->t_array.element;

                        found = lpctype_contains(srcbase->t_array.base, destelem);
                    }
                    else
                    {
                        lpctype_t *srcelem = srcbase->t_array.element;
                        int i = destbase->t_array.depth - 1;

                        for (;i;i--)
                            srcelem = srcelem->t_array.element;

                        found = lpctype_contains(srcelem, destbase->t_array.base);
                    }
                }
                break;

            case TCLASS_UNION:
                /* Shouldn't happen. */
                fatal("Union type within a union type.\n");
                break;
            }

            if (found)
                break;
            else if (dest->t_class == TCLASS_UNION)
                dest = dest->t_union.head;
            else
                break;
        }

        if (!found)
            return false;

        if (src->t_class == TCLASS_UNION)
            src = src->t_union.head;
        else
            break;
    }

    return true;
} /* lpctype_contains() */

/*-------------------------------------------------------------------------*/
static bool
has_inherit (program_t *prog, string_t *name, bool only_public)

/* Check whether the given program <prog> has an inherit <name>.
 * If <only_public> is true, then only public inherits are considered.
 */

{
    int num;

    if (prog->name == name)
        return !only_public;

    num = prog->num_inherited;
    for (inherit_t *inheritp = prog->inherit; --num >= 0; inheritp++)
    {
        if (has_inherit(inheritp->prog, name,
            inheritp->inherit_hidden ? true  :
            inheritp->inherit_public ? false :
            only_public))
            return true;
    }

    return false;
} /* has_inherit() */

/*-------------------------------------------------------------------------*/
static bool
is_compatible_program (program_t* prog, object_types_t otype, lpctype_t *t)

/* Checks whether the an object of type <otype> and with program <prog>
 * matches the type <t>.
 */

{
    if (t == NULL)
        return true;

    /* Walk through the union of t (until it isn't a union anymore). */
    while (true)
    {
        lpctype_t *base = t->t_class == TCLASS_UNION ? t->t_union.member : t;

        if (base->t_class == TCLASS_OBJECT && base->t_object.type == otype)
        {
            if (base->t_object.program_name == NULL)
                return true;
            if (has_inherit(prog, base->t_object.program_name, false))
                return true;
        }

        if (t->t_class == TCLASS_UNION)
            t = t->t_union.head;
        else
            break;
    }

    return false;
} /* is_compatible_object() */

/*-------------------------------------------------------------------------*/
bool
is_compatible_object (object_t* ob, lpctype_t *t)

/* Checks whether the given object <ob> matches the type <t>.
 */

{
    return is_compatible_program(ob->prog, OBJECT_REGULAR, t);
} /* is_compatible_object() */

/*-------------------------------------------------------------------------*/
bool
is_compatible_lwobject (lwobject_t* lwob, lpctype_t *t)

/* Checks whether the given lwobject <ob> matches the type <t>.
 */

{
    return is_compatible_program(lwob->prog, OBJECT_LIGHTWEIGHT, t);
} /* is_compatible_lwobject() */

/*-------------------------------------------------------------------------*/
struct lpctypename_s
{
    const char *name;
    lpctype_t  *type;
};

static struct lpctypename_s lpctypenames[] = {
    { "status",       &_lpctype_int          },
    { "int",          &_lpctype_int          },
    { "string",       &_lpctype_string       },
    { "void",         &_lpctype_void         },
    { "mapping",      &_lpctype_mapping      },
    { "float",        &_lpctype_float        },
    { "mixed",        &_lpctype_mixed        },
    { "closure",      &_lpctype_closure      },
    { "symbol",       &_lpctype_symbol       },
    { "quoted_array", &_lpctype_quoted_array },
    { "bytes",        &_lpctype_bytes        },
    { "coroutine",    &_lpctype_coroutine    },
    { "lpctype",      &_lpctype_lpctype      },
    { "struct",       &_lpctype_any_struct   },
    { "object",       &_lpctype_any_object   },
    { "lwobject",     &_lpctype_any_lwobject },
    { NULL, NULL }
};

/*-------------------------------------------------------------------------*/
static const char*
skip_whitespace(const char* str, const char* end)

/* Skip any whitespace characters and return a pointer to the first
 * non-whitespace character.
 */

{
    while (str != end)
    {
        p_int c;
        size_t clen = utf8_to_unicode(str, end - str, &c);

        if (!clen || !iswspace(c))
            return str;
        str += clen;
    }
    return end;
} /* skip_whitespace() */

/*-------------------------------------------------------------------------*/
static const char*
skip_alunum(const char* str, const char* end)

/* Skip any alpha-numeric characters (incl. underscore) and return a pointer
 * to the first non-matching character.
 */

{
    while (str != end)
    {
        p_int c;
        size_t clen = utf8_to_unicode(str, end - str, &c);

        if (!clen || !(c < 128 ? isalunum(c) : iswalnum((wint_t)c)))
            return str;
        str += clen;
    }
    return end;
} /* skip_alunum() */

/*-------------------------------------------------------------------------*/
lpctype_t *
parse_lpctype (const char** start, const char* end)

/* Parse the string starting at <*start> as a type, not going beyond <end>.
 * Return the resulting type (or NULL upon an error). On success <*start>
 * will then point to the next unprocessed character.
 */

{
    const char* str = *start;
    lpctype_t *result = NULL;

    while (true)
    {
        lpctype_t *part;
        p_int c;
        size_t clen = utf8_to_unicode(str, end - str, &c);
        if (!clen)
            break;

        if (c == '<')
        {
            str += clen;
            part = parse_lpctype(&str, end);
            if (part == NULL)
                break;

            if (*str != '>')
                break;
            str++;
        }
        else if (iswspace(c))
        {
            str += clen;
            continue;
        }
        else if (c < 128 ? isalunum(c) : iswalnum((wint_t)c))
        {
            const char* keyword = str;

            /* Skip to end of alphanumeric characters. */
            str = skip_alunum(str + clen, end);

            part = NULL;
            for (struct lpctypename_s *lpctypename = lpctypenames; lpctypename->name != NULL; lpctypename++)
            {
                if (!strncmp(lpctypename->name, keyword, str - keyword)
                 && strlen(lpctypename->name) == str - keyword)
                {
                    part = lpctypename->type;
                    break;
                }
            }

#ifdef USE_PYTHON
            if (part == NULL)
            {
                ident_t *p = find_shared_identifier_n(keyword, str - keyword, I_TYPE_PYTHON_TYPE, 0);
                while (p && p->type != I_TYPE_PYTHON_TYPE)
                    p = p->inferior;
                if (p)
                    part = get_python_type(p->u.python_type_id);
            }
#endif

            if (part == NULL)
                break;

            str = skip_whitespace(str, end);

            if (part == lpctype_any_struct)
            {
                /* Struct needs to have specific a name or 'mixed'. */
                const char* structname = str;
                str = skip_alunum(str, end);

                if (structname == str)
                    break;

                if (str - structname != 5 || memcmp(structname, "mixed", 5))
                {
                    /* First look at the current program. */
                    program_t *prog = get_current_object_program();
                    string_t *name;
                    if (!prog)
                        break;

                    name = find_tabled_str_n(structname, str - structname, STRING_UTF8);
                    if (!name)
                        break;

                    part = NULL;
                    for (int idx = 0; idx < prog->num_structs; idx++)
                    {
                        struct_type_t *st = prog->struct_defs[idx].type;
                        if (st->name->name == name
                         && !(prog->struct_defs[idx].flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)))
                        {
                            part = get_struct_type(st);
                            break;
                        }
                    }

                    if (!part)
                    {
                        /* Look at global struct definitions. */
                        ident_t *p = find_shared_identifier_mstr(name, I_TYPE_GLOBAL, 0);
                        while (p && p->type != I_TYPE_GLOBAL)
                            p = p->inferior;
                        if (p)
                        {
                            struct_type_t *st = NULL;
                            if (p->u.global.sefun_struct_id != I_GLOBAL_SEFUN_STRUCT_NONE)
                                st = get_simul_efun_program()->struct_defs[p->u.global.sefun_struct_id].type;
                            else if (p->u.global.std_struct_id != I_GLOBAL_STD_STRUCT_NONE)
                                st = get_std_struct_type(p->u.global.std_struct_id);

                            if (st != NULL)
                                part = get_struct_type(st);
                        }
                    }

                    if (!part)
                        break;
                }

                str = skip_whitespace(str, end);
            }
            else if ((part == lpctype_any_object || part == lpctype_any_lwobject)
                  && str != end && *str == '"')
            {
                /* Specific (lw)object type. */
                char buf[512];
                const char *obname = ++str;
                size_t oblen;
                string_t *obstr;

                while (str != end)
                {
                    if (*str == '\\')
                    {
                        str++;
                        if (str == end)
                            break;
                    }
                    else if (*str == '"')
                        break;
                    str++;
                }

                if (str == end)
                    break;

                oblen = unescape_string(obname, str - obname, buf, sizeof(buf));
                if (!oblen)
                    break;

                obstr = new_n_unicode_mstring(buf, oblen);
                if (!obstr)
                    break;

                if (part == lpctype_any_object)
                    part = get_object_type(obstr);
                else
                    part = get_lwobject_type(obstr);
                free_mstring(obstr);

                str = skip_whitespace(str+1, end);
            }
        }
        else
            break;

        /* At this point we have single type in <part>.
         * Any whitespaces have been skipped.
         */

        if (part != lpctype_void)
        {
            while (str != end && *str == '*')
            {
                lpctype_t *dummy = part;
                part = get_array_type(part);
                free_lpctype(dummy);
                str = skip_whitespace(str+1, end);
            }
        }

        if (result == NULL)
            result = part;
        else if (part == lpctype_void)
            break;
        else
        {
            lpctype_t *u = get_union_type(result, part);
            free_lpctype(result);
            free_lpctype(part);
            result = u;
        }

        if (str == end || *str == '>')
        {
            *start = str;
            return result;
        }
        if (result == lpctype_void || *str != '|')
            break;
        str++;
    }

    /* We get here on errors only. */
    free_lpctype(result);
    return NULL;
} /* parse_lpctype() */


/*-------------------------------------------------------------------------*/

/* The same definitions are in sys/lpctypes.h for the mudlibs. */
#define COMPAT_TYPE_OBJECT       4
#define COMPAT_TYPE_STRUCT      11
#define COMPAT_TYPE_LWOBJECT    13
#define COMPAT_MOD_POINTER    0x0040

int
get_type_compat_int (lpctype_t *t)

/* Calculates an integer for a type that can be returned by efuns
 * as the compile-time type for a function or variable.
 */

{
    switch(t->t_class)
    {
        case TCLASS_PRIMARY:
        {
            int val = t->t_primary;
            if (val >= COMPAT_TYPE_OBJECT)
                val++;
            if (val >= COMPAT_TYPE_STRUCT)
                val++;
            if (val >= COMPAT_TYPE_LWOBJECT)
                val++;
            return val;
        }

        case TCLASS_STRUCT:
            return COMPAT_TYPE_STRUCT;

        case TCLASS_OBJECT:
            return t->t_object.type == OBJECT_REGULAR ? COMPAT_TYPE_OBJECT : COMPAT_TYPE_LWOBJECT;

#ifdef USE_PYTHON
        case TCLASS_PYTHON:
            return TYPE_ANY;
#endif

        case TCLASS_ARRAY:
            return get_type_compat_int(t->t_array.element) | COMPAT_MOD_POINTER;

        case TCLASS_UNION:
            return TYPE_ANY;
    }

    return TYPE_UNKNOWN; /* Shouldn't happen. */
} /* get_type_compat_int() */

/*-------------------------------------------------------------------------*/
void
types_driver_info (svalue_t *svp, int value)

/* Returns the types information for driver_info(<what>).
 * <svp> points to the svalue for the result.
 */

{
    switch (value)
    {
        case DI_NUM_NAMED_OBJECT_TYPES:
            put_number(svp, num_object_types);
            break;

        case DI_NUM_NAMED_OBJECT_TYPES_TABLE_SLOTS:
            put_number(svp, object_type_table_size);
            break;

        case DI_SIZE_NAMED_OBJECT_TYPES_TABLE:
            put_number(svp, object_type_table_size * sizeof(*object_type_table));
            break;

        default:
            fatal("Unknown option for types_driver_info(): %d\n", value);
            break;
    }

} /* types_driver_info() */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
clear_lpctype_ref (lpctype_t *t)

/* Clear all references by <t>.
 */

{
    if (!t)
        return;

    if (!t->t_static)
    {
        /* Just in case the allocator forgot something... */
        clear_memory_reference(t);
        t->ref = 0;
    }

    /* Not reference counted pointers become NULL, so they don't
     * become dangling pointers after GC collected their blocks.
     */
    t->array_of = NULL;
    t->unions_of = NULL;

    switch(t->t_class)
    {
    case TCLASS_PRIMARY:
        break; /* Can't happen. See above. */

    case TCLASS_STRUCT:
        if (t->t_struct.name)
            clear_struct_name_ref(t->t_struct.name);
        if (!t->t_static)
            t->t_struct.def = NULL;
        break;

    case TCLASS_OBJECT:
        if (t->t_object.program_name)
            t->t_object.program_name->info.ref = 0;
        break;

#ifdef USE_PYTHON
    case TCLASS_PYTHON:
        break;
#endif

    case TCLASS_ARRAY:
        clear_lpctype_ref(t->t_array.element);
        break;

    case TCLASS_UNION:
        clear_lpctype_ref(t->t_union.head);
        clear_lpctype_ref(t->t_union.member);

        /* Mark it as not-yet counted. We need this mark
         * for static lpctype objects, because there
         * test_memory_reference() doesn't work.
         */
        t->t_union.next = lpctype_void;
        break;
    }

} /* clear_lpctype_ref() */

/*-------------------------------------------------------------------------*/
void
count_lpctype_ref (lpctype_t *t)

/* Count all references by <t>.
 */

{
    bool repair = false;
    if (!t)
        return;

    if (t->t_static)
    {
        /* Just repair not-refcounted references. */
        repair = true;
    }
    else
    {
        t->ref++;
        if (test_memory_reference(t))
        {
            note_malloced_block_ref(t);
            repair = true;

            switch(t->t_class)
            {
            case TCLASS_PRIMARY:
                break; /* Can't happen. See above. */

            case TCLASS_STRUCT:
                if (t->t_struct.name)
                {
                    count_struct_name_ref(t->t_struct.name);
                    t->t_struct.name->lpctype = t;
                }
                break;

            case TCLASS_OBJECT:
                if (t->t_object.program_name)
                    count_ref_from_string(t->t_object.program_name);
                break;

#ifdef USE_PYTHON
            case TCLASS_PYTHON:
                break;
#endif

            case TCLASS_ARRAY:
                count_lpctype_ref(t->t_array.element);
                break;

            case TCLASS_UNION:
                count_lpctype_ref(t->t_union.head);
                count_lpctype_ref(t->t_union.member);
                break;
            }
        }
    }

    if (repair)
    {
        switch(t->t_class)
        {
        case TCLASS_PRIMARY:
        case TCLASS_STRUCT:
        case TCLASS_OBJECT:
#ifdef USE_PYTHON
        case TCLASS_PYTHON:
#endif
            break; /* Nothing to do. */

        case TCLASS_ARRAY:
            t->t_array.element->array_of = t;
            break;

        case TCLASS_UNION:
            /* Did we already set the pointers back? */
            if (t->t_union.next == lpctype_void)
            {
                t->t_union.next = t->t_union.head->unions_of;
                t->t_union.head->unions_of = t;
            }
            break;
        }
    }
} /* count_lpctype_ref() */

/*-------------------------------------------------------------------------*/
void
clear_object_type_table_refs ()

/* Clear all references held by the named object types in the hash table.
 */

{
    if (!object_type_table || !object_type_table_size)
        return;

    for (size_t ix = 0; ix < object_type_table_size; ix++)
    {
        for (lpctype_t *this = object_type_table[ix]; this != NULL; this = this->t_object.next)
            clear_lpctype_ref(this);
    }
} /* clear_object_type_table_refs() */

/*-------------------------------------------------------------------------*/
void
remove_unreferenced_object_types ()

/* Free all named object types from the hash table which are not marked
 * as referenced. This function must be called before freeing all
 * unreferenced strings (which we need for the debug output).
 */

{
    if (!object_type_table || !object_type_table_size)
        return;

    for (size_t ix = 0; ix < object_type_table_size; ix++)
    {
        lpctype_t **prev = object_type_table + ix;

        for (lpctype_t *this = object_type_table[ix]; this != NULL;)
        {
            lpctype_t *next = this->t_object.next;

            if (!test_memory_reference(this))
            {
                *prev = this;
                prev = &(this->t_object.next);
            }
            else
            {
                /* Deallocate the memory. */
                num_object_types--;

                dprintf2(gcollect_outfd, "object type %x '%s' was left "
                                         "unreferenced, freeing now.\n"
                                       , (p_int) this
                                       , (p_int) get_txt(this->t_object.program_name)
                        );

                /* Reference the string and free it to avoid unnecessary
                 * 'string unreferenced' diagnostics.
                 */
                count_ref_from_string(this->t_object.program_name);
                free_mstring(this->t_object.program_name);

                /* Reference the memory (to update its flags) and free it */
                note_malloced_block_ref(this);
                xfree(this);
            }

            this = next;
        }

        *prev = NULL;
    }
} /* remove_unreferenced_object_types() */

/*-------------------------------------------------------------------------*/
#endif /* GC_SUPPORT */
