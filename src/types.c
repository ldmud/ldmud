/*---------------------------------------------------------------------------
 * LPC type handling
 *
 *---------------------------------------------------------------------------
 */

#include <assert.h>

#include "gcollect.h"
#include "types.h"
#include "simulate.h"
#include "structs.h"
#include "xalloc.h"

/* Base types are statically allocated. */
lpctype_t _lpctype_int          = { 0, { TCLASS_PRIMARY, true }, {TYPE_NUMBER},       NULL, NULL };
lpctype_t _lpctype_string       = { 0, { TCLASS_PRIMARY, true }, {TYPE_STRING},       NULL, NULL };
lpctype_t _lpctype_object       = { 0, { TCLASS_PRIMARY, true }, {TYPE_OBJECT},       NULL, NULL };
lpctype_t _lpctype_mapping      = { 0, { TCLASS_PRIMARY, true }, {TYPE_MAPPING},      NULL, NULL };
lpctype_t _lpctype_float        = { 0, { TCLASS_PRIMARY, true }, {TYPE_FLOAT},        NULL, NULL };
lpctype_t _lpctype_mixed        = { 0, { TCLASS_PRIMARY, true }, {TYPE_ANY},          NULL, NULL };
lpctype_t _lpctype_closure      = { 0, { TCLASS_PRIMARY, true }, {TYPE_CLOSURE},      NULL, NULL };
lpctype_t _lpctype_symbol       = { 0, { TCLASS_PRIMARY, true }, {TYPE_SYMBOL},       NULL, NULL };
lpctype_t _lpctype_quoted_array = { 0, { TCLASS_PRIMARY, true }, {TYPE_QUOTED_ARRAY}, NULL, NULL };
lpctype_t _lpctype_void         = { 0, { TCLASS_PRIMARY, true }, {TYPE_VOID},         NULL, NULL };
lpctype_t _lpctype_bytes        = { 0, { TCLASS_PRIMARY, true }, {TYPE_BYTES},        NULL, NULL };
lpctype_t _lpctype_unknown      = { 0, { TCLASS_PRIMARY, true }, {TYPE_UNKNOWN},      NULL, NULL };
lpctype_t _lpctype_any_struct   = { 0, { TCLASS_STRUCT,  true }, {.t_struct = {NULL, NULL}},  NULL, NULL };

/* And are used via pointer. */
lpctype_t *lpctype_int          = &_lpctype_int;
lpctype_t *lpctype_string       = &_lpctype_string;
lpctype_t *lpctype_object       = &_lpctype_object;
lpctype_t *lpctype_mapping      = &_lpctype_mapping;
lpctype_t *lpctype_float        = &_lpctype_float;
lpctype_t *lpctype_mixed        = &_lpctype_mixed;
lpctype_t *lpctype_closure      = &_lpctype_closure;
lpctype_t *lpctype_symbol       = &_lpctype_symbol;
lpctype_t *lpctype_quoted_array = &_lpctype_quoted_array;
lpctype_t *lpctype_any_struct   = &_lpctype_any_struct;
lpctype_t *lpctype_void         = &_lpctype_void;
lpctype_t *lpctype_bytes        = &_lpctype_bytes;
lpctype_t *lpctype_unknown      = &_lpctype_unknown;

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
get_struct_type (struct_type_t* def)

/* Create an lpctype_t around <def>, check for already
 * existing definitions.
 */

{
    struct_name_t* name = def->name;
    lpctype_t *type = name->lpctype;

    if (type != NULL)
        ref_lpctype(type);
    else
    {
        name->lpctype = type = lpctype_new();
        type->t_class = TCLASS_STRUCT;
        type->t_struct.name = ref_struct_name(name);
    }

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
    if(!t || t->t_class != TCLASS_STRUCT)
        return;

    t->t_struct.def = NULL;
} /* clean_struct_type */

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

    if (member == NULL)
        return ref_lpctype(head);
    if (head == NULL)
        return ref_lpctype(member);
    if (head == lpctype_unknown || member == lpctype_unknown)
        return lpctype_unknown;
    if (head == lpctype_mixed || member == lpctype_mixed)
        return lpctype_mixed;

    if(member->t_class == TCLASS_UNION)
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
        while(next_member->t_class == TCLASS_UNION);

        insert = result;
        result = get_union_type(result, next_member);
        free_lpctype(insert);

        return result;
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

/* Determine the intersection of both types.
 * Returns NULL if there is no common type.
 * If one of both types is TYPE_UNKNOWN, then
 * the result will by TYPE_UNKNOWN, too.
 */

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

            case TCLASS_ARRAY:
                if (destbase->t_class == TCLASS_ARRAY)
                {
                    if (destbase->t_array.depth == srcbase->t_array.depth)
                        found = lpctype_contains(srcbase->t_array.base, destbase->t_array.base);
                    else if (destbase->t_array.depth > srcbase->t_array.depth)
                    {
                        lpctype_t *destelem = destbase->t_array.element;
                        int i = srcbase->t_array.depth;

                        for (;i;i--)
                            destelem = destelem->t_array.element;

                        found = lpctype_contains(srcbase->t_array.base, destelem);
                    }
                    else
                    {
                        lpctype_t *srcelem = srcbase->t_array.element;
                        int i = destbase->t_array.depth;

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
        t->t_struct.def = NULL;
        break;

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

#endif /* GC_SUPPORT */
