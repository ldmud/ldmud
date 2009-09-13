/*---------------------------------------------------------------------------
 * Object Table
 *
 *---------------------------------------------------------------------------
 * The OTable:
 *
 *   This table is a lookup-by-name table of all (non destructed)
 *   objects in the game. Similar to the shared string table, this one
 *   is even simpler because object names are unique by name and address.
 *
 *   Note: if you change an object name, you must remove it and reenter it.
 *
 *   The hash index is computed from the object name, and if the object
 *   is found in the index chain, it is moved to the head of the chain
 *   to speed up further lookups.
 *
 *   The size of the hash table is given by OTABLE_SIZE in config.h. It does
 *   not need to be prime, and should probably be set to 1/4 of the number
 *   of objects in the game.
 *
 *   The table links are not counted in the object's refcount.
 *
 *   TODO: Maybe make the object table size dynamic (start with 128 and double
 *   TODO:: as required). This also requires to store the hashvalue in
 *   TODO:: the object.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include <stdio.h>

#include "otable.h"

#include "backend.h"
#include "gcollect.h"
#include "hash.h"
#include "mstrings.h"
#include "object.h"
#include "strfuns.h"
#include "simulate.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/debug_info.h"

/*=========================================================================*/
/*                           OBJECT TABLE                                  */
/*-------------------------------------------------------------------------*/

/* Hash the string <s> and compute the appropriate table index
 */
static INLINE hash32_t ObjHash(string_t * const s)
{
#if !( (OTABLE_SIZE) & (OTABLE_SIZE)-1 )
    return mstr_get_hash(s) & ((OTABLE_SIZE)-1);
#else
    return mstr_get_hash(s) % (OTABLE_SIZE);
#endif
}
static INLINE hash32_t ObjHashStr(char const * const s, size_t len)
{
#if !( (OTABLE_SIZE) & (OTABLE_SIZE)-1 )
    return hash_string(s, len) & ((OTABLE_SIZE)-1);
#else
    return hash_string(s, len) % (OTABLE_SIZE);
#endif
}

static object_t ** obj_table = NULL;
  /* Pointer to the (allocated) hashtable.
   */

static long objs_in_table = 0;
  /* Number of objects in the table.
   */

static statcounter_t obj_searches = 0;
static statcounter_t obj_probes = 0;
static statcounter_t objs_found = 0;
  /* Total number of object lookups, of visited objects, and
   * the number of successfull lookups.
   */

static statcounter_t user_obj_lookups = 0;
static statcounter_t user_obj_found = 0;
  /* Number of externally requested lookups, and how many succeeded.
   */

/*-------------------------------------------------------------------------*/
static object_t *
find_obj_n (string_t *s)

/* Lookup the object with name <s> in the table and return
 * the pointer to its structure. If it is not in the table, return NULL.
 *
 * The call updates the statistics and also moves the found object
 * to the head of its hash chain.
 */

{
    object_t * curr, *prev;

    int h = ObjHash(s);

    curr = obj_table[h];
    prev = NULL;

    obj_searches++;

    while (curr)
    {
        obj_probes++;
        if (mstreq(curr->name, s)) /* found it */
        {
            if (prev) /* not at head of list */
            {
                prev->next_hash = curr->next_hash;
                curr->next_hash = obj_table[h];
                obj_table[h] = curr;
            }
            objs_found++;
            return curr;
        }
        prev = curr;
        curr = curr->next_hash;
    }

    /* Not found */
    return NULL;

} /* find_obj_n() */

/*-------------------------------------------------------------------------*/
static object_t *
find_obj_n_str (char const * const s)

/* Lookup the object with name <s> (length <slen>) in the table and return
 * the pointer to its structure. If it is not in the table, return NULL.
 *
 * The call updates the statistics and also moves the found object
 * to the head of its hash chain.
 */

{
    object_t * curr, *prev;

    int h = ObjHashStr(s,strlen(s));

    curr = obj_table[h];
    prev = NULL;

    obj_searches++;

    while (curr)
    {
        obj_probes++;
        if (!strcmp(get_txt(curr->name), s)) /* found it */
        {
            if (prev) /* not at head of list */
            {
                prev->next_hash = curr->next_hash;
                curr->next_hash = obj_table[h];
                obj_table[h] = curr;
            }
            objs_found++;
            return curr;
        }
        prev = curr;
        curr = curr->next_hash;
    }

    /* Not found */
    return NULL;

} /* find_obj_n() */

/*-------------------------------------------------------------------------*/
void
enter_object_hash (object_t *ob)

/* Add the object <ob> to the table. There must not be an object
 * with the same name in the table already (not even <ob> itself).
 */

{
#ifdef DEBUG
    object_t * s;
#endif
    int h = ObjHash(ob->name);

#ifdef DEBUG
    s = find_obj_n(ob->name);
    if (s)
    {
        if (s != ob)
            fatal("Duplicate object \"%s\" in object hash table.\n"
                 , get_txt(ob->name));
        else
            fatal( "Entering object \"%s\" twice in object table.\n"
                 , get_txt(ob->name));
    }
    if (ob->next_hash)
        fatal( "Object \"%s\" not found in object table but next link not null"
             , get_txt(ob->name));
#endif

    ob->next_hash = obj_table[h];
    obj_table[h] = ob;
    objs_in_table++;
}

/*-------------------------------------------------------------------------*/
void
remove_object_hash (object_t *ob)

/* Remove object <ob> from the table, where it must be in.
 */

{
    object_t * s;
    int h = ObjHash(ob->name);

    s = find_obj_n(ob->name);

    if (s != ob)
        fatal( "Remove object \"%s\": found a different object!"
             , get_txt(ob->name));

    obj_table[h] = ob->next_hash;
    ob->next_hash = NULL;
    objs_in_table--;
}

/*-------------------------------------------------------------------------*/
object_t *
lookup_object_hash (string_t *s)

/* Lookup an object by name <s>. If found, return its pointer, if not,
 * return NULL.
 */

{
    object_t * ob = find_obj_n(s);
    user_obj_lookups++;
    if (ob)
        user_obj_found++;
    return ob;
}

/*-------------------------------------------------------------------------*/
object_t *
lookup_object_hash_str (const char *s)

/* Lookup an object by name <s>. If found, return its pointer, if not,
 * return NULL.
 */

{
    object_t * ob = find_obj_n_str(s);
    user_obj_lookups++;
    if (ob)
        user_obj_found++;
    return ob;
}

/*-------------------------------------------------------------------------*/
size_t
show_otable_status (strbuf_t * sbuf, Bool verbose)

/* Return the amount of memory used by the object table.
 * If <verbose> is TRUE, also print the statistics to the current user.
 */

{
    if (verbose)
    {
#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_largeargs off
#endif
        strbuf_add(sbuf, "\nObject name hash table status:\n");
        strbuf_add(sbuf, "------------------------------\n");
        strbuf_addf(sbuf
                   , "Average hash chain length                   %.2f\n"
                   , (float) objs_in_table / (float) OTABLE_SIZE);
        strbuf_addf(sbuf
                   , "Searches/average search length       %"PRIuSTATCOUNTER" (%.2f)\n"
                   , obj_searches
                   , (float) obj_probes / (float) obj_searches);
        strbuf_addf(sbuf, "External lookups (succeed)   %"PRIuSTATCOUNTER" (%"PRIuSTATCOUNTER")\n"
                   , user_obj_lookups, user_obj_found);
#if defined(__MWERKS__)
#    pragma warn_largeargs reset
#endif
    }
    /* objs_in_table * sizeof(object_t) is already accounted for
       in tot_alloc_object_size.  */
    strbuf_addf(sbuf, "hash table overhead\t\t\t %9ld\n",
                (long)(OTABLE_SIZE * sizeof(object_t *)));
    return OTABLE_SIZE * sizeof(object_t *);
}

/*-------------------------------------------------------------------------*/
void
otable_dinfo_status (svalue_t *svp, int value)

/* Return the object table information for debug_info(DINFO_DATA, DID_STATUS).
 * <svp> points to the svalue block for the result, this function fills in
 * the spots for the object table.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */

{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    ST_NUMBER(DID_ST_OTABLE, objs_in_table);
    ST_NUMBER(DID_ST_OTABLE_SLOTS, OTABLE_SIZE);
    ST_NUMBER(DID_ST_OTABLE_SIZE, OTABLE_SIZE * sizeof(object_t *));

#undef ST_NUMBER
} /* otable_dinfo_status() */

/*=========================================================================*/
/*                           GENERAL ROUTINES                              */

/*-------------------------------------------------------------------------*/
void
init_otable (void)

/* Allocate and initialise the hash table
 */

{
    int x;
    obj_table = xalloc(sizeof(object_t *) * OTABLE_SIZE);

    for (x = 0; x < OTABLE_SIZE; x++)
        obj_table[x] = NULL;
}

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT
void
note_otable_ref (void)

/* GC support: mark the memory used by the hashtable as used.
 */

{
    note_malloced_block_ref((char *)obj_table);
}

#endif /* GC_SUPPORT */


/***************************************************************************/

