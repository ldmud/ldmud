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

#include <stdio.h>

#include "otable.h"

#include "backend.h"
#include "comm.h"
#include "gcollect.h"
#include "hash.h"
#include "interpret.h"
#include "object.h"
#include "simulate.h"


/*=========================================================================*/
/*                           OBJECT TABLE                                  */
/*-------------------------------------------------------------------------*/

#if !( (OTABLE_SIZE) & (OTABLE_SIZE)-1 )
#    define ObjHash(s) (whashstr((s), 100) & ((OTABLE_SIZE)-1) )
#else
#    define ObjHash(s) (whashstr((s), 100) % OTABLE_SIZE)
#endif
/* Hash the string <s> and compute the appropriate table index
 */

static struct object ** obj_table = NULL;
  /* Pointer to the (allocated) hashtable.
   */

static long objs_in_table = 0;
  /* Number of objects in the table.
   */

static long obj_searches = 0;
static long obj_probes = 0;
static long objs_found = 0;
  /* Total number of object lookups, of visited objects, and
   * the number of successfull lookups.
   */

static long user_obj_lookups = 0;
static long user_obj_found = 0;
  /* Number of externally requested lookups, and how many succeeded.
   */

/*-------------------------------------------------------------------------*/
static struct object *
find_obj_n (char *s)

/* Lookup the object with name <s> in the table and return the pointer
 * to its structure. If it is not in the table, return NULL.
 *
 * The call updates the statistics and also moves the found object
 * to the head of its hash chain.
 */

{
    struct object * curr, *prev;

    int h = ObjHash(s);

    curr = obj_table[h];
    prev = NULL;

    obj_searches++;

    while (curr)
    {
        obj_probes++;
        if (!strcmp(curr->name, s)) /* found it */
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
enter_object_hash (struct object *ob)

/* Add the object <ob> to the table. There must not be an object
 * with the same name in the table already (not even <ob> itself).
 */

{
#ifdef DEBUG
    struct object * s;
#endif
    int h = ObjHash(ob->name);

#ifdef DEBUG
    s = find_obj_n(ob->name);
    if (s)
    {
        if (s != ob)
            fatal("Duplicate object \"%s\" in object hash table"
                 , ob->name);
        else
            fatal( "Entering object \"%s\" twice in object table"
                 , ob->name);
    }
    if (ob->next_hash)
        fatal( "Object \"%s\" not found in object table but next link not null"
             , ob->name);
#endif

    ob->next_hash = obj_table[h];
    obj_table[h] = ob;
    objs_in_table++;
}

/*-------------------------------------------------------------------------*/
void
remove_object_hash (struct object *ob)

/* Remove object <ob> from the table, where it must be in.
 */
 
{
    struct object * s;
    int h = ObjHash(ob->name);

    s = find_obj_n(ob->name);

    if (s != ob)
        fatal( "Remove object \"%s\": found a different object!"
             , ob->name);

    obj_table[h] = ob->next_hash;
    ob->next_hash = NULL;
    objs_in_table--;
}

/*-------------------------------------------------------------------------*/
/*
 * Lookup an object in the hash table; if it isn't there, return null.
 * This is only different to find_object_n in that it collects different
 * stats; more finds are actually done than the user ever asks for.
 */

struct object *
lookup_object_hash (char *s)

/* Lookup an object by name <s>. If found, return its pointer, if not,
 * return NULL.
 */

{
    struct object * ob = find_obj_n(s);
    user_obj_lookups++;
    if (ob)
        user_obj_found++;
    return ob;
}

/*-------------------------------------------------------------------------*/
size_t
show_otable_status (/* TODO: BOOL */ short verbose)

/* Return the amount of memory used by the object table.
 * If <verbose> is TRUE, also print the statistics to the current user.
 */

{
    if (verbose)
    {
        char sbuf[100];
        
        add_message("\nObject name hash table status:\n");
        add_message("------------------------------\n");
        sprintf(sbuf, "%.2f", (float) objs_in_table / (float) OTABLE_SIZE);
        add_message("Average hash chain length                   %s\n", sbuf);
        sprintf(sbuf, "%.2f", (float) obj_probes / (float) obj_searches);
        add_message("Searches/average search length       %ld (%s)\n",
                    obj_searches, sbuf);
        add_message("External lookups succeeded (succeed) %ld (%ld)\n",
                    (long)user_obj_lookups, (long)user_obj_found);
    }
    /* objs_in_table * sizeof(struct object) is already accounted for
       in tot_alloc_object_size.  */
    add_message("hash table overhead\t\t\t %8ld\n",
                (long)(OTABLE_SIZE * sizeof(struct object *)));
    return OTABLE_SIZE * sizeof(struct object *);
}


/*=========================================================================*/
/*                           GENERAL ROUTINES                              */

/*-------------------------------------------------------------------------*/
void
init_otable (void)

/* Allocate and initialise the hash table
 */

{
    int x;
    obj_table = xalloc(sizeof(struct object *) * OTABLE_SIZE);

    for (x = 0; x < OTABLE_SIZE; x++)
        obj_table[x] = NULL;
}

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_smalloc
void
note_otable_ref (void)

/* GC support: mark the memory used by the hashtable as used.
 */

{
    note_malloced_block_ref((char *)obj_table);
}

#endif /* MALLOC_smalloc */


/***************************************************************************/

