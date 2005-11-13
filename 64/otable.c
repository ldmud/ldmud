/*---------------------------------------------------------------------------
 * Object Tables
 *
 *---------------------------------------------------------------------------
 * This module implements two table objects: the 'otable' to lookup objects
 * by their name, and the 'reset table' which sorts objects according to
 * their next reset time.
 *
 * Both tables do not affect the ref count of the objects.
 *
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
 *   TODO: Maybe make the object table size dynamic (start with 128 and double
 *   TODO:: as required). This also requires to store the hashvalue in
 *   TODO:: the object.
 *
 *
 * The Reset Table:
 *
 *   This table keeps all objects which require a reset (which is about
 *   every object in the game) sorted by the due times. The sorting allows
 *   to quickly find the next due object without scanning the whole list
 *   of objects.
 *
 *   The table is organized as list of lists. The objects are grouped by
 *   their due time (.time_reset) into intervals of RESET_GRANULARITY, each
 *   group linked into an (unsorted) list using the .next_reset link.
 *   The lists are held in sorted top-level list with one node for each
 *   used interval.
 *
 *   The proper value of RESET_GRANULARITY is a bit tricky to chose:
 *   too high and the driver spends a lot time searching the group lists
 *   even though the memory overhead is low. Too low and the driver spends
 *   quite some time searching the top-level list and the memory overhead
 *   is higher. However, a too low value is less critical performancewise
 *   than a too high value.
 *
 *   In a typical game, you can expect between TIME_TO_RESET/RESET_GRANULARITY
 *   and twice that value entries in the reset table. If this number of
 *   entries is N, and you have M objects in the game, you'll have on average
 *   M/N objects in each group list. This gives approximately the following
 *   average performance figures:
 *     adding an object:            O(N/2)
 *     removing an object:          O(N/2+ M/2N)
 *     finding the next due object: O(1)
 *
 *   TODO: It has to be seen if a more complex datastructure (binary search tree)
 *   TODO:: would perform better.
 *   TODO:: OTOH, see f-990323-1
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
                    user_obj_lookups, user_obj_found);
    }
    /* objs_in_table * sizeof(struct object) is already accounted for
       in tot_alloc_object_size.  */
    add_message("hash table overhead\t\t\t %8ld\n",
                OTABLE_SIZE * sizeof(struct object *));
    return OTABLE_SIZE * sizeof(struct object *);
}

/*=========================================================================*/
/*                             RESET TABLE                                 */
#ifndef OLD_RESET

/*-------------------------------------------------------------------------*/
/* One entry in the reset table.
 */

struct reset_entry {
    struct reset_entry * next;  /* next entry in the table */
    struct object      * obj;   /* first object listed under this entry */
    time_t               duetime;
      /* due time of this entry divided by RESET_GRANULARITY */
    long                 num;  /* number of objects listed in here */
};

static struct reset_entry *reset_table = NULL;
  /* The Reset Table.
   */

static long rtable_num_entries = 0;
  /* Number of distinct entries in the table.
   */

static long rtable_num_objs = 0;
  /* Number of objects kept in the table.
   */

static long rtable_num_add = 0;
static long rtable_add_steps = 0;
  /* Number of additions to the table, and number of search steps.
   */

static long rtable_num_remove = 0;
static long rtable_remove_steps = 0;
  /* Number of removes from the table, and number of search steps.
   */

/*-------------------------------------------------------------------------*/
void
rtable_add (struct object *obj)

/* Add object <obj> to the reset_table according to its 'time_reset' time,
 * unless it's 0.
 * <obj> must not be part of the table already!
 */

{
    time_t duetime;
    struct reset_entry *prev, *pr;

#if 0
printf("DEBUG: rtable_add('%s' due %ld) cur %ld\n", obj->name, obj->time_reset, current_time);
#endif
    rtable_num_add++;

    if (!obj->time_reset)
        return;

    duetime = (obj->time_reset + RESET_GRANULARITY - 1) / RESET_GRANULARITY;

    /* Find/create the proper entry for this object */
    for (prev = NULL, pr = reset_table; pr && pr->duetime < duetime; )
    {
        prev = pr;
        pr = pr->next;
        rtable_add_steps++;
    }

    if (!pr || pr->duetime != duetime)
    {
        struct reset_entry *new;
        
        new = xalloc(sizeof(*new));
        if (!new)
            error("Out of memory.\n");
        new->obj = NULL;
        new->duetime = duetime;
        new->num = 0;

        if (!prev)
        {
            new->next = reset_table;
            reset_table = new;
        }
        else
        {
            prev->next = new;
            new->next = pr;
        }

        rtable_num_entries++;
        pr = new;
    }

    /* Insert the object into the list of entry pr.
     */
    obj->next_reset = pr->obj;
    pr->obj = obj;
    pr->num++;
    rtable_num_objs++;
} /* rtable_add() */

/*-------------------------------------------------------------------------*/
void
rtable_remove (struct object *obj)

/* Remove object <obj> to the reset_table according to its 'time_reset',
 * unless it's 0.
 *
 * <obj> need not be part of the table - usually it is, but if an object
 * is destructed during its own reset(), destruct_object() will call
 * rtable_remove() even though at that time the object is indeed not in
 * the table.
 */

{
    time_t duetime;
    struct reset_entry *pr, *prev_e;
    struct object *prev, *ob;

#if 0
printf("DEBUG: rtable_remove('%s' due %ld) cur %ld\n", obj->name, obj->time_reset, current_time);
#endif
    rtable_num_remove++;

    if (!obj->time_reset)
        return;

    duetime = (obj->time_reset + RESET_GRANULARITY - 1) / RESET_GRANULARITY;

    /* Find the proper entry for this object */
    for (prev_e = NULL, pr = reset_table; pr && pr->duetime < duetime; )
    {
        prev_e = pr;
        pr = pr->next;
        rtable_remove_steps++;
    }

    if (!pr || pr->duetime != duetime)
        return;

    /* Find the object in the list of this entry
     */
    for (prev = NULL, ob = pr->obj; ob && ob != obj; )
    {
        prev = ob;
        ob = ob->next_reset;
        rtable_remove_steps++;
    }

    if (!ob || ob != obj)
        return;

    /* Remove the object from the table
     */
    if (prev)
        prev->next_reset = obj->next_reset;
    else
        pr->obj = obj->next_reset;

    pr->num--;
    rtable_num_objs--;
    
    /* If possible, remove the entry from the table.
     */
    if (!pr->num)
    {
        if (prev_e)
            prev_e->next = pr->next;
        else
            reset_table = pr->next;
        xfree(pr);
        rtable_num_entries--;
    }
} /* rtable_remove() */

/*-------------------------------------------------------------------------*/
struct object *
rtable_next_due (time_t cur_time)

/* Find the first object which reset is due at time <cur_time>, remove
 * it from the table and return it.
 * If there is no such object, return NULL.
 */

{
    time_t duetime;
    struct reset_entry *pr;
    struct object *obj;

    duetime = cur_time / RESET_GRANULARITY;
#if 0
printf("DEBUG: rtable_next_due(%ld) %ld first due %ld\n"
      , cur_time, duetime, reset_table ? reset_table->duetime : 0);
#endif
    /* Either the first entry in the table is suitable, or
     * no object is (over)due.
     */
    if (!reset_table || reset_table->duetime > duetime)
        return NULL;
    pr = reset_table;

    /* Remove the first object in the entry from the table
     */
    obj = pr->obj;
#ifdef DEBUG
    if (!obj)
        fatal("rtable_next_due(): Empty entry in the table.\n");
    else
#endif
    pr->obj = obj->next_reset;
    pr->num--;
    rtable_num_objs--;
    
    /* If possible, remove the entry from the table.
     */
    if (!pr->num)
    {
        reset_table = pr->next;
        xfree(pr);
        rtable_num_entries--;
    }

    return obj;
} /* rtable_next_due() */

/*-------------------------------------------------------------------------*/
size_t
show_rtable_status (/* TODO: BOOL */ short verbose)

/* Return the amount of memory used by the reset table.
 * If <verbose> is TRUE, also print the statistics to the current user.
 */

{
    if (verbose)
    {
        char sbuf[100];
        time_t duetime;
        long overdue, due, maxdue;
        struct reset_entry *pr;
        
        duetime = (current_time + RESET_GRANULARITY - 1) / RESET_GRANULARITY;
        overdue = 0;
        due = 0;
        maxdue = 0;
        for (pr = reset_table; pr; pr = pr->next)
        {
            if (pr->num > maxdue)
                maxdue = pr->num;
            if (pr->duetime < duetime)
                overdue += pr->num;
            if (pr->duetime == duetime)
                due = pr->num;
        }
        
        add_message("\nReset table status:\n");
        add_message("-------------------\n");
        if (rtable_num_entries)
        {
            sprintf(sbuf, "%.2f"
                        , (float)rtable_add_steps / (float)rtable_num_add);
            add_message("Objects added/search steps\t\t%8ld / %8ld (%s)\n"
                       , rtable_num_add, rtable_add_steps, sbuf
                       );
            sprintf(sbuf, "%.2f"
                   , (float)rtable_remove_steps / (float)rtable_num_remove);
            add_message("Objects removed/search steps\t\t%8ld / %8ld (%s)\n"
                       , rtable_num_remove, rtable_remove_steps, sbuf
                       );
            add_message("reset table overhead\t\t\t %8ld\n",
                        rtable_num_entries * sizeof(struct reset_entry));
            sprintf(sbuf, "%.2f"
                        , (float)rtable_num_objs / (float)rtable_num_entries);
            add_message("%ld objects due at %ld times: avg. %s, max %ld objs per time.\n"
                       , rtable_num_objs, rtable_num_entries, sbuf, maxdue);
            /* TODO: Something like 'n percent complete last time' would
             * TODO:: be more useful than 'n objects overdue'
             */
            add_message("%ld objects due, %ld objects overdue.\n"
                       , due, overdue
                       );
        }
        else
            add_message("No objects listed in the table.\n");
    }
    else
        add_message("reset table overhead\t\t\t %8ld\n",
                     rtable_num_entries * sizeof(struct reset_entry));

    return rtable_num_entries * sizeof(struct reset_entry);
}

/*-------------------------------------------------------------------------*/

#endif /* OLD_RESET */

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

#ifndef OLD_RESET
    {
        struct reset_entry *pr;
        
        for (pr = reset_table; pr; pr = pr->next)
            note_malloced_block_ref((char *)pr);
    }
#endif OLD_RESET
}

#endif /* MALLOC_smalloc */


/***************************************************************************/

