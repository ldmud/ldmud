/*---------------------------------------------------------------------------
 * Heartbeat Management
 *
 *---------------------------------------------------------------------------
 * This module holds the datastructures and function related to the
 * handling of heartbeats.
 *
 * Objects with active heartbeats are referenced from a list which
 * is sorted in ascending order of the object pointers. However, these
 * object pointers do not count as 'refs'.
 *
 * The backend will call call_heart_beat() in every cycle right after
 * starting a new alarm(). The function evaluates as many heartbeats
 * as possible before the alarm sets comm_time_to_call_heart_beat,
 * and then returns. If some heartbeats are left unprocessed, the first
 * of them is remembered in a pointer so that the next call can
 * continue from there.
 *
 * However, no heartbeats are executed at all if there is no player
 * in the game.
 *
 * TODO: Make the list a skiplist or similar.
 * TODO: Add an object flag O_IN_HB_LIST so that several toggles of the
 * TODO:: heart beat status only toggle O_HEARTBEAT, but leave the object
 * TODO:: in the list until call_+heart_beat() can remove it. This would
 * TODO:: also remove the need for a double-linked or skiplist, but
 * TODO:: require the object-pointer to count as ref and it could let
 * TODO:: keep destructed objects in the list for a while.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#ifndef OLD_RESET

#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <math.h>

#define NO_INCREMENT_STRING_REF
#include "heartbeat.h"

#include "array.h"
#include "backend.h"
#include "comm.h"
#include "datatypes.h"
#include "gcollect.h"
#include "object.h"
#include "sent.h"
#include "simulate.h"
#include "wiz_list.h"

/*-------------------------------------------------------------------------*/

/* Listnode for one object with a heartbeat
 */

struct hb_info {
    struct hb_info * next;  /* next node in list */
    struct hb_info * prev;  /* previous node in list */
    struct object  * obj;   /* the object itself */
};

/* Allocation block for a bunch of listnodes.
 * They are kept in a list themselves for the garbage collector.
 */

#define NUM_NODES 32

struct hb_block {
    struct hb_block * next;           /* next allocated block */
    struct hb_info entry[NUM_NODES];  /* the heartbeat nodes */
};

/*-------------------------------------------------------------------------*/
struct object *current_heart_beat = NULL;
  /* The object whose heart_beat() is currently executed.
   * interpret.c needs to know this for the heart-beat tracing, and
   * simulate.c test this in the error() function to react properly.
   */

static struct hb_info * hb_list = NULL;
  /* Head of the list of heart_beat infos.
   */

static struct hb_info * next_hb = NULL;
  /* Next hb_info whose objects' heartbeat must be executed.
   * If NULL, the first info in the list is meant.
   */

static struct hb_info * free_list = NULL;
  /* List of unused nodes
   */

static struct hb_block * block_list = NULL;
  /* List of heartbeat node blocks.
   */

static mp_int num_blocks = 0;
  /* Number of allocated node blocks.
   */

#if !defined(OLD_RESET) && defined(DEBUG)
mp_int num_hb_objs = 0;
#else
static mp_int num_hb_objs = 0;
#endif
  /* Number of objects with a heart beat.
   */

static mp_int hb_num_done;
  /* Number of heartbeats done in last process_objects().
   */

static long avg_num_hb_objs = 0;
static long avg_num_hb_done = 0;
  /* Decaying average of num_hb_objs and hb_num_done.
   */

static long num_hb_calls = 0;
  /* Number of calls to call_heart_beat() with active heartbeats.
   */

/*-------------------------------------------------------------------------*/
void
call_heart_beat (void)

/* Call the heart_beat() lfun in all registered heart beat objects; or at
 * at least call as many as possible until the next alarm timeout (as
 * registered in comm_time_to_call_heart_beat) occurs. If a timeout occurs,
 * next_hb will point to the next object with a due heartbeat.
 *
 * If the object in question (or one of its shadows) is living, command_giver
 * is set to the object, else it is set to NULL. If there are no players
 * in the game, no heart_beat() will be called (but the call outs will!).
 *
 * The function does not change the time_to_call-flags or messes with alarm().
 * It may be aborted prematurely if an error occurs during the heartbeat.
 */

{
    struct hb_info *this;
    mp_int num_hb_to_do;

    /* Housekeeping */

    current_interactive = NULL;

    /* Set this new round through the hb list */
    hb_num_done = 0;

    if (num_player < 1 || !num_hb_objs)
    {
        next_hb = NULL;
        return;
    }

    num_hb_calls++;
    num_hb_to_do = num_hb_objs;

    this = next_hb;

    while (hb_num_done < num_hb_to_do && !comm_time_to_call_heart_beat)
    {
        struct object * obj;

        /* If 'this' object is NULL, we reached the end of the
         * list and have to wrap around.
         */
        if (!this)
            this = hb_list;

        obj = this->obj;
        next_hb = this->next;

        hb_num_done++;

#ifdef DEBUG
        if (!(obj->flags & O_HEART_BEAT))
            fatal("Heart beat not set in object on heart beat list!");
        if (obj->flags & O_SWAPPED)
            fatal("Heart beat in swapped object.\n");
#endif

        if (obj->prog->heart_beat == -1)
        {
            /* Swapped? No heart_beat()-lfun? Turn off the heart.
             */

            obj->flags &= ~O_HEART_BEAT;
            num_hb_objs--;
            if (this->prev)
                this->prev->next = this->next;
            if (this->next)
                this->next->prev = this->prev;
            if (this == hb_list)
                hb_list = this->next;
            this->next = free_list;
            free_list = this;
#ifdef DEBUG
            this->prev = NULL;
            this->obj = NULL;
#endif
        }
        else
        {
            /* Prepare to call <ob>->heart_beat().
             */
            current_prog = obj->prog;
            current_object = obj;
            current_heart_beat = obj;

            /* Determine the command_giver. Usually it's the object
             * itself, but it may be one of the shadows if there are
             * some. In any case it must be a living.
             */
            command_giver = obj;
            if (command_giver->flags & O_SHADOW)
            {
                struct shadow_sentence *shadow_sent;

                while(shadow_sent = O_GET_SHADOW(command_giver),
                      shadow_sent->shadowing)
                {
                    command_giver = shadow_sent->shadowing;
                }
                if (!(command_giver->flags & O_ENABLE_COMMANDS))
                {
                    command_giver = NULL;
                    trace_level = 0;
                }
                else
                {
                    trace_level =
                      shadow_sent->type == SENT_INTERACTIVE ?
                        ((struct interactive *)shadow_sent)->trace_level : 0;
                }
            }
            else
            {
                if (!(command_giver->flags & O_ENABLE_COMMANDS))
                    command_giver = NULL;
                trace_level = 0;
            }

            obj->user->heart_beats++;
            CLEAR_EVAL_COST;
            call_function(obj->prog, obj->prog->heart_beat);

        } /* if (object has heartbeat) */

        this = next_hb;
    } /* while (not done) */

    /* Update stats */
    avg_num_hb_objs += num_hb_to_do - (avg_num_hb_objs >> 10);
    avg_num_hb_done += hb_num_done  - (avg_num_hb_done >> 10);

    current_heart_beat = NULL;
} /* call_heart_beat() */

/*-------------------------------------------------------------------------*/
int
set_heart_beat (struct object *ob, /* TODO: BOOL */ int to)

/* EFUN set_heart_beat() and internal use.
 *
 * Add (<to> != 0) or remove (<to> == 0) object <ob> to/from the list
 * of heartbeat objects, thus activating/deactivating its heart beat.
 * Return 0 on failure (including calls for destructed objects or if
 * the object is already in the desired state) and 1 on success.
 *
 * The function is aware that it might be called from within a heart_beat()
 * and adjusts the correct pointers if that is so.
 */

{
    /* Safety checks */
    if (ob->flags & O_DESTRUCTED)
        return 0;
    if (to)
        to = O_HEART_BEAT; /* ...so that the following comparison works */
    if (to == (ob->flags & O_HEART_BEAT))
        return 0;

    if (to)  /* Add a new heartbeat */
    {
        struct hb_info *new;

        /* Get a new node */
        if (!free_list)
        {
            struct hb_block * new_block;
            int i;

            /* We need a whole new block */
            new_block = xalloc(sizeof(*new_block));
            if (!new_block)
                return 0;
            new_block->next = block_list;
            block_list = new_block;
            num_blocks++;

            /* Put the new nodes into the freelist */
            for (i = 0; i < NUM_NODES-1; i++)
                new_block->entry[i].next = &new_block->entry[i+1];
            new_block->entry[NUM_NODES-1].next = free_list;
            free_list = &new_block->entry[0];
        }
        new = free_list;
        free_list = free_list->next;

        new->obj = ob;

        /* Insert the new node at the proper place in the list */
        if (!hb_list || ob < hb_list->obj)
        {
            new->next = hb_list;
            new->prev = NULL;
            if (hb_list)
                hb_list->prev = new;
            hb_list = new;
        }
        else
        {
            struct hb_info *prev = hb_list;

            while (prev->next && ob > prev->next->obj)
                prev = prev->next;

            new->next= prev->next;
            new->prev = prev;
            prev->next = new;
            if (new->next)
                new->next->prev = new;
        }

        num_hb_objs++;
        ob->flags |= O_HEART_BEAT;
    }
    else  /* remove an existing heartbeat */
    {
        struct hb_info *this;

        /* Remove the node from the list */
        for (this = hb_list; this && this->obj != ob; this = this->next)
            NOOP;
#ifdef DEBUG
        if (!this)
            fatal("Object '%s' not found in heart beat list.\n", ob->name);
#endif
        if (this->next)
            this->next->prev = this->prev;
        if (this->prev)
            this->prev->next = this->next;
        if (this == hb_list)
            hb_list = this->next;
        if (this == next_hb)
            next_hb = this->next;

        /* ... and put it into the freelist */
        this->next = free_list;
        free_list = this;

        num_hb_objs--;
        ob->flags &= ~O_HEART_BEAT;
    }

    /* That's it */
    return 1;
}

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_smalloc

void
count_heart_beat_refs (void)

/* Count the reference to the hb_list array in a garbage collection.
 */

{
    struct hb_block *block;

    for (block = block_list; block; block = block->next)
        note_malloced_block_ref((char *)block);
}
#endif

/*-------------------------------------------------------------------------*/
int
heart_beat_status (int /* TODO: BOOL */ verbose)

/* If <verbose> is true, print the heart beat status information directly
 * to the current interactive user. In any case, return the amount of
 * memory used by the heart beat functions.
 */

{
    char buf[20];

    if (verbose) {
        add_message("\nHeart beat information:\n");
        add_message("-----------------------\n");
        add_message("Number of objects with heart beat: %ld, beats: %ld, reserved: %ld\n"
                   , (long)num_hb_objs, (long)num_hb_calls
                   , (long)num_blocks * NUM_NODES);
        add_message("HB calls completed in last cycle:  %ld (%.2f%%)\n"
                   , (long)hb_num_done
                   , num_hb_objs || hb_num_done > num_hb_objs
                     ? 100.0 * (float)hb_num_done / (float)num_hb_objs
                     : 100.0
                   );
        sprintf(buf, "%.2f",
          avg_num_hb_objs ?
            100 * (double) avg_num_hb_done / avg_num_hb_objs :
            100.0
        );
        add_message("Average of HB calls completed:     %s%%\n", buf);
    }
    return 0;
}

/*=========================================================================*/

/*                               EFUNS                                     */

/*-------------------------------------------------------------------------*/
struct svalue *
f_heart_beat_info (struct svalue *sp)

/* EFUN heart_beat_info()
 *
 * Create a vector of all objects with a heart beat and push it
 * onto the stack. The resulting vector may be empty.
 *
 * This efun is expensive!
 * TODO: Invent something like a hash table where all objects are store
 * TODO:: which had a heartbeat at least once. Repeated starts and stops
 * TODO:: of the heartbeat would be cheap, also deletion when an object
 * TODO:: is destructed.
 */

{
    int i;
    struct vector *vec;
    struct svalue *v;
    struct hb_info *this;

    vec = allocate_array(i = num_hb_objs);
    for (v = vec->item, this = hb_list; i >= 0 && this; this = this->next)
    {
#ifdef DEBUG
        if (this->obj->flags & O_DESTRUCTED)  /* TODO: Can't happen. */
            continue;
#endif
        v->type = T_OBJECT;
        v->u.ob = this->obj;
        v++;
        i--;
        add_ref(this->obj, "heart_beat_info");
    }
    sp++;
    sp->type = T_POINTER;
    sp->u.vec = vec;
    return sp;
}

/***************************************************************************/

#else /* ******************************************************************/

#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef AMIGA
#include "hosts/amiga/nsignal.h"
#else
#include <signal.h>
#include <sys/times.h>
#endif
#include <math.h>

#define NO_INCREMENT_STRING_REF
#include "heartbeat.h"

#include "array.h"
#include "backend.h"
#include "datatypes.h"
#include "comm.h"
#include "gcollect.h"
#include "object.h"
#include "simulate.h"
#include "wiz_list.h"

/*-------------------------------------------------------------------------*/

#define ALARM_TIME  2  /* The granularity of alarm() calls */

/*-------------------------------------------------------------------------*/

struct object *current_heart_beat = NULL;
  /* The object whose heart_beat() is currently executed.
   * interpret.c needs to know this for the heart-beat tracing, and
   * simulate.c test this in the error() function to react properly.
   */

/*-------------------------------------------------------------------------*/

/* The 'ring list' of objects with heart beats.
 *
 * Actually it is an array of <hb_max> (struct object *), pointing to the
 * objects. hb_list points to the beginning of the array, hb_tail to the
 * currently last used entry.
 *
 * hb_last_called and hb_last_to_call determine which objects' heart_beat()
 * needs to be called next. Depending on the context, hb_last_called ==
 * hb_last_to_call can mean 'all objects called' or 'no objects called'.
 *
 * In every round as many objects are called as possible, and the latter
 * two pointers are adjusted accordingly. This way, the next round can take
 * off where the first one left off.
 */

static struct object **hb_list = NULL; /* head of the array */
static struct object **hb_tail = NULL; /* for sane wrap around */

static struct object **hb_last_called, **hb_last_to_call;

static mp_int num_hb_objs = 0; /* current number of objects in list */
static mp_int num_hb_to_do;    /* number of objects to do this round */
static mp_int hb_num_done;     /* number of objects done this round */
static mp_int hb_max = 0;      /* max size of the allocated array */

static long num_hb_calls = 0; /* stats */
static long avg_num_hb_objs = 0, avg_num_hb_done = 0; /* decaying average */

/*-------------------------------------------------------------------------*/
void
call_heart_beat (void)

/* Call the heart_beat() lfun in all registered heart beat objects; or at
 * at least call as many as possible until the next alarm() timeout
 * occurs.
 *
 * If the object in question (or one of its shadows) is living, command_giver
 * is set to the object, else it is set to NULL. If there are no players
 * in the game, no heart_beat() will be called (but the call outs will!).
 */

{
    struct object *ob;

    /* Housekeeping */

    time_to_call_heart_beat = MY_FALSE; /* interrupt loop if we take too long */
    comm_time_to_call_heart_beat = MY_FALSE;
    alarm(ALARM_TIME);

    current_interactive = NULL;

    /* Set this new round through the hb list */
    hb_last_to_call = hb_last_called;
    hb_num_done = 0;
    if (num_player > 0 && 0 != (num_hb_to_do = num_hb_objs))
    {
        num_hb_calls++;
        while (!comm_time_to_call_heart_beat)
        {
            /* Step to the next object to call, wrapping
             * around the end of the list if necessary.
             * Objects without hb lfun are skipped.
             */
            hb_num_done++;
            if (++hb_last_called == hb_tail)
                hb_last_called = hb_list;
            ob = *hb_last_called;

#ifdef DEBUG
            if (!(ob->flags & O_HEART_BEAT))
                fatal("Heart beat not set in object on heart beat list!");
            if (ob->flags & O_SWAPPED)
                fatal("Heart beat in swapped object.\n");
#endif

            if (ob->prog->heart_beat == -1) {
                /* Swapped? No heart_beat()-lfun? TODO: Dunno */
                if (hb_num_done == num_hb_to_do)
                    break;
                continue;
            }

            /* Prepare to call <ob>->heart_beat().
             */
            current_prog = ob->prog;
            current_object = ob;
            current_heart_beat = ob;

            command_giver = ob;
            if (command_giver->flags & O_SHADOW) {
                struct shadow_sentence *shadow_sent;

                while(shadow_sent = O_GET_SHADOW(command_giver),
                      shadow_sent->shadowing)
                {
                    command_giver = shadow_sent->shadowing;
                }
                if (!(command_giver->flags & O_ENABLE_COMMANDS)) {
                    command_giver = 0;
                    trace_level = 0;
                } else {
                    trace_level =
                      shadow_sent->type == SENT_INTERACTIVE ?
                        ((struct interactive *)shadow_sent)->trace_level : 0;
                }
            } else {
                if (!(command_giver->flags & O_ENABLE_COMMANDS))
                    command_giver = 0;
                trace_level = 0;
            }

            ob->user->heart_beats++;
            CLEAR_EVAL_COST;
            call_function(ob->prog, ob->prog->heart_beat);

            /* (hb_last_called == hb_last_to_call) is not a sufficient
             * condition, since the first object with heart beat might
             * call set_heart_beat(0) in the heart beat, causing
             * hb_last_to_call to move.
             */
            if (hb_num_done == num_hb_to_do)
                break;
        } /* while (no timeout) */

        /* Update stats */
        avg_num_hb_objs += num_hb_to_do - (avg_num_hb_objs >> 10);
        avg_num_hb_done += hb_num_done  - (avg_num_hb_done >> 10);
    }
    current_heart_beat = 0;
}

/*-------------------------------------------------------------------------*/
int
set_heart_beat (struct object *ob, /* TODO: BOOL */ int to)

/* EFUN set_heart_beat() and internal use.
 *
 * Add (<to> != 0) or remove (<to> == 0) object <ob> to/from the list
 * of heartbeat objects, thus activating/deactivating its heart beat.
 * Return 0 on failure (including calls for destructed objects or if
 * the object is already in the desired state) and 1 on success.
 *
 * The function is aware that it might be called from within a heart_beat()
 * and adjusts the correct pointers if that is so.
 */

{
    /* Safety checks */
    if (ob->flags & O_DESTRUCTED)
        return 0;
    if (to)
        to = O_HEART_BEAT; /* ...so that the following comparison works */
    if (to == (ob->flags & O_HEART_BEAT))
        return 0;

    if (to)
    {

        /*
         * --- Add <ob> to the list of heartbeat objects ---
         *
         * The new object will be added right after hb_last_called,
         * and hb_last_called will then be set to point at it.
         */

        struct object **new_op;

        if (++num_hb_objs > hb_max) {

            /* We need to (re)allocate more memory for the array. */

            if (!hb_max) {

                /* First allocation */

                hb_max = 16;
                hb_list =
                  (struct object **)xalloc(hb_max * sizeof(struct object **));
                if (!hb_list) {
                    hb_max = 0;
                    return 0;
                }
                hb_last_called = hb_last_to_call = (hb_tail = hb_list) - 1;

            } else {

                /* Double the size of the current allocation */

                struct object **new;
                ptrdiff_t tail_diff, called_diff, to_call_diff;

                tail_diff    = hb_tail         - hb_list;
                called_diff  = hb_last_called  - hb_list;
                to_call_diff = hb_last_to_call - hb_list;

                hb_max <<= 1;
                new = (struct object **)
                  rexalloc((char *)hb_list, hb_max * sizeof(struct object **));
                if (!new) {
                    hb_max >>= 1;
                    return 0;
                }

                /* Adjust the hb_* ptr to point into the new memory block. */
                hb_list = new;
                hb_tail         = new + tail_diff;
                hb_last_called  = new + called_diff;
                hb_last_to_call = new + to_call_diff;
            }
        }

        ob->flags |= O_HEART_BEAT;
        new_op = ++hb_last_called;
        move_memory(
          (char *)(new_op+1),
          (char *)new_op,
          (char *)hb_tail++ - (char *)new_op
        );
        *new_op = ob;
        if (hb_last_to_call >= new_op)
            hb_last_to_call++;
    }
    else {

        /*
         * --- Remove <ob> from the list of heartbeat objects ---
         */

        struct object **op;
        int active;

        ob->flags &= ~O_HEART_BEAT;

        /* Search the object in the list and remove it. */
        for (op = hb_list; *op != ob; op++) NOOP;
        move_memory(
          (char *)op,
          (char *)(op+1),
          (char *)hb_tail-- - (char *)(op+1)
        );

        /* Check which pointers need to be adjusted */
        active = hb_last_called >= hb_last_to_call;
        if (hb_last_called >= op) {
            hb_last_called--;
            active ^= 1;
        }
        if (hb_last_to_call >= op) {
            hb_last_to_call--;
            active ^= 1;
        }
        /* hb_last_called == hb_last_to_call can mean either all called or
         * all to be called - if the first object did a set_heart_beat(0) .
         * If we decremented num_hb_to_do anyways, the statistics would
         * be wrong.
         */
        if (num_hb_to_do > hb_num_done)
            num_hb_to_do -= active;

        num_hb_objs--;
    }

    /* That's it */
    return 1;
}

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_smalloc

void
count_heart_beat_refs (void)

/* Count the reference to the hb_list array in a garbage collection.
 */

{
    if (hb_list)
        note_malloced_block_ref((char *)hb_list);
}
#endif

/*-------------------------------------------------------------------------*/
int
heart_beat_status (int /* TODO: BOOL */ verbose)

/* If <verbose> is true, print the heart beat status information directly
 * to the current interactive user. In any case, return the amount of
 * memory used by the heart beat functions.
 */

{
    char buf[20];

    if (verbose) {
        add_message("\nHeart beat information:\n");
        add_message("-----------------------\n");
        add_message("Number of objects with heart beat: %ld, starts: %ld, reserved %ld\n",
                    (long)num_hb_objs, (long)num_hb_calls, (long)hb_max);
        sprintf(buf, "%.2f",
          avg_num_hb_objs ?
            100 * (double) avg_num_hb_done / avg_num_hb_objs :
            100.0
        );
        add_message("Average of HB calls completed:     %s%%\n", buf);
    }
    return 0;
}

/*=========================================================================*/

/*                               EFUNS                                     */

/*-------------------------------------------------------------------------*/
struct svalue *
f_heart_beat_info (struct svalue *sp)

/* EFUN heart_beat_info()
 *
 * Create a vector of all objects with a heart beat and push it
 * onto the stack. The resulting vector may be empty.
 *
 * This efun is expensive!
 */

{
    int i;
    struct object *ob, **op;
    struct vector *vec;
    struct svalue *v;

    vec = allocate_array(i = num_hb_objs);
    for (v = vec->item, op = hb_list; --i >= 0; v++) {
        v->type = T_OBJECT;
        v->u.ob = ob = *op++;
        add_ref(ob, "heart_beat_info");
    }
    sp++;
    sp->type = T_POINTER;
    sp->u.vec = vec;
    return sp;
}

/***************************************************************************/
#endif /* OLD_RESET */

