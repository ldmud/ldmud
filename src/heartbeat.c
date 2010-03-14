/*---------------------------------------------------------------------------
 * Heartbeat Management
 *
 *---------------------------------------------------------------------------
 * This module holds the datastructures and function related to the
 * handling of heartbeats.
 *
 * Objects with active heartbeats are referenced from a list which
 * is sorted in ascending order of the object pointers. However, these
 * object pointers do not count as 'refs'. The sorting prevents that
 * objects with a rapid change of the heartbeat status are called more
 * often than others.
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
 * TODO:: in the list until call_heart_beat() can remove it. This would
 * TODO:: also remove the need for a double-linked or skiplist, but
 * TODO:: require the object-pointer to count as ref and it could let
 * TODO:: keep destructed objects in the list for a while.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <math.h>

#include "heartbeat.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "comm.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "mstrings.h"
#include "object.h"
#include "sent.h"
#include "simulate.h"
#include "strfuns.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/debug_info.h"

/*-------------------------------------------------------------------------*/

/* Listnode for one object with a heartbeat
 * It is no use pooling the nodes to reduce allocation overhead, as
 * the average heartbeat use is usually much lower than the peak usage.
 */

struct hb_info {
    struct hb_info * next;  /* next node in list */
    struct hb_info * prev;  /* previous node in list */
    mp_int           tlast; /* time of last heart_beat */
    object_t       * obj;   /* the object itself */
};


/*-------------------------------------------------------------------------*/

object_t *current_heart_beat = NULL;
  /* The object whose heart_beat() is currently executed. It is NULL outside
   * of heartbeat executions.
   *
   * interpret.c needs to know this for the heart-beat tracing, and
   * simulate.c test this in the errorf() function to react properly.
   */

static struct hb_info * hb_list = NULL;
  /* Head of the list of heart_beat infos.
   */

static struct hb_info * next_hb = NULL;
  /* Next hb_info whose objects' heartbeat must be executed.
   * If NULL, the first info in the list is meant.
   */

#if defined(DEBUG)
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

static long total_hb_calls = 0;
  /* Total number of calls to call_heart_beat().
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
 * is set to the object, else it is set to NULL. If heart_beats_active is 
 * false, no heart_beat() will be called (but the call outs will!).
 *
 * The function does not change the time_to_call-flags or messes with alarm().
 * It may be aborted prematurely if an error occurs during the heartbeat.
 */

{
    struct hb_info *this;
      /* Current list pointer, static so that longjmp() won't clobber it */

    static mp_int num_hb_to_do;
      /* For statistics only */

    struct error_recovery_info error_recovery_info;

    /* Housekeeping */

    current_interactive = NULL;
    total_hb_calls++;

    /* Set this new round through the hb list */
    hb_num_done = 0;

    if (!heart_beats_enabled || !num_hb_objs)
    {
        next_hb = NULL;
        return;
    }

    num_hb_to_do = num_hb_objs;
    num_hb_calls++;

    /* Activate the local error recovery context */

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_BACKEND;
    rt_context = (rt_context_t *)&error_recovery_info.rt;

    if (setjmp(error_recovery_info.con.text))
    {
        /* An error occured: recover. The guilt heartbeat has already
         * been removed by simulate::error().
         */
        mark_end_evaluation();
        clear_state();
        debug_message("%s Error in heartbeat.\n", time_stamp());
    }

    /* Set this to the next hb to be execute.
     * This is the loop invariant.
     */
    this = next_hb;

    while (num_hb_objs && !comm_time_to_call_heart_beat)
    {
        object_t * obj;

        /* If 'this' object is NULL, we reached the end of the
         * list and have to wrap around.
         */
        if (!this)
        {
#ifdef DEBUG
            if (!hb_list)
                fatal("hb_list is NULL, but num_hb_objs is %ld\n", (long)num_hb_objs);
#endif
            this = hb_list;
        }

        /* Check the time of the last heartbeat to see, if we
         * processed all objects.
         */
        if (current_time < this->tlast + heart_beat_interval)
            break;

        this->tlast = current_time;

        obj = this->obj;
        next_hb = this->next;

        hb_num_done++;

#ifdef DEBUG
        if (!(obj->flags & O_HEART_BEAT))
            fatal("Heart beat not set in object '%s' on heart beat list!\n"
                 , get_txt(obj->name)
                 );
        if (obj->flags & O_SWAPPED)
            fatal("Heart beat in swapped object '%s'.\n", get_txt(obj->name));
        if (obj->flags & O_DESTRUCTED)
            fatal("Heart beat in destructed object '%s'.\n", get_txt(obj->name));
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

            xfree(this);
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
                shadow_t *shadow_sent;

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
                    trace_level = shadow_sent->ip
                                  ? shadow_sent->ip->trace_level
                                  : 0;
                }
            }
            else
            {
                if (!(command_giver->flags & O_ENABLE_COMMANDS))
                    command_giver = NULL;
                trace_level = 0;
            }

            mark_start_evaluation();
            obj->user->heart_beats++;
            CLEAR_EVAL_COST;
            RESET_LIMITS;
            call_function(obj->prog, obj->prog->heart_beat);
            mark_end_evaluation();

        } /* if (object has heartbeat) */

        /* Step to next object with heart beat */
        this = next_hb;
    } /* while (not done) */

    rt_context = error_recovery_info.rt.last;

    /* Update stats */
    avg_num_hb_objs += num_hb_to_do - (avg_num_hb_objs >> 10);
    avg_num_hb_done += hb_num_done  - (avg_num_hb_done >> 10);

    current_heart_beat = NULL;
    current_prog = NULL;
} /* call_heart_beat() */

/*-------------------------------------------------------------------------*/
int
set_heart_beat (object_t *ob, Bool to)

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
        new = xalloc(sizeof(*new));

        new->tlast = current_time;
        new->obj = ob;

        /* Insert the new node at the proper place in the list */
        if (!hb_list || !next_hb || !next_hb->prev)
        {
            new->next = hb_list;
            new->prev = NULL;
            if (hb_list)
                hb_list->prev = new;
            hb_list = new;
	    
	    /* Handle all the other objects first. */
	    if(!next_hb)
		next_hb = new->next;
        }
        else
        {
	    /* Insert it just before next_hb, so this is the last object. */
            new->next = next_hb;
            new->prev = next_hb->prev;
            next_hb->prev->next = new;
	    next_hb->prev = new;
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
            fatal("Object '%s' not found in heart beat list.\n", get_txt(ob->name));
#endif
        if (this->next)
            this->next->prev = this->prev;
        if (this->prev)
            this->prev->next = this->next;
        if (this == hb_list)
            hb_list = this->next;
        if (this == next_hb)
            next_hb = this->next;

        xfree(this);

        num_hb_objs--;
        ob->flags &= ~O_HEART_BEAT;
    }

    /* That's it */
    return 1;
} /* set_heart_beat() */

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
count_heart_beat_refs (void)

/* Count the reference to the hb_list array in a garbage collection.
 */

{
    struct hb_info *this;

    for (this = hb_list; this != NULL; this = this->next)
        note_malloced_block_ref(this);
}
#endif

/*-------------------------------------------------------------------------*/
int
heart_beat_status (strbuf_t * sbuf, Bool verbose)

/* If <verbose> is true, print the heart beat status information directly
 * to the current interactive user. In any case, return the amount of
 * memory used by the heart beat functions.
 */

{
    if (verbose) {
        strbuf_add(sbuf, "\nHeart beat information:\n");
        strbuf_add(sbuf, "-----------------------\n");
        strbuf_addf(sbuf, "Number of objects with heart beat: %ld, "
                          "beats: %ld\n"
                   , (long)num_hb_objs, (long)num_hb_calls);
#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_largeargs off
#endif
        strbuf_addf(sbuf, "HB calls completed in last cycle:  %ld (%.2f%%)\n"
                   , (long)hb_num_done
                   , num_hb_objs && hb_num_done <= num_hb_objs
                     ? 100.0 * (float)hb_num_done / (float)num_hb_objs
                     : 100.0
                   );
        strbuf_addf(sbuf
                   , "Average of HB calls completed:     %.2f%%\n"
                   , avg_num_hb_objs ?
                     100 * (double) avg_num_hb_done / avg_num_hb_objs :
                     100.0
                   );
#if defined(__MWERKS__)
#    pragma warn_largeargs reset
#endif
    }
    return num_hb_objs * sizeof(struct hb_info);
} /* heart_beat_status() */

/*-------------------------------------------------------------------------*/
void
hbeat_dinfo_status (svalue_t *svp, int value)

/* Return the heartbeat information for debug_info(DINFO_DATA, DID_STATUS).
 * <svp> points to the svalue block for the result, this function fills in
 * the spots for the object table.
 * If <value> is -1, <svp> points indeed to a value block; other it is
 * the index of the desired value and <svp> points to a single svalue.
 */

{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

#define ST_DOUBLE(which,code) \
    if (value == -1) { \
        svp[which].type = T_FLOAT; \
        STORE_DOUBLE(svp+which, code); \
    } else if (value == which) { \
        svp->type = T_FLOAT; \
        STORE_DOUBLE(svp, code); \
    }

    STORE_DOUBLE_USED;

    ST_NUMBER(DID_ST_HBEAT_OBJS, num_hb_objs);
    ST_NUMBER(DID_ST_HBEAT_CALLS, num_hb_calls);
    ST_NUMBER(DID_ST_HBEAT_CALLS_TOTAL, total_hb_calls);
    ST_NUMBER(DID_ST_HBEAT_SLOTS, num_hb_objs);
    ST_NUMBER(DID_ST_HBEAT_SIZE, num_hb_objs * sizeof(struct hb_info));
    ST_NUMBER(DID_ST_HBEAT_PROCESSED, hb_num_done);
    ST_DOUBLE(DID_ST_HBEAT_AVG_PROC
             , avg_num_hb_objs
               ? ((double)avg_num_hb_done / avg_num_hb_objs)
               : 1.0
             );

#undef ST_NUMBER
#undef ST_DOUBLE
} /* hbeat_dinfo_status() */

/*=========================================================================*/

/*                               EFUNS                                     */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_heart_beat (svalue_t *sp)

/* EFUN set_heart_beat()
 *
 *   int set_heart_beat(int flag)
 *
 * Enable or disable heart beat. The driver will apply
 * the lfun heart_beat() to the current object every 2 seconds,
 * if it is enabled. If the heart beat is not needed for the
 * moment, then do disable it. This will reduce system overhead.
 *
 * Return true for success, and false for failure.
 *
 * Disabling an already disabled heart beat (and vice versa
 * enabling and enabled heart beat) counts as failure.
 */

{
    int i;

    i = set_heart_beat(current_object, sp->u.number != 0);
    sp->u.number = i;

    return sp;
} /* f_set_heart_beat() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_heart_beat_info (svalue_t *sp)

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
    vector_t *vec;
    svalue_t *v;
    struct hb_info *this;

    vec = allocate_array(i = num_hb_objs);
    for (v = vec->item, this = hb_list; i >= 0 && this; this = this->next)
    {
#ifdef DEBUG
        if (this->obj->flags & O_DESTRUCTED)  /* TODO: Can't happen. */
            continue;
#endif
        put_ref_object(v, this->obj, "heart_beat_info");
        v++;
        i--;
    }

    push_array(sp, vec);
    return sp;
} /* f_heart_beat_info() */

/***************************************************************************/

