/*---------------------------------------------------------------------------
 * Gamedriver - Garbage Collection
 *
 *---------------------------------------------------------------------------
 * The garbage collection is used in times of memory shortage (or on request)
 * to find and deallocate any unused objects, arrays, memory blocks, or
 * whatever. The purpose is to detect and get rid of unreachable data (like
 * circular array references), but the collector is also a last line of
 * defense against bug-introduced memory leaks.
 *
 * This facility is available currently only when using the 'smalloc'
 * memory allocator. When using a different allocator, all garbage_collect()
 * does is freeing as much memory as possible.
 *
 * Additionally this module also offers a couple of functions to 'clean up'
 * an object, ie. to scan all data referenced by this object for destructed
 * objects and remove those references, and to change all untabled strings
 * into tabled strings. These functions are used by the garbage collector
 * to deallocate as much memory by normal means as possible; but they
 * are also called from the backend as part of the regular reset/swap/cleanup
 * handling.
 *
#ifdef GC_SUPPORT
 * The garbage collector is a simple mark-and-sweep collector. First, all
 * references (refcounts and memory block markers) are cleared, then in
 * a second pass, all reachable references are recreated (refcounts are
 * incremented, memory blocks marked as used). The last pass then looks
 * for all allocated but unmarked memory blocks: these are provably
 * garbage and can be given back to the allocator. For debugging purposes,
 * the collected memory blocks can be printed onto a file for closer
 * inspection.
 *
 * In order to do its job, the garbage collector calls functions clear_...
 * and count_... in all the other driver modules, where they are supposed
 * to perform their clearing and counting operations. To aid the other
 * modules in this, the collector offers a set of primitives to clearing
 * and marking:
 *
 *     int clear_memory_reference(void *p)
 *         Clear the memory block marker for <p>.
 *
 *     void note_malloced_block_ref(void *p)
 *         Note the reference to memory block <p>.
 *
 *     void clear_program_ref(program_t *p, Bool clear_ref)
 *         Clear the refcounts of all inherited programs and other
 *         data of <p>. If <clear_ref> is TRUE, the refcounts of
 *         <p> itself and of <p>->name are cleared, too.
 *
 *     void clear_object_ref(object_t *p)
 *         Make sure that the refcounts in object <p> are cleared.
 *
 *     void mark_program_ref(program_t *p);
 *         Set the marker of program <p> and of all data referenced by <p>.
 *
 *     void reference_destructed_object(object_t *ob)
 *         Note the reference to a destructed object <ob>.
 *
 *     void clear_string_ref(string_t *p)
 *         Clear the refcount in string <p>.
 *
 *     void count_ref_from_string(string_t *p);
 *         Count the reference to string <p>.
 *
 *     void clear_ref_in_vector(svalue_t *svp, size_t num);
 *         Clear the refs of the <num> elements of vector <svp>.
 *
 *     void count_ref_in_vector(svalue_t *svp, size_t num)
 *         Count the references the <num> elements of vector <p>.
 *
 * The referencing code for dynamic data should mirror the destructor code,
 * thus, memory leaks can show up as soon as the memory is allocated.
 *
 * TODO: Allow to deactivate the dump of unreferenced memory on freeing.
#endif
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>
#include <stdio.h>

#include "gcollect.h"
#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "efuns.h"
#include "filestat.h"
#include "heartbeat.h"
#include "interpret.h"
#if defined(GC_SUPPORT) && defined(MALLOC_TRACE)
#include "instrs.h" /* Need F_ALLOCATE for setting up print dispatcher */
#endif
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "mempools.h"
#include "mregex.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#include "parse.h"
#include "pkg-pgsql.h"
#include "prolang.h"
#include "ptrtable.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#ifdef USE_STRUCTS
#include "structs.h"
#endif /* USE_STRUCTS */
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/driver_hook.h"

/*-------------------------------------------------------------------------*/

time_t time_last_gc = 0;
  /* Time of last gc, used by the backend to avoid repeated collections
   * when the memory usage is at the edge of a shortage.
   */


#if defined(GC_SUPPORT)

int default_gcollect_outfd = 2;
  /* The default file (default is stderr) to dump the reclaimed blocks on.
   */

int gcollect_outfd = 2;
#define gout gcollect_outfd
  /* The current file (default is stderr) to dump the reclaimed blocks on.
   * After the GC, this will be reset to <default_gcollect_outfd>.
   */

gc_status_t gc_status = gcInactive;
  /* The current state of the garbage collection.
   * swap uses this information when swapping in objects.
   */

object_t *gc_obj_list_destructed;
  /* List of referenced but destructed objects.
   * Scope is global so that the GC support functions in mapping.c can
   * add their share of information.
   */

lambda_t *stale_misc_closures;
  /* List of non-lambda closures bound to a destructed object.
   * The now irrelevant .ob pointer is used to link the list elements.
   * Scope is global so that the GC support functions in mapping.c can
   * add their share of information.
   */

static lambda_t *stale_lambda_closures;
  /* List of lambda closures bound to a destructed object.
   * The now irrelevant .ob pointer is used to link the list elements.
   */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/

/*=========================================================================*/

/*            Object clean up
 */

#ifdef NEW_CLEANUP

typedef struct cleanup_s cleanup_t;
typedef struct cleanup_map_extra_s cleanup_map_extra_t;


/* --- struct cleanup: The cleanup context information
 *
 * The controlling instance of this struct is passed to all cleanup functions.
 */
struct cleanup_s
{
    ptrtable_t * ptable;
      /* Pointertable to catch loops in the variable values.
       * This table is re-allocated whenever the object cleant up is
       * swapped, as the swapper can re-use the already listed memory
       * blocks.
       */
    unsigned long numValues; /* Number of values examined. */
    Bool         mcompact;  /* TRUE: mappings are forcibly compacted */
    ptrtable_t * mtable;
      /* Pointertable of mappings to compact.
       * This table holds all mappings listed in mlist so that multiple
       * encounters of the same dirty mapping are easily recognized.
       */
    mapping_t  * mlist;
      /* List of mappings to compact. The list is linked through
       * the map->next field.
       * The references are counted to prevent premature freeing.
       */
};


/* --- struct cleanup_map_extra: Info needed to clean up a mapping.
 *
 * A pointer to an instance of this structure is passed to the
 * cleanup_mapping_filter() callback function.
 */

struct cleanup_map_extra_s
{
    p_int       width;    /* Width of the mapping */
    cleanup_t  *context;  /* The cleanup context */
};


/* Forward declarations */
static void cleanup_vector (svalue_t *svp, size_t num, cleanup_t * context);

/*-------------------------------------------------------------------------*/
static cleanup_t *
cleanup_new (Bool mcompact)

/* Create a new cleanup_t context and return it. <mcompact> is the 
 * .mcompact setting.
 */

{
    cleanup_t * rc;

    rc = xalloc(sizeof(*rc));
    if (rc == NULL)
    {
        outofmemory("object cleanup context");
        return NULL;
    }

    rc->ptable = new_pointer_table();
    if (rc->ptable == NULL)
    {
        xfree(rc);
        outofmemory("object cleanup pointertable");
        return NULL;
    }

    rc->mtable = new_pointer_table();
    if (rc->mtable == NULL)
    {
        free_pointer_table(rc->ptable);
        xfree(rc);
        outofmemory("object cleanup pointertable");
        return NULL;
    }

    rc->mcompact = mcompact;
    rc->mlist = NULL;
    rc->numValues = 0;

    return rc;
} /* cleanup_new() */

/*-------------------------------------------------------------------------*/
static Bool
cleanup_reset (cleanup_t * context)

/* Reallocate the pointertable in the context.
 * Return TRUE if successful, and FALSE when out of memory.
 */

{
    if (context->ptable)
        free_pointer_table(context->ptable);

    context->ptable = new_pointer_table();
    if (context->ptable == NULL)
    {
        outofmemory("object cleanup pointertable");
        return MY_FALSE;
    }

    return MY_TRUE;
} /* cleanup_reset() */

/*-------------------------------------------------------------------------*/
static void
cleanup_free (cleanup_t * context)

/* Deallocate the cleanup context <context>.
 */

{
    if (context->ptable)
        free_pointer_table(context->ptable);
    free_pointer_table(context->mtable);
    xfree(context);
} /* cleanup_free() */

/*-------------------------------------------------------------------------*/
static void
cleanup_mapping_filter (svalue_t *key, svalue_t *data, void *extra)

/* Clean up a single mapping element, <extra> points to
 * a cleanup_map_extra_t instance.
 */

{
    cleanup_map_extra_t * pData = (cleanup_map_extra_t*)extra;
    cleanup_vector(key, 1, pData->context);
    cleanup_vector(data, pData->width, pData->context);
} /* cleanup_mapping_filter() */

/*-------------------------------------------------------------------------*/
static void
cleanup_closure (svalue_t *csvp, cleanup_t * context)

/* Cleanup the closure <csvp>, using <context> as cleanup context.
 * This may change *<csvp> to svalue-0.
 */

{
    ph_int    type = csvp->x.closure_type;
    lambda_t *l    = csvp->u.lambda;

    /* If this closure is bound to or defined in a destructed object, zero it
     * out.
     */
    if (destructed_object_ref(csvp))
    {
        free_closure(csvp);
        put_number(csvp, 0);
        return;
    }

    if (!CLOSURE_MALLOCED(type)
     || register_pointer(context->ptable, l) == NULL
       )
        return;

    /* If the creating program has been destructed, zero out the reference.
     */
    if (CLOSURE_MALLOCED(type)
     && l->prog_ob
     && (l->prog_ob->flags & O_DESTRUCTED))
    {
        free_object(l->prog_ob, "cleanup_closure");
        l->prog_ob = NULL;
        l->prog_pc = 0;
    }

    if (CLOSURE_HAS_CODE(type))
    {
        mp_int num_values;
        svalue_t *svp;

        svp = (svalue_t *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0x100].u.number;
        svp -= num_values;
        cleanup_vector(svp, (size_t)num_values, context);
    }
    else if (type == CLOSURE_BOUND_LAMBDA)
    {
        svalue_t dummy;

        dummy.type = T_CLOSURE;
        dummy.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        dummy.u.lambda = l->function.lambda;

        cleanup_closure(&dummy, context);
    }

#ifdef USE_NEW_INLINES
    if (type == CLOSURE_LFUN && l->function.lfun.context_size != 0)
    {
        unsigned short size = l->function.lfun.context_size;
        cleanup_vector(l->context, size, context);
    }
#endif /* USE_NEW_INLINES */
} /* cleanup_closure() */

/*-------------------------------------------------------------------------*/
static void
cleanup_vector (svalue_t *svp, size_t num, cleanup_t * context)

/* Cleanup the <num> svalues in vector/svalue block <svp>.
 * <context> is used to keep track which complex values we already
 * cleaned up.
 */

{
    svalue_t *p;

    if (!svp) /* e.g. when called for obj->variables */
        return;

    if (register_pointer(context->ptable, svp) == NULL) /* already cleaned up */
        return;

    for (p = svp; p < svp+num; p++)
    {
        context->numValues++;
        switch(p->type)
        {
        case T_OBJECT:
          {
            if (p->u.ob->flags & O_DESTRUCTED)
            {
                free_object(p->u.ob, "cleanup svalues");
                put_number(p, 0);
            }
            break;
          }

        case T_POINTER:
        case T_QUOTED_ARRAY:
            /* Don't clean the null vector */
            if (p->u.vec != &null_vector)
            {
                cleanup_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec), context);
            }
            break;

#ifdef USE_STRUCTS
        case T_STRUCT:
            cleanup_vector(&p->u.strct->member[0], struct_size(p->u.strct), context);
            break;
#endif /* USE_STRUCTS */

        case T_MAPPING:
            if (register_pointer(context->ptable, p->u.map) != NULL)
            {
                cleanup_map_extra_t extra;

                extra.width = p->u.map->num_values;
                extra.context = context;
                check_map_for_destr(p->u.map); // Need to check both keys and values
                walk_mapping(p->u.map, cleanup_mapping_filter, &extra);

                /* Remember the mapping for later compaction (unless
                 * we have it already).
                 * Only 'dirty' mappings need to be listed, as the cleanup
                 * can't cause a mapping to add a hash part.
                 */
                if (p->u.map->hash
                 && NULL != register_pointer(context->mtable, p->u.map)
                  )
                {
                    p->u.map->next = context->mlist;
                    context->mlist = ref_mapping(p->u.map);
                }
            }
            break;

        case T_STRING:
            if (!mstr_tabled(p->u.str))
                p->u.str = make_tabled(p->u.str);
            break;

        case T_CLOSURE:
            cleanup_closure(p, context);
            break;
        }
    } /* for */
} /* cleanup_vector() */

/*-------------------------------------------------------------------------*/
static Bool
cleanup_single_object (object_t * obj, cleanup_t * context)

/* Cleanup object <ob> using context <context>.
 *
 * If the object is on the swap, it will be swapped in for the time
 * of the function.
 *
 * Return FALSE if the objects was present in memory, and TRUE if it
 * had to be swapped in.
 *
 * The function checks all variables of this object for references
 * to destructed objects and removes them. Also, untabled strings
 * are made tabled.
 */

{
    int was_swapped = 0;

    /* Swap in the object if necessary */
    if ((obj->flags & O_SWAPPED)
     && (was_swapped = load_ob_from_swap(obj)) < 0)
    {
        errorf("(%s:%d) Out of memory swapping in %s\n", __FILE__, __LINE__
             , get_txt(obj->name));
        return MY_FALSE;
    }


    /* If the object's program blueprint is destructed, remove that
     * reference.
     */

    if (obj->prog->blueprint
     && (obj->prog->blueprint->flags & O_DESTRUCTED)
       )
    {
        free_object(obj->prog->blueprint, "cleanup object");
        obj->prog->blueprint = NULL;
        remove_prog_swap(obj->prog, MY_TRUE);
    }

    /* Clean up all the variables */
    cleanup_vector(obj->variables, obj->prog->num_variables, context);

    /* Clean up */
    if (was_swapped)
    {
        swap(obj, was_swapped);
    }

    return was_swapped != 0;
} /* cleanup_single_object() */

/*-------------------------------------------------------------------------*/
static void
cleanup_structures (cleanup_t * context)

/* Cleanup the value holding structures of the driver, using context
 * <context>.
 *
 * The structures are:
 *   - driver hooks
 *   - wizlist extra entries
 */

{
    /* Cleanup the wizlist */
    {
        wiz_list_t * wiz;

        for (wiz = all_wiz; wiz != NULL; wiz = wiz->next)
            cleanup_vector(&wiz->extra, 1, context);
        
        cleanup_vector(&default_wizlist_entry.extra, 1, context);
    }

    /* Cleanup the driver hooks.
     * We have to be careful here to not free the lambda-closure hooks even
     * if they are bound to destructed objects.
     */
    {
        int i;

        for (i = 0; i < NUM_DRIVER_HOOKS; i++)
        {
            if (driver_hook[i].type == T_CLOSURE
             && (   driver_hook[i].x.closure_type == CLOSURE_LAMBDA
                 || driver_hook[i].x.closure_type == CLOSURE_BOUND_LAMBDA
                )
               )
            {
                if (destructed_object_ref(&driver_hook[i]))
                {
                    lambda_t * l = driver_hook[i].u.lambda;

                    free_object(l->ob, "cleanup_structures");
                    l->ob = ref_object(master_ob, "cleanup_structures");
                }
            }
            else
                cleanup_vector(&driver_hook[i], 1, context);
        }
    }
} /* cleanup_structures() */

/*-------------------------------------------------------------------------*/
static void
cleanup_compact_mappings (cleanup_t * context)

/* Compact all mappings listed in the <context>.
 * This must be the very last action in the cleanup process.
 */

{
    mapping_t * m;

    for (m = context->mlist; m != NULL; m = context->mlist)
    {
        context->mlist = m->next;
        if (m->ref > 1)
            compact_mapping(m, context->mcompact);
        free_mapping(m); /* Might deallocate it fully */
    }
} /* cleanup_compact_mappings() */

#endif /* NEW_CLEANUP */

/*-------------------------------------------------------------------------*/
void
cleanup_object (object_t * obj)

/* Cleanup object <ob>, but don't force the mapping compaction.
 *
 * If the object is on the swap, it will be swapped in for the time
 * of the function.
 *
 * The function checks all variables of this object for references
 * to destructed objects and removes them. Also, untabled strings
 * are made tabled. The time for the next cleanup is set to 
 * a time in the interval [0.9*time_to_cleanup .. 1.1 * time_to_cleanup]
 * from now (if time_to_cleanup is 0, DEFAULT_CLEANUP_TIME is assumed).
 *
 * This function is called by the backend.
 */

{
#ifndef NEW_CLEANUP
    return;
#else
    cleanup_t      * context = NULL;
#ifdef LOG_NEW_CLEANUP
    struct timeval   t_begin, t_end;
#endif /* LOG_NEW_CLEANUP */
    Bool             didSwap = MY_FALSE;
    unsigned long    numValues = 0;

#ifdef LOG_NEW_CLEANUP
    if (gettimeofday(&t_begin, NULL))
    {
        t_begin.tv_sec = t_begin.tv_usec = 0;
    }
#endif /* LOG_NEW_CLEANUP */

    context = cleanup_new(MY_FALSE);
    if (context != NULL)
    {
        didSwap = cleanup_single_object(obj, context);
        cleanup_compact_mappings(context);
        numValues = context->numValues;
        cleanup_free(context);
    }
    obj->time_cleanup = current_time + (9*time_to_data_cleanup)/10
                                     + random_number((2*time_to_data_cleanup)/10);

#ifdef LOG_NEW_CLEANUP
    if (t_begin.tv_sec == 0
     || gettimeofday(&t_end, NULL))
    {
        debug_message("%s Data-Clean: %6lu values: /%s %s\n"
                     , time_stamp(), numValues, get_txt(obj->name)
                     , didSwap ? "(swapped)" : "");
        printf("%s Data-Clean: %6lu values: /%s %s\n"
              , time_stamp(), numValues, get_txt(obj->name)
              , didSwap ? "(swapped)" : "");
    }
    else
    {
        t_end.tv_sec -= t_begin.tv_sec;
        t_end.tv_usec -= t_begin.tv_usec;
        if (t_end.tv_usec < 0)
        {
            t_end.tv_sec--;
            t_end.tv_usec += 1000000;
        }

        debug_message("%s Data-Clean: %3ld.%06ld s, %6lu values: /%s%s\n"
                     , time_stamp()
                     , (long)t_end.tv_sec, (long)t_end.tv_usec
                     , numValues
                     , get_txt(obj->name)
                     , didSwap ? " (swapped)" : ""
                     );
        printf("%s Data-Clean: %3ld.%06ld s, %6lu values: /%s%s\n"
              , time_stamp()
              , (long)t_end.tv_sec, (long)t_end.tv_usec
              , numValues
              , get_txt(obj->name)
              , didSwap ? " (swapped)" : ""
              );
    }
#endif /* LOG_NEW_CLEANUP */

#endif /* NEW_CLEANUP */
} /* cleanup_object() */

/*-------------------------------------------------------------------------*/
void
cleanup_driver_structures (void)

/* Cleanup the fixed driver structures if it is time.
 *
 * The time for the next cleanup is set to a time in the interval
 * [0.9*time_to_cleanup .. 1.1 * time_to_cleanup] from now (if time_to_cleanup
 * is 0, DEFAULT_CLEANUP_TIME is assumed).
 *
 * This function is called by the backend.
 */

{
#ifndef NEW_CLEANUP
    return;
#else
    cleanup_t      * context = NULL;
#ifdef LOG_NEW_CLEANUP
    struct timeval   t_begin, t_end;
#endif /* LOG_NEW_CLEANUP */
    unsigned long    numValues = 0;

static mp_int time_cleanup = 0;
    /* Time of the next regular cleanup. */

    /* Is it time for the cleanup yet? */
    if (time_cleanup != 0 && time_cleanup >= current_time)
        return;

    time_cleanup = current_time + (9*time_to_data_cleanup)/10
                                + random_number((2*time_to_data_cleanup)/10);

#ifdef LOG_NEW_CLEANUP
    if (gettimeofday(&t_begin, NULL))
    {
        t_begin.tv_sec = t_begin.tv_usec = 0;
    }
#endif /* LOG_NEW_CLEANUP */

    context = cleanup_new(MY_FALSE);
    if (context != NULL)
    {
        cleanup_structures(context);
        cleanup_compact_mappings(context);
        numValues = context->numValues;
        cleanup_free(context);
    }

#ifdef LOG_NEW_CLEANUP
    if (t_begin.tv_sec == 0
     || gettimeofday(&t_end, NULL))
    {
        debug_message("%s Data-Clean: %6lu values: Fixed structures\n"
                     , time_stamp(), numValues);
        printf("%s Data-Clean: %6lu values: Fixed structures\n"
              , time_stamp(), numValues);
    }
    else
    {
        t_end.tv_sec -= t_begin.tv_sec;
        t_end.tv_usec -= t_begin.tv_usec;
        if (t_end.tv_usec < 0)
        {
            t_end.tv_sec--;
            t_end.tv_usec += 1000000;
        }

        debug_message("%s Data-Clean: %3ld.%06ld s, %6lu values: Fixed structures\n"
                     , time_stamp()
                     , (long)t_end.tv_sec, (long)t_end.tv_usec
                     , numValues
                     );
        printf("%s Data-Clean: %3ld.%06ld s, %6lu values: Fixed structures\n"
              , time_stamp()
              , (long)t_end.tv_sec, (long)t_end.tv_usec
              , numValues
              );
    }
#endif /* LOG_NEW_CLEANUP */

#endif /* NEW_CLEANUP */
} /* cleanup_driver_structures() */

/*-------------------------------------------------------------------------*/
void
cleanup_all_objects (void)

/* Cleanup all objects in the game, and force the mapping compaction.
 * This function is called by the garbage-collector right at the start,
 * and also by the backend.
 */

{
#ifndef NEW_CLEANUP
    return;
#else
    cleanup_t      * context = NULL;
#ifdef LOG_NEW_CLEANUP_ALL
    struct timeval   t_begin, t_end;
#endif /* LOG_NEW_CLEANUP_ALL */
    long             numObjects = 0;
    unsigned long    numValues = 0;

#ifdef LOG_NEW_CLEANUP_ALL
    if (gettimeofday(&t_begin, NULL))
    {
        t_begin.tv_sec = t_begin.tv_usec = 0;
    }
    debug_message("%s Data-Clean: All Objects\n"
                 , time_stamp()
                 );
    printf("%s Data-Clean: All Objects\n"
          , time_stamp()
          );
#endif /* LOG_NEW_CLEANUP_ALL */

    context = cleanup_new(MY_TRUE);
    if (context != NULL)
    {
        object_t   * ob;
        for (ob = obj_list; ob; ob = ob->next_all)
        {
            /* If the object is swapped for the cleanup, throw away
             * the pointertable afterwards as the memory locations
             * are no longer unique.
             */
            if ( cleanup_single_object(ob, context)
             && !cleanup_reset(context))
            {
                cleanup_free(context);
                return;
            }
            numObjects++;
        }
        cleanup_structures(context);
        cleanup_compact_mappings(context);
        numValues = context->numValues;
        cleanup_free(context);
    }

#ifdef LOG_NEW_CLEANUP_ALL
    if (t_begin.tv_sec == 0
     || gettimeofday(&t_end, NULL))
    {
        debug_message("%s Data-Cleaned %ld objects: %lu values.\n", time_stamp(), numObjects, numValues);
        printf("%s Data-Cleaned %ld objects: %lu values.\n", time_stamp(), numObjects, numValues);
    }
    else
    {
        t_end.tv_sec -= t_begin.tv_sec;
        t_end.tv_usec -= t_begin.tv_usec;
        if (t_end.tv_usec < 0)
        {
            t_end.tv_sec--;
            t_end.tv_usec += 1000000;
        }

        debug_message("%s Data-Cleaned %ld objects in %ld.%06ld s, %6lu values.\n"
                     , time_stamp(), numObjects
                     , (long)t_end.tv_sec, (long)t_end.tv_usec
                     , numValues
                     );
        printf("%s Data-Cleaned %ld objects in %ld.%06ld s, %6lu values.\n"
              , time_stamp(), numObjects
              , (long)t_end.tv_sec, (long)t_end.tv_usec
              , numValues
              );
    }
#endif /* LOG_NEW_CLEANUP_ALL */

#endif /* NEW_CLEANUP */
} /* cleanup_all_objects() */

/*=========================================================================*/

/*            The real collector - only if the allocator allows it.
 */

#if defined(GC_SUPPORT)

#if defined(CHECK_OBJECT_GC_REF) && defined(DUMP_GC_REFS)
#  error Must define either CHECK_OBJECT_GC_REF or DUMP_GC_REFS.
#  undef DUMP_GC_REFS
#endif

#define CLEAR_REF(p) x_clear_ref(p)
  /* Clear the memory block marker for <p>
   */

#ifdef CHECK_OBJECT_GC_REF
unsigned long gc_mark_ref(void * p, const char * file, int line)
{
    if (is_object_allocation(p))
    {
        dprintf3(gout, "DEBUG: Object %x referenced as something else from %s:%d\n"
               , (p_int)p, (p_int)file, (p_int)line);
    }
    if (is_program_allocation(p))
    {
        dprintf3(gout, "DEBUG: Program %x referenced as something else from %s:%d\n"
               , (p_int)p, (p_int)file, (p_int)line);
    }
    return x_mark_ref(p);
}

#define MARK_REF(p) gc_mark_ref(p, __FILE__, __LINE__)
#define MARK_PLAIN_REF(p) x_mark_ref(p)

#else
#define MARK_REF(p) x_mark_ref(p)
  /* Set the memory block marker for <p>
   */
#define MARK_PLAIN_REF(p) MARK_REF(p)
#endif

#define TEST_REF(p) x_test_ref(p)
  /* Check the memory block marker for <p>, return TRUE if _not_ set.
   */

#define CHECK_REF(p) ( TEST_REF(p) && ( MARK_REF(p),MY_TRUE ) )
  /* Check the memory block marker for <p> and set it if necessary.
   * Return TRUE if the marker was not set, FALSE else.
   */

#define MSTRING_REFS(str) ((str)->info.ref)
  /* Return the refcount of mstring <str>
   */

/* Forward declarations */

static void clear_map_ref_filter (svalue_t *, svalue_t *, void *);
static void clear_ref_in_closure (lambda_t *l, ph_int type);
static void gc_count_ref_in_closure (svalue_t *csvp);
static void gc_MARK_MSTRING_REF (string_t * str);

#define count_ref_in_closure(p) \
  GC_REF_DUMP(svalue_t*, p, "Count ref in closure", gc_count_ref_in_closure)

#define MARK_MSTRING_REF(str) \
  GC_REF_DUMP(string_t*, str, "Mark string", gc_MARK_MSTRING_REF)

/*-------------------------------------------------------------------------*/

#if defined(MALLOC_TRACE)

#define WRITES(d, s) writes(d, s)

/*-------------------------------------------------------------------------*/
static INLINE void
write_malloc_trace (void * p)

/* Dump the allocation information for <p>, if any.
 */

{
    WRITES(gout, ((char **)(p))[-2]);
    WRITES(gout, " ");
    writed(gout, (int)((p_uint *)(p))[-1]);
    WRITES(gout, "\n");
} /* write_malloc_trace() */

#else

#define write_malloc_trace(p)
#define WRITES(d, s)

#endif /* MALLOC_TRACE */

/*-------------------------------------------------------------------------*/
void
clear_memory_reference (void *p)

/* Clear the memory block marker for block <p>.
 */

{
    CLEAR_REF(p);
} /* clear_memory_reference() */

/*-------------------------------------------------------------------------*/
Bool
test_memory_reference (void *p)

/* Test if the memory block <p> is marked as referenced.
 * Return TRUE if it is NOT referenced, and FALSE it it is.
 */

{
    return TEST_REF(p);
} /* test_memory_reference() */

/*-------------------------------------------------------------------------*/
static INLINE void
gc_note_ref (void *p
#ifdef CHECK_OBJECT_GC_REF
            , const char * file, int line
#endif
            )

/* Note the reference to memory block <p>.
 *
 * It is no use to write a diagnostic on the second or higher reference
 * to the memory block, as this can happen when an object is swapped in,
 * marked, swapped out, and the next swapped-in object reuses the memory block
 * released from the one before.
 */

{
    if (TEST_REF(p))
    {
#ifdef CHECK_OBJECT_GC_REF
        gc_mark_ref(p, file, line);
#else
        MARK_REF(p);
#endif
        return;
    }
} /* gc_note_ref() */

#ifdef CHECK_OBJECT_GC_REF
void gc_note_malloced_block_ref (void *p, const char * file, int line) { gc_note_ref(p, file, line); }
#define note_ref(p) gc_note_ref(p, __FILE__, __LINE__)
#define passed_note_ref(p) gc_note_ref(p, file, line)
#else
void gc_note_malloced_block_ref (void *p) { gc_note_ref(p); }

#define note_ref(p) GC_REF_DUMP(void*, p, "Note ref", gc_note_ref)
#define passed_note_ref(p) note_ref(p)
#endif

/*-------------------------------------------------------------------------*/
void
clear_string_ref (string_t *p)

/* Clear the references in string <p>
 */

{
    p->info.ref = 0;
} /* clear_string_ref() */

/*-------------------------------------------------------------------------*/
void
clear_program_ref (program_t *p, Bool clear_ref)

/* Clear the refcounts of all inherited programs and other associated
 * data of of <p> .
 * If <clear_ref> is TRUE, the refcount of <p> itself is cleared, too.
 */

{
    int i;

    if (clear_ref)
    {
        p->ref = 0;
    }

    if (p->name)
        clear_string_ref(p->name);

    /* Variables */
    for (i = p->num_variables; --i >= 0;)
    {
        clear_fulltype_ref(&p->variables[i].type);
    }

    /* Non-inherited functions */

    for (i = p->num_functions; --i >= 0; )
    {
        if ( !(p->functions[i] & NAME_INHERITED) )
        {
            vartype_t vt;

            memcpy(
              &vt,
              FUNCTION_TYPEP(p->program + (p->functions[i] & FUNSTART_MASK)),
              sizeof vt
            );
            clear_vartype_ref(&vt);
        }
    }

#ifdef USE_STRUCTS
    /* struct definitions */
    for (i = 0; i <p->num_structs; i++)
    {
        clear_struct_type_ref(p->struct_defs[i].type);
    }
#endif /* USE_STRUCTS */

    for (i = 0; i < p->num_inherited; i++)
    {
        /* Inherited programs are never swapped. Only programs with blueprints
         * are swapped, and a blueprint and one inheritance makes two refs.
         */
        program_t *p2;

        p2 = p->inherit[i].prog;
        if (p2->ref)
        {
            clear_program_ref(p2, MY_TRUE);
        }
    }
} /* clear_program_ref() */

/*-------------------------------------------------------------------------*/
void
clear_object_ref (object_t *p)

/* If <p> is a destructed object, its refcounts are cleared.
 * If <p> is a live object, its refcounts are assumed to be cleared
 * by the GC main method.
 */

{
    if ((p->flags & O_DESTRUCTED) && p->ref)
    {
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
        p->extra_ref = p->ref;
#endif
        p->ref = 0;
        clear_string_ref(p->name);
        if (p->prog->blueprint
         && (p->prog->blueprint->flags & O_DESTRUCTED)
         && p->prog->blueprint->ref
           )
        {
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
            p->prog->blueprint->extra_ref = p->prog->blueprint->ref;
#endif
            p->prog->blueprint->ref = 0;
        }
        clear_program_ref(p->prog, MY_TRUE);
    }
} /* clear_object_ref() */

/*-------------------------------------------------------------------------*/
void
gc_mark_program_ref (program_t *p)

/* Set the marker of program <p> and of all data referenced by <p>.
 */

{
#ifdef CHECK_OBJECT_GC_REF
    if (TEST_REF(p) && ( MARK_PLAIN_REF(p),MY_TRUE ) )
#else
    if (CHECK_REF(p))  /* ...then mark referenced data */
#endif
    {
        int i;

        unsigned char *program = p->program;
        uint32 *functions = p->functions;
        string_t **strings;
        variable_t *variables;

        if (p->ref++)
        {
            dump_malloc_trace(1, p);
            fatal("First reference to program %p '%s', but ref count %ld != 0\n"
                 , p, p->name ? get_txt(p->name) : "<null>", (long)p->ref - 1
                 );
        }

        MARK_MSTRING_REF(p->name);

        /* Mark the blueprint object, if any */
        if (p->blueprint)
        {
            if (p->blueprint->flags & O_DESTRUCTED)
            {
                reference_destructed_object(p->blueprint);
                p->blueprint = NULL;
                remove_prog_swap(p, MY_TRUE);
            }
            else
            {
                p->blueprint->ref++;
                /* No note_ref() necessary: the blueprint is in
                 * the global object list
                 */
            }
        }

        if (p->line_numbers)
            note_ref(p->line_numbers);

        /* Non-inherited functions */

        for (i = p->num_functions; --i >= 0; )
        {
            if ( !(functions[i] & NAME_INHERITED) )
            {
                string_t *name;
                vartype_t vt;

                memcpy(
                  &name,
                  FUNCTION_NAMEP(program + (functions[i] & FUNSTART_MASK)),
                  sizeof name
                );
                MARK_MSTRING_REF(name);

                memcpy(
                  &vt,
                  FUNCTION_TYPEP(program + (functions[i] & FUNSTART_MASK)),
                  sizeof vt
                );
                count_vartype_ref(&vt);
            }
        }

        /* String literals */

        strings = p->strings;
        for (i = p->num_strings; --i >= 0; )
        {
            string_t *str = *strings++;
            MARK_MSTRING_REF(str);
        }

        /* Variable names */

        variables = p->variables;
        for (i = p->num_variables; --i >= 0; variables++)
        {
            MARK_MSTRING_REF(variables->name);
            count_fulltype_ref(&variables->type);
        }

        /* Inherited programs */

        for (i=0; i< p->num_inherited; i++)
            mark_program_ref(p->inherit[i].prog);

#ifdef USE_STRUCTS
        /* struct definitions */
        for (i = 0; i < p->num_structs; i++)
        {
            count_struct_type_ref(p->struct_defs[i].type);
        }
#endif /* USE_STRUCTS */

        /* Included files */

        for (i=0; i< p->num_includes; i++)
        {
            string_t *str;
            str = p->includes[i].name; MARK_MSTRING_REF(str);
            str = p->includes[i].filename; MARK_MSTRING_REF(str);
        }
    }
    else
    {
        if (!p->ref++)
        {
            dump_malloc_trace(1, p);
            fatal("Program block %p '%s' referenced as something else\n"
                 , p, p->name ? get_txt(p->name) : "<null>");
        }
    }
} /* gc_mark_program_ref() */

/*-------------------------------------------------------------------------*/
static void
mark_object_ref (object_t *ob)

/* Mark the object <ob> as referenced and increase its refcount.
 * This method should be called only for destructed objects and
 * from the GC main loop for the initial count of live objects.
 */

{
    MARK_PLAIN_REF(ob); ob->ref++;
    if (ob->prog) mark_program_ref(ob->prog);
    if (ob->name) MARK_MSTRING_REF(ob->name);
    if (ob->load_name) MARK_MSTRING_REF(ob->load_name);
} /* mark_object_ref() */

/*-------------------------------------------------------------------------*/
void
gc_reference_destructed_object (object_t *ob)

/* Note the reference to a destructed object <ob>. The referee has to
 * replace its reference by a svalue.number 0 since all these objects
 * will be freed later.
 */

{
    if (TEST_REF(ob))
    {
        if (ob->ref)
        {
            dump_malloc_trace(1, ob);
            fatal("First reference to destructed object %p '%s', "
                  "but ref count %ld != 0\n"
                 , ob, ob->name ? get_txt(ob->name) : "<null>", (long)ob->ref
                 );
        }

        /* Destructed objects are not swapped */
        ob->next_all = gc_obj_list_destructed;
        gc_obj_list_destructed = ob;
        mark_object_ref(ob);
    }
    else
    {
        if (!ob->ref)
        {
            write_malloc_trace(ob);
            dump_malloc_trace(1, ob);
            fatal("Destructed object %p '%s' referenced as something else\n"
                 , ob, ob->name ? get_txt(ob->name) : "<null>");
        }
    }
} /* gc_reference_destructed_object() */

/*-------------------------------------------------------------------------*/
static void
gc_MARK_MSTRING_REF (string_t * str)

/* Increment the refcount of mstring <str>. How it works:
 * If MSTRING_REFS() is 0, the refcount either overflowed or it is
 * the first visit to the block. If it's the first visit, CHECK_REF
 * will return TRUE, otherwise we have an overflow and the MSTRING_REFS--
 * will undo the ++ from earlier.
 */

{
    if (CHECK_REF(str))
    {
        /* First visit to this block */
        MSTRING_REFS(str)++;
    }
    else if (MSTRING_REFS(str))
    {
        /* Not the first visit, and refcounts didn't overrun either */
        MSTRING_REFS(str)++;
        if (!MSTRING_REFS(str))
        {
            /* Refcount overflow */
            dprintf2(gout, "DEBUG: mark string: %x '%s' refcount reaches max!\n"
                    , (p_int)str, (p_int)str->txt);
        }
    }
} /* gc_MARK_MSTRING_REF(str) */

/*-------------------------------------------------------------------------*/
void
gc_count_ref_from_string (string_t *p)

/* Count the reference to mstring <p>.
 */

{
   gc_MARK_MSTRING_REF(p);
} /* gc_count_ref_from_string() */

/*-------------------------------------------------------------------------*/
static void
clear_map_ref_filter (svalue_t *key, svalue_t *data, void *extra)

/* Auxiliary function to clear the refs in a mapping.
 * It is called with the <key> and <data> vector, the latter of
 * width (p_int)<extra>
 */
{
    clear_ref_in_vector(key, 1);
    clear_ref_in_vector(data, (size_t)extra);
} /* clear_map_ref_filter() */

/*-------------------------------------------------------------------------*/
void
clear_ref_in_vector (svalue_t *svp, size_t num)

/* Clear the refs of the <num> elements of vector <svp>.
 */

{
    svalue_t *p;

    if (!svp) /* e.g. when called for obj->variables */
        return;

    for (p = svp; p < svp+num; p++)
    {
        switch(p->type)
        {
        case T_OBJECT:
            /* this might be a destructed object, which has it's ref not
             * cleared by the obj_list because it is no longer a member
             * Alas, swapped objects must not have prog->ref cleared.
             */
            clear_object_ref(p->u.ob);
            continue;

        case T_STRING:
        case T_SYMBOL:
            clear_string_ref(p->u.str);
            break;

        case T_POINTER:
        case T_QUOTED_ARRAY:
            if (!p->u.vec->ref)
                continue;
            p->u.vec->ref = 0;
            clear_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            continue;

#ifdef USE_STRUCTS
        case T_STRUCT:
            clear_struct_ref(p->u.strct);
            continue;
#endif /* USE_STRUCTS */

        case T_MAPPING:
            if (p->u.map->ref)
            {
                mapping_t *m;
                p_int num_values;

#ifdef DEBUG
                /* The initial cleanup phase should take care of compacting
                 * all dirty mappings, however just in case one slips
                 * through...
                 */
                if (p->u.map->hash != NULL)
                    dprintf1(gcollect_outfd
                            , "Mapping %x still has a hash part.\n"
                            , (p_int)p->u.map);
#endif
                m = p->u.map;
                m->ref = 0;
                num_values = m->num_values;
                walk_mapping(m, clear_map_ref_filter, (char *)num_values );
            }
            continue;

        case T_CLOSURE:
            if (CLOSURE_MALLOCED(p->x.closure_type))
            {
                lambda_t *l;

                l = p->u.lambda;
                if (l->ref)
                {
                    l->ref = 0;
                    clear_ref_in_closure(l, p->x.closure_type);
                }
            }
            else
                clear_object_ref(p->u.ob);
            continue;
        }
    }
} /* clear_ref_in_vector() */

/*-------------------------------------------------------------------------*/
void
gc_count_ref_in_vector (svalue_t *svp, size_t num
#ifdef CHECK_OBJECT_GC_REF
            , const char * file, int line
#endif
                       )

/* Count the references the <num> elements of vector <p>.
 */

{
    svalue_t *p;

    if (!svp) /* e.g. when called for obj->variables */
        return;

    for (p = svp; p < svp+num; p++) {
        switch(p->type)
        {
        case T_OBJECT:
          {
            object_t *ob;

            ob = p->u.ob;
            if (ob->flags & O_DESTRUCTED)
            {
                put_number(p, 0);
                reference_destructed_object(ob);
            }
            else
            {
                ob->ref++;
            }
            continue;
          }

        case T_POINTER:
        case T_QUOTED_ARRAY:
            /* Don't use CHECK_REF on the null vector */
            if (p->u.vec != &null_vector && CHECK_REF(p->u.vec))
            {
                count_array_size(p->u.vec);
#ifdef CHECK_OBJECT_GC_REF
                gc_count_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec), file, line);
#else
                count_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
#endif
            }
            p->u.vec->ref++;
            continue;

#ifdef USE_STRUCTS
        case T_STRUCT:
            count_struct_ref(p->u.strct);
            continue;
#endif /* USE_STRUCTS */

        case T_MAPPING:
            if (CHECK_REF(p->u.map))
            {
                mapping_t *m;

                m = p->u.map;
                count_ref_in_mapping(m);
                count_mapping_size(m);
            }
            p->u.map->ref++;
            continue;

        case T_STRING:
            MARK_MSTRING_REF(p->u.str);
            continue;

        case T_CLOSURE:
            if (CLOSURE_MALLOCED(p->x.closure_type))
            {
                if (p->u.lambda->ref++ <= 0)
                {
                    count_ref_in_closure(p);
                }
            }
            else
            {
                object_t *ob;

                ob = p->u.ob;
                if (ob->flags & O_DESTRUCTED)
                {
                    put_number(p, 0);
                    reference_destructed_object(ob);
                }
                else
                {
                    ob->ref++;
                }
            }
            continue;

        case T_SYMBOL:
            MARK_MSTRING_REF(p->u.str);
            continue;
        }
    } /* for */
} /* gc_count_ref_in_vector() */

/*-------------------------------------------------------------------------*/
static void
mark_unreferenced_string (string_t *string)

/* If the shared string <string> stored in the memory block <start> is
 * not referenced, it is deallocated.
 */

{
    if (TEST_REF(string))
    {
        dprintf2(gout,
"tabled string %x '%s' was left unreferenced, freeing now.\n",
          (p_int) string, (p_int)string->txt
        );

        MARK_REF(string);
        MSTRING_REFS(string) = 0;
    }
} /* mark_unreferenced_string() */

/*-------------------------------------------------------------------------*/
static void
clear_action_ref (action_t *p)

/* Clear the refs of all sentences in list <p>.
 */

{
    do
    {
        clear_ref_in_callback(&(p->cb));
    } while ( NULL != (p = (action_t *)p->sent.next) );
}

/*-------------------------------------------------------------------------*/
static void
gc_note_action_ref (action_t *p)

/* Mark the strings of function and verb of all sentences in list <p>.
 */

{
    do {
        count_ref_in_callback(&(p->cb));
        if (p->verb)
            MARK_MSTRING_REF(p->verb);
        note_ref(p);
    } while ( NULL != (p = (action_t *)p->sent.next) );
}

#define note_action_ref(p) \
    GC_REF_DUMP(action_t*, p, "Note action ref", gc_note_action_ref)

/*-------------------------------------------------------------------------*/
static void
gc_count_ref_in_closure (svalue_t *csvp)

/* Count the reference to closure <csvp> and all referenced data.
 * Closures using a destructed object are stored in the stale_ lists
 * for later removal (and .ref is set to -1).
 */

{
    lambda_t *l = csvp->u.lambda;
    ph_int type = csvp->x.closure_type;

    if (!l->ref)
    {
        /* This closure was bound to a destructed object, and has been
         * encountered before.
         */
        l->ref--; /* Undo ref increment that was done by the caller */
        if (type == CLOSURE_BOUND_LAMBDA)
        {
            csvp->x.closure_type = CLOSURE_UNBOUND_LAMBDA;
            (csvp->u.lambda = l->function.lambda)->ref++;
        }
        else
        {
            put_number(csvp, 0);
        }
        return;
    }

    /* If the closure is bound, make sure that the object it is
     * bound to really exists.
     */

    if (type != CLOSURE_UNBOUND_LAMBDA)
    {
        object_t *ob;

        ob = l->ob;
        if (ob->flags & O_DESTRUCTED
         || (   type == CLOSURE_LFUN
             && l->function.lfun.ob->flags & O_DESTRUCTED) )
        {
            l->ref = -1;
            if (type == CLOSURE_LAMBDA)
            {
                l->ob = (object_t *)stale_lambda_closures;
                stale_lambda_closures = l;
            }
            else
            {
                l->ob = (object_t *)stale_misc_closures;
                stale_misc_closures = l;
                if (type == CLOSURE_LFUN)
                {
                    if (l->function.lfun.ob->flags & O_DESTRUCTED)
                    {
                        reference_destructed_object(l->function.lfun.ob);
                    }
                }
            }

            if (ob->flags & O_DESTRUCTED)
            {
                reference_destructed_object(ob);
            }

            if (type == CLOSURE_BOUND_LAMBDA)
            {
                csvp->x.closure_type = CLOSURE_UNBOUND_LAMBDA;
                csvp->u.lambda = l->function.lambda;
            }
            else
            {
                put_number(csvp, 0);
            }
        }
        else
        {
             /* Object exists: count reference */

            ob->ref++;
            if (type == CLOSURE_LFUN)
            {
                l->function.lfun.ob->ref++;
                if(l->function.lfun.inhProg)
                    mark_program_ref(l->function.lfun.inhProg);
            }
        }
    }

    /* Count the references in the code of the closure */

    if (l->prog_ob)
    {
        if (l->prog_ob->flags & O_DESTRUCTED)
        {
            reference_destructed_object(l->prog_ob);
            l->prog_ob = NULL;
            l->prog_pc = 0;
        }
        else
        {
             /* Object exists: count reference */
            l->prog_ob->ref++;
        }
    }

    if (CLOSURE_HAS_CODE(type))
    {
        mp_int num_values;
        svalue_t *svp;

        svp = (svalue_t *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0x100].u.number;
        svp -= num_values;
        note_ref(svp);
        count_ref_in_vector(svp, (size_t)num_values);
    }
    else
    {
        note_ref(l);
        if (type == CLOSURE_BOUND_LAMBDA)
        {
            lambda_t *l2 = l->function.lambda;

            if (!l2->ref++) {
                svalue_t sv;

                sv.type = T_CLOSURE;
                sv.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
                sv.u.lambda = l2;
                count_ref_in_closure(&sv);
            }
        }
#ifdef USE_NEW_INLINES
        if (type == CLOSURE_LFUN && l->function.lfun.context_size != 0)
        {
            unsigned short size = l->function.lfun.context_size;
            l->function.lfun.context_size = 0; /* Prevent recursion */
            count_ref_in_vector(l->context, size);
            l->function.lfun.context_size = size;
        }
#endif /* USE_NEW_INLINES */
    }
} /* count_ref_in_closure() */

/*-------------------------------------------------------------------------*/
static void
clear_ref_in_closure (lambda_t *l, ph_int type)

/* Clear the references in closure <l> which is of type <type>.
 */

{
    if (l->prog_ob)
        clear_object_ref(l->prog_ob);

    if (CLOSURE_HAS_CODE(type))
    {
        mp_int num_values;
        svalue_t *svp;

        svp = (svalue_t *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0x100].u.number;
        svp -= num_values;
        clear_ref_in_vector(svp, (size_t)num_values);
    }
    else if (type == CLOSURE_BOUND_LAMBDA)
    {
        lambda_t *l2 = l->function.lambda;

        if (l2->ref) {
            l2->ref = 0;
            clear_ref_in_closure(l2, CLOSURE_UNBOUND_LAMBDA);
        }
    }

    if (type != CLOSURE_UNBOUND_LAMBDA)
        clear_object_ref(l->ob);

    if (type == CLOSURE_LFUN)
    {
        clear_object_ref(l->function.lfun.ob);
        if (l->function.lfun.inhProg)
            clear_program_ref(l->function.lfun.inhProg, MY_TRUE);
    }
    
#ifdef USE_NEW_INLINES
    if (type == CLOSURE_LFUN && l->function.lfun.context_size != 0)
    {
        unsigned short size = l->function.lfun.context_size;
        l->function.lfun.context_size = 0; /* Prevent recursion */
        clear_ref_in_vector(l->context, size);
        l->function.lfun.context_size = size;
    }
#endif /* USE_NEW_INLINES */
} /* clear_ref_in_closure() */

/*-------------------------------------------------------------------------*/
void
restore_default_gc_log (void)

/* If gcollect_outfd was redirected to some other file, that file is
 * closed and the default log file is restored.
 */

{
    if (gcollect_outfd != default_gcollect_outfd)
    {
        if (gcollect_outfd != 1 && gcollect_outfd != 2)
            close(gcollect_outfd);
        gcollect_outfd = default_gcollect_outfd;
    }
} /* restore_default_gc_log() */

/*-------------------------------------------------------------------------*/
void
new_default_gc_log (int fd)

/* Redirect the default and the current log file to file <fd>. If the
 * current log file is identical to the default log file, it is
 * redirected, too.
 */

{
    if (default_gcollect_outfd != fd)
    {
        restore_default_gc_log();

        if (default_gcollect_outfd != 1 && default_gcollect_outfd != 2)
            close(default_gcollect_outfd);
        default_gcollect_outfd = gcollect_outfd = fd;
    }
} /* new_default_gc_log() */

/*-------------------------------------------------------------------------*/
void
garbage_collection(void)

/* The Mark-Sweep garbage collector.
 *
 * Free all possible memory, then loop through every object and variable
 * in the game, check the reference counts and deallocate unused memory.
 * This takes time and should not be used lightheartedly.
 *
 * The function must be called outside of LPC evaluations.
 */

{
    object_t *ob, *next_ob;
    lambda_t *l, *next_l;
    int i;
    long dobj_count;

    if (gcollect_outfd != 1 && gcollect_outfd != 2)
    {
        dprintf1(gcollect_outfd, "\n%s --- Garbage Collection ---\n"
                               , (long)time_stamp());
    }

    /* --- Pass 0: dispose of some unnecessary stuff ---
     */

    dobj_count = tot_alloc_object;

    malloc_privilege = MALLOC_MASTER;
    RESET_LIMITS;
    CLEAR_EVAL_COST;
    out_of_memory = MY_FALSE;
    assert_master_ob_loaded();
    malloc_privilege = MALLOC_SYSTEM;

    /* Recover as much memory from temporaries as possible.
     * However, don't call mb_release() yet as the swap buffer
     * is still needed.
     */
    if (obj_list_replace)
        replace_programs();
    handle_newly_destructed_objects();
    free_save_object_buffers();
    free_interpreter_temporaries();
    free_action_temporaries();
#ifdef USE_PGSQL
    pg_purge_connections();
#endif /* USE_PGSQL */
    remove_stale_player_data();
    remove_stale_call_outs();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
    check_wizlist_for_destr();
    cleanup_all_objects();
    if (current_error_trace)
    {
        free_array(current_error_trace);
        current_error_trace = NULL;
    }
    if (uncaught_error_trace)
    {
        free_array(uncaught_error_trace);
        uncaught_error_trace = NULL;
    }

    remove_destructed_objects(MY_TRUE); /* After reducing all object references! */

    if (dobj_count != tot_alloc_object)
    {
        dprintf2(gcollect_outfd, "%s GC pass 1: Freed %d objects.\n"
                , (long)time_stamp(), dobj_count - tot_alloc_object);
    }

#ifdef CHECK_OBJECT_REF
    while (newly_destructed_obj_shadows != NULL)
    {
        object_shadow_t *sh = newly_destructed_obj_shadows;
        newly_destructed_obj_shadows = sh->next;
        xfree(sh);
    }
    while (destructed_obj_shadows != NULL)
    {
        object_shadow_t *sh = destructed_obj_shadows;
        destructed_obj_shadows = sh->next;
        xfree(sh);
    }
#endif /* CHECK_OBJECT_REF */


    /* --- Pass 1: clear the 'referenced' flag in all malloced blocks ---
     */
    mem_clear_ref_flags();

    /* --- Pass 2: clear the ref counts ---
     */

    gc_status = gcClearRefs;
    if (d_flag > 3)
    {
        debug_message("%s start of garbage_collection\n", time_stamp());
    }

    clear_array_size();
    clear_mapping_size();

    /* Process the list of all objects */

    for (ob = obj_list; ob; ob = ob->next_all) {
        int was_swapped;
        Bool clear_prog_ref;

        if (d_flag > 4)
        {
            debug_message("%s clearing refs for object '%s'\n"
                         , time_stamp(), get_txt(ob->name));
        }
        was_swapped = 0;
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
        ob->extra_ref = ob->ref;
#endif
        if (ob->flags & O_SWAPPED
         && (was_swapped = load_ob_from_swap(ob)) & 1)
        {
            /* don't clear the program ref count. It is 1 */
            clear_prog_ref = MY_FALSE;
        }
        else
        {
            /* Take special care of inherited programs, the associated
             * objects might be destructed.
             */
            clear_prog_ref = MY_TRUE;
        }
        if (was_swapped < 0)
            fatal("Totally out of MEMORY in GC: (swapping in '%s')\n"
                 , get_txt(ob->name));

        clear_program_ref(ob->prog, clear_prog_ref);

        ob->ref = 0;
        clear_string_ref(ob->name);
        clear_ref_in_vector(ob->variables, ob->prog->num_variables);

        if (ob->sent)
        {
            sentence_t *sent;

            sent = ob->sent;
            if (ob->flags & O_SHADOW)
                sent = sent->next;
            if (sent)
                clear_action_ref((action_t *)sent);
        }


        if (was_swapped)
        {
            swap(ob, was_swapped);
        }
    }
    if (d_flag > 3)
    {
        debug_message("%s ref counts referenced by obj_list cleared\n"
                     , time_stamp());
    }

    /* Process the interactives */

    for(i = 0 ; i < MAX_PLAYERS; i++)
    {
        input_t * it;

        if (all_players[i] == NULL)
            continue;

        for ( it = all_players[i]->input_handler; it != NULL; it = it->next)
        {
            clear_memory_reference(it);
            clear_input_refs(it);
        }

#ifdef USE_TLS
        if (all_players[i]->tls_cb != NULL)
        {
            clear_memory_reference(all_players[i]->tls_cb);
            clear_ref_in_callback(all_players[i]->tls_cb);
        }
#endif
        clear_ref_in_vector(&all_players[i]->prompt, 1);

        /* snoop_by and modify_command are known to be NULL or non-destructed
         * objects.
         */
    }

    /* Process the driver hooks */

    clear_ref_in_vector(driver_hook, NUM_DRIVER_HOOKS);

    /* Let the modules process their data */

    mstring_clear_refs();
    clear_ref_from_wiz_list();
    clear_ref_from_call_outs();
    clear_ref_from_efuns();
#if defined(USE_PARSE_COMMAND)
    clear_parse_refs();
#endif
    clear_simul_efun_refs();
    clear_interpreter_refs();
    clear_comm_refs();
    clear_rxcache_refs();
#ifdef USE_STRUCTS
    clear_tabled_struct_refs();
#endif
#ifdef USE_PGSQL
    pg_clear_refs();
#endif /* USE_PGSQL */

    mb_clear_refs();
      /* As this call also covers the swap buffer, it MUST come after
       * processing (and potentially swapping) the objects.
       */

    null_vector.ref = 0;

    /* Finally, walk the list of destructed objects and clear all references
     * in them.
     */
    for (ob = destructed_objs; ob;  ob = ob->next_all)
    {
        if (d_flag > 4)
        {
            debug_message("%s clearing refs for destructed object '%s'\n"
                         , time_stamp(), get_txt(ob->name));
        }

        if (ob->name)
            clear_string_ref(ob->name);
        if (ob->load_name)
            clear_string_ref(ob->load_name);
        ob->prog->ref = 0;
        clear_program_ref(ob->prog, MY_TRUE);
        ob->ref = 0;
    }


    /* --- Pass 3: Compute the ref counts, and set the 'referenced' flag where
     *             appropriate ---
     */

    gc_status = gcCountRefs;

    gc_obj_list_destructed = NULL;
    stale_lambda_closures = NULL;
    stale_misc_closures = NULL;
    stale_mappings = NULL;


    /* Handle the known destructed objects first, as later calls to
     * reference_destructed_object() will clobber the list.
     */
    for (ob = destructed_objs; ob; )
    {
        object_t *next = ob->next_all;

        dprintf1(gcollect_outfd
                , "Freeing destructed object '%s'\n"
                , (p_int)get_txt(ob->name)
                );
        reference_destructed_object(ob); /* Clobbers .next_all */

        ob = next;
    }

    num_destructed = 0;
    destructed_objs = NULL;


    /* Process the list of all objects.
     */
    for (ob = obj_list; ob; ob = ob->next_all)
    {
        int was_swapped;

        was_swapped = 0;
        if (ob->flags & O_SWAPPED)
        {
            was_swapped = load_ob_from_swap(ob);
            if (was_swapped & 1)
            {
#ifdef DUMP_GC_REFS
                dprintf1(gcollect_outfd, "Clear ref of swapped-in program %x\n", (long)ob->prog);
#endif
                CLEAR_REF(ob->prog);
                ob->prog->ref = 0;
            }
        }

        mark_object_ref(ob);

        if (ob->prog->num_variables)
        {
            note_ref(ob->variables);
        }

        count_ref_in_vector(ob->variables, ob->prog->num_variables);

        if (ob->sent)
        {
            sentence_t *sent;

            sent = ob->sent;
            if (ob->flags & O_SHADOW)
            {
                note_ref(sent);

                /* If there is a ->ip, it will be processed as
                 * part of the player object handling below.
                 */
                sent = sent->next;
            }
            if (sent)
                note_action_ref((action_t *)sent);
        }

        if (was_swapped)
        {
            swap(ob, was_swapped);
        }
    }

    if (d_flag > 3)
    {
        debug_message("%s obj_list evaluated\n", time_stamp());
    }

    /* Process the interactives. */

    for(i = 0 ; i < MAX_PLAYERS; i++)
    {
        input_t * it;

        if (all_players[i] == NULL)
            continue;

        note_ref(all_players[i]);

        if (all_players[i]->write_first)
        {
            struct write_buffer_s *tmp = all_players[i]->write_first;
            
            do
            {
                note_ref(tmp);
                tmp = tmp->next;
            } while (tmp != NULL);
        }
#ifdef USE_MCCP
        if (all_players[i]->out_compress != NULL)
            note_ref(all_players[i]->out_compress);
        if (all_players[i]->out_compress_buf != NULL)
            note_ref(all_players[i]->out_compress_buf);
#endif /* USE_MCCP */

        /* There are no destructed interactives, or interactives
         * referencing destructed objects.
         */

        all_players[i]->ob->ref++;
        if ( NULL != (ob = all_players[i]->snoop_by) )
        {
            if (!O_IS_INTERACTIVE(ob))
            {
                ob->ref++;
            }
        } /* end of snoop-processing */

        for ( it = all_players[i]->input_handler; it != NULL; it = it->next)
        {
            note_ref(it);
            count_input_refs(it);
        } /* end of input_to processing */

#ifdef USE_TLS
        if (all_players[i]->tls_cb != NULL)
        {
            note_ref(all_players[i]->tls_cb);
            count_ref_in_callback(all_players[i]->tls_cb);
        }
#endif

        if ( NULL != (ob = all_players[i]->modify_command) )
        {
            ob->ref++;
        }

        count_ref_in_vector(&all_players[i]->prompt, 1);

        if (all_players[i]->trace_prefix)
        {
            count_ref_from_string(all_players[i]->trace_prefix);
        }
    }

    /* Let the modules process their data */

    count_ref_from_wiz_list();
    count_ref_from_call_outs();
    count_ref_from_efuns();

    if (master_ob)
        master_ob->ref++;
    else
        fatal("No master object\n");

    MARK_MSTRING_REF(master_name_str);
    count_lex_refs();
    count_compiler_refs();
    count_simul_efun_refs();
#if defined(SUPPLY_PARSE_COMMAND)
    count_old_parse_refs();
#endif
    mstring_note_refs();
    note_otable_ref();
    count_comm_refs();
    count_interpreter_refs();
    count_heart_beat_refs();
    count_rxcache_refs();
#ifdef USE_PGSQL
    pg_count_refs();
#endif /* USE_PGSQL */

    mb_note_refs();

    if (reserved_user_area)
        note_ref(reserved_user_area);
    if (reserved_master_area)
        note_ref(reserved_master_area);
    if (reserved_system_area)
        note_ref(reserved_system_area);

    note_ref(mud_lib);
    null_vector.ref++;

    /* Process the driver hooks */

    count_ref_in_vector(driver_hook, NUM_DRIVER_HOOKS);

    gc_status = gcInactive;

    /* --- Pass 4: remove unreferenced strings and struct types ---
     */

#ifdef USE_STRUCTS
    remove_unreferenced_structs();
#endif
    mstring_walk_table(mark_unreferenced_string);
    mstring_gc_table();

    /* --- Pass 5: Release all destructed objects ---
     *
     * It is vital that all information freed here is already known
     * as referenced, so we won't free it a second time in pass 6.
     */

    dobj_count = 0;
    for (ob = gc_obj_list_destructed; ob; ob = next_ob)
    {
        next_ob = ob->next_all;
        free_object(ob, "garbage collection");
        dobj_count++;
    }

    for (l = stale_lambda_closures; l; )
    {
        svalue_t sv;

        next_l = (lambda_t *)l->ob;
        l->ref = 1;
        sv.type = T_CLOSURE;
        sv.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        sv.u.lambda = l;
        l = (lambda_t *)l->ob;
        free_closure(&sv);
    }

    for (l = stale_misc_closures; l; l = next_l)
    {
        next_l = (lambda_t *)l->ob;
        xfree((char *)l);
    }

    clean_stale_mappings();

    /* --- Pass 6: Release all unused memory ---
     */

    mem_free_unrefed_memory();
    reallocate_reserved_areas();
    if (!reserved_user_area)
    {
        svalue_t *res = NULL;
        if (reserved_system_area)
        {
            RESET_LIMITS;
            CLEAR_EVAL_COST;
            malloc_privilege = MALLOC_MASTER;
            res = callback_master(STR_QUOTA_DEMON, 0);
        }
        /* Once: remove_uids(res && (res->type != T_NUMBER || res->u.number) );
         * but that function was never implemented.
         */
    }

    /* Release the memory from the buffers. Eventually it will be
     * allocated again, but for now the point is to reduce the amount
     * of allocated memory.
     */
    mb_release();

    /* Allow the memory manager to do some consolidation */
    mem_consolidate(MY_TRUE);

    /* Finally, try to reclaim the reserved areas */

    reallocate_reserved_areas();

    time_last_gc = time(NULL);
    dprintf2(gcollect_outfd, "%s GC freed %d destructed objects.\n"
            , (long)time_stamp(), dobj_count);
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
    for (ob = obj_list; ob; ob = ob->next_all) {
        if (ob->extra_ref != ob->ref
         && strchr(get_txt(ob->name), '#') == NULL
           )
        {
            dprintf4(2, "DEBUG: GC object %x '%s': refs %d, extra_refs %d\n"
                      , (p_int)ob, (p_int)get_txt(ob->name), (p_int)ob->ref
                      , (p_int)ob->extra_ref);
        }
    }
#endif

    /* If the GC log was redirected, close that file and set the
     * logging back to the default file.
     */
    restore_default_gc_log();
} /* garbage_collection() */


#if defined(MALLOC_TRACE)

/* Some functions to print the tracing data from the memory blocks.
 * The show_ functions are called from xmalloc directly.
 */

#ifdef USE_STRUCTS
static void show_struct(int d, void *block, int depth);
#endif /* USE_STRUCTS */

/*-------------------------------------------------------------------------*/
static void
show_string (int d, char *block, int depth UNUSED)

/* Print the string from memory <block> on filedescriptor <d>.
 */

{
#ifdef __MWERKS__
#    pragma unused(depth)
#endif
    size_t len;

    if (block == NULL)
    {
        WRITES(d, "<null>");
    }
    else
    {
        WRITES(d, "\"");
        if ((len = strlen(block)) < 70)
        {
            write(d, block, len);
            WRITES(d, "\"");
        }
        else
        {
            write(d, block, 50);
            WRITES(d, "\" (truncated, length ");writed(d, len);WRITES(d, ")");
        }
    }
} /* show_string() */

/*-------------------------------------------------------------------------*/
static void
show_mstring_data (int d, void *block, int depth UNUSED)

/* Print the stringdata from memory <block> on filedescriptor <d>.
 */

{
#ifdef __MWERKS__
#    pragma unused(depth)
#endif
    string_t *str;

    str = (string_t *)block;
    WRITES(d, "(");
    writed(d, (p_uint)str->size);
    WRITES(d, ")\"");
    if (str->size < 50)
    {
        write(d, block, str->size);
        WRITES(d, "\"");
    }
    else
    {
        write(d, block, 50);
        WRITES(d, "\" (truncated)");
    }
} /* show_mstring_data() */

/*-------------------------------------------------------------------------*/
static void
show_mstring (int d, void *block, int depth)

/* Print the mstring from memory <block> on filedescriptor <d>.
 */

{
    if (block == NULL)
    {
        WRITES(d, "<null>");
    }
    else
    {
        string_t *str;

        str = (string_t *)block;
        if (str->info.tabled)
        {
            WRITES(d, "Tabled string: ");
            show_mstring_data(d, str, depth);
        }
        else
        {
            WRITES(d, "Untabled string: ");
            show_mstring_data(d, str, depth);
        }
        /* TODO: This is how it should be
         * TODO:: show_mstring_data(d, str->str, depth);
         * TODO:: alas it crashes the driver when destructed leaked objects
         * TODO:: are found 'cause their name is no langer value (though
         * TODO:: the reason for that is yet unknown). See 3.3.168 mails/bugs.
         */
    }
} /* show_mstring() */

/*-------------------------------------------------------------------------*/
static void
show_object (int d, void *block, int depth)

/* Print the data about object <block> on filedescriptor <d>.
 */

{
    object_t *ob;

    ob = (object_t *)block;
    if (depth) {
        object_t *o;

        for (o = obj_list; o && o != ob; o = o->next_all) NOOP;
        if (!o || o->flags & O_DESTRUCTED) {
            WRITES(d, "Destructed object in block 0x");
            write_x(d, (p_uint)((unsigned *)block - xalloc_overhead()));
            WRITES(d, "\n");
            return;
        }
    }
    WRITES(d, "Object: ");
    if (ob->flags & O_DESTRUCTED)
        WRITES(d, "(destructed) ");
    show_mstring(d, ob->name, 0);
    WRITES(d, " from ");
    show_mstring(d, ob->load_name, 0);
    WRITES(d, ", uid: ");
    show_string(d, ob->user->name ? get_txt(ob->user->name) : "0", 0);
    WRITES(d, "\n");
} /* show_object() */

/*-------------------------------------------------------------------------*/
static void
show_cl_literal (int d, void *block, int depth UNUSED)

/* Print the data about literal closure <block> on filedescriptor <d>.
 */

{
#ifdef __MWERKS__
#    pragma unused(depth)
#endif
    lambda_t *l;
    object_t *obj;

    l = (lambda_t *)block;

    WRITES(d, "Closure literal: Object ");

    obj = l->ob;
    if (obj)
    {
        if (obj->name)
            show_mstring(d, obj->name, 0);
        else
            WRITES(d, "(no name)");
        if (obj->flags & O_DESTRUCTED)
            WRITES(d, " (destructed)");
    }
    else
        WRITES(d, "<null>");

    WRITES(d, ", index ");
    writed(d, l->function.var_index);
    WRITES(d, ", ref ");
    writed(d, l->ref);
    WRITES(d, "\n");
} /* show_cl_literal() */

/*-------------------------------------------------------------------------*/
static void
show_array(int d, void *block, int depth)

/* Print the array at recursion <depth> from memory <block> on
 * filedescriptor <d>. Recursive printing stops at <depth> == 2.
 */

{
    vector_t *a;
    mp_int i, j;
    svalue_t *svp;
    wiz_list_t *user = NULL;
    mp_int a_size;

    a = (vector_t *)block;

    /* Can't use VEC_SIZE() here, as the memory block may have been
     * partly overwritten by the malloc pointers already.
     */
    a_size = (mp_int)(  xalloced_size(a)
                   - ( xalloc_overhead() +
                       ( sizeof(vector_t) - sizeof(svalue_t) ) 
                     )

                  ) / (sizeof(svalue_t));

    if (depth && a != &null_vector)
    {
        int freed;
        wiz_list_t *wl;

        wl = NULL;
        freed = is_freed(block, sizeof(vector_t) );
        if (!freed)
        {
            user = a->user;
            wl = all_wiz;
            if (user)
                for ( ; wl && wl != user; wl = wl->next) NOOP;
        }
        if (freed || !wl || a_size <= 0 || a_size > MAX_ARRAY_SIZE
         || xalloced_size((char *)a) - xalloc_overhead() !=
              sizeof(vector_t) + sizeof(svalue_t) * (a_size - 1) )
        {
            WRITES(d, "Array in freed block 0x");
            write_x(d, (p_uint)((unsigned *)block - xalloc_overhead()));
            WRITES(d, "\n");
            return;
        }
    }
    else
    {
        user = a->user;
    }

    WRITES(d, "Array ");
    write_x(d, (p_int)a);
    WRITES(d, " size ");
    writed(d, (p_uint)a_size);
    WRITES(d, ", uid: ");
    show_string(d, user ? (user->name ? get_txt(user->name) : "<null>")
                        :"0", 0);
    WRITES(d, "\n");
    if (depth > 2)
        return;

    i = 32 >> depth;
    if (i > a_size)
        i = a_size;

    for (svp = a->item; --i >= 0; svp++)
    {
        for (j = depth + 1; --j >= 0;) WRITES(d, " ");
        switch(svp->type)
        {
        case T_POINTER:
            show_array(d, (char *)svp->u.vec, depth+1);
            break;

#ifdef USE_STRUCTS
        case T_STRUCT:
            show_struct(d, (char *)svp->u.strct, depth+1);
            break;
#endif /* USE_STRUCTS */

        case T_NUMBER:
            writed(d, (p_uint)svp->u.number);
            WRITES(d, "\n");
            break;

        case T_STRING:
            if (is_freed(svp->u.str, 1) )
            {
                WRITES(d, "String in freed block 0x");
                write_x(d, (p_uint)((unsigned *)block - xalloc_overhead()));
                WRITES(d, "\n");
                break;
            }
            WRITES(d, "String: ");
            show_mstring(d, svp->u.str, 0);
            WRITES(d, "\n");
            break;

        case T_CLOSURE:
            if (svp->x.closure_type == CLOSURE_LFUN
             || svp->x.closure_type == CLOSURE_IDENTIFIER)
               show_cl_literal(d, (char *)svp->u.lambda, depth);
            else
            {
                WRITES(d, "Closure type ");
                writed(d, svp->x.closure_type);
                WRITES(d, "\n");
            }
            break;

        case T_OBJECT:
            show_object(d, (char *)svp->u.ob, 1);
            break;

        default:
            WRITES(d, "Svalue type ");writed(d, svp->type);WRITES(d, "\n");
            break;
        }
    }
} /* show_array() */

#ifdef USE_STRUCTS
/*-------------------------------------------------------------------------*/
static void
show_struct(int d, void *block, int depth)

/* Print the struct at recursion <depth> from memory <block> on
 * filedescriptor <d>. Recursive printing stops at <depth> == 2.
 */

{
    struct_t *a;
    mp_int i, j;
    svalue_t *svp;
    wiz_list_t *user;
    mp_int a_size;

    user = NULL;

    a = (struct_t *)block;

    /* Can't use struct_size() here, as the memory block may have been
     * partly overwritten by the smalloc pointers already.
     */
    a_size = (mp_int)(  xalloced_size(a)
                   - ( xalloc_overhead() + 
                       ( sizeof(struct_t) - sizeof(svalue_t) ) / SIZEOF_CHAR_P 
                     ) 

                  ) / (sizeof(svalue_t)/SIZEOF_CHAR_P);

    if (depth)
    {
        int freed;
        wiz_list_t *wl;

        wl = NULL;
        freed = is_freed(block, sizeof(vector_t) );
        if (!freed)
        {
            user = a->user;
            wl = all_wiz;
            if (user)
                for ( ; wl && wl != user; wl = wl->next) NOOP;
        }
        if (freed || !wl || a_size <= 0
         || (xalloced_size((char *)a) - xalloc_overhead()) << 2 !=
              sizeof(struct_t) + sizeof(svalue_t) * (a_size - 1) )
        {
            WRITES(d, "struct in freed block 0x");
            write_x(d, (p_uint)((unsigned *)block - xalloc_overhead()));
            WRITES(d, "\n");
            return;
        }
    }
    else
    {
        user = a->user;
    }

    WRITES(d, "struct ");
    write_x(d, (p_int)a);
    WRITES(d, " size ");writed(d, (p_uint)a_size);
    WRITES(d, ", uid: ");
    show_string(d, user ? (user->name ? get_txt(user->name) : "<null>")
                        : "0", 0);
    WRITES(d, "\n");
    if (depth > 2)
        return;

    i = 32 >> depth;
    if (i > a_size)
        i = a_size;

    for (svp = a->member; --i >= 0; svp++)
    {
        for (j = depth + 1; --j >= 0;) WRITES(d, " ");
        switch(svp->type)
        {
        case T_POINTER:
            show_array(d, (char *)svp->u.vec, depth+1);
            break;

        case T_STRUCT:
            show_struct(d, (char *)svp->u.strct, depth+1);
            break;

        case T_NUMBER:
            writed(d, (p_uint)svp->u.number);
            WRITES(d, "\n");
            break;

        case T_STRING:
            if (is_freed(svp->u.str, 1) )
            {
                WRITES(d, "String in freed block 0x");
                write_x(d, (p_uint)((unsigned *)block - xalloc_overhead()));
                WRITES(d, "\n");
                break;
            }
            WRITES(d, "String: ");
            show_mstring(d, svp->u.str, 0);
            WRITES(d, "\n");
            break;

        case T_CLOSURE:
            if (svp->x.closure_type == CLOSURE_IDENTIFIER)
               show_cl_literal(d, (char *)svp->u.lambda, depth);
            else
            {
                WRITES(d, "Closure type ");
                writed(d, svp->x.closure_type);
                WRITES(d, "\n");
            }
            break;

        case T_OBJECT:
            show_object(d, (char *)svp->u.ob, 1);
            break;

        default:
            WRITES(d, "Svalue type ");writed(d, svp->type);WRITES(d, "\n");
            break;
        }
    }
} /* show_struct() */
#endif /* USE_STRUCTS */

/*-------------------------------------------------------------------------*/
void
setup_print_block_dispatcher (void)

/* Setup the tracing data dispatcher in xmalloc with the show_ functions
 * above. Remember that the data dispatcher works by storing the file
 * and line information of sample allocations. We just have to make sure
 * to cover all possible allocation locations (with the string module and
 * its pervasive inlining this is not easy).
 *
 * This is here because I like to avoid xmalloc calling closures, and
 * gcollect.c is already notorious for including almost every header file
 * anyway.
 */

{
    svalue_t tmp_closure;
    vector_t *a, *b;

    assert_master_ob_loaded();

#if 0
    /* Since the strings store the location of the call to the function
     * which in turn called the string module function, the print
     * block dispatcher won't be able to recognize them.
     */
    store_print_block_dispatch_info(STR_EMPTY, show_mstring);
    store_print_block_dispatch_info(STR_EMPTY->str, show_mstring_data);
    str = mstring_alloc_string(1);
    store_print_block_dispatch_info(str, show_mstring);
    store_print_block_dispatch_info(str->str, show_mstring_data);
    mstring_free(str);
    str = mstring_new_string("\t");
    store_print_block_dispatch_info(str, show_mstring);
    store_print_block_dispatch_info(str->str, show_mstring_data);
    str = mstring_table_inplace(str);
    store_print_block_dispatch_info(str->link, show_mstring);
    mstring_free(str);
#endif

    a = allocate_array(1);
    store_print_block_dispatch_info((char *)a, show_array);
    b = slice_array(a, 0, 0);
    store_print_block_dispatch_info((char *)b, show_array);
    free_array(a);
    free_array(b);
    store_print_block_dispatch_info((char *)master_ob, show_object);
#ifdef CHECK_OBJECT_GC_REF
    note_object_allocation_info((char*)master_ob);
    note_program_allocation_info((char*)(master_ob->prog));
#endif

    tmp_closure.type = T_CLOSURE;
    tmp_closure.x.closure_type = CLOSURE_EFUN + F_ALLOCATE;
    tmp_closure.u.ob = master_ob;
    push_number(inter_sp, 1);
    call_lambda(&tmp_closure, 1);
    store_print_block_dispatch_info(inter_sp->u.vec, show_array);
    free_svalue(inter_sp--);

    current_object = master_ob;
#ifdef USE_NEW_INLINES
    closure_literal(&tmp_closure, 0, 0, 0);
#else
    closure_literal(&tmp_closure, 0, 0);
#endif /* USE_NEW_INLINES */
    store_print_block_dispatch_info(tmp_closure.u.lambda, show_cl_literal);
    free_svalue(&tmp_closure);
}
#endif /* MALLOC_TRACE */

#endif /* GC_SUPPORT */

/*=========================================================================*/

/*       Default functions when the allocator doesn't support GC.
 */

#if !defined(GC_SUPPORT)

void
garbage_collection (void)

/* Free as much memory as possible and try to reallocate the
 * reserved areas - that's all we can do.
 */

{
    assert_master_ob_loaded();
    handle_newly_destructed_objects();
    free_save_object_buffers();
    free_interpreter_temporaries();
    free_action_temporaries();
#ifdef USE_PGSQL
    pg_purge_connections();
#endif /* USE_PGSQL */
    remove_stale_player_data();
    remove_stale_call_outs();
    mb_release();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
    check_wizlist_for_destr();
    cleanup_all_objects();
    if (current_error_trace)
    {
        free_array(current_error_trace);
        current_error_trace = NULL;
    }
    if (uncaught_error_trace)
    {
        free_array(uncaught_error_trace);
        uncaught_error_trace = NULL;
    }
    remove_destructed_objects(MY_TRUE);

    reallocate_reserved_areas();
    time_last_gc = time(NULL);
}
#endif /* GC_SUPPORT */


#if !defined(MALLOC_TRACE) || !defined(GC_SUPPORT)

void setup_print_block_dispatcher (void) { NOOP }

#endif

/***************************************************************************/

