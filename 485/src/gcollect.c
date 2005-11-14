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
 * This facility is available only when using the 'smalloc'
 * memory allocator. When using a different allocator, all garbage_collect()
 * does is freeing as much memory as possible.
 *
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
 *     void clear_memory_reference(void *p)
 *         Clear the memory block marker for <p>.
 *
 *     void note_malloced_block_ref(void *p)
 *         Note the reference to memory block <p>.
 *
 *     void clear_inherit_ref(program_t *p)
 *         Clear the refcounts of all inherited programs of <p>.
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
 *     void count_ref_from_string(char *p);
 *         Count the reference to shared string <p>.
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
 * TODO: Change all non-shared strings in shared ones after finishing the GC.
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

#define NO_REF_STRING
#include "gcollect.h"
#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
#include "heartbeat.h"
#include "instrs.h"
#include "interpret.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "otable.h"
#include "parse.h"
#include "prolang.h"
#include "rxcache.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "smalloc.h"
#include "stdstrings.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

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

static size_t size_alloc_strings;
static size_t num_alloc_strings;
  /* Number and size of unshared strings encountered.
   */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/

#if defined(MALLOC_smalloc)

#define CLEAR_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] &= ~M_REF )
  /* Clear the memory block marker for <p>
   */

#ifdef CHECK_OBJECT_GC_REF
static unsigned long gc_mark_ref(void * p, const char * file, int line)
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
    return ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] |= M_REF );
}

#define MARK_REF(p) gc_mark_ref(p, __FILE__, __LINE__)
#define MARK_PLAIN_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] |= M_REF )

#else
#define MARK_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] |= M_REF )
  /* Set the memory block marker for <p>
   */
#define MARK_PLAIN_REF(p) MARK_REF(p)
#endif

#define TEST_REF(p) ( !( ((p_uint *)(p))[-SMALLOC_OVERHEAD] & M_REF ) )
  /* Check the memory block marker for <p>, return TRUE if _not_ set.
   */

#define CHECK_REF(p) ( TEST_REF(p) && ( MARK_REF(p),MY_TRUE ) )
  /* Check the memory block marker for <p> and set it if necessary.
   * Return TRUE if the marker was not set, FALSE else.
   */

#define STRING_REFS(str)  (*(StrRefCount *)((char *) (str)\
                           - sizeof(StrRefCount)))
  /* Return the refcount of shared string <str>
   */

#if !defined(CHECK_STRINGS) && !defined(DEBUG)

#ifndef DUMP_GC_REFS

#define MARK_STRING_REF(str) ((void)(\
    STRING_REFS(str)++ || (\
        CHECK_REF( (str)-sizeof(StrRefCount)-sizeof(char *) ) ||\
            /* reached max ref count, which is given as 0... */ \
            STRING_REFS(str)--\
    ))\
)
  /* Increment the refcount of shared string <str>. How it works:
   * If STRING_REFS() is 0, the refcount either overflowed or it is
   * the first visit to the block. If it's the first visit, CHECK_REF
   * will return TRUE, otherwise we have an overflow and the STRING_REFS--
   * will undo the ++ from earlier.
   */

#else

#define MARK_STRING_REF(p) \
    do { char *p_ = p; \
         dprintf4(gcollect_outfd, "Mark string ref %x (%x): %s %d\n", (long)p_, (long)(p_-sizeof(StrRefCount)-sizeof(char*)), (long)__FILE__, (long)__LINE__); \
        STRING_REFS(p_)++ \
        || (CHECK_REF( (p_)-sizeof(StrRefCount)-sizeof(char *) ) \
            ||  STRING_REFS(p_)-- \
           ); \
    } while(0)

#endif /* DUMP_GC_REFS */

#else

static void MARK_STRING_REF (char * str)
{
    if (CHECK_REF( (str)-sizeof(StrRefCount)-sizeof(char *) ) )
    {
        /* First visit to this block */
        STRING_REFS(str)++;
#ifdef CHECK_STRINGS
        mark_shadow_string_ref(str);
#endif
    }
    else if (STRING_REFS(str))
    {
        /* Not the first visit, and refcounts didn't overrun either */
        STRING_REFS(str)++;
        if (!STRING_REFS(str))
        {
            /* Refcount overflow */
            dprintf2(gout, "DEBUG: mark string: %x '%s' refcount reaches max!\n"
                    , (p_int)str, (p_int)str);
        }
#ifdef CHECK_STRINGS
        inc_shadow_string_ref(str);
#endif
    }
}

#endif /* DEBUG || CHECK_STRINGS */


/* Forward declarations */

static void clear_map_ref_filter (svalue_t *, svalue_t *, void *);
static void clear_ref_in_closure (lambda_t *l, ph_int type);
static void gc_count_ref_in_closure (svalue_t *csvp);

#define count_ref_in_closure(p) \
    GC_REF_DUMP(svalue_t*, p, "Count ref in closure", gc_count_ref_in_closure)

#endif /* MALLOC_smalloc */


/*=========================================================================*/

/*            The real collector - only with smalloc.
 */

#if defined(MALLOC_smalloc)

#if defined(MALLOC_TRACE)

#define WRITES(d, s) write((d), (s), strlen(s))

/*-------------------------------------------------------------------------*/
static INLINE void
write_malloc_trace (void * p)

/* Dump the allocation information for <p>, if any.
 */

{
    WRITES(gout, ((char **)(p))[-3]);
    WRITES(gout, " ");
    writed(gout, (int)((p_uint *)(p))[-2]);
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
}

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
clear_inherit_ref (program_t *p)

/* Clear the refcounts of all inherited programs of <p>.
 */

{
    int i;

    for (i = 0; i < p->num_inherited; i++)
    {
        /* Inherited programs are never swapped. Only programs with blueprints
         * are swapped, and a blueprint and one inheritance makes two refs.
         */
        program_t *p2;

        p2 = p->inherit[i].prog;
        if (p2->ref)
        {
            p2->ref = 0;
            clear_inherit_ref(p2);
        }
    }
} /* clear_inherit_ref() */

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
        p->prog->ref = 0;
#ifndef NO_BLUEPRINT
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
#endif /* !NO_BLUEPRINT */
        clear_inherit_ref(p->prog);
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
        char **strings;
        variable_t *variable_names;

        if (p->ref++)
        {
            dump_malloc_trace(1, p);
            fatal("First reference to program %p '%s', but ref count %ld != 0\n"
                 , p, p->name, (long)p->ref - 1
                 );
        }

        /* Mark the blueprint object, if any */
#ifndef NO_BLUEPRINT
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
#endif /* !NO_BLUEPRINT */

        if (p->line_numbers)
            note_ref(p->line_numbers);

        /* Non-inherited functions */

        for (i = p->num_functions; --i >= 0; )
        {
            if ( !(functions[i] & NAME_INHERITED) )
            {
                char *name;

                memcpy(
                  (char *)&name,
                  program + (functions[i] & FUNSTART_MASK) - 1 - sizeof name,
                  sizeof name
                );
                MARK_STRING_REF(name);
            }
        }

        /* String literals */

        strings = p->strings;
        for (i = p->num_strings; --i >= 0; )
        {
            char *str = *strings++;
            MARK_STRING_REF(str);
        }

        /* Variable names */

        variable_names = p->variable_names;
        for (i = p->num_variables; --i >= 0; variable_names++)
            MARK_STRING_REF(variable_names->name);

        /* Inherited programs */

        for (i=0; i< p->num_inherited; i++)
            mark_program_ref(p->inherit[i].prog);

        /* Included files */

        for (i=0; i< p->num_includes; i++)
        {
            char *str;
            str = p->includes[i].name; MARK_STRING_REF(str);
            str = p->includes[i].filename; MARK_STRING_REF(str);
        }

        note_ref(p->name);
    }
    else
    {
        if (!p->ref++)
        {
            dump_malloc_trace(1, p);
            fatal("Program block %p '%s' referenced as something else\n"
                 , p, p->name);
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
    mark_program_ref(ob->prog);
    note_ref(ob->name);
    MARK_STRING_REF(ob->load_name);
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
                 , ob, ob->name, (long)ob->ref
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
                 , ob, ob->name);
        }
    }
} /* gc_reference_destructed_object() */

/*-------------------------------------------------------------------------*/
void
gc_count_ref_from_string (char *p)

/* Count the reference to shared string <p>.
 */

{
   MARK_STRING_REF(p);
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
}

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

        case T_POINTER:
        case T_QUOTED_ARRAY:
            if (!p->u.vec->ref)
                continue;
            p->u.vec->ref = 0;
            clear_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            continue;

        case T_MAPPING:
            if (p->u.map->ref)
            {
                mapping_t *m;
                p_int num_values;

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
 * Closures bound to destructed objects are replaced by #'undef operator
 * closures.
 * TODO: Was the done to preserve sorting orders? Apart from callbacks,
 * TODO:: replacing it by 0 would do (but see mapping.c). Or even better:
 * TODO:: introduce an ILLEGAL svalue type for such occasions which compares
 * TODO:: equal to svalue 0.
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

        case T_MAPPING:
            if (CHECK_REF(p->u.map))
            {
                mapping_t *m;
                struct condensed_mapping *cm;
                int num_values;

                m = p->u.map;
                cm = m->condensed;
                num_values = m->num_values;
                passed_note_ref((char *)CM_MISC(cm) - cm->misc_size *(num_values + 1));
                /* hash mappings have been eleminated at the start */
                count_ref_in_mapping(m);
                count_mapping_size(m);
            }
            p->u.map->ref++;
            continue;

          case T_STRING:
              switch(p->x.string_type)
              {
              case STRING_MALLOC:
                  passed_note_ref(p->u.string);
                  size_alloc_strings += strlen(p->u.string)+1;
                  num_alloc_strings++;
                  break;
              case STRING_SHARED:
                  MARK_STRING_REF(p->u.string);
                  break;
              }
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
                      p->x.closure_type = F_UNDEF+CLOSURE_EFUN;
                      p->u.ob = master_ob;
                      master_ob->ref++;
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
                      master_ob->extra_ref++;
#endif
                      reference_destructed_object(ob);
                  }
                  else
                  {
                      ob->ref++;
                  }
              }
              continue;

          case T_SYMBOL:
              MARK_STRING_REF(p->u.string);
              continue;
        }
    } /* for */
} /* gc_count_ref_in_vector() */

/*-------------------------------------------------------------------------*/
static void
remove_unreferenced_string (char *start, char *string)

/* If the shared string <string> stored in the memory block <start> is
 * not referenced, it is deallocated.
 */

{
    if (TEST_REF(start))
    {
#ifdef KEEP_STRINGS
        dprintf1(gout, "shared string %x was left unreferenced.\n",
          (p_int) string
        );

        MARK_REF(start);
#else
        dprintf2(gout,
"shared string %x '%s' was left unreferenced, freeing now.\n",
          (p_int) string, (p_int)string
        );

        MARK_REF(start);
#ifdef CHECK_STRINGS
        mark_shadow_string_ref(string);
#endif
        STRING_REFS(string)++;
        free_string(string);
#endif
    }
}

/*-------------------------------------------------------------------------*/
static void
gc_note_action_ref (action_t *p)

/* Mark the strings of function and verb of all sentences in list <p>.
 */

{
    do {
        if (p->function)
            MARK_STRING_REF(p->function);
        if (p->verb)
            MARK_STRING_REF(p->verb);
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
 * for later removal (and .ref is set to -1), and the svalue is transformed
 * into a #'undef operator closure.
 * TODO: Was the transform done to preserve sorting orders? Apart from callbacks,
 * TODO:: replacing it by 0 would do (but see mapping.c). Or even better:
 * TODO:: introduce an ILLEGAL svalue type for such occasions which compares
 * TODO:: equal to svalue 0.
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
            csvp->x.closure_type = F_UNDEF+CLOSURE_EFUN;
            csvp->u.ob = master_ob;
            master_ob->ref++;
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
            master_ob->extra_ref++;
#endif
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
         || (   type == CLOSURE_ALIEN_LFUN
             && l->function.alien.ob->flags & O_DESTRUCTED) )
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
                if (type == CLOSURE_ALIEN_LFUN)
                {
                    if (l->function.alien.ob->flags & O_DESTRUCTED)
                        reference_destructed_object(l->function.alien.ob);
                }
            }

            if (ob->flags & O_DESTRUCTED)
                reference_destructed_object(ob);

            if (type == CLOSURE_BOUND_LAMBDA)
            {
                csvp->x.closure_type = CLOSURE_UNBOUND_LAMBDA;
                csvp->u.lambda = l->function.lambda;
            }
            else
            {
                csvp->x.closure_type = F_UNDEF+CLOSURE_EFUN;
                csvp->u.ob = master_ob;
                master_ob->ref++;
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
                master_ob->extra_ref++;
#endif
            }
        }
        else
        {
             /* Object exists: count reference */

            ob->ref++;
            if (type == CLOSURE_ALIEN_LFUN)
                l->function.alien.ob->ref++;
        }
    }

    /* Count the references in the code of the closure */

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
    }
} /* count_ref_in_closure() */

/*-------------------------------------------------------------------------*/
static void
clear_ref_in_closure (lambda_t *l, ph_int type)

/* Clear the references in closure <l> which is of type <type>.
 */

{
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

    if (type == CLOSURE_ALIEN_LFUN)
        clear_object_ref(l->function.alien.ob);
} /* clear_ref_in_closure() */

/*-------------------------------------------------------------------------*/
static void
remove_uids (int smart UNUSED)

/* ??? */

{
#ifdef __MWERKS__
#    pragma unused(smart)
#endif
    NOOP
}

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
} /* restore_default_log() */

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

    size_alloc_strings = 0;
    num_alloc_strings = 0;

    if (gcollect_outfd != 1 && gcollect_outfd != 2)
    {
        dprintf1(gcollect_outfd, "\n%s --- Garbage Collection ---\n"
                               , (long)time_stamp());
    }

    /* --- Pass 0: dispose of some unnecessary stuff ---
     */

    malloc_privilege = MALLOC_MASTER;
    RESET_LIMITS;
    CLEAR_EVAL_COST;
    out_of_memory = MY_FALSE;
    assert_master_ob_loaded();
    malloc_privilege = MALLOC_SYSTEM;
    if (obj_list_replace)
        replace_programs();
    handle_newly_destructed_objects();
    free_interpreter_temporaries();
    free_action_temporaries();
    remove_stale_player_data();
    remove_stale_call_outs();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
#ifdef USE_FREE_CLOSURE_HOOK
    free_old_driver_hooks();
#endif
    purge_action_sent();
    purge_shadow_sent();
    check_wizlist_for_destr();
    compact_mappings(num_dirty_mappings);
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

    /* Lock all interactive structures (in case we're using threads)
     * and dispose of the written buffers.
     */
    for (i = 0 ; i < MAX_PLAYERS; i++)
    {
        if (all_players[i] == NULL)
            continue;

        interactive_lock(all_players[i]);
        interactive_cleanup(all_players[i]);
    }

    remove_destructed_objects(); /* After reducing all object references! */

    clear_array_size();
    clear_mapping_size();

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


    /* --- Pass 1: clear the M_REF flag in all malloced blocks ---
     */
    clear_M_REF_flags();

    /* --- Pass 2: clear the ref counts ---
     */

    gc_status = gcClearRefs;
    if (d_flag > 3)
    {
        debug_message("%s start of garbage_collection\n", time_stamp());
    }

    /* Process the list of all objects */

    for (ob = obj_list; ob; ob = ob->next_all) {
        int was_swapped;

        if (d_flag > 4)
        {
            debug_message("%s clearing refs for object '%s'\n"
                         , time_stamp(), ob->name);
        }
        was_swapped = 0;
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
        ob->extra_ref = ob->ref;
#endif
        if (ob->flags & O_SWAPPED
         && (was_swapped = load_ob_from_swap(ob)) & 1)
        {
            /* don't clear the program ref count. It is 1 */
        }
        else
        {
            /* Take special care of inherited programs, the associated
             * objects might be destructed.
             */
            ob->prog->ref = 0;
        }
        if (was_swapped < 0)
            fatal("Totally out of MEMORY in GC: (swapping in '%s')\n"
                 , ob->name);

        clear_inherit_ref(ob->prog);
        ob->ref = 0;
        clear_ref_in_vector(ob->variables, ob->prog->num_variables);
        if (ob->flags & O_SHADOW)
        {
            ed_buffer_t *buf;

            if ( NULL != (buf = O_GET_EDBUFFER(ob)) )
            {
                clear_ed_buffer_refs(buf);
            } /* end of ed-buffer processing */
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
        input_to_t * it;

        if (all_players[i] == NULL)
            continue;

#ifdef USE_PTHREAD
        {
            struct write_buffer_s *pwb;
            for (pwb = all_players[i]->write_first; pwb != NULL; pwb = pwb->next)
                clear_memory_reference(pwb);
            if ((pwb = all_players[i]->write_current) != NULL)
                clear_memory_reference(pwb);
            /* .written_first has been cleaned upL */
        }
#endif
        for ( it = all_players[i]->input_to; it != NULL; it = it->next)
        {
            clear_memory_reference(it);
            clear_ref_in_callback(&(it->fun));
            clear_ref_in_vector(&(it->prompt), 1);
        }
        clear_ref_in_vector(&all_players[i]->prompt, 1);

        /* snoop_by and modify_command are known to be NULL or non-destructed
         * objects.
         */
    }

    /* Process the driver hooks */

    for (i = NUM_DRIVER_HOOKS; --i >= 0; ) {
        if (driver_hook[i].type == T_CLOSURE
        &&  driver_hook[i].x.closure_type == CLOSURE_LAMBDA)
        {
            driver_hook[i].x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        }
    }
    clear_ref_in_vector(driver_hook, NUM_DRIVER_HOOKS);

    /* Let the modules process their data */

    clear_shared_string_refs();
    clear_ref_from_wiz_list();
    clear_ref_from_call_outs();
#if defined(SUPPLY_PARSE_COMMAND)
    clear_parse_refs();
    clear_old_parse_refs();
#endif
    clear_simul_efun_refs();
    clear_interpreter_refs();
    clear_comm_refs();
#ifdef RXCACHE_TABLE
    clear_rxcache_refs();
#endif
#ifdef CHECK_MAPPINGS
    clear_ref_in_msbuf();
#endif

    null_vector.ref = 0;

    /* Finally, walk the list of destructed objects and clear all references
     * in them.
     */
    for (ob = destructed_objs; ob;  ob = ob->next_all)
    {
        if (d_flag > 4)
        {
            debug_message("%s clearing refs for destructed object '%s'\n"
                         , time_stamp(), ob->name);
        }

        ob->prog->ref = 0;
        clear_inherit_ref(ob->prog);
        ob->ref = 0;
    }


    /* --- Pass 3: Compute the ref counts, and set M_REF where appropriate ---
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
                , (p_int)ob->name
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
            ed_buffer_t *buf;

            sent = ob->sent;
            if (ob->flags & O_SHADOW)
            {
                note_ref(sent);
                if ( NULL != (buf = ((shadow_t *)sent)->ed_buffer) )
                {
                    note_ref(buf);
                    count_ed_buffer_refs(buf);
                } /* end of ed-buffer processing */

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
        input_to_t * it;

        if (all_players[i] == NULL)
            continue;

        note_ref(all_players[i]);
#ifdef USE_PTHREAD
        {
            struct write_buffer_s *pwb;
            for (pwb = all_players[i]->write_first; pwb != NULL; pwb = pwb->next)
                note_ref(pwb);
            if ((pwb = all_players[i]->write_current) != NULL)
                note_ref(pwb);
            /* .written_first has been cleaned upL */
        }
#endif

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

        for ( it = all_players[i]->input_to; it != NULL; it = it->next)
        {
            note_ref(it);
            count_ref_in_callback(&(it->fun));
            count_ref_in_vector(&(it->prompt), 1);
        } /* end of input_to processing */

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

    if (master_ob)
        master_ob->ref++;
    else
        fatal("No master object\n");

    /* TODO: see array.c */
    if (last_insert_alist_shared_string)
    {
        MARK_STRING_REF(last_insert_alist_shared_string);
    }

    count_lex_refs();
    count_compiler_refs();
    count_simul_efun_refs();
#if defined(SUPPLY_PARSE_COMMAND)
    count_old_parse_refs();
#endif
    note_shared_string_table_ref();
    note_otable_ref();
    count_comm_refs();
    count_interpreter_refs();
    count_heart_beat_refs();
#ifdef RXCACHE_TABLE
    count_rxcache_refs();
#endif
#ifdef CHECK_MAPPINGS
    count_ref_in_msbuf();
#endif


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
    for (i = NUM_DRIVER_HOOKS; --i >= 0; )
    {
        if (driver_hook[i].type == T_CLOSURE &&
            driver_hook[i].x.closure_type == CLOSURE_UNBOUND_LAMBDA)
        {
            driver_hook[i].x.closure_type = CLOSURE_LAMBDA;
        }
    }

    gc_status = gcInactive;

    /* --- Pass 4: remove stralloced strings with M_REF cleared ---
     */

    walk_shared_strings(remove_unreferenced_string);

    /* --- Pass 5: Release all destructed objects ---
     *
     * It is vital that all information freed here is already known
     * as referenced, so we won't free it a second time in pass 6.
     */

    dobj_count = 0;
    for (ob = gc_obj_list_destructed; ob; ob = next_ob)
    {
#ifdef DEBUG
        dprintf1(gcollect_outfd, "DEBUG: GC frees destructed '%s'\n", (p_int)ob->name);
#endif
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

    free_unreferenced_memory();
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
        remove_uids(res && (res->type != T_NUMBER || res->u.number) );
    }

    /* Reconsolidate the free lists */
    consolidate_freelists();

    /* Finally, try to reclaim the reserved areas */

    reallocate_reserved_areas();

    time_last_gc = time(NULL);
    dprintf2(gcollect_outfd, "%s GC freed %d destructed objects.\n"
            , (long)time_stamp(), dobj_count);
printf("DEBUG: %s GC found %ld malloced strings using %ld bytes.\n", time_stamp(), (long)num_alloc_strings, (long)size_alloc_strings); /* TODO: Remove this line */
#if defined(CHECK_OBJECT_REF) && defined(DEBUG)
    for (ob = obj_list; ob; ob = ob->next_all) {
        if (ob->extra_ref != ob->ref
         && strchr(ob->name, '#') == NULL
           )
        {
            dprintf4(2, "DEBUG: GC object %x '%s': refs %d, extra_refs %d\n"
                      , (p_int)ob, (p_int)ob->name, (p_int)ob->ref
                      , (p_int)ob->extra_ref);
        }
    }
#endif

    /* Lock all interactive structures (in case we're using threads)
     * and dispose of the written buffers.
     */
    for (i = 0 ; i < MAX_PLAYERS; i++)
    {
        if (all_players[i] == NULL)
            continue;

        interactive_unlock(all_players[i]);
    }

    /* If the GC log was redirected, close that file and set the
     * logging back to the default file.
     */
    restore_default_gc_log();
} /* garbage_collection() */


#if defined(MALLOC_TRACE)

/* Some functions to print the tracing data from the memory blocks.
 * The show_ functions are called from smalloc directly.
 */

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
show_added_string (int d, char *block, int depth UNUSED)

/* Print the string from memory <block> on filedescriptor <d>, prefixed
 * by 'Added string: '.
 */

{
#ifdef __MWERKS__
#    pragma unused(depth)
#endif
    WRITES(d, "Added string: ");
    show_string(d, block, 0);
    WRITES(d, "\n");
}

/*-------------------------------------------------------------------------*/
static void
show_object (int d, char *block, int depth)

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
            write_x(d, (p_uint)((unsigned *)block - SMALLOC_OVERHEAD));
            WRITES(d, "\n");
            return;
        }
    }
    WRITES(d, "Object: ");
    if (ob->flags & O_DESTRUCTED)
        WRITES(d, "(destructed) ");
    show_string(d, ob->name, 0);
    WRITES(d, " from ");
    show_string(d, ob->load_name, 0);
    WRITES(d, ", uid: ");
    show_string(d, ob->user->name ? ob->user->name : "0", 0);
    WRITES(d, "\n");
} /* show_object() */

/*-------------------------------------------------------------------------*/
static void
show_cl_literal (int d, char *block, int depth UNUSED)

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
            show_string(d, obj->name, 0);
        else
            WRITES(d, "(no name)");
        if (obj->flags & O_DESTRUCTED)
            WRITES(d, " (destructed)");
    }
    else
        WRITES(d, "<null>");

    WRITES(d, ", index ");
    writed(d, l->function.index);
    WRITES(d, ", ref ");
    writed(d, l->ref);
    WRITES(d, "\n");
} /* show_cl_literal() */

/*-------------------------------------------------------------------------*/
static void
show_array(int d, char *block, int depth)

/* Print the array at recursion <depth> from memory <block> on
 * filedescriptor <d>. Recursive printing stops at <depth> == 2.
 */

{
    vector_t *a;
    mp_int i, j;
    svalue_t *svp;
    wiz_list_t *user;
    mp_int a_size;

    a = (vector_t *)block;

    /* Can't use VEC_SIZE() here, as the memory block may have been
     * partly overwritten by the smalloc pointers already.
     */
    a_size = (mp_int)(  malloced_size(a)
                   - ( SMALLOC_OVERHEAD + 
                       ( sizeof(vector_t) - sizeof(svalue_t) ) / SIZEOF_CHAR_P 
                     ) 

                  ) / (sizeof(svalue_t)/SIZEOF_CHAR_P);

    if (depth && a != &null_vector)
    {
        int freed;
        wiz_list_t *wl;

        freed = is_freed(block, sizeof(vector_t) );
        if (!freed)
        {
            user = a->user;
            wl = all_wiz;
            if (user)
                for ( ; wl && wl != user; wl = wl->next) NOOP;
        }
        if (freed || !wl || a_size <= 0 || a_size > MAX_ARRAY_SIZE
         || (malloced_size((char *)a) - SMALLOC_OVERHEAD) << 2 !=
              sizeof(vector_t) + sizeof(svalue_t) * (a_size - 1) )
        {
            WRITES(d, "Array in freed block 0x");
            write_x(d, (p_uint)((unsigned *)block - SMALLOC_OVERHEAD));
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
    WRITES(d, " size ");writed(d, (p_uint)a_size);
    WRITES(d, ", uid: ");show_string(d, user ? user->name : "0", 0);
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

        case T_NUMBER:
            writed(d, (p_uint)svp->u.number);
            WRITES(d, "\n");
            break;

        case T_STRING:
            if (svp->x.string_type == STRING_MALLOC &&
                  is_freed(svp->u.string, 1) )
            {
                WRITES(d, "Malloced string in freed block 0x");
                write_x(d, (p_uint)((unsigned *)block - SMALLOC_OVERHEAD));
                WRITES(d, "\n");
                break;
            }
            if (svp->x.string_type == STRING_SHARED &&
                is_freed(SHSTR_BLOCK(svp->u.string),
                         sizeof(char *) + sizeof(StrRefCount) + 1) )
            {

                WRITES(d, "Shared string in freed block 0x");
                write_x(d, (p_uint)(
                  (unsigned *)(block-sizeof(char *)-sizeof(StrRefCount))
                  - SMALLOC_OVERHEAD
                ));
                WRITES(d, "\n");
                break;
            }
            WRITES(d, "String: ");
            show_string(d, svp->u.string, 0);
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

/*-------------------------------------------------------------------------*/
void
setup_print_block_dispatcher (void)

/* Setup the tracing data dispatcher in smalloc with the show_ functions
 * above. Remember that the data dispatcher works by storing the file
 * and line information of sample allocations. We just have to make sure
 * to cover all possible allocation locations.
 *
 * This is here because I like to avoid smalloc calling closures, and
 * gcollect.c is already notorious for including almost every header file
 * anyway.
 */

{
    svalue_t tmp_closure;
    vector_t *a, *b;

    assert_master_ob_loaded();

    tmp_closure.type = T_CLOSURE;
    tmp_closure.x.closure_type = CLOSURE_EFUN + F_ADD;
    tmp_closure.u.ob = master_ob;
    push_volatile_string("");
    push_volatile_string("");
    call_lambda(&tmp_closure, 2);
    store_print_block_dispatch_info(inter_sp->u.string, show_added_string);
    free_svalue(inter_sp--);

    tmp_closure.type = T_CLOSURE;
    tmp_closure.x.closure_type = CLOSURE_EFUN + F_ALLOCATE;
    tmp_closure.u.ob = master_ob;
    push_number(1);
    call_lambda(&tmp_closure, 1);
    store_print_block_dispatch_info(inter_sp->u.vec, show_array);
    free_svalue(inter_sp--);

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

    current_object = master_ob;
    closure_literal(&tmp_closure, 0);
    store_print_block_dispatch_info(tmp_closure.u.lambda, show_cl_literal);
    free_svalue(&tmp_closure);
}
#endif /* MALLOC_TRACE */

#endif /* MALLOC_smalloc */

/*=========================================================================*/

/*       Default functions when not using the smalloc allocator
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
    free_interpreter_temporaries();
    free_action_temporaries();
    remove_stale_player_data();
    remove_stale_call_outs();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
    purge_action_sent();
    purge_shadow_sent();
    check_wizlist_for_destr();
    compact_mappings(num_dirty_mappings);
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
    remove_destructed_objects();

    reallocate_reserved_areas();
    time_last_gc = time(NULL);
}
#endif /* GC_SUPPORT */


#if !defined(MALLOC_TRACE) || !defined(GC_SUPPORT)

void setup_print_block_dispatcher (void) { NOOP }

#endif

/***************************************************************************/

