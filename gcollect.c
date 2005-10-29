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
 *     void clear_memory_reference(char *p)
 *         Clear the memory block marker for <p>.
 *
 *     void note_malloced_block_ref(char *p)
 *         Note the reference to memory block <p>.
 *
 *     void clear_inherit_ref(struct program *p)
 *         Clear the refcounts of all inherited programs of <p>.
 *
 *     void mark_program_ref(struct program *p);
 *         Set the marker of program <p> and of all data referenced by <p>.
 *
 *     void reference_destructed_object(struct object *ob)
 *         Note the reference to a destructed object <ob>.
 *
 *     void count_ref_from_string(char *p);
 *         Count the reference to shared string <p>.
 *
 *     void clear_ref_in_vector(struct svalue *svp, int num);
 *         Clear the refs of the <num> elements of vector <svp>.
 *
 *     void count_ref_in_vector(struct svalue *svp, int num)
 *         Count the references the <num> elements of vector <p>.
 *
 * The referencing code for dynamic data should mirror the destructor code,
 * thus, memory leaks can show up as soon as the memory is allocated.
 *
 * TODO: Allow to deactivate the dump of unreferenced memory on freeing.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <sys/types.h>
#include <sys/time.h>

#define NO_INCREMENT_STRING_REF
#include "gcollect.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
#include "interpret.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "otable.h"
#include "prolang.h"
#include "rxcache.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "smalloc.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"


/*-------------------------------------------------------------------------*/

time_t time_last_gc = 0;
  /* Time of last gc, used by the backend to avoid repeated collections
   * when the memory usage is at the edge of a shortage.
   */


#if defined(MALLOC_smalloc)

int gcollect_outfd = 2;
#define gout gcollect_outfd
  /* The file (default is stderr) to dump the reclaimed blocks on.
   */


/* Are the ref counts unusable, i.e. is phase 2 or two 3 in progress ? */
int garbage_collection_in_progress = 0;
  /* The current state of the garbage collection.
   *   0 means 'no collection' is active, i.e. all refcounts are valid
   *   2 is the 'clear refcounts' phase
   *   3 is the 'recompute refcounts' phase
   * swap uses this information when swapping in objects.
   */

struct object *gc_obj_list_destructed;
  /* List of referenced but destructed objects.
   * mapping.c needs this (TODO: Why?)
   */

struct lambda *stale_misc_closures;
  /* List of non-lambda closures bound to a destructed object.
   * The now irrelevant .ob pointer is used to link the list elements.
   * mapping.c needs this (TODO: Why?)
   */

static struct lambda *stale_lambda_closures;
  /* List of lambda closures bound to a destructed object.
   * The now irrelevant .ob pointer is used to link the list elements.
   */

#endif /* MALLOC_smalloc */

/*-------------------------------------------------------------------------*/

#if defined(MALLOC_smalloc)

#define STRING_REFS(str)  (*(unsigned short *)((char *) (str)\
                           - sizeof(unsigned short)))
  /* Return the refcount of shared string <str>
   */

#define MARK_STRING_REF(str) ((void)(\
    STRING_REFS(str)++ || (\
        CHECK_REF( (str)-sizeof(short)-sizeof(char *) ) ||\
            /* reached max ref count, which is given as 0... */ \
            STRING_REFS(str)--\
    ))\
)
  /* Increment the refcount of shared string <str>
   */

#ifdef SMALLOC_TRACE
#    define WRITES(d, s) write((d), (s), strlen(s))
#    define WRITE_SMALLOC_TRACE(p)  (WRITES(gout, ((char **)(p))[-3]), \
            WRITES(gout, " "), \
            ((p_uint (*)(int, int))writed)(gout, ((p_uint *)(p))[-2]), \
            WRITES(gout, "\n") ),
#else
#    define WRITE_SMALLOC_TRACE(p)
#endif
  /* Dump the allocation information for <p>, if any.
   */

#define CLEAR_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] &= ~M_REF )
  /* Clear the memory block marker for <p>
   */

#define MARK_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] |= M_REF )
  /* Set the memory block marker for <p>
   */

#define TEST_REF(p) ( !( ((p_uint *)(p))[-SMALLOC_OVERHEAD] & M_REF ) )
  /* Check the memory block marker for <p>, return TRUE if _not_ set.
   */

#define CHECK_REF(p) ( TEST_REF(p) && ( MARK_REF(p),MY_TRUE ) )
  /* Check the memory block marker for <p> and set it if necessary.
   * Return TRUE if the marker was not set, FALSE else.
   */

#ifdef __GNUC__
/* typecast would strip NORETURN */
#    define ifatal(s) (fatal(s),0)
#else
#    define ifatal(s) (((p_uint (*)(char *))fatal)(s))
#endif
  /* A fatal() which can be used in an expression.
   */

#define NOTE_REF(p) \
    ( \
        TEST_REF(p) ? \
            MARK_REF(p) \
        : time_to_swap_variables + 1 == 0 && \
          ( WRITE_SMALLOC_TRACE(p) \
            ifatal("memory block referenced twice\n") ) \
    )
  /* Reference a memory block <p>, but fatal() if it is the
   * second reference.
   * TODO: What does 'time_to_swap_variables' do in here?
   */


/* Forward declarations */

static void clear_map_ref_filter (struct svalue *, struct svalue *, char *);
static void clear_ref_in_closure (struct lambda *l, ph_int type);
static void count_ref_in_closure (struct svalue *csvp);

#endif /* MALLOC_smalloc */


/*=========================================================================*/

/*            The real collector - only with smalloc.
 */

#if defined(MALLOC_smalloc)

/*-------------------------------------------------------------------------*/
void
clear_memory_reference (char *p)

/* Clear the memory block marker for block <p>.
 */

{
    CLEAR_REF(p);
}

/*-------------------------------------------------------------------------*/
void
note_malloced_block_ref (char *p)

/* Note the reference to memory block <p>.
 */

{
    NOTE_REF(p);
}

/*-------------------------------------------------------------------------*/
void
clear_inherit_ref (struct program *p)

/* Clear the refcounts of all inherited programs of <p>.
 */

{
    int i;

    for (i = 0; i < p->num_inherited; i++)
    {
        /* Inherited programs are never swapped. Only programs with blueprints
         * are swapped, and a blueprint and one inheritance makes two refs.
         */
        struct program *p2;

        p2 = p->inherit[i].prog;
        if (p2->ref)
        {
            p2->ref = 0;
            clear_inherit_ref(p2);
        }
    }
}

/*-------------------------------------------------------------------------*/
void
mark_program_ref (struct program *p)

/* Set the marker of program <p> and of all data referenced by <p>.
 */

{
    if (CHECK_REF(p))  /* ...then mark referenced data */
    {
        int i;

        unsigned char *program = p->program;
        uint32 *functions = p->functions;
        char **strings;
        struct variable *variable_names;

        if (p->ref++)
            fatal("First reference to program, but ref count != 0\n");

        if (p->swap_num != -1 && p->line_numbers)
            NOTE_REF(p->line_numbers);

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

        NOTE_REF(p->name);
    }
    else
    {
        if (!p->ref++)
            fatal("Program block referenced as something else\n");
    }
}

/*-------------------------------------------------------------------------*/
void
reference_destructed_object (struct object *ob)

/* Note the reference to a destructed object <ob>. The referee has to
 * replace its reference by a svalue.number 0 since all these objects
 * will be freed later.
 */

{
    if (TEST_REF(ob))
    {
        if (ob->ref)
            fatal("First reference to destructed object, but ref count != 0\n");

        /* Destructed objects are not swapped */
        ob->next_all = gc_obj_list_destructed;
        gc_obj_list_destructed = ob;
        MARK_REF(ob);
        mark_program_ref(ob->prog);
        NOTE_REF(ob->name);
        ob->ref++;
    }
    else
    {
        if (!ob->ref)
        {
            WRITE_SMALLOC_TRACE(ob)
            ifatal("Destructed object referenced as something else\n");
        }
    }
}

/*-------------------------------------------------------------------------*/
void
count_ref_from_string (char *p)

/* Count the reference to shared string <p>.
 */

{
   MARK_STRING_REF(p);
}

/*-------------------------------------------------------------------------*/
static void
clear_map_ref_filter (struct svalue *key, struct svalue *data, char *extra)

/* Auxiliary function to clear the refs in a mapping.
 * It is called with the <key> and <data> vector, the latter of
 * width (p_int)<extra>
 */
{
    clear_ref_in_vector(key, 1);
    clear_ref_in_vector(data, (p_int)extra);
}

/*-------------------------------------------------------------------------*/
void
clear_ref_in_vector (struct svalue *svp, int num)

/* Clear the refs of the <num> elements of vector <svp>.
 */

{
    struct svalue *p;

    for (p = svp; p < svp+num; p++)
    {
        switch(p->type)
        {
        case T_OBJECT:
            /* this might be a destructed object, which has it's ref not
             * cleared by the obj_list because it is no longer a member
             * Alas, swapped objects must not have prog->ref cleared.
             */
            if (p->u.ob->flags & O_DESTRUCTED && p->u.ob->ref)
            {
                p->u.ob->ref = 0;
                p->u.ob->prog->ref = 0;
                clear_inherit_ref(p->u.ob->prog);
            }
            continue;

        case T_POINTER:
        case T_QUOTED_ARRAY:
            if (!p->u.vec->ref)
                continue;
            p->u.vec->ref = 0;
            clear_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            continue;

#ifdef MAPPINGS
        case T_MAPPING:
            if (p->u.map->ref)
            {
                struct mapping *m;
                p_int num_values;

                m = p->u.map;
                m->ref = 0;
                num_values = m->num_values;
                walk_mapping(m, clear_map_ref_filter, (char *)num_values );
            }
            continue;
#endif /* MAPPINGS */

        case T_CLOSURE:
            if (CLOSURE_MALLOCED(p->x.closure_type))
            {
                struct lambda *l;

                l = p->u.lambda;
                if (l->ref)
                {
                    l->ref = 0;
                    clear_ref_in_closure(l, p->x.closure_type);
                }
            }
            else if (p->u.ob->flags & O_DESTRUCTED && p->u.ob->ref)
            {
                p->u.ob->ref = 0;
                p->u.ob->prog->ref = 0;
                clear_inherit_ref(p->u.ob->prog);
            }
            continue;
        }
    }
} /* clear_ref_in_vector() */

/*-------------------------------------------------------------------------*/
void
count_ref_in_vector (struct svalue *svp, int num)

/* Count the references the <num> elements of vector <p>.
 */

{
    struct svalue *p;

    for (p = svp; p < svp+num; p++) {
        switch(p->type)
        {
        case T_OBJECT:
          {
            struct object *ob;

            ob = p->u.ob;
            if (ob->flags & O_DESTRUCTED)
            {
                p->type = T_NUMBER;
                p->u.number = 0;
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
                count_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            }
            p->u.vec->ref++;
            continue;

#ifdef MAPPINGS
        case T_MAPPING:
            if (CHECK_REF(p->u.map))
            {
                struct mapping *m;
                struct condensed_mapping *cm;
                int num_values;

                m = p->u.map;
                cm = m->condensed;
                num_values = m->num_values;
                NOTE_REF((char *)CM_MISC(cm) - cm->misc_size *(num_values + 1));
                /* hash mappings have been eleminated at the start */
                count_ref_in_mapping(m);
            }
            p->u.map->ref++;
            continue;
#endif /* MAPPINGS */

          case T_STRING:
              switch(p->x.string_type)
              {
              case STRING_MALLOC:
                  NOTE_REF(p->u.string);
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
                  struct object *ob;

                  ob = p->u.ob;
                  if (ob->flags & O_DESTRUCTED)
                  {
                      p->x.closure_type = F_UNDEF-F_OFFSET+CLOSURE_EFUN;
                      p->u.ob = master_ob;
                      master_ob->ref++;
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
}

/*-------------------------------------------------------------------------*/
static void
remove_unreferenced_string (char *start, char *string)

/* If the shared string <string> stored in the memory block <start> is
 * not referenced, it is deallocated.
 */

{
    if (TEST_REF(start))
    {
        dprintf1(gout,
"'%s' was left unreferenced in the shared string table, freeing now.\n",
          (p_int)string
        );

        MARK_REF(start);
        STRING_REFS(string)++;
        free_string(string);
    }
}

/*-------------------------------------------------------------------------*/
static void
note_sentence_ref (struct sentence *p)

/* Mark the strings of function and verb of all sentences in list <p>.
 */

{
    do {
        if (p->function)
            MARK_STRING_REF(p->function);
        if (p->verb)
            MARK_STRING_REF(p->verb);
        NOTE_REF(p);
    } while ( NULL != (p = p->next) );
}

/*-------------------------------------------------------------------------*/
static void
count_ref_in_closure (struct svalue *csvp)

/* Count the reference to closure <csvp> and all referenced data.
 * Closures using a destructed object are stored in the stale_ lists
 * for later removal (and .ref is set to -1).
 */

{
    struct lambda *l = csvp->u.lambda;
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
            csvp->x.closure_type = F_UNDEF-F_OFFSET+CLOSURE_EFUN;
            csvp->u.ob = master_ob;
            master_ob->ref++;
        }
        return;
    }

    /* If the closure is bound, make sure that the object it is
     * bound to really exists.
     */

    if (type != CLOSURE_UNBOUND_LAMBDA)
    {
        struct object *ob;

        ob = l->ob;
        if (ob->flags & O_DESTRUCTED
         || (   type == CLOSURE_ALIEN_LFUN
             && l->function.alien.ob->flags & O_DESTRUCTED) )
        {
            l->ref = -1;
            if (type == CLOSURE_LAMBDA)
            {
                l->ob = (struct object *)stale_lambda_closures;
                stale_lambda_closures = l;
            }
            else
            {
                l->ob = (struct object *)stale_misc_closures;
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
                csvp->x.closure_type = F_UNDEF-F_OFFSET+CLOSURE_EFUN;
                csvp->u.ob = master_ob;
                master_ob->ref++;
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
        struct svalue *svp;

        svp = (struct svalue *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0xff].u.number;
        svp -= num_values;
        NOTE_REF(svp);
        count_ref_in_vector(svp, num_values);
    }
    else
    {
        NOTE_REF(l);
        if (type == CLOSURE_BOUND_LAMBDA)
        {
            struct lambda *l2 = l->function.lambda;

            if (!l2->ref++) {
                struct svalue sv;

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
clear_ref_in_closure (struct lambda *l, ph_int type)

/* Clear the references in closure <l> which is of type <type>.
 */

{
    if (CLOSURE_HAS_CODE(type))
    {
        mp_int num_values;
        struct svalue *svp;

        svp = (struct svalue *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0xff].u.number;
        svp -= num_values;
        clear_ref_in_vector(svp, num_values);
    }
    else if (type == CLOSURE_BOUND_LAMBDA)
    {
        struct lambda *l2 = l->function.lambda;

        if (l2->ref) {
            l2->ref = 0;
            clear_ref_in_closure(l2, CLOSURE_UNBOUND_LAMBDA);
        }
    }

    if (type != CLOSURE_UNBOUND_LAMBDA && l->ob->flags & O_DESTRUCTED
     && l->ob->ref /* block against bad efficency due to multiple refs */ )
    {
        l->ob->ref = 0;
        l->ob->prog->ref = 0;
        clear_inherit_ref(l->ob->prog);
    }

    if (type == CLOSURE_ALIEN_LFUN
     && l->function.alien.ob->flags & O_DESTRUCTED
     && l->function.alien.ob->ref)
    {
        l->function.alien.ob->ref = 0;
        l->function.alien.ob->prog->ref = 0;
        clear_inherit_ref(l->function.alien.ob->prog);
    }
}

/*-------------------------------------------------------------------------*/
static void
remove_uids (int smart UNUSED)

/* ??? */

{
    NOOP
}

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
    struct object *ob, *next_ob;
    struct lambda *l, *next_l;
    int i;

    /* --- Pass 0: dispose of some unnecessary stuff ---
     */

    malloc_privilege = MALLOC_MASTER;
    CLEAR_EVAL_COST;
    out_of_memory = MY_FALSE;
    assert_master_ob_loaded();
    malloc_privilege = MALLOC_SYSTEM;
    remove_destructed_objects();
    free_all_sent();
    compact_mappings(num_dirty_mappings);
    free_interpreter_temporaries();
    remove_stale_call_outs();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
    free_notifys();

    /* --- Pass 1: clear the M_REF flag in all malloced blocks ---
     */
    clear_M_REF_flags();

    /* --- Pass 2: clear the ref counts ---
     */

    garbage_collection_in_progress = 2;
    if (d_flag > 3)
    {
        debug_message("start of garbage_collection\n");
    }

    /* Process the list of all objects */

    for (ob = obj_list; ob; ob = ob->next_all) {
        int was_swapped;

        if (d_flag > 4)
        {
            debug_message("clearing refs for object '%s'\n",ob->name);
        }
        was_swapped = 0;
        if (ob->flags & O_SWAPPED
         && (was_swapped = load_ob_from_swap(ob)) & 1)
        {
            /* don't clear the program ref count. It is 1 */
        }
        else
        {
            /* Take special care of inherited programs, the associated
             * objects might me destructed.
             */
            ob->prog->ref = 0;
        }
        clear_inherit_ref(ob->prog);
        ob->ref = 0;
        clear_ref_in_vector(ob->variables, ob->prog->num_variables);
        if (ob->flags & O_SHADOW)
        {
            struct ed_buffer *buf;

            if ( NULL != (buf = O_GET_SHADOW(ob)->ed_buffer) )
            {
                clear_ed_buffer_refs(buf);
            } /* end of ed-buffer processing */
        }
        if (was_swapped)
            swap(ob, was_swapped);
    }
    if (d_flag > 3)
    {
        debug_message("ref counts referenced by obj_list cleared\n");
    }

    /* Process the interactives */

    for(i = 0 ; i < MAX_PLAYERS; i++)
    {
        struct input_to * it;

        if (all_players[i] == NULL)
            continue;

        if ( NULL != (it = all_players[i]->input_to) )
        {
            clear_ref_in_vector(it->arg, it->num_arg);
            if (it->ob->flags & O_DESTRUCTED && it->ob->ref)
            {
                it->ob->ref = 0;
                it->ob->prog->ref = 0;
                clear_inherit_ref(it->ob->prog);
            }
        }
        clear_ref_in_vector(&all_players[i]->default_err_message, 1);
        clear_ref_in_vector(&all_players[i]->prompt, 1);

        if ( NULL != (ob = all_players[i]->snoop_by) )
        {
            if (!O_GET_INTERACTIVE(ob) ||
                O_GET_INTERACTIVE(ob)->sent.type != SENT_INTERACTIVE)
            {
                /* snooping monster */
                if (ob->flags & O_DESTRUCTED && ob->ref) {
                    ob->ref = 0;
                    ob->prog->ref = 0;
                    clear_inherit_ref(ob->prog);
                }
            }
        } /* end of snoop-processing */

        if ( NULL != (ob = all_players[i]->modify_command) ) {
            if (ob->flags & O_DESTRUCTED && ob->ref) {
                ob->ref = 0;
                ob->prog->ref = 0;
                clear_inherit_ref(ob->prog);
            }
        }
    }

    /* Process the driver hooks */

    for (i = NUM_CLOSURE_HOOKS; --i >= 0; ) {
        if (closure_hook[i].type == T_CLOSURE
        &&  closure_hook[i].x.closure_type == CLOSURE_LAMBDA)
        {
            closure_hook[i].x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        }
    }
    clear_ref_in_vector(closure_hook, NUM_CLOSURE_HOOKS);

    /* Let the modules process their data */

    clear_shared_string_refs();
    clear_ref_from_wiz_list();
    clear_ref_from_call_outs();
    clear_simul_efun_refs();
    clear_interpreter_refs();
    clear_comm_refs();
#ifdef RXCACHE_TABLE
    clear_rxcache_refs();
#endif

    null_vector.ref = 0;

    /* --- Pass 3: Compute the ref counts, and set M_REF where appropriate ---
     */

    garbage_collection_in_progress = 3;

    gc_obj_list_destructed = NULL;
    stale_lambda_closures = NULL;
    stale_misc_closures = NULL;
    stale_mappings = NULL;

    /* Process the list of all objects.
     */
    for (ob=obj_list; ob; ob = ob->next_all)
    {
        int was_swapped;

        was_swapped = 0;
        if (ob->flags & O_SWAPPED)
        {
            was_swapped = load_ob_from_swap(ob);
            if (was_swapped & 1)
            {
                CLEAR_REF(ob->prog);
                ob->prog->ref = 0;
            }
        }
        ob->ref++;
        NOTE_REF(ob);

        if (ob->prog->num_variables)
            NOTE_REF(ob->variables);

        mark_program_ref(ob->prog);

        count_ref_in_vector(ob->variables, ob->prog->num_variables);

        if (ob->sent)
        {
            struct sentence *sent;
            struct ed_buffer *buf;

            sent = ob->sent;
            if (ob->flags & O_SHADOW)
            {
                NOTE_REF(sent);
                if ( NULL != (buf = ((struct shadow_sentence *)sent)->ed_buffer) )
                {
                    NOTE_REF(buf);
                    count_ed_buffer_refs(buf);
                } /* end of ed-buffer processing */
                sent = sent->next;
            }
            if (sent) note_sentence_ref(sent);
        }

        NOTE_REF(ob->name);

        if (was_swapped)
        {
            swap(ob, was_swapped);
        }
    }

    if (d_flag > 3)
    {
        debug_message("obj_list evaluated\n");
    }

    /* Process the interactives. */

    for(i = 0 ; i < MAX_PLAYERS; i++)
    {
        struct input_to * it;

        if (all_players[i] == NULL)
            continue;

        NOTE_REF(all_players[i]);

        /* There are no destructed interactives */

        all_players[i]->ob->ref++;
        if ( NULL != (ob = all_players[i]->snoop_by) )
        {
            if (!O_GET_INTERACTIVE(ob) ||
                O_GET_INTERACTIVE(ob)->sent.type != SENT_INTERACTIVE)
            {
                /* snooping monster */
                if (ob->flags & O_DESTRUCTED) {
                    all_players[i]->snoop_by = 0;
                    reference_destructed_object(ob);
                } else {
                    ob->ref++;
                }
            }
        } /* end of snoop-processing */

        if ( NULL != (it = all_players[i]->input_to) )
        {
            /* To avoid calling too high-level functions, we want the
             * struct input_to not to be freed by now.
             * Thus, we reference the object even if it is destructed.
             */
            NOTE_REF(it);
            ob = it->ob;
            if (!ob->ref)
            {
                /* destructed */
                NOTE_REF(ob);
                mark_program_ref(ob->prog);
                NOTE_REF(ob->name);
            }
            ob->ref++;
            MARK_STRING_REF(it->function);
            count_ref_in_vector(it->arg, it->num_arg);
        } /* end of input_to processing */

        if ( NULL != (ob = all_players[i]->modify_command) )
        {
            if (ob->flags & O_DESTRUCTED)
            {
                all_players[i]->modify_command = 0;
                reference_destructed_object(ob);
            }
            else
            {
                ob->ref++;
            }
        }

        count_ref_in_vector(&all_players[i]->default_err_message, 1);
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
    note_shared_string_table_ref();
    note_otable_ref();
    count_comm_refs();
    count_interpreter_refs();
    count_heart_beat_refs();
#ifdef RXCACHE_TABLE
    count_rxcache_refs();
#endif
#ifdef FILE_STAT
    count_fstat_refs();
#endif

    if (reserved_user_area)
        NOTE_REF(reserved_user_area);
    if (reserved_master_area)
        NOTE_REF(reserved_master_area);
    if (reserved_system_area)
        NOTE_REF(reserved_system_area);

    NOTE_REF(mud_lib);
    null_vector.ref++;

    /* Process the driver hooks */

    count_ref_in_vector(closure_hook, NUM_CLOSURE_HOOKS);
    for (i = NUM_CLOSURE_HOOKS; --i >= 0; )
    {
        if (closure_hook[i].type == T_CLOSURE &&
            closure_hook[i].x.closure_type == CLOSURE_UNBOUND_LAMBDA)
        {
            closure_hook[i].x.closure_type = CLOSURE_LAMBDA;
        }
    }

    garbage_collection_in_progress = 0;

    /* --- Pass 4: remove stralloced strings with M_REF cleared ---
     */

    walk_shared_strings(remove_unreferenced_string);

    /* --- Pass 5: Release all destructed objects ---
     *
     * It is vital that all information freed here is already known
     * as referenced, so we won't free it a second time in pass 6.
     */

    for (ob = gc_obj_list_destructed; ob; ob = next_ob)
    {
        next_ob = ob->next_all;
        free_object(ob, "garbage collection");
    }

    for (l = stale_lambda_closures; l; )
    {
        struct svalue sv;

        next_l = (struct lambda *)l->ob;
        l->ref = 1;
        sv.type = T_CLOSURE;
        sv.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        sv.u.lambda = l;
        l = (struct lambda *)l->ob;
        free_closure(&sv);
    }

    for (l = stale_misc_closures; l; l = next_l)
    {
        next_l = (struct lambda *)l->ob;
        xfree((char *)l);
    }

    clean_stale_mappings();

    /* --- Pass 6: Release all unused memory ---
     */

    free_unreferenced_memory();
    reallocate_reserved_areas();
    if (!reserved_user_area)
    {
        struct svalue *res = NULL;
        if (reserved_system_area)
        {
            CLEAR_EVAL_COST;
            malloc_privilege = MALLOC_MASTER;
            res = apply_master_ob(STR_QUOTA_DEMON, 0);
        }
        remove_uids(res && (res->type != T_NUMBER || res->u.number) );
    }

    /* Finally, try to reclaim the reserved areas */

    reallocate_reserved_areas();
    time_last_gc = time(NULL);
}


#if defined(SMALLOC_TRACE)

/* Some functions to print the tracing data from the memory blocks.
 * The show_ functions are called from smalloc directly.
 */

/*-------------------------------------------------------------------------*/
static void
show_string (int d, char *block, int depth)

/* Print the string from memory <block> on filedescriptor <d>.
 */

{
    mp_int len;

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

/*-------------------------------------------------------------------------*/
static void
show_added_string (int d, char *block, int depth)

/* Print the string from memory <block> on filedescriptor <d>, prefixed
 * by 'Added string: '.
 */

{
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
    struct object *ob;

    ob = (struct object *)block;
    if (depth) {
        struct object *o;

        for (o = obj_list; o && o != ob; o = o->next_all);
        if (!o || o->flags & O_DESTRUCTED) {
            WRITES(d, "Destructed object in block 0x");
            writex(d, (unsigned)((unsigned *)block - SMALLOC_OVERHEAD));
            WRITES(d, "\n");
            return;
        }
    }
    WRITES(d, "Object: ");
    show_string(d, ob->name, 0);
    WRITES(d, ", uid: ");
    show_string(d, ob->user->name ? ob->user->name : "0", 0);
    WRITES(d, "\n");
}

/*-------------------------------------------------------------------------*/
static void
show_array(int d, char *block, int depth)

/* Print the array at recursion <depth> from memory <block> on
 * filedescriptor <d>. Recursive printing stops at <depth> == 2.
 */

{
    struct vector *a;
    mp_int i, j;
    struct svalue *svp;
    struct wiz_list *user;
    mp_int a_size;

    a = (struct vector *)block;
    a_size = VEC_SIZE(a);
    if (depth && a != &null_vector)
    {
        int freed;
        struct wiz_list *wl;

        freed = is_freed(block, sizeof(struct vector) );
        if (!freed)
        {
            user = a->user;
            wl = all_wiz;
            if (user)
                for ( ; wl && wl != user; wl = wl->next);
        }
        if (freed || !wl || a_size <= 0 || a_size > MAX_ARRAY_SIZE
         || (malloced_size((char *)a) - SMALLOC_OVERHEAD) << 2 !=
              sizeof(struct vector) + sizeof(struct svalue) * (a_size - 1) )
        {
            WRITES(d, "Array in freed block 0x");
            writex(d, (unsigned)((unsigned *)block - SMALLOC_OVERHEAD));
            WRITES(d, "\n");
            return;
        }
    }
    else
    {
        user = a->user;
    }

    WRITES(d, "Array size ");writed(d, a_size);
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
            writed(d, svp->u.number);
            WRITES(d, "\n");
            break;

        case T_STRING:
            if (svp->x.string_type == STRING_MALLOC &&
                  is_freed(svp->u.string, 1) )
            {
                WRITES(d, "Malloced string in freed block 0x");
                writex(d, (unsigned)((unsigned *)block - SMALLOC_OVERHEAD));
                WRITES(d, "\n");
                break;
            }
            if (svp->x.string_type == STRING_SHARED &&
                is_freed(SHSTR_BLOCK(svp->u.string),
                         sizeof(char *) + sizeof(short) + 1) )
            {

                WRITES(d, "Shared string in freed block 0x");
                writex(d, (unsigned)(
                  (unsigned *)(block-sizeof(char *)-sizeof(short))
                  - SMALLOC_OVERHEAD
                ));
                WRITES(d, "\n");
                break;
            }
            WRITES(d, "String: ");
            show_string(d, svp->u.string, 0);
            WRITES(d, "\n");
            break;

        case T_OBJECT:
            show_object(d, (char *)svp->u.ob, 1);
            break;

        default:
            WRITES(d, "Svalue type ");writed(d, svp->type);WRITES(d, "\n");
            break;
        }
    }
}

/*-------------------------------------------------------------------------*/
void
setup_print_block_dispatcher (void)

/* Setup the tracing data dispatcher in smalloc with the show_ functions
 * above.
 *
 * This is here because I like to avoid smalloc calling closures, and
 * gcollect.c is already notorious for including almost every header file
 * anyway.
 */

{
    struct svalue tmp_closure;
    struct vector *a, *b;

    assert_master_ob_loaded();
    tmp_closure.type = T_CLOSURE;
    tmp_closure.x.closure_type = CLOSURE_EFUN + F_ADD - F_OFFSET;
    tmp_closure.u.ob = master_ob;
    push_constant_string("");
    push_constant_string("");
    call_lambda(&tmp_closure, 2);
    store_print_block_dispatch_info(inter_sp->u.string, show_added_string);
    free_svalue(inter_sp--);
    a = allocate_array(1);
    store_print_block_dispatch_info((char *)a, show_array);
    b = slice_array(a, 0, 0);
    store_print_block_dispatch_info((char *)b, show_array);
    free_vector(a);
    free_vector(b);
    store_print_block_dispatch_info((char *)master_ob, show_object);
}
#endif /* SMALLOC_TRACE */

#endif /* MALLOC_smalloc */

/*=========================================================================*/

/*       Default functions when not using the smalloc allocator
 */

#if !defined(MALLOC_smalloc)

void
garbage_collection (void)

/* Free as much memory as possible and try to reallocate the
 * reserved areas - that's all we can do.
 */

{
    assert_master_ob_loaded();
    remove_destructed_objects();
    free_all_sent();
    compact_mappings(num_dirty_mappings);
    free_interpreter_temporaries();
    remove_stale_call_outs();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
    reallocate_reserved_areas();
    time_last_gc = time(NULL);
}
#endif /* MALLOC_smalloc */


#if !defined(SMALLOC_TRACE) || !defined(MALLOC_smalloc)

void setup_print_block_dispatcher (void) { NOOP }

#endif

/***************************************************************************/

