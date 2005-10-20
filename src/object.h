#ifndef OBJECT_H__
#define OBJECT_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "sent.h"    /* O_GET_* */
#include "instrs.h"  /* F_SET_LIGHT */

#ifdef DEBUG
#include <stdio.h>      /* printf() for refcount tracing */
#endif

/* --- Types --- */

/* --- struct object: the base structure of every object
 */

struct object_s
{
    unsigned short flags; /* Bits or'ed together, see below */
    p_int ref;            /* Reference count. */
#ifdef F_SET_LIGHT
    short total_light;    /* Total light */
#endif
    mp_int time_reset;    /* Time of next reset, or 0 if none */
    mp_int time_of_ref;   /* Time when last referenced. Used by swap */
    mp_int load_time;     /* Time when the object was created. */
    p_int  load_id;       /* Load-ID within the time the object was created */
#ifdef DEBUG
    p_int extra_ref;      /* Used to check ref count. */
#endif
    program_t *prog;      /* Program code for this object */
    char *name;
      /* name of the object (allocated), always w/o leading '/' */
    char *load_name;
      /* name of the object's blueprint (shared string), in compat
       * mode without leading '/'
       */
    object_t *next_all;   /* Next object in global list */
    object_t *prev_all;   /* Previous object in global list */
    object_t *next_hash;  /* Next object in chain in the otable */
    object_t *next_inv;   /* Next object in the current environment */
    object_t *contains;   /* First contained object */
    object_t *super;      /* Current environment */
    sentence_t *sent;     /* Sentences, shadows, interactive data */
    wiz_list_t *user;     /* What wizard defined this object */
    wiz_list_t *eff_user; /* Effective user */
#ifdef DEBUG
    int extra_num_variables;
    /* amylaar : used to determine where to check ref counts at all... */
#endif
    svalue_t *variables;
      /* All variables to this object: an array of svalues, allocated
       * in a separate block.
       */
    unsigned long ticks, gigaticks;
      /* Evalcost used by this object. The total cost
       * is computed with gigaticks*1E9+ticks.
       */
};


/* Values of object_t.flags: */

#define O_HEART_BEAT         0x01   /* Does it have an heart beat? */
#ifdef F_SET_IS_WIZARD
#define O_IS_WIZARD          0x02   /* Is it a wizard player.c? TODO: Remove me */
#endif
#define O_ENABLE_COMMANDS    0x04   /* Can it execute commands? */
#define O_CLONE              0x08   /* Is it cloned from a master copy? */
#define O_DESTRUCTED         0x10   /* Is it destructed ? */
#define O_SWAPPED            0x20   /* Is it swapped to file */
#define O_ONCE_INTERACTIVE   0x40   /* Has it ever been interactive? */
#define O_UNUSED_80          0x80
#define O_RESET_STATE        0x100  /* Object in a 'reset':ed state ? */
#define O_WILL_CLEAN_UP      0x200  /* clean_up will be called next time */
#define O_LAMBDA_REFERENCED  0x400  /* be careful with replace_program() */
#define O_SHADOW             0x800  /* Is the object shadowed? */
#define O_REPLACED           0x1000 /* Was the program replaced? */


/* If an object's program or variables are swapped out, the values
 * of .prog resp. .variables are replaced with the associated (even)
 * swap number assigned by the swapper, and the lowest bit of the number
 * is set. The swap number '-1' means 'not swapped'.
 * TODO: This assumes that pointers are always even.
 */

#define P_PROG_SWAPPED(p) ((p_int)(p) & 1)
  /* Is the program <p> swapped out?
   */

#define O_PROG_SWAPPED(ob) ((p_int)(ob)->prog & 1)
  /* Is the program of <ob> swapped out?
   */

#define O_VAR_SWAPPED(ob) ((p_int)(ob)->variables & 1)
  /* Are the variables of <ob> swapped out?
   */

#define O_SWAP_NUM(ob) \
        (O_PROG_SWAPPED(ob) ? (p_int)(ob)->prog  & ~1 : (ob)->prog->swap_num)
  /* The swap number for the program of <ob>.
   */

#define O_IS_INTERACTIVE(o) \
  (((o)->flags & O_SHADOW) && (NULL != O_GET_INTERACTIVE(o)) )

  /* Bool O_IS_INTERACTIVE(object_t *o)
   *   Return TRUE if ob is an interactive object.
   */

#define O_SET_INTERACTIVE(ip,o) \
  (   ( ((o)->flags & O_SHADOW) && (NULL != (ip = O_GET_INTERACTIVE(o))) ) \
   || ( (ip = NULL), MY_FALSE ) )

  /* Bool O_SET_INTERACTIVE(interactive_t *ip, object_t *o)
   *   Return TRUE if ob is an interactive object and set ip to the interactive
   *   structure. Return FALSE is not and clear ip.
   */


/* --- struct replace_ob_s: one scheduled program replacement
 *
 * A list of this structure (obj_list_replace) keeps track of all
 * requested replace_program()s during one backend round.
 *
 * It is possible, though not very useful, to replace an object's
 * program by the very same program.
 */

struct replace_ob_s
{
    object_t *ob;         /* Object requesting the new program */
    program_t *new_prog;  /* Requested new program */
    int var_offset;       /* Variable offset of .new_prog */
    int fun_offset;       /* Function offset of .new_prog */
    replace_ob_t *next;   /* Link pointer for list */
    struct lambda_replace_program_protector *lambda_rpp;
      /* Additional information about lambdas bound to the program
       * after the replacement was scheduled. The exact information
       * is private to closure.c.
       */
};


/* --- Macros --- */

/* object_t *ref_object(object_t *o, char *from)
 *   Add another ref to object <o> from function <from>
 *   and return the object <o>.
 */

#ifndef DEBUG

#    define ref_object(o,from) ((o)->ref++, (o))

#else

#    define ref_object(o,from) (\
     (o)->ref++,\
     d_flag > 1 ? printf("Add ref to object %s: %ld (%s)\n" \
                        , (o)->name, (o)->ref, from) : 0, \
     (o))

#endif

/* void free_object(object_t *o, char *)
 *   Subtract one ref from object <o> from function <o>, and free the
 *   object fully if the refcount reaches zero.
 */

#ifndef DEBUG

#  define free_object(o,from) MACRO( if (--((o)->ref) <= 0) _free_object(o); )

#else

#  define free_object(o,from) MACRO(\
      (o)->ref--;\
      if (d_flag > 1) printf("Sub ref from object %s: %ld (%s)\n"\
                            , (o)->name, (o)->ref, from);\
      if ((o)->ref <= 0) _free_object(o); \
    )

#endif

/* void deref_object(object_t *o, char *from)
 *   Subtract one ref from object <o> from function <from>, but don't
 *   check if it needs to be freed.
 */

#ifndef DEBUG

#    define deref_object(o, from) (--(o)->ref)

#else

#    define deref_object(o,from) (--(o)->ref, \
       d_flag > 1 ? printf("Sub ref from object %s: %ld (%s)\n" \
                          , (o)->name, (o)->ref, from) : 0)

#endif


#define check_object(o) ((o)&&(o)->flags&O_DESTRUCTED ? NULL :(o))

  /* Return NULL, if object <o> is NULL or destructed,
   * return <o> else.
   */


/* --- Variables --- */

extern replace_ob_t *obj_list_replace;
extern int tot_alloc_object;
extern int tot_alloc_object_size;
extern object_t NULL_object;


/* --- Prototypes --- */

extern int32 renumber_programs(void);
extern void remove_destructed_objects(void);
extern void tell_object(object_t *, char *);
extern void tell_npc(object_t *, char *);
extern void reference_prog(program_t *, char *);
#ifdef DEALLOCATE_MEMORY_AT_SHUTDOWN
extern void remove_all_objects(void);
#endif
extern void do_free_sub_strings(int num_strings, char ** strings, int num_variables, variable_t *variable_names);
extern void free_prog(program_t *progp, Bool free_sub_strings);
extern void reset_object(object_t *ob, int arg);
extern void replace_programs(void);
extern Bool shadow_catch_message(object_t *ob, char *str);

extern void _free_object(object_t *);
#ifdef INITIALIZATION_BY___INIT
extern object_t *get_empty_object(int num_var);
#else
extern object_t *get_empty_object(int num_var, variable_t * variables, svalue_t *initialisers);
#endif

extern svalue_t *f_save_object(svalue_t *sp, int numarg);
extern svalue_t *f_save_value(svalue_t *sp);
extern svalue_t *f_restore_object(svalue_t *sp);
extern svalue_t *f_restore_value(svalue_t *sp);

#endif /* OBJECT_H__ */
