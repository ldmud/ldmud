#ifndef __OBJECT_H__
#define __OBJECT_H__ 1

#include "driver.h"

#include "exec.h"       /* struct program */
#include "interpret.h"  /* struct svalue, struct variable */
#include "sent.h"       /* struct sentence */

/* TODO: See also sent.h and comm.h */

/* --- Types --- */

struct pointer_record;
  /* Opaque type for deep data structure walks
   */

#define PTABLE_SIZE 256
  /* The pointertable passed to init_pointer_table() _must_
   * conform the declaration 'struct pointer_record *table[PTABLE_SIZE]'.
   * This value must be a power of 2.
   */

/* --- struct object: the base structure of every object
 */

struct object
{
    unsigned short flags;      /* Bits or'ed together, see below */
    short total_light;         /* Total light */
    int next_reset;            /* Time of next reset of this object */
    int time_of_ref;           /* Time when last referenced. Used by swap */
    p_int ref;                 /* Reference count. */
#ifdef DEBUG
    p_int extra_ref;           /* Used to check ref count. */
#endif
    struct program *prog;      /* Program code for this object */
    char *name;                /* name of the object (allocated) */
    char *load_name;           /* name of the object's blueprint (shared string) */
    struct object *next_all;   /* Next object in global list */
    struct object *next_hash;  /* Next object in chain in the otable */
    struct object *next_inv;   /* Next object in the current environment */
    struct object *contains;   /* First contained object */
    struct object *super;      /* Current environment */
    struct sentence *sent;     /* Sentences, shadows, interactive data */
    struct wiz_list *user;     /* What wizard defined this object */
#ifdef EUIDS
    struct wiz_list *eff_user; /* Used for permissions */
#endif
#ifdef DEBUG
    int extra_num_variables;
    /* amylaar : used to determine where to check ref counts at all... */
#endif
    struct svalue *variables;  /* All variables to this object: an array of svalues. */
    unsigned long ticks, gigaticks;
      /* Evalcost used by this object. The total cost
       * is computed with gigaticks*1E9+ticks.
       */
};


/* Values of struct object.flags: */

#define O_HEART_BEAT         0x01   /* Does it have an heart beat? */
#define O_IS_WIZARD          0x02   /* Is it a wizard player.c? TODO: Remove me */
#define O_ENABLE_COMMANDS    0x04   /* Can it execute commands? */
#define O_CLONE              0x08   /* Is it cloned from a master copy? */
#define O_DESTRUCTED         0x10   /* Is it destructed ? */
#define O_SWAPPED            0x20   /* Is it swapped to file */
#define O_ONCE_INTERACTIVE   0x40   /* Has it ever been interactive? */
#define O_APPROVED           0x80   /* Is std/object.c inherited? TODO: Remove me */
#define O_RESET_STATE        0x100  /* Object in a 'reset':ed state ? */
#define O_WILL_CLEAN_UP      0x200  /* clean_up will be called next time */
#define O_LAMBDA_REFERENCED  0x400  /* be careful with replace_program() */
#define O_SHADOW             0x800  /* Is the object shadowed? */


/* If an object's program or variables are swapped out, the values
 * of .prog resp. .variables are replaced with the associated (even)
 * swap number assigned by the swapper, and the lowest bit of the number
 * is set. The swap number '-1' means 'not swapped'.
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


/* --- struct replace_ob: one scheduled program replacement
 *
 * A list of this structure (obj_list_replace) keeps track of all
 * requested replace_program()s during one backend round.
 *
 * It is possible, though not very useful, to replace an object's
 * program by the very same program.
 */

struct replace_ob
{
    struct object *ob;         /* Object requesting the new program */
    struct program *new_prog;  /* Requested new program */
    int var_offset;            /* Variable offset of .new_prog */
    int fun_offset;            /* Function offset of .new_prog */
    struct replace_ob *next;   /* Link pointer for list */
    struct lambda_replace_program_protector *lambda_rpp;
      /* TODO: ??? */
};


/* --- Macros --- */

#ifdef DEBUG
#    define free_object(object, from) _free_object(object, from)
#else
#    define free_object(object, from) \
            ((void)(--(object)->ref || _free_object(object)))
#endif

  /* Decrement the refcount of <object> and free it if reaches 0.
   * <from> is where the macro is used.
   */


#ifdef DEBUG
#    define decr_object_ref(object, from) free_object(object, from)
#else
#    define decr_object_ref(object, from) (--(object)->ref)
#endif

  /* Decrement the refcount of <object>, but do not free it.
   * <from> is where the macro is used.
   */


#ifndef DEBUG
#   define add_ref(object, from) ((object)->ref++)
#else
    /* use the function add_ref() */
#endif

  /* Increment the refcount of <object>.
   * <from> is where the macro is used.
   */


#define check_object(o) ((o)&&(o)->flags&O_DESTRUCTED ? 0 :(o))

  /* Return 0, if object <o> is NULL or destructed,
   * return <o> else.
   */


/* --- Variables --- */

extern struct replace_ob *obj_list_replace;
extern int tot_alloc_object;
extern int tot_alloc_object_size;
extern struct object NULL_object;


/* --- Prototypes --- */

extern int32 renumber_programs(void);
extern /* TODO: BOOL */ int restore_object(struct object *, char *);
extern void remove_destructed_objects(void);
extern void save_object(struct object *, char *);
extern void move_object(void);
extern void tell_object(struct object *, char *);
extern void tell_npc(struct object *, char *);
extern void reference_prog(struct program *, char *);
extern /* TODO: BOOL */ int register_pointer (void *);
extern void init_pointer_table(struct pointer_record **space);
extern void free_pointer_table(void);
#ifdef DEALLOCATE_MEMORY_AT_SHUTDOWN
extern void remove_all_objects(void);
#endif
extern void do_free_sub_strings(int num_strings, char ** strings, int num_variables, struct variable *variable_names);
extern void free_prog(struct program *progp, short /* TODO: BOOL */  free_sub_strings);
extern void reset_object(struct object *ob, int arg);
extern void replace_programs(void);
extern /* TODO: BOOL */ int shadow_catch_message(struct object *ob, char *str);

#ifdef INITIALIZATION_BY___INIT
extern struct object *get_empty_object(int num_var);
#else
extern struct object *get_empty_object(int num_var, struct variable * variables, struct svalue *initialisers);
#endif

#ifdef DEBUG

extern void add_ref(struct object *, char *);
extern void _free_object(struct object *, char *);

#else

extern int _free_object(struct object *);

#endif

#endif /* __OBJECT_H__ */
