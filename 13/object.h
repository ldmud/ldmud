#ifndef __OBJECT_H__
#define __OBJECT_H__ 1

#include "driver.h"

#include "exec.h"       /* struct program */
#include "interpret.h"  /* struct svalue, struct variable */

/* --- Types --- */

struct pointer_record;

/*
 * Definition of an object.
 * If the object is inherited, then it must not be destructed !
 *
 * The reset is used as follows:
 * 0: There is an error in the reset() in this object. Never call it again.
 * 1: Normal state.
 * 2 or higher: This is an interactive player, that has not given any commands
 *		for a number of reset periods.
 */


#define O_HEART_BEAT		0x01  /* Does it have an heart beat ? */
#define O_IS_WIZARD		0x02  /* Is it a wizard player.c ? */
#define O_ENABLE_COMMANDS	0x04  /* Can it execute commands ? */
#define O_CLONE			0x08  /* Is it cloned from a master copy ? */
#define O_DESTRUCTED		0x10  /* Is it destructed ? */
#define O_SWAPPED		0x20  /* Is it swapped to file */
#define O_ONCE_INTERACTIVE	0x40  /* Has it ever been interactive ? */
#define O_APPROVED		0x80  /* Is std/object.c inherited ? */
#define O_RESET_STATE		0x100 /* Object in a 'reset':ed state ? */
#define O_WILL_CLEAN_UP		0x200 /* clean_up will be called next time */
#define O_LAMBDA_REFERENCED	0x400 /* be careful with replace_program() */
#define O_SHADOW		0x800

#define O_PROG_SWAPPED(ob) ((p_int)(ob)->prog & 1)
#define O_VAR_SWAPPED(ob) ((p_int)(ob)->variables & 1)
#define O_SWAP_NUM(ob) \
	(O_PROG_SWAPPED(ob) ? (p_int)(ob)->prog  & ~1 : (ob)->prog->swap_num)

struct object {
    unsigned short flags;	/* Bits or'ed together from above */
    short total_light;
    int next_reset;		/* Time of next reset of this object */
    int time_of_ref;		/* Time when last referenced. Used by swap */
    p_int ref;			/* Reference count. */
#ifdef DEBUG
    p_int extra_ref;		/* Used to check ref count. */
#endif
    struct program *prog;
    char *name;
    struct object *next_all, *next_inv, *next_hash;
    struct object *contains;
    struct object *super;		/* Which object surround us ? */
    struct sentence *sent;
    struct wiz_list *user;		/* What wizard defined this object */
#ifdef EUIDS
    struct wiz_list *eff_user;		/* Used for permissions */
#endif
#ifdef DEBUG
    int extra_num_variables;
    /* amylaar : used to determine where to check ref counts at all... */
#endif
    struct svalue *variables;		/* All variables to this program */
};

struct replace_ob {
    struct object *ob;
    struct program *new_prog;
    int var_offset;
    int fun_offset;
    struct replace_ob *next;
    struct lambda_replace_program_protector *lambda_rpp;
};


/* --- Variables --- */

extern struct object *obj_list;
extern p_int new_destructed;
extern struct object *current_object;
extern struct object *command_giver;
extern struct replace_ob *obj_list_replace;
extern struct object *previous_ob;
extern int tot_alloc_object, tot_alloc_object_size;


/* --- Prototypes --- */
extern int restore_object PROT((struct object *, char *));
extern void remove_destructed_objects PROT((void));
extern void save_object PROT((struct object *, char *));
extern void move_object PROT((void));
extern void tell_object PROT((struct object *, char *));
extern void tell_npc PROT((struct object *, char *));
extern void reference_prog PROT((struct program *, char *));
extern int register_pointer PROT((char *));
extern void init_pointer_table PROT((struct pointer_record **space));
extern void free_pointer_table PROT((void));
extern void remove_all_objects PROT((void));
extern void do_free_sub_strings PROT((int num_strings, char ** strings, int num_variables, struct variable *variable_names));
extern void free_prog PROT((struct program *progp, int free_sub_strings));
extern void reset_object PROT((struct object *ob, int arg));
extern void replace_programs PROT((void));
extern int shadow_catch_message PROT((struct object *ob, char *str));

#ifdef INITIALIZATION_BY___INIT
extern struct object *get_empty_object PROT((int num_var, struct variable * variables));
#else
extern struct object *get_empty_object PROT((int num_var, struct variable * variables, struct svalue *initialisers));
#endif

#ifdef DEBUG
extern void add_ref PROT((struct object *, char *));
extern void _free_object PROT((struct object *, char *));
#define free_object(object, from) _free_object(object, from)
#define decr_object_ref(object, from) free_object(object, from)
#else
#define add_ref(object, from) ((object)->ref++)
extern int _free_object PROT((struct object *));
#define free_object(object, from) \
	((void)(--(object)->ref || _free_object(object)))
#define decr_object_ref(object, from) (--(object)->ref)
#endif

#define check_object(o) ((o)&&(o)->flags&O_DESTRUCTED?0:(o))

#endif /* __OBJECT_H__ */
