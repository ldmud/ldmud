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

#include "interpret.h" /* for struct svalue variables[1] */

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

#define SCAN_SWAP_BUFSIZE 0x2000

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


extern struct object *load_object PROT((char *, int, int)),
        *find_object PROT((char *));
extern struct object *get_empty_object(), *find_object PROT((char *)),
	*find_object2 PROT((char *));
extern struct object *current_object, *command_giver;
extern struct replace_ob *obj_list_replace;
#define check_object(o) ((o)&&(o)->flags&O_DESTRUCTED?0:(o))

extern struct object *obj_list;
extern p_int new_destructed;

struct value;
void remove_destructed_objects(), save_object PROT((struct object *, char *)),
    move_object PROT(()),
    tell_object PROT((struct object *, char *)),
    tell_npc PROT((struct object *, char *)),
    reference_prog PROT((struct program *, char *));
#ifdef DEBUG
void add_ref PROT((struct object *, char *));
void _free_object PROT((struct object *, char *));
#define free_object(object, from) _free_object(object, from)
#define decr_object_ref(object, from) free_object(object, from)
#else
#define add_ref(object, from) ((object)->ref++)
int _free_object PROT((struct object *));
#define free_object(object, from) \
	((void)(--(object)->ref || _free_object(object)))
#define decr_object_ref(object, from) (--(object)->ref)
#endif

int restore_object PROT((struct object *, char *));
