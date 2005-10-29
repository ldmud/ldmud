/*
 * The reset is used as follows:
 * 0: There is an error in the reset() in this object. Never call it again.
 * 1: Normal state.
 * 2 or higher: This is an interactive player, that has not given any commands
 *		for a number of reset periods.
 */
struct object {
    char enable_heart_beat;	/* Call heart_beat or not. */
    char reset;			/* True if object has been reset */
    char enable_commands;	/* Enable usage of sentence commands */
    char cloned;		/* A cloned object. */
    char name_length;		/* Number of characters in name */
    char destructed;		/* True when the objects is destructed */
    char swapped;		/* True if prog is swapped out */
    char not_touched;		/* how used the object is */
    short total_light;
    short num_variables;	/* Number of variables */
    short ref;			/* Reference count. */
    unsigned short swap_num;	/* File number swapped into */
    struct lnode_def *prog;
    struct lnode_var_def *status;
    struct lnode *heart_beat;
    char *name;
    struct object *next_all, *next_inv;
    struct object *contains;
    struct object *super;		/* Which object surround us ? */
    struct interactive *interactive;	/* Data about an interactive player */
    struct value *variables;		/* All variables to this program */
    struct sentence *sent;
    struct ed_buffer *ed_buffer;	/* Local ed */
    struct wiz_list *wl;		/* What wizard defined this object */
};

extern struct object *load_object(), *find_object(), *find_living_object();
extern struct object *get_empty_object();
extern struct object *current_object, *command_giver;

extern struct object *obj_list;
