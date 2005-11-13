#ifndef __SIMULATE_H__
#define __SIMULATE_H__ 1

/* TODO: See sent.h for the sentence functions */

#include "driver.h"

#include "interpret.h"  /* struct svalue, struct vector */
#include "instrs.h"     /* F_TRANSFER, F_RENAME */
#include "object.h"     /* struct object */
#include "sent.h"       /* struct sentence, struct shadow_sentence */

/* --- Variables --- */

extern char *last_verb;
extern char *inherit_file;
extern int is_wizard_used;

extern struct object *obj_list;
extern struct object *master_ob;
extern p_int new_destructed;

extern struct object *current_object;
extern struct object *command_giver;
extern struct object *current_interactive;
extern struct object *previous_ob;

extern int num_parse_error;

extern struct svalue closure_hook[];

extern int first_showsmallnewmalloced_call;

extern int num_error;
extern char *current_error;
extern char *current_error_file;
extern char *current_error_object_name;
extern mp_int current_error_line_number;

extern int game_is_being_shut_down;
extern int master_will_be_updated;

/* --- Prototypes --- */

extern void free_shadow_sent(struct shadow_sentence *);
extern struct sentence *alloc_sentence(void);

extern void set_svalue_user PROT((struct svalue *svp, struct object *owner));
extern struct object *clone_object PROT((char *str1));
extern struct svalue *f_rename_object PROT((struct svalue *sp));
extern struct object *environment PROT((struct svalue *arg));
extern int command_for_object PROT((char *str, struct object *ob));
extern struct object *object_present PROT((struct svalue *v, struct object *ob));
extern void destruct_object PROT((struct svalue *v));
extern void emergency_destruct PROT((struct object *ob));
extern void destruct2 PROT((struct object *ob));
extern void say PROT((struct svalue *v, struct vector *avoid));
extern void tell_room PROT((struct object *room, struct svalue *v, struct vector *avoid));
extern struct object *first_inventory PROT((struct svalue *arg));
extern void enable_commands PROT((int num));
extern struct vector *get_dir PROT((char *path, int mask));
extern int tail PROT((char *path));
extern int print_file PROT((char *path, int start, int len));
extern int remove_file PROT((char *path));
extern void print_svalue PROT((struct svalue *arg));
extern void do_write PROT((struct svalue *arg));
extern struct object *lookfor_object PROT((char *str, /* TODO: BOOL */ int bLoad));
#define find_object(str) lookfor_object((str), MY_FALSE)
#define get_object(str) lookfor_object((str), MY_TRUE)
extern void move_object PROT((void));
extern struct svalue *f_set_environment PROT((struct svalue *sp));
extern struct svalue *f_set_this_player PROT((struct svalue *sp));
extern void add_light PROT((struct object *p, int n));
extern struct sentence *alloc_sentence PROT((void));
extern void free_all_sent PROT((void));
extern void free_shadow_sent PROT((struct shadow_sentence *p));
extern int player_parser PROT((char *buff));
extern int add_action PROT((struct svalue *func, struct svalue *cmd, int flag));
extern struct svalue *f_add_verb PROT((struct svalue *sp));
extern struct svalue *f_add_xverb PROT((struct svalue *sp));
extern struct svalue *f_remove_action PROT((struct svalue *sp));
extern int status_parse PROT((char *buff));
extern struct vector *get_action PROT((struct object *ob, char *verb));
extern struct vector *get_all_actions PROT((struct object *ob, int mask));
extern struct vector *get_object_actions PROT((struct object *ob1, struct object *ob2));
extern void error VARPROT((char *, ...), printf, 1, 2) NORETURN;
extern void fatal VARPROT((char *, ...), printf, 1, 2) NORETURN;
extern void throw_error PROT((void));
extern char *limit_error_format PROT((char *fixed_fmt, char *fmt));
extern int legal_path PROT((char *path));
extern /* TODO: BOOL */ int check_no_parentdirs (char *path);
extern void smart_log PROT((char *error_file, int line, char *what, char *context));
extern char *check_valid_path PROT((char *path, struct object *caller, char *call_fun, int writeflg));
extern struct svalue *f_shutdown PROT((struct svalue *sp));
extern void startmasterupdate PROT((void));
extern void shutdowngame PROT((void));

extern void slow_shut_down PROT((int minutes));
extern int match_string PROT((char *match, char *str, mp_int len));

extern struct svalue *f_set_driver_hook PROT((struct svalue *sp));
extern void init_closure_hooks PROT((void));
extern struct svalue *f_shadow PROT((struct svalue *sp));
extern struct svalue *f_query_shadowing PROT((struct svalue *sp));
extern struct svalue *f_unshadow PROT((struct svalue *sp));

#ifdef F_TRANSFER
extern int transfer_object PROT((struct svalue *svp));
#endif /* F_TRANSFER */

#ifdef F_RENAME
extern int do_rename PROT((char *fr, char *t));
#endif /* F_RENAME */

#endif  /* __SIMULATE_H__ */
