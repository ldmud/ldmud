#ifndef __ACTIONS_H__
#define __ACTIONS_H__ 1

#include "driver.h"

#include "datatypes.h"  /* struct object */
#include "instrs.h"     /* F_ADD_VERB, F_ADD_XVERB */
#include "backend.h"    /* rt_context_t */

/* --- Variables --- */

extern char *last_verb;
extern char *last_command;
extern struct object *command_giver;

/* --- Prototypes --- */

extern void restore_command_context (rt_context_t *context);
extern void remove_sent(struct object *ob, struct object *player);
extern void remove_environment_sent(struct object *player);
extern Bool execute_command (char *str, struct object *ob);
extern int e_command(char *str, struct object *ob);
extern Bool e_add_action(struct svalue *func, struct svalue *cmd, int flag);
extern struct vector *e_get_action(struct object *ob, char *verb);
extern struct vector *e_get_all_actions(struct object *ob, int mask);
extern struct vector *e_get_object_actions(struct object *ob1, struct object *ob2);
extern void enable_commands(Bool num);
extern struct svalue *f_execute_command(struct svalue *sp);
extern struct svalue *f_notify_fail(struct svalue *sp);
extern struct svalue *f_query_notify_fail(struct svalue *sp);
extern struct svalue *f_command_stack_depth(struct svalue *sp);
extern struct svalue *f_command_stack(struct svalue *sp);
extern struct svalue *f_set_this_player(struct svalue *sp);
extern struct svalue *f_remove_action(struct svalue *sp);

#ifdef F_ADD_VERB
extern struct svalue *f_add_verb(struct svalue *sp);
#endif
#ifdef F_ADD_XVERB
extern struct svalue *f_add_xverb(struct svalue *sp);
#endif

#endif /* __ACTIONS_H__ */
