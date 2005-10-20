#ifndef ACTIONS_H__
#define ACTIONS_H__ 1

#include "driver.h"

#include "typedefs.h"  /* object_t */
#include "instrs.h"     /* F_ADD_VERB, F_ADD_XVERB */

/* --- Variables --- */

extern char *last_verb;
extern char *last_command;
extern object_t *command_giver;
extern p_int alloc_action_sent;

/* --- Prototypes --- */

extern void free_action_temporaries(void);
extern void free_action_sent(action_t *p);
extern void purge_action_sent(void);
extern void remove_action_sent(object_t *ob, object_t *player);
extern void remove_environment_sent(object_t *player);

extern void restore_command_context (rt_context_t *context);
extern Bool execute_command (char *str, object_t *ob);
extern int e_command(char *str, object_t *ob);
extern Bool e_add_action(svalue_t *func, svalue_t *cmd, int flag);
extern vector_t *e_get_action(object_t *ob, char *verb);
extern vector_t *e_get_all_actions(object_t *ob, int mask);
extern vector_t *e_get_object_actions(object_t *ob1, object_t *ob2);
extern void enable_commands(Bool num);
extern svalue_t *f_execute_command(svalue_t *sp);
extern svalue_t *f_notify_fail(svalue_t *sp);
extern svalue_t *f_query_notify_fail(svalue_t *sp);
extern svalue_t *f_command_stack_depth(svalue_t *sp);
extern svalue_t *f_command_stack(svalue_t *sp);
extern svalue_t *f_set_this_player(svalue_t *sp);
extern svalue_t *f_remove_action(svalue_t *sp);

#ifdef F_ADD_VERB
extern svalue_t *f_add_verb(svalue_t *sp);
#endif
#ifdef F_ADD_XVERB
extern svalue_t *f_add_xverb(svalue_t *sp);
#endif

#endif /* ACTIONS_H__ */
