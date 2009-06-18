#ifndef ACTIONS_H__
#define ACTIONS_H__ 1

#include "driver.h"

#include "typedefs.h"  /* object_t */
#include "sent.h"
#include "simulate.h"

/* --- struct action_s: the action sentence structure ---
 *
 * Sentences of this type are used to hold the actions (verbs+functions)
 * available to one object.
 *
 * A special case are SENT_MARKER sentences which are used to
 * mark the progress of a command search.
 */

struct action_s
{
    sentence_t sent;  /* The basic sentence */
    string_t *verb;
      /* Shared string: the defined verb.
       * For SENT_PLAIN and SENT_SHORT_VERB, this is the whole verb.
       * For SENT_NO_SPACE, only the first letters of the command have
       *   to match this verb.
       */

    object_t *ob;
      /* Object defining this sentence. This value is used for comparisons
       * only, and in case of SENT_MARKER it is in fact a *rt_context_t.
       * The reference is not counted.
       */

    object_t *shadow_ob;
      /* If the action originates from an object shadow, .ob will be the
       * shadowed object (as the action has to seem to come from there),
       * and this will be the actual shadow object defining the object.
       * Otherwise, this entry is NULL.
       */

    callback_t cb;
      /* The function that should actually be called.
       */

    unsigned short short_verb;
      /* SENT_SHORT_VERB: the number of characters which have to
       *   match at minimum.
       */
};

/* --- Variables --- */

extern object_t *command_giver;
extern p_int alloc_action_sent;

/* --- Prototypes --- */

extern void free_action_temporaries(void);
extern void free_action_sent(action_t *p);
extern void remove_action_sent(object_t *ob, object_t *player);
extern void remove_shadow_action_sent(object_t *ob, object_t *player);
extern void remove_environment_sent(object_t *player);
extern void remove_shadow_actions (object_t *shadow, object_t *target);

extern void restore_command_context (rt_context_t *context);
extern Bool execute_command (char *str, object_t *ob);
extern svalue_t *v_add_action(svalue_t *sp, int num_arg);
extern svalue_t *v_command(svalue_t *sp, int num_arg);
extern svalue_t *f_disable_commands(svalue_t *sp);
extern svalue_t *f_enable_commands(svalue_t *sp);
extern svalue_t *f_execute_command(svalue_t *sp);
extern svalue_t *f_living(svalue_t *sp);
extern svalue_t *f_notify_fail(svalue_t *sp);
extern svalue_t *f_query_notify_fail(svalue_t *sp);
extern svalue_t *f_query_actions(svalue_t *sp);
extern svalue_t *f_query_verb(svalue_t *sp);
extern svalue_t *f_query_command(svalue_t *sp);
extern svalue_t *f_command_stack_depth(svalue_t *sp);
extern svalue_t *f_command_stack(svalue_t *sp);
extern svalue_t *f_set_modify_command(svalue_t *sp);
extern svalue_t *f_set_this_player(svalue_t *sp);
extern svalue_t *f_remove_action(svalue_t *sp);
extern svalue_t *f_match_command(svalue_t * sp);

#endif /* ACTIONS_H__ */
