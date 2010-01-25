/*---------------------------------------------------------------------------
 * Player command action handling.
 *
 *---------------------------------------------------------------------------
 * The gamedriver offers a simple but quite effective way of associating
 * functions with commands, and parsing the user input for these commands.
 *
 * Central element are the 'actions': the association of a verb with
 * a specific function in a specific object. To allow a living (an object
 * with the O_ENABLE_COMMANDS flag set) to use a verb, the action has
 * to be added to the living by the object with the efun add_action().
 * This usually happens whenever the living enters the vicinity of the
 * object defining the action, but could happen anytime. The restriction
 * on this is that living and action-defining object must share the same
 * vicinity: one must be contained in the other, or share the same
 * environment. If one of the two leaves the shared vicinity, all actions
 * are automatically removed from the living.
 *
 * When the living gives a command (may it be a player or NPC), the parser
 * splits the command string into verb and argument, and searches the list
 * of added actions for a matching action. The search order is the reverse
 * of the order the actions are added. If an action matches the given
 * command, the associated function is called with the parsed argument
 * string as parameter. The function may now decide that it is not the
 * real function for this command and return with result 0; in that case
 * the parser continues its search with the next actions, until it finds one
 * whose function returns a non-zero result.
 *
 * If there is all action functions return 0, the parser prints a failure
 * message. This message can either be set with the efun notify_fail(), or
 * is provided through the H_NOTIFY_FAIL driver hook.
 *
 * Commands usually take the form of '<verb>' or '<verb> <argument>'.
 * Additionally, an action may be defined as 'short verb', that is only
 * the first specified characters of the given command have to match. This
 * allows commands of the form '<verb><argument>', like the famous
 * "'<text to say>" say command. A special use is to use an empty string
 * "" as short verb, which would match every input.
 *
 * Internally all added actions are stored in a linear list of
 * sentence_t attributes (this is actually why the sentences were
 * introduced in the first place). While the parser is searching for
 * an action, a special SENT_MARKER sentence is introduced into this
 * list to mark how far the search has progressed.
 *
 * It is possible to stack commands, ie. to execute a command from within
 * a command.
 * TODO: Make actions optional on three levels: none, only the sentence
 * TODO:: management+match_command(), full.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stddef.h>

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "dumpstat.h"
#include "efuns.h" /* is_wizard_used */
#include "interpret.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "stdstrings.h"
#include "sent.h"
#include "simulate.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/commands.h"
#include "../mudlib/sys/driver_hook.h"

// check user-supplied value for MAX_COMMAND_LENGTH. Length == 0 will crash
// the driver.
#if MAX_COMMAND_LENGTH < 1
#error MAX_COMMAND_LENGTH has to be > 0!
#endif

/*-------------------------------------------------------------------------*/


/* --- struct command_context_s: last command context ---
 *
 * This structure saves the previous command context on the runtime
 * context stack for nested commands.
 *
 * For the very first command the previous context is all 0.
 */

struct command_context_s
{
    rt_context_t rt;         /* the rt_context superclass */
    object_t * this_player;  /* the command_giver */
    object_t * mark_player;  /* the marked command_giver */
    action_t * marker;       /* the marker sentence */
    char * cmd;              /* the full command, a stack buffer */
    string_t * verb;         /* the tabled verb */
    string_t * action_verb;  /* the tabled action verb */
    svalue_t errmsg;         /* the error message */
    object_t * errobj;       /* object which set the error message */
};

/*-------------------------------------------------------------------------*/

/* All the following variables constitute the runtime context for a command,
 * and are saved during the execution of nested commands.
 */

object_t *command_giver;
  /* The object for which the current command is executed.
   * The reference is not counted.
   */

static string_t *last_verb = NULL;
  /* During a command execution, this is the tabled string with the
   * given command verb.
   */

static string_t *last_action_verb = NULL;
  /* During a command execution, this is the tabled string with the
   * command verb as specified in the action definition.
   */

static char *last_command = NULL;
  /* During a command execution, this points to a (stack) buffer with
   * the full command.
   */

static svalue_t error_msg = { T_INVALID };
  /* The error message to be printed when the command can't be found.
   */

static object_t * error_obj = NULL;
  /* The object which set the error message (counted reference).
   */

static action_t * command_marker = NULL;
  /* During a command search/execution, when the search reaches the
   * end of the sentence list, the marker is stored in this variable
   * instead of in the list.
   * The 'ob' in this sentence is the current value of rt_context.
   */

static object_t * marked_command_giver = NULL;
  /* During a command search/execution, this points to the object
   * which sentence list is searched (and thus augmented with the
   * marker sentence). Usually, this value is identical to command_giver.
   * The reference is not counted.
   */

p_int alloc_action_sent = 0;
  /* Statistic: how many action sentences have been allocated.
   */

/*-------------------------------------------------------------------------*/
void
free_action_temporaries (void)

/* GC support: Free the global variables which keep references to objects
 * and svalues. Outside of a command execution these are usually 0, but
 * unfortunately not always (using notify_fail() outside of a command
 * execution for example leaves a value behind).
 */

{
    if (error_msg.type != T_INVALID)
        free_svalue(&error_msg);
    error_msg.type = T_INVALID;

    if (error_obj)
        free_object(error_obj, "free_action_temporaries");
    error_obj = NULL;

    if (last_verb)
        free_mstring(last_verb);
    last_verb = NULL;

    if (last_action_verb)
        free_mstring(last_action_verb);
    last_action_verb = NULL;

} /* free_action_temporaries() */

/*-------------------------------------------------------------------------*/
static INLINE action_t *
new_action_sent(void)

/* Allocate a new empty action sentence and return it.
 */

{
    action_t *p;

    xallocate(p, sizeof *p, "new action sentence");
    alloc_action_sent++;

    p->verb = NULL;
    p->ob = NULL;
    p->shadow_ob = NULL;
    init_empty_callback(&(p->cb));
    return p;
} /* new_action_sent() */

/*-------------------------------------------------------------------------*/
static INLINE void
_free_action_sent (action_t *p)

/* Free the action sentence <p> and all data held by it.
 */

{
#ifdef DEBUG
    if (SENT_IS_INTERNAL(p->sent.type) && SENT_MARKER != p->sent.type)
        fatal("free_action_sent() received internal sent %d\n", p->sent.type);
#endif

    free_callback(&(p->cb));
    if (p->verb)
        free_mstring(p->verb);
    xfree(p);
    alloc_action_sent--;
} /* _free_action_sent() */

void free_action_sent (action_t *p)
  {  _free_action_sent(p); }

#define free_action_sent(p) _free_action_sent(p)

/*-------------------------------------------------------------------------*/
static INLINE void
save_command_context (struct command_context_s * context)

/* Save the current command context into <context> (but don't put it
 * onto the context stack). The saved global variables are zeroed out.
 */

{
    context->rt.type = COMMAND_CONTEXT;
    context->verb = last_verb;
    context->action_verb = last_action_verb;
    context->cmd = last_command;
    context->mark_player = marked_command_giver;
    if (marked_command_giver)
        ref_object(marked_command_giver, "save_command_context");
    context->this_player = command_giver;
    if (command_giver)
        ref_object(command_giver, "save_command_context");
    context->marker = command_marker;
    transfer_svalue_no_free(&(context->errmsg), &error_msg);
    context->errobj = error_obj;

    command_giver = NULL;
    last_verb = NULL;
    last_action_verb = NULL;
    last_command = NULL;
    marked_command_giver = NULL;
    command_marker = NULL;
    error_msg.type = T_INVALID;
    error_obj = NULL;
} /* save_command_context() */

/*-------------------------------------------------------------------------*/
static INLINE void
_restore_command_context (struct command_context_s * context)

/* Restore the last command context from <context>. The global vars
 * are properly freed before they are overwritten.
 */

{
    /* Clear up the current context */
    if (last_verb)
        free_mstring(last_verb);

    if (last_action_verb)
        free_mstring(last_action_verb);

    if (command_marker)
        free_action_sent(command_marker);
    else if (marked_command_giver && !(O_DESTRUCTED & marked_command_giver->flags))
        remove_action_sent((object_t *)context, marked_command_giver);

    /* Restore the previous context */
    last_verb = context->verb;
    last_action_verb = context->action_verb;
    last_command = context->cmd;
    command_giver = check_object(context->this_player);
    if (context->this_player)
        free_object(context->this_player, "restore_command_context");
    marked_command_giver = check_object(context->mark_player);
    if (context->mark_player)
        free_object(context->mark_player, "restore_command_context");
    command_marker = context->marker;
    transfer_svalue(&error_msg, &(context->errmsg));
    if (error_obj)
        free_object(error_obj, "_restore_command_context");
    error_obj = context->errobj;
} /* _restore_command_context() */


void restore_command_context (rt_context_t *context)
  {  _restore_command_context((struct command_context_s *)context); }

#define restore_command_context(c) _restore_command_context(c)

/*-------------------------------------------------------------------------*/
void
remove_action_sent (object_t *ob, object_t *player)

/* Remove all actions defined by <ob> and attached to <player>.
 */

{
    sentence_t **s;

    /* A simple list walk */
    for (s = &player->sent; *s;)
    {
        action_t *tmp;

        tmp = (action_t *)*s;

        if (tmp->ob == ob)
        {
#ifdef DEBUG
            if (d_flag > 1)
            {
                if (tmp->verb)
                    debug_message("%s --Unlinking sentence verb='%s'\n"
                                 , time_stamp(), get_txt(tmp->verb));
                else
                    debug_message("%s --Unlinking sentence verb=0\n"
                                 , time_stamp());
            }
#endif
#ifdef CHECK_OBJECT_REF
            if (s == &player->sent)
                update_object_sent(player, tmp->sent.next);
            else
                *s = tmp->sent.next;
#else
            *s = tmp->sent.next;
#endif /* CHECK_OBJECT_REF */
            free_action_sent(tmp);
        }
        else
            s = &((*s)->next);
    }
} /* remove_action_sent() */

/*-------------------------------------------------------------------------*/
void
remove_shadow_action_sent (object_t *ob, object_t *player)

/* Remove all actions defined by <ob> and attached to <player>.
 */

{
    sentence_t **s;

    /* A simple list walk */
    for (s = &player->sent; *s;)
    {
        action_t *tmp;

        tmp = (action_t *)*s;

        if (tmp->shadow_ob == ob)
        {
#ifdef DEBUG
            if (d_flag > 1)
            {
                if (tmp->verb)
                    debug_message("%s --Unlinking sentence verb='%s'\n"
                                 , time_stamp(), get_txt(tmp->verb));
                else
                    debug_message("%s --Unlinking sentence verb=0\n"
                                 , time_stamp());
            }
#endif
#ifdef CHECK_OBJECT_REF
            if (s == &player->sent)
                update_object_sent(player, tmp->sent.next);
            else
                *s = tmp->sent.next;
#else
            *s = tmp->sent.next;
#endif /* CHECK_OBJECT_REF */
            free_action_sent(tmp);
        }
        else
            s = &((*s)->next);
    }
} /* remove_shadow_action_sent() */

/*-------------------------------------------------------------------------*/
void
remove_environment_sent (object_t *player)

/* Remove all actions on <player> defined by objects with the same
 * environment (which includes the environment object).
 */

{
    sentence_t **p;
    action_t *s;
    object_t *super, *ob;

    super = player->super;
    p= &player->sent;

    if ( NULL != (s = (action_t *)*p) ) for(;;)
    {
        ob = s->ob;
        if (!SENT_IS_INTERNAL(s->sent.type)
         && ((ob->super == super && ob != player) || ob == super )
           )
        {
            do {
                action_t *tmp;

#ifdef DEBUG
                if (d_flag > 1)
                {
                    if (s->verb)
                        debug_message("%s --Unlinking sentence verb='%s'\n"
                                     , time_stamp(), get_txt(s->verb));
                    else
                        debug_message("%s --Unlinking sentence verb=0\n"
                                     , time_stamp());
                }
#endif
                tmp = s;
                s = (action_t *)s->sent.next;
                free_action_sent(tmp);
                if (!s) {
#ifdef CHECK_OBJECT_REF
                if (p == &player->sent)
                    update_object_sent(player, NULL);
                else
                    *p = NULL;
#else
                    *p = NULL;
#endif /* CHECK_OBJECT_REF */
                    return;
                }
            } while (s->ob == ob);
#ifdef CHECK_OBJECT_REF
            if (p == &player->sent)
                update_object_sent(player, (sentence_t *)s);
            else
                *p = (sentence_t *)s;
#else
            *p = (sentence_t *)s;
#endif /* CHECK_OBJECT_REF */
        }
        else
        {
            do {
                p = &s->sent.next;
                if (!(s = (action_t *)*p)) return;
            } while (s->ob == ob);
        }
    }
} /* remove_environment_sent() */

/*-------------------------------------------------------------------------*/
void
remove_shadow_actions (object_t *shadow, object_t *target)

/* Remove all shadow actions defined by <shadow> and attached to <target> or
 * an object in <target>'s vicinity.
 */

{
    object_t *item;
    object_t *shadowing;

    /* Get the real underlying object, just as add_action does. */
    while ((target->flags & O_SHADOW)
     && NULL != (shadowing = O_GET_SHADOW(target)->shadowing))
    {
        target = shadowing;
    }

    remove_shadow_action_sent(shadow, target);
    for (item = target->contains; item; item = item->next_inv)
    {
        if (shadow != item)
            remove_shadow_action_sent(shadow, item);
    }
    if (target->super)
    {
        remove_shadow_action_sent(shadow, target->super);

        for (item = target->super->contains; item; item = item->next_inv)
        {
            if (shadow != item && target != item)
                remove_shadow_action_sent(shadow, item);
        }
    }
} /* remove_shadow_actions() */

/*-------------------------------------------------------------------------*/
static Bool
call_modify_command (char *buff)

/* Call the modify_command function/hook for the given command in <buff>
 * and replace the text in <buff> with the new command (if any).
 *
 * Return FALSE if everything is ok, and TRUE if something happened
 * (like the command_giver selfdestructed or the hook already finished
 * executing the command).
 */

{
    interactive_t *ip;
    svalue_t *svp;

    svp = NULL;

    if (O_SET_INTERACTIVE(ip, command_giver)
     && ip->modify_command )
    {
        object_t *ob = ip->modify_command;

        if (ob->flags & O_DESTRUCTED)
        {
            ip->modify_command = 0;
            free_object(ob, "modify_command");
        }
        else if (driver_hook[H_MODIFY_COMMAND_FNAME].type == T_STRING)
        {
            push_c_string(inter_sp, buff);
            svp = sapply(driver_hook[H_MODIFY_COMMAND_FNAME].u.str, ob, 1);
            /* !command_giver means that the command_giver has been dested. */
            if (!command_giver)
                return MY_TRUE;
        }
    }
    else
    {
        if (driver_hook[H_MODIFY_COMMAND].type == T_CLOSURE)
        {
            lambda_t *l;

            l = driver_hook[H_MODIFY_COMMAND].u.lambda;
            if (driver_hook[H_MODIFY_COMMAND].x.closure_type == CLOSURE_LAMBDA)
            {
                free_object(l->ob, "call_modify_command");
                l->ob = ref_object(command_giver, "call_modify_command");
            }
            push_c_string(inter_sp, buff);
            push_ref_object(inter_sp, command_giver, "call_modify_command");
            call_lambda(&driver_hook[H_MODIFY_COMMAND], 2);
            transfer_svalue(svp = &apply_return_value, inter_sp--);
            if (!command_giver)
                return MY_TRUE;
        }
        else if (driver_hook[H_MODIFY_COMMAND].type == T_STRING
            && !(O_DESTRUCTED & command_giver->flags))
        {
            push_c_string(inter_sp, buff);
            svp =
              sapply(driver_hook[H_MODIFY_COMMAND].u.str, command_giver, 1);
            if (!command_giver)
                return MY_TRUE;
        }
        else if (driver_hook[H_MODIFY_COMMAND].type == T_MAPPING)
        {
            svalue_t sv;
            string_t * str;

            if ( NULL != (str = find_tabled_str(buff)) )
            {
                put_string(&sv, str);
                svp =
                  get_map_value(driver_hook[H_MODIFY_COMMAND].u.map, &sv);
                if (svp->type == T_CLOSURE)
                {
                    push_ref_string(inter_sp, sv.u.str);
                    push_ref_object(inter_sp, command_giver, "call_modify_command");
                    call_lambda(svp, 2);
                    transfer_svalue(svp = &apply_return_value, inter_sp--);
                    if (!command_giver)
                        return MY_TRUE;
                }
            }
        }
    }

    /* If svp is not NULL, it contains the new, modified command.
     */
    if (svp)
    {
        if (svp->type == T_STRING)
        {
            extract_cstr(buff, svp->u.str, (size_t)MAX_COMMAND_LENGTH);
        } else if (svp->type == T_NUMBER && svp->u.number) {
            return MY_TRUE;
        }
    }

    return MY_FALSE;
} /* call_modify_command() */

/*-------------------------------------------------------------------------*/
static int
special_parse (char *buff)

/* Implement a few hardcoded commands. Return TRUE if such a command
 * was given.
 * TODO: Remove this feature.
 */

{
#ifdef USE_SET_IS_WIZARD
    if (!is_wizard_used || command_giver->flags & O_IS_WIZARD)
#endif
    {
        Bool no_curobj = MY_FALSE;

        if (strcmp(buff, "malloc") == 0)
        {
            strbuf_t sbuf;

            status_parse(&sbuf, "malloc");
            strbuf_send(&sbuf);
            return 1;
        }

        if (strcmp(buff, "malloc extstats") == 0)
        {
            strbuf_t sbuf;

            status_parse(&sbuf, "malloc extstats");
            strbuf_send(&sbuf);
            return 1;
        }

        if (strcmp(buff, "dumpallobj") == 0) {

            if (!current_object)
            {
                current_object = ref_object(command_giver, "dumpallobj");
                no_curobj = MY_TRUE;
            }
            add_message("Dumping to /OBJ_DUMP ... ");
            dumpstat(STR_OBJDUMP_FNAME);
            dumpstat_dest(STR_DESTOBJDUMP_FNAME);
            add_message("done\n");
            if (no_curobj)
            {
                free_object(current_object, "dumpallobj");
                current_object = NULL;
            }
            return 1;
        }
#ifdef OPCPROF /* amylaar */
        if (strcmp(buff,  "opcdump") == 0) {
            if (!current_object)
            {
                current_object = ref_object(command_giver, "opcdump");
                no_curobj = MY_TRUE;
            }
            opcdump(STR_OPCDUMP_FNAME);
            if (no_curobj)
            {
                free_object(current_object, "opcdump");
                current_object = NULL;
            }
            return 1;
        }
#endif
        if (strncmp(buff, "status", (size_t)6) == 0)
        {
            Bool rc;
            strbuf_t sbuf;

            rc = status_parse(&sbuf, buff+6+(buff[6]==' '));
            if (rc)
                strbuf_send(&sbuf);
            else
                strbuf_free(&sbuf);
            return rc;
        }
    } /* end of wizard-only special parse commands */

    return 0;
} /* special_parse() */

/*-------------------------------------------------------------------------*/
static void
notify_no_command (char *command, object_t *save_command_giver)

/* No action could be found for <command>, thus print a failure notice
 * to the command_giver. <save_command_giver> is the object for which
 * the command was received, usually it is identical to command_giver.
 *
 * Called by the command parser, this function evaluates the H_NOTIFY_FAIL
 * hook to do its job. If the hook is not set, the default_err_message
 * is printed.
 */

{
    svalue_t *svp;

    Bool      useHook;

    useHook = (   driver_hook[H_SEND_NOTIFY_FAIL].type == T_CLOSURE
               || driver_hook[H_SEND_NOTIFY_FAIL].type == T_STRING
              );

    svp = &error_msg;

    if (svp->type == T_STRING)
    {
        if (!useHook)
            tell_object(command_giver, svp->u.str);
        else
            push_svalue(svp);
    }
    else if (svp->type == T_CLOSURE)
    {
        push_ref_valid_object(inter_sp, save_command_giver, "notify_no_command");
        call_lambda(svp, 1);
        /* add_message might cause an error, thus, we free the closure first. */
        if (inter_sp->type == T_STRING)
        {
            if (!useHook)
            {
                tell_object(command_giver, inter_sp->u.str);
                pop_stack();
            }
        }
        else
        {
            pop_stack();
            useHook = MY_FALSE;
        }
    }
    else if (driver_hook[H_NOTIFY_FAIL].type == T_STRING)
    {
        if (!useHook)
            tell_object(command_giver, driver_hook[H_NOTIFY_FAIL].u.str);
        else
            push_svalue(&driver_hook[H_NOTIFY_FAIL]);
    }
    else if (driver_hook[H_NOTIFY_FAIL].type == T_CLOSURE)
    {
        if (driver_hook[H_NOTIFY_FAIL].x.closure_type == CLOSURE_LAMBDA)
        {
            free_object(driver_hook[H_NOTIFY_FAIL].u.lambda->ob, "notify_no_command");
            driver_hook[H_NOTIFY_FAIL].u.lambda->ob = ref_object(command_giver, "notify_no_command");
        }
        push_c_string(inter_sp, command);
        push_ref_valid_object(inter_sp, save_command_giver, "notify_no_command");
        call_lambda(&driver_hook[H_NOTIFY_FAIL], 2);
        if (inter_sp->type == T_STRING)
        {
            if (!useHook)
            {
                tell_object(command_giver, inter_sp->u.str);
                pop_stack();
            }
        }
        else
        {
            pop_stack();
            useHook = MY_FALSE;
        }
    }
    else /* No H_NOTIFY_FAIL hook set, and no notify_fail() given */
    {
        free_svalue(svp); /* remember: this is &error_msg */
        svp->type = T_INVALID;

        if (error_obj)
            free_object(error_obj, "notify_no_command");
        error_obj = NULL;

        errorf("Missing H_NOTIFY_FAIL hook, and no notify_fail() given.\n");
        /* NOTREACHED */
        useHook = MY_FALSE;
    }

    /* If the output has to go through a hook, push the remaining
     * arguments and call the hook.
     */
    if (useHook)
    {
        if (error_obj != NULL)
            push_ref_valid_object(inter_sp, error_obj, "notify-fail error_obj");
        else
            push_number(inter_sp, 0);
        push_ref_valid_object(inter_sp, save_command_giver, "notify-fail save_command_giver");

        if (driver_hook[H_SEND_NOTIFY_FAIL].type == T_STRING)
        {
            (void)sapply_ign_prot( driver_hook[H_SEND_NOTIFY_FAIL].u.str
                                 , command_giver, 3);
        }
        else
        {
            if (driver_hook[H_SEND_NOTIFY_FAIL].x.closure_type == CLOSURE_LAMBDA)
            {
                free_object(driver_hook[H_SEND_NOTIFY_FAIL].u.lambda->ob, "notify_no_command");
                driver_hook[H_SEND_NOTIFY_FAIL].u.lambda->ob = ref_object(command_giver, "notify_no_command");
            }
            call_lambda(&driver_hook[H_SEND_NOTIFY_FAIL], 3);
            pop_stack();
        }
    }

    free_svalue(svp); /* remember: this is &error_msg */
    svp->type = T_INVALID;

    if (error_obj)
        free_object(error_obj, "notify_no_command");
    error_obj = NULL;

} /* notify_no_command() */

/*-------------------------------------------------------------------------*/
static Bool
parse_command (char *buff, Bool from_efun)

/* Take the command in <buff> and execute it.
 * The command_giver and marked_command_giver are set, last_verb,
 * last_action_verb and last_command may be set (and will be overwritten).
 *
 * The command will be searched in the list of marked_command_giver.
 *
 * Return FALSE on failure (command not found), and TRUE on success.
 *
 * The function distinguishes two calling modes:
 *  - !from_efun: the driver does the traditional command parsing,
 *       that means that modify_command and notify_fail are to
 *       be handled by the driver.
 *  - from_efun: the mudlib handles the command parsing, and this
 *       function must not call modify_command or notify_fail.
 *       Also, marked_command_giver is already set and correct.
 *
 * Since this function is called from execute_command() (directly
 * or indirectly through f_execute_command()), it is not necessary
 * to clean up the globals here.
 */

{
    char *p;                       /* handy string pointer */
    sentence_t *s;                 /* handy sentence pointer */
    action_t *marker_sent;         /* the marker sentence */
    ptrdiff_t length;              /* length of the verb */
    object_t *save_current_object = current_object;
    object_t *save_command_giver  = command_giver;

#ifdef DEBUG
    if (d_flag > 1)
        debug_message("%s cmd [%s]: %s\n", time_stamp()
                     , get_txt(command_giver->name), buff);
#endif

    /* Strip trailing spaces.
     * Afterwards, p will point at the last non-space character.
     */
    for (p = buff + strlen(buff) - 1; p >= buff; p--)
    {
        if (*p != ' ')
            break;
        *p = '\0';
    }
    if (buff[0] == '\0')
        return MY_FALSE;

    /* Call the modify-command function
     * This may clobber command_giver and/or current_object.
     */
    if (!from_efun && call_modify_command(buff))
        return MY_TRUE;

    /* Parse for special commands
     */
    if (!from_efun && special_parse(buff))
        return MY_TRUE;

    /* Determine the verb and set last_verb and last_command
     */

    if (last_verb)
        free_mstring(last_verb);

    length = p - buff;
    p = strchr(buff, ' ');
    if (p == NULL)
    {
        length += 1;
        last_verb = new_tabled(buff);
    }
    else
    {
        *p = '\0';
        last_verb = new_tabled(buff);
        *p = ' ';
        length = p - buff;
    }

    last_command = buff;

    /* Get the empty marker sentence */
    marker_sent = new_action_sent();
    marker_sent->sent.type = SENT_MARKER;

    /* Scan the list of sentences for the saved command giver */
    for (s = marked_command_giver->sent; s; s = s->next)
    {
        svalue_t *ret;
        object_t *command_object;
        action_t *sa;            /* (action_t *)s */
        unsigned char type;      /* s->type */
        sentence_t *next;        /* used only as flag */
        sentence_t *insert;      /* insertion point */

        sa = (action_t *)s;

        /* Test the current sentence */
        if ((type = s->type) == SENT_PLAIN)
        {
            if (sa->verb != last_verb)
                continue;
        }
        else if (type == SENT_SHORT_VERB)
        {
            /* The verb may be shortened to a few leading characters,
             * but not shorter than .short_verb.
             */
            size_t len;
            if (sa->short_verb)
            {
                len = mstrsize(last_verb);
                if (len < sa->short_verb
                 || len > mstrsize(sa->verb)
                 || (   sa->verb != last_verb
                     && strncmp(get_txt(sa->verb), get_txt(last_verb), len) != 0))
                    continue;
            }
            else
            {
                len = mstrsize(sa->verb);
                if (strncmp(buff, get_txt(sa->verb), len) != 0)
                    continue;
            }
        }
        else if (type == SENT_OLD_NO_SPACE || type == SENT_NO_SPACE)
        {
            /* The arguments may follow the verb without space,
             * that means we just have to check if buff[] begins
             * with sa->verb.
             */
            size_t len;
            len = mstrsize(sa->verb);
            if (strncmp(buff, get_txt(sa->verb), len) != 0)
                continue;
        }
        else
        {
            /* SENT_MARKER ... due to recursion. Or another SENT_IS_INTERNAL */
            continue;
        }

        /*
         * Now we have found a special sentence!
         */

        if (last_action_verb)
            free_mstring(last_action_verb);
        last_action_verb = ref_mstring(sa->verb);

#ifdef DEBUG
        if (d_flag > 1)
            debug_message("%s Local command on %s\n", time_stamp()
                         , get_txt(sa->ob->name));
#endif

        /* If the function is static and not defined by current object,
         * then the call will fail.
         *
         * If this command is called directly from player input (and not
         * from the command() efun), then we can allow static functions.
         * For this, we set current_object (which is NULL in this case)
         * to the object defining the function.
         *
         * current_object is reset just after the call to apply().
         */
        if (current_object == NULL)
            current_object = sa->ob;

        /* Remember the object, to update score.
         */
        command_object = sa->ob;

        /* If the next sentence(s) are of type SENT_MARKER themselves,
         * skip them.
         */

        for (insert = s, next = s->next
            ; next && next->type == SENT_MARKER
            ; insert = next, next = next->next
            ) NOOP;

        /* Place the marker_sent in the objects sentence list */

        if (!next)
        {
            /* We are at the end of the sentence list: the marker
             * is stored in the global command_marker.
             * And since new commands are always added at the start,
             * the end will remain the end.
             */
            marker_sent->sent.next = NULL;
            command_marker = marker_sent;
        }
        else
        {
            /* Place the marker, so we can continue the search, no matter what
             * the object does. But beware, if the command_giver is destructed,
             * the marker will be freed.
             * Take care not to alter marker addresses.
             */
            insert->next = (sentence_t *)marker_sent;

            marker_sent->ob = (object_t *)rt_context;
            marker_sent->sent.next = next;
            marker_sent->sent.type = SENT_MARKER;
        }

        /* Clear the other struct elements - after all, this might be
         * a reused command sentence.
         */
        marker_sent->sent.type = SENT_MARKER;
        marker_sent->verb = NULL;
        marker_sent->shadow_ob = NULL;
        init_empty_callback(&(marker_sent->cb));

        /* Push the argument and call the command function.
         */
        if (s->type == SENT_OLD_NO_SPACE)
        {
            if (strlen(buff) > mstrsize(sa->verb))
            {
                push_c_string(inter_sp, &buff[mstrsize(sa->verb)]);
                ret = execute_callback(&(sa->cb), 1, MY_TRUE, save_current_object == NULL);
            }
            else
            {
                ret = execute_callback(&(sa->cb), 0, MY_TRUE, save_current_object == NULL);
            }
        }
        else if (s->type == SENT_NO_SPACE)
        {
            if (strlen(buff) > mstrsize(sa->verb))
            {
                /* We need to cut off the verb right where the
                 * arguments start. On the other hand, we can't modify
                 * the last_verb permanently, as this sentence might
                 * fail and other sentences want the full one.
                 */
                char ch;
                size_t len = mstrsize(sa->verb);

                push_string(inter_sp, last_verb);
                ch = buff[len];
                buff[len] = '\0';
                last_verb = new_tabled(buff);
                buff[len] = ch;
                push_c_string(inter_sp, &buff[len]);
                ret = execute_callback(&(sa->cb), 1, MY_TRUE, save_current_object == NULL);
                free_mstring(last_verb);
                last_verb = inter_sp->u.str; inter_sp--;
            }
            else
            {
                ret = execute_callback(&(sa->cb), 0, MY_TRUE, save_current_object == NULL);
            }
        }
        else if (buff[length] == ' ')
        {
            push_c_string(inter_sp, &buff[length+1]);
            ret = execute_callback(&(sa->cb), 1, MY_TRUE, save_current_object == NULL);
        }
        else
        {
            ret = execute_callback(&(sa->cb), 0, MY_TRUE, save_current_object == NULL);
        }

        /* Restore the old current_object and command_giver */
        current_object = save_current_object;
        command_giver  = save_command_giver;

        /* If the command_giver was destructed, clean up and exit.
         * Note that s might be a dangling pointer right now.
         */
        if (command_giver->flags & O_DESTRUCTED)
        {
            /* the caller (execute_command()) will do the clean up */
            return MY_TRUE;
        }

        /* Remove the marker from the sentence chain, and make s->next valid */
        if ( NULL != (s = marker_sent->sent.next) && s->type != SENT_MARKER)
        {
            /* The following sentence is a non-SENT_MARKER: the data from
             * that sentence is copied into the place of the SENT_MARKER; the
             * storage of the sentence will then be reused for the new
             * SENT_MARKER.
             */
            *marker_sent = *((action_t *)s);
            s->next = (sentence_t *)marker_sent;
            marker_sent = (action_t *)s;
        }
        else
        {
            if (next)
            {
                /* !s : there have been trailing sentences before, but all
                 * have been removed.
                 * s->type == SENT_MARKER : There was a delimiter sentence
                 * between the two markers, which has been removed.
                 */
                sentence_t **pp;

                for (pp = &marked_command_giver->sent
                    ; (s = *pp) != (sentence_t *)marker_sent;
                    )
                    pp = &s->next;
                *pp = s->next;
            }
            s = (sentence_t *)marker_sent;
        }

        /* If we get fail from the call, it was wrong second argument. */
        if (!ret || (ret->type == T_NUMBER && ret->u.number == 0))
        {
            continue;
        }

        /* Command was found */
        if (O_IS_INTERACTIVE(command_giver)
#ifdef USE_SET_IS_WIZARD
            && !(command_giver->flags & O_IS_WIZARD)
#endif
           )
        {
            command_object->user->score++;
        }
        break;
    } /* for() */

    /* At this point, the marker_sent is not part of the sentence
     * list anymore. Make sure it will be freed.
     */
    marker_sent->sent.type = SENT_MARKER;
    marker_sent->verb = NULL;
    marker_sent->shadow_ob = NULL;
    init_empty_callback(&(marker_sent->cb));
    command_marker = marker_sent;

    /* If the command was not found, notify the failure */
    if (s == 0)
    {
        if (!from_efun)
            notify_no_command(buff, marked_command_giver);
        return MY_FALSE;
    }

    return MY_TRUE;
} /* parse_command() */

/*-------------------------------------------------------------------------*/
Bool
execute_command (char *str, object_t *ob)

/* Parse and execute the command <str> for object <ob> as command_giver.
 * <ob> may be an interactive player or a NPC.
 *
 * Return TRUE if the command was recognized, and FALSE if not.
 * Return the result from the player_parser().
 *
 * The current command execution context (which is all 0 when called
 * for an interactive command) is saved on the runtime stack.
 *
 * This is the main entry point for driver based command parsing.
 * For interactive commands, this is called from the backend loop; for other
 * commands this is called from v_command().
 *
 * Note that the buffer of <str> may be modified and/or extended by this
 * call.
 */

{
    struct command_context_s context;
    Bool res;

    /* Save the current context */
    save_command_context(&context);
    context.rt.last = rt_context;
    rt_context = (rt_context_t *)&context.rt;

    /* Default settings */
    command_giver = ob;
    marked_command_giver = ob;
    last_command = str;

    /* Execute the command */
    if (driver_hook[H_COMMAND].type == T_STRING)
    {
        svalue_t *svp;

        push_c_string(inter_sp, str);
        svp = sapply_ign_prot(driver_hook[H_COMMAND].u.str, ob, 1);
        if (!svp)
        {
            errorf("Can't find H_COMMAND lfun '%s' in object '%s'.\n"
                 , get_txt(driver_hook[H_COMMAND].u.str), get_txt(ob->name)
                 );
            res = 0;
        }
        else
            res = (svp->type != T_NUMBER) || (svp->u.number != 0);
    }
    else if (driver_hook[H_COMMAND].type == T_CLOSURE)
    {
        lambda_t *l;

        l = driver_hook[H_COMMAND].u.lambda;
        if (driver_hook[H_COMMAND].x.closure_type == CLOSURE_LAMBDA)
        {
            free_object(l->ob, "execute_command");
            l->ob = ref_object(ob, "execute_command");
        }
        push_c_string(inter_sp, str);
        push_ref_object(inter_sp, ob, "execute_command");
        call_lambda(&driver_hook[H_COMMAND], 2);
        res = (inter_sp->type != T_NUMBER) || (inter_sp->u.number != 0);
        free_svalue(inter_sp);
        inter_sp--;
    }
    else
        res = parse_command(str, MY_FALSE);

    /* Restore the previous context */
    rt_context = context.rt.last;
    restore_command_context(&context);

    return res;
} /* execute_command() */

/*-------------------------------------------------------------------------*/
static Bool
e_add_action (svalue_t *func, svalue_t *cmd, p_int flag)

/* Implementation of the efun add_action().
 *
 * This function returns TRUE if an error occured, or FALSE if the
 * action was successfull.
 */

{
    action_t *p;
    object_t *ob, *shadow_ob;
    string_t *str;
    int error_index;

    /* Can't take actions from destructed objects */
    if (current_object->flags & O_DESTRUCTED)
        return MY_TRUE;

    shadow_ob = NULL;
    ob = current_object;

    /* Check if the call comes from a shadow of the current object */
    if (func->type == T_STRING
     && ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
    {
        shadow_ob = ob;
        str = find_tabled(func->u.str);
        do
        {
            ob = O_GET_SHADOW(ob)->shadowing;
            if (find_function(str, ob->prog) >= 0)
            {
                if (!privilege_violation4(
                    STR_SHADOW_ADD_ACTION, ob, str, 0, inter_sp)
                )
                    return MY_TRUE;
            }
        } while(O_GET_SHADOW(ob)->shadowing);
    }

    /* We must have a valid commandgiver to succeed */
    if (command_giver == 0 || (command_giver->flags & O_DESTRUCTED))
    {
        return MY_TRUE;
    }

    /* And the commandgiver must be in the vicinity */
    if (ob != command_giver
     && ob->super != command_giver
     && (ob->super == NULL || ob->super != command_giver->super)
       /* above condition includes the check command_giver->super == NULL */
     && ob != command_giver->super)
        errorf("add_action from object '%s' that was not present to '%s'.\n"
             , get_txt(ob->name), get_txt(command_giver->name));

#ifdef DEBUG
    if (d_flag > 1)
        debug_message("%s --Add action for %s\n", time_stamp(), get_txt(cmd->u.str));
#endif

    /* Allocate and initialise a new sentence */
    p = new_action_sent();

    if (func->type == T_STRING)
    {
        /* Sanity checks */
        if (get_txt(func->u.str)[0] == ':')
        {
            free_action_sent(p);
            errorf("Illegal function name: %s\n", get_txt(func->u.str));
        }

        if (compat_mode)
        {
            char *s;
            s = get_txt(func->u.str);
            if (*s++=='e' && *s++=='x' && *s++=='i' && *s++=='t'
             && (!*s || mstrsize(func->u.str) == 4))
            {
                free_action_sent(p);
                errorf("Illegal to define a command to the exit() function.\n");
                /* NOTREACHED */
                return MY_TRUE;
            }
        }

        error_index = setup_function_callback(&(p->cb), ob, func->u.str
                                             , 0, NULL, MY_TRUE
                                             );
        /* setup_function_callback makes its own copy, so we free ours. */
        free_mstring(func->u.str);
        func->type = T_INVALID; /* So that an error won't free it again. */
    }
    else if (func->type == T_CLOSURE)
    {
        error_index = setup_closure_callback(&(p->cb), func
                                             , 0, NULL, MY_TRUE
                                             );
    }
    else
    {
        error_index = 0;
    }

    if (error_index >= 0)
    {
        free_action_sent(p);
        vefun_bad_arg(error_index + 1, inter_sp);
        /* NOTREACHED */
        return MY_TRUE;
    }

    p->ob = ob;
    p->shadow_ob = shadow_ob;

    /* Set ->verb to the command verb, made tabled */
    p->verb = make_tabled(cmd->u.str); cmd->type = T_NUMBER;
    p->sent.type = SENT_PLAIN;
    p->short_verb = 0;

    if (flag)
    {
        if (flag == AA_SHORT)
        {
            p->sent.type = SENT_SHORT_VERB;
        }
        else if (flag == AA_NOSPACE)
        {
            p->sent.type = SENT_OLD_NO_SPACE;
        }
        else if (flag == AA_IMM_ARGS)
        {
            p->sent.type = SENT_NO_SPACE;
        }
        else if (flag < AA_VERB)
        {
            if ((size_t)(-flag) >= mstrsize(p->verb))
            {
                free_action_sent(p);
                errorf("Bad arg 3 to add_action(): value %"PRIdPINT" larger than verb '%s'.\n"
                     , flag, get_txt(p->verb));
                /* NOTREACHED */
                return MY_TRUE;
            }
            else
            {
                p->sent.type = SENT_SHORT_VERB;
                p->short_verb = 0 - (unsigned short)flag;
            }
        }
        else
        {
            free_action_sent(p);
            errorf("Bad arg 3 to add_action(): value %"PRIdPINT" too big.\n"
                 , flag);
            /* NOTREACHED */
            return MY_TRUE;
        }
    }

    /* Now chain in the sentence */
    if (command_giver->flags & O_SHADOW)
    {
        sentence_t *previous = command_giver->sent;

        p->sent.next = previous->next;
        previous->next = (sentence_t *)p;
    }
    else
    {
        p->sent.next = command_giver->sent;
#ifdef CHECK_OBJECT_REF
        update_object_sent(command_giver, (sentence_t *)p);
#else
        command_giver->sent = (sentence_t *)p;
#endif /* CHECK_OBJECT_REF */
    }

    return MY_FALSE;
} /* e_add_action() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_add_action (svalue_t *sp, int num_arg)

/* EFUN add_action()
 *
 *   void add_action(string fun, string cmd [, int flag])
 *
 * Add an action (verb + function) to the commandgiver.
 *
 * Attempting to add an action from a shadow causes a privilege violation
 * ("shadow_add_action", shadow, func).
 *
 * TODO: In the long run, get rid of actions.
 */

{
    svalue_t *arg;
    svalue_t *verb;

    arg = sp - num_arg + 1;

    verb = &arg[1];

    if (e_add_action(&arg[0], verb
                  , num_arg > 2 ? arg[2].u.number : 0))
    {
        /* silent error condition, deallocate strings by hand */
        sp = pop_n_elems(num_arg, sp);
    }
    else
    {
        /* add_action has reused the strings or freed it */
        sp -= num_arg;
    }

    return sp;
} /* v_add_action() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_command (svalue_t *sp, int num_arg)

/* EFUN command()
 *
 *   int command(string str)
 *   int command(string str, object ob)
 *
 * Execute str as a command given directly by the user. Any
 * effects of the command will apply to object <ob> (defaults to
 * the current object if not given).
 *
 * Return value is 0 for failure. Otherwise a numeric value is
 * returned which tells the evaluation cost. Bigger number means
 * higher cost.  The evaluation cost is approximately the number
 * of LPC machine code instructions executed.
 *
 * If command() is called on another object, it is not possible
 * to call static functions in this way, to give some protection
 * against illegal forces.
 *
 * TODO: With add_action(), this should go in the long run.
 */

{
    int rc;
    svalue_t *arg;
    object_t *ob;
    char buff[MAX_COMMAND_LENGTH];
    int save_eval_cost = eval_cost - 1000;
    interactive_t *ip;

    arg = sp - num_arg + 1;
    if (num_arg == 1)
        ob = current_object;
    else
        ob = arg[1].u.ob;

    rc = 0;

    if (!(current_object->flags & O_DESTRUCTED)
     && !(ob->flags & O_DESTRUCTED))
    {
        size_t len;

        /* Make a copy of the given command as the parser might change it */

        len = mstrsize(arg->u.str);
        if (len >= sizeof(buff) - 1)
            errorf("Command too long: '%.200s...'\n", get_txt(arg->u.str));
        strncpy(buff, get_txt(arg[0].u.str), len);
        buff[len] = '\0';

        if (O_SET_INTERACTIVE(ip, ob))
            trace_level |= ip->trace_level;

        if (execute_command(buff, ob))
            rc = eval_cost - save_eval_cost;
    }

    if (num_arg > 1)
        free_svalue(sp--);

    free_svalue(sp);
    put_number(sp, rc);

    return sp;
} /* v_command() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_execute_command (svalue_t *sp)

/* EFUN execute_command()
 *
 *   int execute_command (string command, object origin, object player)
 *
 * Low-level access to the command parser: take the <command>, parse
 * it into verb and argument and call the appropriate action added
 * to <origin> (read: <origin> is the object 'issuing' the command). For the
 * execution of the function(s), this_player() is set to <player>.
 *
 * The efun raises a privilege violation ("execute_command", this_object(),
 * origin, command).
 *
 * Note that this function does not use the H_MODIFY_COMMAND
 * and H_NOTIFY_FAIL hooks; the notify_fail() efun can be used,
 * but must be evaluated by the caller.
 *
 * Return TRUE if the command was found, and FALSE if not.
 */

{
    svalue_t *argp;
    object_t *origin, *player;
    char buf[MAX_COMMAND_LENGTH];
    size_t len;
    Bool res;

    /* Test and get the arguments from the stack */
    argp = sp - 2;

    /* Make a copy of the given command as the parser might change it */

    len = mstrsize(argp->u.str);
    if (len >= sizeof(buf) - 1)
        errorf("Command too long (size: %zu): '%.200s...'\n", 
               len, get_txt(argp->u.str));
    strncpy(buf, get_txt(argp->u.str), len);
    buf[len+1] = '\0';

    origin = check_object(argp[1].u.ob);
    if (!origin)
        errorf("origin '%s' destructed.\n", get_txt(argp[1].u.ob->name));
    if (!(O_ENABLE_COMMANDS & origin->flags))
        errorf("origin '%s' not a living.\n", get_txt(origin->name));

    player = check_object(argp[2].u.ob);
    if (!player)
        errorf("player '%s' destructed.\n", get_txt(argp[2].u.ob->name));
    if (!(O_ENABLE_COMMANDS & player->flags))
        errorf("player '%s' not a living.\n", get_txt(player->name));

    res = MY_FALSE;  /* default result */

    /* Test if we are allowed to use this function */
    if (privilege_violation4(STR_EXECUTE_COMMAND, origin, argp->u.str, 0, sp))
    {
        marked_command_giver = origin;
        command_giver = player;
        res = parse_command(buf, MY_TRUE);
    }

    /* Clean up the stack and push the result. */
    free_svalue(argp+2);
    free_svalue(argp+1);
    free_svalue(argp);

    put_number(argp, res ? 1 : 0);

    return argp;
} /* f_execute_command() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_match_command(svalue_t * sp)

/* EFUN match_command()
 *
 *   mixed * match_command (string command, object origin)
 *
 * Take the command <command>, parse it, and return an array of all
 * matching actions added to <origin> (read: <origin> is the object
 * 'issuing' the command).
 *
 * Each entry in the result array is itself an array of:
 *
 *   string [CMDM_VERB]:   The matched verb.
 *   string [CMDM_ARG]:    The argument string remaining, or 0 if none.
 *   object [CMDM_OBJECT]: The object defining the action.
 *   mixed  [CMDM_FUN]:    The name of the function to call in CMDM_OBJECT,
 *                         which may be static, or a closure.
 *
 * The efun is useful for both debugging, and for implementing your
 * own H_COMMAND handling.
 *
 * TODO: Export the basic data gathering into a separate function which
 * TODO:: can then be co-used by parse_command(), removing the need
 * TODO:: for a SENT_MARKER. It might also remove the need for copying
 * TODO:: the command buffer.
 */

{
    object_t *origin;
    string_t *cmd;                 /* The given command */
    char     *cmdbuf;              /* The given command text buffer */
    string_t *verb;                /* The verb from the command, tabled */
    char     *p;                   /* handy string pointer */
    sentence_t *s;                 /* handy sentence pointer */
    size_t    cmd_length;          /* length of command w/o trailing spaces */
    size_t    verb_length;         /* length of the verb */
    vector_t *rc;                  /* Result array */

      /* The found matching actions are kept in a list of these
       * structures. References to strings and objects are counted.
       */
    struct cmd_s {
        struct cmd_s *next;
        string_t     *verb;  /* The verb */
        string_t     *arg;   /* The arg string, or NULL */
        svalue_t      fun;   /* The function to call */
        object_t     *ob;    /* The object to call */
        sentence_t   *s;     /* The sentence */
    };
    struct cmd_s *first, *last;
    struct cmd_s *pcmd;
    int           num_cmd;   /* Number of matches found */
    int           i;

    first = last = NULL;

    /* Get the arguments */
    origin = sp->u.ob;
    cmd = sp[-1].u.str;
    cmdbuf = get_txt(cmd);

    /* Strip trailing spaces.
     */
    for (p = cmdbuf + mstrsize(cmd) - 1; p >= cmdbuf; p--)
    {
       if (*p != ' ')
            break;
    }

    if  (p < cmdbuf)
    {
        free_svalue(sp); sp--;
        free_svalue(sp);
        put_array(sp, allocate_array(0));
        return sp;
    }

    cmd_length = p - cmdbuf + 1;

    /* Determine verb length */
    p = strchr(cmdbuf, ' ');
    if (p == NULL)
        verb_length = cmd_length;
    else
        verb_length = (size_t)(p - cmdbuf);

    verb = new_n_tabled(cmdbuf, verb_length);

    /* Scan the list of sentences for the saved command giver
     * and collect the found matches in a list.
     */
    num_cmd = 0;
    for (s = origin->sent; s; s = s->next)
    {
        struct cmd_s * new_cmd;
        action_t *sa;            /* (action_t *)s */
        unsigned char type;      /* s->type */

        sa = (action_t *)s;

        /* Test the current sentence */
        if ((type = s->type) == SENT_PLAIN)
        {
            if (!mstreq(sa->verb, verb))
                continue;
        }
        else if (type == SENT_SHORT_VERB)
        {
            /* The verb may be shortened to a few leading characters,
             * but not shorter than .short_verb.
             */
            size_t len;
            if (sa->short_verb)
            {
                len = mstrsize(verb);
                if (len < sa->short_verb
                 || len > mstrsize(sa->verb)
                 || (   !mstreq(sa->verb, verb)
                     && strncmp(get_txt(sa->verb), get_txt(verb), len) != 0))
                    continue;
            }
            else
            {
                len = mstrsize(sa->verb);
                if (strncmp(cmdbuf, get_txt(sa->verb), len) != 0)
                    continue;
            }
        }
        else if (type == SENT_OLD_NO_SPACE || type == SENT_NO_SPACE)
        {
            /* The arguments may follow the verb without space,
             * that means we just have to check if buff[] begins
             * with sa->verb.
             */
            size_t len;
            len = mstrsize(sa->verb);
            if (strncmp(cmdbuf, get_txt(sa->verb), len) != 0)
                continue;
        }
        else
        {
            /* SENT_MARKER ... due to recursion. Or another SENT_IS_INTERNAL */
            continue;
        }

        /*
         * Now we have found a matching sentence!
         */

        num_cmd++;
        memsafe(new_cmd = alloca(sizeof(*new_cmd)), sizeof(*new_cmd)
               , "temporary buffer");

        new_cmd->next = NULL;
        new_cmd->s = s;
        new_cmd->ob = ref_object(sa->ob, "match_command");
        transfer_svalue_no_free(&(new_cmd->fun), callback_function(&(sa->cb)));

        /* Fill in the verb and arg information of the cmd_s structure.
         */
        if (s->type == SENT_OLD_NO_SPACE)
        {
            new_cmd->verb = ref_mstring(verb);
            if (cmd_length > mstrsize(sa->verb))
            {
                new_cmd->arg = mstr_extract(cmd, mstrsize(sa->verb), cmd_length-1);
            }
            else
            {
                new_cmd->arg = NULL;
            }
        }
        else if (s->type == SENT_NO_SPACE)
        {
            if (cmd_length > mstrsize(sa->verb))
            {
                /* We need to cut off the verb right where the
                 * arguments start.
                 */
                size_t len = mstrsize(sa->verb);

                new_cmd->verb = mstr_extract(cmd, 0, len-1);
                new_cmd->arg = mstr_extract(cmd, len, cmd_length-1);
            }
            else
            {
                new_cmd->verb = ref_mstring(verb);
                new_cmd->arg = NULL;
            }
        }
        else if (cmdbuf[verb_length] == ' ')
        {
            new_cmd->verb = ref_mstring(verb);

            /* Try to find an earlier action which uses the same
             * argument and just reference that one.
             */
            for (pcmd = first; pcmd != NULL; pcmd = pcmd->next)
            {
                if (pcmd->s->type != SENT_OLD_NO_SPACE
                 && pcmd->s->type != SENT_NO_SPACE
                 && pcmd->arg != NULL
                   )
                {
                    new_cmd->arg = ref_mstring(pcmd->arg);
                    break;
                }
            }

            if (pcmd == NULL)
            {
                /* First time this arg is used */
                new_cmd->arg = mstr_extract(cmd, verb_length+1, cmd_length-1);
            }
        }
        else
        {
            new_cmd->verb = ref_mstring(verb);
            new_cmd->arg = NULL;
        }

        /* Insert the command info into the list */
        if (first == NULL)
        {
            first = last = new_cmd;
        }
        else
        {
            last->next = new_cmd;
            last = new_cmd;
        }
    } /* for(sentences) */

    /* We got the matched commands, now transfer the information
     * into the result array.
     */
    rc = allocate_array(num_cmd);
    for ( i = 0, pcmd = first
        ; i < num_cmd && pcmd != NULL
        ; i++, pcmd = pcmd->next)
    {
        vector_t *sub = allocate_array(CMDM_SIZE);

        put_string(&(sub->item[CMDM_VERB]), pcmd->verb);
        if (pcmd->arg)
            put_string(&(sub->item[CMDM_ARG]), pcmd->arg);
        /* else: entry is svalue-0 already */
        transfer_svalue_no_free(&(sub->item[CMDM_FUN]), &(pcmd->fun));
        put_object(&(sub->item[CMDM_OBJECT]), pcmd->ob);

        put_array(&(rc->item[i]), sub);
    }

    /* Clean up */
    free_mstring(verb);
      /* No need to clean up the references from the list as they
       * have been transferred into the result array.
       * And the list itself lives on the stack and is cleaned up
       * automatically.
       */

    /* Put the result onto the stack */
    free_svalue(sp); sp--;
    free_svalue(sp);
    put_array(sp, rc);

    return sp;
} /* f_match_command() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_remove_action (svalue_t *sp)

/* EFUN: remove_action()
 *
 *   int remove_action(string verb, object ob)
 *
 * Remove the first action defined by the current object with command verb
 * <verb> from <ob> (default is this_player()).
 * Return 1 if the action was found and removed, and 0 else.
 *
 *   int remove_action(int flag, object ob)
 *
 * if <flag> is non-0, remove all actions defined by the current object from
 * <ob> (default is this_player()).
 * Return the number of actions removed.
 */

{
    object_t    *ob, *shadow_ob;
    string_t    *verb;
    sentence_t **sentp;
    action_t    *s;
    int          rc;

    /* Get and test the arguments */

    ob = sp->u.ob;

    verb = NULL;
    if (sp[-1].type == T_STRING)
    {
        verb = find_tabled(sp[-1].u.str);
        if (!verb)
            verb = (string_t *)f_remove_action; /* won't be found */
    }
    else if (sp[-1].type == T_NUMBER)
    {
        if (sp[-1].u.number != 0)
            verb = NULL; /* finds all */
        else
            verb = (string_t *)f_remove_action; /* won't be found */
    }
    else
    {
        efun_gen_arg_error(1, sp[-1].type, sp);
        /* NOTREACHED */
    }
    
    rc = 0;
    sentp = &ob->sent;

    ob = current_object;
    shadow_ob = NULL;
    
    /* Look for the underlying object, just as add_action does. */
    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
    {
        object_t *shadowing;

        shadow_ob = ob;

        while ((ob->flags & O_SHADOW)
         && NULL != (shadowing = O_GET_SHADOW(ob)->shadowing))
        {
            ob = shadowing;
        }
    }

    /* Now search and remove the sentence */
    while ( NULL != (s = (action_t *)*sentp) )
    {
        if (!SENT_IS_INTERNAL((*sentp)->type) && s->ob == ob 
	 && (!verb || s->verb == verb)
	 && (!shadow_ob || s->shadow_ob == shadow_ob))
        {
#ifdef CHECK_OBJECT_REF
            if (sentp == &ob->sent)
                update_object_sent(ob, s->sent.next);
            else
                *sentp = s->sent.next;
#else
            *sentp = s->sent.next;
#endif /* CHECK_OBJECT_REF */
            free_action_sent(s);
            rc++;
            if (verb != NULL)
                break;
        }
        else
        {
            sentp = &s->sent.next;
        }
    }

    /* Clean up the stack and push the result */
    free_object_svalue(sp);
    sp--;
    free_svalue(sp);

    put_number(sp, rc);

    return sp;
} /* f_remove_action() */

/*-------------------------------------------------------------------------*/
static vector_t *
e_get_action (object_t *ob, string_t *verb)

/* EFUN query_actions()
 *
 *   mixed *query_actions (object ob, string verb)
 *
 * Return information about the <verb> attached to <ob>ject, or 0 if
 * there is no such verb.
 *
 * See f_query_actions() for a long explanation.
 */

{
    vector_t *v;
    sentence_t *s;
    svalue_t *p;

    if ( !(verb = find_tabled(verb)) )
        return NULL;

    for (s = ob->sent; s; s = s->next)
    {
        action_t *sa;

        if (SENT_IS_INTERNAL(s->type))
            continue;

        sa = (action_t *)s;

        if (verb != sa->verb)
            continue;
        /* verb will be 0 for SENT_MARKER */

        v = allocate_array(4);
        p = v->item;

        put_number(p, s->type);
        p++;
        put_number(p, s->type != SENT_PLAIN ? sa->short_verb : 0);
        p++;
        put_ref_object(p, sa->ob, "get_action");
        p++;
        transfer_svalue_no_free(p, callback_function(&(sa->cb)));

        return v;
    }
    /* not found */
    return NULL;
} /* e_get_action() */

/*-------------------------------------------------------------------------*/
static vector_t *
e_get_all_actions (object_t *ob, int mask)

/* EFUN query_actions()
 *
 *   mixed *query_actions (object ob, int mask)
 *
 * Return information about all verbs attached to <ob>ject which match
 * the <mask>.
 *
 * See f_query_actions() for a long explanation.
 */

{
    vector_t *v;
    sentence_t *s;
    int num;
    svalue_t *p;
    int nqueries;

    /* Set nqueries to the number of set bit in mask */
    nqueries = ((mask>>1) & 0x55) + (mask & 0x55);
    nqueries = ((nqueries>>2) & 0x33) + (nqueries & 0x33);
    nqueries = ((nqueries>>4) & 0x0f) + (nqueries & 0x0f);
    num = 0;

    /* Count the number of actions */
    for (s = ob->sent; s; s = s->next)
    {
        if (SENT_IS_INTERNAL(s->type))
            continue;
        num += nqueries;
    }

    /* Allocate and fill the result array */
    v = allocate_array(num);
    p = v->item;
    for (s = ob->sent; s; s = s->next)
    {
        action_t * sa;

        if (SENT_IS_INTERNAL(s->type))
            continue;

        sa = (action_t *)s;

        if (mask & QA_VERB)
        {
            string_t * str;
            if ( NULL != (str = sa->verb) ) {
                put_ref_string(p, str);
            }
            p++;
        }
        if (mask & QA_TYPE)
        {
            p->u.number = s->type;
            p++;
        }
        if (mask & QA_SHORT_VERB)
        {
            p->u.number = sa->short_verb;
            p++;
        }
        if (mask & QA_OBJECT)
        {
            put_ref_object(p, sa->ob, "get_action");
            p++;
        }
        if (mask & QA_FUNCTION)
        {
            transfer_svalue_no_free(p, callback_function(&(sa->cb)));
            p++;
        }
    }

    /* Done */
    return v;
} /* e_get_all_actions() */

/*-------------------------------------------------------------------------*/
static vector_t *
e_get_object_actions (object_t *ob1, object_t *ob2)

/* EFUN query_actions()
 *
 *   mixed *query_actions (object ob, object from)
 *
 * Return information about all verbs attached to <ob>ject which are
 * defined by object <from>.
 *
 * See f_query_actions() for a long explanation.
 */

{
    vector_t *v;
    sentence_t *s;
    int num;
    svalue_t *p;

    /* Count the number of actions */
    num = 0;
    for (s = ob1->sent; s; s = s->next)
        if (!SENT_IS_INTERNAL(s->type) && ((action_t *)s)->ob == ob2)
            num += 2;

    /* Allocate and fill in the result array */
    v = allocate_array(num);
    p = v->item;
    for (s = ob1->sent; s; s = s->next)
    {
        action_t *sa;

        if (SENT_IS_INTERNAL(s->type))
            continue;

        sa = (action_t *)s;

        if (sa->ob == ob2) {
            put_ref_string(p, sa->verb);
            p++;

            transfer_svalue_no_free(p, callback_function(&(sa->cb)));
            p++;
        }
    }

    /* Return the result */
    return v;
} /* e_get_object_actions() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_actions (svalue_t *sp)

/* EFUN query_actions()
 *
 *   mixed *query_actions(object ob, mixed mask_or_verb)
 *
 * query_actions takes either an object or a filename as first
 * argument and a bitmask (int) or string as a second argument.
 * If the second argument is a string, query_actions() will return
 * an array containing information (see below) on the verb or
 * zero if the living object "ob" cannot use the verb. If the
 * second argument is a bitmask, query_actions() will return a
 * flat array containing information on all verbs added to ob.
 * The second argument is optional (default is QA_VERB).
 *     QA_VERB       ( 1):  the verb
 *     QA_TYPE       ( 2):  type
 *     QA_SHORT_VERB ( 4):  short_verb
 *     QA_OBJECT     ( 8):  object
 *     QA_FUNCTION   (16): function
 *
 * "type" is one of the values defined in <sent.h> (/sys/sent.h)
 * (which is provided with the parser source).
 *
 * SENT_PLAIN       added with add_action (fun, cmd);
 * SENT_SHORT_VERB  added with add_action (fun, cmd, 1);
 * SENT_NO_SPACE    added with add_action (fun); add_xverb (cmd);
 * SENT_MARKER      internal, won't be in the returned array
 */

{
    vector_t *v;
    svalue_t *arg;
    object_t *ob;

    arg = sp - 1;

    /* Get the arguments */
    if (arg[0].type == T_OBJECT)
        ob = arg[0].u.ob;
    else
    {
        if (arg->type != T_STRING)
            efun_arg_error(1, T_STRING, arg->type, sp);
        ob = get_object(arg[0].u.str);
        if (!ob)
            errorf("query_actions() failed\n");
    }

    /* Get the actions */
    if (arg[1].type == T_STRING)
        v = e_get_action(ob, arg[1].u.str);
    else if (arg[1].type == T_NUMBER)
        v = e_get_all_actions(ob, arg[1].u.number);
    else {
        if (arg[1].type != T_OBJECT)
            efun_arg_error(2, T_OBJECT, arg[1].type, sp);
        v = e_get_object_actions(ob, arg[1].u.ob);
    }

    /* Clean up the stack and push the result */
    free_svalue(sp--);
    free_svalue(sp);
    if (v)
    {
        put_array(sp, v);
    } else
        put_number(sp, 0);

    return sp;
} /* f_query_actions() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_disable_commands (svalue_t *sp)

/* EFUN disable_commands()
 *
 *   void disable_commands()
 *
 * Disable this object to use commands normally accessible to
 * users.
 */

{
    if (current_object->flags & O_DESTRUCTED)
        return sp;

    if (d_flag > 1) {
        debug_message("%s Disable commands %s (ref %"PRIdPINT")\n"
                     , time_stamp(), get_txt(current_object->name)
                     , current_object->ref);
    }

    current_object->flags &= ~O_ENABLE_COMMANDS;
    command_giver = NULL;

    return sp;
} /* f_disable_commands() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_enable_commands (svalue_t *sp)

/* EFUN enable_commands()
 *
 *   void enable_commands()
 *
 * Enable this object to use commands normally accessible to
 * users.
 */

{
    interactive_t *ip;

    if (current_object->flags & O_DESTRUCTED)
        return sp;

    if (d_flag > 1) {
        debug_message("%s Enable commands %s (ref %"PRIdPINT")\n"
                     , time_stamp(), get_txt(current_object->name)
                     , current_object->ref);
    }

    current_object->flags |= O_ENABLE_COMMANDS;
    command_giver = current_object;
    if (O_SET_INTERACTIVE(ip, command_giver))
    {
        trace_level |= ip->trace_level;
    }

    return sp;
} /* f_enable_commands() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_notify_fail (svalue_t *sp)

/* EFUN notify_fail()
 *
 *   int notify_fail(string str)
 *   int notify_fail(closure cl)
 *
 * Store str as the error message given instead of the default
 * message ``What ?''. The result is always 0.
 *
 * If a closure is given, it is executed to return the error
 * message string, but not before all attempts to execute the
 * commandline failed (read: not at the time of the call to
 * notify_fail()).
 *
 * If notify_fail() is called more than once, only the last call
 * will be used.
 */

{
    if (command_giver && !(command_giver->flags & O_DESTRUCTED))
    {
        transfer_svalue(&error_msg, sp);
        if (error_obj)
            free_object(error_obj, "notify_fail");
        error_obj = ref_object(current_object, "notify_fail");
    }
    else
        free_svalue(sp);

    put_number(sp, 0);

    return sp;
} /* f_notify_fail() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_verb (svalue_t *sp)

/* EFUN query_verb()
 *
 *   string query_verb(void)
 *   string query_verb(int flag)
 *
 * Return the verb of the current command, of 0 if not executing from
 * a command. If <flag> is 0 or not given, the verb as given by the user
 * is returned; if <flag> is non-0, the verb as specified in the
 * add_action() statement is returned.
 *
 * This efun allows add_action() of several commands
 * to the same function. query_verb() returns 0 when invoked by a
 * function which was started by a call_out or the heart beat.
 * Also when a user logs in query_verb() returns 0.
 */

{
    p_int flag = sp->u.number;
    free_svalue(sp);
    if (flag == 0)
    {
        if (!last_verb)
            put_number(sp, 0);
        else
            put_ref_string(sp, last_verb);
    }
    else
    {
        if (!last_action_verb)
            put_number(sp, 0);
        else
            put_ref_string(sp, last_action_verb);
    }

    return sp;
} /* f_query_verb() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_command (svalue_t *sp)

/* EFUN query_command()
 *
 *   string query_command(void)
 *
 * Return the full command string, or 0 if not executing from
 * a command.
 *
 * The string returned is the string as seen by the parser:
 * after any modify_command handling and after stripping
 * trailing spaces.
 */

{
    if (!last_command)
        push_number(sp, 0);
    else
        push_c_string(sp, last_command);

    return sp;
} /* f_query_command() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_notify_fail (svalue_t *sp)

/* EFUN query_notify_fail()
 *
 *   mixed query_notify_fail()
 *   mixed query_notify_fail(int flag = 0)
 *
 * If no flag is given, or flag is 0: return the last error message
 * resp. closure set with notify_fail().
 * If flag is non-zero, return the object which executed the last notify_fail().
 *
 * If nothing was set yet, return 0.
 */

{
    p_int flag;

    flag = sp->u.number;
    free_svalue(sp);

    if (flag)
    {
        if (error_obj && !(error_obj->flags & O_DESTRUCTED))
            put_ref_object(sp, error_obj, "query_notify_fail");
        else
            put_number(sp, 0);
    }
    else
    {
        if (error_msg.type == T_STRING || error_msg.type == T_CLOSURE)
            assign_svalue_no_free(sp, &error_msg);
        else
            put_number(sp, 0);
    }

    return sp;
} /* f_query_notify_fail() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_modify_command (svalue_t *sp)

/* EFUN set_modify_command()
 *
 *   object set_modify_command(object)
 *   object set_modify_command(string)
 *   object set_modify_command(int)
 *
 * All commands for the current object (that must obviously be interactive)
 * will be passed to ob->modify_command() before actually being executed. The
 * argument can be passed an object or a file_name.
 *
 * When set_modify_command() was called, the parser won't expand the standard
 * abbreviations n,e,s,w,nw,sw,ne,se for that user anymore, nor use any hook
 * set for this.
 *
 * 0 as argument will stop the command modification and reinstall
 *   the standard abbreviations.
 * -1 as argument will just return the object previously set.
 *
 * The return value is the object that was previously set with
 * set_modify_command(), if any.
 */

{
    object_t *old, *new;
    interactive_t *ip;

    inter_sp = sp;

    /* Make sure the current_object is interactive */

    if (!(O_SET_INTERACTIVE(ip, current_object))
     || ip->closing)
    {
        errorf("set_modify_command in non-interactive object\n");
    }

    /* Get the old setting */
    old = ip->modify_command;
    if (old && old->flags & O_DESTRUCTED)
    {
        free_object(old, "set_modify_command");
        old = NULL;
        ip->modify_command = NULL;
    }

    /* Set the new setting */
    new = sp->u.ob;
    switch(sp->type)
    {
    default:
        efun_gen_arg_error(1, sp->type, sp);
        break;

    case T_STRING:
        new = get_object(sp->u.str);
        if (!new)
            errorf("Object '%s' not found.\n", get_txt(sp->u.str));
        /* FALLTHROUGH */

    case T_OBJECT:
        ip->modify_command = ref_object(new, "set_modify_command");
        break;

    case T_NUMBER:
        if (sp->u.number == 0 )
        {
            /* ref count of old is reused below, so don't free now */
            ip->modify_command = NULL;
        }
        else
        {
            if (sp->u.number != -1)
                errorf("Bad num arg 1 to set_modify_command(): got %"PRIdPINT
                       ", expected 0 or -1\n", sp->u.number);
            if (old) ref_object(old, "set_modify_command");
        }
    }

    free_svalue(sp);

    /* Return the old setting */
    if (old)
        put_object(sp, old); /* reuse ref count */
    else
        put_number(sp, 0);

    return sp;
} /* f_set_modify_command() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_this_player (svalue_t *sp)

/* EFUN set_this_player()
 *
 *   void set_this_player(object ob)
 *
 * Change the current command giver to <ob>. <ob> may be 0 if you want to
 * 'deactivate' the current command giver.
 *
 * This efun is not privileged, therefore it should be redefined by a nomask
 * simul_efun which then either completely disables the efun or at least
 * performs some security checks.  It is easy to undermine a mudlibs security
 * using this efun.
 */

{
    object_t *ob;
    interactive_t *ip;

    /* Special case, can happen if a function tries to restore
     * an old this_player() setting which happens to be 0.
     */
    if (sp->type == T_NUMBER && !sp->u.number)
    {
        command_giver = NULL;
        return sp - 1;
    }

    if (sp->type != T_OBJECT)
        efun_arg_error(1, T_OBJECT, sp->type, sp);

    ob = sp->u.ob;
    command_giver = ob;
    if (O_SET_INTERACTIVE(ip, ob))
    {
        trace_level |= ip->trace_level;
    }
    free_object(ob, "set_this_player");
    return sp - 1;
} /* f_set_this_player() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_command_stack_depth (svalue_t *sp)

/* EFUN command_stack_depth()
 *
 *   int command_stack_depth(void)
 *
 * Return the depth of the command stack, that is the number of nested
 * commands.
 */

{
    rt_context_t * context;
    int num;

    /* Simply count the number of COMMAND_CONTEXT entries on
     * the context stack.
     */
    for (num = 0, context = rt_context; context; context = context->last)
        if (context->type == COMMAND_CONTEXT)
            num++;

    push_number(sp, num);

    return sp;
} /* f_command_stack_depth() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_command_stack (svalue_t *sp)

/* EFUN command_stack()
 *
 *   mixed * command_stack(void)
 *
 * Return an array describing the current command stack. The array has
 * command_stack_depth() entries, the first describing the top-level
 * command, and the last describing the current one.
 *
 * Each entry is an array itself with these entries:
 *
 *   string [CMD_VERB]:    the verb of this command
 *   string [CMD_TEXT]:    the full command text
 *   object [CMD_ORIGIN]:  the original command giver
 *   object [CMD_PLAYER]:  the current command giver
 *   mixed  [CMD_FAIL]:    the notify_fail setting
 *   mixed  [CMD_FAILOBJ]: the object which set the notify_fail
 */

{
    rt_context_t * context;
    vector_t * result;
    svalue_t * entry;
    int num, i;

    /* Count the number of COMMAND_CONTEXT entries on
     * the context stack.
     */
    for (num = 0, context = rt_context; context; context = context->last)
        if (context->type == COMMAND_CONTEXT)
            num++;

    /* Get the array */
    result = allocate_uninit_array(num);
    if (!result)
        errorf("(command_stack) Out of memory: array[%d] for result.\n", num);

    for ( i = num-1, entry = result->item + num - 1, context = rt_context
        ; i >= 0
        ; i--, entry--
        )
    {
        vector_t * sub;
        svalue_t * svp;
        string_t * t_verb;    /* current verb */
        char     * t_cmd;     /* current command */
        object_t * t_player, * t_mplayer; /* current command givers */
        svalue_t * t_errmsg;  /* current error message */
        object_t * t_errobj;  /* current error message giver */

        /* Create the entry array */
        sub = allocate_array(CMD_SIZE);
        if (!sub)
            errorf("(command_stack) Out of memory: array[%d] for entry.\n"
                 , CMD_SIZE);

        put_array(entry, sub);

        svp = sub->item;

        if (i == num-1)
        {
            /* Get the active environment */
            t_verb = last_verb;
            t_cmd = last_command;
            t_player = check_object(command_giver);
            t_mplayer = check_object(marked_command_giver);
            t_errmsg = &error_msg;
            t_errobj = check_object(error_obj);
        }
        else
        {
            struct command_context_s *cmd;

            /* Find the next command context */
            while (context->type != COMMAND_CONTEXT)
                context = context->last;
            cmd = (struct command_context_s *)context;
            context = context->last;

            t_verb = cmd->verb;
            t_cmd = cmd->cmd;
            t_player = check_object(cmd->this_player);
            t_mplayer = check_object(cmd->mark_player);
            t_errmsg = &(cmd->errmsg);
            t_errobj = check_object(cmd->errobj);
        }

        /* Now put the data into the array */
        if (t_verb)
            put_ref_string(svp+CMD_VERB, t_verb);

        if (t_cmd)
            put_c_string(svp+CMD_TEXT, t_cmd);

        if (t_mplayer)
            put_ref_object(svp+CMD_ORIGIN, t_mplayer, "command_stack");

        if (t_player)
            put_ref_object(svp+CMD_PLAYER, t_player, "command_stack");

        if (t_errmsg->type == T_STRING || t_errmsg->type == T_CLOSURE)
            assign_svalue_no_free(svp+CMD_FAIL, t_errmsg);

        if (t_errobj)
            put_ref_object(svp+CMD_FAILOBJ, t_errobj, "command_stack");

    }

    /* Put the result onto the stack */
    push_array(sp, result);

    return sp;
} /* f_command_stack() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_living (svalue_t *sp)

/* EFUN living()
 *
 * int living(object ob)
 *
 * Return true if ob is a living object (that is,
 * enable_commands() has been called from inside the ob).
 * ob may be 0, in which case the result is obviously 0, too.
 */

{
    int i;

    if (sp->type == T_NUMBER && !sp->u.number)
        return sp;

    if (sp->type != T_OBJECT)
    {
        efun_arg_error(1, T_OBJECT, sp->type, sp);
        return sp;
    }

    i = (sp->u.ob->flags & O_ENABLE_COMMANDS) != 0;
    free_object_svalue(sp);
    put_number(sp, i);
    return sp;
} /* f_living() */

/***************************************************************************/

