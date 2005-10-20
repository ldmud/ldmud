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
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include <stddef.h>

#define USES_SVALUE_STRLEN
#include "actions.h"
#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "dumpstat.h"
#include "efuns.h"
#include "exec.h"
#include "interpret.h"
#include "mapping.h"
#include "object.h"
#include "sent.h"
#include "simulate.h"
#include "smalloc.h"
#include "stralloc.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "../mudlib/sys/commands.h"
#include "../mudlib/sys/driver_hook.h"

/*-------------------------------------------------------------------------*/

#define COMMAND_FOR_OBJECT_BUFSIZE 1000
  /* The maximum length of a command.
   */

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
    char * verb;             /* the shared verb */
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

char *last_verb = NULL;
  /* During a command execution, this is the shared string with the
   * command verb.
   */

char *last_command = NULL;
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

static sentence_t * free_sent = NULL;
  /* List of allocated but unused action sentences.
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
        free_string(last_verb);
    last_verb = NULL;

} /* free_action_temporaries() */

/*-------------------------------------------------------------------------*/
static INLINE action_t *
new_action_sent(void)

/* Allocate a new empty action sentence and return it.
 */

{
    action_t *p;

    if (free_sent == NULL)
    {
        xallocate(p, sizeof *p, "new action sentence");
        alloc_action_sent++;
    }
    else
    {
        p = (action_t *)free_sent;
        free_sent = free_sent->next;
    }
    p->verb = NULL;
    p->function = NULL;
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

    if (p->function)
        free_string(p->function);
    if (p->verb)
        free_string(p->verb);
    p->sent.next = free_sent;
    free_sent = (sentence_t *)p;
} /* _free_action_sent() */

void free_action_sent (action_t *p)
  {  _free_action_sent(p); }

#define free_action_sent(p) _free_action_sent(p)

/*-------------------------------------------------------------------------*/
void
purge_action_sent(void)

/* Actually deallocate all action sentences held in the free list.
 * Called during a GC and shutdown.
 */

{
    sentence_t *p;
    
    for (;free_sent; free_sent = p) {
        p = free_sent->next;
        xfree(free_sent);
        alloc_action_sent--;
    }
} /* purge_action_sent() */

/*-------------------------------------------------------------------------*/
static INLINE void
save_command_context (struct command_context_s * context)

/* Save the current command context into <context> (but don't put it
 * onto the context stack). The saved global variables are zeroed out.
 */

{
    context->rt.type = COMMAND_CONTEXT;
    context->verb = last_verb;
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
        free_string(last_verb);

    if (command_marker)
        free_action_sent(command_marker);
    else if (marked_command_giver && !(O_DESTRUCTED & marked_command_giver->flags))
        remove_action_sent((object_t *)context, marked_command_giver);

    /* Restore the previous context */
    last_verb = context->verb;
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
                if (tmp->function && tmp->verb)
                    debug_message("%s --Unlinking sentence fun='%s', verb='%s'\n"
                                 , time_stamp(), tmp->function, tmp->verb);
                else if (tmp->function)
                    debug_message("%s --Unlinking sentence fun='%s', verb=0\n"
                                 , time_stamp(), tmp->function);
                else if (tmp->verb)
                    debug_message("%s --Unlinking sentence fun=0, verb='%s'\n"
                                 , time_stamp(), tmp->verb);
                else 
                    debug_message("%s --Unlinking sentence fun=0, verb=0\n"
                                 , time_stamp());
            }
#endif
            *s = tmp->sent.next;
            free_action_sent(tmp);
        }
        else
            s = &((*s)->next);
    }
} /* remove_action_sent() */

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
                    debug_message("%s --Unlinking sentence %s\n"
                                 , time_stamp(), s->function);
#endif
                tmp = s;
                s = (action_t *)s->sent.next;
                free_action_sent(tmp);
                if (!s) {
                    *p = NULL;
                    return;
                }
            } while (s->ob == ob);
            *p = (sentence_t *)s;
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
        else if (closure_hook[H_MODIFY_COMMAND_FNAME].type == T_STRING)
        {
            push_volatile_string(buff);
            svp = sapply(closure_hook[H_MODIFY_COMMAND_FNAME].u.string, ob, 1);
            /* !command_giver means that the command_giver has been destructed. */
            if (!command_giver)
                return MY_TRUE;
        }
    }
    else
    {
        if (closure_hook[H_MODIFY_COMMAND].type == T_CLOSURE)
        {
            lambda_t *l;

            l = closure_hook[H_MODIFY_COMMAND].u.lambda;
            if (closure_hook[H_MODIFY_COMMAND].x.closure_type == CLOSURE_LAMBDA)
                l->ob = command_giver;
            push_volatile_string(buff);
            push_object(command_giver);
            call_lambda(&closure_hook[H_MODIFY_COMMAND], 2);
            transfer_svalue(svp = &apply_return_value, inter_sp--);
            if (!command_giver)
                return MY_TRUE;
        }
        else if (closure_hook[H_MODIFY_COMMAND].type == T_STRING
            && !(O_DESTRUCTED & command_giver->flags))
        {
            push_volatile_string(buff);
            svp =
              sapply(closure_hook[H_MODIFY_COMMAND].u.string, command_giver, 1);
            if (!command_giver)
                return MY_TRUE;
        }
        else if (closure_hook[H_MODIFY_COMMAND].type == T_MAPPING)
        {
            svalue_t sv;
            char * str;

            if ( NULL != (str = findstring(buff)) )
            {
                put_string(&sv, str);
                svp =
                  get_map_value(closure_hook[H_MODIFY_COMMAND].u.map, &sv);
                if (svp->type == T_CLOSURE)
                {
                    push_shared_string(sv.u.string);
                    push_object(command_giver);
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
        if (svp->type == T_STRING) {
            strncpy(buff, svp->u.string, COMMAND_FOR_OBJECT_BUFSIZE-1);
            buff[COMMAND_FOR_OBJECT_BUFSIZE-1] = '\0';
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
    if (strcmp(buff, "malloc") == 0)
    {
        strbuf_t sbuf;

        status_parse(&sbuf, "malloc");
        strbuf_send(&sbuf);
        return 1;
    }

#ifdef O_IS_WIZARD
    if (!is_wizard_used || command_giver->flags & O_IS_WIZARD)
#endif
    {
        Bool no_curobj = MY_FALSE;

        if (strcmp(buff, "dumpallobj") == 0) {

            if (!current_object)
            {
                current_object = ref_object(command_giver, "dumpallobj");
                no_curobj = MY_TRUE;
            }
            add_message("Dumping to /OBJ_DUMP ... ");
            dumpstat("/OBJ_DUMP");
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
            opcdump("/OPC_DUMP");
            if (no_curobj)
            {
                free_object(current_object, "opcdump");
                current_object = NULL;
            }
            return 1;
        }
#endif
        if (strncmp(buff, "status", 6) == 0)
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

    svp = &error_msg;

    if (svp->type == T_STRING)
    {
        tell_object(command_giver, svp->u.string);
    }
    else if (svp->type == T_CLOSURE)
    {
        push_valid_ob(save_command_giver);
        call_lambda(svp, 1);
        /* add_message might cause an error, thus, we free the closure first. */
        if (inter_sp->type == T_STRING)
            tell_object(command_giver, inter_sp->u.string);
        pop_stack();
    }
    else if (closure_hook[H_NOTIFY_FAIL].type == T_STRING)
    {
        tell_object(command_giver, closure_hook[H_NOTIFY_FAIL].u.string);
    }
    else if (closure_hook[H_NOTIFY_FAIL].type == T_CLOSURE)
    {
        if (closure_hook[H_NOTIFY_FAIL].x.closure_type == CLOSURE_LAMBDA)
            closure_hook[H_NOTIFY_FAIL].u.lambda->ob = command_giver;
        push_volatile_string(command);
        push_valid_ob(save_command_giver);
        call_lambda(&closure_hook[H_NOTIFY_FAIL], 2);
        if (inter_sp->type == T_STRING)
            tell_object(command_giver, inter_sp->u.string);
        pop_stack();
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
 * The command_giver and marked_command_giver are set, last_verb
 * and last_command may be set (and will be overwritten).
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
                     , command_giver->name, buff);
#endif

    /* Strip trailing spaces.
     * Afterwards, p will point to the last non-space character.
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
        free_string(last_verb);

    length = p - buff;
    p = strchr(buff, ' ');
    if (p == NULL)
    {
        length += 1;
        last_verb = make_shared_string(buff);
    }
    else
    {
        *p = '\0';
        last_verb = make_shared_string(buff);
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
                len = strlen(last_verb);
                if (len < sa->short_verb
                 || len > strlen(sa->verb)
                 || (   sa->verb != last_verb
                     && strncmp(sa->verb, last_verb, len) != 0))
                    continue;
            }
            else
            {
                len = strlen(sa->verb);
                if (strncmp(buff, sa->verb, len) != 0)
                    continue;
            }
        }
        else if (type == SENT_NO_SPACE)
        {
            /* The arguments may follow the verb without space,
             * that means we just have to check if buff[] begins
             * with sa->verb.
             */
            size_t len;
            len = strlen(sa->verb);
            if (strncmp(buff, sa->verb, len) != 0)
                continue;
        }
        else if (type == SENT_NO_VERB)
        {
            /* TODO: Without add_(x)verb(), this case can go, too. */
            /* Give an error only the first time we scan this sentence */
            if (sa->short_verb)
                continue;
            sa->short_verb++;
            error("An 'action' had an undefined verb.\n");
        }
        else
        {
            /* SENT_MARKER ... due to recursion. Or another SENT_IS_INTERNAL */
            continue;
        }

        /*
         * Now we have found a special sentence!
         */

#ifdef DEBUG
        if (d_flag > 1)
            debug_message("%s Local command %s on %s\n", time_stamp()
                         , sa->function, sa->ob->name);
#endif

        /* If the function is static and not defined by current object,
         * then it will fail. If this is called directly from player input,
         * then we set current_object so that static functions are allowed.
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
        marker_sent->function = NULL;

        /* Push the argument and call the command function.
         *
         * For NO_SPACE commands it would be logical to cut off the
         * actual verb part from the first word and add it to the arguments,
         * but this would break all existing mudlibs.
         */
        if (s->type == SENT_NO_SPACE)
        {
            if (strlen(buff) > strlen(sa->verb))
            {
                push_volatile_string(&buff[strlen(sa->verb)]);
                ret = sapply(sa->function, sa->ob, 1);
            }
            else
            {
                ret = sapply(sa->function, sa->ob, 0);
            }
        }
        else if (buff[length] == ' ')
        {
            push_volatile_string(&buff[length+1]);
            ret = sapply(sa->function, sa->ob, 1);
        }
        else
        {
            ret = sapply(sa->function, sa->ob, 0);
        }

        if (ret == 0)
        {
            error("function %s not found.\n", sa->function);
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
        if (ret->type == T_NUMBER && ret->u.number == 0) {
            continue;
        }

        /* Command was found */
        if (O_IS_INTERACTIVE(command_giver)
#ifdef O_IS_WIZARD
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
    marker_sent->function = NULL;
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
 * commands this is called from command_for_object() (ie. the F_COMMAND efun).
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
    rt_context = (rt_context_t *)&context;

    /* Default settings */
    command_giver = ob;
    marked_command_giver = ob;
    last_command = str;

    /* Execute the command */
    if (closure_hook[H_COMMAND].type == T_STRING)
    {
        svalue_t *svp;

        push_volatile_string(str);
        svp = sapply_int(closure_hook[H_COMMAND].u.string, ob, 1, MY_TRUE);
        res = (svp->type != T_NUMBER) || (svp->u.number != 0);
    }
    else if (closure_hook[H_COMMAND].type == T_CLOSURE)
    {
        lambda_t *l;

        l = closure_hook[H_COMMAND].u.lambda;
        if (closure_hook[H_COMMAND].x.closure_type == CLOSURE_LAMBDA)
            l->ob = ob;
        push_volatile_string(str);
        push_object(ob);
        call_lambda(&closure_hook[H_COMMAND], 2);
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
int
e_command (char *str, object_t *ob)

/* EFUN command()
 *
 * Execute command <str> for object <ob> (if not given the current object
 * is used). Return 0 on failure, otherwise the execution cost.
 *
 * This function is the frontend of the command parser for the efun command().
 */

{
    char buff[COMMAND_FOR_OBJECT_BUFSIZE];
    int save_eval_cost = eval_cost - 1000;
    interactive_t *ip;

    if (ob == NULL)
        ob = current_object;

    if (current_object->flags & O_DESTRUCTED || ob->flags & O_DESTRUCTED)
        return 0;

    /* Make a copy of the given command as the parser might change it */

    if (strlen(str) > sizeof(buff) - 1)
        error("Too long command.\n");
    strncpy(buff, str, sizeof buff);
    buff[sizeof buff - 1] = '\0';

    if (O_SET_INTERACTIVE(ip, ob))
        trace_level |= ip->trace_level;

    if (execute_command(buff, ob))
        return eval_cost - save_eval_cost;
    else
        return 0;
} /* e_command() */

/*-------------------------------------------------------------------------*/
Bool
e_add_action (svalue_t *func, svalue_t *cmd, int flag)

/* EFUN add_action()
 *
 *   void add_action(string fun, string cmd [, int flag])
 *   void add_action(string fun) // historical
 *
 * Add an action (verb + function) to the commandgiver.
 *
 * This function returns TRUE if an error occured, or FALSE if the
 * action was successfull.
 *
 * Attempting to add an action from a shadow causes a privilege violation
 * ("shadow_add_action", shadow, func).
 *
 * TODO: In the long run, make actions an optional feature.
 */
{
    action_t *p;
    object_t *ob;
    char *str;
    short string_type;

    /* Can't take actions from destructed objects */
    if (current_object->flags & O_DESTRUCTED)
        return MY_TRUE;

    ob = current_object;

    /* Check if the call comes from a shadow of the current object */
    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
    {
        str = findstring(func->u.string);
        do
        {
            ob = O_GET_SHADOW(ob)->shadowing;
            if (find_function(str, ob->prog) >= 0)
            {
                if (!privilege_violation4(
                    "shadow_add_action", ob, str, 0, inter_sp)
                )
                    return MY_TRUE;
            }
        } while(O_GET_SHADOW(ob)->shadowing);
    }

    /* We must have a valid commandgiver to succeed */
    if (command_giver == 0 || (command_giver->flags & O_DESTRUCTED))
        return MY_TRUE;

    /* And the commandgiver must be in the vicinity */
    if (ob != command_giver
     && ob->super != command_giver
     && ob->super != command_giver->super
     && ob != command_giver->super)
        error("add_action from object '%s' that was not present to '%s'.\n"
             , ob->name, command_giver->name);

#ifdef DEBUG
    if (d_flag > 1)
        debug_message("%s --Add action %s\n", time_stamp(), func->u.string);
#endif

    /* Sanity checks */
    if (*func->u.string == ':')
        error("Illegal function name: %s\n", func->u.string);

#ifdef COMPAT_MODE
    str = func->u.string;
    if (*str++=='e' && *str++=='x' && *str++=='i' && *str++=='t' && !*str)
    {
        error("Illegal to define a command to the exit() function.\n");
        /* NOTREACHED */
        return MY_TRUE;
    }
#endif

    /* Allocate and initialise a new sentence */
    p = new_action_sent();

    /* Set str to the function, made shared */
    str = func->u.string;
    if ((string_type = func->x.string_type) != STRING_SHARED)
    {
        char *str2;
        str = make_shared_string(str2 = str);
        if (string_type == STRING_MALLOC)
        {
            xfree(str2);
        }
    }

    p->function = str;
    p->ob = ob;

    if (cmd)
    {
        /* Set str to the command verb, made shared */
        str = cmd->u.string;
        if ((string_type = cmd->x.string_type) != STRING_SHARED)
        {
            char *str2;
            str = make_shared_string(str2 = str);
            if (string_type == STRING_MALLOC)
            {
                xfree(str2);
            }
        }
        p->verb = str;
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
                p->sent.type = SENT_NO_SPACE;
            }
            else if (flag < AA_VERB)
            {
                if (-flag >= strlen(p->verb))
                {
                    error("Bad arg 3 to add_action(): value %ld larger than verb '%s'.\n"
                         , (long)flag, p->verb);
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
                error("Bad arg 3 to add_action(): value %ld too big.\n"
                     , (long)flag);
                /* NOTREACHED */
                return MY_TRUE;
            }
        }
    }
    else
    {
        /* No verb given */
        p->short_verb = 0;
        p->verb = NULL;
        p->sent.type = SENT_NO_VERB;
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
        command_giver->sent = (sentence_t *)p;
    }

    return MY_FALSE;
} /* e_add_action() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_execute_command (svalue_t *sp)

/* TEFUN execute_command()
 *
 *   int execute_command (string command, object origin, object player)
 *
 * Low-level access to the command parser: take the <command>, parse
 * it into verb and argument and call the appropriate action added
 * to <origin>. For the execution of the function(s), this_player()
 * is set to player.
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
    char buf[COMMAND_FOR_OBJECT_BUFSIZE];
    Bool res;

    /* Test and get the arguments from the stack */
    argp = sp - 2;

    if (argp[0].type != T_STRING)
        bad_xefun_arg(1, sp);
    if (argp[1].type != T_OBJECT)
        bad_xefun_arg(2, sp);
    if (argp[2].type != T_OBJECT)
        bad_xefun_arg(3, sp);

    if (svalue_strlen(argp) >= COMMAND_FOR_OBJECT_BUFSIZE)
        error("Command too long.\n");
    strcpy(buf, argp->u.string);

    origin = check_object(argp[1].u.ob);
    if (!origin)
        error("origin '%s' destructed.\n", argp[1].u.ob->name);
    if (!(O_ENABLE_COMMANDS & origin->flags))
        error("origin '%s' not a living.\n", origin->name);

    player = check_object(argp[2].u.ob);
    if (!player)
        error("player '%s' destructed.\n", argp[2].u.ob->name);
    if (!(O_ENABLE_COMMANDS & player->flags))
        error("player '%s' not a living.\n", player->name);

    res = MY_FALSE;  /* default result */

    /* Test if we are allowed to use this function */
    if (privilege_violation4("execute_command", origin, buf, 0, sp))
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
f_remove_action (svalue_t *sp)

/* TEFUN: remove_action()
 *
 *   int remove_action(string verb, object ob)
 *
 * Removes the action for the optional object, default is for
 * this_player().
 * Return 1 if the action was found and removed, and 0 else.
 */

{
    object_t *ob;
    char *verb;
    sentence_t **sentp;
    action_t *s;

    /* Get and test the arguments */
    if (sp[-1].type != T_STRING)
        bad_xefun_arg(1, sp);
    if (sp->type != T_OBJECT)
        bad_xefun_arg(2, sp);

    ob = sp->u.ob;
    verb = sp[-1].u.string;

    if (sp[-1].x.string_type != STRING_SHARED)
        if ( !(verb = findstring(verb)) )
            verb = (char *)f_remove_action; /* won't be found */

    /* Now search and remove the sentence */
    sentp = &ob->sent;
    ob = current_object;
    while ( NULL != (s = (action_t *)*sentp) )
    {
        if (s->ob == ob && s->verb == verb)
        {
            *sentp = s->sent.next;
            free_action_sent(s);
            break;
        }
        sentp = &s->sent.next;
    }

    /* Clean up the stack and push the result */
    free_object_svalue(sp);
    sp--;
    free_string_svalue(sp);

    put_number(sp, s != 0);

    return sp;
} /* f_remove_action() */

/*-------------------------------------------------------------------------*/
vector_t *
e_get_action (object_t *ob, char *verb)

/* EFUN query_actions()
 *
 *   mixed *query_actions (object ob, string verb)
 *
 * Return information about the <verb> attached to <ob>ject, or 0 if
 * there is no such verb.
 *
 * See interpret.c:F_QUERY_ACTIONS for a long explanation.
 */

{
    vector_t *v;
    sentence_t *s;
    svalue_t *p;

    if ( !(verb = findstring(verb)) )
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
        put_ref_string(p, sa->function);

        return v;
    }
    /* not found */
    return NULL;
} /* e_get_action() */

/*-------------------------------------------------------------------------*/
vector_t *
e_get_all_actions (object_t *ob, int mask)

/* EFUN query_actions()
 *
 *   mixed *query_actions (object ob, int mask)
 *
 * Return information about all verbs attached to <ob>ject which match
 * the <mask>.
 *
 * See interpret.c:F_QUERY_ACTIONS for a long explanation.
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
        
        if (mask & 1)
        {
            char * str;
            if ( NULL != (str = sa->verb) ) {
                put_ref_string(p, str);
            }
            p++;
        }
        if (mask & 2)
        {
            p->u.number = s->type;
            p++;
        }
        if (mask & 4)
        {
            p->u.number = sa->short_verb;
            p++;
        }
        if (mask & 8)
        {
            put_ref_object(p, sa->ob, "get_action");
            p++;
        }
        if (mask & 16)
        {
            put_ref_string(p, sa->function);
            p++;
        }
    }

    /* Done */
    return v;
} /* e_get_all_actions() */

/*-------------------------------------------------------------------------*/
vector_t *
e_get_object_actions (object_t *ob1, object_t *ob2)

/* EFUN query_actions()
 *
 *   mixed *query_actions (object ob, object from)
 *
 * Return information about all verbs attached to <ob>ject which are
 * defined by object <from>.
 *
 * See interpret.c:F_QUERY_ACTIONS for a long explanation.
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

            put_ref_string(p, sa->function);
            p++;
        }
    }

    /* Return the result */
    return v;
} /* e_get_object_actions() */

/*-------------------------------------------------------------------------*/
void
enable_commands (Bool num)

/* Enable/disable O_ENABLE_COMMANDS for the current object.
 *
 * Called in context of the efuns enable_commands()/disable_commands().
 */

{
    if (current_object->flags & O_DESTRUCTED)
        return;

    if (d_flag > 1) {
        debug_message("%s Enable commands %s (ref %ld)\n"
                     , time_stamp(), current_object->name
                     , current_object->ref);
    }

    if (num)
    {
        interactive_t *ip;

        current_object->flags |= O_ENABLE_COMMANDS;
        command_giver = current_object;
        if (O_SET_INTERACTIVE(ip, command_giver))
        {
            trace_level |= ip->trace_level;
        }
    }
    else
    {
        current_object->flags &= ~O_ENABLE_COMMANDS;
        command_giver = NULL;
    }
} /* enable_commands() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_notify_fail (svalue_t *sp)

/* TEFUN notify_fail()
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
    if (sp->type != T_STRING && sp->type != T_CLOSURE)
        bad_xefun_arg(1, sp);

    if (command_giver && !(command_giver->flags & O_DESTRUCTED))
    {
        if (error_msg.type == T_CLOSURE)
            /* It might be the closure we're just executing, so
             * keep it around for now.
             * TODO: It'd be safer if the interpreter would keep
             * TODO:: an additional ref on all closures it is executing.
             */
            free_closure_hooks(&error_msg, 1);
        else
            free_svalue(&error_msg);
        transfer_svalue_no_free(&error_msg, sp);
        if (error_obj)
            free_object(error_obj, "notify_fail");
        error_obj = ref_object(current_object, "notify_fail");
    }

    put_number(sp, 0);

    return sp;
} /* f_notify_fail() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_notify_fail (svalue_t *sp)

/* TEFUN query_notify_fail()
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
    
    if (sp->type != T_NUMBER)
        bad_xefun_arg(1, sp);

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
f_set_this_player (svalue_t *sp)

/* TEFUN set_this_player()
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
        bad_xefun_arg(1, sp);

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

/* TEFUN command_stack_depth()
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

    sp++;
    put_number(sp, num);

    return sp;
} /* f_command_stack_depth() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_command_stack (svalue_t *sp)

/* TEFUN command_stack()
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
        error("(command_stack) Out of memory: array[%d] for result.\n", num);

    for ( i = num-1, entry = result->item + num - 1, context = rt_context
        ; i >= 0
        ; i--, entry--
        )
    {
        vector_t * sub;
        svalue_t * svp;
        char * t_verb, * t_cmd; /* current verb and command */
        object_t * t_player, * t_mplayer; /* current command givers */
        svalue_t * t_errmsg;  /* current error message */
        object_t * t_errobj;  /* current error message giver */

        /* Create the entry array */
        sub = allocate_array(CMD_SIZE);
        if (!sub)
            error("(command_stack) Out of memory: array[%d] for entry.\n"
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
            put_malloced_string(svp+CMD_TEXT, string_copy(t_cmd));

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
    sp++;
    put_array(sp, result);

    return sp;
} /* f_command_stack() */

/*-------------------------------------------------------------------------*/
/* I won't comment these. The sooner add_verb()/add_xverb() are gone,
 * the better.
 */

#if defined(F_ADD_VERB) || defined(F_ADD_XVERB)
static svalue_t *add_verb(sp, type)
    svalue_t *sp;
    int type;
{
    if (sp->type != T_STRING)
        bad_xefun_arg(1, sp);
    if (command_giver  && !(command_giver->flags & O_DESTRUCTED)) {
        action_t *sent;

        sent = (action_t *)command_giver->sent;
        if (command_giver->flags & O_SHADOW)
            sent = (action_t *)sent->sent.next;
        if (!sent)
            error("No add_action().\n");
        if (sent->verb != 0)
            error("Tried to set verb again.\n");
        sent->verb = make_shared_string(sp->u.string);
        sent->sent.type = (sent_type_t)type;
        if (d_flag > 1)
            debug_message("%s --Adding verb %s to action %s\n"
                         , time_stamp(), sp->u.string, sent->function);
    }
    free_svalue(sp--);
    return sp;
}
#endif

#ifdef F_ADD_VERB
svalue_t *f_add_verb (svalue_t *sp)
{
    return add_verb(sp, SENT_PLAIN);
}
#endif

#ifdef F_ADD_XVERB
svalue_t *f_add_xverb (svalue_t *sp)
{
    return add_verb(sp, SENT_NO_SPACE);
}
#endif

/***************************************************************************/

