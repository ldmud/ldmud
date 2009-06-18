#ifndef SENT_H__
#define SENT_H__ 1

/*---------------------------------------------------------------------------
 * Dynamic data structures used to annotate objects.
 *
 *---------------------------------------------------------------------------
 * This include file defines two types of annotation structures: actions
 * and shadow structures. A third type, interactive structures, which are
 * a special case of shadow structures, is defined in comm.h. All annotations
 * structures are derived from the same sentence base structure.
 *
 * Sentences annotate an object with information about actions (verbs
 * and functions), and shadows, and network connection data ('interactive
 * sentences'). In fact, the shadow sentence serves as anchor point for
 * all annotations but actions.
 *
 * The sentences are kept in a single-linked list, originating at
 * object->sent. Within this list, there can be only one shadow
 * sentence, and it is always placed at the head of the list. This very
 * instance forms the head of an independent double-linked list should there
 * be more than one shadow for the object.
 *
 * The different types of sentences are distinguished by their '.type'
 * member. The following types are defined:
 *
 *    SENT_PLAIN, SENT_SHORT_VERB, SENT_NO_SPACE
 *        The sentence is of type 'action_t' and holds information
 *        about an action (verb + function) available to one object.
 *
 *    SENT_MARKER
 *        A special case of 'action_t' sentence which is used to
 *        control the search for a command.
 *
 *    SENT_SHADOW:
 *        The sentence is of type 'shadow_sentence' and describes
 *        an object shadow.
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

enum sent_type_e {
    SENT_PLAIN = 0     /* Normal action */
 ,  SENT_SHORT_VERB    /* Action with abbreviatable verb */
 ,  SENT_OLD_NO_SPACE  /* Action with embedded verb (old style) */
 ,  SENT_NO_SPACE      /* Action with embedded verb */
 ,  SENT_MARKER        /* Internal: marker for a command search */
 ,  SENT_SHADOW        /* Internal: shadow data */
};

typedef enum sent_type_e sent_type_t;

#define SENT_IS_INTERNAL(x) ((x) >= SENT_MARKER)

/* --- struct sentence_s: the basic sentence structure ---
 */

struct sentence_s
{
    sentence_t * next;  /* Next sentence in the list */
    sent_type_t  type;  /* Type of this sentence */
};


/* --- struct shadow_s: the action sentence structure ---
 *
 * Main purpose of the shadow sentence is to link together the shadowing
 * object with the shadowee. Multi-level shadows are this way (indirectly
 * through the object and object->sent pointers) kept in a double-linked
 * list.
 *
 * Additionally the shadow sentence is used to hold additionally information
 * used by the object for short time. Such information is the interactive_t
 * for interactive objects.
 */
struct shadow_s
{
    sentence_t sent;         /* The basic sentence */
    object_t *shadowing;     /* "prev": the shadowed object */
    object_t *shadowed_by;   /* "next": the shadowing object */

    interactive_t *ip;       /* the information for interactive objects */
};

/* --- Macros --- */

#define O_GET_SHADOW(ob)      ((shadow_t *)(ob)->sent)
#define O_GET_INTERACTIVE(ob) (O_GET_SHADOW(ob)->ip)

  /* Expand to an expression suitable to query or set the
   * indicated attribute. No checks are performed.
   */

/* --- Prototypes --- */

/* In actions.c: */
extern void remove_action_sent(object_t *ob, object_t *player);
extern void remove_environment_sent(object_t *player);

/* In simulate.c */
extern void check_shadow_sent (object_t *ob);
extern void assert_shadow_sent (object_t *ob);

#endif /* SENT_H__ */
