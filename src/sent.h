#ifndef __SENT_H__
#define __SENT_H__ 1

/*---------------------------------------------------------------------------
 * Dynamic data structures used to annotate objects.
 *
 *---------------------------------------------------------------------------
 * This include file defines two types of annotation structures: sentences
 * and shadow structures. A third type, interactive structures, which are
 * a special case of shadow structures, is defined in comm.h
 * TODO: the remaining prototypes of this file could go into
 * TODO:: simulate.h
 *
 *
 * Sentences annotate an object with information about actions (verbs
 * and functions), shadows, and network connection data ('interactive
 * sentences'). In fact, interactive sentences (defined in comm.h) are
 * just a special case of shadow sentences .
 *
 * The sentences are kept in a single-linked list, originating at
 * object->sent. Within this list, there can be only one shadow/interactive
 * sentence, and it is always placed at the head of the list. This very
 * instance forms the head of an independent double-linked list should there
 * be more than one shadow for the object.
 *
 * This means that the first sentence for an interactive object, shadows
 * or not, is always an interactive sentence.
 *
 * The different types of sentences are distinguished by their '.type'
 * member. The following types are defined:
 *
 *    SENT_SHADOW:
 *        The sentence is of type 'shadow_sentence' and describes
 *        an object shadow.
 *
 *    SENT_INTERACTIVE:
 *        The sentence is of type 'interactive' and holds the network
 *        connection data.
 * TODO: Doc the other sentence types.
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#define SENT_PLAIN          0
#define SENT_SHORT_VERB     1
#define SENT_NO_SPACE       2
#define SENT_NO_VERB        3
#define SENT_IS_INTERNAL(x) ((x) >= SENT_MARKER)
#define SENT_MARKER         4
#define SENT_SHADOW         5
#define SENT_INTERACTIVE    6

/* TODO: Document the struct sentence members */
struct sentence {
    char *verb;
    struct object *ob;
    char *function;
    struct sentence *next;
    unsigned short short_verb; /* Only leading characters count */
    unsigned char type;
};

/* TODO: Document the struct shadow_sentence members */
struct shadow_sentence {
    struct object *shadowing;
    struct ed_buffer *ed_buffer;
    struct object *shadowed_by;
    struct sentence *next;
    unsigned short dummy;
    unsigned char type;
};

/* TODO: sentence and shadow_sentence can be merged in one structure */

#define O_GET_SHADOW(ob) ((struct shadow_sentence *)(ob)->sent)
#define O_GET_INTERACTIVE(ob) ((struct interactive *)(ob)->sent)

/* --- Prototypes (in simulate.c) --- */

extern void free_shadow_sent(struct shadow_sentence *);
extern struct sentence *alloc_sentence(void);


#endif /* __SENT_H__ */
