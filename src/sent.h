#ifndef SENT_H
#define SENT_H

#include "interpret.h"

#define SENT_PLAIN	0
#define SENT_SHORT_VERB	1
#define SENT_NO_SPACE	2
#define SENT_NO_VERB	3
#define SENT_IS_INTERNAL(x) ((x) >= SENT_MARKER)
#define SENT_MARKER	4
#define SENT_SHADOW	5
#define SENT_INTERACTIVE 6

struct sentence {
    char *verb;
    struct object *ob;
    char *function;
    struct sentence *next;
    unsigned short short_verb;	/* Only leading characters count */
    unsigned char type;
};

struct shadow_sentence {
    struct object *shadowing;
    struct ed_buffer *ed_buffer;
    struct object *shadowed_by;
    struct sentence *next;
    unsigned short dummy;
    unsigned char type;
};

#define O_GET_SHADOW(ob) ((struct shadow_sentence *)(ob)->sent)
#define O_GET_INTERACTIVE(ob) ((struct interactive *)(ob)->sent)

struct input_to {
    struct object *ob;
    char *function;
    int num_arg;
    struct svalue arg[1];
};

extern void free_shadow_sent PROT((struct shadow_sentence *));
extern void free_input_to PROT((struct input_to *));
struct sentence *alloc_sentence PROT((void));


#endif /* SENT_H */
