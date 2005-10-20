#ifndef _STRFUNS_H_
#define _STRFUNS_H_ 1

#include "driver.h"

#include <sys/types.h> /* TODO: Does C99 guarantee ushort in here? */

#include "datatypes.h"  /* struct svalue */

/* --- strbuf_t: extendable stringbuffer ---
 *
 * This buffer type allows to construct a string in small steps.
 * The string length is limited to USHRT_MAX - if your mudlib needs
 * more, it should be reprogrammed.
 */

struct strbuf_s
{
    ushort alloc_len;  /* Allocated size */
    ushort length;     /* Used size in the string, excl. trailing \0 */
    char *buf;         /* The string */
};

typedef struct strbuf_s strbuf_t;

#define MAX_STRBUF_LEN (USHRT_MAX-2)  /* Max length of a strbuf text */

extern void strbuf_zero(strbuf_t * buf);
extern void strbuf_free(strbuf_t * buf);
extern void strbuf_add(strbuf_t *buf, char * text);
extern void strbuf_addn(strbuf_t *buf, char * text, size_t len);
extern void strbuf_addc(strbuf_t *buf, char ch);
extern void strbuf_addf(strbuf_t *buf, char *format, ...);
extern void strbuf_send(strbuf_t *buf);
extern void strbuf_store(strbuf_t *buf, struct svalue *svp);

#endif /* _STRFUNS_H_ */
