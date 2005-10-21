#ifndef STRFUNS_H_
#define STRFUNS_H_ 1

#include "driver.h"
#include "typedefs.h" 

#include <sys/types.h> /* TODO: Does C99 guarantee u_long in here? */

/* --- strbuf_t: extendable stringbuffer ---
 *
 * This buffer type allows to construct a string in small steps.
 */

struct strbuf_s
{
    u_long alloc_len;   /* Allocated size */
    u_long length;      /* Used size in the string, excl. trailing \0 */
    char *buf;          /* The string */
};

#define MAX_STRBUF_LEN (ULONG_MAX-2)  /* Max length of a strbuf text */

extern void strbuf_zero(strbuf_t * buf);
extern void strbuf_free(strbuf_t * buf);
extern void strbuf_add(strbuf_t *buf, char * text);
extern void strbuf_addn(strbuf_t *buf, char * text, size_t len);
extern void strbuf_addc(strbuf_t *buf, char ch);
extern void strbuf_addf(strbuf_t *buf, char *format, ...);
extern void strbuf_send(strbuf_t *buf);
extern void strbuf_store(strbuf_t *buf, svalue_t *svp);
extern char * intersect_strings (char * left, char * right, Bool bSubtract);
extern char * xstrncpy(char * dest, const char * src, size_t num);

#endif /* STRFUNS_H_ */
