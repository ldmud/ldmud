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
extern void strbuf_add(strbuf_t *buf, const char * text);
extern void strbuf_addn(strbuf_t *buf, const char * text, size_t len);
extern void strbuf_addc(strbuf_t *buf, const char ch);
extern void strbuf_addf(strbuf_t *buf, const char *format, ...)
                           FORMATDEBUG(printf,2,3);
extern void strbuf_send(strbuf_t *buf);
extern void strbuf_store(strbuf_t *buf, svalue_t *svp);
extern void strbuf_copy (strbuf_t *buf, char *cbuf);
#define strbuf_length(sbuf) ((sbuf)->length)
extern string_t * trim_all_spaces (const string_t * txt);
extern char * xstrncpy(char * dest, const char * src, size_t num);

#ifdef HAS_ICONV
extern svalue_t * f_convert_charset (svalue_t *sp); 
#endif
extern string_t * intersect_strings (const string_t * left, const string_t * right, Bool bSubtract);
extern svalue_t * x_map_string (svalue_t *sp, int num_arg);
extern svalue_t * x_filter_string (svalue_t *sp, int num_arg);

#endif /* STRFUNS_H_ */
