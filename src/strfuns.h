#ifndef STRFUNS_H_
#define STRFUNS_H_ 1

#include "driver.h"
#include "iconv_opt.h"
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
extern size_t get_escaped_character(p_int c, char* buf, size_t buflen);

extern size_t byte_to_char_index(const char* text, size_t pos, bool* error);
extern size_t char_to_byte_index(const char* text, size_t len, size_t pos, bool* error);
extern bool is_ascii(const char* text, size_t len);
extern size_t utf8_size (p_int code);
extern char* utf8_prev (char* text, size_t pos);
extern size_t unicode_to_utf8(p_int code, char* buf);
extern size_t utf8_to_unicode(const char* buf, size_t len, p_int *code);

extern char* get_illegal_sequence(char* buf, size_t len, iconv_t cd);

extern size_t next_grapheme_break(const char* str, size_t len, int* width) __attribute__((nonnull(1,3)));
extern int get_string_width(const char* str, size_t len, bool* error) __attribute__((nonnull(1)));
extern size_t get_string_up_to_width(const char* str, size_t len, int width, bool* error) __attribute__((nonnull(1)));

extern svalue_t * v_to_bytes(svalue_t *sp, int num_arg);
extern svalue_t * v_to_text(svalue_t *sp, int num_arg);
extern string_t * intersect_strings (string_t * left, string_t * right, Bool bSubtract);
extern svalue_t * x_map_string (svalue_t *sp, int num_arg);
extern svalue_t * x_filter_string (svalue_t *sp, int num_arg);

#endif /* STRFUNS_H_ */
