#ifndef __LEX_H__
#define __LEX_H__ 1

#include "driver.h"
#include "interpret.h"  /* struct vector, struct svalue */

/* --- Types --- */

/* --- struct lpc_predef_s: predefined preprocessor macros ---
 *
 * The structures are used in a list to store macro definitions
 * given to the driver on the commandline.
 */

struct lpc_predef_s {
    char  * flag;
      /* The raw text of the definition in the form 'NAME' or 'NAME=<value>'
       * in its own allocated memory.
       */
    struct lpc_predef_s *next;
      /* Next predefinition in the list, or NULL.
       */
};

struct defn {
    union {
        char *str;
        char *(*fun)();
    } exps;
    short nargs;
    char permanent;
    char special;
};

/* --- struct ident: known identifiers ---
 *
 * The structure is used to keep the information about all identifiers
 * encountered so far (including reserved words, efuns, etc).
 *
 * There can several entries for the same identifier name but with
 * different types in the table. These entries are put into a list formed
 * by their .inferior pointers, sorted by falling type values. The entry
 * with the highest type is the one linked into the hash chain.
 *
 * The identifiers are stored in a table of chains, hashed over their names.
 * The most recently identifier always brought to the head of its hash-chain.
 *
 * Additionally, all efuns and defines are stored in their
 * own 'all_...' lists, linked by the '.next_all' field.
 */

struct ident
{
    char *name;              /* Name of the identifier (shared string) */ 
    short type;              /* Type of this entry */
    short hash;              /* Hashvalue of this identifier */
    struct ident *next;      /* Next in hash chain */
    struct ident *inferior;  /* Ident of same name, but lower type */
    union {                  /* Type-depend data: */
        struct defn define;
        int code;            /* Reserved word: lexem code */
        struct {             /*   Global identifier: */
            short function; /* TODO: efun: -2 */
            short variable; /* TODO: efun: -2 */
            short efun;      /*     efun: Index in instrs[] */
            short sim_efun;  /* TODO: efun: -1 */
        } global;
        int local;
    } u;
    struct ident *next_all;  /* 'all_...' list link */
};

/* struct ident.type values: */

#define I_TYPE_UNKNOWN    0
#define I_TYPE_GLOBAL     2  /* function, variable or efuns/simul_efuns */
#define I_TYPE_LOCAL      3
#define I_TYPE_RESWORD    4  /* reserved word */
#define I_TYPE_DEFINE     5

#define lookup_predef(p) (p->type == I_TYPE_GLOBAL ? p->u.global.efun : -1)

/* --- Variables --- */

extern struct lpc_predef_s * lpc_predefs;
extern int current_line;
extern int total_lines;
extern char *current_file;
extern /* TODO: BOOL */ int pragma_strict_types;
extern /* TODO: BOOL */ int pragma_save_types;
extern /* TODO: BOOL */ int pragma_combine_strings;
extern /* TODO: BOOL */ int pragma_verbose_errors;
extern char *last_lex_string;
extern struct ident *all_efuns;

/* --- Prototypes --- */

extern void init_lexer(void);
extern struct ident *make_shared_identifier(char *, int);
extern void free_shared_identifier(struct ident*);
extern int yylex(void);
extern void end_new_file(void);
extern void lex_close(char *msg);
extern void start_new_file(int fd);
extern char *get_f_name(int n);
extern void free_defines(void);
extern void set_inc_list(struct vector *v);
extern void clear_auto_include_string(void);
extern struct svalue *f_set_auto_include_string(struct svalue *sp);
extern void remove_unknown_identifier(void);
extern char *lex_error_context(void);
extern struct svalue *f_expand_define(struct svalue *sp);

#ifdef MALLOC_smalloc
extern void count_lex_refs(void);
#endif /* MALLOC_smalloc */

#endif /* __LEX_H__ */
