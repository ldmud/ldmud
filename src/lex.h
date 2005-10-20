#ifndef __LEX_H__
#define __LEX_H__ 1

#include "driver.h"
#include "interpret.h"  /* struct vector, struct svalue */

/* --- Types --- */

struct lpc_predef_s {
    char *flag;
    struct lpc_predef_s *next;
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

#define I_TYPE_UNKNOWN	0
#define I_TYPE_GLOBAL	2 /* function, variable AND efuns/simul_efuns */
#define I_TYPE_LOCAL	3
#define I_TYPE_RESWORD	4
#define I_TYPE_DEFINE	5

struct ident {
    char *name;
    short type;
    short hash;
    struct ident *next; /* next in hash chain */
    struct ident *inferior;
    union {
        struct defn define;
        int code;
        struct {
            short function, variable, efun, sim_efun;
        } global;
        int local;
    } u;
    struct ident *next_all;
};

#define lookup_predef(p) (p->type == I_TYPE_GLOBAL ? p->u.global.efun : -1)

/* --- Variables --- */
extern struct lpc_predef_s * lpc_predefs;
extern int current_line;
extern int total_lines;
extern char *current_file;
extern int pragma_strict_types;
extern int pragma_save_types;
extern int pragma_combine_strings;
extern int pragma_verbose_errors;
extern char *last_lex_string;
extern struct ident *all_efuns;

/* --- Prototypes --- */

extern struct ident *make_shared_identifier PROT((char *, int));
extern void free_shared_identifier PROT((struct ident*));
extern int yylex PROT((void));
extern void end_new_file PROT((void));
extern void lex_close PROT((char *msg));
extern void start_new_file PROT((int fd));
extern void init_num_args PROT((void));
extern char *get_f_name PROT((int n));
extern void free_defines PROT((void));
extern void set_inc_list PROT((struct vector *v));
extern void clear_auto_include_string PROT((void));
extern struct svalue *f_set_auto_include_string PROT((struct svalue *sp));
extern void remove_unknown_identifier PROT((void));
extern char *lex_error_context PROT((void));
extern struct svalue *f_expand_define PROT((struct svalue *sp));

#ifdef MALLOC_smalloc
extern void count_lex_refs PROT((void));
#endif /* MALLOC_smalloc */

#endif /* __LEX_H__ */
