#ifndef LEX_H__
#define LEX_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "hash.h"

/* --- Types --- */

typedef struct source_file_s source_file_t; /* forward */

/* --- struct source_loc_s: location within a source file ---
 *
 * This structure is used to identify the location of lexical elements
 * in the input source. The pointers contained become invalid after
 * a compilation has finished.
 */

typedef struct source_loc_s
{
    source_file_t * file;  /* The source file, if any, or NULL */
    int             line;  /* The source line */
} source_loc_t;


/* --- struct source_file_s: a source file ---
 *
 * This structure is used to describe a source file used in the compilation.
 * The embedded source_loc_t structure references the file this particular
 * source file was included from.
 *
 * Together the structures form a tree, describing the include structure
 * of the compiled program. With the end of the compilation, all contained
 * pointers become invalid.
 *
 * The structures are additionally linked together into a singly linked list
 * in order of allocation; the list is used by the lexer to deallocate the
 * tree.
 */

struct source_file_s
{
    source_file_t * next;    /* next source_file structure, or NULL */
    char          * name;    /* Allocated: the name of the file */
    source_loc_t    parent;  /* the file/line this source was included from;
                              * or NULL if none.
                              */
};


/* --- struct lpc_predef_s: predefined preprocessor macros ---
 *
 * The structures are used in a list to store macro definitions
 * given to the driver on the commandline. The list is evaluated
 * when the lexer is first initialised.
 */

struct lpc_predef_s
{
    char  * flag;
      /* The raw text of the definition in the form 'NAME' or 'NAME=<value>'
       * in its own allocated memory.
       */
    struct lpc_predef_s *next;
      /* Next predefinition in the list, or NULL.
       */
};


/* --- typedef defn_fun: dynamic macro expansion ---
 *
 * Functions of this type are used to provide dynamic macro expansions.
 * When used in a macro definition instead of a static replacement text,
 * they are called at every point of macro use.
 *
 * If the implemented macro takes no argument, NULL is passed
 * in the call and the function has to return a fresh allocation of
 * the replacement text.
 *
 * If the macro takes arguments, they are passed as char** with as
 * many arguments as the definition requires. The macro has to add
 * the replacement text via add_input() and return NULL.
 *
 * TODO: Also, the different handling of the replacement text is ugly.
 */

typedef char * (*defn_fun)(char **);


/* --- struct defn: definition of a macro ---
 *
 * Macros are stored in the ident_table[] as I_TYPE_DEFINEs. Their replacement
 * can be given as literal text, or as the result of a function called every
 * time the macro is used.
 *
 * The function is passed an char*[] with the current macro argument values,
 * terminated with a NULL, and has to return an allocated c-string with
 * the replacement text.
 *
 * The replacement text must not contain comments.
 *
 * Function macros (.nargs >= 0) expect the replacement text to contain
 * special MARKS sequences ('@' followed by another character) to mark
 * the places where the function arguments are to be inserted. The code
 * of the second character minus ('@'+1) gives the number of the argument
 * to insert. The sequence '@@' stands for the literal '@' character.
 *
 * Plain macros (.nargs < 0) take the replacement text as it is.
 */

struct defn
{
    union {            /* The replacement text: */
        char    *str;  /*   given as tabled literal (.special is false) */
        defn_fun fun;  /*   return by fun() (.special is true) */
    } exps;
    short        nargs;       /* Number of arguments, 0 for non-function macros
                               */
    SBool        permanent;   /* true: permanent define */
    SBool        special;     /* true: <fun> returns the replacement text */
    source_loc_t loc;         /* location of the definition,
                               * NULL for predefined macros.
                               */
};


/* --- struct ident_s: known identifiers ---
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

struct ident_s
{
    string_t *name;          /* Name of the identifier (tabled string)
                              * The inferiour structures (if any) and this
                              * structure all share the same reference to
                              * the string. */
    short type;              /* Type of this entry */
    hash16_t hash;           /* Hashvalue of this identifier */
    ident_t *next;           /* Next in hash chain */
    ident_t *inferior;       /* Ident of same name, but lower type */
    union {                  /* Type-depend data: */
        struct defn define;  /*   Macro definition */
        int code;            /*   Reserved word: lexem code */
        struct {             /*   Global identifier: */
            unsigned short function;
              /* lfun: Index number of the lfun in den function table,
               * == I_GLOBAL_FUNCTION_OTHER: Undefined
               */
            unsigned short variable;
              /* variable: Index number in the variable table.
               *           During compilation, virtual variables are offset
               *           by VIRTUAL_VAR_TAG.
               * == I_GLOBAL_VARIABLE_OTHER: lfun/inherited hidden var
               * == I_GLOBAL_VARIABLE_FUN:   efun/sefun
               */
            unsigned short efun;
              /* efun: Index in instrs[]
               * == I_GLOBAL_EFUN_OTHER: gvar/sefun
               */
            unsigned short sim_efun;
              /* simul-efun: Index in simul_efun[], negative else
               * == I_GLOBAL_SEFUN_OTHER: efun/gvar
               */
            unsigned short struct_id;
              /* struct index ('id') in the current program's struct table.
               * == I_GLOBAL_STRUCT_NONE: undefined
               */
#ifdef USE_PYTHON
            unsigned short python_efun;
              /* python-efun: Index into python_efun_table, negative else
               * == I_GLOBAL_PYTHON_EFUN_OTHER: efun/sefun/gvar
               */
#endif
        } global;
        struct {               /*   Local identifier: */
            int num;           /*     Number, also the index on the stack */
            int context;       /*     -1 for normal locals, or the index
                                *       in the context frame. In that case,
                                *       .num is either -1 or the index
                                *       of the related local of the defining
                                *       function.
                                */
            short depth;       /*     Definition depth */
            bool initializing; /*    The initializer for the variable is
                                *    compiled just now.
                                */
        } local;
    } u;
    ident_t *next_all;       /* 'all_...' list link */
};

/* ident_t.type values: */

#define I_TYPE_UNKNOWN    0
#define I_TYPE_GLOBAL     2  /* function, variable or efuns/simul_efuns */
#define I_TYPE_LOCAL      3
#define I_TYPE_RESWORD    4  /* reserved word */
#define I_TYPE_DEFINE     5

/* ident_t.global magic values */

#define I_GLOBAL_FUNCTION_OTHER  (USHRT_MAX)
#define I_GLOBAL_VARIABLE_OTHER  (USHRT_MAX)
#define I_GLOBAL_VARIABLE_FUN    (USHRT_MAX - 1)
#define I_GLOBAL_EFUN_OTHER      (USHRT_MAX)
#define I_GLOBAL_SEFUN_OTHER     (USHRT_MAX)
#define I_GLOBAL_STRUCT_NONE     (USHRT_MAX)
#ifdef USE_PYTHON
#  define I_GLOBAL_PYTHON_EFUN_OTHER (USHRT_MAX)
#endif


#define lookup_predef(p) (((p)->type == I_TYPE_GLOBAL && (p)->u.global.efun != I_GLOBAL_EFUN_OTHER) ? (p)->u.global.efun : -1)


/* Values of pragma_strict_types */
typedef enum pragma_cttype_checks
{
    PRAGMA_WEAK_TYPES    = 0,
    PRAGMA_STRONG_TYPES  = 1,
    PRAGMA_STRICT_TYPES  = 2,
} pragma_cttype_checks_e;

/* --- Variables --- */

extern struct lpc_predef_s * lpc_predefs;
extern int total_lines;
extern source_loc_t current_loc;
extern pragma_cttype_checks_e pragma_strict_types;
extern Bool pragma_save_types;
extern Bool pragma_no_clone;
extern Bool pragma_no_inherit;
extern Bool pragma_no_shadow;
extern Bool pragma_pedantic;
extern Bool pragma_range_check;
extern Bool pragma_warn_missing_return;
extern Bool pragma_warn_dead_code;
extern Bool pragma_warn_deprecated;
extern Bool pragma_warn_empty_casts;
extern Bool pragma_check_overloads;
extern Bool pragma_share_variables;
extern Bool pragma_rtt_checks;
extern Bool pragma_warn_rtt_checks;
extern bool pragma_no_bytes_type;
extern bool pragma_warn_unused_variables;
extern string_t *last_lex_string;
extern ident_t *all_efuns;


/* Function name overrides. */
enum efun_override_e
{
    OVERRIDE_NONE  = 0,
    OVERRIDE_EFUN  = 1,
    OVERRIDE_SEFUN = 2,
    OVERRIDE_LFUN  = 3,
    OVERRIDE_VAR   = 4,
};
typedef enum efun_override_e efun_override_t;

/* --- Prototypes --- */

extern void init_lexer(void);
extern int  symbol_operator(const char *symbol, const char **endp);
extern void symbol_efun_str(const char *str, size_t len, svalue_t *sp, efun_override_t is_efun, bool privileged);
extern void symbol_efun(string_t *name, svalue_t *sp);
extern void init_global_identifier (ident_t * ident, Bool bVariable);
extern ident_t *lookfor_shared_identifier(const char *s, size_t len, int n, int depth, bool bCreate, bool bExactDepth);
#define make_shared_identifier(s,n,d) lookfor_shared_identifier(s,strlen(s),n,d, true, false)
#define find_shared_identifier(s,n,d) lookfor_shared_identifier(s,strlen(s),n,d, false, false)
#define make_shared_identifier_n(s,l,n,d) lookfor_shared_identifier(s,l,n,d, true, false)
#define find_shared_identifier_n(s,l,n,d) lookfor_shared_identifier(s,l,n,d, false, false)
#define make_shared_identifier_mstr(s,n,d) lookfor_shared_identifier(get_txt(s),mstrsize(s),n,d, true, false)
#define find_shared_identifier_mstr(s,n,d) lookfor_shared_identifier(get_txt(s),mstrsize(s),n,d, false, false)
#define insert_shared_identifier_mstr(s,n,d) lookfor_shared_identifier(get_txt(s),mstrsize(s),n,d, true, true)
extern ident_t *make_global_identifier(char *, int);
extern void free_shared_identifier(ident_t*);
extern int yylex(void);
extern void end_new_file(void);
extern void lex_close(char *msg);
extern void start_new_file(int fd, const char * fname);
extern char *get_f_name(int n);
extern void free_defines(void);
extern size_t show_lexer_status (strbuf_t * sbuf, Bool verbose);
extern void set_inc_list(vector_t *v);
extern void remove_unknown_identifier(void);
extern char *lex_error_context(void);
extern svalue_t *f_expand_define(svalue_t *sp);
extern char * lex_parse_number (char * cp, unsigned long * p_num, Bool * p_overflow);
extern void * get_include_handle (void);

#ifdef GC_SUPPORT
extern void count_lex_refs(void);
#endif /* GC_SUPPORT */

#endif /* LEX_H__ */
