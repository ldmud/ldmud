#ifndef __INTERPRET_H__
#define __INTERPRET_H__ 1

#include <setjmp.h>

#include "driver.h"
#include "datatypes.h"
#include "instrs.h"

/* Some functions are inlined in interpret.c, and called normally
 * from everywhere else.
 */
#if defined(HAS_INLINE) && defined(INTERPRET)
#define INTER_INLINE LOCAL_INLINE
#else
#define INTER_INLINE extern
#endif

/* --- Types --- */

/* --- struct control_stack: one control stack element
 *
 * Every structure describes the previous function call levels, the
 * current function call data is kept in interpret's global variables..
 * 'prog' is usually the same as ob->prog, except when
 * executing inherited functions.
 */

struct control_stack {
    struct object *ob;          /* Current object */
    struct object *prev_ob;     /* Save previous object */
    struct program *prog;       /* Current program */
    /* TODO: opcode */ char *pc;                   /* Program counter, points to next bytecode */
    struct svalue *fp;          /* Frame pointer: first arg on stack */
    char *funstart;             /* Start of the function code */
    int num_local_variables;    /* Number of local vars + arguments */
    int function_index_offset;  /* TODO: ??? Used when executing functions in inherited
                                   programs */
    struct svalue *current_variables;        /* Same */
    /* TODO: BOOL */ short extern_call;  /* Flag if evaluator should return */
    short dummy; /* TODO: ??? */
    char **break_sp; /* Points to address to branch to at next F_BREAK. */
    struct object *pretend_to_be; /* TODO: ??? */
};

/* TODO: LI_xxx: ??? */
#define LI_MAXOFFSET        0x3c
#define LI_INCLUDE        0x3d
#define LI_INCLUDE_END        0x3e
#define LI_L_RELOCATED        0x3f

#define LI_RELOCATED        0xc0
#define LI_SMALL_REL        0x20

#define LI_MAXEMPTY        0x20

/* TODO: MAX_SHIFT: ??? */
#define MAX_SHIFT ((sizeof(p_int) << 3) - 1)


/* --- Variables --- */

extern struct program *current_prog;
extern int tracedepth;
extern int trace_level;
#ifdef SMALLOC_LPC_TRACE
extern /* TODO: opcode */ char *inter_pc;
#endif
extern struct svalue *inter_sp;
extern int function_index_offset;
extern struct svalue *current_variables;
extern struct svalue apply_return_value;
extern struct svalue catch_value;
extern struct svalue last_indexing_protector;

#ifdef APPLY_CACHE_STAT
extern int apply_cache_hit;
extern int apply_cache_miss;
#endif

/* --- Prototypes --- */

extern void assign_eval_cost(void);

INTER_INLINE void free_string_svalue(struct svalue *v);
extern void free_object_svalue(struct svalue *v);
extern void zero_object_svalue(struct svalue *v);
extern void free_svalue(struct svalue *v);
INTER_INLINE void assign_svalue_no_free(struct svalue *to, struct svalue *from);
extern void assign_svalue(struct svalue *dest, struct svalue *v);
INTER_INLINE void transfer_svalue_no_free(struct svalue *dest, struct svalue *v);
extern void transfer_svalue(struct svalue *dest, struct svalue *v);

extern void push_object(struct object *ob);
extern void push_valid_ob(struct object *ob);
extern void push_number(p_int n);
extern void push_shared_string(char *p);
extern void push_referenced_shared_string(char *p);
extern void push_svalue(struct svalue *v);
extern void push_svalue_block(int num, struct svalue *v);
INTER_INLINE void pop_stack(void);
extern void drop_stack(void);
extern void push_vector(struct vector *v);
extern void push_referenced_vector(struct vector *v);
extern void push_string_malloced(char *p);
extern void push_string_shared(char *p);
INTER_INLINE void push_malloced_string(char *p);
extern void push_volatile_string(char *p);

extern void init_interpret(void);
extern void bad_efun_arg(int arg, int instr, struct svalue *sp) NORETURN;
extern void bad_xefun_arg(int arg, struct svalue *sp) NORETURN;
extern void bad_xefun_vararg(int arg, struct svalue *sp) NORETURN;
extern int _privilege_violation(char *what, struct svalue *where, struct svalue *sp);
extern int privilege_violation4(char *what, struct object *whom, char *how_str, int how_num, struct svalue *sp);
extern void check_for_destr(struct vector *v);
extern void push_apply_value(void);
extern void pop_apply_value (void);
extern struct svalue *sapply_int(char *fun, struct object *ob, int num_arg, short /* TODO: BOOL */ b_ign_prot);
#define sapply(f,o,n) sapply_int(f,o,n, MY_FALSE)
extern struct svalue *apply(char *fun, struct object *ob, int num_arg);
extern char *function_exists(char *fun, struct object *ob);
extern void call_function(struct program *progp, int fx);
extern void free_old_driver_hooks(void);
extern int get_line_number(char *p, struct program *progp, char **namep);
extern char *dump_trace(int how);
extern int get_line_number_if_any(char **name);
extern void reset_machine(int first);
extern struct svalue *secure_apply(char *fun, struct object *ob, int num_arg);
extern struct svalue *apply_master_ob(char *fun, int num_arg);
extern void assert_master_ob_loaded(void);
extern struct svalue *secure_call_lambda(struct svalue *closure, int num_arg);
extern void remove_object_from_stack(struct object *ob);
extern void call_lambda(struct svalue *lsvp, int num_arg);
extern void free_interpreter_temporaries(void);
extern void invalidate_apply_low_cache(void);
extern void add_eval_cost(int num);
extern void push_referenced_mapping(struct mapping *m);
extern void m_indices_filter (struct svalue* key, struct svalue* data, void *extra);
int last_instructions(int length, int verbose, struct svalue **svpp);
struct svalue *f_last_instructions(struct svalue *sp);
struct svalue *f_set_modify_command(struct svalue *sp);
struct svalue *f_set_prompt(struct svalue *sp);
struct svalue *f_extract_lvalue(struct svalue *sp);
struct svalue *f_transpose_array(struct svalue *sp);
struct svalue *f_trace(struct svalue *sp);
struct svalue *f_traceprefix(struct svalue *sp);
struct svalue *f_set_is_wizard(struct svalue *sp);
struct svalue *f_to_object(struct svalue *sp);
#ifdef F_DEBUG_INFO
struct svalue *f_debug_info(struct svalue *sp, int num_arg);
#endif

#ifdef OPCPROF
extern void opcdump(void);
#endif

#ifdef TRACE_CODE
extern int last_instructions(int length, int verbose, struct svalue **svpp);
#endif

#ifdef DEBUG
extern int check_state(void);
extern void count_inherits(struct program *progp);
extern void count_extra_ref_in_object(struct object *ob);
extern void count_extra_ref_in_vector(struct svalue *svp, mp_int num);
extern void check_a_lot_ref_counts(struct program *search_prog);
#endif

#ifdef MALLOC_smalloc
extern void clear_interpreter_refs(void);
extern void count_interpreter_refs(void);
#endif

#define bad_efun_vararg           bad_xefun_arg
#define push_constant_string(str) push_volatile_string(str)

#endif /* __INTERPRET_H__ */
