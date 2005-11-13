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
 *
 * TODO: The frames should have special flags to mark stuff like catches,
 * TODO:: sefun closures, closures, etc.
 */

struct control_stack {
    struct object *ob;          /* Current object */
    struct object *prev_ob;     /* Save previous object */
    struct program *prog;       /* Current program, NULL in the bottom entry */
    bytecode_p pc;              /* Program counter, points to next bytecode */
    struct svalue *fp;          /* Frame pointer: first arg on stack */
    bytecode_p funstart;        /* Start of the function code */
    int num_local_variables;    /* Number of local vars + arguments */
    int function_index_offset;
      /* Index of current program's function block within the functions of the
       * current objects program (needed for inheritance).
       */
    struct svalue *current_variables;        /* Same */
    int   extern_call;
      /* TRUE if the call came from outside the object (call_others to
       * oneself are a special case of this). Only entries with this flag
       * set save the .ob and .prev_ob for the flagged and all previous
       * unflagged entries.
       * If the current this_object was changed, the 'imposter' object is
       * stored in .pretend_to_be and this flag is or'ed with CS_PRETEND.
       */
#   define CS_PRETEND 0x80

#if 0  /* TODO: Remove me fully if nobody complains */
    short dummy; /* TODO: ??? */
#endif

    bytecode_p *break_sp; 
      /* Points to address to branch to at next F_BREAK, which is also
       * the actual bottom of the break stack.
       */
    struct object *pretend_to_be;
      /* After set_this_object(), the this_object imposter.
       */
};

/* --- Macros --- */

#define MAX_SHIFT ((sizeof(p_int) << 3) - 1)
  /* The maximally useful shift (left or right) of a number in LPC.
   */

/* Evaluator stack macros
 * TODO: Make these comprehensive. See MudOS for good realisation.
 */

#define put_number(n)  sp->type = T_NUMBER, sp->u.number = n

/* --- Variables --- */

extern struct program *current_prog;
extern int tracedepth;
extern int trace_level;
#ifdef SMALLOC_LPC_TRACE
extern bytecode_p inter_pc;
#endif
extern struct svalue *inter_sp;
extern int function_index_offset;
extern struct svalue *current_variables;
extern struct svalue apply_return_value;
extern struct svalue catch_value;
extern struct svalue last_indexing_protector;

#ifdef APPLY_CACHE_STAT
extern p_int apply_cache_hit;
extern p_int apply_cache_miss;
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

extern void put_object(struct object *ob, struct svalue *sp);
extern void push_object(struct object *ob);
extern void push_valid_ob(struct object *ob);
extern void push_number(p_int n);
extern void push_shared_string(char *p);
extern void push_referenced_shared_string(char *p);
extern void push_svalue(struct svalue *v);
extern void push_svalue_block(int num, struct svalue *v);
extern struct svalue *pop_n_elems (int n, struct svalue *sp);
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
extern Bool _privilege_violation(char *what, struct svalue *where, struct svalue *sp);
extern Bool privilege_violation4(char *what, struct object *whom, char *how_str, int how_num, struct svalue *sp);
#if defined(SUPPLY_PARSE_COMMAND) && !defined(COMPAT_MODE)
extern void check_for_destr(struct vector *v);
#endif
extern void push_apply_value(void);
extern void pop_apply_value (void);
extern struct svalue *sapply_int(char *fun, struct object *ob, int num_arg, Bool b_ign_prot);
#define sapply(f,o,n) sapply_int(f,o,n, MY_FALSE)
extern struct svalue *apply(char *fun, struct object *ob, int num_arg);
extern char *function_exists(char *fun, struct object *ob);
extern void call_function(struct program *progp, int fx);
extern int get_line_number(bytecode_p p, struct program *progp, char **namep);
extern char *dump_trace(Bool how);
extern int get_line_number_if_any(char **name);
extern void reset_machine(Bool first);
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
extern int last_instructions(int length, Bool verbose, struct svalue **svpp);
extern struct svalue *f_last_instructions(struct svalue *sp);
extern struct svalue *f_extract_lvalue(struct svalue *sp);
extern struct svalue *f_trace(struct svalue *sp);
extern struct svalue *f_traceprefix(struct svalue *sp);

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
