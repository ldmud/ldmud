#ifndef INTERPRET_H__
#define INTERPRET_H__ 1

#include <setjmp.h>

#include "driver.h"
#include "typedefs.h"
#include "instrs.h"

/* --- Types --- */

/* --- Macros --- */

#define MAX_SHIFT ((sizeof(p_int) << 3) - 1)
  /* The maximally useful shift (left or right) of a number in LPC.
   */

/* Reset the evaluation cost/time counter.
 */
#define CLEAR_EVAL_COST (assigned_eval_cost = eval_cost = 0)


/* --- Variables --- */

extern program_t *current_prog;
extern int tracedepth;
extern int trace_level;
#ifdef MALLOC_LPC_TRACE
extern bytecode_p inter_pc;
#endif
extern svalue_t *inter_sp;
extern int function_index_offset;
extern svalue_t *current_variables;
extern int32  eval_cost;
extern int32  assigned_eval_cost;
extern svalue_t apply_return_value;
extern svalue_t catch_value;
extern svalue_t last_indexing_protector;

#ifdef APPLY_CACHE_STAT
extern p_int apply_cache_hit;
extern p_int apply_cache_miss;
#endif

/* --- Prototypes --- */

extern void assign_eval_cost(void);

extern void free_string_svalue(svalue_t *v);
extern void free_object_svalue(svalue_t *v);
extern void zero_object_svalue(svalue_t *v);
extern void free_svalue(svalue_t *v);
extern void assign_svalue_no_free(svalue_t *to, svalue_t *from);
extern void assign_svalue(svalue_t *dest, svalue_t *v);
extern void transfer_svalue_no_free(svalue_t *dest, svalue_t *v);
extern void transfer_svalue(svalue_t *dest, svalue_t *v);

extern void put_c_string (svalue_t *sp, const char *p);
extern void push_c_string (svalue_t *sp, const char *p);

#if 0
extern void push_referenced_shared_string(char *p);
extern void push_string_malloced(char *p);
extern void push_string_shared(char *p);
#endif

extern void push_svalue(svalue_t *v);
extern void push_svalue_block(int num, svalue_t *v);
extern svalue_t *pop_n_elems (int n, svalue_t *sp);
extern void pop_stack(void);

extern void init_interpret(void);
extern const char *typename(int type);
extern const char *efun_arg_typename (long type);
extern void vefun_bad_arg (int arg, svalue_t *sp) NORETURN;
extern void efun_gen_arg_error (int arg, int got, svalue_t *sp) NORETURN;
extern void vefun_gen_arg_error (int arg, int got, svalue_t *sp) NORETURN;
extern void efun_arg_error (int arg, int expected, int got, svalue_t *sp) NORETURN;
extern void efun_exp_arg_error (int arg, long expected, int got, svalue_t *sp) NORETURN;
extern void vefun_arg_error (int arg, int expected, int got, svalue_t *sp) NORETURN;
extern void vefun_exp_arg_error (int arg, long expected, int got, svalue_t *sp) NORETURN;
extern Bool _privilege_violation(const string_t *what, svalue_t *where, svalue_t *sp);
extern Bool privilege_violation4(const string_t *what, object_t *whom, const string_t *how_str, int how_num, svalue_t *sp);
extern void push_apply_value(void);
extern void pop_apply_value (void);
extern svalue_t *sapply_int(string_t *fun, object_t *ob, int num_arg, Bool b_ign_prot);
#define sapply(f,o,n) sapply_int(f,o,n, MY_FALSE)
extern svalue_t *apply(string_t *fun, object_t *ob, int num_arg);
extern void call_function(program_t *progp, int fx);
extern int get_line_number(bytecode_p p, program_t *progp, char **namep);
extern char *dump_trace(Bool how);
extern int get_line_number_if_any(char **name);
extern void reset_machine(Bool first);
extern svalue_t *secure_apply(string_t *fun, object_t *ob, int num_arg);
extern svalue_t *apply_master_ob(string_t *fun, int num_arg);
extern void assert_master_ob_loaded(void);
extern svalue_t *secure_call_lambda(svalue_t *closure, int num_arg);
extern void remove_object_from_stack(object_t *ob);
extern void call_lambda(svalue_t *lsvp, int num_arg);
extern void free_interpreter_temporaries(void);
extern void invalidate_apply_low_cache(void);
extern void add_eval_cost(int num);
extern void push_referenced_mapping(mapping_t *m);
extern void m_indices_filter (svalue_t *key, svalue_t *data, void *extra);
extern void m_values_filter (svalue_t *key, svalue_t *data, void *extra);
extern void m_unmake_filter ( svalue_t *key, svalue_t *data, void *extra);
extern int last_instructions(int length, Bool verbose, svalue_t **svpp);
extern svalue_t *f_apply (svalue_t *sp, int num_arg);
extern svalue_t *f_funcall (svalue_t *sp, int num_arg);
extern svalue_t *f_call_resolved (svalue_t *sp, int num_arg);
extern svalue_t *f_caller_stack_depth (svalue_t *sp);
extern svalue_t *f_caller_stack (svalue_t *sp);
extern svalue_t *f_get_eval_cost (svalue_t *sp);
extern svalue_t *f_previous_object (svalue_t *sp);
extern svalue_t *f_last_instructions(svalue_t *sp);
extern svalue_t *f_set_this_object (svalue_t *sp);
extern svalue_t *f_trace(svalue_t *sp);
extern svalue_t *f_traceprefix(svalue_t *sp);

#ifndef COMPAT_MODE
extern string_t *add_slash (string_t *str);
#endif

#ifdef OPCPROF
extern Bool opcdump(char *fname);
#endif

#ifdef TRACE_CODE
extern int last_instructions(int length, int verbose, svalue_t **svpp);
#endif

#ifdef DEBUG
extern int check_state(void);
extern void count_inherits(program_t *progp);
extern void count_extra_ref_in_object(object_t *ob);
extern void count_extra_ref_in_vector(svalue_t *svp, size_t num);
extern void check_a_lot_ref_counts(program_t *search_prog);
#endif

extern size_t interpreter_overhead(void);

#ifdef GC_SUPPORT
extern void clear_interpreter_refs(void);
extern void count_interpreter_refs(void);
#endif


#endif /* INTERPRET_H__ */
