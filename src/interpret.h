#ifndef INTERPRET_H__
#define INTERPRET_H__ 1

#include <setjmp.h>

#include "driver.h"
#include "typedefs.h"
#include "instrs.h"

/* --- Types --- */

/* --- struct control_stack: one control stack element
 *
 * Every structure describes the previous function call levels, the
 * current function call data is kept in interpret's global variables..
 * 'prog' is usually the same as ob->prog, except when
 * executing inherited functions.
 *
 * TODO: The frames should have special flags to mark stuff like
 * TODO:: sefun closures, closures, etc.
 */

struct control_stack {
    object_t   *ob;         /* Current object */
    object_t   *prev_ob;    /* Save previous object */
    program_t  *prog;       /* Current program, NULL in the bottom entry */
    bytecode_p  pc;         /* Program counter, points to next bytecode */
    svalue_t   *fp;         /* Frame pointer: first arg on stack */
    bytecode_p  funstart;
      /* Start of the function code.
       * Two magic values (SIMUL_EFUN_FUNSTART and EFUN_FUNSTART) mark
       * entries for simul-efun and efun closures.
       */
    int num_local_variables;    /* Number of local vars + arguments */
    int function_index_offset;
      /* Index of current program's function block within the functions of the
       * current objects program (needed for inheritance).
       */
    svalue_t *current_variables;        /* Same */
    int   extern_call;
      /* TRUE if the call came from outside the object (call_others to
       * oneself are a special case of this). Only entries with this flag
       * set save the .ob and .prev_ob for the flagged and all previous
       * unflagged entries.
       * If the current this_object was changed, the 'imposter' object is
       * stored in .pretend_to_be and this flag is or'ed with CS_PRETEND.
       */
#   define CS_PRETEND 0x80
    Bool  catch_call;
      /* This is the 'faked' call context for the code inside a catch().
       * Since the interpreter fakes a subroutine call for this, F_RETURN
       * must be able to tell the contexts apart.
       * (Right now the LPC compiler prohibits the use of 'return' inside
       * of a catch, but providing it on this level already doesn't hurt).
       */
    int   instruction;
      /* For EFUN_FUNSTART entries, this is the efun executed.
       */

    bytecode_p *break_sp;
      /* Points to address to branch to at next F_BREAK, which is also
       * the actual bottom of the break stack.
       */
    object_t *pretend_to_be;
      /* After set_this_object(), the this_object imposter.
       */
};

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
extern struct control_stack *csp;
extern svalue_t * inter_sp;
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

extern Bool eval_instruction(bytecode_p first_instruction, svalue_t *initial_sp);
extern void free_string_svalue(svalue_t *v);
extern void push_control_stack(svalue_t *sp, bytecode_p pc, svalue_t *fp);
extern void pop_control_stack(void);
extern struct longjump_s *push_error_context(svalue_t *sp, bytecode_t catch_inst);
extern void pop_error_context (void);
extern svalue_t *pull_error_context (svalue_t *sp);
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
extern string_t *add_slash (string_t *str);

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
