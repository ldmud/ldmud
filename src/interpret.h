#ifndef INTERPRET_H__
#define INTERPRET_H__ 1

#include <setjmp.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>
#include <sys/types.h>

#include "driver.h"
#include "typedefs.h"
#include "exec.h"

#include "backend.h"
#include "bytecode.h"
#include "svalue.h"

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
    svalue_t    ob;         /* Current object (not counted) */
    svalue_t    prev_ob;    /* Save previous object (not counted) */
    program_t  *prog;       /* Current program, NULL in the bottom entry */
    svalue_t    lambda;     /* Current lambda, counted, or svalue-0 if none */
    bytecode_p  pc;         /* Program counter, points to next bytecode */
    svalue_t   *fp;         /* Frame pointer: first arg on stack */
    svalue_t   *context;    /* Context pointer */
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
    int variable_index_offset;          /* Same for variables. */
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

    svalue_t *break_sp;
      /* Points to address to branch to at next F_BREAK, which is also
       * the actual bottom of the break stack.
       */
    svalue_t pretend_to_be;
      /* After set_this_object(), the this_object imposter (refcounted).
       * TODO: This should be mirrored in the current_object global variable,
       * TODO:: to avoid accesses to wrong functions/variables.
       */
#ifdef EVAL_COST_TRACE
    int32 eval_cost;
      /* The eval cost at that moment. */
#endif
};

/* An error handler is simply a function that is given the
 * pointer to a structure containing the function pointer, so that
 * more data for cleaning up may be embedded into this structure.
 */
struct error_handler_s {
    void (*fun) (error_handler_t *);
};

/* a general error handler structure. head is assigned as payload to an 
 * T_LVALUE svalue of type T_ERROR_HANDLER and pushed onto the value stack.
 * If the stack is unrolled during runtime errors the error_handler function
 * is called and frees buff. */
typedef struct mem_error_handler_s {
  error_handler_t head;      /* The T_ERROR_HANDLER structure */
  char          * buff;      /* The allocated buffer to free. */
} mem_error_handler_t;

/* --- struct protected_lvalue: protect a single value
 * On creation the original svalue is taken out of its place
 * (a variable, array or whatever) and put in this structure.
 * A T_LVALUE/LVALUE_PROTECTED pointing to this structure will
 * then be stored into the original place. This is done to have the
 * lifetime of the lvalue not be bound by the lifetime of the variable,
 * array or whatever.
 */
struct protected_lvalue
{
    p_int    ref; /* Number of references */
    svalue_t val; /* The svalue. */
};

/* --- struct protected_char_lvalue: protect an lvalue to a single
 * character in a string. We'll save a counted reference to the string.
 */
struct protected_char_lvalue
{
    p_int     ref;                      /* Number of references to this structure. */
    string_t *str;                      /* The string that is indexed. */
    char     *charp;                    /* The indexed character. */
    struct protected_lvalue *var;       /* A (counted) protected_lvalue
                                           referencing that string or vector. */

    struct protected_char_lvalue* next; /* Next in the String's lvalue list. */
};

/* -- struct protected_range_lvalue: protected an lvalue to a
 * bytes, string or vector range. The reference to the vector is counted.
 * We also make the variable holding the vector a protected lvalue,
 * so we are able to update variable when changing the size of
 * a vector or changing the (not-mutable) string.
 * Indices into a unicode string are given as byte offsets.
 */
struct protected_range_lvalue
{
    p_int    ref;                        /* Number of references, */
    svalue_t vec;                        /* The string or vector containing the range. */
    mp_int   index1, index2;             /* first and last index of the range.
                                            index2 is the index after the last
                                            element of the range. */
    struct protected_lvalue *var;        /* A (counted) protected_lvalue
                                            referencing that string or vector. */
    struct protected_range_lvalue* next; /* Next in the String's lvalue list. */
};

/* -- struct protected_mapentry_lvalue: An lvalue into a mapping.
 * This is used when a map entry does not yet exist, so we don't
 * need to add an entry prematurely.
 */
struct protected_mapentry_lvalue
{
    p_int      ref;                     /* Number of references.  */
    mapping_t *map;                     /* The (counted) mapping. */
    svalue_t   key;                     /* The key of the entry.  */
    int        index;                   /* Column of the lvalue.  */
};



/* --- Constants --- */

static const short MAX_SHIFT = (sizeof(p_int) << 3) - 1;
  /* The maximally useful shift (left or right) of a number in LPC.
   */

/* --- Variables --- */

extern program_t *current_prog;
extern int tracedepth;
extern int trace_level;
extern bytecode_p inter_pc;
extern struct control_stack *csp;
extern svalue_t * inter_sp;
extern int function_index_offset;
extern svalue_t *current_variables;
extern int32  eval_cost;
extern int32  assigned_eval_cost;
extern svalue_t apply_return_value;

#ifdef APPLY_CACHE_STAT
extern statcounter_t apply_cache_hit;
extern statcounter_t apply_cache_miss;
#endif

extern p_uint eval_number;
extern unsigned long total_evalcost;
extern unsigned long last_total_evalcost;
extern struct timeval last_eval_duration;
extern statistic_t stat_total_evalcost;
extern statistic_t stat_eval_duration;
extern struct timeval profiling_timevalue;
extern p_int used_memory_at_eval_start;

extern int num_protected_lvalues;

/* --- Prototypes --- */

extern void assign_eval_cost(void);
extern void mark_start_evaluation (void);
extern void mark_end_evaluation(void);

extern Bool eval_instruction(bytecode_p first_instruction, svalue_t *initial_sp);
extern void free_string_svalue(svalue_t *v);
extern void push_control_stack(svalue_t *sp, bytecode_p pc, svalue_t *fp, svalue_t *context);
extern void pop_control_stack(void);
extern struct longjump_s *push_error_context(svalue_t *sp, int catch_flags);
extern void pop_error_context (void);
extern svalue_t *pull_error_context (svalue_t *sp, svalue_t *msg);
extern void transfer_error_message (svalue_t *v, rt_context_t *rt);
extern Bool destructed_object_ref (svalue_t *svp);
extern void free_object_svalue(svalue_t *v);
extern void zero_object_svalue(svalue_t *v);
extern void free_svalue(svalue_t *v);
extern void normalize_svalue(svalue_t *svp, bool collapse_lvalues);
extern void assign_svalue_no_free(svalue_t *to, svalue_t *from);
extern void assign_rvalue_no_free(svalue_t *to, svalue_t *from);
extern void assign_rvalue_no_free_no_collapse(svalue_t *to, svalue_t *from);
extern void assign_svalue(svalue_t *dest, svalue_t *v);
extern void copy_svalue_no_free (svalue_t *to, svalue_t *from);
extern void transfer_svalue_no_free(svalue_t *dest, svalue_t *v);
extern void transfer_rvalue_no_free(svalue_t *dest, svalue_t *v);
extern void transfer_svalue(svalue_t *dest, svalue_t *v);
extern void assign_protected_lvalue_no_free(svalue_t *dest, svalue_t *src);
extern void assign_protected_lvalue(svalue_t *dest, svalue_t *src);
extern void assign_protected_char_lvalue_no_free(svalue_t *dest, struct protected_lvalue *var, string_t *src, char *charp);
extern void assign_protected_range_lvalue_no_free(svalue_t *dest, struct protected_lvalue *var, svalue_t *vec, mp_int index1, mp_int index2);
extern void assign_protected_mapentry_lvalue_no_free(svalue_t *dest, mapping_t *map, svalue_t *key, int index);

extern svalue_t *get_rvalue(svalue_t *v, bool *last_reference);
extern svalue_t *get_rvalue_no_collapse(svalue_t *v, bool *last_reference);

extern void put_c_string (svalue_t *sp, const char *p);
extern void put_c_n_string (svalue_t *sp, const char *p, size_t len);
extern void put_bytes_buf (svalue_t *sp, const void *p, size_t len);

extern void push_svalue(svalue_t *v);
extern void push_rvalue(svalue_t *v);
extern void push_svalue_block(int num, svalue_t *v);
extern svalue_t *pop_n_elems (int n, svalue_t *sp);
extern void pop_stack(void);
extern void push_apply_value(void);
extern void pop_apply_value (void);
extern void push_referenced_mapping(mapping_t *m);
extern svalue_t *push_error_handler(void (*errorhandler)(error_handler_t *), error_handler_t *arg);
extern void *xalloc_with_error_handler(size_t size);

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
extern Bool privilege_violation(string_t *what, svalue_t *arg, svalue_t *sp);
extern Bool privilege_violation2(string_t *what, svalue_t *arg, svalue_t *arg2, svalue_t *sp);
extern Bool privilege_violation4(string_t *what, svalue_t whom, string_t *how_str, int how_num, svalue_t *sp);
extern Bool privilege_violation_n(string_t *what, svalue_t whom, svalue_t *sp, int num_arg);

extern Bool check_rtt_compatibility(lpctype_t *formaltype, svalue_t *svp) __attribute__((nonnull(2)));
extern lpctype_t* get_rtt_type(lpctype_t *formaltype, svalue_t *svp) __attribute__((nonnull(2)));
extern int translate_virtual_variable_index(int num);

extern svalue_t *sapply_lwob_int(string_t *fun, lwobject_t *lwob, int num_arg, bool b_find_static);
#define sapply_lwob(f,o,n) sapply_lwob_int(f,o,n,false)
#define sapply_lwob_ign_prot(f,o,n) sapply_lwob_int(f,o,n,true)
extern svalue_t *sapply_int(string_t *fun, object_t *ob, int num_arg, Bool b_ign_prot, Bool b_use_default);
#define sapply(f,o,n) sapply_int(f,o,n, MY_FALSE, MY_TRUE)
#define sapply_ign_prot(f,o,n) sapply_int(f,o,n, MY_TRUE, MY_TRUE)
extern svalue_t *apply(string_t *fun, object_t *ob, int num_arg);
extern void call_function(program_t *progp, int fx);
extern void call_ob_function_args(object_t* ob, int fx, int num_arg);
extern void call_lwob_function_args(lwobject_t* lwob, int fx, int num_arg);
extern int get_line_number(bytecode_p p, program_t *progp, string_t **namep);
extern string_t *collect_trace(strbuf_t * sbuf, vector_t ** rvec);
extern string_t *dump_trace(Bool how, vector_t **rvec, string_t ** rstr);
extern int get_line_number_if_any(string_t **name);
extern void reset_machine(Bool first);
extern void secure_apply_error(svalue_t *save_sp, struct control_stack *save_csp, Bool clear_costs);
extern svalue_t *secure_apply_lwob(string_t *fun, lwobject_t *lwob, int num_arg);
extern svalue_t *secure_apply_ob(string_t *fun, object_t *ob, int num_arg, Bool external);
#define secure_apply(fun, ob, num_arg) secure_apply_ob(fun, ob, num_arg, MY_FALSE)
#define secure_callback(fun, ob, num_arg) secure_apply_ob(fun, ob, num_arg, MY_TRUE)

extern svalue_t *apply_master_ob(string_t *fun, int num_arg, Bool external);
#define apply_master(fun, num_arg) apply_master_ob(fun, num_arg, MY_FALSE)
#define callback_master(fun, num_arg) apply_master_ob(fun, num_arg, MY_TRUE)

extern void assert_master_ob_loaded(void);
extern svalue_t *secure_call_lambda(svalue_t *closure, int num_arg, Bool external);
#define secure_apply_lambda(fun, num_arg) secure_call_lambda(fun, num_arg, MY_FALSE)
#define secure_callback_lambda(fun, num_arg) secure_call_lambda(fun, num_arg, MY_TRUE)

extern void remove_object_from_stack(object_t *ob);
extern void int_call_lambda(svalue_t *lsvp, int num_arg, Bool external);
#define call_lambda(lsvp, num_arg) int_call_lambda(lsvp, num_arg, MY_TRUE)
extern inherit_t *adjust_variable_offsets(const inherit_t *inheritp, const program_t *prog, const program_t *obprog);
extern void free_interpreter_temporaries(void);
extern void invalidate_apply_low_cache(void);
extern void m_indices_filter (svalue_t *key, svalue_t *data, void *extra);
extern void m_values_filter (svalue_t *key, svalue_t *data, void *extra);
extern void m_unmake_filter ( svalue_t *key, svalue_t *data, void *extra);
extern svalue_t *v_apply (svalue_t *sp, int num_arg);
extern svalue_t *v_funcall (svalue_t *sp, int num_arg);
extern svalue_t *v_call_other (svalue_t *sp, int num_arg);
extern svalue_t *v_call_strict (svalue_t *sp, int num_arg);
extern svalue_t *v_call_direct_resolved (svalue_t *sp, int num_arg);
extern svalue_t *v_call_resolved (svalue_t *sp, int num_arg);
extern svalue_t *f_caller_stack_depth (svalue_t *sp);
extern svalue_t *f_caller_stack (svalue_t *sp);
extern svalue_t *f_get_eval_cost (svalue_t *sp);
extern svalue_t *f_previous_object (svalue_t *sp);
extern svalue_t *f_set_this_object (svalue_t *sp);
extern svalue_t *f_trace(svalue_t *sp);
extern svalue_t *f_traceprefix(svalue_t *sp);

#ifdef OPCPROF
extern Bool opcdump(string_t *fname);
#endif

#ifdef TRACE_CODE
extern svalue_t *f_last_instructions(svalue_t *sp);
extern int last_instructions(int length, Bool verbose, svalue_t **svpp);
#endif

#ifdef DEBUG
extern int check_state(void);
extern void count_inherits(program_t *progp);
extern void count_extra_ref_in_object(object_t *ob);
extern void count_extra_ref_in_vector(svalue_t *svp, size_t num);
extern void check_a_lot_ref_counts(program_t *search_prog);
#endif

// signal handler for profiling (SIGPROF)
extern void handle_profiling_signal(int ignored);
extern Bool set_profiling_time_limit(mp_int limit);
extern mp_int get_profiling_time_limit();

extern size_t interpreter_overhead(void);

#ifdef GC_SUPPORT
extern void clear_interpreter_refs(void);
extern void count_interpreter_refs(void);
#endif

extern int  control_stack_depth(void);


#endif /* INTERPRET_H__ */
