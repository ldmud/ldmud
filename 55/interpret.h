#ifndef INTERPRET_H
#define INTERPRET_H

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


/*
 * Control stack element.
 * 'prog' is usually same as 'ob->prog' (current_object), except when
 * when the current function is defined by inheritance.
 * The pointer, csp, will point to the values that will be used at return.
 */
struct control_stack {
    struct object *ob;                /* Current object */
    struct object *prev_ob;        /* Save previous object */
    struct program *prog;        /* Current program */
    char *pc;
    struct svalue *fp;
    char *funstart;
    int num_local_variables;        /* Local + arguments */
    int function_index_offset;        /* Used when executing functions in inherited
                                   programs */
    struct svalue *current_variables;        /* Same */
    short extern_call;                /* Flag if evaluator should return */
    short dummy;
    char **break_sp;
    struct object *pretend_to_be;
};

struct con_struct { jmp_buf text; };

extern struct error_recovery_info {
    struct error_recovery_info *last;
    int type;
    struct con_struct con;
} *error_recovery_pointer;

#define ERROR_RECOVERY_NONE        0
#define ERROR_RECOVERY_BACKEND        1
#define ERROR_RECOVERY_APPLY        2
#define ERROR_RECOVERY_CATCH        3

#define LI_MAXOFFSET        0x3c
#define LI_INCLUDE        0x3d
#define LI_INCLUDE_END        0x3e
#define LI_L_RELOCATED        0x3f

#define LI_RELOCATED        0xc0
#define LI_SMALL_REL        0x20

#define LI_MAXEMPTY        0x20

#define MAX_SHIFT ((sizeof(p_int) << 3) - 1)


/* --- Variables --- */

extern struct program *current_prog;
extern struct error_recovery_info *error_recovery_pointer;
extern int tracedepth;
extern int trace_level;
extern struct svalue *inter_sp;
extern int function_index_offset;
extern struct svalue *current_variables;
extern struct svalue apply_return_value;
extern struct svalue catch_value;
extern struct svalue last_indexing_protector;

#ifdef SMALLOC_LPC_TRACE
extern char *inter_pc;
#endif

#ifdef APPLY_CACHE_STAT
extern int apply_cache_hit, apply_cache_miss;
#endif

/* --- Prototypes --- */

extern void init_interpret PROT((void));
extern void assign_eval_cost PROT((void));
extern void push_object PROT((struct object *ob));
extern void push_valid_ob PROT((struct object *ob));
extern void push_number PROT((p_int n));
extern void push_shared_string PROT((char *p));
extern void push_referenced_shared_string PROT((char *p));
INTER_INLINE void free_string_svalue PROT((struct svalue *v));
extern void free_object_svalue PROT((struct svalue *v));
extern void zero_object_svalue PROT((struct svalue *v));
extern void free_svalue PROT((struct svalue *v));
INTER_INLINE void assign_svalue_no_free PROT((struct svalue *to, struct svalue *from));
extern void assign_svalue PROT((struct svalue *dest, struct svalue *v));
INTER_INLINE void transfer_svalue_no_free PROT((struct svalue *dest, struct svalue *v));
extern void transfer_svalue PROT((struct svalue *dest, struct svalue *v));
extern void push_svalue PROT((struct svalue *v));
extern void push_svalue_block PROT((int num, struct svalue *v));
INTER_INLINE void pop_stack PROT((void));
extern void drop_stack PROT((void));
extern void bad_efun_arg PROT((int arg, int instr, struct svalue *sp)) NORETURN;
extern void bad_xefun_arg PROT((int arg, struct svalue *sp)) NORETURN;
extern void bad_xefun_vararg PROT((int arg, struct svalue *sp)) NORETURN;
extern void push_vector PROT((struct vector *v));
extern void push_referenced_vector PROT((struct vector *v));
extern void push_string_malloced PROT((char *p));
extern void push_string_shared PROT((char *p));
INTER_INLINE void push_malloced_string PROT((char *p));
extern void push_volatile_string PROT((char *p));
extern int _privilege_violation PROT((char *what, struct svalue *where, struct svalue *sp));
extern int privilege_violation4 PROT((char *what, struct object *whom, char *how_str, int how_num, struct svalue *sp));
extern void check_for_destr PROT((struct vector *v));
extern void push_apply_value PROT((void));
extern void pop_apply_value  PROT((void));
extern struct svalue *sapply_int(char *fun, struct object *ob, int num_arg, short /* TODO: BOOL */ b_ign_prot);
#define sapply(f,o,n) sapply_int(f,o,n, MY_FALSE)
extern struct svalue *apply PROT((char *fun, struct object *ob, int num_arg));
extern char *function_exists PROT((char *fun, struct object *ob));
extern void call_function PROT((struct program *progp, int fx));
extern int get_line_number PROT((char *p, struct program *progp, char **namep));
extern char *dump_trace PROT((int how));
extern int get_line_number_if_any PROT((char **name));
extern void reset_machine PROT((int first));
extern struct svalue *secure_apply PROT((char *fun, struct object *ob, int num_arg));
extern struct svalue *apply_master_ob PROT((char *fun, int num_arg));
extern void assert_master_ob_loaded PROT((void));
extern struct svalue *secure_call_lambda PROT((struct svalue *closure, int num_arg));
extern void remove_object_from_stack PROT((struct object *ob));
extern void call_lambda PROT((struct svalue *lsvp, int num_arg));
extern void free_interpreter_temporaries PROT((void));
extern void invalidate_apply_low_cache PROT((void));
extern void add_eval_cost PROT((int num));
extern void push_referenced_mapping PROT((struct mapping *m));
extern void m_indices_filter PROT((struct svalue* key, struct svalue* data, char *extra));
int last_instructions PROT((int length, int verbose, struct svalue **svpp));
struct svalue *f_last_instructions PROT((struct svalue *sp));
struct svalue *f_set_modify_command PROT((struct svalue *sp));
struct svalue *f_set_prompt PROT((struct svalue *sp));
struct svalue *f_extract_lvalue PROT((struct svalue *sp));
struct svalue *f_transpose_array PROT((struct svalue *sp));
struct svalue *f_trace PROT((struct svalue *sp));
struct svalue *f_traceprefix PROT((struct svalue *sp));
struct svalue *f_set_is_wizard PROT((struct svalue *sp));
struct svalue *f_to_object PROT((struct svalue *sp));
#ifdef F_DEBUG_INFO
struct svalue *f_debug_info PROT((struct svalue *sp, int num_arg));
#endif

#ifdef OPCPROF
extern void opcdump PROT((void));
#endif

#ifdef TRACE_CODE
extern int last_instructions PROT((int length, int verbose, struct svalue **svpp));
#endif

#ifdef DEBUG
extern int check_state PROT((void));
extern void count_inherits PROT((struct program *progp));
extern void count_extra_ref_in_object PROT((struct object *ob));
extern void count_extra_ref_in_vector PROT((struct svalue *svp, mp_int num));
extern void check_a_lot_ref_counts PROT((struct program *search_prog));
#endif

#ifdef MALLOC_smalloc
extern void clear_interpreter_refs PROT((void));
extern void count_interpreter_refs PROT((void));
#endif

#define bad_efun_vararg bad_xefun_arg
#define push_constant_string(str) push_volatile_string(str)

#endif /* INTERPRET_H */
