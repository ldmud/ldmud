#ifndef SIMULATE_H__
#define SIMULATE_H__ 1

#include <setjmp.h>

#include "driver.h"
#include "typedefs.h"

#include "instrs.h"     /* F_TRANSFER, F_RENAME */
#include "sent.h"       /* shadow_sentence_t */
#include "strfuns.h"    /* strbuf_t */
#include "svalue.h"

/* TODO: Since the VM now keeps a reference on lambda during their
 * TODO:: execution, the free_closure_hook() is no longer needed.
 * TODO:: Just in case we keep the code around for now. (14-Aug-01)
 */
/* #define USE_FREE_CLOSURE_HOOK */

/* --- Types --- */

/* --- struct rt_context_s: runtime context information
 *
 * The runtime context struct is made up from children of these structures.
 * The stack is used to store nested changes in the runtime context, may
 * they be error recovery contexts, changes in the runtime limits, or else.
 *
 * Storing all this information into one stack makes it much easier to restore
 * a previous context in case of errors. For that, error recovery information
 * plays the most important role in this stack.
 *
 * Most of the stack entries are allocated on the stack, only few are
 * allocated on the heap.
 */

struct rt_context_s
{
    struct rt_context_s *last;   /* Previous context, or NULL */
    int                  type;   /* Type of this context entry */
};

/* rt_context_s.type:
 *
 * Positive values are all ERROR_RECOVERY types, negative values
 * denote other context types.
 */

#define LIMITS_CONTEXT          -2
  /* The previous set of runtime limits.
   */

#define COMMAND_CONTEXT         -1
  /* The previous command context (struct command_context in simulate.c).
   * For the very first command given, this is all 0.
   */

/* For the following context types, the stack entry is in fact
 * a error_recovery_info structure.
 */

#define ERROR_RECOVERY_NONE         0
  /* No error recovery available (used by the top entry in the stack).
   */
#define ERROR_RECOVERY_BACKEND      1
  /* Errors fall back to the backend, e.g. process_objects(),
   * call_heart_beat() and others.
   */
#define ERROR_RECOVERY_APPLY        2
  /* Errors fall back into the secure_apply() function used for sensitive
   * applies.
   */
#define ERROR_RECOVERY_CATCH        3
#define ERROR_RECOVERY_CATCH_NOLOG  4
  /* Errors are caught in interpret.c by the catch()/catch_nolog() construct.
   * This is in fact an extended error_recovery_info structure which
   * is allocated on the heap.
   * _CATCH_NOLOG catches don't log the error in the logfiles and is meant
   * for objects like wiztools.
   */

#define ERROR_RECOVERY_CONTEXT(t) ((t) >= ERROR_RECOVERY_NONE)
  /* True, if rt_context_s.type 't' denotes a error recovery context.
   */

#define ERROR_RECOVERY_CAUGHT(t) (  (t) == ERROR_RECOVERY_CATCH \
                                 || (t) == ERROR_RECOVERY_CATCH_NOLOG )
  /* True, if rt_context_s.type 't' denotes a catch recovery context.
   */

/* --- struct longjump_s: longjump context information
 *
 * This structure contains the necessary data to execute a longjmp()
 * when recovering from an error.
 */

struct longjump_s { jmp_buf text; };


/* --- struct error_recovery_info: error recovery context
 *
 * The error recovery stack is made up from these structures, describing
 * the nature of the error handling, and where to jump back to.
 *
 * For ERROR_RECOVERY_CATCH contexts, the stack element is in fact a larger
 * structure containing the error_recovery_info as first member, with
 * additional members holding the information needed to perform a catch().
 * That structure and its handling routines are local to interpret.c.
 */

struct error_recovery_info
{
    rt_context_t      rt;
    struct longjump_s con;             /* longjmp() information */
};


/* --- struct callback: describes a function call plus arguments
 *
 * This structure is used by input_tos and call_outs to store the
 * information about the function to call and the arguments to pass.
 */

struct callback_s {
    union {               /* The function to call: by name or the closure */
        struct {
            char     *name;  /* the shared function name */
            object_t *ob;    /* reference to the object to call */
        } named;
        svalue_t lambda;     /* the closure to call */
    } function;
    Bool is_lambda;         /* Closure or named function? */
    int         num_arg;    /* Number of arguments */
    svalue_t    arg;
      /* Arguments to pass:
       *   - T_INVALID if no arguments
       *   - a single argument
       *   - T_LVALUE with u.lvalue pointing to the svalue_t[] with
       *     the arguments. If arg.x.extern_args is TRUE, the block
       *     was allocated from outside and is outside of our control.
       * No argument can be a LVALUE itself.
       */
};

#ifdef CHECK_OBJECT_REF
typedef struct object_shadow_s {
    struct object_shadow_s * next;
    object_t * obj;
    unsigned short flags;
    p_int ref;
    sentence_t *sent;
} object_shadow_t;
extern object_shadow_t * destructed_obj_shadows;
extern object_shadow_t * newly_destructed_obj_shadows;
extern void check_object_shadow (object_t *ob, object_shadow_t *sh);
extern void check_all_object_shadows (void);
extern void update_object_sent(object_t *obj, sentence_t *new_sent);
#endif /* CHECK_OBJECT_REF */

/* --- Macros --- */

#define RESET_LIMITS ( max_array_size = def_array_size \
                     , max_mapping_size = def_mapping_size \
                     , max_eval_cost = def_eval_cost \
                     , max_file_xfer = def_file_xfer \
                     , max_byte_xfer = def_byte_xfer \
                     , max_callouts = def_callouts \
                     )

  /* (Re)Initialize the runtime limits from the given default values.
   */

/* --- Variables --- */

extern struct error_recovery_info toplevel_context;
extern rt_context_t *rt_context;

extern size_t def_array_size;
extern size_t def_mapping_size;
extern int32  def_eval_cost;
extern int32  def_file_xfer;
extern int32  def_byte_xfer;
extern int32  def_callouts;

extern size_t max_array_size;
extern size_t max_mapping_size;
extern int32  max_eval_cost;
extern int32  max_file_xfer;
extern int32  max_byte_xfer;
extern int32  max_callouts;

extern object_t *obj_list;
extern object_t *obj_list_end;
extern object_t *master_ob;
extern object_t *destructed_objs;
extern object_t *newly_destructed_objs;
extern long num_destructed;
extern long num_newly_destructed;

extern object_t *current_object;
extern object_t *current_interactive;
extern object_t *previous_ob;

extern svalue_t driver_hook[];

extern int num_error;
extern char *current_error;
extern char *current_error_file;
extern char *current_error_object_name;
extern mp_int current_error_line_number;
extern vector_t *uncaught_error_trace;
extern vector_t *current_error_trace;

extern int game_is_being_shut_down;
extern int master_will_be_updated;

/* --- Prototypes --- */

extern Bool catch_instruction (bytecode_t catch_inst, uint offset, volatile svalue_t ** volatile i_sp, bytecode_p i_pc, svalue_t * i_fp);
extern void purge_shadow_sent(void);
extern void check_shadow_sent (object_t *ob);
extern void assert_shadow_sent (object_t *ob);
extern void init_empty_callback (callback_t *cb);
extern int  setup_function_callback(callback_t *cb, object_t* ob, char *fun, int nargs, svalue_t * args, Bool use_lvalues);
extern int  setup_closure_callback(callback_t *cb, svalue_t *cl, int nargs, svalue_t * args, Bool use_lvalues);
extern int  setup_efun_callback ( callback_t *cb, svalue_t *args, int nargs);
extern void free_callback (callback_t *cb);
extern svalue_t *execute_callback (callback_t *cb, int nargs, Bool keep, Bool toplevel);
#define apply_callback(cb,nargs)    execute_callback(cb,nargs,MY_TRUE,MY_FALSE)
#define backend_callback(cb,nargs)  execute_callback(cb,nargs,MY_FALSE,MY_TRUE)
extern object_t *callback_object(callback_t *cb);
#ifdef DEBUG
extern void count_callback_extra_refs (callback_t *cb);
#endif
#ifdef GC_SUPPORT
extern void clear_ref_in_callback (callback_t *cb);
extern void count_ref_in_callback (callback_t *cb);
#endif
extern void init_driver_hooks(void);
#ifdef USE_FREE_CLOSURE_HOOK
extern void free_closure_hooks(svalue_t* svp, int count);
extern void free_old_driver_hooks (void);
#endif
extern void set_svalue_user(svalue_t *svp, object_t *owner);
extern object_t *clone_object(char *str1);
extern void destruct_object(svalue_t *v);
extern void destruct(object_t *ob);
extern void deep_destruct (object_t *ob);
extern void handle_newly_destructed_objects(void);
extern void remove_destructed_objects (void);
extern void print_svalue(svalue_t *arg);
extern const char *make_name_sane(const char *pName, Bool addSlash);
extern object_t *lookfor_object(char *str, Bool bLoad);
#define find_object(str) lookfor_object((str), MY_FALSE)
#define get_object(str) lookfor_object((str), MY_TRUE)
extern void move_object(void);
extern Bool status_parse(strbuf_t * sbuf, char *buff);
extern void dinfo_data_status(svalue_t * svp, int value);
extern void warnf VARPROT((char *, ...), printf, 1, 2) NORETURN;
extern void error VARPROT((char *, ...), printf, 1, 2) NORETURN;
extern void fatal VARPROT((char *, ...), printf, 1, 2) NORETURN;
extern void throw_error(void);
extern char *limit_error_format(char *fixed_fmt, size_t fixed_fmt_len, char *fmt);
extern Bool legal_path(char *path);
extern Bool check_no_parentdirs(char *path);
extern void parse_error(Bool warning, char *error_file, int line, char *what, char *context);
extern char *check_valid_path(char *path, object_t *caller, char *call_fun, Bool writeflg);
extern Bool match_string(char *match, char *str, mp_int len);

extern void e_write(svalue_t *arg);
extern svalue_t *f_set_environment(svalue_t *sp);
extern svalue_t *f_rename_object(svalue_t *sp);
extern svalue_t *f_set_driver_hook(svalue_t *sp);
#ifdef F_SET_AUTO_INCLUDE_STRING
extern svalue_t *f_set_auto_include_string(svalue_t *sp);
#endif
extern svalue_t *f_shadow(svalue_t *sp);
extern svalue_t *f_query_shadowing(svalue_t *sp);
extern svalue_t *f_unshadow(svalue_t *sp);
extern svalue_t *f_limited(svalue_t * sp, int num_arg);
extern svalue_t *f_set_limits(svalue_t * sp, int num_arg);
extern svalue_t *f_query_limits(svalue_t * sp);

#ifdef F_SET_LIGHT
extern void add_light(object_t *p, int n);
#endif

extern vector_t *e_get_dir(char *path, int mask);
extern Bool e_tail(char *path);
extern int e_print_file(char *path, int start, int len);
extern Bool e_remove_file(char *path);
extern int e_rename(char *fr, char *t);
extern svalue_t *f_copy_file(svalue_t *sp);

#endif  /* SIMULATE_H__ */
