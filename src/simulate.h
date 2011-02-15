#ifndef SIMULATE_H__
#define SIMULATE_H__ 1

#include <setjmp.h>

#include "driver.h"
#include "typedefs.h"

#include "bytecode.h"
#include "sent.h"       /* shadow_sentence_t */
#include "strfuns.h"    /* strbuf_t */
#include "svalue.h"

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
  /* Errors are caught in interpret.c by the catch() construct.
   * This is in fact an extended error_recovery_info structure which
   * is allocated on the heap.
   *
   * The CATCH context has a number of attributes, expressed by bitflags:
   */
#define CATCH_FLAG_NOLOG   (0x01)  /* The traceback is not logged */
#define CATCH_FLAG_PUBLISH (0x02)  /* master::runtime_error() is called
                                    * despite the error being caught.
                                    */
  /* The following flags are used only by the bytecode interpreter
   * to set up the instruction.
   */
#define CATCH_FLAG_RESERVE (0x04)  /* The amount of ticks to keep in reserve
                                    * is given on the stack.
                                    */


#define ERROR_RECOVERY_CONTEXT(t) ((t) >= ERROR_RECOVERY_NONE)
  /* True, if rt_context_s.type 't' denotes a error recovery context.
   */

#define ERROR_RECOVERY_CAUGHT(t) ((t) == ERROR_RECOVERY_CATCH)
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
 * That structure and all handling routines are local to interpret.c .
 */

struct error_recovery_info
{
    rt_context_t      rt;
    int               flags;  /* Flags for ERROR_RECOVERY_CATCH. */
    struct longjump_s con;    /* longjmp() information */
};


/* --- struct callback: describes a function call plus arguments
 *
 * This structure is used by input_tos and call_outs to store the
 * information about the function to call and the arguments to pass.
 */

struct callback_s {
    union {               /* The function to call: by name or the closure */
        struct {
            string_t *name;  /* the tabled function name */
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
                     , max_mapping_keys = def_mapping_keys \
                     , max_eval_cost = def_eval_cost \
                     , max_file_xfer = def_file_xfer \
                     , max_byte_xfer = def_byte_xfer \
                     , max_callouts = def_callouts \
                     , max_memory = def_memory \
                     , use_eval_cost = DEF_USE_EVAL_COST \
                     )

  /* (Re)Initialize the runtime limits from the given default values.
   */

#define ERROR_BUF_LEN 10240
  /* Length of the fixed buffer for error messages.
   */

#define ERROR_FMT_LEN 2048
  /* Length of the fixed buffer for error message formats.
   */

/* --- Variables --- */

extern struct error_recovery_info toplevel_context;
extern rt_context_t *rt_context;

extern size_t def_array_size;
extern size_t def_mapping_size;
extern size_t def_mapping_keys;
extern int32  def_eval_cost;
extern int32  def_file_xfer;
extern int32  def_byte_xfer;
extern int32  def_callouts;
extern p_int  def_memory;
#define DEF_USE_EVAL_COST (-100)

extern size_t max_array_size;
extern size_t max_mapping_size;
extern size_t max_mapping_keys;
extern int32  max_eval_cost;
extern int32  max_file_xfer;
extern int32  max_byte_xfer;
extern int32  max_callouts;
extern p_int  max_memory;
extern int32  use_eval_cost;

extern object_t *obj_list;
extern object_t *obj_list_end;
extern object_t *master_ob;
extern object_t *destructed_objs;
extern object_t *newly_destructed_objs;
extern long num_destructed;
extern long num_newly_destructed;
extern uint32_t destructed_ob_counter;

extern object_t *current_object;
extern object_t *current_interactive;
extern object_t *previous_ob;

extern svalue_t driver_hook[];

extern int num_error;
extern int num_warning;
extern string_t *current_error;
extern string_t *current_error_file;
extern string_t *current_error_object_name;
extern mp_int current_error_line_number;
extern vector_t *uncaught_error_trace;
extern vector_t *current_error_trace;

extern Bool game_is_being_shut_down;
extern Bool master_will_be_updated;

/* --- Prototypes --- */

#ifndef USE_NEW_INLINES
extern Bool catch_instruction (int flags, uint offset, volatile svalue_t ** volatile i_sp, bytecode_p i_pc, svalue_t * i_fp, int32 reserve_cost);
#else
extern Bool catch_instruction (int flags, uint offset, volatile svalue_t ** volatile i_sp, bytecode_p i_pc, svalue_t * i_fp, int32 reserve_cost, svalue_t *i_context);
#endif /* USE_NEW_INLINES */
extern void check_shadow_sent (object_t *ob);
extern void assert_shadow_sent (object_t *ob);
extern void init_empty_callback (callback_t *cb);
extern int  setup_function_callback(callback_t *cb, object_t* ob, string_t *fun, int nargs, svalue_t * args, Bool delayed_callback);
extern int  setup_closure_callback(callback_t *cb, svalue_t *cl, int nargs, svalue_t * args, Bool delayed_callback);
extern int  setup_efun_callback_base ( callback_t *cb, svalue_t *args, int nargs, Bool bNoObj);
#define setup_efun_callback(cb,args,nargs)       setup_efun_callback_base(cb,args,nargs,MY_FALSE)
#define setup_efun_callback_noobj(cb,args,nargs) setup_efun_callback_base(cb,args,nargs,MY_TRUE)
extern void free_callback (callback_t *cb);
extern svalue_t *execute_callback (callback_t *cb, int nargs, Bool keep, Bool toplevel);
#define apply_callback(cb,nargs)   execute_callback(cb,nargs,MY_TRUE,MY_FALSE)
#define backend_callback(cb,nargs) execute_callback(cb,nargs,MY_FALSE,MY_TRUE)
extern object_t *callback_object(callback_t *cb);
extern svalue_t *callback_function (callback_t *cb);
extern void callback_change_object (callback_t *cb, object_t *obj);
#ifdef DEBUG
extern void count_callback_extra_refs (callback_t *cb);
#endif
#ifdef GC_SUPPORT
extern void clear_ref_in_callback (callback_t *cb);
extern void count_ref_in_callback (callback_t *cb);
#endif
extern void init_driver_hooks(void);
extern void set_svalue_user(svalue_t *svp, object_t *owner);
extern void destruct_object(svalue_t *v);
extern void destruct(object_t *ob);
extern void deep_destruct (object_t *ob);
extern void handle_newly_destructed_objects(void);
extern void remove_destructed_objects (Bool force);
extern void print_svalue(svalue_t *arg);
extern const char *make_name_sane(const char *pName, Bool addSlash);
extern object_t *lookfor_object(string_t *str, Bool bLoad);
#define find_object(str) lookfor_object((str), MY_FALSE)
#define get_object(str) lookfor_object((str), MY_TRUE)
extern object_t *find_object_str(const char *str);
extern Bool status_parse(strbuf_t * sbuf, char *buff);
extern void dinfo_data_status(svalue_t * svp, int value);
extern void warnf VARPROT((char *, ...), printf, 1, 2);
extern void errorf VARPROT((const char *, ...), printf, 1, 2) NORETURN;
extern void fatal VARPROT((const char *, ...), printf, 1, 2) NORETURN;
extern void throw_error(svalue_t *v) NORETURN;
extern char *limit_error_format(char *fixed_fmt, size_t fixed_fmt_len, const char *fmt);
extern Bool legal_path(const char *path);
extern Bool check_no_parentdirs(const char *path);
extern void parse_error(Bool warning, const char *error_file, int line, const char *what, const char *context);
extern string_t *check_valid_path(string_t *path, object_t *caller, string_t *call_fun, Bool writeflg);
extern Bool match_string(const char *match, const char *str, mp_int len);

extern svalue_t *f_write(svalue_t *sp);
extern svalue_t *f_clone_object(svalue_t *sp);
extern svalue_t *f_destruct(svalue_t *sp);
extern svalue_t *f_find_object(svalue_t *sp);
extern svalue_t *f_load_object(svalue_t *sp);
extern svalue_t *f_set_driver_hook(svalue_t *sp);
extern svalue_t *f_shadow(svalue_t *sp);
extern svalue_t *f_query_shadowing(svalue_t *sp);
extern svalue_t *f_unshadow(svalue_t *sp);
extern svalue_t *v_limited(svalue_t * sp, int num_arg);
extern svalue_t *v_set_limits(svalue_t * sp, int num_arg);
extern svalue_t *f_query_limits(svalue_t * sp);

#endif  /* SIMULATE_H__ */
