#ifndef __BACKEND_H__
#define __BACKEND_H__ 1

#include <setjmp.h>

#include "driver.h"
#include "datatypes.h"  /* struct svalue */
#include "main.h"       /* max_time */
#include "object.h"     /* struct object */

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
 *
 * TODO: Move this stack into simulate?
 */

struct rt_context_s
{
    struct rt_context_s *last;   /* Previous context, or NULL */
    int                  type;   /* Type of this context entry */
};

typedef struct rt_context_s rt_context_t;

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

#define ERROR_RECOVERY_NONE      0
  /* No error recovery available (used by the top entry in the stack).
   */
#define ERROR_RECOVERY_BACKEND   1
  /* Errors fall back to the backend, e.g. process_objects(),
   * call_heart_beat() and others.
   */
#define ERROR_RECOVERY_APPLY     2
  /* Errors fall back into the secure_apply() function used for sensitive
   * applies.
   */
#define ERROR_RECOVERY_CATCH     3
  /* Errors are caught in interpret.c by the catch() construct.
   * This is in fact an extended error_recovery_info structure which
   * is allocated on the heap.
   */

#define ERROR_RECOVERY_CONTEXT(t) ((t) >= ERROR_RECOVERY_NONE)
  /* True, if rt_context_s.type 't' denotes a error recovery context.
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

/* --- Variables --- */

extern struct error_recovery_info toplevel_context;
extern rt_context_t *rt_context;
extern mp_int current_time;
extern Bool time_to_call_heart_beat;
extern volatile Bool comm_time_to_call_heart_beat;
extern uint32 total_player_commands;
extern volatile mp_int total_alarms;
extern uint num_listed_objs;
extern uint num_last_processed;
extern long avg_last_processed;
extern long avg_in_list;

extern Bool extra_jobs_to_do;
extern Bool garbage_collect_to_do;

/* --- Prototypes --- */

extern void  clear_state (void);
extern void  logon (struct object *ob);
extern void  free_closure_hooks(struct svalue* svp, int count);
extern void  free_old_driver_hooks (void);
extern void  backend (void);
extern void  preload_objects (int eflag);
extern struct svalue *f_debug_message (struct svalue *sp);
ALARM_HANDLER_PROT(catch_alarm);
extern int   write_file (char *file, char *str);
extern char *read_file (char *file, int start, int len);
extern char *read_bytes (char *file, int start, int len);
extern int   write_bytes (char *file, int start, char *str);
extern long  file_size (char *file);
extern void  update_compile_av (int lines);
extern char *query_load_av (void);
extern struct svalue* f_regreplace (struct svalue *sp);

/* --- Macros --- */

#endif /* __BACKEND_H__ */
