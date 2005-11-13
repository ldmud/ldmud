#ifndef __BACKEND_H__
#define __BACKEND_H__ 1

#include <setjmp.h>

#include "driver.h"
#include "datatypes.h"  /* struct svalue */
#include "main.h"       /* max_time */
#include "object.h"     /* struct object */

/* --- Macros --- */

/* Reset the evaluation cost/time counters back to the maximum.
 */
#define CLEAR_EVAL_COST (assigned_eval_cost = eval_cost = initial_eval_cost)


/* --- Types --- */

/* --- struct con_struct: context information
 *
 * This structure contains the necessary data to execute a longjmp()
 * when recovering from an error.
 */

struct con_struct { jmp_buf text; };

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

struct error_recovery_info {
    struct error_recovery_info *last;  /* Previous context, or NULL */
    int type;                          /* Type of this context */
    struct con_struct con;             /* longjmp() information */
};

/* error_recovery_info.type: */
#define ERROR_RECOVERY_NONE      0
  /* No error recovery available (used by the top entry in the stack)
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
   */

/* --- Variables --- */

extern struct error_recovery_info toplevel_error_recovery_info;
extern struct error_recovery_info *error_recovery_pointer;
extern mp_int current_time;
extern Bool time_to_call_heart_beat;
extern volatile Bool comm_time_to_call_heart_beat;
extern uint32 total_player_commands;
extern volatile mp_int total_alarms;
extern int32  initial_eval_cost;
extern int32  eval_cost;
extern int32  assigned_eval_cost;
#ifndef OLD_RESET
extern uint num_listed_objs;
extern uint num_last_processed;
extern long avg_last_processed;
extern long avg_in_list;
#endif

extern Bool extra_jobs_to_do;
extern Bool garbage_collect_to_do;

/* --- Prototypes --- */

extern void  clear_state (void);
extern void  logon (struct object *ob);
extern int   parse_command (char *str, struct object *ob);
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
extern int   file_size (char *file);
extern void  update_compile_av (int lines);
extern char *query_load_av (void);
extern struct svalue* f_regreplace (struct svalue *sp);

/* --- Macros --- */

#endif /* __BACKEND_H__ */
