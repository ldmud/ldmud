#ifndef __BACKEND_H__
#define __BACKEND_H__ 1

#include "driver.h"
#include "interpret.h"  /* struct error_recovery_info, struct svalue */
#include "object.h"     /* struct object */

/* --- Macros --- */

/* Reset the evaluation cost counters back to the maximum. 
 */
#define CLEAR_EVAL_COST (assigned_eval_cost = eval_cost = initial_eval_cost)

/* --- Variables --- */

extern struct error_recovery_info toplevel_error_recovery_info;
extern struct error_recovery_info *error_recovery_pointer;
extern struct object *current_heart_beat;
extern int    current_time;
extern /* TODO: BOOL */ int    time_to_call_heart_beat;
extern volatile /* TODO: BOOL */ int    comm_time_to_call_heart_beat;
extern uint32 total_player_commands;
extern volatile mp_int total_alarms;
extern int32  initial_eval_cost;
extern int32  eval_cost;
extern int32  assigned_eval_cost;
extern /* TODO: BOOL */ int    extra_jobs_to_do;
extern /* TODO: BOOL */ int    garbage_collect_to_do;

/* --- Prototypes --- */

extern void  clear_state (void);
extern void  logon (struct object *ob);
extern int   parse_command (char *str, struct object *ob);
extern void  backend (void);
extern int   set_heart_beat (struct object *ob, int to);
extern int   heart_beat_status (int /* TODO: BOOL */ verbose);
extern void  preload_objects (int eflag);
extern struct svalue *f_debug_message (struct svalue *sp);
ALARM_HANDLER_PROT(catch_alarm);
extern void  remove_destructed_objects (void);
extern int   write_file (char *file, char *str);
extern char *read_file (char *file, int start, int len);
extern char *read_bytes (char *file, int start, int len);
extern int   write_bytes (char *file, int start, char *str);
extern int   file_size (char *file);
extern void  update_compile_av (int lines);
extern char *query_load_av (void);
extern struct svalue *f_heart_beat_info (struct svalue *sp);
extern struct svalue* f_regreplace (struct svalue *sp);

#ifdef MALLOC_smalloc
extern void  count_heart_beat_refs (void);
#endif

#endif /* __BACKEND_H__ */
