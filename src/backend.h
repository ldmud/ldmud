#ifndef BACKEND_H__
#define BACKEND_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "main.h"       /* max_time */

/* --- Variables --- */

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

typedef enum { gcDont = 0, gcMalloc, gcEfun } GC_Request;
extern GC_Request gc_request;

extern Bool mud_is_up;

/* --- Prototypes --- */

extern void  clear_state (void);
extern void check_alarm (void);
extern void  backend (void);
extern void  preload_objects (int eflag);
extern svalue_t *f_debug_message (svalue_t *sp);
ALARM_HANDLER_PROT(catch_alarm);
extern int   e_write_file (char *file, char *str);
extern char *e_read_file (char *file, int start, int len);
extern char *e_read_bytes (char *file, int start, int len);
extern int   e_write_bytes (char *file, int start, char *str);
extern long  e_file_size (char *file);
extern void  update_compile_av (int lines);
extern char *query_load_av (void);
extern svalue_t* f_regreplace (svalue_t *sp);

/* --- Macros --- */

#endif /* BACKEND_H__ */
