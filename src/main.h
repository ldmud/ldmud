#ifndef MAIN_H__
#define MAIN_H__ 1

#include "driver.h"
#include "typedefs.h"

#include <stdarg.h>

/* --- Variables --- */

extern int d_flag;
extern Bool disable_timers_flag;
extern Bool comp_flag;
extern Bool strict_euids;
extern Bool share_variables;
extern Bool allow_filename_spaces;
extern long time_to_reset;
extern long time_to_cleanup;
extern long time_to_swap;
extern long time_to_swap_variables;
extern long alarm_time;
extern long heart_beat_interval;
extern Bool synch_heart_beats;
extern Bool heart_beats_enabled;
extern svalue_t const0, const1;
extern double avg_consts[5];
extern char *mud_lib;
extern char *erq_file;
extern char **erq_args;
extern char master_name[];
extern string_t * master_name_str;
extern char *debug_file;
extern object_t dummy_current_object_for_loads;
extern int slow_shut_down_to_do;
extern Bool reopen_debug_log;
extern mp_int boot_time;
extern long time_to_data_cleanup;

#ifdef DEBUG
extern Bool check_a_lot_ref_counts_flag;
extern int check_state_level;
#endif

#ifdef CHECK_OBJECT_STAT
extern Bool check_object_stat;
#endif

extern int port_numbers[];
extern int numports;

extern int udp_port;

extern Bool compat_mode;
extern p_int regex_package;

extern char input_escape;

extern int exit_code;

/* --- Prototypes --- */

extern int main(int argc, char **argv);
extern void initialize_master_uid(void);
extern void debug_message(const char *, ...) FORMATDEBUG(printf, 1, 2);
extern void vdebug_message(const char *, va_list) FORMATDEBUG(printf, 1, 0);

extern void write_X (int d, unsigned char i);
extern void write_x(int d, p_uint i);
extern void writed(int d, p_uint i);
extern void writes (int d, const char *s);
extern char *dprintf_first(int fd, char *s, p_int a);
extern void dprintf1(int fd, char *s, p_int a);
extern void dprintf2(int fd, char *s, p_int a, p_int b);
extern void dprintf3(int fd, char *s, p_int a, p_int b, p_int c);
extern void dprintf4(int fd, char *s, p_int a, p_int b, p_int c, p_int d);
extern void set_cloexec_flag(int fd);

#endif /* MAIN_H__ */
