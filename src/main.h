#ifndef MAIN_H__
#define MAIN_H__ 1

#include "driver.h"
#include "typedefs.h"

#include <stdarg.h>

/* --- Variables --- */

extern int d_flag;
extern Bool t_flag;
extern Bool comp_flag;
extern Bool strict_euids;
extern long time_to_reset;
extern long time_to_cleanup;
extern long time_to_swap;
extern long time_to_swap_variables;
extern svalue_t const0, const1;
extern double consts[5];
extern char *mud_lib;
extern char master_name[];
extern char *debug_file;
extern object_t dummy_current_object_for_loads;
extern int slow_shut_down_to_do;

#ifdef DEBUG
extern Bool check_a_lot_ref_counts_flag;
extern int check_state_level;
#endif

#ifdef CHECK_STRINGS
extern Bool check_string_table_flag;
#endif

extern int port_numbers[];
extern int numports;

#ifdef CATCH_UDP_PORT
extern int udp_port;
#endif

/* --- Prototypes --- */

extern int main(int argc, char **argv);
extern void initialize_master_uid(void);
extern void debug_message(char *, ...) FORMATDEBUG(printf, 1, 2);
extern void vdebug_message(char *, va_list);

void reallocate_reserved_areas(void);

extern void write_x(int d, p_uint i);
extern void writed(int d, p_uint i);
extern char *dprintf_first(int fd, char *s, p_int a);
extern void dprintf1(int fd, char *s, p_int a);
extern void dprintf2(int fd, char *s, p_int a, p_int b);
extern void dprintf3(int fd, char *s, p_int a, p_int b, p_int c);


#endif /* MAIN_H__ */
