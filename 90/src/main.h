#ifndef __MAIN_H__
#define __MAIN_H__ 1

#include "driver.h"

#include "interpret.h"  /* struct svalue */

/* --- Variables --- */

extern int d_flag;
extern /* TODO: BOOL */ int t_flag;
extern int e_flag;
extern /* TODO: BOOL */ int comp_flag;
extern long time_to_swap;
extern long time_to_swap_variables;
extern struct svalue const0, const1;
extern double consts[5];
extern char *mud_lib;
extern char master_name[];
extern char *debug_file;
extern struct object dummy_current_object_for_loads;
extern int slow_shut_down_to_do;

/* TODO: This should go into the memory handler */
extern /* TODO: BOOL */ int out_of_memory;
extern int malloc_privilege;
extern char *reserved_user_area;
extern char *reserved_master_area;
extern char *reserved_system_area;
extern mp_int reserved_user_size;
extern mp_int reserved_master_size;
extern mp_int reserved_system_size;
#ifdef MAX_MALLOCED
extern mp_int max_malloced;
extern mp_int max_small_malloced;
#endif

#ifdef DEBUG
extern /* TODO: BOOL */ int check_a_lot_ref_counts_flag;
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

#if defined(MALLOC_smalloc) && defined(SMALLOC_TRACE) && defined(__STDC__)
#define string_copy(s) (_string_copy(s, __FILE__ "::string_copy", __LINE__))
extern char *_string_copy(const char *, const char *, int);
#else
extern char *string_copy(const char *str);
#endif


/* TODO: This should go into the malloc-files */
#ifndef MALLOC_smalloc
extern POINTER xalloc(size_t size);
#endif

void reallocate_reserved_areas(void);

extern void writex(int d, p_uint i);
extern void writed(int d, p_uint i);

#endif /* __MAIN_H__ */
