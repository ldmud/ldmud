#ifndef LINT_H
#define LINT_H

#include "config.h"

#include <errno.h>
#ifdef __SASC
#include <sys/errno.h>
extern int errno;
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else /* !HAVE_ALLOCA_H */
#ifdef __GNUC__
#ifndef alloca
#define alloca(size) __builtin_alloca(size)
#endif
#else /* !__GNUC__ */
#ifdef _AIX
 #pragma alloca
#endif /* _AIX */
#endif /* !__GNUC__ */
#endif /* !HAVE_ALLOCA_H */

/*
 * Some structure forward declarations are needed.
 */
struct program;
struct function;
struct svalue;
struct sockaddr;
struct variable;

#ifndef LANG
struct s_lrvalue { struct s_lrvalue *fake_member; };
#endif

#ifdef BUFSIZ
#    define PROT_STDIO(x) PROT(x)
#else /* BUFSIZ */
#    define PROT_STDIO(x) ()
#endif /* BUFSIZ */

#ifdef __STDC__
#    define PROT(x) x
#else /* __STDC__ */
#    define PROT(x) ()
#endif /* __STDC */

#if defined(__GNUC__) && __GNUC__ >= 2 && (__GNUC_MINOR__ > 5 || __GNUC__ > 2)
#define NORETURN __attribute__ ((noreturn))
#else
#define NORETURN
#endif

#ifdef __GNUC__
#define FORMATDEBUG(f,a,b) __attribute__ ((format (f,a,b)))
#else
#define FORMATDEBUG(f,a,b)
#endif

#ifdef __STDC__
#define VARPROT(proto,like,form,var) proto FORMATDEBUG(like,form,var)
#else
#define VARPROT(proto, like,form,var) ()
#endif

#if (defined(__GNUC__) || defined(inline)) && !defined(DEBUG)
#define INLINE inline
#else
#define INLINE
#endif

#if defined(__GNUC__) && !defined(DEBUG) && defined(INTERPRET)
#define INTER_INLINE inline
#else
#define INTER_INLINE
#endif

#if defined(__GNUC__) && !defined(DEBUG) && defined(STRALLOC)
#define STRALLOC_INLINE inline
#else
#define STRALLOC_INLINE
#endif

#define _MCTe 0x01 /* escaped character in save/restore object. */
#define _MCTd 0x02 /* numeric digit		*/


#define _MCTs 0x10 /* whitespace EXCLUDING '\n'	*/

#define _MCTx 0x40 /* hexadecimal		*/
#define _MCTa 0x80 /* alphanumeric or '_' 	*/
extern unsigned char _my_ctype[];
#define isescaped(c) (_my_ctype[(unsigned char)(c)]&_MCTe)
#define isalunum( c) (_my_ctype[(unsigned char)(c)]&_MCTa)
#define lexdigit( c) (_my_ctype[(unsigned char)(c)]&_MCTd)

#if defined(sun) && !defined(solaris)
#define SunOS4
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBC_H
#include <libc.h>
#endif
#if !defined(STDC_HEADERS) && defined(HAVE_MEMORY_H)
#include <memory.h>
#endif
#ifdef HAVE_VALUES_H
#include <values.h>
#endif
#ifdef AMIGA
#include "hosts/amiga/patchfloat.h"
#endif /* AMIGA */
#ifdef ATARI_TT
#include <math-688.h>
#define _MATH_H
#endif
#include <math.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#include <sys/types.h>
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif
#if defined(AMIGA) && defined(_DCC)
extern int current_time;
#define MY_TRUE (current_time)
#define MY_FALSE (!current_time)
/* DICE v2.06 pessimizes constant pointer arithmetic,
 * assuming 0 as difference of constant pointers. */
#define PTRTYPE unsigned long
#else
#define MY_TRUE (1)
#define MY_FALSE (0)
#define PTRTYPE char *
#endif

#if defined(MALLOC_smalloc) && !defined(MAKE_FUNC)
#if !defined( SMALLOC ) || defined( SBRK_OK )
#undef malloc
#endif
#undef calloc
#ifdef SBRK_OK
#define amalloc         malloc
#define afree           free
#else  /* SBRK_OK */
POINTER amalloc PROT((size_t));
POINTER smalloc_calloc PROT((size_t, size_t));
FREE_RETURN_TYPE afree PROT((POINTER));
#ifndef SMALLOC
#define malloc  amalloc
#endif
#define calloc  smalloc_calloc
#define free    afree
#endif /* SBRK_OK */
void xfree PROT((POINTER));
POINTER rexalloc PROT((POINTER, size_t));
#if MALLOC_ALIGN > SIZEOF_P_INT || FREE_NULL_POINTER
#define PFREE_RETURN_TYPE void
#define PFREE_RETURN return;
PFREE_RETURN_TYPE pfree PROT((POINTER));
#else  /* MALLOC_ALIGN */
#define PFREE_RETURN_TYPE FREE_RETURN_TYPE
#define PFREE_RETURN FREE_RETURN
#define pfree  afree
#endif /* MALLOC_ALIGN */
POINTER permanent_xalloc PROT((size_t));
PFREE_RETURN_TYPE pfree PROT((POINTER));
#else  /* MALLOC_smalloc */
#define xfree free
#define rexalloc realloc
#define amalloc         xalloc
#define permanent_xalloc xalloc
#define afree           free
#define pfree		free
#endif /* MALLOC_smalloc */

#if defined(SunOS4) || defined (ultrix)
void bzero PROT((char *, int));
#endif
#if defined(SunOS4)
/* These prototypes used to have a wider scope, but I suspect they are
 * only needed on suns.
 */
void srandom PROT((int));
char *_crypt PROT((char *, char *));
int ioctl PROT((int, ...)); /* should be in <ioctl.h> */
#endif
#if defined(SunOS4) || defined(ultrix)
int gethostname PROT((char *, int));
#endif
#if defined(SunOS4) || defined(ultrix)
char *getdomainname PROT((char *, int));
#endif
#ifdef SunOS4
/* cc won't see the types inside PROT(), because it isn't ansi. gcc < 2.5
 * hasn't these prototypes, but 2.5 has, thus types must match.
 */
extern int rename PROT((const char *, const char *));
extern void perror PROT((const char *));
extern long int strtol PROT((const char *, char **, int));
#endif

#ifdef DRAND48
double drand48 PROT((void));
#endif
#ifdef RANDOM
long random PROT((void));
#endif

/* apple unix does prototype memmove, but there is no such function in the
 * library. Moreover, bcopy() won't handle overlapping right there.
 */
#ifndef HAVE_MEMMEM
char *memmem PROT((char *, size_t, char *, size_t));
#endif
#if !defined(HAVE_MEMMOVE) && !defined(OVERLAPPING_BCOPY)
void move_memory PROT((char *, char *, size_t));
#endif
#if ((!defined(HAVE_CRYPT) && !defined(HAVE__CRYPT))) || \
    (defined(sgi) && !defined(_MODERN_C)) || defined(ultrix)
char *crypt PROT((const char *, const char *));
#endif

struct object;
void save_error PROT((char *, char *, int));
int write_file PROT((char *, char *));
int file_size PROT((char *));
void remove_all_players PROT((void));
void load_wiz_file PROT((void));
void wizlist PROT((char *));
void backend PROT((void));
#if defined(MALLOC_smalloc) && defined(SMALLOC_TRACE)
#define xalloc(size) (smalloc((size), __FILE__, __LINE__))
POINTER smalloc PROT((size_t, char *, int));
#ifdef __STDC__
#define string_copy(s) (_string_copy(s, __FILE__ "::string_copy", __LINE__))
#define allocate_array(n) (_allocate_array(n, __FILE__ "::allocate_array", __LINE__))
#define allocate_uninit_array(n) (_allocate_array(n, __FILE__ "::allocate_uninit_array", __LINE__))
#define implode_string(a,d) (_implode_string(a,d, __FILE__ "::implode_string", __LINE__))
struct vector;
char *_string_copy PROT((char *, char *, int));
struct vector *_allocate_array PROT((mp_int, char *, int));
struct vector *_allocate_uninit_array PROT((mp_int, char *, int));
char *_implode_string PROT((struct vector *, char *, char *, int));
#endif /* __STDC__ */
#endif /* SMALLOC_TRACE */
#ifndef xalloc
POINTER xalloc PROT((size_t));
#endif
#ifndef string_copy
char *string_copy PROT((char *));
#endif
void init_string_space PROT((void));
/*VARARGS1*/
void error VARPROT((char *, ...), printf, 1, 2) NORETURN;
/*VARARGS1*/
void add_message VARPROT((char *, ...), printf, 1, 2);
/*VARARGS1*/
void yyerrorf VARPROT((char *format, ...), printf, 1, 2);
/*VARARGS1*/
void fatal VARPROT((char *, ...), printf, 1, 2) NORETURN;
/*VARARGS1*/
void debug_message VARPROT((char *, ...), printf, 1, 2);
void debug_message_value PROT((struct svalue *)),
	print_local_commands(),
	list_files PROT((char *)),
	enable_commands PROT((int));
struct svalue *new_call_out PROT((struct svalue *, int));
int load_ob_from_swap PROT((struct object *));
int load_line_numbers_from_swap PROT((struct program *));
int add_action PROT((struct svalue *, struct svalue *, int));
int tail PROT((char *));
void enter_object_hash PROT((struct object *));
void remove_object_hash PROT((struct object *));
struct object *lookup_object_hash PROT((char *));
int show_otable_status PROT((int verbose));
void dumpstat PROT((void));
struct vector;
void free_vector PROT((struct vector *));
void set_vector_user PROT((struct vector *, struct object *ob));
void free_empty_vector PROT((struct vector *));
char *query_load_av PROT((void));
void update_compile_av PROT((int));
void map_array PROT((
		       struct vector *arr,
		       char *func,
		       struct object *ob,
		       int num_extra,
		       struct svalue *extra
		       ));
struct vector *order_alist PROT((struct svalue *, int, int));
struct vector *make_unique PROT((struct vector *arr,char *func,
    struct svalue *skipnum));

char *describe_items PROT((struct svalue *arr,char *func,int live));
struct svalue *filter PROT((struct svalue *sp, int num_arg));
int match_string PROT((char *, char *, mp_int));
int set_heart_beat PROT((struct object *, int));
struct svalue;

/* When you expect special lvalues, use assign_svalue/transfer_svalue . */
void assign_svalue PROT((struct svalue *, struct svalue *));
void transfer_svalue PROT((struct svalue *, struct svalue *));

INTER_INLINE void
  assign_svalue_no_free   PROT((struct svalue *to, struct svalue *from)),
  transfer_svalue_no_free PROT((struct svalue *to, struct svalue *from)),
  free_string_svalue      PROT((struct svalue *));
void free_svalue PROT((struct svalue *));
void set_svalue_user PROT((struct svalue *, struct object *));
void assign_eval_cost();
void bad_efun_arg PROT((int, int, struct svalue *)) NORETURN;
void bad_xefun_arg PROT((int, struct svalue *)) NORETURN;
#define bad_efun_vararg bad_xefun_arg
void bad_xefun_vararg PROT((int, struct svalue *)) NORETURN;
char *make_shared_string PROT((char *));
char *findstring PROT((char *));
STRALLOC_INLINE void decrement_string_ref PROT((char *));
void free_string PROT((char *));
struct lambda *lambda PROT((struct vector *, struct svalue *, struct object *));
void free_closure PROT((struct svalue *));
void set_closure_user PROT((struct svalue *, struct object *));
void call_lambda PROT((struct svalue *, int));
struct svalue *secure_call_lambda PROT((struct svalue *, int));
int add_string_status PROT((int verbose));
void notify_no_command PROT((char *));
void clear_notify PROT((void));
void free_notifys PROT((void));
void throw_error PROT((void));
int lookup_predef PROT((char *));
char *lex_error_context();
void yyerror PROT((char *));
int hashstr PROT((char *, int, int));
int whashstr PROT((char *, int));
int chashstr PROT((char *, int));
int lookup_predef PROT((char *));
char *dump_trace PROT((int));
int parse_command PROT((char *, struct object *));
struct svalue *apply PROT((char *, struct object *, int));
struct svalue *secure_apply PROT((char *, struct object *, int));
struct svalue *sapply PROT((char *, struct object *, int));
int find_function PROT((char *, struct program *));
void push_string_malloced PROT((char *));
void push_string_shared   PROT((char *));
void push_malloced_string PROT((char *));
void push_shared_string   PROT((char *));
void push_number PROT((p_int));
void push_object PROT((struct object *));
void push_referenced_vector PROT((struct vector *));
INTER_INLINE void pop_stack PROT((void));
void drop_stack PROT((void));
int _privilege_violation PROT((char *, struct svalue *, struct svalue *));
int privilege_violation4
	PROT((char *, struct object *, char *, int, struct svalue *));
struct object *clone_object PROT((char *));
void init_num_args PROT((void));
int restore_object PROT((struct object *, char *));
void tell_object PROT((struct object *, char *));
struct object *first_inventory PROT((struct svalue *));
struct vector *slice_array PROT((struct vector *,int,int));
int query_idle PROT((struct object *));
char *implode_string PROT((struct vector *, char *));
struct object *query_snoop PROT((struct object *));
struct vector *all_inventory PROT((struct object *));
struct vector *deep_inventory PROT((struct object *, int));
struct object *environment PROT((struct svalue *));
struct vector *add_array PROT((struct vector *, struct vector *));
char *get_f_name PROT((int));

struct svalue *f_shutdown PROT((struct svalue *));
void startmasterupdate ();

void set_notify_fail_message PROT((struct svalue *));
int swap PROT((struct object *, int));
int swap_program PROT((struct object *));
int swap_variables PROT((struct object *));
void set_swapbuf PROT((char *));
int transfer_object PROT((struct svalue *));
struct vector *users PROT((void));
void do_write PROT((struct svalue *));
char *create_wizard PROT((char *, char *));
void destruct_object PROT((struct svalue *));
void emergency_destruct PROT((struct object *));
void set_snoop PROT((struct object *, struct object *));
int new_set_snoop PROT((struct object *, struct object *));
void ed_start PROT((char *, char *, struct object *));
void say PROT((struct svalue *, struct vector *));
void tell_room PROT((struct object *, struct svalue *, struct vector *));
int command_for_object PROT((char *, struct object *));
int remove_file PROT((char *));
int print_file PROT((char *, int, int));
int print_call_out_usage PROT((int verbose));
struct svalue *input_to PROT((struct svalue *, int));
int parse PROT((char *, struct svalue *, char *, struct svalue *, int));
struct object *object_present PROT((struct svalue *, struct object *));
void add_light PROT((struct object *, int));
int indent_program PROT((char *));
void call_function PROT((struct program *, int));
void store_line_number_info PROT((void));
void store_include_info PROT((char *));
void store_include_end PROT((void));
void push_volatile_string PROT((char *));
void store_line_number_relocation PROT((int));
#define push_constant_string(str) push_volatile_string(str)
void push_svalue PROT((struct svalue *));
void push_lrvalue PROT((struct svalue *));
void push_svalue_block PROT((int, struct svalue *));
void free_prog PROT((struct program *, int));
int heart_beat_status PROT((int verbose));
void opcdump PROT((void));
void slow_shut_down PROT((int));
struct vector *allocate_array PROT((mp_int));
struct vector *allocate_uninit_array PROT((mp_int));
void reset_machine PROT((int));
void clear_state PROT((void));
void load_first_objects PROT((void));
void preload_objects PROT((int));
mp_uint random_number PROT((mp_uint));
void seed_random PROT((int));
void reset_object PROT((struct object *, int));
int replace_interactive PROT((struct object *ob, struct object *obf, char *));
char *get_wiz_name PROT((char *));
mp_int get_current_time PROT((void));
char *time_string PROT((int));
char *limit_error_format PROT((char *fixed_fmt, char *fmt));
char *process_string PROT((char *));
int32 renumber_programs();
void count_ref_from_call_outs PROT((void));
void clear_ref_from_call_outs PROT((void));
void count_ref_in_vector PROT((struct svalue *svp, int num));
void clear_ref_in_vector PROT((struct svalue *svp, int num));
void count_ref_from_string PROT((char *));
void clean_stale_mappings();
void remove_stale_call_outs();
void reference_destructed_object PROT((struct object *));
void note_malloced_block_ref PROT((char *p));
struct ed_buffer;
void clear_ed_buffer_refs PROT((struct ed_buffer *b));
void count_ed_buffer_refs PROT((struct ed_buffer *b));
void clear_ref_from_wiz_list();
void count_ref_from_wiz_list();
void free_unreferenced_memory();
void remove_unknown_identifier();
void count_lex_refs();
void count_compiler_refs();
void free_defines();
void free_all_local_names();
void clear_simul_efun_refs();
void count_simul_efun_refs();
void note_shared_string_table_ref();
void note_otable_ref();
void clear_comm_refs();
void count_comm_refs();
void clear_interpreter_refs();
void count_interpreter_refs();
void count_heart_beat_refs();
void count_extra_ref_in_vector PROT((struct svalue *svp, mp_int num));
void count_extra_ref_from_wiz_list();
void count_simul_efun_extra_refs();
void count_comm_extra_refs();
void count_inherits PROT((struct program *));
void count_ed_buffer_extra_refs PROT((struct ed_buffer *b));
void count_extra_ref_in_object PROT((struct object *));
void check_a_lot_ref_counts PROT((struct program *));
int shadow_catch_message PROT((struct object *ob, char *str));
struct vector *get_all_call_outs PROT((void));
char *read_file PROT((char *file, int, int));
char *read_bytes PROT((char *file, int, int));
int write_bytes PROT((char *file, int, char *str));
struct wiz_list *add_name PROT((char *str));
char *check_valid_path PROT((char *, struct object *, char *, int));
int privilege_violation PROT((char *, struct svalue *));
int get_line_number_if_any PROT((char **));
void logon PROT((struct object *ob));
struct svalue *apply_master_ob PROT((char *fun, int num_arg));
void assert_master_ob_loaded();
void free_interpreter_temporaries();
struct vector *explode_string PROT((char *str, char *del));
struct vector *new_explode_string PROT((char *str, char *del));
void find_call_out PROT((struct object *ob, struct svalue *fun, int));
void remove_object_from_stack PROT((struct object *ob));
void compile_file PROT((void));
void unlink_swap_file();
char *function_exists PROT((char *, struct object *));
void set_inc_list PROT((struct vector *));
int legal_path PROT((char *path));
struct vector *get_dir PROT((char *path, int));
struct object *get_simul_efun_object PROT((void));
struct function *find_simul_efun PROT((char *));
char *query_simul_efun_file_name PROT((void));
struct vector *match_regexp PROT((struct vector *v, char *pattern));
void setup_print_block_dispatcher();
char *query_host_name();
void init_telopts();
void mudlib_telopts();

#ifndef MAXINT
#define MAXINT (0x7fffffff)
#endif
#define MALLOC_USER   (0)
#define MALLOC_MASTER (1)
#define MALLOC_SYSTEM (2)

#endif /* LINT_H */
