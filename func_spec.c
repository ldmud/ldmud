#include "config.h"
/*
 * This file specifies types and arguments for efuns.
 * An argument can have two different types with the syntax 'type1 | type2'.
 * An argument is marked as optional if it also takes the type 'void'.
 *
 * Look at the end for the list of functions that are optionally available.
 * If you don't want them, simply comment out them. All other functions must
 * remain defined.
 */
string *regexp(string *, string);
void add_action(string, void|string, void|int);
void add_verb(string);
void add_worth(int, void|object);
void add_xverb(string);
object *all_inventory(object default: F_THIS_OBJECT);
mixed *allocate(int);
mixed assoc(mixed, mixed *, mixed|void, mixed|void);
void break_point();
unknown call_other(object|string, string, ...);
void call_out(string, int, void|mixed);
mixed *call_out_info();
string capitalize(string);
int cat(string, void|int, void|int);
int cindent(string);
string clear_bit(string, int);
object clone_object(string);
int command(string, void|object);
string crypt(string, string|int);	/* An int as second argument ? */
string ctime(int);
mixed debug_info(int, mixed|void, ...);
object *deep_inventory(object);
void destruct(object);
void disable_commands();
void ed(void|string, void|string);
void enable_commands();
object environment(void|object);
int exec(object, object);
string *explode(string, string);
string extract(string, void|int, void|int);
string file_name(object default: F_THIS_OBJECT);
int file_size(string);
mixed *filter_array(mixed *, string, object|string, void|mixed);
int find_call_out(string);
object find_living(string);
object find_object(string);
object find_player(string);
string function_exists(string, object default: F_THIS_OBJECT);
string implode(string *, string);
void input_to(string, void|int);
mixed insert_alist(mixed, mixed, ...);
int interactive(object default: F_THIS_OBJECT);
mixed *intersect_alist(mixed *,mixed *);
int intp(mixed);
int living(object);
void localcmd();
void log_file(string, string);
string lower_case(string);
string *get_dir(string);
mixed *map_array(mixed *, string, object|string, void|mixed);
int member_array(mixed, mixed *);
int mkdir(string);
void move_object(object|string, object|string);
void notify_fail(string);
int objectp(mixed);
mixed *order_alist(mixed *, void|mixed *, ...);
int pointerp(mixed);
object present(object|string, void|object);
object previous_object();
string process_string(string);
string query_host_name();
int query_idle(object);
string query_ip_name(void|object);
string query_ip_number(void|object);
string query_load_average();
object query_snoop(object);
string query_verb();
int random(int);
string read_bytes(string, void|int, void|int);
string read_file(string, void|int, void|int);
int remove_call_out(string);
int restore_object(string);
int rm(string);
void rmdir(string);
#ifdef RUSAGE
string rusage();
#endif
void save_object(string);
void say(string|mixed *, void|object|object *);
string set_bit(string, int);
int set_heart_beat(int);
int set_light(int);
void set_living_name(string);
object shadow(object, int);
void shout(string);
void shutdown();
int sizeof(mixed *);
object snoop(void|object, void|object);
mixed *sort_array(mixed *,string,object|string default: F_THIS_OBJECT);
int stringp(mixed);
int strlen(string);
void swap(object);		/* Only used for debugging */
void tail(string);
void tell_object(object, string);
void tell_room(object|string, string, void|object *);
int test_bit(string, int);
object this_object();
object this_player(void|int);
void throw(mixed);
int time();
int trace(int);
string traceprefix(string|int);
mixed *unique_array(mixed *, string, void|mixed);
object *users();
string version();
void wizlist(void|string);
void write(mixed);
int write_bytes(string, int, string);
int write_file(string, string);
string *inherit_list(object default: F_THIS_OBJECT);

/*
 * List of functions only used in compatibility mode.
 */
#ifdef COMPAT_MODE
string creator(object);
int transfer(object, object|string);
string create_wizard(string, void|string);
#endif

/*
 * List of functions only used in native mode.
 */
#ifndef COMPAT_MODE
int rename(string, string);
int export_uid(object);
string geteuid(object default: F_THIS_OBJECT);
string getuid(object default: F_THIS_OBJECT);
int seteuid(string|int);
#endif

#ifdef MALLOC_malloc
/*
 * This one is needed if you use the old malloc.c, which is no longer
 * supplied. It is called from church.c in the original 2.4.5 mudlib. This
 * call is not needed if malloc.c is not used, and should be removed.
 */
void combine_free_list();
#endif

/*
 *
 * The following functions are optional. Comment out the ones not wanted.
 * Beware that some may be used in mudlib 2.4.5.
 *
 */
object first_inventory(object|string default: F_THIS_OBJECT);
object next_inventory(object default: F_THIS_OBJECT);
