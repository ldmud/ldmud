#include <stdio.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <ctype.h>
#ifdef __STDC__
#include <stdarg.h>
#endif
#ifdef NeXT
#include <sys/param.h>
#endif
#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "config.h"
#include "lex.h"
#include "patchlevel.h"
#include "wiz_list.h"   /* this is not a real dependency */
#include "rxcache.h"
#include "stralloc.h"

#ifdef AMIGA
#include "hosts/amiga/socket.h"
#endif

extern char *prog;

extern int current_line;

int d_flag = 0;	/* Run with debug */
int t_flag = 0;	/* Disable heart beat and reset */
int e_flag = 0;	/* Load empty, without castles. */
int comp_flag = 0; /* Trace compilations */
#ifdef DEBUG
int check_a_lot_ref_counts_flag = 0;
#endif
long time_to_swap = TIME_TO_SWAP; /* marion - invocation parameter */
long time_to_swap_variables = TIME_TO_SWAP_VARIABLES;
#ifdef D_FLAG
int D_flag;	/* Log specific trace messages to /log/D_TRACE */
#endif

#ifdef YYDEBUG
extern int yydebug;
#endif

#ifndef MAXNUMPORTS
int port_number = PORTNO;
#else
int port_numbers[MAXNUMPORTS] = {PORTNO};
int numports = 0;
#endif
#ifdef CATCH_UDP_PORT
int udp_port = CATCH_UDP_PORT;
#endif

int out_of_memory = 0;
int malloc_privilege = MALLOC_USER;
char *reserved_user_area = 0, *reserved_master_area = 0,
     *reserved_system_area = 0; /* reserved for malloc()  */
mp_int reserved_user_size = RESERVED_USER_SIZE; /* for statistical purposes */
mp_int reserved_master_size = RESERVED_MASTER_SIZE;
mp_int reserved_system_size = RESERVED_SYSTEM_SIZE;
#ifdef MAX_MALLOCED
mp_int max_malloced = MAX_MALLOCED;
mp_int max_small_malloced = MAX_SMALL_MALLOCED;
#endif

struct svalue const0, const1;
extern struct svalue assoc_shared_string_key;

double consts[5];

extern struct error_recovery_info toplevel_error_recovery_info;

extern struct object *master_ob;

char *mud_lib; /* store the path so that it can be restored later quickly */

char master_name[100] = MASTER_NAME;

char *debug_file;

extern struct wiz_list default_wizlist_entry;

struct object dummy_current_object_for_loads = {
  0, /* flags */
  0, /* light */
  0, /* next_reset */
  0, /* time of ref */
  1, /* ref */
#ifdef DEBUG
  0, /* extra ref */
#endif
  0, /*  struct program *prog; */
  0, /*  char *name; */
  0,0,0, /* struct object *next_all, *next_inv, *next_hash; */
  0, /*  struct object *contains; */
  0, /*  struct object *super;            * Which object surround us ? */
  0, /*  struct sentence *sent;		  */
  &default_wizlist_entry, /*  user        * What wizard defined this object */
};
	/* To have an object to assign array usage to */

int main(argc, argv)
    int argc;
    char **argv;
{
    extern void init_shared_strings();
    extern void init_otable();
    extern void init_closure_hooks();
    extern void start_erq_demon();
    extern void initialize_host_ip_number();
    void initialize_master_uid();
    extern int game_is_being_shut_down;
    extern int current_time;
    extern int32 initial_eval_cost, eval_cost, assigned_eval_cost;
    extern struct svalue walk_mapping_string_svalue;

    int i, new_mudlib = 0;
    int no_erq_demon = 0;
    char *p;
#ifdef MALLOC_gc
    extern void gc_init();
#endif

#ifdef MALLOC_gc
    gc_init();
#endif
    init_interpret();
#ifdef RXCACHE_TABLE
    rxcache_init();
#endif
    const0.type = T_NUMBER; const0.u.number = 0;
    const1.type = T_NUMBER; const1.u.number = 1;
    assoc_shared_string_key.type = T_STRING;
    assoc_shared_string_key.x.string_type = STRING_SHARED;
    /*
     * Check that the definition of EXTRACT_UCHAR() is correct.
     */
    p = (char *)&i;
    *p = -10;
    if (EXTRACT_UCHAR(p) != 0x100 - 10) {
	fprintf(stderr, "Bad definition of EXTRACT_UCHAR() in config.h.\n");
	exit(1);
    }
#ifdef DRAND48
    srand48(get_current_time());
#else
#ifdef RANDOM
    srandom(get_current_time());
#else
#ifdef RAND
    srand(get_current_time());
#else
    seed_random(get_current_time());
#endif /* RAND */
#endif /* RANDOM */
#endif /* DRAND48 */
    current_time = get_current_time();

    HOST_DEPENDENT_INIT

    /*
     * The flags are parsed twice !
     * The first time, we search for the -m flag, which specifies
     * another mudlib, -Dmacro flags & port specification, so that they
     * will be available when compiling master.c , -N, -rsize and the -o flag.
     */
    for (i=1; i < argc; i++) {
#ifndef MAXNUMPORTS
	if (atoi(argv[i]))
	    port_number = atoi(argv[i]);
#else
  	if (atoi(argv[i]) && numports < MAXNUMPORTS) {
  	    port_numbers[numports++] = atoi(argv[i]);
  	}
#endif
	if (argv[i][0] != '-')
	    continue;
	switch(argv[i][1]) {
#ifdef MAXNUMPORTS
	  case 'P':
	    /* inherit a socket from priviledged program */
	    /* inherited sockets are marked as negative port numbers */
	    if (argv[i][2] && numports < MAXNUMPORTS) {
		port_numbers[numports++] = -atoi(argv[i]+2);
		continue;
	    }
	    break;
#endif
	  case 'D':
	    if (argv[i][2]) { /* Amylaar : allow flags to be passed down to
					   the LPC preprocessor */
		struct lpc_predef_s *tmp;

		tmp = (struct lpc_predef_s *)
		alloca(sizeof(struct lpc_predef_s));
		if (!tmp) fatal("alloca failed\n");
		tmp->flag = argv[i]+2;
		tmp->next = lpc_predefs;
		lpc_predefs = tmp;
		continue;
	    }
	    break;
	  case 'o':
	    fprintf(stderr, "-o is an obsolete flag. Use COMPAT_MODE in config.h.\n");
	    exit(0);
	    break;
#ifdef CATCH_UDP_PORT
	  case 'u':
	    udp_port = atoi(&argv[i][2]);
	    continue;
#endif
	  case 'N':
	    no_erq_demon++; continue;
	  case 'M':
	    p = argv[i] + 2;
	    if (!*p && ++i < argc)
		p = argv[i];
	    if (strlen(p) >= sizeof(master_name)) {
	    /* master_name needs to have space for \0 */
	        fprintf(stderr, "Too long master name '%s'\n", p);
		exit(1);
	    }
	    strcpy(master_name, p);
	    continue;
	  case 'm':
	    p = argv[i] + 2;
	    if (!*p && ++i < argc)
		p = argv[i];
	    if (chdir(p) == -1) {
	        fprintf(stderr, "Bad mudlib directory: %s\n", p);
		exit(1);
	    }
	    new_mudlib = 1;
	    break;
	  case 'r':
	  {
	    mp_int *sizep;

	    p = argv[i] + 2;
	    switch(*p++) {
	      default:
		p--;
	      case 'u': sizep = &reserved_user_size; break;
	      case 'm': sizep = &reserved_master_size; break;
	      case 's': sizep = &reserved_system_size; break;
	    }
	    *sizep = strtol(p, (char**)0, 0);
	    break;
	  }
	  case 'E':
	    initial_eval_cost = -atoi(&argv[i][2]);
	    break;
	  case '-':
	    p = argv[i] + 2;
#ifdef MAX_MALLOCED
	    if ( !strncmp(p, "max_malloced", 12) ) {
		if ( !*(p+=12) ) {
		    i++;
		    p = argv[i];
		}
		max_malloced = strtol(p, (char **)0, 0);
		if (!max_malloced)
		    fatal("0 max_malloced\n");
		break;
	    }
	    if ( !strncmp(p, "max_small_malloced", 18) ) {
		if ( !*(p+=18) && ++i < argc) {
		    p = argv[i];
		}
		max_small_malloced = strtol(p, (char **)0, 0);
		break;
	    }
#endif
#ifdef DEBUG
	    if ( !strcmp(p, "check_a_lot_ref_counts") ) {
		check_a_lot_ref_counts_flag = 1;
		break;
	    }
	    if ( !strncmp(p, "gobble_descriptors", 18) ) {
		int n;

		if ( !*(p+=18) && ++i < argc) {
		    p = argv[i];
		}
		n = strtol(p, (char **)0, 0);
		while(--n >= 0) {
		    (void)dup(2);
		}
		break;
	    }
#endif
	    if ( !strcmp(p, "version") ) {
		printf("Version %5.5s%s\nRelease date: %s\ncompiled %s\n",
		   GAME_VERSION, PATCH_LEVEL LOCAL_LEVEL,
		   RELEASE_DATE,
#ifdef __STDC__
		   __DATE__
#else  /* !__STDC__ */
		   "no __DATE__ available"
#endif /* !__STDC__ */
		);
		exit(0);
	    }
#ifdef MALLOC_smalloc
	    if ( !strncmp(p, "gcollect_outfd", 14) ) {
		extern int gcollect_outfd;
		if ( !*(p+=14) ) {
		    i++;
		    p = argv[i];
		}
		if (isdigit(*p)) {
		    gcollect_outfd = strtol(p, (char **)0, 0);
		} else {
		    gcollect_outfd = ixopen3(p, O_CREAT|O_TRUNC|O_WRONLY, 0640);
		}
		break;
	    }
#endif
	    if ( !strcmp(p, "debug_file") ) {
		i++;
		debug_file = argv[i];
		break;
	    }
	    fprintf(stderr, "Unknown flag: %s\n", argv[i]);
	    exit(1);
	}
    }
#ifdef MAXNUMPORTS
      if (numports < 1)
        numports = 1;
#endif
    init_closure_hooks();
#ifdef MIN_MALLOCED
    xfree(xalloc(MIN_MALLOCED));
#endif
#ifdef MALLOC_smalloc
    if (reserved_system_size > 0)
	reserved_system_area = xalloc(reserved_system_size);
    if (reserved_master_size > 0)
	reserved_master_area = xalloc(reserved_master_size);
#endif
    if (reserved_user_size > 0)
	reserved_user_area = xalloc(reserved_user_size);
    init_shared_strings();
    walk_mapping_string_svalue.x.string_type = STRING_SHARED;
    init_otable();
    for (i=0; i < sizeof consts / sizeof consts[0]; i++)
	consts[i] = exp(- i / 900.0);
    init_num_args();
    reset_machine(1);
    CLEAR_EVAL_COST;
    if (!new_mudlib && chdir(MUD_LIB) == -1) {
        fprintf(stderr, "Bad mudlib directory: %s\n", MUD_LIB);
	exit(1);
    }
    {
#ifdef MAXPATHLEN
        char path[MAXPATHLEN];
#else
        char path[2048];
#endif
#ifdef HAVE_GETCWD
        if (!getcwd(path, sizeof(path) ))
#else
        if (!getwd(path))
#endif
	{
	    perror("get(c)wd failed");
	    fatal("must be able to obtain current directory name\n");
	}
        mud_lib = string_copy(path);
    }

    if (!no_erq_demon)
	start_erq_demon("");
    initialize_host_ip_number();

    /* if we get low on file descriptors, the descriptor for debug_message
     * should be there
     */
    debug_message((char *)(long)"");
    (void)signal(SIGFPE, SIG_IGN);
    current_object = &dummy_current_object_for_loads;
    if (setjmp(toplevel_error_recovery_info.con.text)) {
	clear_state();
	add_message("Anomaly in the fabric of world space.\n");
    } else {
	toplevel_error_recovery_info.type = ERROR_RECOVERY_BACKEND;

	master_ob = load_object(master_name, 0, 60);
    }
    current_object = master_ob;
    toplevel_error_recovery_info.type = ERROR_RECOVERY_NONE;
    if (master_ob == 0) {
	fprintf(stderr, "The file %s must be loadable.\n", master_name);
	exit(1);
    }
    /*
     * Make sure master_ob is never made a dangling pointer.
     * Look at apply_master_ob() for more details.
     */
    add_ref(master_ob, "main");
    initialize_master_uid();
    push_number(0);
    apply_master_ob(STR_INAUGURATE, 1);
    setup_print_block_dispatcher();
    for (i=1; i < argc; i++) {
	if (atoi(argv[i]))
	    ;
	else if (argv[i][0] != '-') {
	    fprintf(stderr, "Bad argument %s\n", argv[i]);
	    exit(1);
	} else {
	    /*
	     * Look at flags. -m and -o have already been tested.
	     */
	    switch(argv[i][1]) {
	    case 'f':
		push_constant_string(argv[i]+2);
		(void)apply_master_ob(STR_FLAG, 1);
		if (game_is_being_shut_down) {
		    fprintf(stderr, "Shutdown by master object.\n");
		    exit(0);
		}
		continue;
#ifdef MAXNUMPORTS
	    case 'P':
		if (argv[i][2]) {
		    continue;
		}
		fprintf(stderr, "P flag must be followed by socket number!\n");
		break;
#endif
	    case 'D':
		if (argv[i][2]) { /* Amylaar : allow flags to be passed down to
					       the LPC preprocessor */
		    continue;
		}
#ifdef D_FLAG
		D_flag++; continue;
#else
		fprintf(stderr, "'-D' is not supported\n");
		exit(1);
#endif
	    case 'e':
		e_flag++; continue;
	    case '-':
	    {
		p = argv[i] + 2;
		if (p[-1] == '-') {
		    if (!strcmp(p, "max_malloced") ||
			!strcmp(p, "max_small_malloced") ||
			!strcmp(p, "gobble_descriptors") ||
			!strcmp(p, "gcollect_outfd") ||
			!strcmp(p, "debug_file") )
		    {
			i++;
		    }
		}
		break;
	    }
	    case 'M':
	    case 'm':
		if (!argv[i][2])
		    i++;
		/* fall through */
	    case 'o':
	    case 'u':
	    case 'N':
	    case 'r':
	    case 'E':
		continue; /* these flags have been recognized above */
	    case 'd':
		d_flag++; continue;
	    case 'c':
		comp_flag++; continue;
	    case 't':
		t_flag++; continue;
#if TIME_TO_SWAP > 0
	    case 's':
		if (argv[i][2] == 'v') {
		    time_to_swap_variables = atoi (&argv[i][3]);
		    if (time_to_swap_variables < 0)
			time_to_swap_variables = (unsigned)-1>>1;
		} else if (argv[i][2] == 'f') {
		    extern void name_swap_file PROT((char*));

		    name_swap_file(&argv[i][3]);
		} else {
		    time_to_swap = atoi (&argv[i][2]);
		    if (time_to_swap < 0)
			time_to_swap = (unsigned)-1>>1;
		}
		continue;
#endif
#ifdef YYDEBUG
	    case 'y':
		yydebug = 1; continue;
#endif
	    default:
		fprintf(stderr, "Unknown flag: %s\n", argv[i]);
		exit(1);
	    }
	}
    }
#ifdef DEBUG
    if (d_flag > 1 && time_to_swap_variables + 1 == 0)
	check_a_lot_ref_counts_flag = 1;
#endif
    get_simul_efun_object();
    if (game_is_being_shut_down)
	exit(1);
    load_wiz_file();
    preload_objects(e_flag);
    backend();
    return 0;
}


void initialize_master_uid() {
    struct svalue *ret;

    ret = apply_master_ob(STR_GET_M_UID, 0);
#ifndef NATIVE_MODE
    if (ret && ret->type == T_NUMBER && ret->u.number) {
	master_ob->user = &default_wizlist_entry;
#ifdef EUIDS
	master_ob->eff_user = 0;
#endif
    } else
#endif
    if (ret == 0 || ret->type != T_STRING) {
	fprintf(stderr, "get_master_uid() in %s does not work\n", master_name);
#ifdef NATIVE_MODE
	exit(1);
#endif
    } else {
	master_ob->user = add_name(ret->u.string);
#ifdef EUIDS
	master_ob->eff_user = master_ob->user;
#endif
    }
}


#ifdef string_copy
char *_string_copy(str, file, line)
    char *str, *file;
    int line;
{
    char *p;

    p = smalloc(strlen(str)+1, file, line);
    if (p) {
	(void)strcpy(p, str);
    }
    return p;
}
#else
char *string_copy(str)
    char *str;
{
    char *p;

    p = xalloc(strlen(str)+1);
    if (p) {
	(void)strcpy(p, str);
    }
    return p;
}
#endif

#ifdef __STDC__
void debug_message(char *a, ...)
#else
/*VARARGS1*/
void debug_message(a, b, c, d, e, f, g, h, i, j)
    char *a;
    int b, c, d, e, f, g, h, i, j;
#endif
{
    static FILE *fp = NULL;
    char deb[100];
    char *file;
#ifdef __STDC__
    va_list va;
#endif

#ifdef __STDC__
    va_start(va, a);
#endif
    if (fp == NULL) {
	if ( !(file = debug_file) ) {
#ifndef MSDOS
	    sprintf(deb,"%s.debug.log", query_host_name());
#else
	    strcpy(deb,"debug.log");
#endif
	    file = deb;
	}
#ifndef AMIGA
	fp = fopen(file, "w");
#else
	fp = fopen(file, "a");
#endif
	if (fp == NULL) {
	    perror(file);
	    abort();
	}
    }
#ifdef __STDC__
    (void)vfprintf(fp, a, va);
    va_end(va);
#else
    (void)fprintf(fp, a, b, c, d, e, f, g, h, i, j);
#endif
    (void)fflush(fp);
}

#if 0
void debug_message_svalue(v)
    struct svalue *v;
{
    if (v == 0) {
	debug_message("<NULL>");
	return;
    }
    switch(v->type) {
    case T_NUMBER:
	debug_message("%ld", v->u.number);
	return;
    case T_STRING:
	debug_message("\"%s\"", v->u.string);
	return;
    case T_OBJECT:
	debug_message("OBJ(%s)", v->u.ob->name);
	return;
    case T_LVALUE:
	debug_message("Pointer to ");
	debug_message_svalue(v->u.lvalue);
	return;
    default:
	debug_message("<INVALID>\n");
	return;
    }
}
#endif

int slow_shut_down_to_do = 0;

#ifndef MALLOC_smalloc
POINTER xalloc(size)
    size_t size;
{
    char *p;
    static int going_to_exit;

    if (going_to_exit)
	exit(3);
    if (size == 0)
	fatal("Tried to allocate 0 bytes.\n");
#ifdef MAX_MALLOCED
    {
	static mp_int total_malloced = 0;

	if ((total_malloced += size + sizeof(p_int)) > max_malloced) {
	    total_malloced -= size + sizeof(p_int);
	    p = 0;
	} else {
	    p = malloc(size);
	}
    }
#else
    p = malloc(size);
#endif
    if (p == 0) {
	if (reserved_user_area) {
	    extern int garbage_collect_to_do;
	    extern int extra_jobs_to_do;

	    free(reserved_user_area);
	    p = "Temporary out of MEMORY. Freeing reserve.\n";
	    write(1, p, strlen(p));
	    reserved_user_area = 0;
	    garbage_collect_to_do = 1;
	    extra_jobs_to_do = 1;
	    return xalloc(size);	/* Try again */
	}
	/* We can hardly survive out of memory without the garbage collector */
	going_to_exit = 1;
	p = "Totally out of MEMORY.\n";
	write(1, p, strlen(p));
	(void)dump_trace(0);
	exit(2);
    }
    return p;
}
#endif /* MALLOC_smalloc */

void reallocate_reserved_areas() {
    char *p;
    malloc_privilege = MALLOC_USER;
#ifdef MALLOC_smalloc
    if (reserved_system_size && !reserved_system_area) {
	if ( !(reserved_system_area = xalloc(reserved_system_size)) ) {
	    slow_shut_down_to_do = 1;
	    return;
	}
	else {
	    p = "Reallocated System reserve.\n";
	    write(1, p, strlen(p));
	}
    }
    if (reserved_master_size && !reserved_master_area) {
	if ( !(reserved_master_area = xalloc(reserved_master_size)) ) {
	    slow_shut_down_to_do = 1;
	    return;
	}
	else {
	    p = "Reallocated Master reserve.\n";
	    write(1, p, strlen(p));
	}
    }
#endif /* MALLOC_smalloc */
    if (reserved_user_size && !reserved_user_area) {
	if ( !(reserved_user_area = xalloc(reserved_user_size)) )
	    slow_shut_down_to_do = 6;
	else {
	    p = "Reallocated User reserve.\n";
	    write(1, p, strlen(p));
	}
	    return;
    }
    slow_shut_down_to_do = 0;
}
