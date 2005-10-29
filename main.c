#include <stdio.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>
#if defined(sun)
#include <alloca.h>
#endif
#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "config.h"
#include "lex.h"

extern char *prog;

extern int current_line;

int d_flag = 0;	/* Run with debug */
int t_flag = 0;	/* Disable heart beat and reset */
int e_flag = 0;	/* Load empty, without castles. */
int comp_flag = 0; /* Trace compilations */
long time_to_swap = TIME_TO_SWAP; /* marion - invocation parameter */

#ifdef YYDEBUG
extern int yydebug;
#endif

int port_number = PORTNO;
char *reserved_area;	/* reserved for malloc() */
struct svalue const0, const1;

double consts[5];

extern jmp_buf error_recovery_context;
extern int error_recovery_context_exists;

extern struct object *master_ob;

struct wiz_list *back_bone_uid;

static void start_ip_demon();

int main(argc, argv)
    int argc;
    char **argv;
{
    extern int game_is_being_shut_down;
    extern int current_time;
    int i, new_mudlib = 0;
    int no_ip_demon = 0;
    char *p;
#ifndef COMPAT_MODE
    struct svalue *ret;
#endif
#ifdef MALLOC_gc
    extern void gc_init();
#endif

#ifdef MALLOC_gc
    gc_init();
#endif
    const0.type = T_NUMBER; const0.u.number = 0;
    const1.type = T_NUMBER; const1.u.number = 1;
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
    fprintf(stderr, "No random generator specified!\n");
#endif /* RANDOM */
#endif /* DRAND48 */
    current_time = get_current_time();;
    if (RESERVED_SIZE > 0)
	reserved_area = malloc(RESERVED_SIZE);
    for (i=0; i < sizeof consts / sizeof consts[0]; i++)
	consts[i] = exp(- i / 900.0);
    init_num_args();
    reset_machine(1);

    /*
     * The flags are parsed twice !
     * The first time, we only search for the -m flag, which specifies
     * another mudlib, and the D-flags, so that they will be available
     * when compiling master.c.
     */
    for (i=1; i < argc; i++) {
	if (argv[i][0] != '-')
	    continue;
	switch(argv[i][1]) {
	case 'D':
	    if (argv[i][2]) { /* Amylaar : allow flags to be passed down to
				 the LPC preprocessor */
		struct lpc_predef_s *tmp;
		
		tmp = (struct lpc_predef_s *)
		    alloca(sizeof(struct lpc_predef_s));
		if (!tmp) fatal("alloca failed");
		tmp->flag = argv[i]+2;
		tmp->next = lpc_predefs;
		lpc_predefs = tmp;
		continue;
	    }
	    fprintf(stderr, "Illegal flag syntax: %s\n", argv[i]);
	    exit(1);
	case 'o':
	    fprintf(stderr, "-o is an obsolete flag. Use COMPAT_MODE in config.h.\n");
	    exit(0);
	    break;
	case 'N':
	    no_ip_demon++; continue;
	case 'm':
	    if (chdir(argv[i]+2) == -1) {
	        fprintf(stderr, "Bad mudlib directory: %s\n", argv[i]+2);
		exit(1);
	    }
	    new_mudlib = 1;
	    break;
	}
    }
    if (!new_mudlib && chdir(MUD_LIB) == -1) {
        fprintf(stderr, "Bad mudlib directory: %s\n", MUD_LIB);
	exit(1);
    }

#ifndef NO_IP_DEMON
    if (!no_ip_demon)
	start_ip_demon();
#endif

    if (setjmp(error_recovery_context)) {
	clear_state();
	add_message("Anomaly in the fabric of world space.\n");
    } else {
	error_recovery_context_exists = 1;

#ifdef COMPAT_MODE
	    master_ob = load_object("obj/master",0);
#else
	    master_ob = load_object("secure/master",0);
#endif	
    }
    error_recovery_context_exists = 0;
    if (master_ob == 0) {
	fprintf(stderr, "The file secure/master must be loadable.\n");
	exit(1);
    }
    /*
     * Make sure master_ob is never made a dangling pointer.
     * Look at apply_master_ob() for more details.
     */
    add_ref(master_ob, "main");
#ifndef COMPAT_MODE
    ret = apply_master_ob("get_root_uid", 0);
    if (ret == 0 || ret->type != T_STRING) {
	fprintf(stderr, "get_root_uid() in secure/master.c does not work\n");
	exit(1);
    }
    master_ob->user = add_name(ret->u.string);
    master_ob->eff_user = master_ob->user;
    ret = apply_master_ob("get_bb_uid", 0);
    if (ret == 0 || ret->type != T_STRING) {
	fprintf(stderr, "get_bb_uid() in secure/master.c does not work\n");
	exit(1);
    }
    back_bone_uid = add_name(ret->u.string);
#endif
    for (i=1; i < argc; i++) {
	if (atoi(argv[i]))
	    port_number = atoi(argv[i]);
	else if (argv[i][0] != '-') {
	    fprintf(stderr, "Bad argument %s\n", argv[i]);
	    exit(1);
	} else {
	    /*
	     * Look at flags. -m and -o has already been tested.
	     */
	    switch(argv[i][1]) {
	    case 'f':
		push_constant_string(argv[i]+2);
		(void)apply_master_ob("flag", 1);
		if (game_is_being_shut_down) {
		    fprintf(stderr, "Shutdown by master object.\n");
		    exit(0);
		}
		continue;
	    case 'e':
		e_flag++; continue;
	    case 'D':
		continue;
	    case 'N':
		continue;
	    case 'm':
		continue;
	    case 'd':
		d_flag++; continue;
	    case 'c':
		comp_flag++; continue;
	    case 't':
		t_flag++; continue;
#if TIME_TO_SWAP > 0
	    case 's':
		time_to_swap = atoi (&argv[i][2]);
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
    get_simul_efun(apply_master_ob("get_simul_efun", 0));
    if (game_is_being_shut_down)
	exit(1);
    load_wiz_file();
    set_inc_list(apply_master_ob("define_include_dirs", 0));
#ifdef COMPAT_MODE
	load_first_objects();
	(void)apply_master_ob("epilog", 0);
#else
	preload_objects(e_flag);
#endif
    backend();
    return 0;
}

char *string_copy(str)
    char *str;
{
    char *p;

    p = xalloc(strlen(str)+1);
    (void)strcpy(p, str);
    return p;
}

/*VARARGS1*/
void debug_message(a, b, c, d, e, f, g, h, i, j)
    char *a;
    int b, c, d, e, f, g, h, i, j;
{
    static FILE *fp = NULL;
    char deb[100];
    char name[100];

    if (fp == NULL) {
#ifndef MSDOS
	gethostname(name,sizeof name);
	sprintf(deb,"%s.debug.log",name);
#else
	strcpy(deb,"debug.log");
#endif
	fp = fopen(deb, "w");
	if (fp == NULL) {
	    perror(deb);
	    abort();
	}
    }
    (void)fprintf(fp, a, b, c, d, e, f, g, h, i, j);
    (void)fflush(fp);
}

void debug_message_svalue(v)
    struct svalue *v;
{
    if (v == 0) {
	debug_message("<NULL>");
	return;
    }
    switch(v->type) {
    case T_NUMBER:
	debug_message("%d", v->u.number);
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

#ifdef malloc
#undef malloc
#endif

int slow_shut_down_to_do = 0;

char *xalloc(size)
    int size;
{
    char *p;
    static int going_to_exit;

    if (going_to_exit)
	exit(3);
    if (size == 0)
	fatal("Tried to allocate 0 bytes.\n");
    p = malloc(size);
    if (p == 0) {
	if (reserved_area) {
	    free(reserved_area);
	    p = "Temporary out of MEMORY. Freeing reserve.\n";
	    write(1, p, strlen(p));
	    reserved_area = 0;
	    slow_shut_down_to_do = 6;
	    return xalloc(size);	/* Try again */
	}
	going_to_exit = 1;
	p = "Totally out of MEMORY.\n";
	write(1, p, strlen(p));
	(void)dump_trace(0);
	exit(2);
    }
    return p;
}

#ifndef NO_IP_DEMON
static void start_ip_demon()
{
    extern FILE *f_ip_demon, *f_ip_demon_wr;
    char path[100];
    int tochild[2], fromchild[2];
    int pid;
    char c;

    if(pipe(tochild) < 0)
        return;
    if(pipe(fromchild) < 0) {
        close(tochild[0]);
        close(tochild[1]);
        return;
    }
    if((pid = fork()) == 0) {
        /* Child */
        dup2(tochild[0], 0);
        dup2(fromchild[1], 1);
        close(tochild[0]);
        close(tochild[1]);
        close(fromchild[0]);
        close(fromchild[1]);
	if (strlen(BINDIR) + 7 <= sizeof path) {
	    sprintf(path, "%s/hname", BINDIR);
	    execl((char *)path, "hname", 0);
	}
	write(1, "0", 1);	/* indicate failure */
        fprintf(stderr, "exec of hname failed.\n");
        _exit(1);
    }
    if(pid == -1) {
        close(tochild[0]);
        close(tochild[1]);
        close(fromchild[0]);
        close(fromchild[1]);
        return;
    }
    close(tochild[0]);
    close(fromchild[1]);
    read(fromchild[0], &c, 1);
    if (c == '0') {
        close(tochild[1]);
        close(fromchild[0]);
	return;
    }
    f_ip_demon_wr = fdopen(tochild[1], "w");
    f_ip_demon = fdopen(fromchild[0], "r");
    if (f_ip_demon == NULL || f_ip_demon_wr == NULL) {
	f_ip_demon = NULL;
        close(tochild[1]);
        close(fromchild[0]);
    }
}
#endif
