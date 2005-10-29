#include <stdio.h>
#include <string.h>
#include "lnode.h"
#include "interpret.h"
#include "object.h"
#include "config.h"

struct value const0, const1;

extern struct lnode *prog;

extern int current_line;

extern void *malloc();
extern void backend(), perror(), exit(), fatal(), remove_all_players(),
       load_wiz_file();

extern long time();

int d_flag = 0;	/* Run with debug */
int t_flag = 0;	/* Disable heart beat and reset */
int e_flag = 0;	/* Load empty, without castles. */
#ifdef YYDEBUG
extern int yydebug;
#endif

int port_number = PORTNUM;

int main(argc, argv)
    int argc;
    char **argv;
{
    if (argc == 2) {
	if (strcmp(argv[1], "-e") == 0)
	    e_flag++;
	else if (strcmp(argv[1], "-d") == 0)
	    d_flag++;
	else if (strcmp(argv[1], "-t") == 0)
	    t_flag++;
#ifdef YYDEBUG
	else if (strcmp(argv[1], "-y") == 0)
	    yydebug = 1;
#endif
	else port_number = atoi(argv[1]);
    }
#ifdef DRAND48
    srand48(time(0));
#else
#ifdef RANDOM
    srandom(time(0));
#else
    fprintf(stderr, "No random generator specified!\n");
#endif /* RANDOM */
#endif /* DRAND48 */
    init_string_space();
    chdir(MUD_LIB);
    compute_lnode_size();
    load_wiz_file();
    const0.type = T_NUMBER;
    const0.u.number = 0;
    const1.type = T_NUMBER;
    const1.u.number = 1;
    backend();
    return 0;
}

void *xalloc(size)
    int size;
{
    static out_of_mem;	/* Prevent double call to remove_all_players() */
    char *p;

#ifdef free
    p = (char *)malloc(size+4);
#else
    p = (char *)malloc(size);
#endif
    if (p == 0) {
	if (out_of_mem)
	    exit(0);
	out_of_mem = 1;
	remove_all_players();
	fprintf(stderr, "Out of memory.\n");
    }
#ifdef free
    *(int *)p = 0x45872300;	/* Just a MAGIC "unique" number */
    p += 4;
#endif
    return (void *)p;
}

char *string_copy(str)
    char *str;
{
    char *p;

    p = (char *)xalloc(strlen(str)+1);
    (void)strcpy(p, str);
    return p;
}

/*VARARGS1*/
void debug_message(a, b, c, d, e, f, g, h, i, j)
    char *a;
    int b, c, d, e, f, g, h, i, j;
{
    static FILE *fp = NULL;
#define DEBUG_LOG   "debug.log"
    if (fp == NULL) {
	fp = fopen(DEBUG_LOG, "w");
	if (fp == NULL) {
	    perror(DEBUG_LOG);
	    abort();
	}
    }
    (void)fprintf(fp, a, b, c, d, e, f, g, h, i, j);
    (void)fflush(fp);
}

void debug_message_value(v)
    struct value *v;
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
    default:
	fatal("<INVALID>\n");
	return;
    }
}

#ifdef free
void xfree(p)
    char *p;
{
    p -= 4;
    if (*(int *)p == 0x45872300) {
	*(int *)p = 0x45872338;
	return;				/* Won't free when debugging */
    }
    fatal("Bad pointer to free: 0x%x\n", *(int *)p);
}
#endif
