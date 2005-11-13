/*---------------------------------------------------------------------------
 * LPMud Driver main module.
 *
 *---------------------------------------------------------------------------
 * Here is the main() function which parses the commandline arguments,
 * initializes everything and starts the backend loop. A documentation
 * of the available commandline arguments is in the file
 * doc/driver/invocation; the argument '--help' causes the driver to
 * print a short help to all available arguments.
 *
 * The commandline arguments are actually parsed twice, since some
 * arguments expect a functional master object (like the 'f' argument).
 *
 * The argument parser is an adaption of a generic parser. All the associated
 * code and comments is kept in the lower half of this source for better
 * readability.
 *
 * This file also contains several global functions and variables, some of
 * which should probably go into a dedicated 'global' module or somewhere else
 * appropriate.
 * TODO: Move out all those variables and functions which are illfitting here.
 * TODO: Put the argument parsing into a separate file?
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include "my-alloca.h"
#include <stdio.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <ctype.h>
#include <stdarg.h>
#ifdef NeXT
#include <sys/param.h>
#endif
#ifdef AMIGA
#include "hosts/amiga/socket.h"
#endif

#define NO_INCREMENT_STRING_REF
#include "main.h"

#include "backend.h"
#include "array.h"
#include "comm.h"
#include "filestat.h"
#include "gcollect.h"
#include "interpret.h"
#include "lex.h"
#include "mapping.h"
#include "object.h"
#include "otable.h"
#include "patchlevel.h"
#include "random.h"
#include "rxcache.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stralloc.h"
#include "swap.h"
#include "wiz_list.h"

/*-------------------------------------------------------------------------*/

/* -- Pure commandline arguments -- */

int d_flag    = 0;  /* Debuglevel */
/* TODO: BOOL */ int t_flag    = MY_FALSE;  /* True: Disable heart beat and reset */
int e_flag    = MY_FALSE;  /* Passed to preload(), usually disables it */
/* TODO: BOOL */ int comp_flag = MY_FALSE;  /* Trace compilations */
#ifdef DEBUG
/* TODO: BOOL */ int check_a_lot_ref_counts_flag = MY_FALSE;  /* The name says it. */
#endif

long time_to_swap           = TIME_TO_SWAP;
long time_to_swap_variables = TIME_TO_SWAP_VARIABLES;
  /* A value <= 0 disables the swapping. */

#ifndef MAXNUMPORTS
int port_number = PORTNO;
#else
int port_numbers[MAXNUMPORTS] = { PORTNO };
  /* The login port numbers.
   * Negative numbers are not ports, but the numbers of inherited
   * socket file descriptors.
   */
int numports = 0;  /* Number of specified ports */
#endif

#ifdef CATCH_UDP_PORT
int udp_port = CATCH_UDP_PORT;  /* Port number for UDP */
#endif

char *mud_lib;                        /* Path to the mudlib */
char master_name[100] = MASTER_NAME;  /* Name of the master object */

static int new_mudlib = 0;    /* True: mudlib directory was specified */
static int no_erq_demon = 0;  /* True: don't start the erq */

#ifdef MAX_TIME
int max_time = MAX_TIME;  /* Time in seconds to spend on one evaluation */
#endif

/* -- Global Variables/Arguments dealing with memory -- */

/* TODO: BOOL */ int out_of_memory = MY_FALSE;              /* True: we are out of memory */
int malloc_privilege = MALLOC_USER; /* Priviledge for next allocation */

char *reserved_user_area   = NULL;  /* Reserved memory areas */
char *reserved_master_area = NULL;
char *reserved_system_area = NULL;

mp_int reserved_user_size   = RESERVED_USER_SIZE;   /* The reserved sizes */
mp_int reserved_master_size = RESERVED_MASTER_SIZE;
mp_int reserved_system_size = RESERVED_SYSTEM_SIZE;

#ifdef MAX_MALLOCED
mp_int max_malloced       = MAX_MALLOCED;           /* Allocation limits */
mp_int max_small_malloced = MAX_SMALL_MALLOCED;
#endif

/* -- Other Global Variables -- */
struct svalue const0, const1;
  /* The values 0 and 1 as svalues, mem-copied when needed */

double consts[5];  /* Weight constants used to compute average figures */

char *debug_file;  /* Name of the debug log file. */

struct object dummy_current_object_for_loads;
  /* Dummy object for functions, which need a current_object though
   * there is none. This is usually the case when (re)loading the
   * master object.
   */

int slow_shut_down_to_do = 0;
  /* If non-zero, the game should perform a graceful shutdown.
   * The value is the number of minutes to still last before shutting down.
   */

/*-------------------------------------------------------------------------*/

/* Forward declarations for the argument parser in the lower half */

static int getargs (int argc, char ** argv, int (*opt_eval)(int, const char *) );
static int firstscan (int, const char *);
static int secondscan (int, const char *);

/*-------------------------------------------------------------------------*/
int
main (int argc, char **argv)

/* The main function. Nuff said. */

{
    int i;
    char *p;

    /* Initialisations */

    init_interpret();
#ifdef RXCACHE_TABLE
    rxcache_init();
#endif

    const0.type = T_NUMBER; const0.u.number = 0;
    const1.type = T_NUMBER; const1.u.number = 1;
    assoc_shared_string_key.type = T_STRING;
    assoc_shared_string_key.x.string_type = STRING_SHARED;

    current_time = get_current_time();
    seed_random(current_time);

    dummy_current_object_for_loads = NULL_object;
#ifdef DEBUG
    if (dummy_current_object_for_loads.user)
    {
        fprintf(stderr, "Assigning NULL_object does not clear the target.\n");
        exit(1);
    }
#endif
    dummy_current_object_for_loads.ref = 1;
    dummy_current_object_for_loads.user = &default_wizlist_entry;

    /*
     * Check that the definition of EXTRACT_UCHAR() is correct.
     */
    p = (char *)&i;
    *p = -10;
    if (EXTRACT_UCHAR(p) != 0x100 - 10) {
        fprintf(stderr, "Bad definition of EXTRACT_UCHAR() in config.h.\n");
        exit(1);
    }

#ifdef HOST_DEPENDENT_INIT
    HOST_DEPENDENT_INIT
#endif

    /* First scan of the arguments.
     * This evaluates everything but the 'f' arguments.
     */
    if (getargs(argc, argv, firstscan))
      exit(1);

#ifdef MAXNUMPORTS
    if (numports < 1) /* then use the default port */
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
    init_otable();
    for (i = 0; i < (int)(sizeof consts / sizeof consts[0]); i++)
        consts[i] = exp(- i / 900.0);
    init_lexer();
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

        master_ob = get_object(master_name);
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

    /* Second scan of the arguments, now we're looking just for
     * the 'f' flag.
     */
    if (getargs(argc, argv, secondscan))
        exit(1);

#ifdef DEBUG
    if (d_flag > 1 && time_to_swap_variables <= 0)
        check_a_lot_ref_counts_flag = MY_TRUE;
#endif

    get_simul_efun_object();
    if (game_is_being_shut_down)
        exit(1);

    load_wiz_file();
    preload_objects(e_flag);

    /* Start the backend loop. This won't return before
     * the game shuts down.
     */
    backend();

    return 0;
}


/*-------------------------------------------------------------------------*/
void initialize_master_uid (void)

/* After loading the master object, determine its (e)uid by calling the
 * lfun get_master_uid() in it. For details, better read the code.
 */

{
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


/*-------------------------------------------------------------------------*/

/* string_copy() acts like strdup() with the additional bonus that it can
 * trace file/line of the calling place if SMALLOC_TRACE is defined.
 */

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

/*-------------------------------------------------------------------------*/
void
debug_message(char *a, ...)

/* Print a message into the debug logfile, printf() style.
 */

{
    static FILE *fp = NULL;
    char deb[100];
    char *file;
    va_list va;

    va_start(va, a);
    if (fp == NULL) {
        if ( !(file = debug_file) ) {
            sprintf(deb,"%s.debug.log", query_host_name());
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
    (void)vfprintf(fp, a, va);
    va_end(va);
    (void)fflush(fp);
}

/*-------------------------------------------------------------------------*/
#ifndef MALLOC_smalloc
POINTER
xalloc (size_t size)

/* Allocate <size> bytes of memory like malloc() does.
 * This function catches out of memory situations and tries to recover
 * from them by using the reserved memory areas. If it totally runs
 * out of memory, the program exit()s with code 3.
 */

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
            free(reserved_user_area);
            p = "Temporary out of MEMORY. Freeing reserve.\n";
            write(1, p, strlen(p));
            reserved_user_area = 0;
            garbage_collect_to_do = MY_TRUE;
            extra_jobs_to_do = MY_TRUE;
            return xalloc(size);        /* Try again */
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

/*-------------------------------------------------------------------------*/
void
reallocate_reserved_areas (void)

/* Try to reallocate the reserved memory areas. If this is possible,
 * a pending slow-shutdown is canceled.
 */

{
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
        if ( !(reserved_user_area = xalloc(reserved_user_size)) ) {
            slow_shut_down_to_do = 6;
            return;
        }
        else {
            p = "Reallocated User reserve.\n";
            write(1, p, strlen(p));
        }
    }
    slow_shut_down_to_do = 0;
}

/*-------------------------------------------------------------------------*/
void
writex (int d, p_uint i)

/* Memory safe function to write hexvalue <i> to fd <d>. */

{
    int j;
    char c;

    for (j = 2 * sizeof i; --j >= 0; i <<= 4) {
        c = (i >> (8 * sizeof i - 4) ) + '0';
        if (c >= '9' + 1)
            c += 'a' - ('9' + 1);
        write(d, &c, 1);
    }
}

/*-------------------------------------------------------------------------*/
void
writed (int d, p_uint i)

/* Memory safe function to write integer value <i> to fd <d>. */

{
    p_uint j;
    char c;

    for (j = 1000000000; j > i; j /= 10) NOOP;
    if (!j) j = 1;
    do {
        c = (i / j) % 10 + '0';
        write(d, &c, 1);
        j /= 10;
    } while (j > 0);
}

/*=========================================================================*/
/*                        The argument parser                              */
/*=========================================================================*/
/* This code parses the arguments passed to the program in the count <argc>
 * and the array of strings <argv>. The parser distinguishes options, which
 * start with a '-', from normal arguments; options are further distinguished
 * by their name and may take an additional value. The parser neither
 * imposes nor expects any order of options and arguments.
 *
 * Options are recognized in two forms. In the short form the option must
 * be given as a single '-' followed by a single letter. In the long form,
 * options start with '--' followed by a string of arbitrary length.
 * Short options are case sensitive, long options aren't.
 * Examples are: '-r' and '--recursive'.
 *
 * If an option takes a value, it must follow the option immediately after
 * a separating space or '='. Additionally, the value for a short option
 * may follow the option without separator. Examples are: '-fMakefile',
 * '-f Makefile', '--file=Makefile' and '--file Makefile'.
 *
 * Short options may be collated into one argument, e.g. '-rtl', but
 * of these only the last may take a value.
 *
 * The option '--' marks the end of options. All following command arguments
 * are considered proper arguments even if they start with a '-' or '--'.
 *-------------------------------------------------------------------------
 * Internally every option recognized by the program is associated with
 * an id number, defined as the enum OptNumber. The parser itself uses the
 * two id numbers 'cUnknown' for unrecognized options, and 'cArgument' for
 * proper command arguments.
 *
 * Id numbers are associated with their option strings/letters by the
 * statically initialized arrays aShortOpts and aLongOpts. Every element
 * of these two arrays is a structure defining the option's name (string
 * or letter), the associated id number, and whether or not the option
 * takes a value. The order of the elements does not matter.
 *
 * The parsing is done by calling the function
 *
 *   int getargs(int argc, char ** argv, int (*)handler(int, const char *))
 *
 * The function is passed the argument count <argc> and vector <argv> as
 * they were received from the main() function, and a callback function
 * <handler>. getargs() returns 0 if the parsing completed successfully,
 * and non-zero else.
 *
 * The handler function is called for every successfully recognized option
 * and argument. Its prototype is
 *
 *   int handler(int eOption, const char *pValue)
 *
 * Parameter <eOption> denotes the recognized option, and pValue points
 * to the beginning of the value string if the option takes a value.
 * Proper arguments are parsed with eOption==cArgument and pValue
 * pointing to the argument string. The handler has to return 0 if the
 * option/argument was processed correctly, and non-zero else.
 *-------------------------------------------------------------------------
 */
/* Desription of short ('-') options */

typedef struct ShortOpt {
  char      cOption;  /* The option character */
  int       eNumber;  /* The associated option number */
  short     bValue;   /* True: takes a value */
} ShortOpt;

/* Desription of long ('--') options */

typedef struct LongOpt {
  char      * pOption;  /* The option string */
  int         eNumber;  /* The associated option number */
  short       bValue;   /* True: takes a value */
} LongOpt;

/* Every recognized option has a ordinal number */

typedef enum OptNumber {
   cUnknown = 0   /* unknown option                     */
 , cArgument      /* normal argument (for us: filename) */
 , cInherited     /* --inherit            */
#ifdef CATCH_UDP_PORT
 , cUdpPort       /* --udp                */
#endif
 , cTrace         /* --list-compiles      */
 , cDebug         /* --debug              */
 , cDefine        /* --define             */
 , cEvalcost      /* --eval-cost          */
 , cFuncall       /* --funcall            */
 , cMaster        /* --master             */
 , cMudlib        /* --mudlib             */
 , cDebugFile     /* --debug-file         */
#ifdef MAX_MALLOCED
 , cMaxMalloc     /* --max-malloc         */
 , cMaxSmall      /* --max-small-malloc   */
#endif
#ifdef MAX_TIME
 , cMaxTime       /* --max-time           */
#endif
 , cNoERQ         /* --no-erq             */
 , cNoHeart       /* --no-heart           */
 , cNoPreload     /* --no-preload         */
 , cReserved      /* -r                   */
 , cReserveUser   /* --reserve-user       */
 , cReserveMaster /* --reserve-master     */
 , cReserveSystem /* --reserve-system     */
#if TIME_TO_SWAP > 0
 , cSwap          /* -s                   */
 , cSwapTime      /* --swap-time          */
 , cSwapVars      /* --swap-variables     */
 , cSwapFile      /* --swap-file          */
 , cSwapCompact   /* --swap-compact       */
#endif
#ifdef MALLOC_smalloc
 , cGcollectFD    /* --gcollect-outfd     */
#endif
#ifdef DEBUG
 , cCheckRefs     /* --check-refcounts    */
 , cGobbleFDs     /* --gobble-descriptors */
#endif
#ifdef YYDEBUG
 , cYYDebug       /* --yydebug            */
#endif
 , cOptions       /* --options            */
 , cVersion       /* --version            */
 , cLongHelp      /* --longhelp           */
 , cHelp          /* --help               */
} OptNumber;

/* Comprehensive lists of recognized options */

static ShortOpt aShortOpts[]
  = { { 'c', cTrace,     MY_FALSE }
    , { 'D', cDefine,    MY_TRUE }
    , { 'd', cDebug,     MY_FALSE }
    , { 'E', cEvalcost,  MY_TRUE }
    , { 'e', cNoPreload, MY_FALSE }
    , { 'f', cFuncall,   MY_TRUE }
    , { 'M', cMaster,    MY_TRUE }
    , { 'm', cMudlib,    MY_TRUE }
    , { 'N', cNoERQ,     MY_FALSE }
    , { 'P', cInherited, MY_TRUE }
    , { 'r', cReserved,  MY_TRUE }
#if TIME_TO_SWAP > 0
    , { 's', cSwap,      MY_TRUE }
#endif
    , { 't', cNoHeart,   MY_FALSE }
#ifdef CATCH_UDP_PORT
    , { 'u', cUdpPort,   MY_TRUE }
#endif
#ifdef YYDEBUG
    , { 'y', cYYDebug,   MY_FALSE }
#endif
    , { 'V', cVersion,   MY_FALSE }
    , { 'h', cHelp,      MY_FALSE }
    , { '?', cHelp,      MY_FALSE }
    };

static LongOpt aLongOpts[]
  = { { "debug",              cDebug,         MY_FALSE }
    , { "define",             cDefine,        MY_TRUE }
    , { "debug-file",         cDebugFile,     MY_TRUE }
    , { "debuf_file",         cDebugFile,     MY_TRUE } /* TODO: COMPAT */
    , { "eval-cost",          cEvalcost,      MY_TRUE }
    , { "funcall",            cFuncall,       MY_TRUE }
    , { "list-compiles",      cTrace,         MY_FALSE }
    , { "master",             cMaster,        MY_TRUE }
    , { "mudlib",             cMudlib,        MY_TRUE }
#ifdef MAX_MALLOCED
    , { "max-malloc",         cMaxMalloc,     MY_TRUE }
    , { "max-small-malloc",   cMaxSmall,      MY_TRUE }
    , { "max_malloced",       cMaxMalloc,     MY_TRUE } /* TODO: COMPAT */
    , { "max_small_malloced", cMaxSmall,      MY_TRUE } /* TODO: COMPAT */
#endif
#ifdef MAX_TIME
    , { "max-time",           cMaxTime,       MY_TRUE }
#endif
    , { "inherit",            cInherited,     MY_TRUE }
    , { "no-erq",             cNoERQ,         MY_FALSE }
    , { "no-heart",           cNoHeart,       MY_FALSE }
    , { "no-preload",         cNoPreload,     MY_FALSE }
    , { "reserved-user",      cReserveUser,   MY_TRUE }
    , { "reserved-master",    cReserveMaster, MY_TRUE }
    , { "reserved-system",    cReserveSystem, MY_TRUE }
#if TIME_TO_SWAP > 0
    , { "swap-time",          cSwapTime,      MY_TRUE }
    , { "swap-variables",     cSwapVars,      MY_TRUE }
    , { "swap-file",          cSwapFile,      MY_TRUE }
    , { "swap-compact",       cSwapCompact,   MY_FALSE }
#endif
#ifdef MALLOC_smalloc
    , { "gcollect-outfd",     cGcollectFD,    MY_TRUE }
    , { "gcollect_outfd",     cGcollectFD,    MY_TRUE } /* TODO: COMPAT */
#endif
#ifdef CATCH_UDP_PORT
    , { "udp",                cUdpPort,       MY_TRUE }
#endif
#ifdef DEBUG
    , { "check-refcounts",    cCheckRefs,     MY_TRUE }
    , { "gobble-descriptors", cGobbleFDs,     MY_TRUE }
    , { "check_a_lot_of_ref_counts", cCheckRefs, MY_TRUE } /* TODO: COMPAT */
    , { "gobble_descriptors", cGobbleFDs,     MY_TRUE }    /* TODO: COMPAT */
#endif
#ifdef YYDEBUG
    , { "yydebug",            cYYDebug,       MY_FALSE }
#endif
    , { "options",            cOptions,       MY_FALSE }
    , { "version",            cVersion,       MY_FALSE }
    , { "longhelp",           cLongHelp,      MY_FALSE }
    , { "help",               cHelp,          MY_FALSE }
    };

/*-------------------------------------------------------------------------*/
static void
version (void)

/* Print the version of the gamedriver.
 */

{
  fputs("LDMud ", stdout);

  if (IS_RELEASE())
      fputs(GAME_VERSION, stdout);
  else
      fputs(LONG_VERSION, stdout);

  fputs(LOCAL_LEVEL " - a LPMud Game Driver.\n"
        "\nRelease:  " PROJ_VERSION ", " RELEASE_DATE
        "\nCompiled: " __DATE__
#ifdef __TIME__
        " " __TIME__
#endif
        "\n"
       , stdout);
}

/*-------------------------------------------------------------------------*/
static void
options (void)

/* Print the version of the gamedriver and the compile time options.
 */

{
  version();
  fputs("\n           Mode: "
#ifdef COMPAT_MODE
#    ifdef NATIVE_MODE
        "Compat+Native"
#    else
        "Compat"
#    endif
#elif defined(NATIVE_MODE)
        "Native"
#else
        "Plain (aka cross-compat)"
#endif
#ifdef EUIDS
        " with euids.\n"
#else
        ", no euids.\n"
#endif
       , stdout);

  fputs("    Mudlib path: " MUD_LIB "\n"
        "    Binary path: " BINDIR "\n"
        "  Master object: <mudlib>/" MASTER_NAME "\n"
       , stdout);

#ifdef MAXNUMPORTS
  printf(" Multiple ports: %d ports max, default is %d.\n", MAXNUMPORTS, PORTNO);
#else
  printf("    Single port: default is %d\n", PORTNO);
#endif

#ifdef CATCH_UDP_PORT
#    ifdef UDP_SEND
  printf("            UDP: default port is %d.\n", CATCH_UDP_PORT);
#    else
  printf("            UDP: recv only, default port is: %d.\n", CATCH_UDP_PORT);
#    endif
#else
  fputs("            UDP: disabled.\n", stdout);
#endif

#ifdef ERQ_DEMON
  printf("            ERQ: max data length: send %d / recv %d bytes.\n"
         "                 directory: %s.\n"
        , ERQ_MAX_SEND, ERQ_MAX_REPLY, ERQ_DIR);
#else
  fputs("            ERQ: disabled.\n", stdout);
#endif

#ifdef ACCESS_CONTROL
  fputs(" Access control: using <mudlib>/" ACCESS_FILE
#    ifdef ACCESS_LOG
        ", logs into <mudlib>/" ACCESS_LOG "\n"
#    else
        ", no logs.\n"
#    endif
        , stdout);
#else
  fputs(" Access control: disabled.\n", stdout);
#endif

  fputs("       Language: "
#ifdef SUPPLY_PARSE_COMMAND
                         "parse_command() enabled\n"
#endif
#ifdef INITIALIZATION_BY___INIT
        "                 initialization by __INIT()\n"
#else
        "                 static initialization\n"
#endif
       , stdout);

  printf(" Runtime limits: max log size:          %7d\n"
         "                 max read file size:    %7d\n"
         "                 max byte read/write:   %7d\n"
         "                 max socket buf size:   %7d\n"
         "                 max eval cost:         %7d\n"
         "                 catch eval cost:       %7d\n"
         "                 master eval cost:      %7d\n"
#ifdef MAX_TIME
         "                 max eval time:         %7d\n"
#endif
         "                 eval stack:            %7d\n"
         "                 user call depth:       %7d\n"
         "                 max call depth:        %7d\n"
         "                 max bitfield length:   %7d\n"
         "                 max array size:        %7d\n"
         "                 max number players:    %7d\n"
         "                 ed cmd/cmd ratio:      %7d:1\n"
#if defined(TRACE_CODE)
         "                 max trace length:      %7d\n"
#endif
        , MAX_LOG_SIZE, READ_FILE_MAX_SIZE, MAX_BYTE_TRANSFER
        , SET_BUFFER_SIZE_MAX
        , MAX_COST, CATCH_RESERVED_COST, MASTER_RESERVED_COST
#ifdef MAX_TIME
        , MAX_TIME
#endif
        , EVALUATOR_STACK_SIZE
        , MAX_USER_TRACE, MAX_TRACE
        , MAX_BITS, MAX_ARRAY_SIZE
        , MAX_PLAYERS
        , ALLOWED_ED_CMDS /* MAX_CMDS_PER_BEAT is not implemented */
#ifdef TRACE_CODE
        , TOTAL_TRACE_LENGTH
#endif
        );

  printf("         Timing: reset:                 %7d s (granularity %d s)\n"
         "                 clean up:              %7d s\n"
        , TIME_TO_RESET, RESET_GRANULARITY, TIME_TO_CLEAN_UP
        );

#if TIME_TO_SWAP > 0
  printf("       Swapping: objects             after %4d s\n"
         "                 variables           after %4d s\n"
        , TIME_TO_SWAP, TIME_TO_SWAP_VARIABLES
        );
  if (SWAP_FILE[0] == '/')
      printf("                 file: %s.<host>\n"
            , SWAP_FILE
            );
  else
      printf("                 file: <mudlib>/%s.<host>\n"
            , SWAP_FILE
            );
#else
  fputs("Swapping disabled.\n", stdout);
#endif

  printf("       Compiler: max stack size:         %6d\n"
         "                 max local variables:    %6d\n"
         "                 max define length:      %6d\n"
#ifdef ALIGN_FUNCTIONS
         "                 functions are aligned.\n"
#endif
        , COMPILER_STACK_SIZE
        , MAX_LOCAL
        , DEFMAX
        );

  printf("         Memory: using %s\n"
         "                 reserved user size:   %8d\n"
         "                 reserved master size: %8d\n"
         "                 reserved system size: %8d\n"
#ifdef MIN_MALLOCED
         "                 initial allocation:   %8d\n"
#endif
#if defined(MALLOC_smalloc) && defined(MAX_MALLOCED)
         "                 max allocation:       %8d\n"
         "                 max small allocation: %8d\n"
#endif
#ifdef MALLOC_sysmalloc
        , "system malloc"
#elif defined(MALLOC_smalloc)
        , "smalloc"
#    if defined(SMALLOC_TRACE)
#        if defined(SMALLOC_LPC_TRACE)
                  " (trace enabled, lpc-trace enabled)"
#        else
                  " (trace enabled)"
#        endif
#    elif defined(SMALLOC_LPC_TRACE)
                  " (lpc-trace enabled)"
#    endif
#else
        , "unknown malloc"
#endif
        , RESERVED_USER_SIZE
        , RESERVED_MASTER_SIZE
        , RESERVED_SYSTEM_SIZE
#ifdef MIN_MALLOCED
        , MIN_MALLOCED
#endif
#if defined(MALLOC_smalloc) && defined(MAX_MALLOCED)
        , MAX_MALLOCED, MAX_SMALL_MALLOCED
#endif
        );

  printf("Internal tables: shared string hash:     %6d entries\n"
         "                 object hash:            %6d entries\n"
         "                 reserved name hash:     %6d entries\n"
         "                 apply cache:            %6d entries\n"
#ifdef RXCACHE_TABLE
         "                 regexp cache:           %6d entries\n"
#endif
        , HTABLE_SIZE
        , OTABLE_SIZE
        , ITABLE_SIZE
        , 1<<APPLY_CACHE_BITS
#ifdef RXCACHE_TABLE
        , RXCACHE_TABLE
#endif
        );

    /* Print the other options, nicely formatted. */
    {
        char * optstrings[] = { "  Other options: "
#       if defined(DEBUG)
                              , "DEBUG"
#       endif
#       if defined(YYDEBUG)
                              , "YYDEBUG"
#       endif
#       if defined(TRACECODE)
                              , "TRACECODE"
#       endif
#       if defined(COMM_STAT)
                              , "COMM_STAT"
#       endif
#       if defined(APPLY_CACHE_STAT)
                              , "APPLY_CACHE_STAT"
#       endif
#       if defined(OPCPROF)
                              , "OPC_PROF"
#           if defined(OPCPROF_VERBOSE)
                              , "OPCPROF_VERBOSE"
#           endif
#       endif
                              };
        size_t nStrings = sizeof(optstrings) / sizeof(optstrings[0]);
        size_t iInitial = strlen(optstrings[0]);
        size_t curlen = 0;
        size_t i;

        if (nStrings > 1)
        {
            fputs(optstrings[0], stdout);
            curlen = iInitial;

            for (i = 1; i < nStrings; i++)
            {
                curlen += strlen(optstrings[i]) + 2;
                if (curlen > 78)
                {
                    printf("\n%*s", (int)iInitial, " ");
                    curlen = iInitial;
                }
                fputs(optstrings[i], stdout);
                if (i < nStrings-1)
                    fputs(", ", stdout);
            }
            fputs(".\n", stdout);
        }
    }
}

/*-------------------------------------------------------------------------*/
static void
shortusage (void)

/* Print the short help information to stdout. */

{
  version();
  fputs("\n"
#ifdef MAXNUMPORTS
"Usage: driver [options] [<portnumber>...]\n"
#else
"Usage: driver [options] [<portnumber>]\n"
#endif
"\nOptions are:\n"
"\n"
#ifdef MAXNUMPORTS
"  -P|--inherit <fd-number>\n"
#endif
#ifdef CATCH_UDP_PORT
"  -u|--udp <portnumber>\n"
#endif
"  -D|--define <macro>[=<text>]\n"
"  -E|--eval-cost <ticks>\n"
"  -M|--master <filename>\n"
"  -m|--mudlib <pathname>\n"
"  --debug-file <filename>\n"
"  -d|--debug\n"
"  -c|--list-compiles\n"
"  -e|--no-preload\n"
"  -N|--no-erq\n"
"  -t|--no-heart\n"
"  -f|--funcall <word>\n"
#if TIME_TO_SWAP > 0
"  -s <time>  | --swap-time <time>\n"
"  -s v<time> | --swap-variables <time>\n"
"  -s f<name> | --swap-file <name>\n"
"  -s c       | --swap-compact\n"
#endif
#ifdef MAX_MALLOCED
"  --max-malloc <size>\n"
"  --max-small-malloc <size>\n"
#endif
#ifdef MAX_TIME
"  --max-time <time>\n"
#endif
"  -r u<size> | --reserve-user <size>\n"
"  -r m<size> | --reserve-master <size>\n"
"  -r s<size> | --reserve-system <size>\n"
#ifdef MALLOC_smalloc
"  --gcollect-outfd <filename>|<num>\n"
#endif
#ifdef YYDEBUG
"  --y|--yydebug\n"
#endif
#ifdef DEBUG
"  --check-refcounts\n"
"  --gobble-descriptors <num>\n"
#endif
"  -V|--version\n"
"  --options\n"
"  --longhelp\n"
"  -h|-?|--help\n"
       , stdout);

}

/*-------------------------------------------------------------------------*/
static void
usage (void)

/* Print the help information to stdout. */

{
  version();
  fputs("\n"
#ifdef MAXNUMPORTS
"Usage: driver [options] [<portnumber>...]\n"
#else
"Usage: driver [options] [<portnumber>]\n"
#endif
"\nOptions are:\n"
"\n"
#ifdef MAXNUMPORTS
"  -P|--inherit <fd-number>\n"
"    Inherit filedescriptor <fd-number> from the parent process\n"
"    as socket to listen for connections.\n"
"\n"
#endif
#ifdef CATCH_UDP_PORT
"  -u|--udp <portnumber>\n"
"    Specify the <portnumber> for the UDP port, overriding the compiled-in\n"
"    default.\n"
"\n"
#endif
"  -D|--define <macro>[=<text>]\n"
"    Add <macro> (optionally to be expanded to <text>) to the list of\n"
"    predefined macros known by the LPC compiler.\n"
"\n"
"  -E|--eval-cost <ticks>\n"
"    Set the number of <ticks> available for one evaluation thread.\n"
"\n"
"  -M|--master <filename>\n"
"    Use <filename> for the master object.\n"
"\n"
"  -m|--mudlib <pathname>\n"
"    Use <pathname> as the top directory of the mudlib.\n"
"\n"
"  --debug-file <filename>\n"
"    Log all debug output in <filename> instead of <host>.debug.log .\n"
"\n"
"  -d|--debug\n"
"    Generate debug output; repeat the argument for even more output.\n"
"\n"
"  -c|--list-compiles\n"
"    List the name of every compiled file on stderr.\n"
"\n"
"  -e|--no-preload\n"
"    Pass a non-zero argument (the number of occurences of this option)\n"
"    to master->preload(), which usually inhibits all preloads of castles\n"
"    and other objects.\n"
"\n"
"  -N|--no-erq\n"
"    Don't start the erq demon (if it would be started at all).\n"
"\n"
"  -t|--no-heart\n"
"    Disable heartbeats and call_outs.\n"
"\n"
"  -f|--funcall <word>\n"
"    The lfun master->flag() is called with <word> as argument before the\n"
"    gamedriver accepts netword connections.\n"
"\n"
#if TIME_TO_SWAP > 0
"  -s <time>  | --swap-time <time>\n"
"  -s v<time> | --swap-variables <time>\n"
"    Time in seconds before an object (or its variables) are swapped out.\n"
"    A time less or equal 0 disables swapping.\n"
"\n"
"  -s f<name> | --swap-file <name>\n"
"    Swap into file <name> instead of LP_SWAP.<host> .\n"
"\n"
"  -s c | --swap-compact\n"
"    Reuse free space in the swap file immediately.\n"
"\n"
#endif
#ifdef MAX_MALLOCED
"  --max-malloc <size>\n"
"    Restrict total memory allocations to <size> bytes.\n"
"\n"
"  --max-small-malloc <size>\n"
"    Restrict total small block allocations to <size> bytes.\n"
"\n"
#endif
#ifdef MAX_TIME
"  --max-time <time>\n"
"    Set the max time in seconds to spend in one evaluation.\n"
"\n"
#endif
"  -r u<size> | --reserve-user <size>\n"
"  -r m<size> | --reserve-master <size>\n"
"  -r s<size> | --reserve-system <size>\n"
"    Reserve <size> amount of memory for user/master/system allocations to\n"
"    be held until main memory runs out.\n"
"\n"
#ifdef MALLOC_smalloc
"  --gcollect-outfd <filename>|<num>\n"
"    Garbage collector output (like a log of all reclaimed memory blocks)\n"
"    is sent to <filename> (or inherited fd <num>) instead of stderr.\n"
"\n"
#endif
#ifdef YYDEBUG
"  --y|--yydebug\n"
"    Enable debugging of the LPC compiler.\n"
"\n"
#endif
#ifdef DEBUG
"  --check-refcounts\n"
"    Every backend cycle, all refcounts in the system are checked.\n"
"    SLOW!\n"
"\n"
"  --gobble-descriptors <num>\n"
"    <num> (more) filedescriptors are used up. You'll know when you need it.\n"
"\n"
#endif
"  -V|--version\n"
"    Print the version of the driver, then exit.\n"
"\n"
"  --options\n"
"    Print the version and compilation options of the driver, then exit.\n"
"\n"
"  --longhelp\n"
"    Display this help and exit.\n"
"  -h|-?|--help\n"
"    Display the short help text and exit.\n"
       , stdout);

}

/*-------------------------------------------------------------------------*/
static int
firstscan (int eOption, const char * pValue)

/* Callback from getargs() for the first scan of the commandline
 * arguments. <eOption> is the option recognized, <pValue> a value
 * or NULL.
 * Return 0 on success, non-zero on a failure.
 */

{
    switch (eOption)
    {
    case cArgument:
#ifndef MAXNUMPORTS
        if (atoi(pValue))
            port_number = atoi(pValue);
        else
            fprintf(stderr, "Illegal portnumber '%s' ignored.\n", pValue);
#else
        if (numports >= MAXNUMPORTS)
            fprintf(stderr, "Portnumber '%s' ignored.\n", pValue);
        else if (atoi(pValue))
              port_numbers[numports++] = atoi(pValue);
        else
            fprintf(stderr, "Illegal portnumber '%s' ignored.\n", pValue);
#endif
        break;

#ifdef MAXNUMPORTS
    case cInherited:
        if (numports >= MAXNUMPORTS)
            fprintf(stderr, "fd '%s' ignored.\n", pValue);
        else if (atoi(pValue))
              port_numbers[numports++] = -atoi(pValue);
        else
            fprintf(stderr, "Illegal fd '%s' ignored.\n", pValue);
        break;
#endif

#ifdef CATCH_UDP_PORT
    case cUdpPort:
        if (atoi(pValue))
            udp_port = atoi(pValue);
        else
            fprintf(stderr, "Illegal portnumber '%s' ignored.\n", pValue);
        break;
#endif

    case cDefine:
        {
            struct lpc_predef_s *tmp;

            tmp = (struct lpc_predef_s *) xalloc(sizeof(struct lpc_predef_s));
            tmp->flag = strdup(pValue);
            tmp->next = lpc_predefs;
            lpc_predefs = tmp;
        }
        break;

    case cEvalcost:
        if (atoi(pValue))
            initial_eval_cost = -atoi(pValue);
        else
            fprintf(stderr, "Illegal eval-cost '%s' ignored.\n", pValue);
        break;

    case cNoPreload:
        e_flag++;
        break;

    case cNoERQ:
        no_erq_demon++;
        break;

    case cDebug:
        d_flag++;
        break;

    case cTrace:
        comp_flag = MY_TRUE;
        break;

    case cNoHeart:
        t_flag = MY_TRUE;
        break;

#if TIME_TO_SWAP > 0
    case cSwap:
        /* Compatibility vs. one-char-only options *sigh* */
        switch (*pValue) {
        case 'c': eOption = cSwapCompact; break;
        case 'v': eOption = cSwapVars;    pValue++; break;
        case 'f': eOption = cSwapFile;    pValue++; break;
        default:  eOption = cSwapTime;    break;
        }
        /* FALLTHROUGH */

    case cSwapVars:
    case cSwapFile:
    case cSwapTime:
    case cSwapCompact:
        if (cSwapTime == eOption)
        {
            time_to_swap = atoi(pValue);
            if (time_to_swap < 0)
                time_to_swap = 0;
        }
        else if (cSwapVars == eOption)
        {
            time_to_swap_variables = atoi(pValue);
            if (time_to_swap_variables < 0)
                time_to_swap_variables = 0;
        }
        else if (cSwapFile == eOption)
            name_swap_file(pValue);
        else /* cSwapCompact */
            swap_compact_mode = MY_TRUE;
        break;
#endif

#ifdef YYDEBUG
    case cYYDebug:
        yydebug = MY_TRUE;
        break;
#endif

    case cMaster:
        if (strlen(pValue) >= sizeof(master_name)) {
            fprintf(stderr, "Too long master name '%s'\n", pValue);
            return 1;
        }
        strcpy(master_name, pValue);
        break;

#ifdef MAX_MALLOCED
    case cMaxMalloc:
        max_malloced = strtol(pValue, (char **)0, 0);
        if (!max_malloced)
        {
            fprintf(stderr, "Illegal value '%s' for --max-malloc\n", pValue);
            return 1;
        }
        break;

    case cMaxSmall:
        max_small_malloced = strtol(pValue, (char **)0, 0);
        if (!max_small_malloced)
        {
            fprintf(stderr, "Illegal value '%s' for --max-small-malloc\n", pValue);
            return 1;
        }
        break;
#endif

#ifdef MAX_TIME
    case cMaxTime:
        max_time = strtol(pValue, (char **)0, 0);
        if (max_time <= 0)
        {
            fprintf(stderr, "Illegal value '%s' for --max-time\n", pValue);
            return 1;
        }
        break;
#endif

    case cMudlib:
        if (chdir(pValue) == -1) {
            fprintf(stderr, "Bad mudlib directory: %s\n", pValue);
            return 1;
        }
        new_mudlib = 1;
        break;

    case cDebugFile:
        if (debug_file != NULL)
            free(debug_file);
        debug_file = strdup(pValue);
        break;

    case cReserved:
    case cReserveUser:
    case cReserveMaster:
    case cReserveSystem:
        {
            mp_int *sizep = &reserved_user_size;

            if (cReserved == eOption)
            {
                /* This is a rather nasty compromise between being compatible
                 * to original Amylaar and the one-char-only short options.
                 */

                switch(*pValue++)
                {
                default:  pValue--; /* FALLTHROUGH */
                case 'u': sizep = &reserved_user_size; break;
                case 'm': sizep = &reserved_master_size; break;
                case 's': sizep = &reserved_system_size; break;
                }
            }
            else
            switch (eOption)
            {
            case cReserveUser:   sizep = &reserved_user_size; break;
            case cReserveMaster: sizep = &reserved_master_size; break;
            case cReserveSystem: sizep = &reserved_system_size; break;
            }

            *sizep = strtol(pValue, (char**)0, 0);
            break;
        }

#ifdef MALLOC_smalloc
    case cGcollectFD:
        if (isdigit((unsigned char)*pValue)) {
            gcollect_outfd = strtol(pValue, (char **)0, 0);
        } else {
                  gcollect_outfd = ixopen3(pValue, O_CREAT|O_TRUNC|O_WRONLY, 0640);
        }
        break;
#endif

    case cOptions:
        options();
        exit(0);
        break;

    case cVersion:
        version();
        exit(0);
        break;

    case cHelp:
        shortusage();
        return 1;

    case cLongHelp:
        usage();
        return 1;

#ifdef DEBUG
    case cCheckRefs:
        check_a_lot_ref_counts_flag = MY_TRUE;
        break;

    case cGobbleFDs:
        {
            int n;

            n = strtol(pValue, (char **)0, 0);
            while(--n >= 0) {
                (void)dup(2);
            }
            break;
        }
#endif

    case cFuncall:
        /* ignored */
        break;

    default:
        /* This shouldn't happen. */
        fprintf(stderr, "driver: (firstscan) Internal error, eOption is %d\n", eOption);
        return 1;
    } /* switch */

  return 0;
}

/*-------------------------------------------------------------------------*/
static int
secondscan (int eOption, const char * pValue)

/* Callback from getargs() for the second scan of the commandline
 * arguments. <eOption> is the option recognized, <pValue> a value
 * or NULL.
 * Return 0 on success, non-zero on a failure.
 */

{
    switch (eOption)
    {
    case cFuncall:
        push_constant_string((char *)pValue);
        (void)apply_master_ob(STR_FLAG, 1);
        if (game_is_being_shut_down) {
            fprintf(stderr, "Shutdown by master object.\n");
            exit(0);
        }
        /* ignored */
        break;

    default:
        /* ignored */
        break;
    } /* switch */

  return 0;
}

/*-------------------------------------------------------------------------*/
static int
getargs (int argc, char ** argv, int (*opt_eval)(int, const char *) )

/* Get the arguments from the commandline and pass them
 * as (number, optional value) to the opt_eval callback.
 * If opt_eval() returns non-zero, argument scanning is terminated.
 * In that case, or if getargs() detects an error itself, getargs() returns
 * non-zero.
 * A zero return means 'success' in both cases.
 */

{
  int         i;        /* all purpose */
  int         iArg;     /* Number of argument under inspection */
  OptNumber   eOption;  /* The current recognized option */
  int         iOption;  /* The index of the recognized option */
  short       bCont;    /* True: find another option in the same argument */
  short       bDone;    /* True: all options parsed, only args left */
  short       bShort;   /* True: current argument is a short option */
  char      * pArg;     /* Next argument character to consider */

  /* Make the compiler happy */
  bShort = MY_FALSE;
  pArg = NULL;

  /* Scan all arguments */
  bCont = MY_FALSE;
  bDone = MY_FALSE;
  for (iArg = 1; iArg < argc; !bCont ? iArg++ : iArg)
  {
    size_t   iArglen;      /* Length of remaining argument */
    char   * pValue;       /* First character of an option value, or NULL */
    int      bTakesValue;  /* This option takes a value */

    /* Make the compiler happy */
    iArglen = 0;
    pValue = NULL;
    bTakesValue = MY_FALSE;

    if (bDone)
      eOption = cArgument;
    else
    /* If this is not a continuation, reinitialise the inspection vars.
     * For --opt=val arguments, pValue is set to the first character of val.
     */
    if (!bCont)
    {
      pArg = argv[iArg];
      if ('-' == pArg[0] && '-' == pArg[1]) /* Long option? */
      {
        eOption = cUnknown;
        bShort = MY_FALSE;
        pArg += 2;
        /* Special case: if the argument is just '--', it marks the
         * end of all options.
         * We set a flag and continue with the next argument.
         */
        if ('\0' == *pArg)
        {
          bDone = MY_TRUE;
          continue;
        }
        pValue = strchr(pArg, '=');
        if (pValue != NULL)
        {
          iArglen = pValue - pArg;
          pValue++;
        }
        else
          iArglen = strlen(pArg);
      }
      else if ('-' == pArg[0]) /* Short option? */
      {
        eOption = cUnknown;
        bShort = MY_TRUE;
        pArg++;
        iArglen = strlen(pArg);
        pValue = NULL;
      }
      else /* No option */
      {
        eOption = cArgument;
        pValue = pArg;
        iArglen = 0;
      }
    }
    else
      eOption = cUnknown;

    /* If the option is not determined yet, do it.
     * Set pValue to the first character of the value if any.
     */
    if (cUnknown == eOption)
    {
      if (bShort) /* search the short option */
      {
        for (iOption = 0; iOption < sizeof(aShortOpts) / sizeof(aShortOpts[0]); iOption++)
        {
          if (*pArg == aShortOpts[iOption].cOption)
          {
            eOption = (OptNumber)aShortOpts[iOption].eNumber;
            bTakesValue = aShortOpts[iOption].bValue;
            pArg++; iArglen--;  /* Consume this character */
            break;
          }
        }
        /* Consume a following '=' if appropriate */
        if (cUnknown != eOption && bTakesValue && iArglen > 0 && '=' == *pArg)
        {
          pArg++; iArglen--;
        }

        /* If there is a value following in the same argument, set pValue to
         * it and mark the remaining characters as 'consumed'
         */
        if (cUnknown != eOption && bTakesValue && iArglen > 0)
        {
          pValue = pArg;
          pArg += iArglen;
          iArglen = 0;
        }
      }
      else  /* search the long option */
      {
        for (iOption = 0; iOption < sizeof(aLongOpts) / sizeof(aLongOpts[0]); iOption++)
          if (iArglen == strlen(aLongOpts[iOption].pOption)
           && !strncasecmp(pArg, aLongOpts[iOption].pOption, iArglen))
          {
            eOption = (OptNumber)aLongOpts[iOption].eNumber;
            bTakesValue = aLongOpts[iOption].bValue;
            break;
          }
      }

      if (cUnknown == eOption)
      {
        fputs("driver: Unknown option '", stderr);
        if (bShort)
          fprintf(stderr, "-%c", *pArg);
        else
          fprintf(stderr, "--%*.*s", (int)iArglen, (int)iArglen, pArg);
        fputs("'.\n", stderr);
        return 1;
      }

      /* If at this point bTakesValue is true, but pValue is still NULL,
       * then the value is in the next argument. Get it if it's there.
       */
      if (bTakesValue && pValue == NULL && iArg + 1 < argc)
      {
        iArg++;
        pValue = argv[iArg];
      }

      /* Signal an error if pValue is still NULL or if it's empty. */
      if (bTakesValue && (pValue == NULL || !strlen(pValue)))
      {
        fputs("driver: Option '", stderr);
        if (bShort)
          putc(aShortOpts[iOption].cOption, stderr);
        else
          fputs(aLongOpts[iOption].pOption, stderr);
        fputs("' expects a value.\n", stderr);
        return 1;
      }

    } /* if (unknown option) */

    /* Before evaluation of the parsed option, determine 'bCont' */
    bCont = bShort && (iArglen > 0) && !bTakesValue;

    /* --- The option evaluation --- */

    i = (*opt_eval)(eOption, pValue);
    if (i)
      return i;

  } /* for (iArg) */

  return 0;
} /* getargs() */

/***************************************************************************/
