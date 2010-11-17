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
#include "typedefs.h"

#include "my-alloca.h"
#include <ctype.h>
#include <fcntl.h>
#ifdef HAVE_NETDB_H
#    include <netdb.h>
     /* MAXHOSTNAMELEN on Solaris */
#endif
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>

#include "main.h"

#include "backend.h"
#include "array.h"
#include "access_check.h"
#include "comm.h"
#include "filestat.h"
#include "gcollect.h"
#include "interpret.h"
#include "lex.h"
#include "mapping.h"
#include "mempools.h"
#include "mregex.h"
#include "mstrings.h"
#include "object.h"
#include "otable.h"
#include "patchlevel.h"
#include "pkg-tls.h"
#include "random.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "svalue.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

#ifdef USE_MYSQL
#include "pkg-mysql.h"
#endif

#ifdef USE_XML
#    if defined(HAS_XML2) && defined(HAS_IKSEMEL)
#        error Both, libxml2 and iksemel enabled.
#    endif
#    ifdef HAS_XML2
#        include "pkg-xml2.h"
#    endif
#    ifdef HAS_IKSEMEL
#        include "pkg-iksemel.h"
#    endif
#endif

#ifdef USE_GCRYPT
#include "pkg-gcrypt.h"
#endif

#include "i-eval_cost.h"

#include "../mudlib/sys/regexp.h"

/*-------------------------------------------------------------------------*/

#define PLAIN_MASTER "secure/master"
#define COMPAT_MASTER "obj/master"

/*-------------------------------------------------------------------------*/

/* -- Pure commandline arguments -- */

int d_flag    = 0;  /* Debuglevel */
/* TODO: Make this bitflags, one for 'trace refcounts' etc */
Bool disable_timers_flag    = MY_FALSE;  /* True: Disable heart beat and reset */
static int e_flag = MY_FALSE;  /* Passed to preload(), usually disables it */
Bool comp_flag = MY_FALSE;  /* Trace compilations */
#ifdef DEBUG
Bool check_a_lot_ref_counts_flag = MY_FALSE;  /* The name says it. */
int check_state_level = 0;     /* how of to check the state in the loop */
#endif

#ifdef CHECK_OBJECT_STAT
Bool check_object_stat = MY_FALSE;
#endif

Bool strict_euids = MY_FALSE;     /* Enforce use of the euids */
Bool share_variables = MY_FALSE;  /* Clones are initialized from their
                                   * blueprints.
                                   */

Bool allow_filename_spaces = MY_FALSE; /* Allow spaces in filenames */

static char * hostname = NULL;
static char * hostaddr = NULL;
  /* Hostname and -addr given on the commandline. They are passed as
   * arguments to initialize_host_ip() and are then no longer used.
   */


/* -- Configuration options -- */

long time_to_reset          = TIME_TO_RESET;
long time_to_cleanup        = TIME_TO_CLEAN_UP;
  /* A value <= 0 disables the reset/cleanup */

long time_to_swap           = TIME_TO_SWAP;
long time_to_swap_variables = TIME_TO_SWAP_VARIABLES;
  /* A value <= 0 disables the swapping. */

long alarm_time             = ALARM_TIME;
long heart_beat_interval    = HEART_BEAT_INTERVAL;
  /* Minimum value is 1. */

#ifdef SYNCHRONOUS_HEART_BEAT
Bool synch_heart_beats      = MY_TRUE;
#else
Bool synch_heart_beats      = MY_FALSE;
#endif

Bool heart_beats_enabled    = MY_TRUE;
  /* heart beats are currently active and will be called. */

int port_numbers[MAXNUMPORTS] = { PORTNO };
  /* The login port numbers.
   * Negative numbers are not ports, but the numbers of inherited
   * socket file descriptors.
   */
int numports = 0;  /* Number of specified ports */

int udp_port = UDP_PORT;
  /* Port number for UDP. A negative number disables it. */

char *erq_file = NULL;        /* Base- or pathname of the erq executable,
                               * or NULL */
char **erq_args = NULL;       /* Optional arguments of the erq executable,
                                 or NULL */

char *mud_lib;                /* Path to the mudlib */
char master_name[100] = "";   /* Name of the master object */
string_t * master_name_str = NULL;  /* master_name[] as tabled string */

static int new_mudlib = 0;    /* True: mudlib directory was specified */
static int no_erq_demon = 0;  /* True: don't start the erq */

#ifndef COMPAT_MODE
Bool compat_mode = MY_FALSE; /* Plain mode */
#else
Bool compat_mode = MY_TRUE;  /* Compat mode */
#endif

#ifdef USE_PCRE
p_int regex_package = RE_PCRE;
#else
p_int regex_package = RE_TRADITIONAL;
#endif
  /* The default regex package to use, expressed as one of the
   * acknowledged bitflags.
   */

/* -- Other Global Variables -- */
svalue_t const0, const1;
  /* The values 0 and 1 as svalues, mem-copied when needed */

double avg_consts[5];
  /* Weight constants used to compute average figures */

char *debug_file = NULL;  /* Name of the debug log file. */

object_t dummy_current_object_for_loads;
  /* Dummy object for functions, which need a current_object though
   * there is none. This is usually the case when (re)loading the
   * master object.
   */

int slow_shut_down_to_do = 0;
  /* If non-zero, the game should perform a graceful shutdown.
   * The value is the number of minutes to still last before shutting down.
   */

char input_escape = '!';
  /* The input escape/input_to() bypass character.
   */

Bool reopen_debug_log = MY_FALSE;
  /* Set to TRUE by the USR2 handler to force the driver to reopen
   * the debug.log file.
   */

mp_int boot_time = 0;
  /* The time() the driver was started.
   */

long time_to_data_cleanup = 0;
  /* The time delay between two data cleans, derived from time_to_cleanup. */

int exit_code = 0; /* TODO: There are constants for this */
  /* Exit code from the program, can be changed with the shutdown()
   * efun.
   */

/*-------------------------------------------------------------------------*/

/* Forward declarations */

static const char * drivertag (void);

/* Forward declarations for the argument parser in the lower half */

static int getargs (int argc, char ** argv, int (*opt_eval)(int, const char *) );
static int eval_arg (int, const char *);

/* Datastructures used to gather data during the argument scan which
 * needs to be evaluated later.
 *
 * struct FData: the values given to the '-f' options, allocated to size.
 */

typedef struct FData {
    struct FData * next;
    char           txt[1];  /* The value, allocated to size */
} FData;

static FData * f_head = NULL;
static FData * f_tail = NULL;

/*-------------------------------------------------------------------------*/
int
main (int argc, char **argv)

/* The main function. Nuff said. */

{
    int i;
    char *p;
    sigset_t set;
    volatile int rc;

    rc = 0;

    /* On some systems, SIGALRM is sometimes blocked.
     * The reasons are unknown, but this seems to be the cure.
     */
    sigemptyset(&set);
    sigaddset(&set, SIGALRM);
    sigprocmask(SIG_UNBLOCK, &set, 0);

    /* Initialisations */

    boot_time = (mp_int)time(NULL);
    setlocale(LC_CTYPE, ""); /* Use the locale defined in the LANG env var */
    setlocale(LC_TIME, "");
    get_stack_direction();
    mb_init();
    init_interpret();
    rx_init();

    put_number(&const0, 0);
    put_number(&const1, 1);

    current_time = get_current_time();
    
         
    // Set prng_device_name to the default //
    prng_device_name = strdup(PRNG_DEFAULT_DEVICE);
    // init PRG by the default device (/dev/urandom) 
    // if --random-seed or --randomdevice is given on the command-line it
    // will re-seeded later.
    seed_random(prng_device_name);
    
#ifdef ACCESS_FILE
    access_file = strdup(ACCESS_FILE);
#endif
#ifdef ACCESS_LOG
    access_log = strdup(ACCESS_LOG);
#endif
#ifdef USE_TLS
#  ifdef TLS_DEFAULT_KEYFILE
    tls_keyfile = strdup(TLS_DEFAULT_KEYFILE);
#  endif
#  ifdef TLS_DEFAULT_CERTFILE
    tls_certfile = strdup(TLS_DEFAULT_CERTFILE);
#  endif
#  ifdef TLS_DEFAULT_TRUSTFILE
    tls_trustfile = strdup(TLS_DEFAULT_TRUSTFILE);
#  endif
#  ifdef TLS_DEFAULT_TRUSTDIRECTORY
    tls_trustdirectory = strdup(TLS_DEFAULT_TRUSTDIRECTORY);
#  endif
#  ifdef TLS_DEFAULT_CRLFILE
    tls_crlfile = strdup(TLS_DEFAULT_CRLFILE);
#  endif
#  ifdef TLS_DEFAULT_CRLDIRECTORY
    tls_crldirectory = strdup(TLS_DEFAULT_CRLDIRECTORY);
#  endif
#endif

    do {
        dummy_current_object_for_loads = NULL_object;
#ifdef DEBUG
        if (dummy_current_object_for_loads.user)
        {
            fprintf(stderr, "Assigning NULL_object does not clear the target.\n");
            rc = 1;
            break;
        }
#endif
        dummy_current_object_for_loads.ref = 1;
        dummy_current_object_for_loads.user = &default_wizlist_entry;

#ifdef STRICT_EUIDS
        strict_euids = MY_TRUE;
#endif
#ifdef SHARE_VARIABLES
        share_variables = MY_TRUE;
#endif
#ifdef ALLOW_FILENAME_SPACES
        allow_filename_spaces = MY_TRUE;
#endif

#ifdef INPUT_ESCAPE
        /* Check the definition of the INPUT_ESCAPE character */
        if (strlen(INPUT_ESCAPE) < 1)
        {
            fprintf(stderr, "Bad definition of INPUT_ESCAPE: string is empty.\n");
            rc = 1;
            break;
        }
        if (strlen(INPUT_ESCAPE) > 1)
        {
            fprintf(stderr, "Bad definition of INPUT_ESCAPE: "
                            "'%s' contains more than one character.\n"
                          , INPUT_ESCAPE);
            rc = 1;
            break;
        }
        input_escape = INPUT_ESCAPE[0];
#endif

        /*
         * Check that the definition of EXTRACT_UCHAR() is correct.
         */
        p = (char *)&i;
        *p = -10;
        if (EXTRACT_UCHAR(p) != 0x100 - 10) {
            fprintf(stderr, "Bad definition of EXTRACT_UCHAR().\n");
            rc = 1;
            break;
        }

        init_driver_hooks();
        init_rusage();
#ifdef HOST_DEPENDENT_INIT
        HOST_DEPENDENT_INIT
#endif

#ifdef WIZLIST_FILE
        /* Select a sensible default for the wizlist file.
         * This must be done before the parsing of the arguments so
         * that the name can be removed by commandline option.
         */
        if ('\0' == wizlist_name[0])
        {
            name_wizlist_file(WIZLIST_FILE);
        }
#endif

        /* Scan of the arguments.
         */
        if (getargs(argc, argv, eval_arg))
        {
            rc = 1;
            break;
        }

        /* Print the driver tag line to stdout. The output to the debug.log
         * will follow when we opened it.
         */
        printf("%s LDMud " DRIVER_VERSION LOCAL_LEVEL
               " (" PROJ_VERSION ")%s\n"
              , time_stamp(), drivertag()
              );

        /* Setup comm::host_name, so that we can open the debug.log file
         * with the proper name. We do the complete setup later.
         */
        initialize_host_name(hostname);

        /* Change to the mudlib dir early so that the debug.log file
         * is opened in the right place.
         * If a mudlib dir has been given by command option, we are already
         * in it.
         */
        if (!new_mudlib && chdir(MUD_LIB) == -1) {
            printf("%s Bad mudlib directory: %s\n", time_stamp(), MUD_LIB);
            rc = 1;
            break;
        }

        /* If the name of the debug log file hasn't been set, use a sensible
         * default and make it available in the macro __DEBUG_LOG__. This
         * should happen before the first debug_message().
         */

        if  (!debug_file)
        {
            char buf[MAXHOSTNAMELEN+40];
            char * name;
            struct lpc_predef_s *tmp;

            if (compat_mode)
                strcpy(buf, "__DEBUG_LOG__=\"");
            else
                strcpy(buf, "__DEBUG_LOG__=\"/");
            name = buf + strlen(buf);
            sprintf(name, "%s.debug.log", query_host_name());
            debug_file = strdup(name);
            strcat(name, "\"");

            tmp = (struct lpc_predef_s *) xalloc(sizeof(struct lpc_predef_s));
            tmp->flag = string_copy(buf);
            tmp->next = lpc_predefs;
            lpc_predefs = tmp;
        }

        debug_message("%s LDMud " DRIVER_VERSION LOCAL_LEVEL
                      " (" PROJ_VERSION ")%s\n"
                     , time_stamp(), drivertag()
                     );
          /* This also assures the existance of the fd for the debug log */

        time_to_data_cleanup = (time_to_cleanup > 0) ? time_to_cleanup
                                                     : DEFAULT_CLEANUP_TIME;
        reserve_memory();
        mstring_init();
          /* Also initializes the standard strings, which may be required
           * early on should an error happen.
           */

#ifdef USE_TLS
        tls_global_init();
#endif

#ifdef USE_GCRYPT
        if (!pkg_gcrypt_init())
        {
            rc = 1;
            break;
        }
#endif

        if (numports < 1) /* then use the default port */
            numports = 1;

        init_otable();
        for (i = 0; i < (int)(sizeof avg_consts / sizeof avg_consts[0]); i++)
            avg_consts[i] = exp(- i / 900.0);

#ifdef USE_MYSQL
        if (!pkg_mysql_init())
        {
            rc = 1;
            break;
        }
#endif

#ifdef USE_XML
#ifdef HAS_IKSEMEL
        pkg_iksemel_init();
#endif

#ifdef HAS_XML2
        pkg_xml2_init();
#endif
#endif

        /* If the master_name hasn't been set, select a sensible default */
        if ('\0' == master_name[0])
        {
#ifdef MASTER_NAME
            strcpy(master_name, MASTER_NAME);
#elif defined(COMPAT_MODE)
            strcpy(master_name, COMPAT_MASTER);
#else
            strcpy(master_name, PLAIN_MASTER);
#endif
        }

        /* Make sure the name of the master object is sensible.
         * This is important for modules like the lexer which
         * use it directly.
         *
         * We also need a copy of the master name as string_t (for
         * this the strings module has to be initialized).
         */
        {
            const char *pName = make_name_sane(master_name, MY_FALSE);
            if (pName)
                strcpy(master_name, pName);
            master_name_str = new_tabled(master_name);
            if (!master_name_str)
            {
                printf("%s Out of memory for master object name (%lu bytes).\n"
                      , time_stamp()
                      , (unsigned long)strlen(master_name));
                rc = 1;
                break;
            }
        }

        reset_machine(MY_TRUE); /* Cold reset the machine */
        init_lexer();
          /* The lexer needs the master_name, but also the VM
           * to throw errors.
           */

        RESET_LIMITS;
        CLEAR_EVAL_COST;
        {
            char path[MAXPATHLEN+1];
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

#ifdef ERQ_DEMON
        /* Make sure that erq_file contains a complete absolute pathname. */

        if (!erq_file)
        {
            erq_file = malloc(strlen(BINDIR)+6);
            if (!erq_file)
            {
                fatal("Out of memory for erq pathname (%lu bytes).\n"
                     , (unsigned long)strlen(BINDIR)+6);
            }
            strcpy(erq_file, BINDIR);
            strcat(erq_file, "/erq");
        }
        else if (*erq_file != '/')
        {
            char * tmp;
            tmp = malloc(strlen(BINDIR)+1+strlen(erq_file)+1);
            if (!tmp)
            {
                fatal("Out of memory for erq pathname (%lu bytes).\n"
                     , (unsigned long)(strlen(BINDIR)+2+strlen(erq_file)));
            }
            strcpy(tmp, BINDIR);
            strcat(tmp, "/");
            strcat(tmp, erq_file);
            free(erq_file);
            erq_file = tmp;
        }

        if (!no_erq_demon)
            start_erq_demon("", 0);
#endif /* ERQ_DEMON */
        initialize_host_ip_number(hostname, hostaddr);
        free(hostname); hostname = NULL;
        free(hostaddr); hostaddr = NULL;

        initialize_host_access();
        
        install_signal_handlers();
        
        (void)signal(SIGFPE, SIG_IGN);
        current_object = &dummy_current_object_for_loads;
        if (setjmp(toplevel_context.con.text)) {
            clear_state();
            add_message("Anomaly in the fabric of world space.\n");
        }
        else
        {
            toplevel_context.rt.type = ERROR_RECOVERY_BACKEND;
            master_ob = get_object(master_name_str);
        }
        current_object = master_ob;
        toplevel_context.rt.type = ERROR_RECOVERY_NONE;
        if (master_ob == NULL) {
            printf("%s The file %s must be loadable.\n"
                  , time_stamp(), master_name);
            rc = 1;
            break;
        }

        /* Make sure master_ob is never made a dangling pointer.
         * Look at apply_master_ob() for more details.
         */
        ref_object(master_ob, "main");
        initialize_master_uid();
        push_number(inter_sp, 0);
        callback_master(STR_INAUGURATE, 1);
        setup_print_block_dispatcher();

        /* Evaluate all the 'f' arguments we received, if any. */
        while (f_head != NULL)
        {
            FData * fdata = f_head;

            f_head = f_head->next;
            push_c_string(inter_sp, fdata->txt);
            (void)callback_master(STR_FLAG, 1);
            free(fdata);
            if (game_is_being_shut_down) {
                fprintf(stderr, "%s Shutdown by master object.\n", time_stamp());
                rc = 0;
                break;
            }
        }

#ifdef DEBUG
        if (d_flag > 1 && time_to_swap_variables <= 0)
            check_a_lot_ref_counts_flag = MY_TRUE;
#endif

        if (!assert_simul_efun_object())
        {
            rc = 1;
            break;
        }

        if (game_is_being_shut_down)
        {
            rc = 1;
            break;
        }

        load_wiz_file();
        preload_objects(e_flag);

        /* Start the backend loop. This won't return before
         * the game shuts down.
         */
        backend();

        /* Shutdown the game.
         */

        rc = exit_code;
        printf("%s LDMud shutting down.\n", time_stamp());

        callback_master(STR_NOTIFY_SHUTDOWN, 0);
        ipc_remove();
        remove_all_players();
        handle_newly_destructed_objects();
          /* Will perform the remove_interactive calls */
        unlink_swap_file();
#ifdef DEALLOCATE_MEMORY_AT_SHUTDOWN
        remove_all_objects();
        remove_wiz_list();
#if defined(MALLOC_smalloc)
        dump_malloc_data();
#endif
#endif
    } while(0);

    /* Mandatory cleanups - see also simulate::fatal() */
#ifdef USE_TLS
    tls_global_deinit();
#endif

    return rc; /* TODO: There are constants for this */
} /* main() */


/*-------------------------------------------------------------------------*/
void initialize_master_uid (void)

/* After loading the master object, determine its (e)uid by calling the
 * lfun get_master_uid() in it. For details, better read the code.
 */

{
    svalue_t *ret;

    ret = apply_master(STR_GET_M_UID, 0);
    if (ret && ret->type == T_NUMBER && ret->u.number)
    {
        master_ob->user = &default_wizlist_entry;
        master_ob->eff_user = 0;
    }
    else if (ret == 0 || ret->type != T_STRING)
    {
        printf("%s %s: %s() in %s does not work\n"
              , time_stamp(), strict_euids ? "Fatal" : "Warning"
              , get_txt(STR_GET_M_UID), master_name);
        if (strict_euids)
        {
            exit(1);
        }
    }
    else
    {
        master_ob->user = add_name(ret->u.str);
        master_ob->eff_user = master_ob->user;
    }
} /* initialize_master_uid() */


/*-------------------------------------------------------------------------*/
void
vdebug_message(const char *fmt, va_list va)

/* Print a message into the debug logfile, vprintf() style.
 */

{
    static FILE *fp = NULL;

    if (fp == NULL || reopen_debug_log) {
        if (fp != NULL)
        {
            fclose(fp);
            fp = NULL;
        }
        reopen_debug_log = MY_FALSE;

        if (!debug_file) /* We can get called before it's been set */
            return;

        fp = fopen(debug_file, "w");
        if (fp == NULL) {
            perror(debug_file);
            abort();
        }
        else
            set_cloexec_flag(fileno(fp));
    }
    (void)vfprintf(fp, fmt, va);
    (void)fflush(fp);
} /* vdebug_message() */

/*-------------------------------------------------------------------------*/
void
debug_message(const char *a, ...)

/* Print a message into the debug logfile, printf() style.
 */

{
    va_list va;

    va_start(va, a);
    vdebug_message(a, va);
    va_end(va);
} /* debug_message() */

/*-------------------------------------------------------------------------*/
void
write_x (int d, p_uint i)

/* Memory safe function to write 4-byte hexvalue <i> to fd <d>. */

{
    int j;
    char c;

    for (j = 2 * sizeof i; --j >= 0; i <<= 4) {
        c = (char)((i >> (8 * sizeof i - 4) ) + '0');
        if (c >= '9' + 1)
            c += (char)('a' - ('9' + 1));
        write(d, &c, 1);
    }
} /* write_x() */

/*-------------------------------------------------------------------------*/
void
write_X (int d, unsigned char i)

/* Memory safe function to write 1-byte hexvalue <i> to fd <d>. */

{
    int j;
    char c;

    for (j = 2 * sizeof i; --j >= 0; i <<= 4) {
        c = (char)((i >> (8 * sizeof i - 4) ) + '0');
        if (c >= '9' + 1)
            c += (char)('a' - ('9' + 1));
        write(d, &c, 1);
    }
} /* write_X() */

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
        c = (char)((i / j) % 10 + '0');
        write(d, &c, 1);
        j /= 10;
    } while (j > 0);
} /* writed() */

/*-------------------------------------------------------------------------*/
void
writes (int d, const char *s)

/* Memory safe function to string <s> to fd <d>. */

{
    write(d, s, strlen(s));
}

/*-------------------------------------------------------------------------*/
char *
dprintf_first (int fd, char *s, p_int a)

/* Write the string <s> up to the next "%"-style argument to <fd>, the
 * write <a> according to the %-formatter. Recognized are %s, %d, %c,
 * %x (4-Byte hex) and %X (a 1-Byte hex).
 * If no %-formatter is present, the whole string is written.
 *
 * Result is a pointer to the remaining string.
 */

{
    char *p;

    do {
        if ( !(p = strchr(s, '%')) )
        {
            write(fd, s, strlen(s));
            return "";
        }

        write(fd, s, p - s);
        switch(p[1])
        {
        case '%':
            write(fd, p+1, 1);
            continue;
        case 's':
            write(fd, (char *)a, strlen((char*)a));
            break;
        case 'c':
          {
            char c = (char)a;
            write(fd, (char *)&c, 1);
            break;
          }
        case 'd':
            writed(fd, a);
            break;
        case 'x':
            write_x(fd, a);
            break;
        case 'X':
            write_X(fd, (unsigned char)a);
            break;
        }
        return p+2;
    } while(1);
} /* dprintf_first() */

/*-------------------------------------------------------------------------*/
void
dprintf1 (int fd, char *s, p_int a)

/* Write a message <s> to <fd>. <s> may contain one %-style formatter.
 * for the argument <a>.
 */

{
    s = dprintf_first(fd, s, a);
    write(fd, s, strlen(s));
} /* dprintf1() */

/*-------------------------------------------------------------------------*/
void
dprintf2 (int fd, char *s, p_int a, p_int b)

/* Write a message <s> to <fd>. <s> may contain two %-style formatter.
 * for the arguments <a> and <b>.
 */

{
    s = dprintf_first(fd, s, a);
    dprintf1(fd, s, b);
} /* dprintf2() */

/*-------------------------------------------------------------------------*/
void
dprintf3 (int fd, char *s, p_int a, p_int b, p_int c)

/* Write a message <s> to <fd>. <s> may contain three %-style formatter.
 * for the arguments <a>, <b> and <c>.
 */

{
    s = dprintf_first(fd, s, a);
    dprintf2(fd, s, b, c);
} /* dprintf3() */

/*-------------------------------------------------------------------------*/
void
dprintf4 (int fd, char *s, p_int a, p_int b, p_int c, p_int d)

/* Write a message <s> to <fd>. <s> may contain three %-style formatter.
 * for the arguments <a>, <b>, <c> and <d>.
 */

{
    s = dprintf_first(fd, s, a);
    dprintf3(fd, s, b, c, d);
} /* dprintf4() */

/*-------------------------------------------------------------------------*/
void
set_cloexec_flag (int fd)

/* Sets the FD_CLOEXEC flag, so that the file is closed on exec()
 * and the erq doesn't inherit it.
 */
{
#if defined(HAVE_FCNTL) && defined(FD_CLOEXEC)
    int flags = fcntl(fd, F_GETFD);

    if (flags != -1)
        fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
#endif
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
 *
 * The arguments are usually taken from the commandline; but the parser
 * is also able to read them from a textfiles, which can be nested. The
 * content of the textfiles is broken down into words delimited by whitespace,
 * which are then treated as given on the commandline at the place where
 * the instruction to read the textfile stood.
 *
 * The file parser recognizes simple double-quoted strings, which must be
 * contained on a single line. Additionally, the '#' character given by
 * itself is a comment marker - everthing after the '#' until the end
 * of the current line is ignored.
 *-------------------------------------------------------------------------
 * Internally every option recognized by the program is associated with
 * an id number, defined as the enum OptNumber. The parser itself uses the
 * two id numbers 'cUnknown' for unrecognized options, and 'cArgument' for
 * proper command arguments.
 *
 * Id numbers are associated with their option strings/letters by the
 * statically initialized arrays aOptions. Every element
 * in this array is a structure defining the option's name (string
 * or letter), the associated id number, whether or not the option
 * takes a value, and the short and long help text. The order of the
 * elements does not matter, except for the help text output.
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
 * pointing to the argument string. The handler has to return one of the
 * following values:
 *    hrSuccess: if the option/argument was processed correctly.
 *    hrError:   if the option/argument couldn't be processed.
 *    hrArgFile: if the given value is the name of an arguments file
 *               to include.
 *-------------------------------------------------------------------------
 */

/* Handler return values */

typedef enum HandlerResult {
    hrSuccess = 0  /* Argument parsed */
  , hrError        /* Error parsing argument */
  , hrArgFile      /* Value of this argument is the filename of an argument
                    * file.
                    */
} HandlerResult;

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

/* Description of an option */

typedef struct Option {
  char        cOption;  /* The short option char, or \0 if none */
  char      * pOption;  /* The long option string, or NULL if none */
  int         eNumber;  /* The associated option number */
  short       bValue;   /* True: takes a value */
  char      * pSHelp;   /* Short help string, or NULL */
  char      * pLHelp;   /* Long help string, or NULL */
} Option;

/* Every recognized option has a ordinal number */

typedef enum OptNumber {
   cUnknown = 0     /* unknown option                     */
 , cArgument        /* normal argument (for us: portnumber) */
 , cArgFile         /* --args               */
 , cInherited       /* --inherit            */
 , cUdpPort         /* --udp                */
 , cTrace           /* --list-compiles      */
 , cAlarmTime       /* --alarm-time       */
 , cCleanupTime     /* --cleanup-time       */
 , cCompat          /* --compat             */
 , cNoCompat        /* --no-compat          */
 , cDebug           /* --debug              */
 , cDefine          /* --define             */
 , cErq             /* --erq                */
 , cEvalcost        /* --eval-cost          */
 , cFilenameSpaces   /* --filename-spaces    */
 , cNoFilenameSpaces /* --no-filename-spaces */
 , cFuncall         /* --funcall            */
 , cMaster          /* --master             */
 , cMudlib          /* --mudlib             */
 , cDebugFile       /* --debug-file         */
 , cHBInterval      /* --heart-beat-interval     */
 , cHostname        /* --hostname           */
 , cHostaddr        /* --hostaddr           */
 , cAccessFile      /* --access-file        */
 , cAccessLogFile   /* --access-log         */
 , cMaxMalloc       /* --hard-malloc-limit  */
 , cSoftMallocLimit /* --soft-malloc-limit  */
 , cMaxArray        /* --max-array          */
 , cMaxBytes        /* --max-bytes          */
 , cMaxCallouts     /* --max-callouts       */
 , cMaxFile         /* --max-file           */
 , cMaxMapping      /* --max-mapping        */
 , cMaxMappingKeys  /* --max-mapping-keys   */
 , cMaxWriteBuffer  /* --max-write-buffer   */
 , cMinMalloc       /* --min-malloc         */
 , cMinSmallMalloc  /* --min-small-malloc   */
 , cNoERQ           /* --no-erq             */
 , cNoTimers        /* --no-timers          */
 , cNoPreload       /* --no-preload         */
 , cPidFile         /* --pidfile            */
 , cRandomdevice    /* --randomdevice       */
 , cRandomSeed      /* --random-seed        */
 , cRegexp          /* --regexp             */
 , cResetTime       /* --reset-time         */
 , cReserved        /* -r                   */
 , cReserveUser     /* --reserve-user       */
 , cReserveMaster   /* --reserve-master     */
 , cReserveSystem   /* --reserve-system     */
 , cStrictEuids     /* --strict-euids       */
 , cNoStrictEuids   /* --no-strict-euids    */
 , cShareVariables    /* --share-variables    */
 , cNoShareVariables  /* --init-variables     */
 , cSwap            /* -s                   */
 , cSwapTime        /* --swap-time          */
 , cSwapVars        /* --swap-variables     */
 , cSwapFile        /* --swap-file          */
 , cSwapCompact     /* --swap-compact       */
 , cSyncHB          /* --synchronous-heart-beat         */
 , cASyncHB         /* --asynchronous-heart-beat        */
 , cWizlistFile     /* --wizlist-file       */
 , cNoWizlistFile   /* --no-wizlist-file    */
#ifdef GC_SUPPORT
 , cGcollectFD      /* --gcollect-outfd     */
#endif
#ifdef USE_TLS
 , cTLSkey          /* --tls-key            */
 , cTLScert         /* --tls-cert           */
 , cTLStrustdir     /* --tls-trustdirectory */
 , cTLStrustfile    /* --tls-trustfile      */
 , cTLScrlfile	    /* --tls-crlfile        */
 , cTLScrldir       /* --tls-crldirectory   */
#endif
#ifdef DEBUG
 , cCheckRefs       /* --check-refcounts    */
 , cCheckState      /* --check-state        */
 , cGobbleFDs       /* --gobble-descriptors */
#endif
#ifdef CHECK_OBJECT_STAT
 , cCheckObjectStat /* --check-object-stat  */
#endif
#ifdef YYDEBUG
 , cYYDebug         /* --yydebug            */
#endif
 , cOptions         /* --options            */
 , cVersion         /* --version            */
 , cLongHelp        /* --longhelp           */
 , cHelp            /* --help               */
} OptNumber;

/* Comprehensive lists of recognized options */

static Option aOptions[]
  = { { 0,   "args",               cArgFile,        MY_TRUE
      , "  --args <filename>\n"
      , "  --args <filename>\n"
        "    Read the options from <filename> as if they were given on the\n"
        "    commandline.\n"
      }

    , { 'P', "inherit",            cInherited,      MY_TRUE
      , "  -P|--inherit <fd-number>\n"
      , "  -P|--inherit <fd-number>\n"
        "    Inherit filedescriptor <fd-number> from the parent process\n"
        "    as socket to listen for connections.\n"
      }

    , { 'u', "udp",                cUdpPort,        MY_TRUE
      , "  -u|--udp <portnumber>\n"
      , "  -u|--udp <portnumber>\n"
        "    Specify the <portnumber> for the UDP port, overriding the compiled-in\n"
        "    default.\n"
      }

    , { 'D', "define",             cDefine,         MY_TRUE
      , "  -D|--define <macro>[=<text>]\n"
      , "  -D|--define <macro>[=<text>]\n"
        "    Add <macro> (optionally to be expanded to <text>) to the list of\n"
        "    predefined macros known by the LPC compiler.\n"
      }

    , { 'E', "eval-cost",          cEvalcost,       MY_TRUE
      , "  -E|--eval-cost <ticks>\n"
      , "  -E|--eval-cost <ticks>\n"
        "    Set the number of <ticks> available for one evaluation thread.\n"
      }

    , { 'M', "master",             cMaster,         MY_TRUE
      , "  -M|--master <filename>\n"
      , "  -M|--master <filename>\n"
        "    Use <filename> for the master object.\n"
      }

    , { 'm', "mudlib",             cMudlib,         MY_TRUE
      , "  -m|--mudlib <pathname>\n"
      , "  -m|--mudlib <pathname>\n"
        "    Use <pathname> as the top directory of the mudlib.\n"
      }

    , { 0,   "debug-file",         cDebugFile,      MY_TRUE
      , "  --debug-file <filename>\n"
      , "  --debug-file <filename>\n"
        "    Log all debug output in <filename> instead of <host>.debug.log .\n"
      }

    , { 0,   "access-file",        cAccessFile,     MY_TRUE
      , "  --access-file <filename>|none\n"
      , "  --access-file <filename>|none\n"
        "    Activate access control with these access permissions data.\n"
        "    If 'none' is given access control is deactivated.\n"
      }

    , { 0,   "access-log",        cAccessLogFile, MY_TRUE
      , "  --access-log <filename>|none\n"
      , "  --access-log <filename>|none\n"
        "    Log valid and rejected connections into this file.\n"
        "    If 'none' is given no log is written.\n"
      }

    , { 0,   "hostname",           cHostname,       MY_TRUE
      , "  --hostname <name>\n"
      , "  --hostname <name>\n"
        "    Use <name> as hostname, instead of what the system says.\n"
      }

    , { 0,   "hostaddr",           cHostaddr,       MY_TRUE
      , "  --hostaddr <addr>\n"
      , "  --hostaddr <addr>\n"
        "    Use <addr> as address of this machine, instead of what the\n"
        "    system says. In particular this address will be used to open\n"
        "    the driver ports.\n"
      }

    , { 0,   "compat",             cCompat,         MY_FALSE
      , "  --compat\n"
      , "  --compat\n"
        "  --no-compat\n"
        "    Select the mode (compat or plain) of the driver.\n"
        "    Note that this choice does not affect the default name of the master\n"
        "    object.\n"
      }

    , { 0,   "no-compat",          cNoCompat,       MY_FALSE
      , "  --no-compat\n"
      , NULL
      }

    , { 'd', "debug",              cDebug,          MY_FALSE
      , "  -d|--debug\n"
      , "  -d|--debug\n"
        "    Generate debug output; repeat the argument for even more output.\n"
      }

    , { 'c', "list-compiles",      cTrace,          MY_FALSE
      , "  -c|--list-compiles\n"
      , "  -c|--list-compiles\n"
        "    List the name of every compiled file on stderr.\n"
      }

    , { 'e', "no-preload",         cNoPreload,      MY_FALSE
      , "  -e|--no-preload\n"
      , "  -e|--no-preload\n"
        "    Pass a non-zero argument (the number of occurences of this option)\n"
        "    to master->preload(), which usually inhibits all preloads of castles\n"
        "    and other objects.\n"
      }

    , { 0,   "erq",                cErq,            MY_TRUE
      , "  --erq <filename> | --erq \"<filename> <erq args>\"\n"
      , "  --erq <filename>\n"
        "  --erq \"<filename> <erq arguments>\"\n"
        "    Use <filename> instead of 'erq' as the name of the ERQ executable.\n"
        "    If the name starts with a '/', it is take to be an absolute pathname,\n"
        "    otherwise it is interpreted relative to " BINDIR ".\n"
        "    If not specified, 'erq' is used as executable name.\n"
        "    With the proper use of quotes it is legal to pass arbitrary arguments\n"
        "    to the erq, however, these may not contain spaces themselves.\n"
      }

    , { 'N', "no-erq",             cNoERQ,          MY_FALSE
      , "  -N|--no-erq\n"
      , "  -N|--no-erq\n"
        "    Don't start the erq demon (if it would be started at all).\n"
      }

    , { 0,   "alarm-time",         cAlarmTime,    MY_TRUE
      , "  --alarm-time <time>\n"
      , "  --alarm-time <time>\n"
        "    The granularity of call_out()s and heartbeats (minimum: 1).\n"
      }

    , { 0,   "heart-interval",     cHBInterval,   MY_TRUE
      , "  --heart-beat-interval <time>\n"
      , "  --heart-beat-interval <time>\n"
        "    The time to elapse between two heartbeats (minimum: 1).\n"
      }

    , { 0,   "sync-heart",         cSyncHB,       MY_FALSE
      , "  --synchronous-heart-beat\n"
      , "  --synchronous-heart-beat\n"
        "    All heartbeats are executed at the same time (modulo granularity).\n"
      }

    , { 0,   "asynchronous-heart-beat",        cASyncHB,       MY_FALSE
      , "  --asynchronous-hear-beat\n"
      , "  --asynchronous-hear-beatt\n"
        "    Heartbeats are executed immediately when they are due (modulo granularity).\n"
      }

    , { 't', "no-timers",           cNoTimers,        MY_FALSE
      , "  -t|--no-timers\n"
      , "  -t|--no-timers\n"
        "    Disable heartbeats and call_outs.\n"
      }

    , { 'f', "funcall",            cFuncall,        MY_TRUE
      , "  -f|--funcall <word>\n"
      , "  -f|--funcall <word>\n"
        "    The lfun master->flag() is called with <word> as argument before the\n"
        "    gamedriver accepts network connections.\n"
      }

    , { 0,   "cleanup-time",       cCleanupTime,    MY_TRUE
      , "  --cleanup-time <time>\n"
      , "  --cleanup-time <time>\n"
        "    The idle time in seconds for an object before the driver tries to\n"
        "    clean it up. This time should be substantially higher than the\n"
        "    reset time. A time <= 0 disables the cleanup mechanism.\n"
      }

    , { 0,   "reset-time",         cResetTime,      MY_TRUE
      , "  --reset-time <time>\n"
      , "  --reset-time <time>\n"
        "    The time in seconds for an object before it is reset.\n"
        "    A time <= 0 disables the reset mechanism.\n"
      }

    , { 0,   "regexp",             cRegexp,         MY_TRUE
      , "  --regexp pcre|traditional\n"
      , "  --regexp pcre|traditional\n"
        "    Select the default regexp package.\n"
      }

    , { 0,   "max-array",          cMaxArray,       MY_TRUE
      , "  --max-array <size>\n"
      , "  --max-array <size>\n"
        "    The maximum number of elements an array can hold.\n"
        "    Set to 0, arrays of any size are allowed.\n"
      }

    , { 0,   "max-callouts",       cMaxCallouts,    MY_TRUE
      , "  --max-callouts <number>\n"
      , "  --max-callouts <number>\n"
        "    The maximum number of callouts at one time.\n"
        "    Set to 0, any number of callouts is allowed.\n"
      }

    , { 0,   "max-mapping",        cMaxMapping,     MY_TRUE
      , "  --max-mapping <size>\n"
      , "  --max-mapping <size>\n"
        "    The maximum number of elements (keys+values) a mapping can hold.\n"
        "    Set to 0, mappings of any size are allowed.\n"
      }

    , { 0,   "max-mapping-keys",   cMaxMappingKeys, MY_TRUE
      , "  --max-mapping-keys <size>\n"
      , "  --max-mapping-keys <size>\n"
        "    The maximum number of entries (keys) a mapping can hold.\n"
        "    Set to 0, mappings of any size are allowed.\n"
      }

    , { 0,   "max-bytes",          cMaxBytes,       MY_TRUE
      , "  --max-bytes <size>\n"
      , "  --max-bytes <size>\n"
        "    The maximum number of bytes one read_bytes()/write_bytes() call\n"
        "    can handle.\n"
        "    Set to 0, reads and writes of any size are allowed.\n"
      }

    , { 0,   "max-file",           cMaxFile,        MY_TRUE
      , "  --max-file <size>\n"
      , "  --max-file <size>\n"
        "    The maximum number of bytes one read_file()/write_file() call\n"
        "    can handle.\n"
        "    Set to 0, reads and writes of any size are allowed.\n"
      }

    , { 0,   "max-write-buffer", cMaxWriteBuffer,  MY_TRUE
      , "  --max-write-buffer<size>\n"
      , "  --max-write-buffer <size>\n"
        "    The maximum number of bytes to be kept pending for each socket\n"
        "    to write.\n"
        "    Set to 0, an unlimited amount of data can be kept pending.\n"
      }

    , { 's', NULL,                 cSwap,           MY_TRUE
      , NULL
      , "  -s <time>  | --swap-time <time>\n"
        "  -s v<time> | --swap-variables <time>\n"
        "    Time in seconds before an object (or its variables) are swapped out.\n"
        "    A time less or equal 0 disables swapping.\n"
      }

    , { 0,   "swap-time",          cSwapTime,       MY_TRUE
      , "  -s <time>  | --swap-time <time>\n"
      , NULL
      }

    , { 0,   "swap-variables",     cSwapVars,       MY_TRUE
      , "  -s v<time> | --swap-variables <time>\n"
      , NULL
      }

    , { 0,   "swap-file",          cSwapFile,       MY_TRUE
      , "  -s f<name> | --swap-file <name>\n"
      , "  -s f<name> | --swap-file <name>\n"
        "    Swap into file <name> instead of " SWAP_FILE ".<host> .\n"
      }

    , { 0,   "swap-compact",       cSwapCompact,    MY_FALSE
      , "  -s c       | --swap-compact\n"
      , "  -s c | --swap-compact\n"
        "    Reuse free space in the swap file immediately.\n"
      }

    , { 0,   "hard-malloc-limit",  cMaxMalloc,      MY_TRUE
      , "  --hard-malloc-lmit <size>\n"
      , "  --hard-malloc-limit <size>\n"
        "    Restrict total memory allocation to <size> bytes. A <size> of 0\n"
        "    or 'unlimited' removes any restriction.\n"
      }

    , { 0,   "soft-malloc-limit",  cSoftMallocLimit,  MY_TRUE
      , "  --soft-malloc-limit <size>\n"
      , "  --soft-malloc-limit <size>\n"
        "    If total memory allocation exceeds <size> bytes, inform the mudlib\n"
        "    master about a developing low memory situation. A <size> of 0\n"
        "    or 'unlimited' removes the threshold. <size> must be smaller than\n"
        "    --hard-malloc-limit.\n"
      }

    , { 0,   "min-malloc",         cMinMalloc,      MY_TRUE
      , "  --min-malloc <size>\n"
      , "  --min-malloc <size>\n"
        "  --min-small-malloc <size>\n"
        "    Determine the sizes for the explicite initial large resp. small chunk\n"
        "    allocation. A size of 0 disables the explicite initial allocations.\n"
      }

    , { 0,   "min-small-malloc",   cMinSmallMalloc, MY_TRUE
      , "  --min-small-malloc <size>\n"
      , NULL
      }

    , { 'r', NULL,                 cReserved,       MY_TRUE
      , NULL
      , "  -r u<size> | --reserve-user <size>\n"
        "  -r m<size> | --reserve-master <size>\n"
        "  -r s<size> | --reserve-system <size>\n"
        "    Reserve <size> amount of memory for user/master/system allocations to\n"
        "    be held until main memory runs out.\n"
      }

    , { 0,   "reserve-user",       cReserveUser,    MY_TRUE
      , "  -r u<size> | --reserve-user <size>\n"
      , NULL
      }

    , { 0,   "reserve-master",     cReserveMaster,  MY_TRUE
      , "  -r m<size> | --reserve-master <size>\n"
      , NULL
      }

    , { 0,   "reserve-system",     cReserveSystem,  MY_TRUE
      , "  -r s<size> | --reserve-system <size>\n"
      , NULL
      }

    , { 0,   "filename-spaces",    cFilenameSpaces,  MY_FALSE
      , "  --filename-spaces\n"
      , "  --filename-spaces\n"
        "  --no-filename-spaces\n"
        "    Allow/disallow the use of spaces in filenames.\n"
      }

    , { 0,   "no-filename-spaces",    cNoFilenameSpaces,  MY_FALSE
      , "  --no-filename-spaces\n"
      , NULL
      }

    , { 0,   "strict-euids",       cStrictEuids,    MY_FALSE
      , "  --strict-euids\n"
      , "  --strict-euids\n"
        "  --no-strict-euids\n"
        "    Enforce/don't enforce the proper use of euids.\n"
      }

    , { 0,   "no-strict-euids",     cNoStrictEuids,    MY_FALSE
      , "  --no-strict-euids\n"
      , NULL
      }

    , { 0,   "share-variables",    cShareVariables,  MY_FALSE
      , "  --share-variables\n"
      , "  --share-variables\n"
        "  --init-variables\n"
        "    Select how clones initialize their variables:\n"
        "      - by sharing the current values of their blueprint\n"
        "      - by initializing them afresh (using __INIT()).\n"
      }

    , { 0,   "init-variables",    cNoStrictEuids,  MY_FALSE
      , "  --init-variables\n"
      , NULL
      }

#ifdef USE_TLS
    , { 0,   "tls-key",               cTLSkey,            MY_TRUE
      , "  --tls-key <pathname>|none\n"
      , "  --tls-key <pathname>|none\n"
#  ifdef TLS_DEFAULT_KEYFILE
        "    Use <pathname> as the x509 keyfile, default is '" TLS_DEFAULT_KEYFILE "'.\n"
#  else
        "    Use <pathname> as the x509 keyfile, default is 'none'.\n"
#  endif
        "    If relative, <pathname> is interpreted relative to <mudlib>.\n"
        "    If 'none' is given TLS is deactivated.\n"
      }

    , { 0,   "tls-cert",              cTLScert,           MY_TRUE
      , "  --tls-cert <pathname>\n"
      , "  --tls-cert <pathname>\n"
#  ifdef TLS_DEFAULT_CERTFILE
        "    Use <pathname> as the x509 certfile, default is '" TLS_DEFAULT_CERTFILE "'.\n"
#  else
        "    Use <pathname> as the x509 certfile, default is 'none'.\n"
#  endif
        "    If relative, <pathname> is interpreted relative to <mudlib>.\n"
      }
    , { 0,      "tls-trustfile",       cTLStrustfile,     MY_TRUE
      , "  --tls-trustfile <pathname>|none\n"
      , "  --tls-trustfile <pathname>|none\n"
        "    Use <pathname> as the filename holding your trusted PEM certificates,\n"
#  ifdef TLS_DEFAULT_TRUSTFILE
       "     default is '" TLS_DEFAULT_TRUSTFILE "'.\n"
#  else
       "     default is 'none'.\n"
#  endif
        "    If relative, <pathname> is interpreted relative to <mudlib>.\n"
      }
    , { 0,      "tls-trustdirectory",  cTLStrustdir,      MY_TRUE
      , "  --tls-trustdirectory <pathname>|none\n"
      , "  --tls-trustdirectory <pathname>|none\n"
        "    Use <pathname> as the directory where your trusted PEM certificates reside,\n"
#  ifdef TLS_DEFAULT_TRUSTDIRECTORY
       "     default is '" TLS_DEFAULT_TRUSTDIRECTORY "'.\n"
#  else
       "     default is 'none'.\n"
#  endif
        "    If relative, <pathname> is interpreted relative to <mudlib>.\n"
      }
    , { 0,      "tls-crlfile",       cTLScrlfile,     MY_TRUE
      , "  --tls-crlfile <pathname>|none\n"
      , "  --tls-crlfile <pathname>|none\n"
        "    Use <pathname> as the filename holding your certificate revocation lists,\n"
#  ifdef TLS_DEFAULT_CRLFILE
       "     default is '" TLS_DEFAULT_CRLFILE "'.\n"
#  else
       "     default is 'none'.\n"
#  endif
        "    If relative, <pathname> is interpreted relative to <mudlib>.\n"
      }
    , { 0,      "tls-crldirectory",  cTLScrldir,      MY_TRUE
      , "  --tls-crldirectory <pathname>|none\n"
      , "  --tls-crldirectory <pathname>|none\n"
        "    Use <pathname> as the directory where your certificate revocation lists reside,\n"
#  ifdef TLS_DEFAULT_CRLDIRECTORY
       "     default is '" TLS_DEFAULT_CRLDIRECTORY "'.\n"
#  else
       "     default is 'none'.\n"
#  endif
        "    If relative, <pathname> is interpreted relative to <mudlib>.\n"
      }
#endif /* USE_TLS */

    , { 0,   "wizlist-file",       cWizlistFile,    MY_TRUE
      , "  --wizlist-file <filename>\n"
      , "  --wizlist-file <filename>\n"
        "  --no-wizlist-file\n"
        "    Read and save the wizlist in the named file (always interpreted\n"
        "    relative the mudlib); resp. don't read or save the wizlist.\n"
      }

    , { 0,   "no-wizlist-file",    cNoWizlistFile,  MY_FALSE
      , "  --no-wizlist-file\n"
      ,
      }

    , { 0,   "pidfile",            cPidFile,        MY_TRUE
      , "  --pidfile <filename>\n"
      , "  --pidfile <filename>\n"
        "    Write the pid of the driver process into <filename>.\n"
      }

    , { 0,   "randomdevice",       cRandomdevice,   MY_TRUE
      , "  --randomdevice <filename>\n"
      , "  --randomdevice <filename>\n"
        "    Determines the source of the seed for the random number generator.\n"
        "    (tries /dev/urandom by default and uses system clock as fallback)\n"
      }

    , { 0,   "random-seed",        cRandomSeed,     MY_TRUE
      , "  --random-seed <num>\n"
      , "  --random-seed <num>\n"
        "    Seed value for the random number generator. If not given, the\n"
        "    driver chooses a seed value on its own.\n"
      }

#ifdef GC_SUPPORT
    , { 0,   "gcollect-outfd",     cGcollectFD,     MY_TRUE
      , "  --gcollect-outfd <filename>|<num>\n"
      , "  --gcollect-outfd <filename>|<num>\n"
        "    Garbage collector output (like a log of all reclaimed memory blocks)\n"
        "    is sent to <filename> (or inherited fd <num>) instead of stderr.\n"
      }
#endif

#ifdef DEBUG
    , { 0,   "check-refcounts",    cCheckRefs,      MY_FALSE
      , "  --check-refcounts\n"
      , "  --check-refcounts\n"
        "    Every backend cycle, all refcounts in the system are checked.\n"
        "    SLOW!\n"
      }

    , { 0,   "check-state",        cCheckState,     MY_TRUE
      , "  --check-state <lvl>\n"
      , "  --check-state <lvl>\n"
        "    Perform a regular simplistic check of the virtual machine according\n"
        "    to <lvl>:\n"
        "      = 0: no check\n"
        "      = 1: once per backend loop\n"
        "      = 2: at various points in the backend loop\n"
      }

    , { 0,   "gobble-descriptors", cGobbleFDs,      MY_TRUE
      , "  --gobble-descriptors <num>\n"
      , "  --gobble-descriptors <num>\n"
        "    <num> (more) filedescriptors are used up. You'll know when you need it.\n"
      }
#endif

#ifdef CHECK_OBJECT_STAT
    , { 0,   "check-object-stat",  cCheckObjectStat, MY_FALSE
      , "  --check-object-stat\n"
      , "  --check-object-stat\n"
        "    Activate tracing of the object size statistic - available in order\n"
        "    to find the bug in the statistics.\n"
      }
#endif

#ifdef YYDEBUG
    , { 'y', "yydebug",            cYYDebug,        MY_FALSE
      , "  -y|--yydebug\n"
      , "  -y|--yydebug\n"
        "    Enable debugging of the LPC compiler.\n"
      }
#endif

    , { 0,   "options",            cOptions,        MY_FALSE
      , "  --options\n"
      , "  --options\n"
        "    Print the version and compilation options of the driver, then exit.\n"
      }

    , { 'V', "version",            cVersion,        MY_FALSE
      , "  -V|--version\n"
      , "  -V|--version\n"
        "    Print the version of the driver, then exit.\n"
      }

    , { 0,   "longhelp",           cLongHelp,       MY_FALSE
      , "  --longhelp\n"
      , "  --longhelp\n"
        "    Display this help and exit.\n"
      }

    , { 'h', "help",               cHelp,           MY_FALSE
      , "  -h|-?|--help\n"
      , "  -h|-?|--help\n"
        "    Display the short help text and exit.\n"
      }

    , { '?', NULL,                 cHelp,           MY_FALSE
      , NULL
      , NULL
      }

    };

/*-------------------------------------------------------------------------*/

/* Internal management structure to handle an (argc,argv) input source.
 * It is allocated to the proper length.
 */

typedef struct InputSource {
    struct InputSource * next;  /* Link pointer */
    int     arg;   /* Number of next argument to evaluate */
    int     argc;  /* Total number of arguments */
    char ** argv;  /* Allocated array of argument pointers */
    char  * name;  /* Filename from where the arguments have been read,
                    * NULL for commandline */
    char    data[1];
      /* Block holding the filename and the text read from
       * the file.
       *
       * The data read from the file has been broken up into separate
       * strings which are pointed to from the argv array.
       */
} InputSource;

/*-------------------------------------------------------------------------*/
static const char *
drivertag (void)

/* Return the driver's type tag string.
 */

{
    if (strcmp(RELEASE_LONGTYPE, ""))
        return " (" RELEASE_LONGTYPE ")";

    return "";
} /* drivertag() */

/*-------------------------------------------------------------------------*/
static void
version (void)

/* Print the version of the gamedriver.
 */

{
  fputs("LDMud ", stdout);

  fputs(DRIVER_VERSION, stdout);

  fputs(LOCAL_LEVEL " - a LPMud Game Driver.\n"
        "\nRelease:  " PROJ_VERSION
       , stdout);

  fputs(drivertag(), stdout);

  fputs("; " RELEASE_DATE
        "\nCompiled: " __DATE__
#ifdef __TIME__
        " " __TIME__
#endif
        "\n"
       , stdout);
} /* version() */

/*-------------------------------------------------------------------------*/
static void
options (void)

/* Print the version of the gamedriver and the compile time options.
 */

{
  version();
  fputs("\n           Mode: "
#ifdef COMPAT_MODE
        "Compat"
#else
        "Plain (aka cross-compat)"
#endif
#ifdef STRICT_EUIDS
        " with strict euids.\n"
#else
        "\n"
#endif
       , stdout);

  fputs("    Mudlib path: " MUD_LIB "\n"
        "    Binary path: " BINDIR "\n"
#ifdef MASTER_NAME
        "  Master object: <mudlib>/" MASTER_NAME "\n"
#elif defined(COMPAT_MODE)
        "  Master object: <mudlib>/" COMPAT_MASTER "\n"
#else
        "  Master object: <mudlib>/" PLAIN_MASTER "\n"
#endif
       , stdout);

  printf(" Multiple ports: %d ports max, default is %d.\n", MAXNUMPORTS, PORTNO);

  printf("            UDP: default port is %d.\n", UDP_PORT);

#ifdef ERQ_DEMON
  printf("            ERQ: max data length: send %d / recv %d bytes.\n"
         "                 directory: %s.\n"
        , ERQ_MAX_SEND, ERQ_MAX_REPLY, ERQ_DIR);
#else
  fputs("            ERQ: disabled.\n", stdout);
#endif

#ifndef INPUT_ESCAPE
  fputs("   Input Escape: '!'\n", stdout);
#else
  fputs("   Input Escape: '" INPUT_ESCAPE "'\n", stdout);
#endif

#ifdef ACCESS_FILE
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

#ifdef WIZLIST_FILE
  fputs("        Wizlist: saved in <mudlib>/" WIZLIST_FILE "\n", stdout);
#else
  fputs("        Wizlist: not saved\n", stdout);
#endif

    /* Print the language options, nicely indented. */
    {
        char * optstrings[] = { "" /* have at least one string in here */
#ifdef USE_ARRAY_CALLS
                              , "call_other() on (object*) enabled\n"
#endif
#ifdef USE_PARSE_COMMAND
                              , "parse_command() enabled\n"
#endif
#ifdef USE_PROCESS_STRING
                              , "process_string() enabled\n"
#endif
#ifdef USE_SET_LIGHT
                              , "set_light() enabled\n"
#endif
#ifdef USE_SET_IS_WIZARD
                              , "set_is_wizard() enabled\n"
#endif
#ifdef SHARE_VARIABLES
                              , "clones initialized from blueprint\n"
#else
                              , "clones initialized by __INIT()\n"
#endif
#ifdef USE_DEPRECATED
                              , "obsolete and deprecated efuns enabled\n"
#endif
#ifdef NO_NEGATIVE_RANGES
                              , "assignments to negative ranges disabled\n"
#endif
#ifdef USE_STRUCTS
                              , "structs enabled\n"
#endif
#ifdef USE_NEW_INLINES
                              , "new inline closures enabled\n"
#endif
#ifdef HAS_ICONV
                              , "convert_charset() via iconv available\n"
#endif
#ifdef ALLOW_FILENAME_SPACES
                              , "filenames may contain space characters\n"
#else
                              , "filenames may not contain space characters\n"
#endif
                              };
        size_t nStrings = sizeof(optstrings) / sizeof(optstrings[0]);
        size_t i;

        for (i = 1; i < nStrings; i++)
        {
            if (1 == i)
                fputs("       Language: ", stdout);
            else
                fputs("                 ", stdout);
            fputs(optstrings[i], stdout);
        }
    } /* print language options */
    fputs("                 default regexps: "
#ifdef USE_PCRE
                                            "PCRE\n"
#else
                                            "traditional\n"
#endif
         , stdout);

    /* Print the package options, nicely indented. */
    {
        char * optstrings[] = { "" /* have at least one string in here */
#ifdef HAS_IDN
                              , "idna supported\n"
#endif
#ifdef USE_XML
                              , "XML supported ("
#  if defined(HAS_XML2)
                                "libxml2"
#  elif defined(HAS_IKSEMEL)
                                "iksemel"
#  else
                                "<unknown>"
#  endif
                                ")\n"
#endif
#ifdef USE_IPV6
                              , "IPv6 supported\n"
#endif
#ifdef USE_MCCP
                              , "MCCP supported\n"
#endif
#ifdef USE_MYSQL
                              , "mySQL supported\n"
#endif
#ifdef USE_PGSQL
                              , "PostgreSQL supported\n"
#endif
#ifdef USE_SQLITE
                              , "SQLite3 supported\n"
#endif
#ifdef USE_ALISTS
                              , "Alists supported\n"
#endif
#ifdef USE_TLS
                              , "TLS supported ("
#  if defined(HAS_OPENSSL)
                                               "OpenSSL"
#  elif defined(HAS_GNUTLS)
                                               "GnuTLS"
#  else
                                               "<unknown>"
#  endif
                                               ", x509 key: '"
#  ifdef TLS_DEFAULT_KEYFILE
                                  TLS_DEFAULT_KEYFILE
#  else
                                               "none"
#  endif
                                               "', cert: '"
#  ifdef TLS_DEFAULT_CERTFILE
                                  TLS_DEFAULT_CERTFILE
#  else
                                               "none"
#  endif
                                               "')\n"
#endif
                              };
        size_t nStrings = sizeof(optstrings) / sizeof(optstrings[0]);
        size_t i;

        fputs("       Packages: PCRE ", stdout);
        fputs(rx_pcre_version(), stdout);
        fputs("\n", stdout);
        for (i = 1; i < nStrings; i++)
        {
            fputs("                 ", stdout);
            fputs(optstrings[i], stdout);
        }
    } /* print package options */

  printf(" Runtime limits: max read file size:     %7d\n"
         "                 max byte read/write:    %7d\n"
         "                 max socket buf size:    %7d\n"
         "                 max write buf size:     %7d\n"
         "                 max eval cost:        %9d %s\n"
         "                 catch eval cost:        %7d\n"
         "                 master eval cost:       %7d\n"
         "                 eval stack:             %7d\n"
         "                 user call depth:        %7d\n"
         "                 max call depth:         %7d\n"
         "                 max bitfield length:    %7d\n"
         "                 max array size:         %7d\n"
         "                 max mapping size:       %7d\n"
         "                 max mapping keys:       %7d\n"
         "                 max number callouts:    %7d\n"
         "                 max number players:     %7d\n"
         "                 ed cmd/cmd ratio:       %7d:1\n"
#if defined(TRACE_CODE)
         "                 max trace length:       %7d\n"
#endif
        , READ_FILE_MAX_SIZE, MAX_BYTE_TRANSFER
        , SET_BUFFER_SIZE_MAX
        , WRITE_BUFFER_MAX_SIZE
        , MAX_COST
#if defined(DYNAMIC_COSTS)
        , "(dynamic)"
#else
        , ""
#endif
        , CATCH_RESERVED_COST, MASTER_RESERVED_COST
        , EVALUATOR_STACK_SIZE
        , MAX_USER_TRACE, MAX_TRACE
        , MAX_BITS, MAX_ARRAY_SIZE
        , MAX_MAPPING_SIZE, MAX_MAPPING_KEYS
        , MAX_CALLOUTS, MAX_PLAYERS
        , ALLOWED_ED_CMDS
#ifdef TRACE_CODE
        , TOTAL_TRACE_LENGTH
#endif
        );

  printf("         Timing: reset:                  %7d s\n"
         "                 clean up:               %7d s\n"
         "                 alarm interval:         %7d s\n"
         "                 heartbeat interval:     %7d s %s\n"
        , TIME_TO_RESET, TIME_TO_CLEAN_UP
        , ALARM_TIME, HEART_BEAT_INTERVAL
#ifdef SYNCHRONOUS_HEART_BEAT
        , "(synchronous)"
#else
        , ""
#endif
        );

  printf("       Swapping: objects              ");
  if (TIME_TO_SWAP > 0)
      printf("after %4d s\n", TIME_TO_SWAP);
  else
      printf("     never\n");
  printf("                 variables            ");
  if (TIME_TO_SWAP_VARIABLES > 0)
      printf("after %4d s\n", TIME_TO_SWAP_VARIABLES);
  else
      printf("     never\n");
  if (SWAP_FILE[0] == '/')
      printf("                 file: %s.<host>\n"
            , SWAP_FILE
            );
  else
      printf("                 file: <mudlib>/%s.<host>\n"
            , SWAP_FILE
            );

  printf("       Compiler: max stack size:          %6d\n"
         "                 max local variables:     %6d\n"
         "                 max define length:       %6d\n"
        , COMPILER_STACK_SIZE
        , MAX_LOCAL
        , DEFMAX
        );

  printf("         Memory: using %s\n"
         "                 reserved user size:    %8d\n"
         "                 reserved master size:  %8d\n"
         "                 reserved system size:  %8d\n"
         "                 initial allocation:   %9d\n"
         "                 initial small alloc:   %8d\n"
#ifdef MALLOC_sysmalloc
        , "system malloc"
#elif defined(MALLOC_slaballoc)
        , "slaballoc"
#elif defined(MALLOC_smalloc)
        , "smalloc"
#else
        , "unknown malloc"
#endif
#if defined(MALLOC_CHECK) || defined(MALLOC_TRACE) || defined(MALLOC_LPC_TRACE) || defined(MALLOC_SBRK_TRACE)
              " ("
#    if defined(MALLOC_CHECK)
                " MALLOC_CHECK"
#    endif
#    if defined(MALLOC_TRACE)
                " MALLOC_TRACE"
#    endif
#    if defined(MALLOC_LPC_TRACE)
                " MALLOC_LPC_TRACE"
#    endif
#    if defined(MALLOC_SBRK_TRACE)
                " MALLOC_SBRK_TRACE"
#    endif
              " )"
#endif
        , RESERVED_USER_SIZE
        , RESERVED_MASTER_SIZE
        , RESERVED_SYSTEM_SIZE
        , MIN_MALLOCED
        , MIN_SMALL_MALLOCED
        );

  printf("                 hard memory allocation limit: ");
  if (HARD_MALLOC_LIMIT_DEFAULT > 0)
      printf("%9d\n", HARD_MALLOC_LIMIT_DEFAULT);
  else
      printf("unlimited\n");
  
  printf("                 soft memory allocation limit: ");
    if (SOFT_MALLOC_LIMIT_DEFAULT > 0)
        printf("%9d\n", SOFT_MALLOC_LIMIT_DEFAULT);
    else
        printf("unlimited\n");
    
  printf("Internal tables: shared string hash:      %6d entries\n"
         "                 object hash:             %6d entries\n"
         "                 reserved name hash:      %6d entries\n"
         "                 apply cache:             %6d entries\n"
#ifdef RXCACHE_TABLE
         "                 regexp cache:            %6d entries\n"
#endif
        , HTABLE_SIZE
        , OTABLE_SIZE
        , ITABLE_SIZE
        , 1<<APPLY_CACHE_BITS
#ifdef RXCACHE_TABLE
        , RXCACHE_TABLE
#endif
        );

#ifdef DEBUG
  printf("  Debug options: check state: %d ("
        , check_state_level
        );
  switch (check_state_level)
  {
  case 0: fputs("never", stdout); break;
  case 1: fputs("once per loop", stdout); break;
  case 2: fputs("several times per loop", stdout); break;
  default: fputs("???", stdout); break;
  }
  fputs(")\n", stdout);

  if (check_a_lot_ref_counts_flag)
      fputs("                 check refcounts\n", stdout);
  else
      fputs("                 don't check refcounts\n", stdout);
#endif

    /* Print the other options, nicely formatted. */
    {
        char * optstrings[] = { "  Other options: "
#       if defined(DEBUG)
                              , "DEBUG"
#       endif
#       if defined(CHECK_OBJECT_STAT)
                              , "CHECK_OBJECT_STAT"
#       endif
#       if defined(DEBUG_TELNET)
                              , "DEBUG_TELNET"
#       endif
#       if defined(DEBUG_MALLOC_ALLOCS)
                              , "DEBUG_MALLOC_ALLOCS"
#       endif
#       if defined(YYDEBUG)
                              , "YYDEBUG"
#       endif
#       if defined(NO_INLINES)
                              , "NO_INLINES"
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
                              , "OPCPROF"
#           if defined(OPCPROF_VERBOSE)
                              , "OPCPROF_VERBOSE"
#           endif
#       endif
#       if defined(CHECK_MAPPINGS)
                              , "CHECK_MAPPINGS"
#       endif
#       if defined(CHECK_MAPPING_TOTAL)
                              , "CHECK_MAPPING_TOTAL"
#       endif
#       if defined(CHECK_OBJECT_REF)
                              , "CHECK_OBJECT_REF"
#       endif
#       if defined(CHECK_OBJECT_GC_REF)
                              , "CHECK_OBJECT_GC_REF"
#       endif
#       if defined(DUMP_GC_REFS)
                              , "DUMP_GC_REFS"
#       endif
#       if defined(NEW_CLEANUP)
                              , "NEW_CLEANUP"
#       endif
#       if defined(LOG_NEW_CLEANUP)
                              , "LOG_NEW_CLEANUP"
#       endif
#       if defined(USE_AVL_FREELIST)
                              , "USE_AVL_FREELIST"
#       endif
#       if defined(SLABALLOC_DYNAMIC_SLABS)
                              , "SLABALLOC_DYNAMIC_SLABS"
#       endif
#       if defined(MALLOC_ORDER_LARGE_FREELISTS)
                              , "MALLOC_ORDER_LARGE_FREELISTS"
#       endif
#       if defined(MALLOC_ORDER_SLAB_FREELISTS)
                              , "MALLOC_ORDER_SLAB_FREELISTS"
#       endif
#       if defined(MALLOC_EXT_STATISTICS)
                              , "MALLOC_EXT_STATISTICS"
#       endif
#       if defined(EXT_STRING_STATS)
                              , "EXT_STRING_STATS"
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
                    curlen = iInitial + strlen(optstrings[i]) + 2;
                }
                fputs(optstrings[i], stdout);
                if (i < nStrings-1)
                    fputs(", ", stdout);
            }
            fputs(".\n", stdout);
        }
    }
} /* options() */

/*-------------------------------------------------------------------------*/
static void
shortusage (void)

/* Print the short help information to stdout. */

{
  int i;

  version();
  fputs("\n"
"Usage: ldmud [options] [<portnumber>...]\n"
"\nOptions are:\n"
"\n"
       , stdout);

  for (i = 0; (size_t)i < sizeof(aOptions) / sizeof(aOptions[0]); i++)
      if (aOptions[i].pSHelp != NULL)
          fputs(aOptions[i].pSHelp, stdout);
} /* shortusage() */

/*-------------------------------------------------------------------------*/
static void
usage (void)

/* Print the help information to stdout. */

{
  int i;

  version();
  fputs("\n"
"Usage: ldmud [options] [<portnumber>...]\n"
"\nOptions are:\n"
       , stdout);

  for (i = 0; (size_t)i < sizeof(aOptions) / sizeof(aOptions[0]); i++)
  {
      if (aOptions[i].pLHelp != NULL)
      {
          fputs("\n", stdout);
          fputs(aOptions[i].pLHelp, stdout);
      }
  }
} /* usage() */

/*-------------------------------------------------------------------------*/
static int
eval_arg (int eOption, const char * pValue)

/* Callback from getargs() for the first scan of the commandline
 * arguments. <eOption> is the option recognized, <pValue> a value
 * or NULL.
 * Return hrSuccess on success, hrError on a failure.
 * Return hrArgFile if the given value is the name of an arguments file.
 */

{
    switch (eOption)
    {
    case cArgument:
        if (numports >= MAXNUMPORTS)
            fprintf(stderr, "Portnumber '%s' ignored.\n", pValue);
        else if (atoi(pValue))
              port_numbers[numports++] = atoi(pValue);
        else
            fprintf(stderr, "Illegal portnumber '%s' ignored.\n", pValue);
        break;

    case cArgFile:
        return hrArgFile;

    case cInherited:
        if (numports >= MAXNUMPORTS)
            fprintf(stderr, "fd '%s' ignored.\n", pValue);
        else if (atoi(pValue))
              port_numbers[numports++] = -atoi(pValue);
        else
            fprintf(stderr, "Illegal fd '%s' ignored.\n", pValue);
        break;

    case cUdpPort:
        if (atoi(pValue))
            udp_port = atoi(pValue);
        else
            fprintf(stderr, "Illegal portnumber '%s' ignored.\n", pValue);
        break;

    case cDefine:
        {
            struct lpc_predef_s *tmp;

            tmp = (struct lpc_predef_s *) xalloc(sizeof(struct lpc_predef_s));
            tmp->flag = string_copy(pValue);
            tmp->next = lpc_predefs;
            lpc_predefs = tmp;
        }
        break;

    case cEvalcost:
      {
        long val;

        val = atoi(pValue);
        if (val >= 0)
            def_eval_cost = val;
        else
            fprintf(stderr, "Illegal eval-cost '%s' ignored.\n", pValue);
        break;
      }

    case cCompat:
        compat_mode = MY_TRUE;
        break;

    case cNoCompat:
        compat_mode = MY_FALSE;
        break;

    case cNoPreload:
        e_flag++;
        break;

    case cErq:
      {
        char * begin_arg;

        if (erq_file != NULL)
            free(erq_file);
        if (erq_args != NULL)
        {
            free(erq_args);
            erq_args = NULL;
        }
        erq_file = strdup(pValue);
        begin_arg = strchr(erq_file, ' ');
        if (begin_arg)
        {
            /* Split the string into command and arguments */

            int num_args;
            char * cp;

            /* Skip leading spaces */
            *begin_arg++ = '\0';
            while (*begin_arg == ' ')
                begin_arg++;

            /* Count the arguments */
            for (num_args = 0, cp = begin_arg; *cp != '\0'; )
            {
                /* Found an argument: skip it */
                num_args++;
                while (*cp != ' ' && *cp != '\0')
                    cp++;

                /* Skip trailing spaces */
                while (*cp == ' ')
                    cp++;
            }

            if (num_args != 0)
            {
                /* There are arguments!
                 * Put them into the argument array.
                 */
                erq_args = malloc(sizeof(*erq_args) * (num_args+3));
                erq_args[0] = "erq";
                erq_args[1] = "--forked";
                erq_args[num_args+2] = NULL;

                for (num_args = 2, cp = begin_arg; *cp != '\0'; )
                {
                    /* Found an argument: store and skip it */
                    erq_args[num_args++] = cp;
                    while (*cp != ' ' && *cp != '\0')
                        cp++;

                    /* Skip trailing spaces, replacing them by \0 to
                     * ensure proper string termination.
                     */
                    while (*cp == ' ')
                        *cp++ = '\0';
                }
            }
        }
        break;
      }

    case cNoERQ:
        no_erq_demon++;
        break;

    case cDebug:
        d_flag++;
        break;

    case cTrace:
        comp_flag = MY_TRUE;
        break;

    case cAlarmTime:
      {
        long t = atoi(pValue);

        if (t >= 1)
        {
            alarm_time = t;
        }
        else
            fprintf(stderr, "Illegal alarm-time '%s' ignored.\n", pValue);
        break;
      }

    case cHBInterval:
      {
        long t = atoi(pValue);

        if (t >= 1)
        {
            heart_beat_interval = t;
        }
        else
            fprintf(stderr, "Illegal heart-interval '%s' ignored.\n", pValue);
        break;
      }

    case cSyncHB:
        synch_heart_beats = MY_TRUE;
        break;

    case cASyncHB:
        synch_heart_beats = MY_FALSE;
        break;

    case cNoTimers:
        disable_timers_flag = MY_TRUE;
        break;

    case cCleanupTime:
        if (atoi(pValue))
        {
            time_to_cleanup = atoi(pValue);
            if (time_to_cleanup < 0)
                time_to_cleanup = 0;
        }
        else
            fprintf(stderr, "Illegal cleanup-time '%s' ignored.\n", pValue);
        break;

    case cResetTime:
        if (atoi(pValue))
        {
            time_to_reset = atoi(pValue);
            if (time_to_reset < 0)
                time_to_reset = 0;
        }
        else
            fprintf(stderr, "Illegal cleanup-time '%s' ignored.\n", pValue);
        break;

    case cRegexp:
        if (!strcasecmp(pValue, "pcre"))
            regex_package = RE_PCRE;
        else if (!strcasecmp(pValue, "traditional"))
            regex_package = RE_TRADITIONAL;
        else
            fprintf(stderr, "Unknown regexp package '%s' ignored.\n", pValue);
        break;

    case cMaxArray:
    case cMaxBytes:
    case cMaxCallouts:
    case cMaxFile:
    case cMaxMapping:
    case cMaxMappingKeys:
      {
        long val = atoi(pValue);

        if (val >= 0)
        {
            switch(eOption)
            {
            case cMaxArray:        def_array_size = (size_t)val;   break;
            case cMaxBytes:        def_byte_xfer = val;            break;
            case cMaxCallouts:     def_callouts = val;             break;
            case cMaxFile:         def_file_xfer = val;            break;
            case cMaxMapping:      def_mapping_size = (size_t)val; break;
            case cMaxMappingKeys:  def_mapping_keys = (size_t)val; break;
            }
        }
        else
            fprintf(stderr, "Illegal value for limit '%s' ignored.\n", pValue);
        break;
      }

    case cMaxWriteBuffer:
      {
        long val = atoi(pValue);

        if (val >= 0)
            write_buffer_max_size = val;
        else
            fprintf(stderr, "Illegal value for limit '%s' ignored.\n", pValue);
        break;
      }

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

    case cWizlistFile:
    case cNoWizlistFile:
        if (cWizlistFile == eOption)
            name_wizlist_file(pValue);
        else /* cNoWizlistFile */
            name_wizlist_file("");
        break;

#ifdef YYDEBUG
    case cYYDebug:
      {
        extern int yydebug;
        yydebug = MY_TRUE;
        break;
      }
#endif


    case cHostname:
        if (hostname != NULL)
            free(hostname);
        hostname = strdup(pValue);
        break;

    case cHostaddr:
        if (hostaddr != NULL)
            free(hostaddr);
        hostaddr = strdup(pValue);
        break;

    case cAccessFile:
        if (access_file != NULL)
            free(access_file);
        if (!strcmp(pValue, "none"))
            access_file = NULL;
        else
            access_file = strdup(pValue);
        break;

    case cAccessLogFile:
        if (access_log != NULL)
            free(access_log);
        if (!strcmp(pValue, "none"))
            access_log = NULL;
        else
            access_log = strdup(pValue);
        break;

    case cMaster:
        if (strlen(pValue) >= sizeof(master_name)) {
            fprintf(stderr, "Too long master name '%s'\n", pValue);
            return hrError;
        }
        strcpy(master_name, pValue);
        break;

    case cMinMalloc:
        min_malloced = strtol(pValue, (char **)0, 0);
        if (min_malloced < 0)
        {
            fprintf(stderr, "Illegal value '%s' for --min-malloc\n", pValue);
            return hrError;
        }
        break;

    case cMinSmallMalloc:
        min_small_malloced = strtol(pValue, (char **)0, 0);
        if (min_small_malloced < 0)
        {
            fprintf(stderr, "Illegal value '%s' for --min-small-malloc\n", pValue);
            return hrError;
        }
        break;

    case cMaxMalloc:
        if (!strcasecmp(pValue, "unlimited"))
        {
            set_memory_limit(MALLOC_HARD_LIMIT, 0);
        }
        else
        {
            if (!set_memory_limit(MALLOC_HARD_LIMIT, strtol(pValue, (char **)0, 0)))
            {
                fprintf(stderr, "Illegal value '%s' for --hard-malloc-limit\n", pValue);
                return hrError;
            }
        }
        break;

    case cSoftMallocLimit:
            if (!strcasecmp(pValue, "unlimited"))
            {
                set_memory_limit(MALLOC_SOFT_LIMIT, 0);
            }
            else
            {
                if (!set_memory_limit(MALLOC_SOFT_LIMIT, strtol(pValue, (char **)0, 0)))
                {
                    fprintf(stderr, "Illegal value '%s' for --soft-malloc-limit\n", pValue);
                    return hrError;
                }
            }
            break;
            
    case cMudlib:
        if (chdir(pValue) == -1) {
            fprintf(stderr, "Bad mudlib directory: %s\n", pValue);
            return hrError;
        }
        new_mudlib = 1;
        break;

    case cDebugFile:
        if (debug_file != NULL)
            free(debug_file);
        debug_file = strdup(pValue);
        break;
        
    case cRandomdevice:
        // sets prng_device_name to some file/device and re-seeds the PRNG
        // from it.
        if (prng_device_name != NULL)
            free(prng_device_name);
        prng_device_name = strdup(pValue);
        seed_random(prng_device_name);
        break;

    case cRandomSeed:
    	// seeds PRG with given value
        seed_random_from_int(strtoul(pValue, (char **)0, 0));
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

    case cStrictEuids:
        strict_euids = MY_TRUE;
        break;

    case cNoStrictEuids:
        strict_euids = MY_FALSE;
        break;

    case cShareVariables:
        share_variables = MY_TRUE;
        break;

    case cNoShareVariables:
        share_variables = MY_FALSE;
        break;

    case cFilenameSpaces:
        allow_filename_spaces = MY_TRUE;
        break;

    case cNoFilenameSpaces:
        allow_filename_spaces = MY_FALSE;
        break;

#ifdef USE_TLS
    case cTLSkey:
        if (tls_keyfile != NULL)
            free(tls_keyfile);
        if (!strcmp(pValue, "none"))
            tls_keyfile = NULL;
        else
            tls_keyfile = strdup(pValue);
        break;

    case cTLScert:
        if (tls_certfile != NULL)
            free(tls_certfile);
        if (!strcmp(pValue, "none"))
            tls_certfile = NULL;
        else
            tls_certfile = strdup(pValue);
        break;

    case cTLStrustdir:
        if (tls_trustdirectory != NULL)
            free(tls_trustdirectory);
        if (!strcmp(pValue, "none"))
            tls_trustdirectory = NULL;
        else
            tls_trustdirectory = strdup(pValue);
        break;

    case cTLStrustfile:
        if (tls_trustfile != NULL)
            free(tls_trustfile);
        if (!strcmp(pValue, "none"))
            tls_trustfile = NULL;
        else
            tls_trustfile = strdup(pValue);
        break;
    case cTLScrlfile:
        if (tls_crlfile != NULL)
           free(tls_crlfile);
        if (!strcmp(pValue, "none"))
            tls_crlfile = NULL;
        else
            tls_crlfile = strdup(pValue);
        break;
    case cTLScrldir:
        if (tls_crldirectory != NULL)
            free(tls_crldirectory);
        if (!strcmp(pValue, "none"))
            tls_crldirectory = NULL;
        else
            tls_crldirectory = strdup(pValue);
        break;
#endif

#ifdef GC_SUPPORT
    case cGcollectFD:
        if (isdigit((unsigned char)*pValue)) {
            default_gcollect_outfd = strtol(pValue, (char **)0, 0);
        } else {
            default_gcollect_outfd = ixopen3(pValue, O_CREAT|O_TRUNC|O_WRONLY, 0640);
        }
        set_cloexec_flag(default_gcollect_outfd);
        gcollect_outfd = default_gcollect_outfd;
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
        return hrError;

    case cLongHelp:
        usage();
        return hrError;

    case cPidFile:
        {
            FILE * pidfile;

            pidfile = fopen(pValue, "w");
            if (!pidfile)
            {
                fprintf(stderr, "Can't open pidfile '%s': %s.\n"
                       , pValue, strerror(errno));
                return hrError;
            }
            fprintf(pidfile, "%ld\n", (long)getpid());
            fclose(pidfile);
            break;
        }

#ifdef DEBUG
    case cCheckRefs:
        check_a_lot_ref_counts_flag = MY_TRUE;
        break;

    case cCheckState:
        {
            int n;
            char * end;

            n = strtol(pValue, &end, 0);
            if (n < 0 || n > 2 || end == NULL || *end != '\0')
            {
                fprintf(stderr, "Bad check-state level: %s\n", pValue);
                return hrError;
            }
            check_state_level = n;
            break;
        }

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

#ifdef CHECK_OBJECT_STAT
    case cCheckObjectStat:
        check_object_stat = MY_TRUE;
        break;
#endif

    case cFuncall:
        /* Store the value in a list for later evaluation */
        {
            FData * fdata;

            fdata = malloc(sizeof(*fdata) + strlen(pValue));
            if (!fdata)
            {
                fprintf(stderr, "Out of memory for '-f %s'.\n", pValue);
                return hrError;
            }

            fdata->next = NULL;
            strcpy(fdata->txt, pValue);

            if (f_tail)
                f_tail->next = fdata;
            f_tail = fdata;
            if (!f_head)
                f_head = fdata;
        }
        break;

    default:
        /* This shouldn't happen. */
        fprintf(stderr, "%s ldmud: (eval_arg) Internal error, eOption is %d\n"
                      , time_stamp(), eOption);
        return hrError;
    } /* switch */

  return hrSuccess;
} /* eval_arg() */

/*-------------------------------------------------------------------------*/
static Bool
open_arg_file (InputSource ** ppInput, const char * pName)

/* Try to open the file <pName> as new input source.
 * If successful, add the source to the list starting at *ppInput and
 * return MY_TRUE.
 * On failure, print an error message and return MY_FALSE.
 */

{
    InputSource * pSrc;
    size_t        size, left;
    FILE        * f;
    struct stat   st;
    char        * pData;

    /* Auxiliary structure to store the found argument words. */
    typedef struct Marker {
        struct Marker * next;
        char          * pArg;
    } Marker;

    Marker * mHead = NULL;
    Marker * mTail = NULL;

    /* If this is a nested include, make sure that we don't get caught
     * in a recursion.
     */
    for (pSrc = *ppInput; pSrc && pSrc->name != NULL; pSrc = pSrc->next)
    {
        if (!strcmp(pSrc->name, pName))
        {
            fprintf(stderr
                   , "ldmud: Recursion in nested argument files: %s\n"
                   , pName);
            for (pSrc = *ppInput; pSrc && pSrc->name != NULL; pSrc = pSrc->next)
                fprintf(stderr, "          included by %s\n", pSrc->name);
            return MY_FALSE;
        }
    }

    /* If the file would be opened in text mode, the size from fstat would
     * not match the number of characters that we can read.
     */
    f = fopen(pName, "rb");
    if (f == NULL)
    {
        int err = errno;
        fprintf( stderr
               , "ldmud: Can't open argument file '%s' for reading: (%d) %s\n"
               , pName, err, strerror(err)
               );
        return MY_FALSE;
    }

    /* Check if the file is small enough to be read. */

    if (fstat(fileno(f), &st) == -1)
    {
        int err = errno;
        fprintf( stderr
               , "ldmud: Can't stat argument file '%s': (%d) %s\n"
               , pName, err, strerror(err)
               );
        fclose(f);
        return MY_FALSE;
    }

    size = (size_t)st.st_size;

    /* Get a new input source structure and read in the file. */
    {
        size_t len = strlen(pName);

        pSrc = malloc(sizeof(*pSrc) + len + 1 + size);
        if (pSrc == NULL)
        {
            fprintf( stderr
                   , "ldmud: Out of memory reading argument file '%s'\n"
                   , pName
                   );
            fclose(f);
            return MY_FALSE;
        }

        pSrc->arg = 0;
        pSrc->argc = 0;
        pSrc->argv = NULL;
        pSrc->next = NULL;

        strcpy(pSrc->data, pName);
        pSrc->data[len] = '\0';
        pSrc->name = pSrc->data;

        pData = &(pSrc->data[len+1]);

        if (1 != fread(pData, size, 1, f))
        {
            int err = errno;
            fprintf( stderr
                   , "ldmud: Error reading argument file '%s': (%d) %s\n"
                   , pName, err, strerror(err)
                   );
            free(pSrc);
            fclose(f);
            return MY_FALSE;
        }

        /* Ensure a terminating 0 */
        pData[size] = '\0';
    }

    fclose(f);

    /* Now scan the read data and search for words.
     * Store the found words in the Marker list, and insert the
     * \0 terminators.
     */

    for (left = 0; left < size && *pData != '\0'; left++, pData++)
    {
        Marker *pMarker;
        Bool    endFound, quoted;

        unsigned char c = (unsigned char)*pData;

        if (isascii(c) && (isspace(c) || c == '\r' || c == '\n'))
            continue;

        /* Found a non-space. If it is a '#', it is a comment - skip it. */
        if (c == '#')
        {
            for ( left++, pData++
                ; left < size && *pData != '\0'
                ; left++, pData++)
                if (*pData == '\r' || *pData == '\n')
                    break;
            /* pData now points to the lineend character, or to the end
             * of the file. Either way, continuing the outer loop will
             * do the right thing.
             */
            continue;
        }

        /* It is a true new word. Store it's starting position in
         * a new marker. Oh, and count it.
         */
        pSrc->argc++;

        pMarker = alloca(sizeof(*pMarker));
        if (pMarker == NULL)
        {
            fprintf( stderr
                   , "ldmud: Out of memory reading argument file '%s'\n"
                   , pName
                   );
            free(pSrc);
            return MY_FALSE;
        }

        pMarker->pArg = pData;
        pMarker->next = NULL;

        if (mTail)
            mTail->next = pMarker;
        mTail = pMarker;
        if (!mHead)
            mHead = pMarker;

        /* Now search for the end of the word.
         * Look at the first character again in case it's a quote.
         */
        for ( endFound = MY_FALSE, quoted = MY_FALSE
            ; left < size && *pData != '\0'
            ; left++, pData++
            )
        {
            c = (unsigned char)*pData;

            /* Line end always terminates the search */
            if (c == '\r' || c == '\n')
            {
                if (quoted)
                {
                    fprintf( stderr
                           , "ldmud: Error in argument file '%s': "
                             "Quoted string spans more than one line.\n"
                           , pName
                           );
                    free(pSrc);
                    return MY_FALSE;
                }
                endFound = MY_TRUE;
                break;
            }

            /* Space outside of a quoted string also terminates */
            if (!quoted && isascii(c) && isspace(c))
            {
                endFound = MY_TRUE;
                break;
            }

            /* If it's a '"', toggle the quoted flag. */
            if (c == '"')
                quoted = !quoted;

            /* Anyway, it's not a space here, so continue the search */
        }

        /* One possible error: end of file in a quoted string */
        if (!endFound && quoted)
        {
            fprintf( stderr
                   , "ldmud: Error in argument file '%s': "
                     "Unexpected end of file in quoted string.\n"
                   , pName
                   );
            free(pSrc);
            return MY_FALSE;
        }

        /* pData now points to the first character after the found word
         * It is a space (or the terminating '\0' at the end of the file),
         * so we can put the '\0' for the argument word in it.
         */
        *pData = '\0';
    }

    /* We found all the arguments. Now setup an argv array. */

    if (pSrc->argc == 0)
    {
        fprintf( stderr
               , "ldmud: Warning: Argument file '%s' contained no data.\n"
               , pName);
        /* We will setup an empty InputSource - no need to abort argument
         * parsing because of this.
         */
    }

    {
        int i;

        pSrc->argv = malloc(sizeof(pSrc->argv[0]) * (pSrc->argc+1));
        if (pSrc->argv == NULL)
        {
            fprintf( stderr
                   , "ldmud: Out of memory reading argument file '%s'\n"
                   , pName
                   );
            free(pSrc);
            return MY_FALSE;
        }

        for (i = 0; i < pSrc->argc; i++, mHead = mHead->next)
        {
            pSrc->argv[i] = mHead->pArg;
        }

        pSrc->argv[pSrc->argc] = NULL; /* Just in case */

    }

    /* Link the now complete InputSource structure into the list */
    pSrc->next = *ppInput;
    *ppInput = pSrc;

    return MY_TRUE;
} /* open_arg_file() */

/*-------------------------------------------------------------------------*/
static INLINE void
free_sources (InputSource * pInput)

/* <pInput> points at the head of a list of input sources.
 * Deallocate all of them.
 */

{
    while (pInput != NULL)
    {
        InputSource * pSrc = pInput;

        pInput = pInput->next;

        if (pSrc->name != NULL && pSrc->argv)
            /* Not the commandline */
            free(pSrc->argv);

        free(pSrc);
    }
} /* free_sources() */

/*-------------------------------------------------------------------------*/
static int
getargs (int argc, char ** argv, int (*opt_eval)(int, const char *) )

/* Get the arguments from the commandline and pass them
 * as (number, optional value) to the opt_eval callback.
 * If opt_eval() returns hrError, argument scanning is terminated.
 * In that case, or if getargs() detects an error itself, getargs() returns
 * non-zero.
 * A zero return means 'success' in both cases.
 */

{
  int           i;        /* all purpose */
  int           iArg;     /* Number of argument under inspection */
  OptNumber     eOption;  /* The current recognized option */
  int           iOption;  /* The index of the recognized option */
  short         bCont;    /* True: find another option in the same argument */
  short         bDone;    /* True: all options parsed, only args left */
  short         bShort;   /* True: current argument is a short option */
  char        * pArg;     /* Next argument character to consider */
  InputSource * pInput;   /* Head of list of input sources */

  /* Make the compiler happy */
  bShort = MY_FALSE;
  pArg = NULL;

  /* Set up the basic input source with the commandline arguments */
  pInput = malloc(sizeof(*pInput));
  if (!pInput)
  {
      fputs("ldmud: Out of memory.\n", stderr);
      return 1;
  }
  pInput->argc = argc-1;
  pInput->argv = argv+1;
  pInput->arg = 0;
  pInput->name = NULL;
  pInput->next = NULL;

  /* Scan arguments from all input sources.
   */
  while (pInput != NULL)
  {
      /* Scan all arguments */
      bCont = MY_FALSE;
      bDone = MY_FALSE;
      for (iArg = pInput->arg; iArg < pInput->argc; !bCont ? iArg++ : iArg)
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
          pArg = pInput->argv[iArg];
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
              iArglen = (size_t)(pValue - pArg);
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
            for (iOption = 0; (size_t)iOption < sizeof(aOptions) / sizeof(aOptions[0]); iOption++)
            {
              if (*pArg == aOptions[iOption].cOption)
              {
                eOption = (OptNumber)aOptions[iOption].eNumber;
                bTakesValue = aOptions[iOption].bValue;
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
            for (iOption = 0; (size_t)iOption < sizeof(aOptions) / sizeof(aOptions[0]); iOption++)
              if (aOptions[iOption].pOption != NULL
               && iArglen == strlen(aOptions[iOption].pOption)
               && !strncasecmp(pArg, aOptions[iOption].pOption, iArglen))
              {
                eOption = (OptNumber)aOptions[iOption].eNumber;
                bTakesValue = aOptions[iOption].bValue;
                break;
              }
          }

          if (cUnknown == eOption)
          {
            if (pInput->name != NULL)
                fprintf(stderr, "ldmud: (%s) Unknown option '", pInput->name);
            else
                fputs("ldmud: Unknown option '", stderr);
            if (bShort)
              fprintf(stderr, "-%c", *pArg);
            else
              fprintf(stderr, "--%*.*s", (int)iArglen, (int)iArglen, pArg);
            fputs("'.\n", stderr);
            free_sources(pInput);
            return 1;
          }

          /* If at this point bTakesValue is true, but pValue is still NULL,
           * then the value is in the next argument. Get it if it's there.
           */
          if (bTakesValue && pValue == NULL && iArg + 1 < pInput->argc)
          {
            iArg++;
            pValue = pInput->argv[iArg];
          }

          /* Signal an error if pValue is still NULL or if it's empty. */
          if (bTakesValue && (pValue == NULL || !strlen(pValue)))
          {
            fputs("ldmud: Option '", stderr);
            if (bShort)
              putc((unsigned char)(aOptions[iOption].cOption), stderr);
            else
              fputs(aOptions[iOption].pOption, stderr);
            fputs("' expects a value.\n", stderr);
            free_sources(pInput);
            return 1;
          }

        } /* if (unknown option) */

        /* Before evaluation of the parsed option, determine 'bCont' */
        bCont = bShort && (iArglen > 0) && !bTakesValue;

        /* --- The option evaluation --- */

        i = (*opt_eval)(eOption, pValue);
        if (i == hrError)
        {
          free_sources(pInput);
          return 1;
        }

        if (i == hrArgFile)
        {
            pInput->arg = iArg+1;
            if (!open_arg_file(&pInput, pValue))
                return 1;

            /* We have a new input source here, so reset the loop
             * parameters.
             */
            iArg = -1;
            bCont = MY_FALSE;
            bDone = MY_FALSE;
        }

      } /* for (iArg) */

      /* We are done with this input source - free it */
      {
          InputSource * pSrc = pInput;

          pInput = pInput->next;
          pSrc->next = NULL;
          free_sources(pSrc);
      }
  } /* while(pInput) */

  return 0;
} /* getargs() */

/***************************************************************************/
