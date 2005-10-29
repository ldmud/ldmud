/*
 * Set MUD_LIB to the directory which contains the mud data.
 */
#define MUD_LIB			"/usr/users/jseidman/lpmud/mudlib"

/*
 * This is the file which contains names of objects to be
 * loaded initially. Do not normally change this.
 */
#define INIT_FILE		"room/init_file"

/*
 * This is the subdirectory where all wizards objects are defined.
 */
#define PLAYER_DIR		"players"

/*
 * This is the castle that a wiz gets a copy of.
 */
#define DEFAULT_CASTLE		"room/def_castle.c"

/*
 * A command to run the C preprocessor on a C file (or really, an
 * object definition file).
 */
#define PRE_COMPILE	"/lib/cpp -I/usr/users/jseidman/lpmud/mudlib/room -I/usr/users/jseidman/lpmud/mudlib/obj"

/*
 * Max number of local variables in a function.
 */
#define MAX_LOCAL	20	

/*
 * A command that lists files in a directory.
 * Note that we need the path since vpopen doesn't glob.
 */
#define LIST_FILES	"/bin/ls -C -F"

/*
 * Define what ioctl to use against tty's.
 */

#define USE_TIOCGETP	/* BSD */
/* #define USE_TCGETA	/* SYSV */

/* Maximum number of evaluated nodes/loop.
 * If this is exceeded, current function is halted.
 * The worst case yet encountered is over 9000.
 */
#define MAX_COST	(5 * 5000)

/* Use fork or vfork to exec a program.
 * Default is fork().
 */

#define USE_VFORK

/*
 * The following define is used when debuging the memory
 * allocation.
 */

/* #define free(x) xfree(x) */

/*
 * Objects are swapped out if they are not used.
 * Specify how many resets the object can stay non-referenced until
 * it is swapped out.
 * A value if '1' will make swap out the object at every reset.
 * A value of '0' will never swap out the object.
 * The count will be cleared by either call_other() or move_object().
 */

#define NUM_RESET_TO_SWAP	0

/*
 * Where to swap out objects. This directory is not used if NUM_RESET_TO_SWAP
 * is 0.
 */
#define SWAP_DIR		"/tmp"

/*
 * Which port to use on this host.  The #ifndef is so that we can define
 * a different port with a -D option to the compiler.
 */
#ifndef PORTNUM
#define PORTNUM			2000
#endif

/*
 * Define this if we want to abort (crash) the program in case of some
 * flaky (but possibly non-fatal) conditions.
 */
/* #define DO_ABORT */
