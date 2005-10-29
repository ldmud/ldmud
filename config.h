/*
 * Define this if you want the MUDWHO-service. You have to configure it
 * in mudwho.h.
 */
/* #define MUDWHO */

/*
 * Define this if you want to use the improved access restriction system.
 * Look at the file ACCESS.ALLOW for information. It replaces the old
 * ACCESS.DENY.
 */
#define ACCESS_RESTRICTED

/*
 * Define the maximum size of log files (in bytes).
 */
#define MAX_LOG_SIZE		50000
/*
 * Max size of a file allowed to be read by 'read_file()'.
 */
#define READ_FILE_MAX_SIZE	50000

/* Version of the game in the form xx.xx.xx (leading zeroes) gc.
 * Two digits will be appended, that is the patch level.
 */
#define GAME_VERSION "03.01."

/*
 * If an object is left alone for a certain time, then the
 * function clean_up will be called. This function can do anything,
 * like destructing the object. If the function isn't defined by the
 * object, then nothing will happen.
 *
 * This time should be substantially longer than the swapping time.
 */
#define TIME_TO_CLEAN_UP	7200

/*
 * How long time until an unused object is swapped out.
 * Machine with too many players and too little memory: 900 (15 minutes)
 * Machine with few players and lot of memory: 10000
 * Machine with infinite memory: 0 (never swap).
 */
#define TIME_TO_SWAP	900

/*
 * How many seconds until an object is reset again.
 * Set this value high if big system, otherwise low.
 * No castles:	 1800	(30 minutes)
 * >100 castles:10000	(almost 3 hours).
 */
#define TIME_TO_RESET	3600	/* one hour */

/*
 * How to extract an unsigned char from a char *.
 * If your compiler has the type 'unsigned char', then the cast-version
 * is best. If you do not know, use the simple version, and the game will
 * immediately terminate with a message if bad.
#define EXTRACT_UCHAR(p) (*p < 0 ? *p + 0x100 : *p)
 */
#define EXTRACT_UCHAR(p) (*(unsigned char *)p)

/*
 * Define the maximum stack size of the stack machine. This stack will also
 * contain all local variables and arguments.
 */
#define EVALUATOR_STACK_SIZE	1000

/*
 * Define the maximum call depth for functions.
 */
#define MAX_TRACE		30

/*
 * Define the size of the compiler stack. This defines how complex
 * expressions the compiler can parse. The value should be big enough.
 */
#define COMPILER_STACK_SIZE	200

/*
 * What is the value of the first constant defined by yacc ? If you do not
 * know, compile, and look at y.tab.h.
 */
#define F_OFFSET		257

/*
 * Does the system have a getrusage call?
 */
/* #define RUSAGE */

/*
 * Maximum number of bits in a bit field. They are stored in printable
 * strings, 6 bits per byte.
 */
#define MAX_BITS		1200	/* 200 bytes */

/*
 * There is a hash table for living objects, used by find_living().
 */
#define LIVING_HASH_SIZE	100

/*
 * Define what port number the game is to use.
 */
#define PORTNO			3000

/*
 * This is the file which contains names of objects to be
 * loaded initially. Do not normally change this.
 */
#define INIT_FILE		"room/init_file"

/*
 * This is the subdirectory where all wizards objects are defined.
 */
#define PLAYER_DIR		"w"

/*
 * If you want to put wizards in groups and give each group a subdirectory
 * you should define this. Domain directorys must start witha capital
 * letter, and wizards names must not.
 */
#define DOMAINS

/*
 * This is the subdirectory where all the domain directorys are placed.
 */
#define DOMAIN_DIR              "d"

/*
 * Undef this if your system doesn't support Berkley type symlinks.
 */
#define SYMLINKS

/*
 * This is the castle that a wiz gets a copy of.
 */
#define DEFAULT_CASTLE		"d/Standard/start/def_castle.c"

/*
 * This is the workroom that a wiz gets a copy of.
 */
#define DEFAULT_WORKROOM	"d/Standard/start/def_workroom.c"

/*
 * This is the room where the new castle should reside. If it is not
 * defined, then it will be put in the same room as the player.
 */
#define CASTLE_ROOM		"room/new_castles"

/*
 * Max number of local variables in a function.
 */
#define MAX_LOCAL	20	

/*
 * Define what ioctl to use against tty's.
 */

#define USE_TIOCGETP		/* BSD */
/* #define USE_TCGETA */ 	/* SYSV */

/* Maximum number of evaluated nodes/loop.
 * If this is exceeded, current function is halted.
 * The worst case yet encountered is 3600	(dec 1989)
 */
#define MAX_COST	100000

/*
 * Where to swap out objects. This file is not used if NUM_RESET_TO_SWAP
 * is 0.
 */
#define SWAP_FILE		"LP_SWAP.3"

/*
 * This is the maximum array size allowed for one single array.
 */
#define MAX_ARRAY_SIZE 1000

/*
 * Some LPmuds on sun4 and sparcstations have had problems with the
 * call of inet_ntoa() in comm1.c.
 * If the game crash in query_ip_number() wen using inet_ntoa(),
 * then undefine the following symbol.
 * The query_ip_number() is called when doing the 'people' command
 * for example.
 */
/* #define INET_NTOA_OK */

/*
 * Define LOG_SHOUT if you want all shouts to be logged in
 * mudlib/log/SHOUTS.
 */
#define LOG_SHOUT

/*
 * Maximum number of players in the game.
 */
#define MAX_PLAYERS	40

/*
 * When uploading files, we want fast response; however, normal players
 * shouldn't be able to hog the system in this way.  Define ALLOWED_ED_CMDS
 * to be the ratio of the no of ed cmds executed per player cmd, and
 * MAX_CMDS_PER_BEAT to be the bax no of buffered player commands the
 * system will accept in each heartbeat interval.
 */

#define	ALLOWED_ED_CMDS		20
#define	MAX_CMDS_PER_BEAT	5 /* not implemented yet :-( */

/*
 * Reserve an extra memory area from malloc(), to free when we run out
 * of memory to get some warning and start Armageddon.
 * If this value is 0, no area will be reserved.
 */
#define RESERVED_SIZE		800000

/* Define the size of the shared string hash table.  This number should
 * a prime, probably between 1000 and 30000; if you set it to about 1/5
 * of the number of distinct strings you have, you will get a hit ratio
 * (number of comparisons to find a string) very close to 1, as found strings
 * are automatically moved to the head of a hash chain.  You will never
 * need more, and you will still get good results with a smaller table.
 * THIS IS NOT IMPLEMENTED YET.
 */

#define	HTABLE_SIZE	2203	/* there is a table of some primes too */

/*
 * Object hash table size.
 * Define this like you did with the strings; probably set to about 1/4 of
 * the number of objects in a game, as the distribution of accesses to
 * objects is somewhat more uniform than that of strings.
 */

#define OTABLE_SIZE	1009	/* we have several thousand obs usually */

/*
 * Define SYSV if you are running system V with a lower release level than
 * Sys V.4.
 */

#undef SYSV

/*
 * Define FCHMOD_MISSING only if your system doesn't have fchmod().
 */

#undef FCHMOD_MISSING

/*
 * Define COMPAT_MODE if you are using mudlib 2.4.6 or older. This
 * replaces the old command line option -o.
 */

#undef COMPAT_MODE

/*
 * Define MAX_BYTE_TRANSFER to the number of bytes you allow to be read
 * and written with read_bytes and write_bytes
 */

#define MAX_BYTE_TRANSFER 10000
   
/************************************************************************/
/*	END OF CONFIG -- DO NOT ALTER ANYTHING BELOW THIS LINE		*/
/************************************************************************/

/*
 * some generic large primes used by various hash functions in different files
 * You can alter these if you know of a better set of numbers!  Be sure
 * they are primes...
 */

#define	P1		701	/* 3 large, different primes */
#define	P2		14009	/* There's a file of them here somewhere :-) */
#define	P3		54001
