#ifndef CONFIG_H
#define CONFIG_H

#include "hosts/os2/os2config.h"

/* #define ACCESS_CONTROL if you want the driver to do any access control.
 */
#define ACCESS_CONTROL

/* Some configurations for this system. Needn't be changed if you don't use
 * the new access restriction system.
 */

/* file for access permissions data */
#define ACCESS_FILE "ACCESS.ALLOW"

/* logfile to show valid and rejected connections
 * simple not define this for NO logs
 */
/* #define ACCESS_LOG "access.allow.log" */


/*
 * Define the maximum size of log files (in bytes).
 */
#define MAX_LOG_SIZE		50000
/*
 * Max size of a file allowed to be read by 'read_file()'.
 */
#define READ_FILE_MAX_SIZE	50000

/*
 * If an object is left alone for a certain time, then the
 * function clean_up will be called. This function can do anything,
 * like destructing the object. If the function isn't defined by the
 * object, then nothing will happen.
 *
 * This time should be substantially longer than the swapping time.
 */
#define TIME_TO_CLEAN_UP	5400

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

#define RESET_GRANULARITY 900	/* 15 minutes */

/*
 * Define the maximum stack size of the stack machine. This stack will also
 * contain all local variables and arguments.
 */
#define EVALUATOR_STACK_SIZE	1000

/*
 * Define the maximum call depth for functions.
 * MAX_USER_TRACE is used for for normal program execution, the full
 * MAX_TRACE is only available in error handling.
 */
#define MAX_USER_TRACE		60
#define MAX_TRACE		65

/*
 * Define the size of the compiler stack. This defines how complex
 * expressions the compiler can parse. The value should be big enough.
 */
#define COMPILER_STACK_SIZE	200

/*
 * Maximum number of bits in a bit field. They are stored in printable
 * strings, 6 bits per byte.
 * The limit is more based on considerations of speed than memory
 * consumption.
 */
#define MAX_BITS		6144	/* 1 KByte */

/*
 * There is a hash table for living objects, used by find_living().
 */
#define LIVING_HASH_SIZE	128

/*
 * Define what port number the game is to use.
 */
#define PORTNO			7680

/*
 * This is the file which contains names of objects to be
 * loaded initially. Do not normally change this.
 */
#define INIT_FILE		"room/init_file"

/*
 * Max number of local variables in a function.
 */
#define MAX_LOCAL	20	

/* Maximum number of evaluated nodes/loop.
 * If this is exceeded, the current function is halted.
 * ls() can take about 30000 for large directories.
 */
#define MAX_COST	100000
/* to catch an eval_cost too big error in an object that called recursive
 * master functions, CATCH_RESERVED_COST should be greater than
 * MASTER_RESERVED_COST * 2.
 */
#define CATCH_RESERVED_COST 2000
#define MASTER_RESERVED_COST 0x200 /* must be power of 2 */

/*
 * Where to swap out objects. This file is not used if TIME_TO_SWAP is 0.
 * If the mudlib is mounted via nfs but your /tmp isn't, and isn't purged
 * periodically either, it's a good idea to place the swap file there.
 * The hostname will be appended to the filename defined here.
 */
#define SWAP_FILE		"LP_SWAP.3"

/*
 * This is the maximum array size allowed for one single array.
 */
#define MAX_ARRAY_SIZE 3000

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
 * MAX_CMDS_PER_BEAT to be the max no of buffered player commands the
 * system will accept in each heartbeat interval.
 */

#define	ALLOWED_ED_CMDS		20
#define	MAX_CMDS_PER_BEAT	5 /* not implemented yet :-( */

/*
 * Reserve an extra memory area from malloc(), to free when we run out
 * of memory to get some warning and start Armageddon.
 * If this value is 0, no area will be reserved.
 */
#define RESERVED_USER_SIZE	800000
#define RESERVED_MASTER_SIZE	 50000
#define RESERVED_SYSTEM_SIZE	100000

/* Define the size of the shared string hash table.  This number needn't
 * be prime, probably between 1000 and 30000; if you set it to about 1/5
 * of the number of distinct strings you have, you will get a hit ratio
 * (number of comparisons to find a string) very close to 1, as found strings
 * are automatically moved to the head of a hash chain.  You will never
 * need more, and you will still get good results with a smaller table.
 * If the size is a power of two, hashing will be faster.
 */

#define	HTABLE_SIZE	4096

/* Define the size of the table of defines, reserved words, identifiers
 * and efun names. Should be either several times smaller than HTABLE_SIZE
 * or identical with it.
 */
#define ITABLE_SIZE  256    /* 256 is probably fastest */

/*
 * Object hash table size.
 * Define this like you did with the strings; probably set to about 1/4 of
 * the number of objects in a game, as the distribution of accesses to
 * objects is somewhat more uniform than that of strings.
 */

#define OTABLE_SIZE	1024

#define APPLY_CACHE_BITS 10

/*
 * Define COMPAT_MODE if you are using mudlib 2.4.6 or older. This
 * replaces the old command line option -o.
 */

#define COMPAT_MODE
#undef NATIVE_MODE

/* Define OLD_PREVIOUS_OBJECT_BEHAVIOUR if the new behaviour gives problems
 * in your security system.
 */
#undef OLD_PREVIOUS_OBJECT_BEHAVIOUR

/* Define OLD_EXPLODE_BEHAVIOUR when you want to have explode geared towards a
 * subroutine of parse_command
 */
#undef OLD_EXPLODE_BEHAVIOUR

/* Define SUPPLY_PARSE_COMMAND if you want the efun parse_command.
 * If you don't need it, better #undef it, lest some new wiz can inadvertly
 * crash your mud or make it leak memory.
 */
#define SUPPLY_PARSE_COMMAND

/* Define INITIALIZATION_BY___INIT if you want all initializations of variables
 * to be suspended till the object is created ( as supposed to initialization
 * at compile time; the latter is more memory efficient for loading and faster
 * at cloning, while the former allows to use efuns, e.g. shutdown().
 */

#undef INITIALIZATION_BY___INIT

/* Define MASTER_NAME if you want something different from "obj/master" resp.
 * "secure/master" as default.
 */
/* #define MASTER_NAME "kernel/master" */
 
/*
 * Define MAX_BYTE_TRANSFER to the number of bytes you allow to be read
 * and written with read_bytes and write_bytes
 */

#define MAX_BYTE_TRANSFER 50000

/* Define FLOATS if you want code for the floating-point type
 */

#define FLOATS
#define TRANSCENDENT_FUNCTIONS

/* Define MAPPINGS if you want a mappings */

#define MAPPINGS
   
/*
 * CATCH_UDP_PORT
 *
 * Define this if the mud are to catch incoming udp messages on a
 * specific port. If == -1 it will not be used unless the mud is started
 * with the -u### flag. Where ### is the portnumber for the udp port.
 * If undefined the -u flag will be ignored.
 */
#undef CATCH_UDP_PORT	4246
#undef UDP_SEND

#define COMM_STAT
#define APPLY_CACHE_STAT
#define FILE_STAT

/* When smalloc is used without SBRK_OK, MIN_MALLOCED will lower large block
 * fragmentation. The value should be a multiple of the large chunk size.
 */
/* #define MIN_MALLOCED	   0x1000000 */
#define MAX_MALLOCED	   0x4000000
#define MAX_SMALL_MALLOCED 0x1000000

/* Define this to annotate all allocations with file:line of the driver
 * source responsible for it.
 */
#undef SMALLOC_TRACE

/* Define this to annotate all allocations with file:line of the lpc program
 * responsible for it.
 */
#undef SMALLOC_LPC_TRACE

/* If using TRACE_CODE , how many instructions should be kept? */
#define TOTAL_TRACE_LENGTH 0x1000


#endif /* CONFIG_H */
