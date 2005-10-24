/* DO NOT EDIT!
 *
 * This file is created automatically by configure from the template
 * in config.h.in.
 */

#ifndef CONFIG_H__
#define CONFIG_H__ 1

/* Should code for the external request demon be included?
 */
#define ERQ_DEMON

/* Maximum sizes for an erq send or reply.
 */
#define ERQ_MAX_REPLY 1024
#define ERQ_MAX_SEND  1024

/* #define ACCESS_CONTROL if you want the driver to do any access control.
 * TODO: Make this a runtime option.
 */
#define ACCESS_CONTROL

/* file for access permissions data */
/* TODO: Make this a runtime option */
#define ACCESS_FILE               "ACCESS.ALLOW"

/* logfile to show valid and rejected connections
 * simple not define this for NO logs
 * TODO: Make this a runtime option.
 */
#undef ACCESS_LOG 

/*
 * Max size of a file allowed to be read by 'read_file()'.
 */
#define READ_FILE_MAX_SIZE        50000

/* If an object is left alone for a certain time, then the
 * function clean_up will be called. This function can do anything,
 * like destructing the object. If the function isn't defined by the
 * object, then nothing will happen.
 *
 * This time should be substantially longer than the swapping time.
 */
#define TIME_TO_CLEAN_UP          10800

/*
 * How long time until an unused object is swapped out.
 * Machine with too many players and too little memory: 900 (15 minutes)
 * Machine with few players and lot of memory: 10000
 * Machine with infinite memory: 0 (never swap).
 */
#define TIME_TO_SWAP              900
#define TIME_TO_SWAP_VARIABLES    2000

/*
 * How many seconds until an object is reset again.
 * Set this value high if big system, otherwise low.
 * No castles:         1800        (30 minutes)
 * >100 castles:10000        (almost 3 hours).
 */
#define TIME_TO_RESET             2700

/*
 * Define the maximum stack size of the stack machine. This stack will also
 * contain all local variables and arguments.
 */
#define EVALUATOR_STACK_SIZE      2000

/*
 * Define the maximum call depth for functions.
 * MAX_USER_TRACE is used for for normal program execution, the full
 * MAX_TRACE is only available in error handling.
 */
#define MAX_USER_TRACE              90
#define MAX_TRACE                   100

/*
 * Define the size of the compiler stack. This defines how complex
 * expressions the compiler can parse. The value should be big enough.
 */
#define COMPILER_STACK_SIZE        400

/*
 * Maximum number of bits in a bit field. They are stored in printable
 * strings, 6 bits per byte.
 * The limit is more based on considerations of speed than memory
 * consumption.
 */
#define MAX_BITS                  6144

/*
 * Define what port number the game is to use.
 */
#define PORTNO                    7680

/* Maximum numbers of ports the GD accepts connections to.
 */
#define MAXNUMPORTS                 20

/*
 * Max number of local variables in a function.
 */
#define MAX_LOCAL                   50

/* Maximum number of evaluated nodes/loop.
 * If this is exceeded, the current function is halted.
 * ls() can take about 30000 for large directories.
 */
#define MAX_COST               1000000

/* CATCH_RESERVED_COST is added to the eval cost for the time executing code
 * guarded by a catch() statement, so that an eval_cost-too-big error can
 * still be caught and handled.
 *
 * To catch an eval_cost too big error in an object that called recursive
 * master functions, CATCH_RESERVED_COST should be greater than
 * MASTER_RESERVED_COST * 2.
 * TODO: Check that at runtime.
 */
#define CATCH_RESERVED_COST       20000

/* MASTER_RESERVED_COST is the total reserve available for master applies.
 * It is halved for every recursion into another master apply.
 */
#define MASTER_RESERVED_COST    0x2000 /* must be power of 2 */
/* TODO: Check that at runtime */

/*
 * Where to swap out objects. This file is not used if TIME_TO_SWAP is 0.
 * If the mudlib is mounted via nfs but your /tmp isn't, and isn't purged
 * periodically either, it's a good idea to place the swap file there.
 * The hostname will be appended to the filename defined here.
 */
#define SWAP_FILE                 "LP_SWAP.3"

/* Where to save the WIZLIST information.
 * If not defined, and neither given on the commandline, the driver will
 * not read the WIZLIST file.
 */
#define WIZLIST_FILE              "WIZLIST"

/* This is the maximum array size allowed for one single array.
 * If 0, any size is allowed.
 */
#define MAX_ARRAY_SIZE            0

/* This is the maximum array size allowed for one single mapping.
 * If 0, any size is allowed.
 */
#define MAX_MAPPING_SIZE            0

/* This is the maximum number of callouts allowed at one time.
 * If 0, any number is allowed.
 */
#define MAX_CALLOUTS              0

/*
 * If this is defined, expensive operations like string additions
 * receive additional evalcosts depending on the amount of data handled.
 */

#undef DYNAMIC_COSTS

/*
 * Maximum number of players in the game.
 */
#define MAX_PLAYERS                70

/*
 * When uploading files, we want fast response; however, normal players
 * shouldn't be able to hog the system in this way.  Define ALLOWED_ED_CMDS
 * to be the ratio of the no of ed cmds executed per player cmd, and
 * MAX_CMDS_PER_BEAT to be the max no of buffered player commands the
 * system will accept in each heartbeat interval.
 */

#define ALLOWED_ED_CMDS           20
/* TODO: ALLOWED_ED_CMDS: make this a runtime option */

#define MAX_CMDS_PER_BEAT            5 /* TODO: not implemented yet :-( */

/*
 * Reserve an extra memory area from malloc(), to free when we run out
 * of memory to get some warning and start Armageddon.
 * If this value is 0, no area will be reserved.
 */
#define RESERVED_USER_SIZE      800000
#define RESERVED_MASTER_SIZE    100000
#define RESERVED_SYSTEM_SIZE    200000

/* Define the size of the shared string hash table.  This number needn't
 * be prime, probably between 1000 and 30000; if you set it to about 1/5
 * of the number of distinct strings you have, you will get a hit ratio
 * (number of comparisons to find a string) very close to 1, as found strings
 * are automatically moved to the head of a hash chain.  You will never
 * need more, and you will still get good results with a smaller table.
 * If the size is a power of two, hashing will be faster.
 */

#define HTABLE_SIZE               16384

/* Define the size of the table of defines, reserved words, identifiers
 * and efun names. Should be either several times smaller than HTABLE_SIZE
 * or identical with it.
 */
#define ITABLE_SIZE               256

/* Object hash table size.
 * Define this like you did with the strings; probably set to about 1/4 of
 * the number of objects in a game, as the distribution of accesses to
 * objects is somewhat more uniform than that of strings.
 */

#define OTABLE_SIZE               1024

/* Maximum size of an expanded preprocessor definition.
 */

#define DEFMAX                   65000

/* the number of apply_low cache entries will be 2^APPLY_CACHE_BITS.
 */
#define APPLY_CACHE_BITS            12

/* The parameters of the regular expression/result cache.
 * The expression cache uses a hashtable of RXCACHE_TABLE entries.
 * Undefine RXCACHE_TABLE to disable the all regexp caching.
 */

#define RXCACHE_TABLE            8192

/* Define COMPAT_MODE if you are using the 2.4.5 mudlib or one of its
 * derivatives.
 * TODO: Make this a runtime option.
 */
#define COMPAT_MODE

/* Define STRICT_EUIDS if the driver is to enforce the use of euids,
 * ie. load_object() and clone_object() require the current object to
 * have a non-zero euid.
 */
#undef STRICT_EUIDS

/* Define USE_PARSE_COMMAND if you want the efun parse_command().
 */
#define USE_PARSE_COMMAND

/* Define USE_SET_LIGHT if you want the efun set_light() and the
 * simple 2.4.5 light system.
 */
#define USE_SET_LIGHT

/* Define USE_SET_IS_WIZARD if you want the efun set_is_wizard().
 */
#define USE_SET_IS_WIZARD

/* Define USE_PROCESS_STRING if you want the efun process_string().
 */
#define USE_PROCESS_STRING

/* Define INITIALIZATION_BY___INIT if you want all initializations of variables
 * to be suspended till the object is created ( as supposed to initialization
 * at compile time; the latter is more memory efficient for loading and faster
 * at cloning, while the former allows to use efuns, e.g. shutdown().
 */
#undef INITIALIZATION_BY___INIT

/* Define USE_SYSTEM_CRYPT if you want crypt() to be implemented by your
 * operating system (assuming it offers this function). This makes your
 * programm smaller and may even let you take advantage of improvements
 * of your OS, but may also prohibit transporting encrypted date like
 * passwords between different systems.
 * Undefine USE_SYSTEM_CRYPT if you want to use the driver's portable
 * crypt() implementation.
 */
#define USE_SYSTEM_CRYPT

/* The name of the master object.
 */
#define MASTER_NAME              "kernel/master"

/* The input escape character.
 */
#define INPUT_ESCAPE             "!"

/*
 * Define MAX_BYTE_TRANSFER to the number of bytes you allow to be read
 * and written with read_bytes and write_bytes
 */

#define MAX_BYTE_TRANSFER        50000

/* Define this if the mud are to catch incoming udp messages on a
 * specific port. If == -1 it will not be used unless the mud is started
 * with the -u### flag. Where ### is the portnumber for the udp port.
 * If undefined the -u flag will be ignored.
 */
#define CATCH_UDP_PORT            7683
#define UDP_SEND

/* Define this if you want IPv6 support (assuming that your host
 * actually offers this.
 */
#undef USE_IPV6

/* Define this if you want alist support.
 */
#undef USE_ALISTS

/* Define this if you want PCRE instead of traditional regexps.
 */

#undef USE_PCRE

/* Define this if you want mySQL support (assuming that your host
 * actually offers this.
 */
#undef USE_MYSQL

/* Maximum size of a socket send buffer.
 */
#define SET_BUFFER_SIZE_MAX      65536

/* Runtime statistics:
 *  COMM_STAT: count number and size of outgoing packets.
 *  APPLY_CACHE_STAT: count number of hits and misses in the apply cache.
 */
#define COMM_STAT
#define APPLY_CACHE_STAT

/* Which memory manager to use. Possible defines are
 *   MALLOC_smalloc:   Satoria's malloc. Fastest, uses the least memory,
 *                     supports garbage collection.
 *   MALLOC_sysmalloc: the normal system malloc()
 *   MALLOC_ptmalloc:  ptmalloc from www.malloc.de - threadsafe.
 */

#define MALLOC_smalloc

/* If  MIN_MALLOCED is > 0,
 * the gamedriver will reserve this amount of memory on startup for
 * large blocks, thus reducing the large block fragmentation. The value
 * therefore should be a significantly large multiple of the large
 * chunk size.
 * As a rule of thumb, reserve enough memory to cover the first couple
 * of days of uptime.
 */
#define MIN_MALLOCED   0

/* If  MIN_SMALL_MALLOCED is > 0,
 * the gamedriver will reserve this amount of memory on startup for
 * small blocks, thus reducing the large block fragmentation. The value
 * therefore should be a significantly large multiple of the small
 * chunk size.
 * As a rule of thumb, reserve enough memory to cover the first couple
 * of days of uptime.
 */
#define MIN_SMALL_MALLOCED   0

/* This value gives the upper limit for the total allocated memory
 * (useful for systems with no functioning process limit).
 * A value of 0 means 'unlimited'.
 */
#define MAX_MALLOCED         0x4000000

/* Define this to annotate all allocations with a magic word to find
 * simple misuses of memory (like multiple frees).
 * Supported by: MALLOC_smalloc
 */
#define MALLOC_CHECK

/* Define this to annotate all allocations with file:line of the driver
 * source responsible for it.
 * Supported by: MALLOC_smalloc
 */
#undef MALLOC_TRACE

/* Define this to annotate all allocations with file:line of the lpc program
 * responsible for it.
 * Supported by: MALLOC_smalloc
 */
#undef MALLOC_LPC_TRACE

/* Trace the most recently executed bytecode instructions?
 */
#define TRACE_CODE

/* If using TRACE_CODE , how many instructions should be kept? */
#define TOTAL_TRACE_LENGTH      0x1000

/* If you want to use threads to write the data to the sockets 
 * define USE_PTHREADS. To limit the memory usage of each thread
 * define PTHREAD_WRITE_MAX_SIZE to a value greater than zero.
 * The implementation will discard the oldest not yet written 
 * data blocks to keep memoty usage under the limit.
 */
#undef USE_PTHREADS
#define PTHREAD_WRITE_MAX_SIZE 100000

/*----------------------------------------------------------------*/
/* The following macros activate various debugging and profiling
 * code segments.
 */

/* Enable basic run time sanity checks. This will use more time
 * and space, but nevertheless you are strongly encouraged to keep
 * it defined.
 */
#define DEBUG

/* The DEBUG level for the ERQ daemon: 0 means 'no debug', 1 means
 * 'standard debug', 2 means 'verbose debug'.
 */
#define ERQ_DEBUG 0

/* Enable debug output from the LPC compiler.
 */
/* #define YYDEBUG 1 */

/* Disable inlining.
 */
/* #define NO_INLINES */

/* Enable the shared string checking (enables commandline option
 * --check-strings).
 */
#define CHECK_STRINGS

/* Shared strings are never really freed.
 */
/* #define KEEP_STRINGS */

/* Activate debug prints in the telnet machine.
 */
/* #define DEBUG_TELNET */

/* Activate allocation debug prints in the smalloc module.
 */
/* #define DEBUG_SMALLOC_ALLOCS */

/* Trace changes to the tot_alloc_object and tot_alloc_object_size
 * statistics, in order to find the status bugs (enables commandline
 * option --check-object-stat). Will produce a decent amount of
 * output on stderr.
 */
#define CHECK_OBJECT_STAT

/* Enable the automatic data cleanup from the backend.
 * TODO: Remove this macro once it is proven stable.
 */
#define NEW_CLEANUP

/* Activate total mapping size consistency check code. It has a small
 * impact on the execution speed. This define was used to find
 * the inaccuracy in the mapping statistic.
 */
/* #define CHECK_MAPPING_TOTAL */

/* Activate object refcount check code. It will produce a decent
 * amount of log output. It will also fatal() the driver as soon
 * as it detects an inconsistency in the list of destructed objects.
 */
/* #define CHECK_OBJECT_REF */

/* Activate object referencing checking code during the GC. It will
 * print error messages to gcout when an object or program is
 * referenced as something else. No penalty for using.
 * Requires MALLOC_TRACE to work. Incompatible with DUMP_GC_REFS.
 */
#ifdef MALLOC_TRACE
#    define CHECK_OBJECT_GC_REF
#endif

/* Sometimes the GC stumbles over invalid references to memory
 * blocks (namely 'Program referenced as something else'). Define
 * this macro to get a detailed dump of all found references
 * (Warning: LOTS of output!). Incompatible with CHECK_OBJECT_GC_REF.
 */
/* #define DUMP_GC_REFS */

/* Enable usage statistics of VM instructions.
 * For profiling of the VM instructions themselves, see the Profiling
 * Options in the Makefile.
 */
/* #define OPCPROF */

#ifdef OPCPROF
/* With OPCPROF, the dump of the statistics include the names
 * of the instructions.
 */
/* #define VERBOSE_OPCPROF */
#endif

/* Define this if you want structs supported.
 */
#define USE_STRUCTS

/* Define this if you want the new inline closure syntax supported.
 * (f-020528)
 */
#undef USE_NEW_INLINES

#endif /* CONFIG_H__ */
