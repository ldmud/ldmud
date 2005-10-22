#ifndef LPC_DEBUG_INFO_H_
#define LPC_DEBUG_INFO_H_ 1

/* Request values for efun debug_info().
 */

#define DINFO_OBJECT  0  /* Print information about an object */
#define DINFO_MEMORY  1  /* Print memory usage of an object */
#define DINFO_OBJLIST 2  /* Return an object from the global list */
#define DINFO_MALLOC  3  /* Print the information from the 'malloc' cmd */
#define DINFO_STATUS  4  /* Return the 'status' information */
#define DINFO_DUMP    5  /* Dump some special information into files */
#define DINFO_DATA    6  /* Return internal information */
#define DINFO_TRACE   7  /* Return the current call trace */

/* Sub-request values for debug_info(DINFO_DATA) */

#define DID_STATUS  0  /* Return the 'status' and 'status tables' information */
#define DID_SWAP    1  /* Return the 'status swap' information */
#define DID_MEMORY  2  /* Return the 'status malloc' information */

/* Sub-request values for debug_info(DINFO_TRACE) */

#define DIT_CURRENT        0  /* Return the current call chain as an array */
#define DIT_ERROR          1  /* Return the last error call chain as an array */
#define DIT_UNCAUGHT_ERROR 2  /* Return the last uncaught error call chain */
#define DIT_STR_CURRENT    3  /* Return the current call chain as a string */

/* Indices into the array resulting from debug_info(DINFO_DATA, DID_STATUS)
 */

#define DID_ST_ACTIONS             0
#define DID_ST_ACTIONS_SIZE        1
#define DID_ST_SHADOWS             2
#define DID_ST_SHADOWS_SIZE        3

#define DID_ST_OBJECTS             4
#define DID_ST_OBJECTS_SIZE        5
#define DID_ST_OBJECTS_SWAPPED     6
#define DID_ST_OBJECTS_SWAP_SIZE   7
#define DID_ST_OBJECTS_LIST        8
#define DID_ST_OBJECTS_NEWLY_DEST  9
#define DID_ST_OBJECTS_DESTRUCTED 10
#define DID_ST_OBJECTS_PROCESSED  11
#define DID_ST_OBJECTS_AVG_PROC   12

#define DID_ST_OTABLE             13
#define DID_ST_OTABLE_SLOTS       14
#define DID_ST_OTABLE_SIZE        15

#define DID_ST_HBEAT_OBJS         16
#define DID_ST_HBEAT_CALLS        17
#define DID_ST_HBEAT_CALLS_TOTAL  18
#define DID_ST_HBEAT_SLOTS        19
#define DID_ST_HBEAT_SIZE         20
#define DID_ST_HBEAT_PROCESSED    21
#define DID_ST_HBEAT_AVG_PROC     22 

#define DID_ST_CALLOUTS           23
#define DID_ST_CALLOUT_SLOTS      24
#define DID_ST_CALLOUT_SIZE       25

#define DID_ST_ARRAYS             26
#define DID_ST_ARRAYS_SIZE        27

#define DID_ST_MAPPINGS           28
#define DID_ST_MAPPINGS_SIZE      29

#define DID_ST_PROGS              30
#define DID_ST_PROGS_SIZE         31

#define DID_ST_PROGS_SWAPPED      32
#define DID_ST_PROGS_SWAP_SIZE    33

#define DID_ST_USER_RESERVE       34
#define DID_ST_MASTER_RESERVE     35
#define DID_ST_SYSTEM_RESERVE     36

#define DID_ST_ADD_MESSAGE        37
#define DID_ST_PACKETS            38
#define DID_ST_PACKET_SIZE        39

#define DID_ST_APPLY              40
#define DID_ST_APPLY_HITS         41

#define DID_ST_STRINGS            42
#define DID_ST_STRING_SIZE        43
#define DID_ST_STR_TABLE_SIZE     44
#define DID_ST_STR_OVERHEAD       45
#define DID_ST_STR_IT_OVERHEAD    46
#define DID_ST_UNTABLED           47
#define DID_ST_UNTABLED_SIZE      48
#define DID_ST_ITABLED            49
#define DID_ST_ITABLED_SIZE       50
#define DID_ST_TABLED             51
#define DID_ST_TABLED_SIZE        52
#define DID_ST_STR_SEARCHES       53
#define DID_ST_STR_SEARCHLEN      54
#define DID_ST_STR_SEARCHES_BYVALUE   55
#define DID_ST_STR_SEARCHLEN_BYVALUE  56
#define DID_ST_STR_CHAINS         57
#define DID_ST_STR_ADDED          58
#define DID_ST_STR_DELETED        59
#define DID_ST_STR_COLLISIONS     60
#define DID_ST_STR_FOUND          61
#define DID_ST_STR_FOUND_BYVALUE  62

#define DID_ST_RX_CACHED          63
#define DID_ST_RX_TABLE           64
#define DID_ST_RX_TABLE_SIZE      65
#define DID_ST_RX_REQUESTS        66
#define DID_ST_RX_REQ_FOUND       67
#define DID_ST_RX_REQ_COLL        68

#define DID_STATUS_MAX            69 /* Total number of entries */


/* Indices into the array resulting from debug_info(DINFO_DATA, DID_SWAP)
 */

#define DID_SW_PROGS            0
#define DID_SW_PROG_SIZE        1
#define DID_SW_PROG_UNSWAPPED   2
#define DID_SW_PROG_U_SIZE      3
#define DID_SW_VARS             4
#define DID_SW_VAR_SIZE         5
#define DID_SW_FREE             6
#define DID_SW_FREE_SIZE        7
#define DID_SW_FILE_SIZE        8
#define DID_SW_REUSED           9
#define DID_SW_SEARCHES        10
#define DID_SW_SEARCH_LEN      11
#define DID_SW_F_SEARCHES      12
#define DID_SW_F_SEARCH_LEN    13
#define DID_SW_COMPACT         14
#define DID_SW_RECYCLE_FREE    15

#define DID_SWAP_MAX           16
    

/* Indices into the array resulting from debug_info(DINFO_DATA, DID_MEMORY)
 */

#define DID_MEM_NAME            0
#define DID_MEM_SBRK            1
#define DID_MEM_SBRK_SIZE       2
#define DID_MEM_LARGE           3
#define DID_MEM_LARGE_SIZE      4
#define DID_MEM_LFREE           5
#define DID_MEM_LFREE_SIZE      6
#define DID_MEM_LWASTED         7
#define DID_MEM_LWASTED_SIZE    8
#define DID_MEM_CHUNK           9
#define DID_MEM_CHUNK_SIZE     10
#define DID_MEM_SMALL          11
#define DID_MEM_SMALL_SIZE     12
#define DID_MEM_SFREE          13
#define DID_MEM_SFREE_SIZE     14
#define DID_MEM_SWASTED        15
#define DID_MEM_SWASTED_SIZE   16
#define DID_MEM_UNUSED         17
#define DID_MEM_MINC_CALLS     18
#define DID_MEM_MINC_SUCCESS   19 
#define DID_MEM_MINC_SIZE      20
#define DID_MEM_PERM           21
#define DID_MEM_PERM_SIZE      22
#define DID_MEM_CLIB           23
#define DID_MEM_CLIB_SIZE      24
#define DID_MEM_OVERHEAD       25
#define DID_MEM_ALLOCATED      26
#define DID_MEM_USED           27
#define DID_MEM_TOTAL_UNUSED   28
 
#define DID_MEMORY_MAX         29


/* Indices into the subarrays resulting from debug_info(DINFO_TRACE, 0)
 */

#define TRACE_TYPE    0
#define TRACE_NAME    1
#define TRACE_PROGRAM 2
#define TRACE_OBJECT  3
#define TRACE_LOC     4

#define TRACE_MAX     5

/* Values for entry TRACE_TYPE */

#define TRACE_TYPE_SYMBOL  0
#define TRACE_TYPE_SEFUN   1
#define TRACE_TYPE_EFUN    2
#define TRACE_TYPE_LAMBDA  3
#define TRACE_TYPE_LFUN    4

#endif /* LPC_DEBUG_INFO_H_ */
