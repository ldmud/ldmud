#ifndef _DEBUG_INFO_H_
#define _DEBUG_INFO_H_ 1

/* Request values for efun debug_info().
 */

#define DINFO_OBJECT  0  /* Print information about an object */
#define DINFO_MEMORY  1  /* Print memory usage of an object */
#define DINFO_OBJLIST 2  /* Return an object from the global list */
#define DINFO_MALLOC  3  /* Print the information from the 'malloc' cmd */
#define DINFO_STATUS  4  /* Return the 'status' information */
#define DINFO_DUMP    5  /* Dump some special information into files */
#define DINFO_DATA    6  /* Return internal information */

/* Sub-request values for debug_info(DINFO_DATA) */

#define DID_STATUS  0  /* Return the 'status' and 'status tables' information */
#define DID_SWAP    1  /* Return the 'status swap' information */
#define DID_MEMORY  2  /* Return the 'status malloc' information */

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
#define DID_ST_OBJECTS_PROCESSED   9
#define DID_ST_OBJECTS_AVG_PROC   10

#define DID_ST_OTABLE             11
#define DID_ST_OTABLE_SLOTS       12
#define DID_ST_OTABLE_SIZE        13

#define DID_ST_HBEAT_OBJS         14
#define DID_ST_HBEAT_CALLS        15
#define DID_ST_HBEAT_SLOTS        16
#define DID_ST_HBEAT_SIZE         17
#define DID_ST_HBEAT_PROCESSED    18
#define DID_ST_HBEAT_AVG_PROC     19

#define DID_ST_CALLOUTS           20
#define DID_ST_CALLOUT_SLOTS      21
#define DID_ST_CALLOUT_SIZE       22

#define DID_ST_ARRAYS             23
#define DID_ST_ARRAYS_SIZE        24

#define DID_ST_MAPPINGS           25
#define DID_ST_MAPPINGS_SIZE      26

#define DID_ST_PROGS              27
#define DID_ST_PROGS_SIZE         28

#define DID_ST_PROGS_SWAPPED      29
#define DID_ST_PROGS_SWAP_SIZE    30

#define DID_ST_USER_RESERVE       31
#define DID_ST_MASTER_RESERVE     32
#define DID_ST_SYSTEM_RESERVE     33

#define DID_ST_ADD_MESSAGE        34
#define DID_ST_PACKETS            35
#define DID_ST_PACKET_SIZE        36

#define DID_ST_APPLY              37
#define DID_ST_APPLY_HITS         38

#define DID_ST_STRINGS            39
#define DID_ST_STRING_SIZE        40
#define DID_ST_STR_TABLE_SIZE     41
#define DID_ST_STR_REQ            42
#define DID_ST_STR_REQ_SIZE       43
#define DID_ST_STR_SEARCHES       44
#define DID_ST_STR_SEARCH_LEN     45

#define DID_ST_RX_CACHED          46
#define DID_ST_RX_TABLE           47
#define DID_ST_RX_TABLE_SIZE      48
#define DID_ST_RX_REQUESTS        49
#define DID_ST_RX_REQ_FOUND       50
#define DID_ST_RX_REQ_COLL        51

#define DID_STATUS_MAX            52 /* Total number of entries */


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
#define DID_MEM_SBKR_SIZE       2
#define DID_MEM_LARGE           3
#define DID_MEM_LARGE_SIZE      4
#define DID_MEM_LFREE           5
#define DID_MEM_LFREE_SIZE      6
#define DID_MEM_CHUNK           7
#define DID_MEM_CHUNK_SIZE      8
#define DID_MEM_SMALL           9
#define DID_MEM_SMALL_SIZE     10
#define DID_MEM_SFREE          11
#define DID_MEM_SFREE_SIZE     12
#define DID_MEM_UNUSED         13
#define DID_MEM_MINC_CALLS     14
#define DID_MEM_MINC_SUCCESS   15
#define DID_MEM_MINC_SIZE      16
 
#define DID_MEMORY_MAX         17

#endif /* _DEBUG_INFO_H_ */
