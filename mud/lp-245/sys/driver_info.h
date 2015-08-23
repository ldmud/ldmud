#ifndef LPC_DRIVER_INFO_H_
#define LPC_DRIVER_INFO_H_

/* Definition of argument values for driver_info()
 */

#include "configuration.h"

/* Driver Environment */
#define DI_BOOT_TIME                                          -1

/* LPC Runtime status */
#define DI_CURRENT_RUNTIME_LIMITS                            -10
#define DI_EVAL_NUMBER                                       -11

/* Network configuration */
#define DI_MUD_PORTS                                         -20
#define DI_UDP_PORT                                          -21

/* Memory management */
#define DI_MEMORY_RESERVE_USER                               -30
#define DI_MEMORY_RESERVE_MASTER                             -31
#define DI_MEMORY_RESERVE_SYSTEM                             -32

/* Traces */
#define DI_TRACE_CURRENT                                     -40
#define DI_TRACE_CURRENT_DEPTH                               -41
#define DI_TRACE_CURRENT_AS_STRING                           -42
#define DI_TRACE_LAST_ERROR                                  -43
#define DI_TRACE_LAST_ERROR_AS_STRING                        -44
#define DI_TRACE_LAST_UNCAUGHT_ERROR                         -45
#define DI_TRACE_LAST_UNCAUGHT_ERROR_AS_STRING               -46

/* LPC Runtime statistics */
#define DI_NUM_FUNCTION_NAME_CALLS                          -100
#define DI_NUM_FUNCTION_NAME_CALL_HITS                      -101
#define DI_NUM_FUNCTION_NAME_CALL_MISSES                    -102

#define DI_NUM_OBJECTS_LAST_PROCESSED                       -103

#define DI_NUM_HEARTBEAT_TOTAL_CYCLES                       -104
#define DI_NUM_HEARTBEAT_ACTIVE_CYCLES                      -105
#define DI_NUM_HEARTBEATS_LAST_PROCESSED                    -106

#define DI_NUM_STRING_TABLE_STRINGS_ADDED                   -110
#define DI_NUM_STRING_TABLE_STRINGS_REMOVED                 -111
#define DI_NUM_STRING_TABLE_LOOKUPS_BY_VALUE                -112
#define DI_NUM_STRING_TABLE_LOOKUPS_BY_INDEX                -113
#define DI_NUM_STRING_TABLE_LOOKUP_STEPS_BY_VALUE           -114
#define DI_NUM_STRING_TABLE_LOOKUP_STEPS_BY_INDEX           -115
#define DI_NUM_STRING_TABLE_HITS_BY_VALUE                   -116
#define DI_NUM_STRING_TABLE_HITS_BY_INDEX                   -117
#define DI_NUM_STRING_TABLE_COLLISIONS                      -118

#define DI_NUM_REGEX_LOOKUPS                                -120
#define DI_NUM_REGEX_LOOKUP_HITS                            -121
#define DI_NUM_REGEX_LOOKUP_MISSES                          -122
#define DI_NUM_REGEX_LOOKUP_COLLISIONS                      -123

/* Network statistics */
#define DI_NUM_MESSAGES_OUT                                 -200
#define DI_NUM_PACKETS_OUT                                  -201
#define DI_NUM_PACKETS_IN                                   -202
#define DI_SIZE_PACKETS_OUT                                 -203
#define DI_SIZE_PACKETS_IN                                  -204

/* Load */
#define DI_LOAD_AVERAGE_COMMANDS                            -300
#define DI_LOAD_AVERAGE_LINES                               -301
#define DI_LOAD_AVERAGE_PROCESSED_OBJECTS                   -302
#define DI_LOAD_AVERAGE_PROCESSED_OBJECTS_RELATIVE          -303
#define DI_LOAD_AVERAGE_PROCESSED_HEARTBEATS_RELATIVE       -304

/* Memory use statistics */
#define DI_NUM_ACTIONS                                      -400
#define DI_NUM_CALLOUTS                                     -401
#define DI_NUM_HEARTBEATS                                   -402
#define DI_NUM_SHADOWS                                      -403
#define DI_NUM_OBJECTS                                      -404
#define DI_NUM_OBJECTS_SWAPPED                              -405
#define DI_NUM_OBJECTS_IN_LIST                              -406
#define DI_NUM_OBJECTS_IN_TABLE                             -407
#define DI_NUM_OBJECTS_DESTRUCTED                           -408
#define DI_NUM_OBJECTS_NEWLY_DESTRUCTED                     -409
#define DI_NUM_OBJECT_TABLE_SLOTS                           -410
#define DI_NUM_PROGS                                        -411
#define DI_NUM_PROGS_SWAPPED                                -412
#define DI_NUM_PROGS_UNSWAPPED                              -413
#define DI_NUM_ARRAYS                                       -414
#define DI_NUM_MAPPINGS                                     -415
#define DI_NUM_MAPPINGS_CLEAN                               -416
#define DI_NUM_MAPPINGS_HASH                                -417
#define DI_NUM_MAPPINGS_HYBRID                              -418
#define DI_NUM_STRUCTS                                      -419
#define DI_NUM_STRUCT_TYPES                                 -420
#define DI_NUM_VIRTUAL_STRINGS                              -421
#define DI_NUM_STRINGS                                      -422
#define DI_NUM_STRINGS_TABLED                               -423
#define DI_NUM_STRINGS_UNTABLED                             -424
#define DI_NUM_STRING_TABLE_SLOTS                           -425
#define DI_NUM_STRING_TABLE_SLOTS_USED                      -426
#define DI_NUM_REGEX                                        -427
#define DI_NUM_REGEX_TABLE_SLOTS                            -428

#define DI_SIZE_ACTIONS                                     -450
#define DI_SIZE_CALLOUTS                                    -451
#define DI_SIZE_HEARTBEATS                                  -452
#define DI_SIZE_SHADOWS                                     -453
#define DI_SIZE_OBJECTS                                     -454
#define DI_SIZE_OBJECTS_SWAPPED                             -455
#define DI_SIZE_OBJECT_TABLE                                -456
#define DI_SIZE_PROGS                                       -457
#define DI_SIZE_PROGS_SWAPPED                               -458
#define DI_SIZE_PROGS_UNSWAPPED                             -459
#define DI_SIZE_ARRAYS                                      -460
#define DI_SIZE_MAPPINGS                                    -461
#define DI_SIZE_STRUCTS                                     -462
#define DI_SIZE_STRUCT_TYPES                                -463
#define DI_SIZE_STRINGS                                     -464
#define DI_SIZE_STRINGS_TABLED                              -465
#define DI_SIZE_STRINGS_UNTABLED                            -466
#define DI_SIZE_STRING_TABLE                                -467
#define DI_SIZE_STRING_OVERHEAD                             -468
#define DI_SIZE_REGEX                                       -469
#define DI_SIZE_BUFFER_FILE                                 -470
#define DI_SIZE_BUFFER_SWAP                                 -471

/* Memory swapper statistics */
#define DI_NUM_SWAP_BLOCKS                                  -500
#define DI_NUM_SWAP_BLOCKS_FREE                             -501
#define DI_NUM_SWAP_BLOCKS_REUSE_LOOKUPS                    -502
#define DI_NUM_SWAP_BLOCKS_REUSE_LOOKUP_STEPS               -503
#define DI_NUM_SWAP_BLOCKS_FREE_LOOKUPS                     -505
#define DI_NUM_SWAP_BLOCKS_FREE_LOOKUP_STEPS                -506
#define DI_SIZE_SWAP_BLOCKS                                 -507
#define DI_SIZE_SWAP_BLOCKS_FREE                            -508
#define DI_SIZE_SWAP_BLOCKS_REUSED                          -509
#define DI_SWAP_RECYCLE_PHASE                               -510

/* Memory allocator statistics */
#define DI_MEMORY_ALLOCATOR_NAME                            -600

#define DI_NUM_SYS_ALLOCATED_BLOCKS                         -610
#define DI_NUM_LARGE_BLOCKS_ALLOCATED                       -611
#define DI_NUM_LARGE_BLOCKS_FREE                            -612
#define DI_NUM_LARGE_BLOCKS_WASTE                           -613
#define DI_NUM_SMALL_BLOCKS_ALLOCATED                       -614
#define DI_NUM_SMALL_BLOCKS_FREE                            -615
#define DI_NUM_SMALL_BLOCKS_WASTE                           -616
#define DI_NUM_SMALL_BLOCK_CHUNKS                           -617
#define DI_NUM_UNMANAGED_BLOCKS                             -618
#define DI_NUM_FREE_BLOCKS_AVL_NODES                        -619

#define DI_SIZE_SYS_ALLOCATED_BLOCKS                        -630
#define DI_SIZE_LARGE_BLOCKS_ALLOCATED                      -631
#define DI_SIZE_LARGE_BLOCKS_FREE                           -632
#define DI_SIZE_LARGE_BLOCKS_WASTE                          -633
#define DI_SIZE_LARGE_BLOCK_OVERHEAD                        -634
#define DI_SIZE_SMALL_BLOCKS_ALLOCATED                      -635
#define DI_SIZE_SMALL_BLOCKS_FREE                           -636
#define DI_SIZE_SMALL_BLOCKS_WASTE                          -637
#define DI_SIZE_SMALL_BLOCK_OVERHEAD                        -638
#define DI_SIZE_SMALL_BLOCK_CHUNKS                          -639
#define DI_SIZE_UNMANAGED_BLOCKS                            -640
#define DI_SIZE_MEMORY_USED                                 -641
#define DI_SIZE_MEMORY_UNUSED                               -642
#define DI_SIZE_MEMORY_OVERHEAD                             -643

#define DI_NUM_INCREMENT_SIZE_CALLS                         -650
#define DI_NUM_INCREMENT_SIZE_CALL_SUCCESSES                -651
#define DI_SIZE_INCREMENT_SIZE_CALL_DIFFS                   -652
#define DI_NUM_REPLACEMENT_MALLOC_CALLS                     -653
#define DI_SIZE_REPLACEMENT_MALLOC_CALLS                    -654
#define DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_FULL            -655
#define DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_TARGETED        -656
#define DI_NUM_MEMORY_DEFRAGMENTATION_CALL_TARGET_HITS      -657
#define DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_INSPECTED      -658
#define DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_MERGED         -659
#define DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_RESULTING      -660

#define DI_MEMORY_EXTENDED_STATISTICS                       -670

/* Status texts */
#define DI_STATUS_TEXT_MEMORY                               -700
#define DI_STATUS_TEXT_TABLES                               -701
#define DI_STATUS_TEXT_SWAP                                 -702
#define DI_STATUS_TEXT_MALLOC                               -703
#define DI_STATUS_TEXT_MALLOC_EXTENDED                      -704

/* Indices into the subarrays of DI_MEMORY_EXTENDED_STATISTICS (if given) */

#define DIM_ES_MAX_ALLOC   0
#define DIM_ES_CUR_ALLOC   1
#define DIM_ES_MAX_FREE    2
#define DIM_ES_CUR_FREE    3
#define DIM_ES_AVG_XALLOC  4
#define DIM_ES_AVG_XFREE   5
#define DIM_ES_FULL_SLABS  6
#define DIM_ES_FREE_SLABS  7
#define DIM_ES_TOTAL_SLABS 8

#define DIM_ES_MAX  9


/* Definition of argument values for dump_driver_info()
 */

#define DDI_OBJECTS                     0
#define DDI_OBJECTS_DESTRUCTED          1
#define DDI_OPCODES                     2
#define DDI_MEMORY                      3

/* Indices into the subarrays resulting from driver_info(DI_TRACE_*)
 */

#define TRACE_TYPE    0
#define TRACE_NAME    1
#define TRACE_PROGRAM 2
#define TRACE_OBJECT  3
#define TRACE_LOC     4
#ifdef __EVAL_COST_TRACE__
#define TRACE_EVALCOST 5

#define TRACE_MAX     6
#else
#define TRACE_MAX     5
#endif

/* Values for entry TRACE_TYPE */

#define TRACE_TYPE_SYMBOL  0
#define TRACE_TYPE_SEFUN   1
#define TRACE_TYPE_EFUN    2
#define TRACE_TYPE_LAMBDA  3
#define TRACE_TYPE_LFUN    4

#endif /* LPC_DRIVER_INFO_H_ */
