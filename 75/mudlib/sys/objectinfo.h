#ifndef _OBJECTINFO_H_
#define _OBJECTINFO_H_

/* Definition of argument values for object_info() and
 * of the indices in the corresponding result arrays.
 */

/* Possible types of information requested from object_info()
 */
#define OINFO_BASIC     0
#define OINFO_POSITION  1
#define OINFO_MEMORY    2


/* Indices in the array resulting from OINFO_BASIC
 */

#define OIB_HEART_BEAT          0
#define OIB_IS_WIZARD           1
#define OIB_ENABLE_COMMANDS     2
#define OIB_CLONE               3
#define OIB_DESTRUCTED          4
#define OIB_SWAPPED             5
#define OIB_ONCE_INTERACTIVE    6
#define OIB_APPROVED            7
#define OIB_RESET_STATE         8
#define OIB_WILL_CLEAN_UP       9
#define OIB_LAMBDA_REFERENCED  10
#define OIB_SHADOW             11
#define OIB_TOTAL_LIGHT        12
#define OIB_NEXT_RESET         13
#define OIB_TIME_OF_REF        14
#define OIB_REF                15
#define OIB_GIGATICKS          16
#define OIB_TICKS              17
#define OIB_SWAP_NUM           18
#define OIB_PROG_SWAPPED       19
#define OIB_VAR_SWAPPED        20
#define OIB_NAME               21
#define OIB_LOAD_NAME          22
#define OIB_NEXT_ALL           23
#define OIB_PREV_ALL           24


/* Indices in the array resulting from OINFO_POSITION
 */
#define OIP_NEXT  0
#define OIP_PREV  1
#define OIP_POS   2


/* Indices in the array resulting from OINFO_MEMORY
 */
#define OIM_REF              0
#define OIM_NAME             1
#define OIM_PROG_SIZE        2
#define OIM_NUM_FUNCTIONS    3
#define OIM_SIZE_FUNCTIONS   4
#define OIM_NUM_VARIABLES    5
#define OIM_SIZE_VARIABLES   6
#define OIM_NUM_STRINGS      7
#define OIM_SIZE_STRINGS     8
#define OIM_NUM_INHERITED    9
#define OIM_SIZE_INHERITED  10 
#define OIM_TOTAL_SIZE      11


#endif /* _OBJECTINFO_H_ */
