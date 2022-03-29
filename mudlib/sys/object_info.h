#ifndef LPC_OBJECT_INFO_H_
#define LPC_OBJECT_INFO_H_

/* Definition of argument values for object_info()
 */

#include "configuration.h"

/* Object flags */
#define OI_ONCE_INTERACTIVE             -1
#define OI_RESET_STATE                  -2
#define OI_WILL_CLEAN_UP                -3
#define OI_LAMBDA_REFERENCED            -4
#define OI_REPLACED                     -5

/* Program flags */
#define OI_NO_INHERIT                   -10
#define OI_NO_CLONE                     -11
#define OI_NO_SHADOW                    -12
#define OI_SHARE_VARIABLES              -13
#define OI_NO_LIGHTWEIGHT               -14

/* Swapping */
#define OI_SWAPPED                      -20
#define OI_PROG_SWAPPED                 -21
#define OI_VAR_SWAPPED                  -22
#define OI_SWAP_NUM                     -23

/* Timing */
#define OI_NEXT_RESET_TIME              -30
#define OI_NEXT_CLEANUP_TIME            -31
#define OI_LAST_REF_TIME                -32

/* Object list */
#define OI_OBJECT_NEXT                  -40
#define OI_OBJECT_PREV                  -41
#define OI_OBJECT_POS                   -42

/* Shadows */
#define OI_SHADOW_NEXT                  -50
#define OI_SHADOW_PREV                  -51
#define OI_SHADOW_ALL                   -52

/* Statistics about the object */
#define OI_OBJECT_REFS                  -60
#define OI_TICKS                        -61
#define OI_GIGATICKS                    -62
#define OI_DATA_SIZE                    -63
#define OI_DATA_SIZE_TOTAL              -64

/* Statistics about the program */
#define OI_PROG_REFS                    -70

#define OI_NUM_FUNCTIONS                -71
#define OI_NUM_VARIABLES                -72
#define OI_NUM_STRINGS                  -73
#define OI_NUM_INHERITED                -74
#define OI_NUM_INCLUDED                 -75

#define OI_SIZE_FUNCTIONS               -76
#define OI_SIZE_VARIABLES               -77
#define OI_SIZE_STRINGS                 -78
#define OI_SIZE_STRINGS_DATA            -79
#define OI_SIZE_STRINGS_DATA_TOTAL      -80
#define OI_SIZE_INHERITED               -81
#define OI_SIZE_INCLUDED                -82

#define OI_PROG_SIZE                    -83
#define OI_PROG_SIZE_TOTAL              -84

#endif /* LPC_OBJECT_INFO_H_ */
