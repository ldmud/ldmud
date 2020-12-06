#ifndef LPC_LWOBJECT_INFO_H_
#define LPC_LWOBJECT_INFO_H_

/* Definition of argument values for lwobject_info()
 */

#include "configuration.h"

/* LWOject info */
#define LI_LWOBJECT_REFS                 -1
#define LI_DATA_SIZE                     -2
#define LI_DATA_SIZE_TOTAL               -3

/* Program flags */
#define LI_NO_INHERIT                   -10
#define LI_NO_CLONE                     -11
#define LI_NO_LIGHTWEIGHT               -12
#define LI_SHARE_VARIABLES              -13

/* Statistics about the program */
#define LI_PROG_REFS                    -20

#define LI_NUM_FUNCTIONS                -30
#define LI_NUM_VARIABLES                -31
#define LI_NUM_STRINGS                  -32
#define LI_NUM_INHERITED                -33
#define LI_NUM_INCLUDED                 -34

#define LI_SIZE_FUNCTIONS               -35
#define LI_SIZE_VARIABLES               -36
#define LI_SIZE_STRINGS                 -37
#define LI_SIZE_STRINGS_DATA            -38
#define LI_SIZE_STRINGS_DATA_TOTAL      -39
#define LI_SIZE_INHERITED               -40
#define LI_SIZE_INCLUDED                -41

#define LI_PROG_SIZE                    -42
#define LI_PROG_SIZE_TOTAL              -43

#endif /* LPC_LWOBJECT_INFO_H_ */
