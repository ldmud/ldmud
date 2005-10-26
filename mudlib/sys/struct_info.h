#ifndef LPC_STRUCTINFO_H_
#define LPC_STRUCTINFO_H_

#ifndef __DRIVER_SOURCE__
#include "lpctypes.h"
#endif

/* Definition of argument values for struct_info() and
 * of the indices in the corresponding result arrays.
 */

/* Possible types of information requested from struct_info()
 */
#define SINFO_FLAT    0
#define SINFO_NESTED  1


/* Indices in the result array
 */

#define SI_NAME         0
#define SI_PROG_NAME    1
#define SI_PROG_ID      2
#define SI_BASE         3
#define SI_MEMBER       4

#define SI_MAX 5  /* Min Number of SI_ result elements */

/* Indices in the SI_MEMBER arrays
 */

#define SIM_NAME   0
#define SIM_TYPE   1
#define SIM_EXTRA  2

#define SIM_MAX 3 /* Number of SIM_ elements */

#endif /* LPC_STRUCTINFO_H_ */
