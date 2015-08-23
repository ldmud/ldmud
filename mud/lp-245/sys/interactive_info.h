#ifndef LPC_INTERACTIVE_INFO_H_
#define LPC_INTERACTIVE_INFO_H_

/* Definition of argument values for interactive_info()
 */

#include "configuration.h"

/* Connection information */
#define II_IP_NAME                      -1
#define II_IP_NUMBER                    -2
#define II_IP_PORT                      -3
#define II_IP_ADDRESS                   -4
#define II_MUD_PORT                     -5

/* Telnet related information */
#define II_MCCP_STATS                   -10

/* Input handling */
#define II_INPUT_PENDING                -20
#define II_EDITING                      -21
#define II_IDLE                         -22

/* Output handling */
#define II_SNOOP_NEXT                   -30
#define II_SNOOP_PREV                   -31
#define II_SNOOP_ALL                    -32

#endif /* LPC_INTERACTIVE_INFO_H_ */
