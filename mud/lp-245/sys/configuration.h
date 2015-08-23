#ifndef LPC_CONFIGURATION_H_
#define LPC_CONFIGURATION_H_

/* Definition of argument values for the configure_*() efuns.
 */

/* Possible options for configure_interactive().
 */
#define IC_MAX_WRITE_BUFFER_SIZE         0
#define IC_SOCKET_BUFFER_SIZE            1
#define IC_COMBINE_CHARSET_AS_STRING     2
#define IC_COMBINE_CHARSET_AS_ARRAY      3
#define IC_CONNECTION_CHARSET_AS_STRING  4
#define IC_CONNECTION_CHARSET_AS_ARRAY   5
#define IC_QUOTE_IAC                     6
#define IC_TELNET_ENABLED                7
#define IC_MCCP                          8
#define IC_PROMPT                        9
#define IC_MAX_COMMANDS                 10
#define IC_MODIFY_COMMAND               11

/* Possible options for configure_opbject().
 */
#define OC_COMMANDS_ENABLED    0
#define OC_HEART_BEAT          1

/* Possible options for configure_driver().
 */
#define DC_MEMORY_LIMIT                  0
#define DC_ENABLE_HEART_BEATS            1
#define DC_LONG_EXEC_TIME                2
#define DC_DATA_CLEAN_TIME               3
#define DC_TLS_CERTIFICATE               4
#define DC_EXTRA_WIZINFO_SIZE            5
#define DC_DEFAULT_RUNTIME_LIMITS        6
#define DC_SWAP_COMPACT_MODE             7

#endif /* LPC_CONFIGURATION_H_ */
