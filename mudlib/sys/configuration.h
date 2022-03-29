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
#define IC_ENCODING                     12

/* Possible options for configure_object().
 */
#define OC_COMMANDS_ENABLED    0
#define OC_HEART_BEAT          1
#define OC_EUID                2

/* Possible options for configure_lwobject().
 */
#define LC_EUID                          0

/* Possible options for configure_driver().
 */
#define DC_MEMORY_LIMIT                  0
#define DC_ENABLE_HEART_BEATS            1
#define DC_LONG_EXEC_TIME                2
#define DC_DATA_CLEAN_TIME               3
#define DC_TLS_CERTIFICATE               4
#define DC_TLS_DHE_PARAMETER             5
#define DC_TLS_CIPHERLIST                6
#define DC_EXTRA_WIZINFO_SIZE            7
#define DC_DEFAULT_RUNTIME_LIMITS        8
#define DC_SWAP_COMPACT_MODE             9
#define DC_SWAP_TIME                     10
#define DC_SWAP_VAR_TIME                 11
#define DC_CLEANUP_TIME                  12
#define DC_RESET_TIME                    13
#define DC_DEBUG_FILE                    14
#define DC_FILESYSTEM_ENCODING           15

#define DC_SIGACTION_SIGHUP              20
#define DC_SIGACTION_SIGINT              21
#define DC_SIGACTION_SIGUSR1             22
#define DC_SIGACTION_SIGUSR2             23

/* Values for the DC_SIGACTION_SIG* options:
 */
#define DCS_DEFAULT                      0
#define DCS_IGNORE                       1
#define DCS_TERMINATE                    2
#define DCS_SHUTDOWN                     3
#define DCS_INFORM_MASTER                4
#define DCS_RELOAD_MASTER                5
#define DCS_THROW_EXCEPTION              6

#endif /* LPC_CONFIGURATION_H_ */
