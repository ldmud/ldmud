#ifndef LPC_DRIVER_HOOK_H_
#define LPC_DRIVER_HOOK_H_ 1

/* --- Driver Hooks ---
 */

#define H_MOVE_OBJECT0           0
#define H_MOVE_OBJECT1           1
#define H_LOAD_UIDS              2
#define H_CLONE_UIDS             3
#define H_CREATE_SUPER           4
#define H_CREATE_OB              5
#define H_CREATE_CLONE           6
#define H_RESET                  7
#define H_CLEAN_UP               8
#define H_MODIFY_COMMAND         9
#define H_NOTIFY_FAIL           10
#define H_NO_IPC_SLOT           11
#define H_INCLUDE_DIRS          12
#define H_TELNET_NEG            13
#define H_NOECHO                14
#define H_ERQ_STOP              15
#define H_MODIFY_COMMAND_FNAME  16
#define H_COMMAND               17
#define H_SEND_NOTIFY_FAIL      18
#define H_AUTO_INCLUDE          19
#define H_DEFAULT_METHOD        20
#define H_DEFAULT_PROMPT        21
#define H_PRINT_PROMPT          22
#define H_REGEXP_PACKAGE        23
#define H_MSG_DISCARDED         24

#define NUM_DRIVER_HOOKS        25  /* Number of hooks */

#endif /* LPC_DRIVER_HOOK_ */

