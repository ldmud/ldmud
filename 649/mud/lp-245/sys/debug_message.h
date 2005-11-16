#ifndef LPC_DEBUG_MESSAGE_H_
#define LPC_DEBUG_MESSAGE_H_ 1

/* Definitions and macros for the debug_message() */

#define DMSG_DEFAULT 0         /* log to stdout and .debug.log */
#define DMSG_STDOUT  (1 << 0)  /* log to stdout */
#define DMSG_STDERR  (1 << 1)  /* log to stderr */
#define DMSG_LOGFILE (1 << 2)  /* log to .debug.log */

#define DMSG_STAMP   (1 << 3)  /* Prepend the timestamp */

/* Derived macros */

#define DMSG_TARGET (DMSG_STDOUT|DMSG_STDERR|DMSG_LOGFILE)
  /* Mask for all target bitflags */

#endif /* LPC_DEBUG_MESSAGE_H_ */
