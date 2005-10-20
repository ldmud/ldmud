#ifndef LPC_TRACE_H_
#define LPC_TRACE_H_

/* Argument values for the trace() efun.
 */

#define TRACE_NOTHING       0  /* Stop tracing */

#define TRACE_CALL          1  /* Trace all lfun calls */
#define TRACE_CALL_OTHER    2  /* Trace inter-object calls */
#define TRACE_RETURN        4  /* Trace function returns */
#define TRACE_ARGS          8  /* Print function arguments and results */
#define TRACE_EXEC         16  /* Trace all executed instructions */
#define TRACE_HEART_BEAT   32  /* Trace heartbeat code */
#define TRACE_APPLY        64  /* Trace (internal) applies */
#define TRACE_OBJNAME     128  /* Print the object names */

#endif /* LPC_TRACE_H_ */
