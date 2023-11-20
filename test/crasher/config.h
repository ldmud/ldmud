/* Configuration for the crasher. */

/* The file to replay. */
#define REPLAY_FILE "/crash/LAST_EXEC"

/* If defined and the replay file doesn't exist, generate a new file. */
#define GENERATE_IF_NO_REPLAY_FILE

/* Number of calls to do in one run. */
#define CALLS 100000

/* Maximum number of arguments for efun calls. */
#define MAX_ARGS 5

/* Reinitialization after this many calls. */
#define REINIT_LENGTH 1000

/* Prints each executed function before executing. */
#undef VERBOSE

/* Test a specific efun. */
// #define TEST_EFUN #'to_type
