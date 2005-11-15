#ifndef LPC_FILES_H_
#define LPC_FILES_H_ 1

/* Definitions and macros for the various file efuns */

/* Special return values of file_size() and the size part of get_dir().
 */

#define FSIZE_NOFILE  -1  /* File doesn't exist or is not readable */
#define FSIZE_DIR     -2  /* The file is actually a directory */


/* Flag values for get_dir().
 */

#define GETDIR_EMPTY      0  /* return an empty array (not very useful) */
#define GETDIR_NAMES      1  /* return the filenames */
#define GETDIR_SIZES      2  /* return the file sizes */
#define GETDIR_DATES      4  /* return the dates of last modification */
#define GETDIR_UNSORTED  32  /* return the results unsorted */

#define GETDIR_ALL        7  /* return names, sizes and dates */

#endif /* LPC_FILES_H_ */
