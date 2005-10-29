#ifndef LPC_FILES_H_
#define LPC_FILES_H_ 1

/* Definitions and macros for the various file efuns */

/* Special return values of file_size() and the size part of get_dir().
 */

#define FSIZE_NOFILE  -1  /* File doesn't exist or is not readable */
#define FSIZE_DIR     -2  /* The file is actually a directory */


/* Flag values for get_dir().
 */

#define GETDIR_EMPTY       (0)  /* return an empty array (not very useful) */
#define GETDIR_NAMES     (0x01)  /* return the filenames */
#define GETDIR_SIZES     (0x02)  /* return the file sizes */
#define GETDIR_DATES     (0x04)  /* return the dates of last modification */
/*                       (0x08) unused */
#define GETDIR_ACCESS    (0x40)  /* return the dates of last access */
#define GETDIR_MODES     (0x80)  /* return the file mode */

#define GETDIR_PATH      (0x10)  /* with _NAMES: add the path to the filenames */
#define GETDIR_UNSORTED  (0x20)  /* return the results unsorted */

#define GETDIR_ALL       (0xDF)  /* return all */

#endif /* LPC_FILES_H_ */
