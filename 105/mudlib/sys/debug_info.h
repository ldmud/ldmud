#ifndef _DEBUG_INFO_H_
#define _DEBUG_INFO_H_ 1

/* Flag values for efun debug_info().
 */

#define DINFO_OBJECT  0  /* Print information about an object */
#define DINFO_MEMORY  1  /* Print memory usage of an object */
#define DINFO_OBJLIST 2  /* Return an object from the global list */
#define DINFO_MALLOC  3  /* Print the information from the 'malloc' cmd */
#define DINFO_STATUS  4  /* Parse and execute a 'status' command */

#endif /* _DEBUG_INFO_H_ */
