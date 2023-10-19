#ifndef ACCESS_CHECK_H__
#define ACCESS_CHECK_H__ 1

#include "config.h"

#include "driver.h"
#include "comm.h"

extern char *access_file;
extern char *access_log;

extern char * allow_host_access(sockaddr_in4or6 *full_addr, int, long *idp);
extern void release_host_access(long num);
extern void initialize_host_access();

#endif /* ACCESS_CHECK_H__ */

