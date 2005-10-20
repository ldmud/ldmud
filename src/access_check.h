#ifndef __ACCESS_CHECK_H__
#define __ACCESS_CHECK_H__ 1

#include "config.h"

#if defined(ACCESS_CONTROL)

#include "driver.h"
#include "comm.h" /* TODO: struct sockaddr_in */

extern char * allow_host_access(struct sockaddr_in *full_addr, int, long *idp);
extern void release_host_access(long num);

#endif /* ACCESS_CONTROL */

#endif /* __ACCESS_CHECK_H__ */

