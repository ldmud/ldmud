/* portable communication, common definitions - WA */

#include <errno.h>
#include <sys/time.h>

#define ECOMNIL    0  /* no error */
#define ECOMUSG  128  /* usage error */
#define ECOMLNK  129  /* link error */
#define ECOMFLW  130  /* flow problem */
#define ECOMPRM  131  /* preempted */

struct caddr {
    int addr;
};

typedef struct _c_time_t {
    int dummy;
} c_time_t;

void c_listen(void);
void c_shutdown(void);
int c_read(int channel,char *data,int length);
int c_write(int channel,char *data,int length);
int c_accept(struct caddr *addr,int *length);
int c_select(int *mask,struct timeval/*c_time_t*/ *timeout);
int c_close(int channel);
void c_echo(int channel,int mode);
int c_getaddr(int channel,struct caddr *addr,int *length);
