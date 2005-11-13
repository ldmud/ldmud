#include "erq.h"
#include "erq-config.h"
#include "driver.h"
#include <stdio.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <stdlib.h>
#include <stdarg.h>

#ifdef _AIX
#include <sys/select.h>
#endif

#ifdef WEXITSTATUS
typedef int wait_status_t;
#else
typedef union wait wait_status_t;
#define WEXITSTATUS(status) ((status).w_retcode)
#define WTERMSIG(status) ((status).w_termsig)
#endif
#ifndef SIGCLD
#define SIGCLD SIGCHLD
#endif

#ifndef SIGKILL
#define SIGKILL 9
#endif
#ifndef SIGTERM
#define SIGTERM 15
#endif

#define randomize(n) seed_random(n)
#define get_ticket() random_number(0xffffffff)

#ifdef TIOCNOTTY
#define DETACH
#endif

struct ticket_s {
    int32 seq;
    int32 rnd;
};

#define TICKET_SIZE sizeof(struct ticket_s)

struct queue_s {
    int pos, len;
    struct queue_s *next;
    char buf[0];
};

struct socket_s {
    int32 handle;
    struct ticket_s ticket;
    struct socket_s *next;
    char type;
    int fd;
    struct queue_s *queue;
};

struct child_s {
    int32 handle;
    struct ticket_s ticket;
    struct child_s *next;
    char type;
    char status;
    pid_t pid;
    wait_status_t return_code;
    struct socket_s *fd, *err;
};

struct auth_s {
    struct socket_s s;
    int32 local_port,remote_port;
    char buf[ERQ_MAX_REPLY];
    int pos;
};

#define AUTH_PORT 113

struct retry_s {
    time_t time;
    void (*func)(char *, int);
    struct retry_s *next;
    char mesg[0];
};

#define CHILD_EXECUTE		1
#define CHILD_SPAWN		2

#define CHILD_RUNNING		1
#define CHILD_EXITED		2

#define SOCKET_UDP		1
#define SOCKET_LISTEN		2
#define SOCKET_CONNECTED	3
#define SOCKET_WAIT_ACCEPT	4
#define SOCKET_WAIT_CONNECT	5
#define SOCKET_STDOUT		6
#define SOCKET_STDERR		7
#define SOCKET_WAIT_AUTH	8
#define SOCKET_AUTH		9

void die();
void bad_request(char *);
void erq_cmd();
void sig_child();
void remove_child(struct child_s *);
void read_socket(struct socket_s *, int);
struct socket_s *new_socket(int, char);
void reply_errno(int32);
void reply1(int32, const char *, int32);
void reply1keep(int32, const char *, int32);
void replyn(int32, int, int, ...);
void write1(char *, int);
void add_to_queue(struct queue_s **, char *, int);
int flush_queue(struct queue_s **, int);
void send_auth(struct auth_s *);

void erq_rlookup(char *, int);
void erq_execute(char *, int);
void erq_fork(char *, int);
void erq_auth(char *, int);
void erq_spawn(char *, int);
void erq_send(char *, int);
void erq_kill(char *, int);
void erq_open_udp(char *, int);
void erq_open_tcp(char *, int);
void erq_listen(char *, int);
void erq_accept(char *, int);
void erq_lookup(char *, int);

extern struct child_s *childs;
extern struct socket_s *sockets;
extern int seq_number, seq_interval;

#ifndef ERQ_MAX_SEND
#define ERQ_MAX_SEND 256
#endif

#ifdef FIX_ALIGNMENT
static inline int32 read_32(char *a)
{
    int32 x;
    memcpy((char*)&x, a, sizeof(x));
    return ntohl(x);
}

static inline void write_32(char *a, int32 i)
{
    i=htonl(i);
    memcpy(a, (char*)&i, sizeof(i));
}
#else /* FIX_ALIGNMENT */
static inline int32 read_32(char *a)
{
    return ntohl(*(int32*)a);
}

static inline void write_32(char *a, int32 i)
{
    *(int32*)a=htonl(i);
}
#endif /* FIX_ALIGNMENT */

#define get_handle(x) read_32((x)+4)
