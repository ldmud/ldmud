#ifndef XERQ_DEFS_H__
#define XERQ_DEFS_H__ 1

/* Standard include for all modules of xerq containing all possible
 * system includes, datatypes and external definitions.
 */

#include "driver.h"

#ifndef ERQ_DEBUG
#  define ERQ_DEBUG 0
#endif

#ifdef USE_IPV6
#    define __IPV6__
#endif

#include "erq.h"

#include <stdio.h>
#include <sys/socket.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#include <time.h>
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
#include <ctype.h>

#ifdef _AIX
#    include <sys/select.h>
#endif

#ifdef WEXITSTATUS
typedef int wait_status_t;
#else
typedef union wait wait_status_t;
#define WEXITSTATUS(status) ((status).w_retcode)
#define WTERMSIG(status) ((status).w_termsig)
#endif

#ifndef SIGCLD
#    define SIGCLD SIGCHLD
#endif

#ifndef SIGKILL
#    define SIGKILL 9
#endif
#ifndef SIGTERM
#    define SIGTERM 15
#endif

#define randomize(n) srandom(n)
#define get_ticket() (random() & 0x7FFFFFFF)

#ifdef TIOCNOTTY
#    define DETACH
#endif

#ifndef ERQ_MAX_SEND
#    define ERQ_MAX_SEND 256
#endif

/* --- Types --- */

typedef struct ticket_s ticket_t;
typedef struct equeue_s equeue_t;
typedef struct socket_s socket_t;
typedef struct child_s  child_t;
typedef struct auth_s   auth_t;
typedef struct retry_s  retry_t;

/* --- struct ticket_s
 * Tickets are used to identify tasks and consist of a computed
 * and a random number.
 */

struct ticket_s
{
    int32 seq;  /* The seq_number assigned to this ticket */
    int32 rnd;  /* A random number generated for this ticket */
};

#define TICKET_SIZE sizeof(struct ticket_s)

/* --- struct equeue_s: one (pending) message
 *
 * We can't call this queue_s/queue_t because Solaris already has
 * a datastructure with that name :-(
 */

struct equeue_s
{
    equeue_t *next;
    int       pos;    /* index of next byte in buf[] to write */
    int       len;    /* remaining length to write */
    int32     handle; /* message handle for send-ok replys */
    char      buf[1]; /* allocated big enough to hold the data */
};

/* --- struct socket_s: one socket descriptor
 * This structure is used for explicite TCP/UDP communication, as well
 * as for the communication with spawned subprograms (then .handle and
 * .ticket are copied from the controlling child_t).
 */

struct socket_s
{
    socket_t *next;
    int32     handle;
    ticket_t  ticket;
    char      type;   /* Type of the socket */
    int       fd;     /* This socket's fd */
    equeue_t *queue;  /* List of messages pending to send */
};

/* Possible socket.types: */

#define SOCKET_UDP           1  /* UDP socket */
#define SOCKET_LISTEN        2  /* Accept-socket waiting for connections */
#define SOCKET_CONNECTED     3  /* Connected TCP socket */
#define SOCKET_WAIT_ACCEPT   4  /* Accept-socket with connection pending */
#define SOCKET_WAIT_CONNECT  5  /* TCP socket while connecting */
#define SOCKET_STDOUT        6  /* Command: stdio socket */
#define SOCKET_STDERR        7  /* Command: stderr socket */
#define SOCKET_WAIT_AUTH     8  /* authd-socket while connecting */
#define SOCKET_AUTH          9  /* authd-socket */

/* --- struct child_s
 * Child structures are used for executed and spawned commands.
 */

struct child_s
{
    child_t       *next;
    int32          handle;
    ticket_t       ticket;
    char           type;         /* Type of the child */
    char           status;       /* child status */
    pid_t          pid;          /* PID of the child process */
    wait_status_t  return_code;  /* exitstatus returned by wait() */
    socket_t      *fd;           /* NULL or the stdio fd */
    socket_t      *err;          /* NULL or the stderr fd */
};

/* Values for child_t.type */

#define CHILD_EXECUTE  1
#define CHILD_SPAWN    2
#define CHILD_FORK     3

/* Values for child_t.status */

#define CHILD_RUNNING  1  /* Child is being started up or running */
#define CHILD_EXITED   2  /* Child exited */

/* --- struct auth_s: one auth_d query
 * auth_t is treated as subclass of socket_t.
 */

struct auth_s
{
    socket_t s;                   /* The subclassed socket_t */
    int32    local_port;          /* Our port of the connection */
    int32    remote_port;         /* Remote port of the connection */
    char     buf[ERQ_MAX_REPLY];  /* Buffer for the reply */
    int      pos;                 /* Position in .buf */
};

#define AUTH_PORT 113

/* --- struct retry_s
 * List of function calls to retry at a given time.
 */

struct retry_s
{
    retry_t *next;
    time_t time;  /* time() when this should be tried */
    void (*func)(char *, int);
      /* Function to call as: (*func)(.mesg, read_32(.mesg))
       */
    char mesg[1];
      /* Allocated big enough, this is the data for .func.
       */
};


/* --- Variables --- */

extern const char * erq_dir;
extern child_t *childs;
extern socket_t *sockets;
extern int seq_number, seq_interval;
extern pid_t master_pid;

/* --- Prototypes --- */

extern char *time_stamp(void);
extern void die(void);
extern void bad_request(char *);
extern void erq_cmd(void);
extern void sig_child();
extern void remove_child(child_t *);
extern int read_socket(socket_t *, int);
extern socket_t *new_socket(int, char);
extern void reply_errno(int32);
extern void reply1(int32, const void *, int32);
extern void reply1keep(int32, const void *, int32);
extern void replyn(int32, int, int, ...);
extern void write1(void *, int);
extern void add_to_queue(equeue_t **, char *, int, int32);
extern int flush_queue(equeue_t **, int, int);
extern void add_retry(void (*func)(char *, int), char *mesg, int len, int t);

extern void erq_rlookup(char *, int);
extern void erq_execute(char *, int);
extern void erq_fork(char *, int);
extern void erq_auth(char *, int);
extern void erq_spawn(char *, int);
extern void erq_send(char *, int);
extern void erq_kill(char *, int);
extern void erq_open_udp(char *, int);
extern void erq_open_tcp(char *, int);
extern void erq_listen(char *, int);
extern void erq_accept(char *, int);
extern void erq_lookup(char *, int);
#ifdef USE_IPV6
extern void erq_rlookupv6(char *, int);
#endif

extern void close_socket(socket_t *);

/* --- Debug Functions --- */

#if ERQ_DEBUG > 1
#   define XPRINTF(x) fprintf x
#else
#   define XPRINTF(x)
#endif

/* --- Inline Functions --- */

static INLINE int32
read_32(char *a)
{
    int32 x;
    memcpy(&x, a, sizeof(x));
    return ntohl(x);
}

static INLINE void
write_32(char *a, int32 i)
{
    i=htonl(i);
    memcpy(a, &i, sizeof(i));
}

#define get_handle(x) read_32((x)+4)

#endif /* XERQ_DEFS_H__ */

