/*---------------------------------------------------------------------------
 * External Request Daemon
 *
 *---------------------------------------------------------------------------
 * This is the standard implementation of the forked ERQ daemon. The
 * implementation is straightforward and follows what is documented in
 * doc/util/erq.
 *
 * The erq can handle up to MAX_CHILDS (yes, it should be "children")
 * concurrent tasks. Simple non-blocking tasks (all the TCP/UDP functions)
 * are handled by the master ERQ, the other tasks are loaded off to
 * subservers - forked copies of the ERQ. If the driver requests to
 * start a program, one of the subservers is selected and forks again
 * to run the program; the communication chain is then driver <-> ERQ
 * <-> subserver <-> program. If a subserver is finished with its
 * task, it is allowed to stay around for a certain idle time CHILD_EXPIRE,
 * reducing the number of costly fork() operations when several requests
 * follow after each other.
 *
 * This ERQ implementation uses two type of tickets.
 * For programs:
 *     { int32 child_number }
 * For sockets:
 *     { int32 child_number; int32 rnd; int32 seq; }
 *
 * TODO: This program could be commented MUCH better, but in comparison
 * TODO:: to its original state it's already a big improvement.
 * TODO:: And it surely still contains bugs.
 *---------------------------------------------------------------------------
 */
 
#include <sys/types.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/times.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include "machine.h"

#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#    include <string.h>
#else
#    include <strings.h>
#endif
#ifdef HAVE_BSTRING_H
#    include <bstring.h>
#endif
#ifdef HAVE_STDLIB_H
#    include <stdlib.h>
#endif
#ifdef HAVE_SYS_TERMIOS_H
#    include <sys/termios.h>
#endif
#ifdef _AIX
#    include <sys/select.h>
#endif

#ifdef WEXITSTATUS
#    define wait_status_t int
#else
#    define wait_status_t union wait
#    define WEXITSTATUS(status) ((status).w_retcode)
#    define WTERMSIG(status) ((status).w_termsig)
#endif

#ifdef sun
time_t time(time_t *);
#endif

#ifndef SIGCLD
#    define SIGCLD  SIGCHLD
#endif
#ifndef SIGKILL
#    define SIGKILL 9
#endif
#ifndef SIGTERM
#    define SIGTERM 15
#endif

#if defined(_AIX)
typedef unsigned long length_t;
#elif defined(__INTEL_COMPILER) || defined (__GNUC__)
typedef socklen_t length_t;
#else
typedef int length_t;
#endif

#define AUTH_PORT 113 /* according to RFC 931 */

#include "config.h"

#ifdef USE_IPV6
#    define __IPV6__
#endif

#include "erq.h"

#define randomize_tickets(n) srandom(n)
#define get_ticket()         random()

#ifndef ERQ_DEBUG
#  define ERQ_DEBUG 0
#endif

/*-------------------------------------------------------------------------*/

/* #define DETACH */
  /* Define this for automatic detach from console.
   */

#define MAX_CHILDS             36
  /* Maximum number of children.
   */

#define TIME_TO_CHECK_CHILDS   900
  /* Delaytime for select() if no TCP children have data to send to
   * the driver.
   */

#define CHILD_EXPIRE           900
   /* Maximum idle time before a child is killed.
    */

#define ERQ_BUFSIZE            1024
  /* Size of the data buffer.
   */

#define MAX_REPLY              ERQ_MAX_REPLY


#if MAX_REPLY >= ERQ_BUFSIZE
#    undef MAX_REPLY
#    define MAX_REPLY (ERQ_BUFSIZE - 1)
#endif

/*-------------------------------------------------------------------------*/

typedef struct child_s child_t;

/* --- ticket: one ticket for a TCP connection ---
 *
 * The actual ticket is composed of the .rnd and .seq
 * field - the union with c[] helps in memcpy()s.
 */

union ticket_u
{
    struct ticket_s
    {
        long rnd, seq;
    } s;
    char c[1];
};

/* --- struct child_s: one child ---
 *
 * This structure is used to describe one child.
 */

struct child_s
{
    int socket; /* Socket this child holds */

    union  /* Child specific information */
    {
        struct /* Subserver child */
        {
            pid_t  pid;        /* pid of the subserver */
            time_t last_used;  /* time of the last use */
        } c;

        struct /* Communication child */
        {
            char           handle[4];
              /* The handle assigned by the driver */
            union ticket_u ticket;
            time_t         last_recv;
              /* Time of the last received transmission */
            int            bytes_recv;
              /* Number of bytes left to send to the driver */
        } s;
    } u;

    int state;
#     define CHILD_FREE   0  /* Idle subserver */
#     define CHILD_LISTEN 1
#     define CHILD_BUSY   2  /* Active subserver */
#     define CHILD_UDP    3  /* UDP socket child */
#     define CHILD_TCP    4  /* TCP socket child */
#     define CHILD_ACCEPT 5  /* TCP port waiting for connections */

    child_t *next_free;  /* Next child in free list */
    child_t *next_all;   /* Next child in its list */
};

/*-------------------------------------------------------------------------*/

static child_t childs[MAX_CHILDS] = {{0}};
  /* Table of all children created so far.
   */

static int next_child_index = 0;
  /* Next index in childs[] to use.
   */

static child_t *all_childs     = NULL;
  /* List of all children connected to active subservers.
   */

static child_t *free_childs    = NULL;
  /* List of all children connected to idle subservers.
   */

static child_t *udp_sockets    = NULL;
  /* List of children with an open UDP socket.
   */

static child_t *tcp_sockets    = NULL;
  /* List of children with an open TCP connection.
   */

static child_t *accept_sockets = NULL;
  /* List of children waiting for TCP connections.
   */

static child_t *child_slots    = NULL;
  /* List of unused children.
   */

static int childs_waited_for = 0;
  /* Number of children which termination has been acknowledged by wait().
   * If waitpid() is not available, this value is compared with
   * childs_terminated to see if there are zombies to be waited for.
   */

static volatile int childs_terminated = 0;
  /* Count of terminated children, maintained by SIGCLD.
   */

static fd_set current_fds, readfds;
  /* Master 'read' fdset and the copy for select().
   */

static fd_set current_fds2, writefds;
  /* Master 'write' fdset and the copy for select().
   */

static int nfds = 2;
  /* Number of opened fds for select().
   */

static time_t current_time;
  /* The current time() after select() returns.
   */

static char buf[ERQ_BUFSIZE];
  /* The receive buffer.
   */

static const char * erq_dir = ERQ_DIR;
  /* The filename of the directory with the ERQ executables. */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static void start_subserver (long server_num, long seed);

/*-------------------------------------------------------------------------*/
static char *
time_stamp (void)

/* Return a textual representation of the current time
 * in the form "YYYY.MM.DD HH:MM:SS [xerq]".
 * Result is a pointer to a static buffer.
 *
 * Putting this function in strfuns is not a good idea, because
 * it is need by almost every module anyway.
 */

{
    time_t t;
    static char result[27];
    struct tm *tm;

    t = time(NULL);
    tm = localtime(&t);
    strftime(result, sizeof(result), "%Y.%m.%d %H:%M:%S [erq]", tm);
    return result;
} /* time_stamp() */

/*-------------------------------------------------------------------------*/
static long
get_seed(void)

/* Return a seed for the random generator.
 */

{
#ifdef HAVE_GETTIMEOFDAY

    struct timeval tv; struct timezone tz;

    gettimeofday(&tv, &tz);
    return tv.tv_sec * 1000000L + tv.tv_usec;

#else

    /* SunOS crashes on times(0), and times returns 0/-1 */

    ret = (long)times(&dummy_tms);
    if (!ret)
        ret = (long)time((time_t *)0) +
          dummy_tms.tms_utime
          dummy_tms.tms_stime +
          dummy_tms.tms_cutime +
          dummy_tms.tms_cstime;
    return ret;

#endif
} /* get_seed() */

/*-------------------------------------------------------------------------*/
static long
read_32 (char *str)

/* Extract an int32 from the data stream at <str> and return it.
 * TODO: Put functions like this, and complete msg-functions into
 * TODO:: an erqlib.c module.
 */

{
    unsigned char *p = (unsigned char *)str;
    return (long)p[0]<<24 | (long)p[1]<<16 | (long)p[2]<<8 | p[3];
} /* read_32() */

/*-------------------------------------------------------------------------*/
static void
write_32 (char *str, long n)

/* Store the int32 <n> into the data stream at <str>.
 */

{
    *(str+=3) = n;
    *--str = n >>= 8;
    *--str = n >>= 8;
    *--str = n >>= 8;
} /* write_32() */

/*-------------------------------------------------------------------------*/
static long
readn (int s, void *buf, long expected)

/* Read at least <expected> bytes from fd <s> into <buf>.
 * Return the number of bytes actually read; 0 for EOF and -1 on error.
 */

{
    long num, total = 0;
    char * pbuf;

    pbuf = (char *)buf;
    do {
        do
            num = read(s, pbuf+total, expected-total);
        while (num == -1 && errno == EINTR);

        if (num <= 0)
        {
            if (!num)
            {
                fprintf(stderr, "%s read: EOF\n", time_stamp());
                return 0;
            }
            else
            {
                perror("read");
                return -1;
           }
        }
        total += num;
#if ERQ_DEBUG > 0
        if (total < expected)
            fprintf(stderr, "%s read fragment %ld\n", time_stamp(), num);
#endif
    } while (num > 0 && total < expected);

    return total;
} /* readn() */

/*-------------------------------------------------------------------------*/
static void
writen (int n, void *message, long length)

/* Write the <message> of <length> bytes onto fd <n>.
 */

{
    long num;

    do
        num = write(n, message, length);
    while (num == -1 && errno == EINTR);

    if (num != length)
    {
        fprintf(stderr, "%s wrote %ld, should be %ld\n"
                      , time_stamp(), num, length);
        fprintf(stderr, "%s Giving up.\n", time_stamp());
        abort();
    }
} /* writen() */

/*-------------------------------------------------------------------------*/
static void
write1 (void *message, long length)

/* Write the <message> of <length> bytes onto stdout, ie to the gamedriver.
 */

{
    writen(1, message, length);
} /* write1() */

/*-------------------------------------------------------------------------*/
static int
execute (char *buf, long buflen, char *status, int *sockets)

/* exec() the command <buf> (length <buflen>), store the ERQ_-success code
 * into <status> and return the pid (0 on failure).
 * If <sockets> is non-NULL, it points to an int[4] into which two
 * socketpairs are generated.
 */

{
    char path[256], argbuf[1024], *p, *args[96], **argp, c;
    pid_t pid;
    int quoted;

    quoted = 0;
    status[0] = ERQ_E_FORKFAIL; /* Good default */
    status[1] = 0;

    if (buflen >= sizeof argbuf)
    {
        status[0] = ERQ_E_ARGLENGTH;
        return 0;
    }

    argp = &args[0];
    *argp++ = p = argbuf;
    while (--buflen >= 0)
    {
        c = *buf++;
        if (c == '\\')
        {
            if (--buflen >= 0)
            {
                *p++ = *buf++;
            }
            else
            {
                status[0] = ERQ_E_ARGFORMAT;
                return 0;
            }
        }
        else if (c == '"')
        {
            quoted = !quoted;
        }
        else if (isgraph(c) || quoted)
        {
            *p++ = c;
        }
        else
        {
            *p++ = '\0';
            *argp++ = p;
            if (argp == &args[sizeof args/sizeof args[0]])
            {
                status[0] = ERQ_E_ARGNUMBER;
                return 0;
            }
            while( ( (c = *buf) == ' ' || c == '\t') && --buflen >= 0)
                buf++;
        }
    }

    *p++ = '\0';
    *argp++ = 0;
    p = args[0];
    if (p[0] == '/' || strstr(p, ".."))
    {
        status[0] = ERQ_E_ILLEGAL;
        return 0;
    }

    if (strlen(erq_dir) + strlen(p) + 2 > sizeof(path))
    {
        status[0] = ERQ_E_PATHLEN;
        return 0;
    }

    sprintf(path, "%s/%s", erq_dir, p);
    if (sockets)
    {
        if(socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0)
        {
            perror("socketpair");
            status[0] = ERQ_E_FORKFAIL;
            status[1] = errno;
            return 0;
        }
        if(socketpair(AF_UNIX, SOCK_STREAM, 0, sockets + 2) < 0)
        {
            perror("socketpair");
            close(sockets[0]);
            close(sockets[1]);
            status[0] = ERQ_E_FORKFAIL;
            status[1] = errno;
            return 0;
        }
    }

    pid = fork();
    if (!pid)
    {
        close(0);
        close(1);
        if (sockets)
        {
            dup2(sockets[0], 0);
            dup2(sockets[0], 1);
            dup2(sockets[2], 2);
            close(sockets[0]);
            close(sockets[1]);
            close(sockets[2]);
            close(sockets[3]);
        }
        execv(path, args);
        _exit(1);
    }

    if (sockets)
    {
        close(sockets[0]);
        close(sockets[2]);
    }

    if (pid < 0)
    {
        if (sockets)
        {
            close(sockets[1]);
            close(sockets[3]);
        }
        status[0] = ERQ_E_FORKFAIL;
        status[1] = errno;
        return 0;
    }
    
    return pid;
} /* execute() */

/*-------------------------------------------------------------------------*/
static void
count_sigcld (int sig)

/* SIGCLD handler, which is called whenever a child process terminates.
 * The function takes care to handle multiple SIGCLD occuring at
 * the same time - the tricky point is to setup the the next signal(SIGCLD,...)
 * after the last pending SIGCLD has been resolved.
 */

{
#ifdef __MWERKS__
#    pragma unused(sig)
#endif
        
#ifndef HAVE_WAITPID
    static volatile int calling_signal = 0;     /* Mutex */
    static volatile int call_signal_again = 0;  /* Number of pending calls */

    if (!calling_signal)
    {
        calling_signal = 1;
        (void)signal(SIGCLD, (RETSIGTYPE(*)())count_sigcld);
        while (call_signal_again)
        {
            --call_signal_again;
            (void)signal(SIGCLD, (RETSIGTYPE(*)())count_sigcld);
        }
        calling_signal = 0;
    }
    else
    {
        call_signal_again++;
    }
#if ERQ_DEBUG > 0
    write(2, "child terminated\n", 17);
#endif
    childs_terminated++;
#endif
} /* count_sigcld() */

/*-------------------------------------------------------------------------*/
static void
dispose_child (child_t *child)

/* The subserver <child> is dead and to be cleant up.
 */

{
    struct child_s **chp;

    /* Remove it from the fdsets */
    FD_CLR(child->socket, &current_fds);
    FD_CLR(child->socket, &current_fds2);
    close(child->socket);

    /* Remove it from the free list */
    if (child->state == CHILD_FREE)
    {
        for (chp = &free_childs; *chp != child; chp = &(*chp)->next_free) /* NOOP */;
        *chp = child->next_free;
    }

    child->state = CHILD_FREE;

    /* Remove it from the all list */
    for (chp = &all_childs; *chp != child; chp = &(*chp)->next_all) /* NOOP */;
    *chp = child->next_all;

    /* Put it into the list of unused children */
    child->next_all = child_slots;
    child_slots = child;
} /* dispose_child() */

/*-------------------------------------------------------------------------*/
static void
kill_child (child_t *child)

/* Kill the subserver child <child>.
 */

{
#if ERQ_DEBUG > 0
    fprintf(stderr, "%s kill_child called\n", time_stamp());
#endif
    kill(child->u.c.pid, SIGKILL);
    dispose_child(child);
} /* kill_child() */

/*-------------------------------------------------------------------------*/
static int
free_socket_childs (void)

/* Is there a child available?
 */

{
    return !(!child_slots && next_child_index >= MAX_CHILDS);
} /* free_socket_childs() */

/*-------------------------------------------------------------------------*/
static child_t *
get_socket_child(void)

/* Get a new socket child - if possible, reuse an earlier child.
 */

{
    struct child_s *child;

    if ( !(child = child_slots) )
    {
        child = &childs[next_child_index++];
    }
    else
    {
        child_slots = child->next_all;
    }

    return child;
} /* get_socket_child() */

/*-------------------------------------------------------------------------*/
static void
free_socket_child (child_t *child, child_t **listp)

/* The socket <child> in list <listp> is no longer used - remove it
 * from the list and make it available again.
 */

{
   while (*listp != child)
       listp = &(*listp)->next_all;
   *listp = child->next_all;
   child->next_all = child_slots;
   child->state = CHILD_FREE;
   child_slots = child;
} /* free_socket_child() */

/*-------------------------------------------------------------------------*/
static int
get_subserver (void)

/* Start a new subserver, resp. reactivate an idle one, and return
 * its socket.
 */

{
    struct child_s *child;

#if ERQ_DEBUG > 0
    fprintf(stderr, "%s get_subserver called\n", time_stamp());
#endif

    if (free_childs)
    {
        /* There is an idle one we can use */

        child = free_childs;
        free_childs = child->next_free;
    }
    else
    {
        /* We have to start a new subserver */

        int sockets[2];
        int pid;

        if (!free_socket_childs())
            return -1;

        if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0)
        {
            int err = errno;
            fprintf(stderr, "%s Error in socketpair()", time_stamp());
            errno = err;
            perror("socketpair");
            return -1;
        }

        pid = fork();
        if(pid == -1)
        {
            close(sockets[0]);
            close(sockets[1]);
            return -1;
        }

        child = get_socket_child();
        if (!pid)
        {
            /* This is the child process */

            dup2(sockets[0], 0); /* Read and write on the same socket */
            dup2(sockets[0], 1);
            close(sockets[0]);
            close(sockets[1]);
            start_subserver(child - &childs[0], (long)get_ticket());
            /* NOTREACHED */
        }

        /* Set up the child and put it into the subserver list */
        
        close(sockets[0]);
        child->u.c.pid = pid;
        child->socket = sockets[1];
        FD_SET(child->socket, &current_fds);
        if (child->socket >= nfds)
            nfds = child->socket + 1;

        child->next_all = all_childs;
        all_childs = child;
    }

    child->state = CHILD_BUSY;
    return child->socket;
} /* get_subserver() */

/*-------------------------------------------------------------------------*/
static void
start_subserver (long server_num, long seed)

/* We are ERQ subserver number <num> and shall use <seed> for our ticket.
 * The ERQ main process forks us off to do some time-expensive things
 * (like looking up hostnames). If we are idle too long, we will be killed
 * off again. Communication to the main process is through stdin/stdout.
 */

{
    union ticket_u ticket;
    char header[16];
    long num, msglen;
    int request;
    pid_t child = 0;
    int child_sockets[4];
    char child_handle[4];

#if defined(DETACH) && defined(TIOCNOTTY)
    /* Detach from console */

    num = open("/dev/tty", O_RDWR);
    if (num >= 0)
    {
        /* We supply header as a 'dummy' argument in case typing is
         * too strict
         */
        ioctl(num, TIOCNOTTY, header);
        close(num);
    }
#endif
    randomize_tickets(seed ^ get_seed());
    FD_ZERO(&readfds);
    FD_ZERO(&writefds);
    nfds = 1;
    childs_terminated = 0;
    childs_waited_for = 0;

    /* possible race conditions make switching the signal handler awkward */
    (void)signal(SIGCLD, (RETSIGTYPE(*)())count_sigcld);

    for (;;) /* The Loop (tm) */
    {
        /* select() for data */
        
        if (child)
        {
            FD_SET(child_sockets[1], &readfds);
            FD_SET(child_sockets[3], &readfds);
        }
        FD_SET(0, &readfds);
        num = select(nfds, &readfds, 0, 0, 0);
        if (num < 0)
        {
            if (errno == EINTR)
                continue;
            perror ("select");
            abort ();
        }

        /* Look for data from our child.
         * We do this before we wait for a died child to make sure that
         * all the data produced by the child is received by us.
         */

        if (child)
        {
            int n = 3;
            do {
                if (FD_ISSET(child_sockets[n], &readfds))
                {
                    do
                        num = read(child_sockets[n], buf+14, MAX_REPLY - 13);
                    while (num == -1 && errno == EINTR);

                    if (num <= 0)
                    {
                        perror("read from spawned child\n");
                    }
                    else
                    {
#if ERQ_DEBUG > 0
                        fprintf(stderr,
                          "%s %d bytes from socket no. %d\n", time_stamp(), num, n);
                        fprintf(stderr,
                          "%s '%.*s'\n", time_stamp(), num, buf+14);
#endif
                        write_32(buf, (num += 14) - 1);
                        write_32(buf+4, ERQ_HANDLE_KEEP_HANDLE);
                        buf[8] = CHILD_LISTEN;
                        memcpy(buf+9, child_handle, 4);
                        buf[13] = n == 1 ? ERQ_STDOUT : ERQ_STDERR;
                        write1(buf, num);
                    }
                }
            } while ((n-=2) > 0);
        }

        /* Check for zombie children and wait for them */

#ifdef HAVE_WAITPID
        while(1) {
#else
        while (childs_terminated != childs_waited_for) {
#endif
            wait_status_t status;
            pid_t pid;

            do {
#ifdef HAVE_WAITPID
                pid = waitpid(-1, &status, WNOHANG);
#else
                pid = wait(&status);
#endif
#ifdef ERESTARTSYS
                if (pid < 0 && errno == ERESTARTSYS)
                    continue;
#endif
            } while (0);

#ifdef HAVE_WAITPID
            if (pid <= 0)
                break;
#else
            if (pid <= 0)
            {
                fprintf(stderr,
                  "%s SIGCLD handler %d times called, but wait not sucessful\n",
                  time_stamp(), childs_terminated - childs_waited_for
                );
                abort();
            }
#endif
            if (pid == child)
            {
                /* Our child terminated - prepare the header for the reply.
                 */
                header[8] = CHILD_FREE;
                if (WIFEXITED(status))
                {
                    header[9] = ERQ_EXITED;
                    header[10] = WEXITSTATUS(status);
                }
                else if (WIFSIGNALED(status))
                {
                    header[9] = ERQ_SIGNALED;
                    header[10] = WTERMSIG(status);
                }
                else
                {
                    header[9] = ERQ_E_UNKNOWN;
                    header[10] = 0;
                }
                child = 0;
                close(child_sockets[1]);
                close(child_sockets[3]);
                FD_CLR(child_sockets[1], &readfds);
                FD_CLR(child_sockets[3], &readfds);
                write_32(header, 10);
                memcpy(header+4, child_handle, 4);
                write1(header, 11);        /* releases handle */
            }
            childs_waited_for++;
        } /* wait for zombies */
        
        if (!FD_ISSET(0, &readfds))
            continue;

        /* Receive the new request from the master ERQ */
        
        do
            num = readn(0, header, 9);
        while (num == -1 && errno == EINTR);

        if (num != 9)
        {
            fprintf(stderr, "%s read %ld, should be 9!\n", time_stamp(), num);
            if (num < 0)
                perror("read");
            break;
        }

        msglen = read_32(header) - 9;
        request = header[8];

        /* ... and the rest of the data */
        
        if (msglen > 0)
        {
            if (msglen > sizeof(buf))
            {
                /* Prevent a buffer overflow */
                fprintf(stderr, "%s ERROR: Read %ld > buffer size %ld\n", time_stamp(), msglen, sizeof(buf));
                num = readn(0, buf, sizeof(buf));

                /* Discard the rest of the input from the channel */
                msglen -= num;
                while (msglen > 0)
                {
                    char tmp[256];
                    num = readn(0, tmp, sizeof(tmp));
                    msglen -= num;
                }

                msglen = sizeof(buf); /* Pretent this was all */
            }
            else
            {
                num = readn(0, buf, msglen);
                if (num != msglen) {
                    fprintf(stderr, "%s Read %ld, should be %ld\n", time_stamp(), num, msglen);
                    break;
                }
            }
        }

        /* What does the ERQ want? */
        
        switch(request)
        {
        case ERQ_RLOOKUP:
          {
            /* Lookup ip -> name */
            
            struct hostent *hp;

            /* handle stays in header[4..7] */
            header[8] = CHILD_FREE;
            memcpy(header+9, buf, 4); /* copy address */
            hp = gethostbyaddr(buf, 4, AF_INET);
            if (!hp && h_errno == TRY_AGAIN)
            {
                /* DNS needs a bit more time */
                sleep(3);
                hp = gethostbyaddr(buf, 4, AF_INET);
            }
            if (!hp && h_errno == TRY_AGAIN)
            {
                /* DNS needs a even more time */
                sleep(7);
                hp = gethostbyaddr(buf, 4, AF_INET);
            }
            
            if (hp)
            {
                /* Send the address and the name */
                msglen = strlen(hp->h_name) + 1;
                write_32(header, msglen + 12);
                write1(header, 13);
                write1((void*)hp->h_name, msglen);
            }
            else
            {
                /* No answer, send just the address */
                write_32(header, 12);
                write1(header, 13);
            }
            break;
          }

#ifdef ERQ_LOOKUP
        case ERQ_LOOKUP:
          {
            /* Lookup name -> ip */
            
            struct hostent *hp;

            /* handle stays in header[4..7] */
            header[8] = CHILD_FREE;
            memcpy(header+9, buf, msglen); /* copy address */
            buf[msglen] = '\0'; /* Make sure the string is terminated */
#if ERQ_DEBUG > 3
            fprintf(stderr, "%s: ERQ_LOOKUP '%s'\n", time_stamp(), buf);
#endif
            hp = gethostbyname(buf);
            if (!hp && h_errno == TRY_AGAIN)
            {
#if ERQ_DEBUG > 2
                 fprintf(stderr, "%s: ERQ_LOOKUP '%s': trying again in 3 sec.\n", time_stamp(), buf);
#endif
                /* DNS needs more time */
                sleep(3);
                hp = gethostbyname(buf);
            }
            if (!hp && h_errno == TRY_AGAIN)
            {
#if ERQ_DEBUG > 2
                 fprintf(stderr, "%s: ERQ_LOOKUP '%s': trying again in 7 sec.\n", time_stamp(), buf);
#endif
                /* DNS needs even more time */
                sleep(7);
                hp = gethostbyname(buf);
            }
            if (hp)
            {
#if ERQ_DEBUG > 3
                 fprintf(stderr, "%s: ERQ_LOOKUP '%s' -> %d.%d.%d.%d\n"
                        , time_stamp(), buf
                        , hp->h_addr[0] & 255
                        , hp->h_addr[1] & 255
                        , hp->h_addr[2] & 255
                        , hp->h_addr[3] & 255
                     );
#endif
                /* Send the name and the address */
                msglen = 4;
                write_32(header, msglen + 8);
                write1(header, 9);
                write1(hp->h_addr, msglen);
            }
            else
            {
                /* No answer, send an empty message */
                write_32(header, 8);
                write1(header, 9);
            }
            break;
          }
#endif

#ifdef ERQ_RLOOKUPV6
            case ERQ_RLOOKUPV6:
              {
                int i;
                char *mbuff;
                struct addrinfo req, *ai, *ai2;

                /* handle stays in header[4..7] */
                header[8] = CHILD_FREE;
                buf[msglen] = 0;

                memset(&req, 0, sizeof(struct addrinfo));
                req.ai_family = AF_INET6;
                req.ai_flags = AI_CANONNAME;

                i = getaddrinfo(buf, NULL, &req, &ai);

                if (!i)
                    for (ai2 = ai
                        ; ai2 && (ai2->ai_family != AF_INET)
                              && (ai2->ai_family != AF_INET6)
                        ; ai2 = ai2->ai_next) /* NOOP */;

                if (!i && ai2 &&ai2->ai_canonname)
                {
                    mbuff = malloc(strlen(ai2->ai_canonname)+strlen(buf)+2);
                    if (!mbuff)
                    {
                        perror("malloc");
                        exit(errno);
                    }
                    strcpy(mbuff, buf);
                    strcat(mbuff, " ");
                    strcat(mbuff, ai2->ai_canonname);
                    msglen = strlen(mbuff) + 1;
                }
                else
                {
                    mbuff = malloc(strlen("invalid-format")+strlen(buf)+1);
                    if (!mbuff)
                    {
                        perror("malloc");
                        exit(errno);
                    }
                    strcpy(mbuff, "invalid-format");
                    msglen = strlen(mbuff) + 1;
                }

                write_32(header, msglen + 8);
                write1(header, 9);
                write1(mbuff, msglen);
                free(mbuff);
                if (!i)
                    freeaddrinfo(ai);
                break;
              }
#endif /* ERQ_RLOOKUPV6 */

        case ERQ_EXECUTE:
          {
            /* Execute a program, wait for its termination and
             * return the exit status.
             */
             
            pid_t pid1, pid2;

            header[8] = CHILD_FREE;
            if ((pid1 = execute(buf, msglen, &header[9], 0)))
            {
                wait_status_t status;

                do {
                    pid2 = wait(&status);
#ifdef ERESTARTSYS
                    if (pid2 < 0 && errno == ERESTARTSYS)
                        continue;
#endif
                    if (pid2 > 0)
                        childs_waited_for++;
                } while (pid2 >= 1 && pid2 != pid1);

                if (pid2 < 1) {
                    header[9] = ERQ_E_NOTFOUND;
                } else if (WIFEXITED(status)) {
                    header[9] = ERQ_OK;
                    header[10] = WEXITSTATUS(status);
                } else if (WIFSIGNALED(status)) {
                    header[9] = ERQ_SIGNALED;
                    header[10] = WTERMSIG(status);
                } else {
                    header[9] = ERQ_E_UNKNOWN;
                }
            }
            write_32(header, 10);
            write1(header, 11);
            break;
          }

        case ERQ_FORK:
            /* Fork off a process and let it run */

            header[8] = CHILD_FREE;
            if (execute(buf, msglen, &header[9], 0)) {
                header[9] = ERQ_OK;
            }
            write_32(header, 10);
            write1(header, 11);
            break;

        case ERQ_SPAWN:

            /* Similar to ERQ_FORK, but we send back additional
             * information to the ERQ so that further communcation
             * is possible.
             */
             
#if ERQ_DEBUG > 0
            if (child) {
                fprintf(stderr, "%s ERQ_SPAWN: busy\n", time_stamp());
                abort();
            }
#endif

            if ((child = execute(buf, msglen, &header[9], child_sockets)))
            {
                /* We want to avoid race conditions when a stale ticket
                 * accesses a fresh process (in the unlikely event that
                 * the rnd part is equal). The below formula ensures that
                 * more than 71 minutes pass before a stale ticket can
                 * refer to a new process (assuming gettimeofday get_seed())
                 * It also makes ticket guessing harder.
                 */
                ticket.s.seq += ((get_seed() - ticket.s.seq) & 0x7fffffff) + 1;
                ticket.s.rnd = get_ticket();
                if (child_sockets[1] >= nfds)
                    nfds = child_sockets[1] + 1;
                if (child_sockets[3] >= nfds)
                    nfds = child_sockets[3] + 1;
                memcpy(child_handle, header+4, 4);
                write_32(buf, 17 + sizeof ticket);
                write_32(buf+4, ERQ_HANDLE_KEEP_HANDLE);
                buf[8] = CHILD_LISTEN;
                memcpy(buf+9, header+4, 4);
                buf[13] = ERQ_OK;
                write_32(buf+14, server_num);
                memcpy(buf+18, ticket.c, sizeof ticket);
                write1(buf, 18 + sizeof ticket);
            }
            else
            {
                header[8] = CHILD_FREE;
                write_32(header, 10);
                write1(header, 11);
            }
            break;

        case ERQ_SEND:
            /* Send some data to our child process */

            if (msglen < sizeof ticket + 1)
                goto bad_request;
            if (!child)
                goto no_child;

            header[8] = CHILD_LISTEN;
            if (memcmp(buf, ticket.c, sizeof ticket))
                goto bad_ticket;

            msglen -= sizeof ticket;
            do
                num = write(child_sockets[1], buf + sizeof ticket, msglen);
            while (num == -1 && errno == EINTR);

            if (num == msglen) {
                header[9] = ERQ_OK;
                write_32(header, 9);
                write1(header, 10);
            } else {
                if (num >= 0) {
                    header[9] = ERQ_E_INCOMPLETE;
                    write_32(header+10, num);
                } else {
                    write_32(header+10, errno);
                    if (0
#ifdef EAGAIN
                        || errno == EAGAIN
#endif
#ifdef EWOULDBLOCK
                        || errno == EWOULDBLOCK
#endif
                        )
                    {
                        header[9] = ERQ_E_WOULDBLOCK;
                    } else if (errno == EPIPE) {
                        header[9] = ERQ_E_PIPE;
                    } else {
                        header[9] = ERQ_E_UNKNOWN;
                    }
                }
                write_32(header, 13);
                write1(header, 14);
            }
            break;

        case ERQ_KILL:

            /* Send the child a signal */

            if (msglen != sizeof ticket + 4 && msglen != sizeof ticket)
                goto bad_request;
            if (!child) {
no_child:
                /* could happen due to a race condition */
#if ERQ_DEBUG > 0
                fprintf(stderr, "%s ERQ_SEND/ERQ_KILL: No child\n", time_stamp());
#endif
                header[8] = CHILD_FREE;
                goto notify_bad_ticket;
            }

            header[8] = CHILD_LISTEN;
            if (memcmp(buf, ticket.c, sizeof ticket)) {
bad_ticket:
#if ERQ_DEBUG > 0
                fprintf(stderr, "%s ticket.s.rnd: %x vs. %x\n",
                  time_stamp(), ((struct ticket_s *)buf)->rnd, ticket.s.rnd);
                fprintf(stderr, "%s ticket.s.seq: %x vs. %x\n",
                  time_stamp(), ((struct ticket_s *)buf)->seq, ticket.s.seq);
#endif
notify_bad_ticket:
                header[9] = ERQ_E_TICKET;
            } else {
                int sig;

                sig = msglen >= 4 ? read_32(buf+sizeof ticket) : SIGKILL;
#if ERQ_DEBUG > 0
                fprintf(stderr, "%s len: %d sig: %d\n", time_stamp(), msglen, sig);
#endif
                if (sig >= 0)
                    sig = kill(child, sig);
                header[9] = sig < 0 ? ERQ_E_ILLEGAL : ERQ_OK;
#if ERQ_DEBUG > 0
                if (sig < 0)
                    perror("kill");
#endif
            }
            write_32(header, 9);
            write1(header, 10);
            break;

#ifdef ERQ_AUTH
        case ERQ_AUTH:
          {
            /* Connect to the authd on the remote system, and send
             * its answer back the the master ERQ resp. the driver.
             */
            struct sockaddr_in server_addr, *remote;
            int s;
            long mud_port;
            long num, total;

            if (msglen != sizeof (struct sockaddr_in) + 4)
                goto bad_request;
            header[8] = CHILD_FREE;

            remote = (struct sockaddr_in *)buf;
            server_addr.sin_family = remote->sin_family;
            server_addr.sin_port = htons(AUTH_PORT);
            server_addr.sin_addr = remote->sin_addr;
            s = socket(remote->sin_family, SOCK_STREAM, 0);
            if (s < 0) {
                perror("socket");
                goto die;
            }
            if (
              connect(s, (struct sockaddr *)&server_addr, sizeof server_addr)
                < 0)
            {
                perror("connect");
                write_32(header, 8);
                write1(header, 9);
                break;
            }
            mud_port = read_32(buf + msglen - 4);
            sprintf(buf, "%d,%ld\r\n", ntohs(remote->sin_port), mud_port);
            writen(s, buf, strlen(buf));
            total = 0;
            do {
                do {
                    num = read(s, buf+total, MAX_REPLY - 8 - total);
                } while (num == -1 && errno == EINTR);
                if (num <= 0) {
                    if (buf[total-2] == '\r' && buf[total-1] == '\n')
                        break;
                    perror("read (auth)");
                    goto die;
                }
                total += num;
            } while (num > 0 && total < sizeof buf);
            close(s);
            write_32(header, 8 + total);
            write1(header, 9);
            write1(buf, total);
            break;
          }
#endif /* ERQ_AUTH */

        default:
bad_request:
            fprintf(stderr, "%s Bad request %d\n", time_stamp(), request);
            fprintf(stderr, "%s %x %x %x %x %x %x %x %x %x\n"
                          , time_stamp()
                          , header[0],header[1],header[2],header[3]
                          ,header[4],header[5],header[6],header[7]
                          ,header[8]);
            fprintf(stderr, "%s %c %c %c %c %c %c %c %c %c\n"
                          , time_stamp()
                          , header[0],header[1],header[2],header[3]
                          ,header[4],header[5],header[6],header[7]
                          ,header[8]);
            write_32(header, 8);
            header[8] = child ? CHILD_LISTEN : CHILD_FREE;
            write1(header, 9);
            break;
        }
    } /* for(;;) */

die:
    /* We terminate - and also the child if there is one */

    if (child)
    {
        if (kill(child, SIGTERM))
            perror("kill");
        sleep(1);
        kill(child, SIGKILL);
        /* make an attempt to release the handle, but without error
         * checking, because things are likely to be screwed up.
         */
        write_32(header, 10);
        memcpy(header+4, child_handle, 4);
        header[9] = ERQ_E_UNKNOWN;
        header[10] = 0;
        write(1, header, 11);        /* releases handle */
    }

    fprintf(stderr, "%s Subserver giving up.\n", time_stamp());
    exit(1);
} /* start_subserver() */

/*-------------------------------------------------------------------------*/
int
main (int argc, char **argv)

/* Main program of the ERQ. It contains the main loop waiting for requests.
 */

{
    char header[32];
    long num;
    long msglen;
    int subserver;
    int s;
    int num_ready;
    struct timeval timeout;
    child_t *child, *next_child;
    union ticket_u ticket;

    /* Print information about this daemon to help debugging */
    {
        fprintf(stderr, "%s Amylaar ERQ %s: Path '%s', debuglevel %d\n"
                      , time_stamp(), __DATE__, argv[0], ERQ_DEBUG
                );
    }

    /* Quick and dirty commandline parser */
    {
        int is_forked = 0;
        int i;

        for (i = 1; i < argc; i++)
        {
            if (!strcmp(argv[i], "--forked"))
                is_forked = 1;
            else if (!strcmp(argv[i], "--execdir"))
            {
                if (i+1 >= argc)
                {
                    fprintf(stderr, "%s Missing value for --execdir.\n"
                                  , time_stamp());
                    goto die;
                }
                erq_dir = argv[i+1];
                i++;
            }
            else
            {
                fprintf(stderr, "%s Unknown argument '%s'.\n"
                              , time_stamp(), argv[i]);
                goto die;
            }
        }
        /* Check if we have been forked off the driver */
        if (is_forked)
        {
            write1("1", 1); /* indicate sucessful fork/execl */
        }
        else
        {
            fprintf(stderr, "%s dynamic attachement unimplemented\n", time_stamp());
            goto die;
        }
    }

#if defined(DETACH) && defined(TIOCNOTTY)
    /* Detach from console */
    s = open("/dev/tty", O_RDWR);
    if (s >= 0)
    {
        /* We supply header as a 'dummy' argument in case typing is
         * too strict
         */
        ioctl(s, TIOCNOTTY, header);
        close(s);
    }
#endif

    /* Initialize variables */

    randomize_tickets(get_seed());
    FD_ZERO(&current_fds);
    FD_ZERO(&current_fds2);
    FD_SET(1, &current_fds);
    (void)signal(SIGCLD, (RETSIGTYPE(*)())count_sigcld);

    /* The main loop */
    
    for (subserver = 0;;)
    {
        int still_corked; /* Determines the select() wait time */

        /* Scan the list of TCP children and check which sent data
         * to the driver more than 3 seconds ago. Those are added
         * back to current_fds.
         * Existence of children which sent data less than 3 seconds
         * ago are flagged as 'still_corked'.
         */
        still_corked = 0;
        for (next_child = tcp_sockets; NULL != (child = next_child); )
        {

            next_child = child->next_all;

            /* Uncork the bottle */
            if (child->u.s.bytes_recv)
            {
                if (child->u.s.last_recv + 3 < time(NULL))
                {
#if ERQ_DEBUG > 1
                    fprintf(stderr,"%s Uncorking child\n", time_stamp());
#endif
                    child->u.s.bytes_recv = 0;
                    FD_SET(child->socket, &current_fds);
                }
                else
                    still_corked = 1;
            }
        } /* for(tcpsockets) */
#if ERQ_DEBUG > 0
        fprintf(stderr,"%s still_corked = %d\n", time_stamp(), still_corked);
#endif

        /* select() for data.
         */
        readfds = current_fds;
        writefds = current_fds2;
        timeout.tv_sec = (still_corked ? 3 : TIME_TO_CHECK_CHILDS);
        timeout.tv_usec = 0;

#if ERQ_DEBUG > 0
        fprintf(stderr, "%s calling select (nfds = %d)\n", time_stamp(), nfds);
#endif
        num_ready = select(nfds, &readfds, &writefds, 0, &timeout);
#if ERQ_DEBUG > 0
        fprintf(stderr, "%s select returns %d\n", time_stamp(), num_ready);
#endif
        if (num_ready < 0 && errno != EINTR)
        {
            perror ("select");
            abort ();
        }
        current_time = time(NULL);

        /* Kill off idle free children */
        
        {
            time_t expired;

            expired = current_time - CHILD_EXPIRE;
            for (next_child = free_childs; (child = next_child); )
            {
                next_child = child->next_free;
                if (child->u.c.last_used > expired)
                    continue;
#if ERQ_DEBUG > 0
                fprintf(stderr, "%s Max child idle time expired.\n", time_stamp());
#endif
                kill_child(child);
            }
        }

        /* Check for zombie children and wait for them */

#ifdef HAVE_WAITPID
        while(1) {
#else
        while (childs_terminated != childs_waited_for) {
#endif
            wait_status_t status;
            pid_t pid;

            do {
#ifdef HAVE_WAITPID
                pid = waitpid(-1, &status, WNOHANG);
#else
                pid = wait(&status);
#endif
#ifdef ERESTARTSYS
                if (pid < 0 && errno == ERESTARTSYS)
                    continue;
#endif
            } while (0);

            if (pid <= 0)
                break;

            for (child = all_childs; child; child = child->next_all)
            {
                if (child->u.c.pid == pid)
                {
                    dispose_child(child);
                    break;
                }
            }
            childs_waited_for++;
        }

        if (num_ready > 0)
        {
            /* Check for data from the subservers for the gamedriver */

            for (next_child = all_childs; (child = next_child); )
            {
                long replylen;
                char replyheader[12];
                char replybuf[ERQ_BUFSIZE];

                next_child = child->next_all;

                s = child->socket;
                if (!FD_ISSET(s, &readfds))
                    continue;
#if ERQ_DEBUG > 0
                fprintf(stderr, "%s query child %d\n", time_stamp(), child - &childs[0]);
#endif
                /* Read the standard erq header plus the one-byte
                 * child state.
                 */
                num = readn(s, replyheader, 9);
                if (num != 9)
                {
                    fprintf(stderr, "%s read %ld, should be 9.\n", time_stamp(), num);
                    kill_child(child);
                    continue;
                }

                /* If there is more data, read the rest of message */
                replylen = read_32(replyheader) - 8;
                if (replylen > 0)
                {
                    if (replylen > sizeof replybuf)
                    {
                        fprintf(stderr, "%s Too long reply.\n", time_stamp());
                        kill_child(child);
                        continue;
                    }
                    num = readn(s, replybuf, replylen);
                    if (num != replylen)
                    {
                        fprintf(stderr, "%s read %ld, should be %ld\n"
                                      , time_stamp(), num, replylen);
                        kill_child(child);
                        continue;
                    }
                }

                /* Pass the message received (sans the state byte) on
                 * to the gamedriver.
                 */
                write1(replyheader, 8);
                write1(replybuf, replylen);
                 /* We can't simply test for ERQ_HANDLE_KEEP_HANDLE, because a
                  * subserver can have several handles at a time.
                  */
                if ((child->state = replyheader[8]) == CHILD_FREE)
                {
                    child->u.c.last_used = current_time;
                    child->next_free = free_childs;
                    free_childs = child;
                }
            }
#if ERQ_DEBUG > 0
            fprintf(stderr, "%s queried all children\n", time_stamp());
#endif

#ifdef ERQ_OPEN_UDP
            /* Check for data received on UDP sockets for the gamedriver */
            
            for (next_child = udp_sockets; (child = next_child); )
            {
                length_t length;
                long replylen;
                char replyheader[16];
                char replybuf[ERQ_BUFSIZE];
                struct sockaddr_in addr;
                int cnt;

                next_child = child->next_all;
                s = child->socket;
                if (!FD_ISSET(s, &readfds))
                    continue;

                /* Receive the UDP message */
                length = sizeof addr;
                cnt = recvfrom( s, replybuf, sizeof(replybuf), 0
                              , (struct sockaddr *)&addr, &length);

                /* Compose and send the UDP-erq message to the driver */
                replylen = cnt + 19;
                if (replylen > MAX_REPLY)
                    replylen = MAX_REPLY;
                write_32(replyheader, replylen);
                write_32(replyheader+4, ERQ_HANDLE_KEEP_HANDLE);
                memcpy(replyheader+8, child->u.s.handle, 4);
                replyheader[12] = ERQ_STDOUT;
                write1(replyheader, 13);
                write1(&addr.sin_addr.s_addr, 4);
                write1(&addr.sin_port, 2);
                write1(replybuf, replylen - 19);
            }
#endif /* ERQ_OPEN_UDP */

#ifdef ERQ_OPEN_TCP

            /* Exchange data between the TCP sockets and the driver */
            
            for (next_child = tcp_sockets; (child = next_child); )
            {
                int length;
                long replylen;
                char replyheader[30];
                char replybuf[ERQ_BUFSIZE];
                int cnt;

                next_child = child->next_all;
                s = child->socket;

                if (FD_ISSET(s, &writefds))
                {
                    /* Socket is ready for writing - nevertheless check
                     * if there is data pending.
                     */
                    FD_CLR(s, &current_fds2);
                    cnt = recv(s, replybuf, 1, MSG_PEEK);

                    if (cnt < 0 && (errno != EWOULDBLOCK && errno != EAGAIN))
                    {
                        /* TCP connection died - inform the driver and
                         * get rid of the child.
                         */

                        replyheader[8] = ERQ_E_UNKNOWN;
                        replyheader[9] = errno;
                        replylen = 10;
                        write_32(replyheader, replylen);
                        memcpy(replyheader+4, child->u.s.handle, 4);
                        write1(replyheader, replylen); /* ..and release handle */
                        FD_CLR(child->socket,&current_fds);
                        FD_CLR(child->socket,&current_fds2);
                        close(child->socket);
                        free_socket_child(child,&tcp_sockets);
                        break;
                    }

                    /* Inform the driver that there is data pending */
                    
                    replyheader[12] = ERQ_OK;
                    write_32(replyheader, 17+sizeof(child->u.s.ticket));
                    write_32(replyheader+4, ERQ_HANDLE_KEEP_HANDLE);
                    memcpy(replyheader+8, child->u.s.handle, 4);
                    write_32(replyheader+13, child - &childs[0]);
                    memcpy(replyheader+17, child->u.s.ticket.c
                                         , sizeof(child->u.s.ticket));
                    write1(replyheader, 17+sizeof(child->u.s.ticket));
                    break;
                }
                else if (!FD_ISSET(s, &readfds))
                    continue;

                /* Read the data */

                cnt = read(s, replybuf, MAX_REPLY-100);
                FD_CLR(s,&readfds);
                if (cnt <= 0)
                {
                    /* No data there - EOF or error */
                    
                    if (!cnt)
                    {
                        replyheader[8] = ERQ_EXITED;
                        replylen = 9;
                    }
                    else
                    {
                        replyheader[8] = ERQ_E_UNKNOWN;
                        replyheader[9] = errno;
                        replylen = 10;
                    }
                    write_32(replyheader, replylen);
                    memcpy(replyheader+4, child->u.s.handle, 4);
                    write1(replyheader, replylen); /* ..and release handle */
                    FD_CLR(child->socket,&current_fds);
                    FD_CLR(child->socket,&current_fds2);
                    close(child->socket);
                    free_socket_child(child,&tcp_sockets);
                    break;
                }
                else
                {
                    /* We got data - send it to the driver (but make
                     * sure not to overrun it).
                     */
                    
                    length = cnt;
#if 0
                    /* Update the "data pending" strategy */
                    child->u.s.last_recv = time(NULL);
                    child->u.s.bytes_recv += cnt;
#endif

                    if (child->u.s.bytes_recv > 4096)
                    {
                        /* Cork the bottle. Let the MUD swallow first */
                        FD_CLR(s, &current_fds);
#if ERQ_DEBUG > 1
                        fprintf(stderr,"%s Corking child.\n", time_stamp());
#endif
                    }

                    /* Compose the message for the driver and send it */

                    replylen = length + 13;
                    if (replylen > MAX_REPLY)
                        replylen = MAX_REPLY;
                    write_32(replyheader, replylen);
                    write_32(replyheader+4, ERQ_HANDLE_KEEP_HANDLE);
                    memcpy(replyheader+8, child->u.s.handle, 4);
                    replyheader[12] = ERQ_STDOUT;
                    write1(replyheader, 13);
                    write1(replybuf, replylen - 13);
                }
            }
#endif /* ERQ_OPEN_TCP */

#ifdef ERQ_LISTEN
            for (next_child = accept_sockets; (child = next_child); )
            {
                int length;
                long replylen;
                char replyheader[30];
                char replybuf[ERQ_BUFSIZE];
                int cnt;

                next_child = child->next_all;
                s = child->socket;
                if (!FD_ISSET(s, &readfds))
                    continue;
                FD_CLR(s,&current_fds);

                /* Connection pending - inform the driver */

                replylen = 13;
                write_32(replyheader, replylen);
                write_32(replyheader+4, ERQ_HANDLE_KEEP_HANDLE);
                memcpy(replyheader+8, child->u.s.handle, 4);
                replyheader[12] = ERQ_STDOUT;
                write1(replyheader, replylen);

            }
#endif /* ERQ_LISTEN */
        } /* if (num_ready) */

        /* We needed to send data to a subserver in the previous loop,
         * but didn't get one then. Now try again.
         */
        if (subserver < 0)
        {
            subserver = get_subserver();
            if (subserver < 0)
                continue;
            writen(subserver, header, 9);
            writen(subserver, buf, msglen);
        }

        if (num_ready > 0 && FD_ISSET(1, &readfds))
        {
            /* TODO: We read from 0 when 1 is ready? */

            /* Read the erq message incl. request */
            num = readn(0, header, 9);
            if (num != 9)
            {
                fprintf(stderr, "%s Read %ld, should be 9!\n"
                              , time_stamp(), num);
                if (num < 0)
                    perror("read");
                break;
            }
#if ERQ_DEBUG > 0
            fprintf(stderr, "%s read command %d\n", time_stamp(), header[8]);
#endif

            /* Read the rest of the message */

            msglen = read_32(header) - 9;
            if (msglen > 0)
            {
                num = readn(0, buf, msglen);
                if (num != msglen) {
                    fprintf(stderr, "%s Read %ld, should be %ld\n"
                                  , time_stamp(), num, msglen);
                    break;
                }
            }

            /* Switch on the request received */
            switch(header[8])
            {

                                      /* ----- Send a signal or data ----- */
            case ERQ_SEND:  
            case ERQ_KILL:
              {
                long n;

                /* Check the ticket - the next_child_index at the time
                 * of creation.
                 */
                n = read_32(buf);
                if ((unsigned long)n >= (unsigned long)next_child_index)
                    goto bad_ticket;

                child = &childs[n];
                switch(child->state)
                {
                    struct child_s **listp;

            bad_ticket:
                default:
                  {
#if ERQ_DEBUG > 0
                    fprintf(stderr, "%s Ticket rejected n: 0x%x nxt: 0x%x state: %d\n",
                        time_stamp(), n, next_child_index,
                        (unsigned long)n >= (unsigned long)next_child_index ?
                          0 : childs[n].state);
#endif
                    subserver = 0; /* ready for new commands */
                    write_32(header, 9);
                    header[8] = ERQ_E_TICKET;
                    write1(header, 9);
                    break;
                  }

                case CHILD_LISTEN:
                    /* Pass on the message to the listening child */
                    subserver = child->socket;
                    write_32(header, 9 + msglen - 4);
                    writen(subserver, header, 9);
                    writen(subserver, buf+4, msglen-4);
                    break;

#ifdef ERQ_LISTEN
                case CHILD_ACCEPT:
                    /* Can only KILL accept children, not SEND data */

                    if (header[8] == ERQ_SEND)
                        goto bad_ticket;
                    listp = &accept_sockets;
                    goto handle_send_on_socket;

                case CHILD_TCP:
                    listp = &tcp_sockets;
                    goto handle_send_on_socket;
#endif
#ifdef ERQ_OPEN_UDP
                case CHILD_UDP:
                    listp = &udp_sockets;
#endif
#if defined(ERQ_OPEN_UDP) || defined(ERQ_LISTEN)
            handle_send_on_socket:

                    /* Check the rest of the ticket */

                    if (msglen < sizeof(union ticket_u)
                     || memcmp(buf+4, child->u.s.ticket.c,
                          sizeof(union ticket_u)))
                    {
#if ERQ_DEBUG > 0
                        fprintf(stderr,"%s Ticket mismatch. (%d, %d)\n", time_stamp(), msglen,sizeof(union ticket_u));
#endif
                        goto bad_ticket;
                    }
                    msglen -= sizeof(union ticket_u);

                    subserver = 0; /* ready for new commands */
                    write_32(header, 9); /* "msg_len" of the answer */
                    if (header[8] == ERQ_SEND)
                    {
                        while(1){
                            if (child->state == CHILD_UDP)
                            {
                                struct sockaddr_in host_ip_addr;

                                if (msglen < 6)
                                {
                                    header[8] = ERQ_E_ARGLENGTH;
                                    break;
                                }
                                memcpy(&host_ip_addr.sin_addr,
                                  buf+sizeof(union ticket_u)+4, 4);
                                host_ip_addr.sin_family = AF_INET;
                                memcpy(&host_ip_addr.sin_port,
                                  buf+sizeof(union ticket_u)+8, 2);
                                num = sendto(child->socket,
                                        buf+sizeof(union ticket_u)+10,
                                        msglen-10, 0,
                                        (struct sockaddr *)&host_ip_addr,
                                        sizeof(host_ip_addr));
                            }
                            else
                            {
                                /* Program or TCP socket */
                                num = write(child->socket,
                                        buf+sizeof(union ticket_u)+4,
                                        msglen-4);
                            }

                            if (num != msglen
				- (child->state == CHILD_UDP ? 10 : 4))
                            {
                                /* Prepare the error message to send back */
                                if (num < 0)
                                {
                                    switch(errno) {
                                      case EWOULDBLOCK:
#if EAGAIN != EWOULDBLOCK
                                      case EAGAIN:
#endif
                                        header[3] = 9;
                                        header[8] = ERQ_E_WOULDBLOCK;
                                        break;
                                      case EPIPE:
                                        header[3] = 9;
                                        header[8] = ERQ_E_PIPE;
                                        break;
                                      case EINTR:
                                        continue;
                                      default:
                                        header[3] = 10;
                                        header[8] = ERQ_E_UNKNOWN;
                                        header[9] = errno;
                                        break;
                                    }
                                }
                                else
                                {
                                    header[3] = 13;
                                    header[8] = ERQ_E_INCOMPLETE;
                                    write_32(header+9, num);
                                }
                            }
                            else
                            {
                                /* Send back "ok" */
                                header[3] = 9;
                                header[8] = ERQ_OK;
                            }
                            break;
                        }
                    }
                    else
                    {
                        /* header[8] == ERQ_KILL */
                        FD_CLR(child->socket,&current_fds);
                        FD_CLR(child->socket,&current_fds2);
                        if (child->state == CHILD_ACCEPT)
                            shutdown(child->socket,0);
                        close(child->socket);
                        free_socket_child(child,listp);
                        write_32(header,9);
                        header[8] = ERQ_OK;
                    }
                    write1(header, header[3]);
                    break;
#endif /* defined(ERQ_OPEN_UDP) || defined(ERQ_LISTEN) */
                }
                break; /* end of ERQ_KILL / ERQ_SEND */
              }

            default:
              {
                /* Non-communication request: pass it on to
                 * a new subserver.
                 */
                subserver = get_subserver();
                if (subserver >= 0)
                {
                    writen(subserver, header, 9);
                    writen(subserver, buf, msglen);
                }
              }
              break;

#ifdef ERQ_OPEN_UDP
            case ERQ_OPEN_UDP:
              {
                /* Open a UDP socket */
                
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct sockaddr_in host_ip_addr;
                    int tmp;

                    if (msglen != 2)
                    {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }
                    host_ip_addr.sin_addr.s_addr = INADDR_ANY;
                    host_ip_addr.sin_family = AF_INET;
                    memcpy(&host_ip_addr.sin_port, buf, 2);
                    if (!free_socket_childs())
                    {
                        header[8] = ERQ_E_NSLOTS;
                        header[9] = MAX_CHILDS;
                        break;
                    }
                    s = socket(AF_INET, SOCK_DGRAM, 0);
                    if (s < 0) {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    tmp = 1;
                    if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR,
                        (char *) &tmp, sizeof (tmp)) < 0)
                    {
                        close(s);
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if (bind(s, (struct sockaddr *)&host_ip_addr,
                        sizeof host_ip_addr) == -1)
                    {
                        close(s);
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }

                    /* We got the socket, now make the child */

                    child = get_socket_child();
                    child->socket = s;
                    FD_SET(child->socket,&current_fds);
                    child->state = CHILD_UDP;
                    child->next_all = udp_sockets;
                    udp_sockets = child;
                    ticket.s.seq +=
                        ((get_seed() - ticket.s.seq) & 0x7fffffff) + 1;
                    ticket.s.rnd = get_ticket();
                    if (s >= nfds)
                        nfds = s + 1;
                    memcpy(child->u.s.handle, header+4, 4);
                    memcpy(child->u.s.ticket.c, ticket.c, sizeof ticket);

                    /* Compose the answer to the driver */
                    
                    header[3] = 17 + sizeof ticket;
                    memcpy(header+8, header+4, 4);
                    write_32(header+4, ERQ_HANDLE_KEEP_HANDLE);
                    header[12] = ERQ_OK;
                    write_32(header+13, child - &childs[0]);
                    memcpy(header+17, ticket.c, sizeof ticket);
                } while(0);
                write1(header, header[3]);
              }
              break;
#endif /* ERQ_OPEN_UDP */

#ifdef ERQ_OPEN_TCP
            case ERQ_OPEN_TCP:
              {
                /* Open a TCP socket and connect it to a given
                 * address.
                 */
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct sockaddr_in host_ip_addr;
                    int tmp;

                    if (msglen != 6)
                    {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }
                    host_ip_addr.sin_family = AF_INET;
                    memcpy(&host_ip_addr.sin_port, buf+4, 2);
                    memcpy(&host_ip_addr.sin_addr.s_addr, buf, 4);

                    if (!free_socket_childs())
                    {
                        header[8] = ERQ_E_NSLOTS;
                        header[9] = MAX_CHILDS;
                        break;
                    }
                    s = socket(AF_INET, SOCK_STREAM, 0);
                    if (s < 0)
                    {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if ((tmp = fcntl(s,F_GETFL,0)) < 0)
                    {
                        fprintf(stderr,"%s fnctl 1\n", time_stamp());
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }

                    if ((tmp = fcntl(s,F_SETFL,tmp | O_NDELAY)) < 0)
                    {
                        fprintf(stderr,"%s fnctl 2\n", time_stamp());
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }

                    tmp = 1;
                    if (connect(s, (struct sockaddr *)&host_ip_addr,
                        sizeof host_ip_addr) == -1)
                    {
                        if(errno != EINPROGRESS)
                        {
                            header[8] = ERQ_E_UNKNOWN;
                            header[9] = errno;
                            close(s);
                            break;
                        }
                    }

                    /* Got the socket, now create the child */
                    
                    child = get_socket_child();
                    child->socket = s;
                    FD_SET(child->socket,&current_fds);
                    FD_SET(child->socket,&current_fds2);
                    child->state = CHILD_TCP;
                    child->u.s.bytes_recv = 0;
                    child->next_all = tcp_sockets;
                    tcp_sockets = child;
                    ticket.s.seq +=
                        ((get_seed() - ticket.s.seq) & 0x7fffffff) + 1;
                    ticket.s.rnd = get_ticket();
                    if (s >= nfds)
                        nfds = s + 1;
                    memcpy(child->u.s.handle, header+4, 4);
                    memcpy(child->u.s.ticket.c, ticket.c, sizeof ticket);

                    /* Compose the answer to the driver */
                    
                    header[3] = 17 + sizeof ticket;
                    memcpy(header+8, header+4, 4);
                    write_32(header+4, ERQ_HANDLE_KEEP_HANDLE);
                    header[12] = ERQ_OK;
                    header[3] = 0;
                    write_32(header+13, child - &childs[0]);
                    memcpy(header+17, ticket.c, sizeof ticket);
                } while(0);
                if(header[3]) write1(header, header[3]);
              }
              break;
#endif /* ERQ_OPEN_TCP */

#ifdef ERQ_LISTEN
            case ERQ_LISTEN:
              {
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct sockaddr_in host_ip_addr;
                    int tmp;

                    if (msglen != 2)
                    {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }
                    host_ip_addr.sin_family = AF_INET;
                    host_ip_addr.sin_addr.s_addr = INADDR_ANY;
                    memcpy(&host_ip_addr.sin_port, buf, 2);

                    if (!free_socket_childs())
                    {
                        header[8] = ERQ_E_NSLOTS;
                        header[9] = MAX_CHILDS;
                        break;
                    }
                    s = socket(AF_INET, SOCK_STREAM, 0);
                    if (s < 0)
                    {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if((tmp = fcntl(s,F_GETFL,0)) < 0)
                    {
                        fprintf(stderr,"%s fnctl 1\n", time_stamp());
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }

                    if((tmp = fcntl(s,F_SETFL,tmp | O_NDELAY)) < 0)
                    {
                        fprintf(stderr,"%s fnctl 2\n", time_stamp());
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    tmp = 1;
                    if (bind(s, (struct sockaddr *)&host_ip_addr,
                        sizeof host_ip_addr) == -1)
                    {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if (listen(s,2) < 0)
                    {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }

                    /* Got the socket, now create the child */

                    child = get_socket_child();
                    child->socket = s;
                    FD_SET(child->socket,&current_fds);
                    child->state = CHILD_ACCEPT;
                    child->next_all = accept_sockets;
                    accept_sockets = child;
                    ticket.s.seq +=
                        ((get_seed() - ticket.s.seq) & 0x7fffffff) + 1;
                    ticket.s.rnd = get_ticket();
                    if (s >= nfds)
                        nfds = s + 1;
                    memcpy(child->u.s.handle, header+4, 4);
                    memcpy(child->u.s.ticket.c, ticket.c, sizeof ticket);

                    /* Compose the message for the driver */

                    header[3] = 17 + sizeof ticket;
                    memcpy(header+8, header+4, 4);
                    write_32(header+4, ERQ_HANDLE_KEEP_HANDLE);
                    header[12] = ERQ_OK;
                    write_32(header+13, child - &childs[0]);
                    memcpy(header+17, ticket.c, sizeof ticket);
                } while(0);
                if(header[3]) write1(header, header[3]);
              }
              break;
#endif /* ERQ_LISTEN */

#ifdef ERQ_ACCEPT
            case ERQ_ACCEPT:
              {

                /* Accept a connection from a socket */
                
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct child_s *parent;
                    struct sockaddr_in host_ip_addr;
                    long tmp2;
                    int tmp;
                    length_t len;
                    int n;

                    if (msglen != sizeof(union ticket_u) + 4)
                    {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }

                    n = read_32(buf);
                    if((unsigned long) n >= (unsigned long) next_child_index)
                    {
                        fprintf(stderr,"%s given: %d, nxt: %d\n", time_stamp(), n,next_child_index);
                        goto accept_bad_ticket;
                    }

                    parent = &childs[n];
                    if(parent->state != CHILD_ACCEPT)
                    {
                        fprintf(stderr,"%s State is %d, should be %d!\n", time_stamp(), parent->state,CHILD_ACCEPT);
                        goto accept_bad_ticket;
                    }

                    if(memcmp(buf+4,parent->u.s.ticket.c,sizeof(union ticket_u)))
                    {
                        accept_bad_ticket:
                        fprintf(stderr,"%s Accept: Ticket mismatch.\n", time_stamp());
                        header[8] = ERQ_E_TICKET;
                        header[9] = 0;
                        break;
                    }

                    if (!free_socket_childs())
                    {
                        header[8] = ERQ_E_NSLOTS;
                        header[9] = MAX_CHILDS;
                        break;
                    }

                    len = sizeof(host_ip_addr);
                    s = accept( parent->socket
                              , (struct sockaddr *) &host_ip_addr
                              , &len);
                    if (s < 0)
                    {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if((tmp = fcntl(s,F_GETFL,0)) < 0)
                    {
                        fprintf(stderr,"%s fnctl 1\n", time_stamp());
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }

                    if((tmp = fcntl(s,F_SETFL,tmp | O_NDELAY)) < 0)
                    {
                        fprintf(stderr,"%s fnctl 2\n", time_stamp());
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    tmp = 1;
                    child = get_socket_child();
                    child->socket = s;

                    /* Socket accepted, wait for more conns */

                    FD_SET(parent->socket, &current_fds);
                    FD_SET(child->socket, &current_fds);
                    child->state = CHILD_TCP;
                    child->u.s.bytes_recv = 0;
                    child->next_all = tcp_sockets;
                    tcp_sockets = child;
                    ticket.s.seq +=
                        ((get_seed() - ticket.s.seq) & 0x7fffffff) + 1;
                    ticket.s.rnd = get_ticket();
                    if (s >= nfds)
                        nfds = s + 1;
                    memcpy(child->u.s.handle, header+4, 4);
                    memcpy(child->u.s.ticket.c, ticket.c, sizeof ticket);
                    header[3] = 23 + sizeof ticket;
                    memcpy(header+8, header+4, 4);
                    write_32(header+4, ERQ_HANDLE_KEEP_HANDLE);
                    header[12] = ERQ_OK;
                    memcpy(header+13, &host_ip_addr.sin_addr.s_addr, 4);
                    memcpy(header+17, &host_ip_addr.sin_port, 2);
                    write_32(header+19, child - &childs[0]);
                    memcpy(header+23, ticket.c, sizeof ticket);
                } while(0);
                if (header[3])
                    write1(header, header[3]);
              }
              break;
#endif /* ERQ_ACCEPT */

            } /* switch() */
        } /* if (num_ready > 0 && FD_ISSET(1, &readfds)) */

        if (subserver < 0)
            FD_CLR(1, &current_fds);
        else
            FD_SET(1, &current_fds);
    } /* main loop */

die:
    fprintf(stderr, "%s Giving up.\n", time_stamp());
    return 1;
} /* main() */

/***************************************************************************/

