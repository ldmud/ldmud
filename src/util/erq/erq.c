/* #define DEBUG */
/* #define DEBUG2 */

#include <sys/types.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/times.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include "../machine.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_TERMIOS_H
#include <sys/termios.h>
#endif
#ifdef _AIX
#include <sys/select.h>
#endif

#ifdef WEXITSTATUS
#define wait_status_t int
#else
#define wait_status_t union wait
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

#if defined(__GNUC__)
#define VOLATILE volatile
#else
#define VOLATILE
#endif

#define AUTH_PORT 113 /* according to RFC 931 */

#include "erq.h"
#include "../config.h"

#define DETACH
#define MAX_CHILDS 36
#define TIME_TO_CHECK_CHILDS 900
#define CHILD_EXPIRE 900
#define ERQ_BUFSIZE 1024
#define MAX_REPLY ERQ_MAX_REPLY

#ifdef RANDOM
#define randomize_tickets(n) srandom(n)
#define get_ticket() random()
#else /* RANDOM */
#ifdef DRAND48
#define randomize_tickets(n) srand48(n)
#define get_ticket() ((unsigned long)(drand48() * 4294967295.4))
#else /* DRAND48 */
#ifdef RAND
#define randomize_tickets(n) srand(n)
#define get_ticket() rand()
#else
#define randomize_tickets(n)
#define get_ticket() get_seed()
#endif /* RAND */
#endif /* DRAND48 */
#endif /* RANDOM */

#if MAX_REPLY >= ERQ_BUFSIZE
#undef MAX_REPLY
#define MAX_REPLY (ERQ_BUFSIZE - 1)
#endif

#ifndef TIOCNOTTY
#undef DETACH
#endif

#undef DETACH

union ticket_u {
    struct ticket_s {
        long rnd, seq;
    } s;
    char c[1];
};

struct child_s {
    int socket;
    union {
        struct {
            pid_t pid;
            time_t last_used;
        } c;
        struct {
            char handle[4];
            union ticket_u ticket;
            time_t last_recv;
            int bytes_recv;
        } s;
    } u;
    int state;
    struct child_s *next_free, *next_all;
} childs[MAX_CHILDS] = {{0}},
  *all_childs = 0, *free_childs = 0, *udp_sockets = 0, *tcp_sockets = 0,
  *accept_sockets = 0, *child_slots = 0;

#define CHILD_FREE   0
#define CHILD_LISTEN 1
#define CHILD_BUSY   2
#define CHILD_UDP    3
#define CHILD_TCP    4
#define CHILD_ACCEPT 5

int next_child_index = 0, childs_waited_for = 0;
VOLATILE int childs_terminated = 0;

fd_set current_fds, readfds;
fd_set current_fds2, writefds;

int nfds = 2;

time_t current_time;

#ifdef HAVE_GETTIMEOFDAY
long get_seed() {
    struct timeval tv; struct timezone tz;

    gettimeofday(&tv, &tz);
    return tv.tv_sec * 1000000L + tv.tv_usec;
}
#else
#ifdef sun
time_t time PROT((time_t *));
#endif

/* SunOS crashes on times(0), and times returns 0/-1 */
long get_seed() {
    long ret;
    struct tms dummy_tms;

    ret = (long)times(&dummy_tms);
    if (!ret)
        ret = (long)time((time_t *)0) +
          dummy_tms.tms_utime
          dummy_tms.tms_stime +
          dummy_tms.tms_cutime +
          dummy_tms.tms_cstime;
    return ret;
}
#endif /* HAVE_GETTIMEOFDAY */

long read_32(str)
    char *str;
{
    unsigned char *p = (unsigned char *)str;
    return (long)p[0]<<24 | (long)p[1]<<16 | (long)p[2]<<8 | p[3];
}

void write_32(str, n)
    char *str;
    long n;
{
    *(str+=3) = n;
    *--str = n >>= 8;
    *--str = n >>= 8;
    *--str = n >>= 8;
}

long readn(s, buf, expected)
    int s;
    char *buf;
    long expected;
{
    long num, total = 0;
    do {
        do
            num = read(s, buf+total, expected-total);
        while (num == -1 && errno == EINTR);
        if (num <= 0) {
            if (!num) {
                fprintf(stderr, "read: EOF\n");
                return 0;
            } else {
                perror("read");
                return -1;
           }
        }
        total += num;
#ifdef DEBUG
        if (total < expected)
            fprintf(stderr, "read fragment %ld\n", num);
#endif
    } while (num > 0 && total < expected);
    return total;
}

void writen(n, message, length)
    int n;
    char *message;
    long length;
{
    long num;

    do
        num = write(n, message, length);
    while (num == -1 && errno == EINTR);
    if (num != length) {
        fprintf(stderr, "wrote %ld, should be %ld\n", num, length);
        fprintf(stderr, "External Request Demon gives up.\n");
        abort();
    }
}

void write1(message, length)
    char *message;
    long length;
{
    writen(1, message, length);
}

int execute(buf, buflen, status, sockets)
    char *buf;
    long buflen;
    char *status;
    int *sockets;
{
    char path[256], argbuf[1024], *p, *args[96], **argp, c;
    pid_t pid;

    status[1] = 0;
    if (buflen >= sizeof argbuf) {
        status[0] = ERQ_E_ARGLENGTH;
        return 0;
    }
    argp = &args[0];
    *argp++ = p = argbuf;
    while (--buflen >= 0) {
        c = *buf++;
        if (c == '\\') {
            if (--buflen >= 0) {
                *p++ = *buf++;
            } else {
                status[0] = ERQ_E_ARGFORMAT;
                return 0;
            }
        } else if (isgraph(c)) {
            *p++ = c;
        } else {
            *p++ = '\0';
            *argp++ = p;
            if (argp == &args[sizeof args/sizeof args[0]]) {
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
    if (p[0] == '/' || strstr(p, "..")) {
        status[0] = ERQ_E_ILLEGAL;
        return 0;
    }
    if (strlen(ERQ_DIR) + strlen(p) + 2 > sizeof(path)) {
        status[0] = ERQ_E_PATHLEN;
        return 0;
    }
    sprintf(path, "%s/%s", ERQ_DIR, p);
    if (sockets) {
        if(socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0) {
            perror("socketpair");
            status[0] = ERQ_E_FORKFAIL;
            status[1] = errno;
            return 0;
        }
        if(socketpair(AF_UNIX, SOCK_STREAM, 0, sockets + 2) < 0) {
            perror("socketpair");
            close(sockets[0]);
            close(sockets[1]);
            status[0] = ERQ_E_FORKFAIL;
            status[1] = errno;
            return 0;
        }
    }
    pid = fork();
    if (!pid) {
        close(0);
        close(1);
        if (sockets) {
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
    if (sockets) {
        close(sockets[0]);
        close(sockets[2]);
    }
    if (pid < 0) {
        if (sockets) {
            close(sockets[1]);
            close(sockets[3]);
        }
        status[0] = ERQ_E_FORKFAIL;
        status[1] = errno;
        return 0;
    }
    return pid;
}

void count_sigcld() {
#ifndef HAVE_WAITPID
    static VOLATILE int calling_signal = 0, call_signal_again = 0;

    if (!calling_signal) {
        calling_signal = 1;
        (void)signal(SIGCLD, (RETSIGTYPE(*)())count_sigcld);
        while (call_signal_again) {
            --call_signal_again;
            (void)signal(SIGCLD, (RETSIGTYPE(*)())count_sigcld);
        }
        calling_signal = 0;
    } else {
        call_signal_again++;
    }
#ifdef DEBUG
    write(2, "child terminated\n", 17);
#endif
    childs_terminated++;
#endif
}

void dispose_child(child)
    struct child_s *child;
{
    struct child_s **chp;

    FD_CLR(child->socket, &current_fds);
    FD_CLR(child->socket, &current_fds2);
    close(child->socket);
    if (child->state == CHILD_FREE) {
        for (chp = &free_childs; *chp != child; chp = &(*chp)->next_free);
        *chp = child->next_free;
    }
    child->state = CHILD_FREE;
    for (chp = &all_childs; *chp != child; chp = &(*chp)->next_all);
    *chp = child->next_all;
    child->next_all = child_slots;
    child_slots = child;
}

void kill_child(child)
    struct child_s *child;
{
#ifdef DEBUG
    fprintf(stderr, "kill_child called\n");
#endif
    kill(child->u.c.pid, SIGKILL);
    dispose_child(child);
}

int free_socket_childs()
{
  return !(!child_slots && next_child_index >= MAX_CHILDS);
}

struct child_s *get_socket_child()
{
  struct child_s *child;
  if ( !(child = child_slots) ) {
    child = &childs[next_child_index++];
   } else {
    child_slots = child->next_all;
  }
  return child;
}

void free_socket_child(struct child_s *child, struct child_s **listp)
{
   while (*listp != child)
     listp = &(*listp)->next_all;
   *listp = child->next_all;
   child->next_all = child_slots;
   child->state = CHILD_FREE;
   child_slots = child;
}

int get_subserver() {
    struct child_s *child;

#ifdef DEBUG
    fprintf(stderr, "get_subserver called\n");
#endif
    if (free_childs) {
        child = free_childs;
        free_childs = child->next_free;
    } else {
        int sockets[2];
        int pid;

        if (!free_socket_childs())
            return -1;
        if(socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0) {
            perror("socketpair");
            return -1;
        }
        pid = fork();
        if(pid == -1) {
            close(sockets[0]);
            close(sockets[1]);
            return -1;
        }
        child = get_socket_child();
        if (!pid) {
            /* Child */
            void start_subserver();

            dup2(sockets[0], 0);
            dup2(sockets[0], 1);
            close(sockets[0]);
            close(sockets[1]);
            start_subserver(child - &childs[0], (long)get_ticket());
        }
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
}

static char buf[ERQ_BUFSIZE];

int main(argc, argv)
    int argc;
    char **argv;
{
    char header[32];
    long num, msglen;
    int subserver;
    int s;
    int num_ready;
    struct timeval timeout;
    struct child_s *child, *next_child;
    union ticket_u ticket;

    if (argc > 1 && !strcmp(argv[1], "--forked")) {
        write1("1", 1); /* indicate sucessful fork/execl */
    } else {
        fprintf(stderr, "dynamic attatchment unimplemented\n");
        goto die;
    }
#ifdef DETACH
    s = open("/dev/tty", O_RDWR);
    if (s >= 0) {
        /* We supply header as a 'dummy' argument in case typing is
         * too strict
         */
        ioctl(s, TIOCNOTTY, header);
        close(s);
    }
#endif
    randomize_tickets(get_seed());
    FD_ZERO(&current_fds);
    FD_ZERO(&current_fds2);
    FD_SET(1, &current_fds);
    (void)signal(SIGCLD, (RETSIGTYPE(*)())count_sigcld);
    for(subserver = 0;;) {
        int still_corked;

        still_corked = 0;
        for (next_child = tcp_sockets; (child = next_child); ) {

          next_child = child->next_all;
          /* Uncork the bottle */
          if(child->u.s.bytes_recv)
           {
            if(child->u.s.last_recv + 3 < time((time_t *)NULL))
             {
#ifdef DEBUG2
              fprintf(stderr,"Uncorking child\n");
#endif
              child->u.s.bytes_recv = 0;
              FD_SET(child->socket,&current_fds);
             } else still_corked = 1;
           }
        }
#ifdef DEBUG
        fprintf(stderr,"still_corked = %d\n",still_corked);
#endif
        readfds = current_fds;
        writefds = current_fds2;
        timeout.tv_sec = (still_corked ? 3 : TIME_TO_CHECK_CHILDS);
        timeout.tv_usec = 0;


#ifdef DEBUG
        fprintf(stderr, "calling select (nfds = %d)\n",nfds);
#endif
        num_ready = select(nfds, &readfds, &writefds, 0, &timeout);
#ifdef DEBUG
        fprintf(stderr, "select returns %d\n", num_ready);
#endif
        if (num_ready < 0 && errno != EINTR) {
            perror ("select");
            abort ();
        }
        current_time = time((time_t *)NULL);
        {
            time_t expired;

            expired = current_time - CHILD_EXPIRE;
            for (next_child = free_childs; (child = next_child); ) {
                next_child = child->next_free;
                if (child->u.c.last_used > expired)
                    continue;
#ifdef DEBUG
                fprintf(stderr, "Max child idle time expired.\n");
#endif
                kill_child(child);
            }
        }
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
            for (child = all_childs; child; child = child->next_all) {
                if (child->u.c.pid == pid) {
                    dispose_child(child);
                    break;
                }
            }
            childs_waited_for++;
        }
        if (num_ready > 0) {
          for (next_child = all_childs; (child = next_child); ) {
            long replylen;
            char replyheader[12];
            char replybuf[ERQ_BUFSIZE];

            next_child = child->next_all;
            s = child->socket;
            if (!FD_ISSET(s, &readfds))
                continue;
#ifdef DEBUG
                fprintf(stderr, "query child %d\n", child - &childs[0]);
#endif
            num = readn(s, replyheader, 9);
            if (num != 9) {
                fprintf(stderr, "read %ld, should be 9.\n", num);
                kill_child(child);
                continue;
            }
            replylen = read_32(replyheader) - 8;
            if (replylen > 0) {
                if (replylen > sizeof replybuf) {
                    fprintf(stderr, "Too long reply.\n");
                    kill_child(child);
                    continue;
                }
                num = readn(s, replybuf, replylen);
                if (num != replylen) {
                    fprintf(stderr, "read %ld, should be %ld\n", num, replylen);
                    kill_child(child);
                    continue;
                }
            }
            write1(replyheader, 8);
            write1(replybuf, replylen);
            /* We can't simply test for ERQ_HANDLE_KEEP_HANDLE, because a
             * subserver can have several handles at a time.
             */
            if ((child->state = replyheader[8]) == CHILD_FREE) {
                child->u.c.last_used = current_time;
                child->next_free = free_childs;
                free_childs = child;
            }
          }
#ifdef DEBUG
            fprintf(stderr, "queried all childs\n");
#endif
#ifdef ERQ_OPEN_UDP
          for (next_child = udp_sockets; (child = next_child); ) {
            int length;
            long replylen;
            char replyheader[16];
            char replybuf[ERQ_BUFSIZE];
            struct sockaddr_in addr;
            int cnt;

            next_child = child->next_all;
            s = child->socket;
            if (!FD_ISSET(s, &readfds))
                continue;
            length = sizeof addr;
            cnt = recvfrom(s, replybuf, sizeof(replybuf), 0,
                        (struct sockaddr *)&addr, &length);
            replylen = length + 19;
            if (replylen > MAX_REPLY)
                replylen = MAX_REPLY;
            write_32(replyheader, replylen);
            write_32(replyheader+4, ERQ_HANDLE_KEEP_HANDLE);
            memcpy(replyheader+8, child->u.s.handle, 4);
            replyheader[12] = ERQ_STDOUT;
            write1(replyheader, 13);
            write1(addr.sin_addr.s_addr, 4);
            write1(addr.sin_port, 2);
            write1(replybuf, replylen - 19);
          }
#endif
#ifdef ERQ_OPEN_TCP

/* TCPFIX */
          for (next_child = tcp_sockets; (child = next_child); ) {
            int length;
            long replylen;
            char replyheader[30];
            char replybuf[ERQ_BUFSIZE];
            int cnt;

            next_child = child->next_all;
            s = child->socket;
            if (FD_ISSET(s, &writefds))
             {
               FD_CLR(s,&current_fds2);
               cnt = recv(s,replybuf,1,MSG_PEEK);
               if(cnt < 0 && (errno != EWOULDBLOCK && errno != EAGAIN))
                {
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
               replyheader[12] = ERQ_OK;
               write_32(replyheader,17+sizeof(child->u.s.ticket));
               write_32(replyheader+4, ERQ_HANDLE_KEEP_HANDLE);
               memcpy(replyheader+8, child->u.s.handle, 4);
               write_32(replyheader+13,child - &childs[0]);
               memcpy(replyheader+17,child->u.s.ticket.c,sizeof(child->u.s.ticket));
               write1(replyheader, 17+sizeof(child->u.s.ticket));
               break;
             } else if (!FD_ISSET(s, &readfds))
                continue;
            cnt = read(s, replybuf, MAX_REPLY-100);
            FD_CLR(s,&readfds);
            if(cnt <= 0)
             {
              if(!cnt)
               {
                replyheader[8] = ERQ_EXITED;
                replylen = 9;
               } else {
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
             } else {
              length = cnt;
#if 0
              child->u.s.last_recv = time((time_t *)NULL);
              child->u.s.bytes_recv += cnt;
#endif
              if(child->u.s.bytes_recv > 4096)
               {
                /* Cork the bottle. Let the MUD swallow first */
                FD_CLR(s,&current_fds);
#ifdef DEBUG2
                fprintf(stderr,"Corking child.\n");
#endif
               }
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
#endif
#ifdef ERQ_LISTEN
          for (next_child = accept_sockets; (child = next_child); ) {
            int length;
            long replylen;
            char replyheader[30];
            char replybuf[ERQ_BUFSIZE];
            int cnt;

            next_child = child->next_all;
            s = child->socket;
            if (!FD_ISSET(s, &readfds))
                continue;
            FD_CLR(s,&current_fds); /* Connection pending */
            {
              replylen = 13;
              write_32(replyheader, replylen);
              write_32(replyheader+4, ERQ_HANDLE_KEEP_HANDLE);
              memcpy(replyheader+8, child->u.s.handle, 4);
              replyheader[12] = ERQ_STDOUT;
              write1(replyheader, replylen);
            }
          }
#endif
        }
        if (subserver < 0) {
            subserver = get_subserver();
            if (subserver < 0)
                continue;
            writen(subserver, header, 9);
            writen(subserver, buf, msglen);
        }
        if (num_ready > 0 && FD_ISSET(1, &readfds)) {
            num = readn(0, header, 9);
            if (num != 9) {
                fprintf(stderr, "Read %ld, should be 9!\n", num);
                if (num < 0)
                    perror("read");
                break;
            }
#ifdef DEBUG
            fprintf(stderr, "read command %d\n", header[8]);
#endif
            msglen = read_32(header) - 9;
            if (msglen > 0) {
                num = readn(0, buf, msglen);
                if (num != msglen) {
                    fprintf(stderr, "Read %ld, should be %ld\n", num, msglen);
                    break;
                }
            }
            switch(header[8]) {
              case ERQ_SEND:
              case ERQ_KILL:
              {
                long n;

                n = read_32(buf);
                if ((unsigned long)n >= (unsigned long)next_child_index)
                    goto bad_ticket;
                child = &childs[n];
                switch(child->state) {
                  struct child_s **listp;

                  bad_ticket:
                  default:
                  {
#ifdef DEBUG
                    fprintf(stderr, "Ticket rejected n: 0x%x nxt: 0x%x state: %d\n",
                        n, next_child_index,
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
                    subserver = child->socket;
                    write_32(header, 9 + msglen - 4);
                    writen(subserver, header, 9);
                    writen(subserver, buf+4, msglen-4);
                    break;
#ifdef ERQ_LISTEN
                  case CHILD_ACCEPT:
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
                    if (msglen < sizeof(union ticket_u) ||
                        memcmp(buf+4, child->u.s.ticket.c,
                          sizeof(union ticket_u)))
                    {
#ifdef DEBUG
                        fprintf(stderr,"Ticket mismatch. (%d, %d)\n",msglen,sizeof(union ticket_u));
#endif
                        goto bad_ticket;
                    }
                    msglen -= sizeof(union ticket_u);
                    subserver = 0; /* ready for new commands */
                    write_32(header, 9);
                    if (header[8] == ERQ_SEND) {
                        while(1) {
                            if (child->state == CHILD_UDP) {
                                struct sockaddr_in host_ip_addr;

                                if (msglen < 6) {
                                    header[8] = ERQ_E_ARGLENGTH;
                                    break;
                                }
                                memcpy(&host_ip_addr.sin_addr,
                                  buf+sizeof(union ticket_u), 4);
                                host_ip_addr.sin_family = AF_INET;
                                memcpy(&host_ip_addr.sin_port,
                                  buf+sizeof(union ticket_u)+4, 2);
                                num = sendto(child->socket,
                                        buf+sizeof(union ticket_u)+6,
                                        msglen - 6, 0,
                                        (struct sockaddr *)&host_ip_addr,
                                        sizeof(host_ip_addr));
                            } else {
                                num = write(child->socket,
                                        buf+sizeof(union ticket_u)+4,
                                        msglen-4);
                            }
                            if (num != msglen-4) {
                                if (num < 0) {
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
                                } else {
                                    header[3] = 13;
                                    header[8] = ERQ_E_INCOMPLETE;
                                    write_32(header+9, num);
                                }
                            } else {
                                header[3] = 9;
                                header[8] = ERQ_OK;
                            }
                            break;
                        }
                    } else { /* header[8] == ERQ_KILL */
                        FD_CLR(child->socket,&current_fds);
                        FD_CLR(child->socket,&current_fds2);
                        if(child->state == CHILD_ACCEPT)
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
                subserver = get_subserver();
                if (subserver >= 0) {
                    writen(subserver, header, 9);
                    writen(subserver, buf, msglen);
                }
              }
              break;
#ifdef ERQ_OPEN_UDP
              case ERQ_OPEN_UDP:
              {
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct sockaddr_in host_ip_addr;
                    int tmp;

                    if (msglen != 2) {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }
                    host_ip_addr.sin_addr.s_addr = INADDR_ANY;
                    host_ip_addr.sin_family = AF_INET;
                    memcpy(&host_ip_addr.sin_port, buf, 2);
                    if (!free_socket_childs()) {
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

/* TCPFIX */
              case ERQ_OPEN_TCP:
              {
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct sockaddr_in host_ip_addr;
                    int tmp;

                    if (msglen != 6) {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }
                    host_ip_addr.sin_family = AF_INET;
                    memcpy(&host_ip_addr.sin_port, buf+4, 2);
                    memcpy(&host_ip_addr.sin_addr.s_addr, buf, 4);

                    if (!free_socket_childs()) {
                        header[8] = ERQ_E_NSLOTS;
                        header[9] = MAX_CHILDS;
                        break;
                    }
                    s = socket(AF_INET, SOCK_STREAM, 0);
                    if (s < 0) {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if((tmp = fcntl(s,F_GETFL,0)) < 0)
                     {
                      fprintf(stderr,"fnctl 1\n");
                      header[8] = ERQ_E_UNKNOWN;
                      header[9] = errno;
                      break;
                     }

                    if((tmp = fcntl(s,F_SETFL,tmp | O_NDELAY)) < 0)
                     {
                      fprintf(stderr,"fnctl 2\n");
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

/* TCPFIX */
              case ERQ_LISTEN:
              {
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct sockaddr_in host_ip_addr;
                    int tmp;

                    if (msglen != 2) {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }
                    host_ip_addr.sin_family = AF_INET;
                    host_ip_addr.sin_addr.s_addr = INADDR_ANY;
                    memcpy(&host_ip_addr.sin_port, buf, 2);

                    if (!free_socket_childs()) {
                        header[8] = ERQ_E_NSLOTS;
                        header[9] = MAX_CHILDS;
                        break;
                    }
                    s = socket(AF_INET, SOCK_STREAM, 0);
                    if (s < 0) {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if((tmp = fcntl(s,F_GETFL,0)) < 0)
                     {
                      fprintf(stderr,"fnctl 1\n");
                      header[8] = ERQ_E_UNKNOWN;
                      header[9] = errno;
                      break;
                     }

                    if((tmp = fcntl(s,F_SETFL,tmp | O_NDELAY)) < 0)
                     {
                      fprintf(stderr,"fnctl 2\n");
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

/* TCPFIX */
              case ERQ_ACCEPT:
              {
                subserver = 0; /* ready for new commands */
                write_32(header, 10);
                do {
                    struct child_s *parent;
                    struct sockaddr_in host_ip_addr;
                    int tmp,n;
                    long tmp2;

                    if (msglen != sizeof(union ticket_u) + 4) {
                        header[8] = ERQ_E_ARGLENGTH;
                        header[9] = 0;
                        break;
                    }
                    n = read_32(buf);
                    if((unsigned long) n >= (unsigned long) next_child_index)
                     {
                      fprintf(stderr,"given: %d, nxt: %d\n",n,next_child_index);
                      goto accept_bad_ticket;
                     }
                    parent = &childs[n];
                    if(parent->state != CHILD_ACCEPT)
                     {
                      fprintf(stderr,"State is %d, should be %d!\n",parent->state,CHILD_ACCEPT);
                      goto accept_bad_ticket;
                     }
                    if(memcmp(buf+4,parent->u.s.ticket.c,sizeof(union ticket_u)))
                     {
                      accept_bad_ticket:
                      fprintf(stderr,"Accept: Ticket mismatch.\n");
                      header[8] = ERQ_E_TICKET;
                      header[9] = 0;
                      break;
                     }
                    if (!free_socket_childs()) {
                        header[8] = ERQ_E_NSLOTS;
                        header[9] = MAX_CHILDS;
                        break;
                    }
                    tmp = sizeof(host_ip_addr);
                    s = accept(parent->socket, (struct sockaddr *)
                      &host_ip_addr, &tmp);
                    if (s < 0) {
                        header[8] = ERQ_E_UNKNOWN;
                        header[9] = errno;
                        break;
                    }
                    if((tmp = fcntl(s,F_GETFL,0)) < 0)
                     {
                      fprintf(stderr,"fnctl 1\n");
                      header[8] = ERQ_E_UNKNOWN;
                      header[9] = errno;
                      break;
                     }

                    if((tmp = fcntl(s,F_SETFL,tmp | O_NDELAY)) < 0)
                     {
                      fprintf(stderr,"fnctl 2\n");
                      header[8] = ERQ_E_UNKNOWN;
                      header[9] = errno;
                      break;
                     }
                    tmp = 1;
                    child = get_socket_child();
                    child->socket = s;
                    /* Socket accepted, wait for more conns */
                    FD_SET(parent->socket,&current_fds);
                    FD_SET(child->socket,&current_fds);
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
                if(header[3]) write1(header, header[3]);
              }
              break;
#endif /* ERQ_ACCEPT */
            }
        }
        if (subserver < 0)
            FD_CLR(1, &current_fds);
        else
            FD_SET(1, &current_fds);
    }
die:
    fprintf(stderr, "External Request Demon gives up.\n");
    return 1;
}

void start_subserver(server_num, seed)
    int server_num;
    long seed;
{
    union ticket_u ticket;
    char header[16];
    long num, msglen;
    int request;
    pid_t child = 0;
    int child_sockets[4];
    char child_handle[4];

#ifdef DETACH
    num = open("/dev/tty", O_RDWR);
    if (num >= 0) {
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
    for (;;) {
        if (child) {
            FD_SET(child_sockets[1], &readfds);
            FD_SET(child_sockets[3], &readfds);
        }
        FD_SET(0, &readfds);
        num = select(nfds, &readfds, 0, 0, 0);
        if (num < 0) {
            if (errno == EINTR)
                continue;
            perror ("select");
            abort ();
        }
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
            if (pid <= 0) {
                fprintf(stderr,
                  "SIGCLD handler %d times called, but wait not sucessful\n",
                  childs_terminated - childs_waited_for
                );
                abort();
            }
#endif
            if (pid == child) {
                header[8] = CHILD_FREE;
                if (WIFEXITED(status)) {
                    header[9] = ERQ_EXITED;
                    header[10] = WEXITSTATUS(status);
                } else if (WIFSIGNALED(status)) {
                    header[9] = ERQ_SIGNALED;
                    header[10] = WTERMSIG(status);
                } else {
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
        }
        if (child) {
            int n = 3;
            do {
                if (FD_ISSET(child_sockets[n], &readfds)) {
                    do
                        num = read(child_sockets[n], buf+14, MAX_REPLY - 13);
                    while (num == -1 && errno == EINTR);
                    if (num <= 0) {
                        perror("read from spawned child\n");
                    } else {
#ifdef DEBUG
                        fprintf(stderr,
                          "%d bytes from socket no. %d\n", num, n);
                        fprintf(stderr,
                          "'%.*s'\n", num, buf+14);
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
        if (!FD_ISSET(0, &readfds))
            continue;
        do
            num = readn(0, header, 9);
        while (num == -1 && errno == EINTR);
        if (num != 9) {
            fprintf(stderr, "read %ld, should be 9!\n", num);
            if (num < 0)
                perror("read");
            break;
        }
        msglen = read_32(header) - 9;
        request = header[8];
        if (msglen > 0) {
            num = readn(0, buf, msglen);
            if (num != msglen) {
                fprintf(stderr, "Read %ld, should be %ld\n", num, msglen);
                break;
            }
        }
        switch(request) {
          case ERQ_RLOOKUP:
          {
            struct hostent *hp;

            /* handle stays in header[4..7] */
            header[8] = CHILD_FREE;
            memcpy(header+9, buf, 4); /* copy address */
            hp = gethostbyaddr(buf, 4, AF_INET);
            if (!hp) {
                sleep(5);
                hp = gethostbyaddr(buf, 4, AF_INET);
            }
            if (hp) {
                msglen = strlen(hp->h_name) + 1;
                write_32(header, msglen + 12);
                write1(header, 13);
                write1(hp->h_name, msglen);
            } else {
                write_32(header, 12);
                write1(header, 13);
            }
            break;
          }
#ifdef ERQ_LOOKUP
          case ERQ_LOOKUP:
          {
            struct hostent *hp;

            /* handle stays in header[4..7] */
            header[8] = CHILD_FREE;
            memcpy(header+9, buf, strlen(buf)); /* copy address */
            hp = gethostbyname(buf);
            if (!hp) {
                sleep(5);
                hp = gethostbyname(buf);
            }
            if (hp) {
                msglen = 4;
                write_32(header, msglen + 8);
                write1(header, 9);
                write1(hp->h_addr, msglen);
            } else {
                write_32(header, 8);
                write1(header, 9);
            }
            break;
          }
#endif
          case ERQ_EXECUTE:
          {
            pid_t pid1, pid2;

            header[8] = CHILD_FREE;
            if ((pid1 = execute(buf, msglen, &header[9], 0))) {
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
            header[8] = CHILD_FREE;
            if (execute(buf, msglen, &header[9], 0)) {
                header[9] = ERQ_OK;
            }
            write_32(header, 10);
            write1(header, 11);
            break;
          case ERQ_SPAWN:
#ifdef DEBUG
            if (child) {
                fprintf(stderr, "ERQ_SPAWN: busy\n");
                abort();
            }
#endif
            if ((child = execute(buf, msglen, &header[9], child_sockets))) {
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
            } else {
                header[8] = CHILD_FREE;
                write_32(header, 10);
                write1(header, 11);
            }
            break;
          case ERQ_SEND:
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
            if (msglen != sizeof ticket + 4 && msglen != sizeof ticket)
                goto bad_request;
            if (!child) {
no_child:
                /* could happen due to a race condition */
#ifdef DEBUG
                fprintf(stderr, "ERQ_SEND/ERQ_KILL: No child\n");
#endif
                header[8] = CHILD_FREE;
                goto notify_bad_ticket;
            }
            header[8] = CHILD_LISTEN;
            if (memcmp(buf, ticket.c, sizeof ticket)) {
bad_ticket:
#ifdef DEBUG
                fprintf(stderr, "ticket.s.rnd: %x vs. %x\n",
                  ((struct ticket_s *)buf)->rnd, ticket.s.rnd);
                fprintf(stderr, "ticket.s.seq: %x vs. %x\n",
                  ((struct ticket_s *)buf)->seq, ticket.s.seq);
#endif
notify_bad_ticket:
                header[9] = ERQ_E_TICKET;
            } else {
                int sig;

                sig = msglen >= 4 ? read_32(buf+sizeof ticket) : SIGKILL;
#ifdef DEBUG
                fprintf(stderr, "len: %d sig: %d\n", msglen, sig);
#endif
                if (sig >= 0)
                    sig = kill(child, sig);
                header[9] = sig < 0 ? ERQ_E_ILLEGAL : ERQ_OK;
#ifdef DEBUG
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
            fprintf(stderr, "Bad request %d\n", request);
            fprintf(stderr, "%x %x %x %x %x %x %x %x %x\n", header[0],header[1],header[2],header[3],header[4],header[5],header[6],header[7],header[8]);
            fprintf(stderr, "%c %c %c %c %c %c %c %c %c\n", header[0],header[1],header[2],header[3],header[4],header[5],header[6],header[7],header[8]);
            write_32(header, 8);
            header[8] = child ? CHILD_LISTEN : CHILD_FREE;
            write1(header, 9);
            break;
        }
    }
die:
    if (child) {
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
    fprintf(stderr, "External Request Demon subserver gives up.\n");
    exit(1);
}
