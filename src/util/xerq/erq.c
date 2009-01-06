/*---------------------------------------------------------------------------
 * XErq - Main module.
 * (C) Copyright 1995 by Brian Gerst.
 * (C) Copyright 2001 by Brian Gerst, Frank Kirschner, Lars Duening.
 *---------------------------------------------------------------------------
 * This module implements the main() function with the central loop, plus
 * a bunch of utility functions.
 *
 * Dispatch of the various ERQ requests is implemented with a lookup table
 * holding the addresses of the request functions.
 *---------------------------------------------------------------------------
 */

#include "defs.h"

/*-------------------------------------------------------------------------*/
void (*erq_table[])(char *, int)
  = { erq_rlookup
    , erq_execute
    , erq_fork
    , erq_auth
    , erq_spawn
    , erq_send
    , erq_kill
    , erq_open_udp
    , erq_open_tcp
    , erq_listen
    , erq_accept
    , erq_lookup
#ifdef USE_IPV6
    , erq_rlookupv6
#endif
};
  /* Dispatchtable for the ERQ request functions.
   * Arguments are (message, msg_len).
   */

#ifndef USE_IPV6
#    define ERQ_REQUEST_MAX ERQ_LOOKUP
#else
#    define ERQ_REQUEST_MAX ERQ_RLOOKUPV6
#endif

/*-------------------------------------------------------------------------*/
const char * erq_dir = ERQ_DIR;
  /* The filename of the directory with the ERQ executables. */

child_t *childs;
  /* List of active children. The main loop will remove _EXITED children.
   */

socket_t *sockets;
  /* List of opened sockets, including those to communicate with spawned
   * commands.
   */

retry_t *retries;
  /* List of function calls to retry at a later point of time.
   */

equeue_t *stdout_queue;
  /* List of messages pending to write to stdout.
   */

int in_select;
  /* TRUE while erq is in select() - during this time sig_child()
   * can write its replies directly.
   */

int seq_number;
  /* The last sequence number assigned to ticket, incremented
   * in <seq_interval>s, initialized with a random number.
   */

int seq_interval;
  /* The interval used to increment seq_number, initialized with
   * an odd random number.
   */

pid_t master_pid;
  /* The pid of the 'master' erq process.
   */

/* When spawning a short-running child process, it used to happen that the
 * process finishes before the parent even manages to put the pid into
 * the child control structure; causing sig_child() not to find the child
 * structure when it gets the signal.
 * To avoid this, the spawning code employs a synchronisation scheme to
 * make sure that the child doesn't execute before the parent completed
 * initialisation.
 *
 * For ERQ_FORKed children (which are not synchronized), and as a fallback
 * solution in case the synchronisation fails and the child terminates
 * prematurely, sig_child() stores the relevant data in these globals for
 * the main process to evaluate. The calling pattern (one ERQ
 * command per select() round) guarantees that there can be only one such
 * 'unfinished' child control structure at a time.
 */

volatile int pending_sig;
  /* Set to true if these variables hold data of for an unaccounted SIG_CLD.
   */

volatile wait_status_t pending_status;
volatile pid_t pending_pid; 
  /* The status and pid for the pending SIG_CLD, valid only while pending_sig
   * is TRUE.
   */

/*-------------------------------------------------------------------------*/
char *
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
    static char result[27+20];
    struct tm *tm;
    int pid;

    t = time(NULL);
    tm = localtime(&t);
    pid = getpid();
    if (pid == master_pid)
        strftime(result, sizeof(result), "%Y.%m.%d %H:%M:%S [xerq]", tm);
    else
    {
        strftime(result, sizeof(result), "%Y.%m.%d %H:%M:%S [xerq:", tm);
        sprintf(result+26, "%d]", pid);
    }
    return result;
} /* time_stamp() */

/*-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])

/* The main program and -loop of the ERQ.
 */

{
    int num;

    master_pid = getpid();

    /* Print information about this daemon to help debugging */
    {
        fprintf(stderr, "%s XERQ %s: Path '%s', debuglevel %d\n"
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
                    die();
                }
                erq_dir = argv[i+1];
                i++;
            }
            else
            {
                fprintf(stderr, "%s Unknown argument '%s'.\n"
                              , time_stamp(), argv[i]);
                die();
            }
        }
        /* Check if we have been forked off the driver */
        if (is_forked)
        {
            write(1, "1", 1); /* indicate sucessful fork/execl */
            fprintf(stderr, "%s Demon started\n", time_stamp() );
        }
        else
        {
            fprintf(stderr, "%s Dynamic attachement unimplemented\n"
                          , time_stamp());
            die();
        }
    }

    /* Initialize */
    
    in_select = 0;
    pending_sig = 0;

    signal(SIGCLD, sig_child);
    signal(SIGPIPE, SIG_IGN);

    sockets = NULL;
    childs = NULL;
    retries = NULL;
    stdout_queue = NULL;
    
    randomize(time(0));
    seq_number = get_ticket();
    seq_interval = get_ticket() | 1; /* make sure it is odd */

#ifdef DETACH
    /* Detach from console */
    num = open("/dev/tty", O_RDWR);
    if (num >= 0) {
        ioctl(num, TIOCNOTTY, 0);
        close(num);
    }
#endif

    /* The main loop */
    
    while(1)
    {
        fd_set read_fds, write_fds;
        int num_fds;
        child_t *chp;
        retry_t *rtp, **rtpp;
        socket_t *sp;
        struct timeval timeout;

        /* Clean up the list of children (may close some sockets) */
        
        for (chp = childs; chp;)
        {
            child_t *this = chp;

            chp = chp->next;

            /* If there is a pending SIG_CLD for this child, handle it.
             * This is to be expected for CHILD_FORK children.
             */
            if (pending_sig && this->pid == pending_pid)
            {
                if (this->type != CHILD_FORK)
                    fprintf(stderr, "%s Pending SIG_CLD for pid %d delivered.\n"
                                  , time_stamp(), pending_pid);
                this->status = pending_status;
                this->pid = pending_pid;
                pending_sig = 0;
            }

            if (this->status == CHILD_EXITED)
            {
                XPRINTF((stderr, "%s Child %p exited.\n", time_stamp(), this));
                remove_child(this); /* will also unlink it from the list */
            }
        }

        /* look for sockets to select on */

        FD_ZERO(&read_fds);
        FD_ZERO(&write_fds);

        FD_SET(0, &read_fds);
        if (stdout_queue)
            FD_SET(1, &write_fds);

        num_fds = 2;
        for (sp = sockets; sp; sp = sp->next)
        {
            switch(sp->type)
            {
            case SOCKET_WAIT_CONNECT:
            case SOCKET_WAIT_AUTH:
                FD_SET(sp->fd, &write_fds);
                FD_SET(sp->fd, &read_fds);
                if (sp->fd >= num_fds)
                    num_fds=sp->fd+1;
                break;

            default:
                FD_SET(sp->fd, &read_fds);
                if (sp->fd >= num_fds)
                    num_fds=sp->fd+1;
                break;

            case SOCKET_WAIT_ACCEPT:
                /* do nothing */;
                /* Without the ; above, Metrowerks Codewarrior reports
                 * an error :-( */
            }

            if (sp->queue)
                FD_SET(sp->fd, &write_fds);
        } /* for (sockets) */

        /* Scan the list of pending retries for the soonest one.
         * Put the time till then into timeout.
         * (If the list is empty, select() will receive NULL for timeout).
         */
        if (retries)
        {
            time_t t;

            t = retries->time;
            for (rtp = retries; rtp; rtp = rtp->next)
            {
                if (rtp->time < t)
                    t = rtp->time;
            }
            timeout.tv_sec = t - time(NULL);
            timeout.tv_usec = 0;
            XPRINTF((stderr, "%s Soonest retry_t: in %ld seconds.\n"
                           , time_stamp(), (long)timeout.tv_sec));
			   
            if (timeout.tv_sec < 0)
                timeout.tv_sec = 0;
        }

#if ERQ_DEBUG > 1
        fprintf(stderr, "%s select()\n", time_stamp());
#endif
        in_select = 1; /* so sig_child() can write reply directly */
        num = select(num_fds, &read_fds, &write_fds, 0, retries ? &timeout : 0);
        in_select = 0; /* don't want sig_child() writing now */

#if ERQ_DEBUG > 1
        {
            int myerrno = errno;
            fprintf(stderr, "%s select() returns %d, time() %ld\n"
                          , time_stamp(), num, (long)time(NULL));
            errno = myerrno;
        }
#endif

#if ERQ_DEBUG > 0
        if (num < 0)
	/* Give an error now, but don't abort this loop,
	 * because the retries have to be handled first.
	 */
        {
            int myerrno = errno;
            fprintf(stderr, "%s select() errno = %d", time_stamp(), errno);
            errno = myerrno;
            perror(" ");
        }
#endif
	
        /* Is stdout ready to write? Then flush the queue. */
        if (num >= 0 && FD_ISSET(1, &write_fds))
        {
            XPRINTF((stderr, "%s stdout_queue ready for flush.\n", time_stamp()));
            flush_queue(&stdout_queue, 1, 0);
        }

        /* Check for retries */
        for (rtpp = &retries; *rtpp; )
        {
            rtp = *rtpp;
            if (rtp->time <= time(NULL))
            {
                XPRINTF((stderr, "%s Call retry %p (time %ld)\n"
                               , time_stamp(), rtp, (long)rtp->time));
                (*(rtp->func))(rtp->mesg, read_32(rtp->mesg));
                *rtpp = rtp->next;
                free(rtp);
            }
            else
            {
                rtpp = &rtp->next;
            }
        }
	
        /* Error in select */
        if (num < 0)
            continue;

        /* check for input from driver */
        if (FD_ISSET(0, &read_fds))
        {
            XPRINTF((stderr, "%s New command from driver.\n", time_stamp()));
            erq_cmd();
        }

        /* Handle the ready sockets.
         * Remember that read_socket() may close the socket.
         */

        for (sp = sockets; sp; )
        {
            socket_t *this = sp;
            int rc;

            sp = sp->next;

            rc = 0;

            if (FD_ISSET(this->fd, &read_fds))
            {
                XPRINTF((stderr, "%s Socket %p (%d) ready for reading.\n"
                               , time_stamp(), this, this->fd));
                rc = read_socket(this, 0);
            }

            if (!rc && FD_ISSET(this->fd, &write_fds))
            {
                XPRINTF((stderr, "%s Socket %p (%d) ready for writing.\n"
                               , time_stamp(), this, this->fd));
                (void)read_socket(this, 1);
            }
        }
    } /* while(1) */

    /* NOTREACHED */
    
    return 0;
} /* main() */

/*-------------------------------------------------------------------------*/
void
erq_cmd (void)

/* There is data ready from the driver - read and execute it when complete.
 * The function maintains a static buffer for the data read - incomplete
 * messages are buffered until they are complete.
 */

{
    static char buf[ERQ_MAX_SEND];
    static int pos = 0;
      /* Position in buf[]. If it extends beyond the end of buf,
       * it is because the message is too long and the function
       * is in the process of skipping the extraneous characters.
       */

    int len, mesg_len;
    char request;

    /* Clear the buffer so that errors can be detected more easily */
    memset(buf, 0, sizeof(buf));

    /* Read the message header */
    if (pos < 9)
    {
        len = read(0, buf+pos, 9-pos);
        if (len <= 0)
        {
            perror("[xerq] read");
            die();
        }
        XPRINTF((stderr, "%s Read %d of the missing %d header bytes.\n"
                       , time_stamp(), len, 9-pos));
        pos += len;
        if (pos < 9)
            return;
    }

    mesg_len = read_32(buf);
    if (mesg_len > sizeof(buf))
    {
        /* This doesn't happen in a functioning system */
        fprintf(stderr
               , "%s Received too long packet: %d bytes.\n"
               , time_stamp(), mesg_len);
        die();
    }

    /* Get the rest of the message */

    if (pos < mesg_len)
    {
        len = read(0, buf+pos, mesg_len-pos);
        if (len <= 0)
        {
            perror("read");
            die();
        }
        XPRINTF((stderr, "%s Read %d of the missing %d message bytes.\n"
                       , time_stamp(), len, mesg_len-pos));
        pos += len;
        if (pos < mesg_len)
            return;
    }

    XPRINTF((stderr, "%s Message complete.\n", time_stamp()));
    pos = 0; /* Message complete */

    /* Branch on the request */
    request = buf[8];
    if (request <= ERQ_REQUEST_MAX)
    {
#if ERQ_DEBUG > 0
        char *mesg, *mesgs[]={
            "rlookup","execute","fork","auth","spawn","send","kill",
            "open_udp","open_tcp","listen","accept","lookup", "rlookupv6"};
        mesg=mesgs[(int)request];
        fprintf(stderr, "%s command: %s\n", time_stamp(), mesg);
#endif
        (*erq_table[(int)request])(buf, mesg_len);
    }
    else
        bad_request(buf);
} /* erq_cmd() */

/*-------------------------------------------------------------------------*/
void
die(void)

/* Terminate the ERQ with status 1.
 */

{
    fprintf(stderr, "%s Demon exiting.\n", time_stamp());
    exit(1);
} /* die() */

/*-------------------------------------------------------------------------*/
#ifndef _AIX
void
sig_child()
#else
void
sig_child(int sig)
#endif

/* A child process exited - update its child structure.
 */

{
    wait_status_t status;
    pid_t pid;
    struct child_s *chp;

    pid = wait(&status);

#if ERQ_DEBUG > 0
    fprintf(stderr, "%s [sigchild] pid=%d status=%d\n"
                  , time_stamp(), pid, status);
#endif

    /* Look for the child and mark it as exited */
    for (chp = childs; chp; chp = chp->next)
    {
        if (chp->pid != pid)
            continue;

        chp->status = CHILD_EXITED;
        chp->return_code = status;
#if ERQ_DEBUG > 0
        fprintf(stderr, "%s [sigchild] Caught SIGCLD for pid %d, child %p.\n"
                      , time_stamp(), pid, chp);
#endif
        if (in_select)
            remove_child(chp); /* safe to do it from here */
        /*  if we're in select, we know we're not going to be messing up
            the main loop with stuff we're doing here */
        break;
    }

    if (!chp)
    {
        /* There is no valid child. Maybe we caught the signal before
         * the child structure was complete (this can happen especially
         * with short-lived CHILD_FORK sub processes).
         */
        if (pending_sig)
        {
            fprintf(stderr, "%s [sigchild] SIGCLD for pid %d not delivered.\n"
                          , time_stamp(), pending_pid);
        }

#if ERQ_DEBUG > 0
        fprintf(stderr, "%s [sigchild] SIGCLD for unknown pid %d received.\n"
                      , time_stamp(), pid);
#endif

        pending_status = status;
        pending_pid = pid;
        pending_sig = 1;
    }

    /* Restore the signal handler */
    signal(SIGCLD, sig_child);
} /* sig_child() */

/*-------------------------------------------------------------------------*/
void
add_retry (void (*func)(char *, int), char *mesg, int len, int t)

/* Add a new retry: function <func> is to be executed in <t> seconds
 * with (<mesg>, <len>) as arguments.
 */

{
    struct retry_s *retry;

    retry = malloc(sizeof(struct retry_s)+len);
    XPRINTF((stderr, "%s New retry %p: %d seconds, func %p, data %p:%d\n"
                   , time_stamp(), retry, t, func, mesg, len));
    retry->time = time(NULL)+t;
    retry->func = func;
    memcpy(&retry->mesg, mesg, len);
    retry->next = retries;
    retries = retry;
} /* add_retry() */

/*-------------------------------------------------------------------------*/
void
bad_request (char *mesg)

/* ERQ received a bad message in <mesg> - print some diagnostics.
 */

{
    fprintf(stderr, "%s Bad request %d\n", time_stamp(), mesg[8]);
    fprintf(stderr, "%s %x %x %x %x %x %x %x %x %x\n", time_stamp(),
        mesg[0], mesg[1], mesg[2], mesg[3], mesg[4],
        mesg[5], mesg[6], mesg[7], mesg[8]);
    fprintf(stderr, "%s %c %c %c %c %c %c %c %c %c\n", time_stamp(),
        mesg[0], mesg[1], mesg[2], mesg[3], mesg[4],
        mesg[5], mesg[6], mesg[7], mesg[8]);
    reply1(get_handle(mesg), "", 0);
} /* bad_request() */

/*-------------------------------------------------------------------------*/
void
reply1 (int32 handle, const void *data, int32 len)

/* Compose a reply message from <handle> and the <len> bytes of <data>
 * and send it back to the driver.
 */

{
    char reply[ERQ_MAX_REPLY];

    write_32(reply,   len+8);
    write_32(reply+4, handle);
    memcpy(reply+8, data, len);
    write1(reply, len+8);
} /* reply1() */

/*-------------------------------------------------------------------------*/
void
reply1keep (int32 handle, const void *data, int32 len)

/* Compose a reply message from <handle> and the <len> bytes of <data>
 * and send it back to the driver. The message will be an _KEEP_HANDLE
 * message.
 */

{
    char reply[ERQ_MAX_REPLY];

    write_32(reply,   len+12);
    write_32(reply+4, ERQ_HANDLE_KEEP_HANDLE);
    write_32(reply+8, handle);
    memcpy(reply+12, data, len);
    write1(reply, len+12);
} /* reply1keep() */

/*-------------------------------------------------------------------------*/
void
replyn (int32 handle, int keep, int num, ...)

/* Compose and send to the driver a replymessage for <handle> with
 * the <num> data arguments concatenated as body. If <keep> is true,
 * a _KEEP_HANDLE message is composed.
 *
 * Each data argument is a tuple (char *data, int len).
 */

{
    char reply[ERQ_MAX_REPLY];
    char *p;
    int total;
    va_list va;

    /* Determine the size of the header */
    total = (keep ? 12 : 8);
    p = reply+total;

    /* Catenate the data arguments */
    va_start(va, num);
    while (num-- > 0 && total < ERQ_MAX_REPLY)
    {
        char *data;
        int len;

        data = va_arg(va, char *);
        len = va_arg(va, int);
        if (total + len > ERQ_MAX_REPLY)
        {
            fprintf(stderr, "%s Too much data in replyn(): %d bytes omitted.\n"
                          , time_stamp(), total + len - ERQ_MAX_REPLY);
            len = ERQ_MAX_REPLY - total;
        }
        memcpy(p, data, len);
        p += len;
        total += len;
    }
    va_end(va);

    if (num > 0)
    {
        fprintf(stderr, "%s Too much data in replyn(): Remaining %d "
                        "data blocks omitted.\n"
                      , time_stamp(), num);
    }

    /* Create the header */
    write_32(reply, total);
    if (keep)
    {
        write_32(reply+4, ERQ_HANDLE_KEEP_HANDLE);
        write_32(reply+8, handle);
    }
    else
    {
        write_32(reply+4, handle);
    }

    /* Send the reply */
    write1(reply, total);
} /* replyn() */

/*-------------------------------------------------------------------------*/
void
reply_errno (int32 handle)

/* Send a (errcode, errno) message to the driver for <handle>.
 */

{
    char mesg[2];

    switch(errno)
    {
    case EWOULDBLOCK:
#if EAGAIN != EWOULDBLOCK
    case EAGAIN:
#endif
        mesg[0] = ERQ_E_WOULDBLOCK;
        break;

    case EPIPE:
        mesg[0] = ERQ_E_PIPE;
        break;

    default:
        mesg[0] = ERQ_E_UNKNOWN;
        break;
    }
    mesg[1] = errno;
    reply1(handle, mesg, 2);
} /* reply_errno() */

/*-------------------------------------------------------------------------*/
int
writen (int fd, char *mesg, int len, struct equeue_s **qpp)

/* Send or queue the message <mesg> (length <len> bytes) to <fd>.
 * If *<qpp> is non-NULL, the message is queued immediately.
 * Otherwise, the function tries to send as much of the message
 * as possible, and the queues whatever is left.
 */

{
    int l = 0;

    XPRINTF((stderr, "%s writen(%d, %p:%d, %p (-> %p) ))\n"
                   , time_stamp(), fd, mesg, len, qpp, *qpp));
    if (!(*qpp))
    {
        /* Send as much of the message as possible */
        do
            l = write(fd, mesg, len);
        while (l == -1 && errno == EINTR);
        XPRINTF((stderr, "%s   Wrote %d bytes.\n", time_stamp(), l));
        if (l < 0 || l == len)
            return l;
        mesg += l;
        len -= l;
    }

    if (!len)
        return 0;

    XPRINTF((stderr, "%s   Queuing data %p:%d\n", time_stamp(), mesg, len));
    add_to_queue(qpp, mesg, len, 0);
    return l;
} /* writen() */

/*-------------------------------------------------------------------------*/
void
write1 (void *mesg, int len)

/* Write the <len> bytes of <mesg> to stdout, ie to the driver.
 */

{
    int l;

    l = writen(1, mesg, len, &stdout_queue);
    if (l < 0)
    {
        int myerrno = errno;
        fprintf(stderr, "%s Error occurred on driver socket, errno=%d",
                time_stamp(), errno);
        errno = myerrno;
        perror(" ");
        die();
    }
#if ERQ_DEBUG > 0
    if (l != len)
        fprintf( stderr
               , "%s Driver-erq socket blocked, queueing %d bytes\n"
               , time_stamp(), len);
#endif
} /* write1() */

/***************************************************************************/

