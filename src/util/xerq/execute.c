/*---------------------------------------------------------------------------
 * XErq - Command Execution
 * (C) Copyright 1995 by Brian Gerst.
 * (C) Copyright 2001 by Brian Gerst, Frank Kirschner, Lars Duening.
 *---------------------------------------------------------------------------
 * The functions here implement the subcommand features of the ERQ.
 *
 * ERQ_EXECUTE and _SPAWN commands are controlled with a child_t structure.
 * _SPAWN commands are additionally given two sockets so that the driver
 * can communicate with them.
 *---------------------------------------------------------------------------
 */

#include "defs.h"

/*-------------------------------------------------------------------------*/
static int
execute (char *buf, int buflen, char *status, int *esockets, int keep_sockets)

/* Execute the command <buf> (length <buflen> bytes) and return the pid
 * on success, and 0 on failure. *<status> is set to the ERQ_E_ code.
 *
 * If <esockets> is non-NULL, it points to an int[4] which is filled
 * with two socketpairs to communicate with the execute command:
 *   esocket[0]: stdin+stdout of the child, closed in the caller
 *   esocket[2]: stderr of the child, closed in the caller
 *   esocket[1]: callers socket to stdin+stdout of the child
 *   esocket[3]: callers socket to stderr of the child
 *
 * A second purpose of the socketpair is to synchronize the parent with
 * the child, so that the child runs after the parent completed the setup.
 *
 * If the sockets are used for just this purpose, <keep_sockets> is FALSE
 * and the sockets in the child are closed after synchronisation (the caller
 * parent has to close its copies, too). If <keep_sockets> is TRUE, the
 * child keeps the sockets and moves them over to the stdio/err channels.
 */

{
    char path[256], argbuf[1024], *p, *args[96], **argp, c;
    pid_t pid;
    int quoted;

    XPRINTF((stderr, "%s execute('%s':%d)\n", time_stamp(), buf, buflen));

    quoted = 0;
    status[0] = ERQ_E_FORKFAIL; /* Good default */
    status[1] = 0;
    if (buflen >= sizeof argbuf)
    {
        status[0] = ERQ_E_ARGLENGTH;
        return 0;
    }

    /* Split the commandline into words, store them in argbuf, and store
     * the index pointers in args[].
     * argp points to the args[] slot for the next word; argp[-1]
     * is the word currently filled.
     */
    argp = &args[0];
    *argp++ = p = argbuf;
    while (--buflen >= 0)
    {
        c = *buf++;
        if (c == '\\')
        {
            if (--buflen >= 0) {
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
            XPRINTF((stderr, "%s   arg[%d]: '%s'\n"
                           , time_stamp(), argp - args - 2, argp[-2]));
            if (argp - args >= sizeof args/sizeof args[0]) {
                status[0] = ERQ_E_ARGNUMBER;
                return 0;
            }
            while( ( (c = *buf) == ' ' || c == '\t') && --buflen >= 0)
                buf++;
        }
    }
    *p++ = '\0';
    *argp++ = 0;

    XPRINTF((stderr, "%s   arg[%d]: '%s'\n"
                   , time_stamp(), argp - args - 2, argp[-2]));

    /* Make sure the command to execute is legal (ie does not try
     * get out of its ERQ_DIR sandbox.
     */
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

    /* Create the sockets to communicate with the caller */
    if (esockets)
    {
        if(socketpair(AF_UNIX, SOCK_STREAM, 0, esockets) < 0)
        {
            perror("socketpair");
            status[0] = ERQ_E_FORKFAIL;
            status[1] = errno;
            return 0;
        }

        if(socketpair(AF_UNIX, SOCK_STREAM, 0, esockets + 2) < 0)
        {
            perror("socketpair");
            close(esockets[0]);
            close(esockets[1]);
            status[0] = ERQ_E_FORKFAIL;
            status[1] = errno;
            return 0;
        }
    }

    pid = fork();
    if (!pid)
    {
        /* This is the child */

        close(0);
        close(1);

        if (esockets)
        {
            char ch, expected;
            int num;

            expected = getpid() & 0xff;
            XPRINTF((stderr, "%s Child process waits for go-ahead byte '%02x'.\n"
                           , time_stamp(), expected & 0xff));

            do {
                num = read(esockets[0], &ch, sizeof(c));
            } while (num < 0 && errno == EINTR);

            if (num < 0)
            {
                int myerrno = errno;

                fprintf(stderr, "%s Child couldn't receive go-ahead byte, errno %d"
                              , time_stamp(), myerrno);
                errno = myerrno;
                perror(" ");
            }

            if (ch != expected)
            {
                fprintf(stderr, "%s Child received go-ahead byte '%02x', expected '%02x'\n"
                              , time_stamp(), ch & 0xff, expected & 0xff);
            }

            XPRINTF((stderr, "%s Child resumes.\n", time_stamp()));

            if (keep_sockets)
            {
                dup2(esockets[0], 0);
                dup2(esockets[0], 1);
                dup2(esockets[2], 2);
            }
            close(esockets[0]);
            close(esockets[1]);
            close(esockets[2]);
            close(esockets[3]);
        }

        execv(path, args);
        _exit(1);
    }

    /* This is the caller */

    XPRINTF((stderr, "%s   Forked process %d.\n", time_stamp(), pid));

    if (esockets)
    {
        /* Not our sockets */
        close(esockets[0]);
        close(esockets[2]);
    }

    if (pid < 0)
    {
        /* Error */
        if (esockets)
        {
            close(esockets[1]);
            close(esockets[3]);
        }
        status[0] = ERQ_E_FORKFAIL;
        status[1] = errno;
        return 0;
    }

    return pid;
} /* execute() */

/*-------------------------------------------------------------------------*/
static child_t *
new_child (void)

/* Create a new child_t structure, give it a ticket and add it to
 * the childs list.
 * Result is the new structure.
 */

{
    child_t *chp;

    chp = malloc(sizeof(struct child_s));
    XPRINTF((stderr, "%s New child %p.\n", time_stamp(), chp));
    chp->next = childs;
    chp->ticket.seq = (seq_number += seq_interval);
    chp->ticket.rnd = get_ticket();
    chp->fd = NULL;
    chp->err = NULL;
    chp->status = CHILD_RUNNING;
    chp->pid = 0;
    childs = chp;
    return chp;
} /* new_child() */

/*-------------------------------------------------------------------------*/
static void
free_child (struct child_s *chp)

/* Remove the child <chp> from the list of childs, if necessary, then
 * free the structure.
 */

{
    child_t **chpp;
    int found = 0;

    XPRINTF((stderr, "%s Freeing child %p.\n", time_stamp(), chp));
    for (chpp = &childs; *chpp; chpp=&(*chpp)->next)
    {
        if (*chpp != chp)
            continue;
        *chpp = chp->next;
        XPRINTF((stderr, "%s   Unlinking child %p.\n", time_stamp(), chp));
        found = 1;
        break;
    }
    if (!found)  /* child was removed already */
        fprintf(stderr, "%s Child %p to free not found.\n", time_stamp(), chp);
    else
        free(chp);
} /* free_child() */

/*-------------------------------------------------------------------------*/
void
remove_child (child_t *chp)

/* The child <chp> has exited. Get the exit status and send the proper
 * message to the driver, then clean up the child and remove it.
 */

{
    char mesg[2];
    child_t *chtmp;
    int found = 0;

    XPRINTF((stderr, "%s Removing child %p.\n", time_stamp(), chp));
    
    for (chtmp = childs; chtmp; chtmp = chtmp->next)
        if (chtmp == chp)
        {
            found = 1;
            break;
        }

    if (!found)  /* child was removed already */
    {
        XPRINTF((stderr, "%s   Child %p removed already.\n", time_stamp(), chp));
        return;
    }

    switch(chp->type)
    {
    case CHILD_FORK:
    case CHILD_EXECUTE:
      {
        mesg[0] = ERQ_OK;
        break;
      }

    case CHILD_SPAWN:
      {
        mesg[0] = ERQ_EXITED;
        if (chp->fd)
        {
            XPRINTF((stderr, "%s   Closing 'fd' socket %p\n", time_stamp(), chp->fd));
            if (!read_socket(chp->fd, 0))
                close_socket(chp->fd);
        }
        if (chp->err)
        {
            XPRINTF((stderr, "%s   Closing 'err' socket %p\n", time_stamp(), chp->err));
            if (!read_socket(chp->err, 0))
                close_socket(chp->err);
        }
        break;
      }
    }

    /* Send an exit message for non-forked children */
    if (chp->type != CHILD_FORK)
    {
        if (WIFEXITED(chp->return_code))
        {
            mesg[1] = WEXITSTATUS(chp->return_code);
        }
        else if (WIFSIGNALED(chp->return_code))
        {
            mesg[0] = ERQ_SIGNALED;
            mesg[1] = WTERMSIG(chp->return_code);
        }
        else
        {
            mesg[0] = ERQ_E_UNKNOWN;
        }
        XPRINTF((stderr, "%s   Sending exit message.\n", time_stamp()));
        reply1(chp->handle, mesg, 2);
    }

    free_child(chp);
} /* remove_child() */

/*-------------------------------------------------------------------------*/
void
erq_fork (char *mesg, int msglen)

/* ERQ_FORK: fire-and-(almost)forget a command.
 */

{
    char status[2];
    child_t *chp;
    pid_t pid;

    chp = new_child();
    chp->type = CHILD_FORK;
    chp->handle = 0;

    if (0 != (pid = execute(&mesg[9], msglen-9, status, NULL, 0)))
    {
        chp->pid = pid;
        status[0] = ERQ_OK;
    }
    reply1(get_handle(mesg), status, 2);
} /* erq_fork() */

/*-------------------------------------------------------------------------*/
void
erq_execute (char *mesg, int msglen)

/* ERQ_EXECUTE: start a command and keep an eye on its progress.
 */

{
    char status[2];
    int esockets[4];
    pid_t pid;
    child_t *chp;

    chp = new_child();
    chp->type = CHILD_EXECUTE;
    chp->handle = get_handle(mesg);

    if (0 != (pid = execute(&mesg[9], msglen-9, status, esockets, 0)))
    {
        char ga_data;
        int rc;

        /* The parent */
        chp->pid = pid;

        /* Send the GoAhead signal to the child process */
        ga_data = pid & 0xff;
        XPRINTF((stderr, "%s Sending go-ahead byte '%02x' to child.\n"
                       , time_stamp(), ga_data & 0xff));
        fcntl(esockets[1], F_SETFD, 1);
        fcntl(esockets[1], F_SETFL, O_NONBLOCK);
        do {
            rc = write(esockets[1], &ga_data, sizeof(ga_data));
        } while (rc < 0 && errno == EINTR);

        if (rc < 0)
        {
            int myerrno = errno;

            fprintf(stderr, "%s Couldn't send go-ahead byte '%02x' to child %p, errno %d"
                          , time_stamp(), ga_data & 0xff, chp, myerrno);
            errno = myerrno;
            perror(" ");
        }

        /* We don't need the sockets anymore */
        close(esockets[1]);
        close(esockets[3]);
        return;
    }

    free_child(chp); /* Don't need this in the child process */
    reply1(get_handle(mesg), status, 2);
} /* erq_execute() */

/*-------------------------------------------------------------------------*/
void
erq_spawn (char *mesg, int msglen)

/* ERQ_SPAWN: start a command and communicate with it.
 */

{
    int esockets[4];
    char status[2];
    pid_t pid;

    child_t *chp;

    /* Set up the child */
    chp = new_child();
    chp->type = CHILD_SPAWN;
    chp->handle = get_handle(mesg);

    if (0 != (pid = execute(&mesg[9], msglen-9, status, esockets, 1)))
    {
        char ga_data;
        int rc;

        /* The parent */
        chp->pid = pid;

        /* Make a socket for the stdio */
        chp->fd = new_socket(esockets[1], SOCKET_STDOUT);
        memcpy(&chp->fd->ticket, &chp->ticket, TICKET_SIZE);
        fcntl(esockets[1], F_SETFD, 1);
        fcntl(esockets[1], F_SETFL, O_NONBLOCK);
        chp->fd->handle = chp->handle;

        /* Make a socket for the stderr */
        chp->err = new_socket(esockets[3], SOCKET_STDERR);
        fcntl(esockets[3], F_SETFD, 1);
        fcntl(esockets[3], F_SETFL, O_NONBLOCK);
        chp->err->handle = chp->handle;

        /* Send the GoAhead signal to the child process */
        ga_data = pid & 0xff;
        XPRINTF((stderr, "%s Sending go-ahead byte '%02x' to child.\n"
                       , time_stamp(), ga_data));
        do {
            rc = write(esockets[1], &ga_data, sizeof(ga_data));
        } while (rc < 0 && errno == EINTR);

        if (rc < 0)
        {
            int myerrno = errno;

            fprintf(stderr, "%s Couldn't send go-ahead byte '%02x' to child %p, errno %d"
                          , time_stamp(), ga_data, chp, myerrno);
            errno = myerrno;
            perror(" ");
        }

        status[0] = ERQ_OK;
        replyn(chp->handle, 1, 2,
            status, 1,
            &chp->ticket, TICKET_SIZE);
        return;
    }

    free_child(chp); /* Don't need this in the child process */
    reply1(get_handle(mesg), status, 2);
} /* erq_spawn() */

/*-------------------------------------------------------------------------*/
void
erq_kill (char *mesg, int msglen)

/* ERQ_KILL: Send a signal to one of the children (default is SIGKILL).
 * If applied to a socket, the socket is closed.
 */

{
    ticket_t *ticket;
    child_t *chp;
    socket_t *sp;
    int sig;
    char status;

    /* Check the message */
    switch(msglen-TICKET_SIZE)
    {
    case 9:
        sig = SIGKILL;
        break;
    case 13:
        sig = read_32(mesg+9+TICKET_SIZE);
        break;
    default:
        bad_request(mesg);
        return;
    }

    ticket = (struct ticket_s *) (mesg+9);

    /* Find the command child by the ticket */
    for (chp = childs; chp; chp = chp->next)
        if (!memcmp(ticket, &chp->ticket, TICKET_SIZE))
            break;

    if (chp)
    {
        /* Found it - send the signal */
        XPRINTF((stderr, "%s Kill child %p (pid %d) with signal %d\n"
                       , time_stamp(), chp, chp->pid, sig));
        if (sig >= 0)
            sig = kill(chp->pid, sig);
        status = sig < 0 ? ERQ_E_ILLEGAL : ERQ_OK;
        reply1(get_handle(mesg), &status, 1);
        return;
    }

    /* Maybe its a socket? */
    for (sp = sockets; sp; sp = sp->next)
        if (!memcmp(ticket, &sp->ticket, TICKET_SIZE))
            break;

    if (sp)
    {
        /* Yup: close it */
        XPRINTF((stderr, "%s Close socket %p", time_stamp(), sp));
        close_socket(sp);
        status = ERQ_OK;
        reply1(get_handle(mesg), &status, 1);
        return;
    }

    status = ERQ_E_TICKET;
    reply1(get_handle(mesg), &status, 1);
} /* erq_kill() */

/***************************************************************************/

