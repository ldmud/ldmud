/*---------------------------------------------------------------------------
 * XErq - Socket functions.
 * (C) Copyright 1995 by Brian Gerst.
 *---------------------------------------------------------------------------
 * This module implements the communication requests, and also contains
 * the functions handling socket_t structures.
 *---------------------------------------------------------------------------
 */

#include "defs.h"

static void send_auth (auth_t *ap);

/*-------------------------------------------------------------------------*/
socket_t *
new_socket(int fd, char type)

/* Create a socket_t of <type> for <fd>, assign it a ticket and add it to
 * the socket list.
 * Return the pointer to the socket structure.
 */

{
    socket_t *sp;

    sp = malloc(sizeof(struct socket_s));
    sp->next = sockets;
    sp->fd = fd;
    sp->type = type;
    sp->ticket.seq = (seq_number+=seq_interval);
    sp->ticket.rnd = get_ticket();
    sp->queue = NULL;
    sockets = sp;
    return sp;
} /* new_socket() */

/*-------------------------------------------------------------------------*/
static void
free_socket (socket_t *sp)

/* The socket in structure <sp> has been closed already, now remove the
 * structure from the socket list and free it and all still pending
 * data.
 */

{
    socket_t **spp;
    equeue_t *qp, *qnext;

    /* Remove the socket from the socket */
    for (spp = &sockets; *spp; spp = &(*spp)->next)
    {
        if (*spp != sp)
            continue;
        *spp = sp->next;
        break;
    }

    /* Free all pending queued data */
    for (qp = sp->queue; qp;)
    {
        qnext = qp->next;
        free(qp);
        qp = qnext;
    }
    
    free(sp);
} /* free_socket() */

/*-------------------------------------------------------------------------*/
void
add_to_queue (equeue_t **qpp, char *data, int len)

/* Add the message <data> of <len> bytes to the end of queue <qpp>.
 */

{
    equeue_t *new;

    new = malloc(sizeof(struct equeue_s)+len);
    new->len = len;
    new->pos = 0;
    new->next = 0;
    memcpy(&new->buf, data, len);
    while (*qpp)
        qpp = &(*qpp)->next;
    *qpp = new;
} /* add_to_queue() */

/*-------------------------------------------------------------------------*/
int
flush_queue (equeue_t **qpp, int fd)

/* Try to write all pending data from queue <qpp> to socket <fd>.
 * Return -1 if an error occured, and 0 if the queue could be emptied.
 * Single queue elements which have been written are always removed.
 */

{
   int l;
   equeue_t *qp = *qpp, *next;

   do {
        do
            l = write(fd, qp->buf+qp->pos, qp->len);
        while (l==-1 && errno==EINTR);
        if (l < 0)
            return l;
        if (l < qp->len)
        {
            qp->pos += l;
            qp->len -= l;
            break;
        }
        next = qp->next;
        free(qp);
        qp = next;
   } while(next);
   *qpp = qp;
   return 0;
} /* flush_queue() */

/*-------------------------------------------------------------------------*/
void
close_socket (socket_t *sp)

/* Close the socket associated with <sp> and free the structure and all
 * data. The function takes care that child_t structures referencing this
 * socket are updated as well.
 * Before the socket is closed, the proper message is sent back to
 * the driver.
 */

{
    child_t *chp;
    socket_t **spp;
    int found = 0;

    /* Check if the socket is still active */
    for (spp = &sockets; *spp; spp = &(*spp)->next)
        if (*spp == sp)
        {
            found = 1;
            break;
        }

    if (!found) /* socket already closed */
        return;

    /* Depending on the socket type, handle the fluff
     * depending on the socket.
     */
    switch(sp->type)
    {
    case SOCKET_STDOUT:
      {
        for (chp = childs; chp; chp=chp->next)
            if (chp->fd == sp) break;
        if (chp)
            chp->fd = NULL;
        break;
      }

    case SOCKET_STDERR:
      {
        for (chp = childs; chp; chp=chp->next)
            if (chp->err == sp)
                break;
        if (chp)
            chp->err = NULL;
        break;
      }

    case SOCKET_WAIT_AUTH:
    case SOCKET_AUTH:
      {
        auth_t *ap = (auth_t *)sp;
        write_32(ap->buf, ap->pos);
        write_32((ap->buf)+4, ap->s.handle);
        write1(ap->buf, ap->pos);
        break;
      }

    default:
      {
        char r[] = { ERQ_EXITED, 0 };
        reply1(sp->handle, r, sizeof(r));
      }
    }

    close(sp->fd);
    free_socket(sp);
} /* close_socket() */

/*-------------------------------------------------------------------------*/
void
read_socket (socket_t *sp, int rw)

/* The socket <sp> was flagged by select() as ready for reading (<rw> == 0)
 * resp. writing (<rw> == 1). According to the type of socket take the
 * appropriate action.
 *
 * If there is data pending on the socket, try to write it in any case.
 */

{
    char buf[ERQ_MAX_REPLY];
    int num;

    num = 0;
    
    if (sp->queue)
        flush_queue(&sp->queue, sp->fd);

    switch(sp->type)
    {
    case SOCKET_LISTEN:
      {
        /* There is a connection pending */
        char r[] = { ERQ_STDOUT };

        reply1keep(sp->handle, r, sizeof(r));
        sp->type = SOCKET_WAIT_ACCEPT;
        return;
      }

    case SOCKET_WAIT_ACCEPT:
        return;

    case SOCKET_UDP:
      {
        /* Read the data message and pass it on */
        
        struct sockaddr_in addr;
        size_t len;

        do {
            char r[] = { ERQ_STDOUT };

            len = sizeof(addr);
            num = recvfrom(sp->fd, buf, ERQ_MAX_REPLY-20, 0,
                           (struct sockaddr *)&addr, &len);
            if (num < 0 && errno==EINTR)
                  continue;
            if (num <= 0)
                break;
            replyn(sp->handle, 1, 4, 
                r, sizeof(r),
                (char *) &addr.sin_addr.s_addr, 4,
                (char *) &addr.sin_port, 2,
                buf, num);
        } while(1);
        break;
      }

    case SOCKET_WAIT_CONNECT:
      {
        if (rw)
        {
            /* Our opened TCP socket finally connected */

            char r[] = { ERQ_OK };
            
            sp->type = SOCKET_CONNECTED;
            replyn(sp->handle, 1, 2,
                r, sizeof(r),
                (char *) &sp->ticket, TICKET_SIZE);
            return;
        }
        /* FALLTHROUGH */
      }

    case SOCKET_STDOUT:
    case SOCKET_STDERR:
    case SOCKET_CONNECTED:
      {
        /* Read data from the stream and pass it on */
        
        char r_err[] = { ERQ_STDERR };
        char r_out[] = { ERQ_STDOUT };

        do {
            num = read(sp->fd, buf, ERQ_MAX_REPLY-14);
            if (num < 0 && errno == EINTR)
                continue;
            if (num <= 0)
                break;
            replyn(sp->handle, 1, 2,
              (sp->type==SOCKET_LISTEN) ? r_err : r_out, 1, 
              buf, num);
        } while(1);
        break;
      }

    case SOCKET_WAIT_AUTH:
      {
        if (rw)
        {
            /* Our authd connection is finally there */

            sp->type = SOCKET_AUTH;
            send_auth((auth_t *)sp);
            return;
        }
        /* FALLTHROUGH */
      }

    case SOCKET_AUTH:
      {
        auth_t *ap = (struct auth_s *)sp;
        do {
            /* Tell the driver what we got from the authd */

            num = read(ap->s.fd, &ap->buf[ap->pos], sizeof(ap->buf)-ap->pos);
            if (num < 0 && errno == EINTR)
                continue;
            if (num <= 0)
            {
                close_socket(sp);
                return;
            }
            ap->pos += num;
        } while(1);
      }
    } /* switch(socket->type) */

    if (!num)
    {
        /* We got EOF when reading data - the socket is no longer needed.
         */
         
        close_socket(sp);
        return;
    }

    if (errno != EWOULDBLOCK
#if EWOULDBLOCK!=EAGAIN
        && errno != EAGAIN
#endif
        )
    {
        reply_errno(sp->handle);
        close_socket(sp);
    }
} /* read_socket() */

/*-------------------------------------------------------------------------*/
void
erq_send (char *mesg, int msglen)

/* ERQ_SEND: send data to the socket identified by the ticket.
 */

{
    ticket_t *ticket;
    socket_t *sp;
    int num;
    char *data;

    /* Find the socket identified in the message */
    
    if (msglen < 9+TICKET_SIZE)
    {
        char r_arglen[] = { ERQ_E_ARGLENGTH };
        reply1(get_handle(mesg), r_arglen, sizeof(r_arglen));
        return;
    }

    ticket = (struct ticket_s *) (mesg+9);
    data = mesg+9+TICKET_SIZE;
    msglen -= (TICKET_SIZE+9);

    for (sp = sockets; sp; sp = sp->next)
        if (!memcmp(ticket, &sp->ticket, TICKET_SIZE))
            break;

    if (!sp)
    {
        char r_ticket[] = { ERQ_E_TICKET };
        reply1(get_handle(mesg), r_ticket, sizeof(r_ticket));
        return;
    }

    /* Act according to the type of the socket */
    switch(sp->type)
    {
    case SOCKET_STDOUT:
    case SOCKET_CONNECTED:
      {
        /* Send the data to the stream */
        
        do
            num = write(sp->fd, data, msglen);
        while (num == -1 && errno == EINTR);
        break;
      }

    case SOCKET_UDP:
      {
        /* Send the message to the specified address */

        struct sockaddr_in addr;

        addr.sin_family=AF_INET;
        memcpy(&addr.sin_addr.s_addr, data, 4);
        memcpy(&addr.sin_port, data+4, 2);
        data += 6;
        msglen -= 6;
        do
            num = sendto(sp->fd, data, msglen, 0, (struct sockaddr *)&addr,
                sizeof(addr));
        while(num == -1 && errno == EINTR);
        break;
      }

    case SOCKET_WAIT_ACCEPT:
      {
        /* Default to accepting the socket. */
        erq_accept(mesg, msglen);
        return;
      }

    default:
      {
        char r_ticket[] = { ERQ_E_TICKET };
        reply1(get_handle(mesg), r_ticket, sizeof(r_ticket));
        return;
      }
    } /* switch(socket->type) */

    /* Return the success of the action */
    
    if (num == msglen)
    {
        char r_ok[] = { ERQ_OK, 0 };
        reply1(get_handle(mesg), r_ok, sizeof(r_ok));
        return;
    }
    else if (num >= 0)
    {
        add_to_queue(&sp->queue, mesg+num, msglen-num);
/*
        num=htonl(num);
        replyn(get_handle(mesg), 0, 2,
            (char[]) { ERQ_E_INCOMPLETE }, 1,
            (char *)&num, 4);
*/
        return;
    }

    reply_errno(sp->handle);
    if (errno != EWOULDBLOCK
#if EWOULDBLOCK!=EAGAIN
        && errno != EAGAIN
#endif
        ) close_socket(sp);
} /* erq_send() */

/*-------------------------------------------------------------------------*/
void
erq_listen (char *mesg, int msglen)

/* ERQ_LISTEN: Open a socket to listen for connections.
 */

{
    struct sockaddr_in addr;
    socket_t *sp;
    int fd, tmp;

    /* Check if the message is ok */
    if (msglen != 11)
    {
        char r_arglen[] = { ERQ_E_ARGLENGTH, 0 };
        reply1(get_handle(mesg), r_arglen, sizeof(r_arglen));
        return;
    }

    /* Get the port number */
    addr.sin_addr.s_addr=INADDR_ANY;
    addr.sin_family=AF_INET;
    memcpy(&addr.sin_port, mesg + 9, 2);

    /* Open and initialise the socket */
    tmp=((fd=socket(AF_INET, SOCK_STREAM, 0)) < 0);
    tmp=tmp || (fcntl(fd, F_SETFD, 1) < 0);
    tmp=tmp || (fcntl(fd, F_SETFL, O_NONBLOCK) < 0);
    tmp=tmp || (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&msglen,
            sizeof(msglen)) < 0);
    tmp=tmp || (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0);
    tmp=tmp || (listen(fd, 5) < 0);
    if (tmp)
    {
        if (fd >= 0)
            close(fd);
        reply_errno(get_handle(mesg));
        return;
    }

    /* Make the new socket structure */
    sp = new_socket(fd, SOCKET_LISTEN);
    sp->handle = get_handle(mesg);

    {
        char r_ok[] = { ERQ_OK };
        replyn(sp->handle, 1, 2,
            r_ok, sizeof(r_ok),
            (char *) &sp->ticket, TICKET_SIZE);
    }
} /* erq_listen() */

/*-------------------------------------------------------------------------*/
void
erq_open_udp (char *mesg, int msglen)

/* ERQ_OPEN_UDP: Open an UDP socket.
 */

{
    struct sockaddr_in addr;
    socket_t *sp;
    int fd, tmp;

    if (msglen != 11)
    {
        char r_arglen[] = { ERQ_E_ARGLENGTH, 0 };
        reply1(get_handle(mesg), r_arglen, sizeof(r_arglen));
        return;
    }

    /* Get the port to use */
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_family = AF_INET;
    memcpy(&addr.sin_port, mesg + 9, 2);

    /* Open and initialise the socket */
    
    if ((fd=socket(AF_INET, SOCK_DGRAM, 0)) < 0
     || fcntl(fd, F_SETFD, 1) < 0
     ||        fcntl(fd, F_SETFL, O_NONBLOCK) < 0
     ||        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&tmp,
            sizeof(tmp)) < 0
     ||        bind(fd, (struct sockaddr *) &addr, sizeof(addr)) < 0)
    {
        if (fd >= 0)
            close(fd);
        reply_errno(get_handle(mesg));
        return;
    }

    /* Create the new socket structure */
    sp = new_socket(fd, SOCKET_UDP);
    sp->handle = get_handle(mesg);

    {
        char r_ok[] = { ERQ_OK };
        replyn(sp->handle, 1, 2,
            r_ok, sizeof(r_ok),
            (char *) &sp->ticket, TICKET_SIZE);
    }
    return;
} /* erq_open_udp() */

/*-------------------------------------------------------------------------*/
void
erq_open_tcp (char *mesg, int msglen)

/* ERQ_OPEN_TCP: Open a TCP socket and connect it to a given address.
 * If the connection can't be completed immediately (EINPROGRESS), the
 * socket is set to SOCKET_WAIT_CONNECT.
 */

{
    struct sockaddr_in addr;
    socket_t *sp;
    int fd, tmp, status;

    if (msglen != 15)
    {
        char r_arglen[] = { ERQ_E_ARGLENGTH, 0 };
        reply1(get_handle(mesg), r_arglen, sizeof(r_arglen));
        return;
    }

    /* Get the address */
    memcpy(&addr.sin_addr.s_addr, mesg + 9, 4);
    addr.sin_family=AF_INET;
    memcpy(&addr.sin_port, mesg + 13, 2);
    status=SOCKET_CONNECTED;

    /* Initialize the socket */
    if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0
     || fcntl(fd, F_SETFD, 1) < 0
     ||        fcntl(fd, F_SETFL, O_NONBLOCK) < 0)
    {
        if (fd >= 0)
            close(fd);
        reply_errno(get_handle(mesg));
        return;
    }

    /* Try to connect */
    do
        tmp=connect(fd, (struct sockaddr *) &addr, sizeof(addr));
    while (tmp && errno==EINTR);

    if (!tmp)
        status = SOCKET_CONNECTED;
    else if (errno == EINPROGRESS)
    {
        status = SOCKET_WAIT_CONNECT;
        tmp = 0;
    }
    else
    {
        if (fd >= 0)
            close(fd);
        reply_errno(get_handle(mesg));
        return;
    }

    /* Create the new socket structure */
    sp = new_socket(fd, status);
    sp->handle = get_handle(mesg);
    if (status == SOCKET_WAIT_CONNECT)
        return;

    {
        char r_ok[] = { ERQ_OK };
        replyn(sp->handle, 1, 2,
            r_ok, sizeof(r_ok),
            (char *) &sp->ticket, TICKET_SIZE);
    }
    return;
} /* erq_open_tcp() */

/*-------------------------------------------------------------------------*/
void
erq_accept (char *mesg, int msglen)

/* ERQ_ACCEPT: Accept a new, pending connection.
 */

{
    struct sockaddr_in addr;
    socket_t *sp;
    int fd;
    size_t tmp;

    if (msglen != 9+TICKET_SIZE)
    {
        char r_arglen[] = { ERQ_E_ARGLENGTH, 0 };
        reply1(get_handle(mesg), r_arglen, sizeof(r_arglen));
        return;
    }

    /* Find the socket we're accepting on */
    for (sp = sockets; sp; sp = sp->next)
        if (!memcmp(mesg+9, &sp->ticket, TICKET_SIZE))
            break;

    /* Is there really something pending? */
    if (sp->type != SOCKET_WAIT_ACCEPT)
    {
        char r_ticket[] = { ERQ_E_TICKET };
        reply1(get_handle(mesg), r_ticket, sizeof(r_ticket));
        return;
    }

    /* Yes, mark the socket as available again */
    sp->type = SOCKET_LISTEN;

    /* Accept and initialize the socket */
    tmp = sizeof(addr);
    if ((fd=accept(sp->fd, (struct sockaddr *)&addr, &tmp)) < 0
     || fcntl(fd, F_SETFD, 1) < 0
     ||        fcntl(fd, F_SETFL, O_NONBLOCK) < 0)
    {
        if (fd >= 0)
            close(fd);
        reply_errno(get_handle(mesg));
        return;
    }

    /* Create the socket */
    sp = new_socket(fd, SOCKET_CONNECTED);
    sp->handle = get_handle(mesg);

    {
        char r_ok[] = { ERQ_OK };
        replyn(sp->handle = get_handle(mesg), 1, 4,
            r_ok, sizeof(r_ok), 
            (char *) &addr.sin_addr.s_addr, 4,
            (char *) &addr.sin_port, 2,
            (char *) &sp->ticket, TICKET_SIZE);
    }
} /* erq_accept() */

/*-------------------------------------------------------------------------*/
static void
send_auth (auth_t *ap)

/* We got connection to the authd - send our request.
 */

{
    char req[100];
    int num;

    sprintf(req, "%ld,%ld\n", ap->remote_port, ap->local_port);
    do
        num = write(ap->s.fd, req, strlen(req));
    while(num == -1 && errno == EINTR);
    if (num <= 0)
        close_socket((socket_t *)ap);
} /* send_auth() */

/*-------------------------------------------------------------------------*/
void
erq_auth (char *mesg, int msglen)

/* ERQ_AUTH: Connect to a remote authd and request authentification.
 * If the connection can't be completed immediately (EINPROGRESS), the
 * socket is set to SOCKET_WAIT_AUTH.
 */
 
{
    struct sockaddr_in addr;
    auth_t *ap;
    int fd, tmp, status, local_port, remote_port;

    switch(msglen) {
    case 17:
      {
        /* The xerq way */
        unsigned short sh;

        memcpy(&sh, mesg+13, sizeof(sh)); remote_port = ntohs(sh);
        memcpy(&sh, mesg+15, sizeof(sh)); local_port = ntohs(sh);
        
        memcpy(&addr.sin_addr.s_addr, mesg+9, 4);
        addr.sin_family = AF_INET;
        addr.sin_port = htons(AUTH_PORT);
        break;
      }

    case 13+sizeof(addr):
      {
        /* The standard erq way */
        memcpy(&addr, mesg+9, sizeof(addr));
        remote_port = ntohs(addr.sin_port);
        local_port = read_32(mesg+sizeof(addr)+9);
        addr.sin_port = htons(AUTH_PORT);
        break;
      }

    default:
      {
        reply1(get_handle(mesg), "", 0);
        return;
      }
    }

    /* Create and initialize the socket */
    if ((fd=socket(AF_INET, SOCK_STREAM, 0))<0
     || fcntl(fd, F_SETFD, 1)<0
     ||        fcntl(fd, F_SETFL, O_NONBLOCK)<0)
    {
        if (fd >= 0)
            close(fd);
        reply1(get_handle(mesg), "", 0);
        return;
    }

    /* Try to connect to the authd */
    do
        tmp = connect(fd, (struct sockaddr *) &addr, sizeof(addr));
    while(tmp && errno==EINTR);

    if (!tmp)
        status = SOCKET_AUTH;
    else if (errno == EINPROGRESS)
        status = SOCKET_WAIT_AUTH;
    else
    {
        tmp = errno;
        if (fd >= 0)
            close(fd);
        reply1(get_handle(mesg), "", 0);
        return;
    }

    /* Create the auth_t structure and put it into the list of sockets */
    ap = malloc(sizeof(struct auth_s));
    ap->s.next = sockets;
    ap->s.fd = fd;
    ap->s.type = status;
    ap->s.ticket.seq = (seq_number+=seq_number);
    ap->s.ticket.rnd = get_ticket();
    ap->s.handle = get_handle(mesg);
    ap->s.queue = NULL;
    ap->local_port = local_port;
    ap->remote_port = remote_port;
    ap->pos = 8;
    sockets = (struct socket_s *)ap;

    /* If we already got connection, send our request */
    if (status == SOCKET_AUTH)
        send_auth(ap);
} /* erq_auth() */

/***************************************************************************/

