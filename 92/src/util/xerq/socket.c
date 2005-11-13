#include "defs.h"

struct socket_s *new_socket(int fd, char type)
{
    struct socket_s *sp;

    sp=(struct socket_s *)malloc(sizeof(struct socket_s));
    sp->next=sockets;
    sp->fd=fd;
    sp->type=type;
    sp->ticket.seq=(seq_number+=seq_interval);
    sp->ticket.rnd=get_ticket();
    sp->queue=0;
    sockets=sp;
    return sp;
}

void free_socket(struct socket_s *sp)
{
    struct socket_s **spp;
    struct queue_s *qp, *qnext;
    for (spp=&sockets; *spp; spp=&(*spp)->next) {
	if (*spp!=sp) continue;
	*spp=sp->next;
	break;
    }
    for (qp=sp->queue; qp;) {
	qnext=qp->next;
	free(qp);
	qp=qnext;
    }
    free(sp);
}

void add_to_queue(struct queue_s **qpp, char *data, int len)
{
    struct queue_s *new;
    new=(struct queue_s *)malloc(sizeof(struct queue_s)+len);
    new->len=len;
    new->pos=0;
    new->next=0;
    memcpy(&new->buf, data, len);
    while (*qpp) qpp=&(*qpp)->next;
    *qpp=new;
}

int flush_queue(struct queue_s **qpp, int fd)
{
   int l;
   struct queue_s *qp=*qpp, *next;
   do {
	do
	  l=write(fd, qp->buf+qp->pos, qp->len);
	while (l==-1 && errno==EINTR);
	if (l<0) return l;
	if (l<qp->len) {
	    qp->pos+=l;
	    qp->len-=l;
	    break;
	}
	next=qp->next;
	free(qp);
   } while(next);
   *qpp=qp;
   return;
}	

void close_socket(struct socket_s *sp)
{
    struct child_s *chp;
    switch(sp->type) {
      case SOCKET_STDOUT: {
	for (chp=childs; chp; chp=chp->next)
	    if (chp->fd == sp) break;
	if (chp) chp->fd=0;
	break;
      }
      case SOCKET_STDERR: {
	for (chp=childs; chp; chp=chp->next)
	    if (chp->err == sp) break;
	if (chp) chp->err=0;
	break;
      }
      case SOCKET_WAIT_AUTH:
      case SOCKET_AUTH: {
	struct auth_s *ap=(struct auth_s *)sp;
	write_32(ap->buf, ap->pos);
	write_32((ap->buf)+4, ap->s.handle);
	write1(ap->buf, ap->pos);
	break;
      }
      default: {
	reply1(sp->handle, (char[]) { ERQ_EXITED, 0 }, 2);
      }
    }
    close(sp->fd);
    free_socket(sp);
}

void read_socket(struct socket_s *sp, int rw)
{
    char buf[ERQ_MAX_REPLY];
    int num, len;

    if (sp->queue) flush_queue(&sp->queue, sp->fd);
    switch(sp->type) {
      case SOCKET_LISTEN: {
	reply1keep(sp->handle, (char[]) { ERQ_STDOUT }, 1);
	sp->type=SOCKET_WAIT_ACCEPT;
	return;
      }
      case SOCKET_WAIT_ACCEPT:
	return;
      case SOCKET_UDP: {
	struct sockaddr_in addr;
	int len;
	do {
	  len=sizeof(addr);
	  num=recvfrom(sp->fd, buf, ERQ_MAX_REPLY-20, 0,
	    (struct sockaddr *)&addr, &len);
	  if (num<0 && errno==EINTR) continue;
	  if (num<=0) break;
	  replyn(sp->handle, 1, 4,
	      (char[]) { ERQ_STDOUT }, 1,
	      (char *) &addr.sin_addr.s_addr, 4,
	      (char *) &addr.sin_port, 2,
	      buf, num);
	} while(1);
	break;
      }
      case SOCKET_WAIT_CONNECT: {
	if (rw) {
	    sp->type=SOCKET_CONNECTED;
	    replyn(sp->handle, 1, 2,
		(char[]) { ERQ_OK }, 1,
		(char *) &sp->ticket, TICKET_SIZE);
	    return;
	}
	/* fall through */
      }
      case SOCKET_STDOUT:
      case SOCKET_STDERR:
      case SOCKET_CONNECTED: {
	do{
	  num=read(sp->fd, buf, ERQ_MAX_REPLY-14);
	  if (num<0 && errno==EINTR) continue;
	  if (num<=0) break;
	  replyn(sp->handle, 1, 2,
	      sp->type==SOCKET_LISTEN ?
	        (char[]) { ERQ_STDERR } :
		(char[]) { ERQ_STDOUT }, 1,
	      buf, num);
	} while(1);
	break;
      }
      case SOCKET_WAIT_AUTH: {
        if (rw) {
	    sp->type=SOCKET_AUTH;
	    send_auth((struct auth_s *)sp);
	    return;
	}
	/* fall through */
      }
      case SOCKET_AUTH: {
        struct auth_s *ap=(struct auth_s *)sp;
	int pos;
	do {
	  num=read(ap->s.fd, &ap->buf[ap->pos], sizeof(ap->buf)-ap->pos);
	  if (num<0 && errno==EINTR) continue;
	  if (num<=0) {
	    close_socket(sp);
	    return;
	  }
	  ap->pos+=num;
	} while(1);
      }
    }
    if (!num) {
	close_socket(sp);
	return;
    }
    if (errno!=EWOULDBLOCK
#if EWOULDBLOCK!=EAGAIN
	&& errno!=EAGAIN
#endif
	)
    {
	reply_errno(sp->handle);
	close_socket(sp);
    }
}

void erq_send(char *mesg, int msglen)
{
    struct ticket_s *ticket;
    struct socket_s *sp;
    int num, type;
    char *data;

    if (msglen < 9+TICKET_SIZE) {
	reply1(get_handle(mesg), (char[]) { ERQ_E_ARGLENGTH }, 1);
	return;
    }

    ticket=(struct ticket_s *) (mesg+9);
    data=mesg+9+TICKET_SIZE;
    msglen-=(TICKET_SIZE+9);

    for (sp=sockets; sp; sp=sp->next)
        if (!memcmp(ticket, &sp->ticket, TICKET_SIZE)) break;

    if (!sp) {
	reply1(get_handle(mesg), (char[]) { ERQ_E_TICKET }, 1);
	return;
    }
    switch(sp->type) {
      case SOCKET_STDOUT:
      case SOCKET_CONNECTED: {
	do
	  num=write(sp->fd, data, msglen);
	while (num==-1 && errno==EINTR);
	break;
      }
      case SOCKET_UDP: {
	struct sockaddr_in addr;
	addr.sin_family=AF_INET;
	memcpy(&addr.sin_addr.s_addr, data, 4);
	memcpy(&addr.sin_port, data+4, 2);
	data+=6;
	msglen-=6;
	do
	  num=sendto(sp->fd, data, msglen, 0, (struct sockaddr *)&addr,
	      sizeof(addr));
	while(num==-1 && errno==EINTR);
	break;
      }
      case SOCKET_WAIT_ACCEPT: {
	erq_accept(mesg, msglen);
	return;
      }
      default:
	reply1(get_handle(mesg), (char[]) { ERQ_E_TICKET }, 1);
	return;
    }
    if (num==msglen) {
	reply1(get_handle(mesg), (char[]) { ERQ_OK, 0 }, 2);
	return;
    } else if (num>=0) {
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
    if (errno!=EWOULDBLOCK
#if EWOULDBLOCK!=EAGAIN
	&& errno!=EAGAIN
#endif
	) close_socket(sp);
}

void erq_listen(char *mesg, int msglen)
{
    struct ticket_s ticket;
    struct sockaddr_in addr;
    struct socket_s *sp;
    int fd, tmp;

    if (msglen != 11) {
	reply1(get_handle(mesg), (char[]) { ERQ_E_ARGLENGTH, 0 }, 2);
	return;
    }

    addr.sin_addr.s_addr=INADDR_ANY;
    addr.sin_family=AF_INET;
    memcpy(&addr.sin_port, mesg + 9, 2);

    tmp=((fd=socket(AF_INET, SOCK_STREAM, 0)) < 0);
    tmp=tmp || (fcntl(fd, F_SETFD, 1) < 0);
    tmp=tmp || (fcntl(fd, F_SETFL, O_NONBLOCK) < 0);
    tmp=tmp || (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&msglen,
	    sizeof(msglen)) < 0);
    tmp=tmp || (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0);
    tmp=tmp || (listen(fd, 5) < 0);
    if (tmp)
/*
    if ((fd=socket(AF_INET, SOCK_STREAM, 0)) < 0 ||
	fcntl(fd, F_SETFD, 1) < 0 ||
	fcntl(fd, F_SETFL, O_NONBLOCK) < 0 ||
	setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&tmp,
	    sizeof(tmp)) < 0 ||
	bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0 ||
	listen(fd, 5) < 0)
*/
    {
	if (fd >= 0) close(fd);
	return reply_errno(get_handle(mesg));
    }

    sp=new_socket(fd, SOCKET_LISTEN);
    sp->handle=get_handle(mesg);
    replyn(sp->handle, 1, 2,
	(char[]) { ERQ_OK }, 1,
	(char *) &sp->ticket, TICKET_SIZE);
    return;
}

void erq_open_udp(char *mesg, int msglen)
{
    struct ticket_s ticket;
    struct sockaddr_in addr;
    struct socket_s *sp;
    int fd, tmp;

    if (msglen != 11) {
	reply1(get_handle(mesg), (char[]) { ERQ_E_ARGLENGTH, 0 }, 2);
	return;
    }

    addr.sin_addr.s_addr=INADDR_ANY;
    addr.sin_family=AF_INET;
    memcpy(&addr.sin_port, mesg + 9, 2);

    if ((fd=socket(AF_INET, SOCK_DGRAM, 0)) < 0 ||
	fcntl(fd, F_SETFD, 1) < 0 ||
	fcntl(fd, F_SETFL, O_NONBLOCK) < 0 ||
	setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&tmp,
	    sizeof(tmp)) < 0 ||
	bind(fd, (struct sockaddr *) &addr, sizeof(addr)) < 0)
    {
	if (fd >= 0) close(fd);
	return reply_errno(get_handle(mesg));
    }

    sp=new_socket(fd, SOCKET_UDP);
    sp->handle=get_handle(mesg);
    replyn(sp->handle, 1, 2,
	(char[]) { ERQ_OK }, 1,
	(char *) &sp->ticket, TICKET_SIZE);
    return;
}

void erq_open_tcp(char *mesg, int msglen)
{
    struct ticket_s ticket;
    struct sockaddr_in addr;
    struct socket_s *sp;
    int fd, tmp, status;

    if (msglen != 15) {
	reply1(get_handle(mesg), (char[]) { ERQ_E_ARGLENGTH, 0 }, 2);
	return;
    }

    memcpy(&addr.sin_addr.s_addr, mesg + 9, 4);
    addr.sin_family=AF_INET;
    memcpy(&addr.sin_port, mesg + 13, 2);
    status=SOCKET_CONNECTED;

    if ((fd=socket(AF_INET, SOCK_STREAM, 0)) < 0 ||
	fcntl(fd, F_SETFD, 1) < 0 ||
	fcntl(fd, F_SETFL, O_NONBLOCK) < 0)
    {
	if (fd >= 0) close(fd);
	return reply_errno(get_handle(mesg));
    }
    do
	tmp=connect(fd, (struct sockaddr *) &addr, sizeof(addr));
    while (tmp && errno==EINTR);
    if (!tmp) status=SOCKET_CONNECTED;
    else if (errno==EINPROGRESS) {
	status=SOCKET_WAIT_CONNECT;
	tmp=0;
    } else {
	if (fd >= 0) close(fd);
	return reply_errno(get_handle(mesg));
    }

    sp=new_socket(fd, status);
    sp->handle=get_handle(mesg);
    if (status==SOCKET_WAIT_CONNECT) return;
    replyn(sp->handle, 1, 2,
	(char[]) { ERQ_OK }, 1,
	(char *) &sp->ticket, TICKET_SIZE);
    return;
}

void erq_accept(char *mesg, int msglen)
{
    struct ticket_s ticket;
    struct sockaddr_in addr;
    struct socket_s *sp;
    int fd, tmp, status;

    if (msglen != 9+TICKET_SIZE) {
	reply1(get_handle(mesg), (char[]) { ERQ_E_ARGLENGTH, 0 }, 2);
	return;
    }

    for (sp=sockets; sp; sp=sp->next)
        if (!memcmp(mesg+9, &sp->ticket, TICKET_SIZE)) break;

    if (sp->type != SOCKET_WAIT_ACCEPT) {
	reply1(get_handle(mesg), (char[]) { ERQ_E_TICKET }, 1);
	return;
    }
    sp->type=SOCKET_LISTEN;

    tmp=sizeof(addr);
    if ((fd=accept(sp->fd, (struct sockaddr *)&addr, &tmp)) < 0 ||
	fcntl(fd, F_SETFD, 1) < 0 ||
	fcntl(fd, F_SETFL, O_NONBLOCK) < 0)
    {
	if (fd >= 0) close(fd);
	return reply_errno(get_handle(mesg));
    }

    sp=new_socket(fd, SOCKET_CONNECTED);
    sp->handle=get_handle(mesg);
    replyn(sp->handle=get_handle(mesg), 1, 4,
	(char[]) { ERQ_OK }, 1,
	(char *) &addr.sin_addr.s_addr, 4,
	(char *) &addr.sin_port, 2,
	(char *) &sp->ticket, TICKET_SIZE);
    return;
}

void erq_auth(char *mesg, int msglen)
{
    struct sockaddr_in addr;
    struct socket_s *sp;
    struct auth_s *ap;
    int32 host;
    int fd, tmp, status, local_port, remote_port;

    switch(msglen) {
      case 17: { /* new way */
        remote_port=ntohs(*(unsigned short *)(mesg+13));
	local_port=ntohs(*(unsigned short *)(mesg+15));
	
	memcpy(&addr.sin_addr.s_addr, mesg+9, 4);
	addr.sin_family=AF_INET;
	addr.sin_port=htons(AUTH_PORT);
	break;
      }
      case 13+sizeof(addr): {
	memcpy(&addr, mesg+9, sizeof(addr));
	remote_port=ntohs(addr.sin_port);
	local_port=read_32(mesg+sizeof(addr)+9);
	addr.sin_port=htons(AUTH_PORT);
	break;
      }
      default: {
	reply1(get_handle(mesg), "", 0);
	return;
      }
    }

    if ((fd=socket(AF_INET, SOCK_STREAM, 0))<0 ||
	fcntl(fd, F_SETFD, 1)<0 ||
	fcntl(fd, F_SETFL, O_NONBLOCK)<0)
    {
	if (fd >= 0) close(fd);
	reply1(get_handle(mesg), "", 0);
	return;
    }
    do
	tmp=connect(fd, (struct sockaddr *) &addr, sizeof(addr));
    while(tmp && errno==EINTR);
    if (!tmp) status=SOCKET_AUTH;
    else if (errno==EINPROGRESS) status=SOCKET_WAIT_AUTH;
    else {
	tmp=errno;
	if (fd >= 0) close(fd);
	reply1(get_handle(mesg), "", 0);
	return;
    }

    ap=(struct auth_s *)malloc(sizeof(struct auth_s));
    ap->s.next=sockets;
    ap->s.fd=fd;
    ap->s.type=status;
    ap->s.ticket.seq=(seq_number+=seq_number);
    ap->s.ticket.rnd=get_ticket();
    ap->s.handle=get_handle(mesg);
    ap->local_port=local_port;
    ap->remote_port=remote_port;
    ap->pos=8;
    sockets=(struct socket_s *)ap;

    if (status==SOCKET_AUTH) send_auth(ap);
    return;
}

void send_auth(struct auth_s *ap)
{
    char req[100];
    int num;

    sprintf(req, "%ld,%ld\n", ap->remote_port, ap->local_port);
    do
	num=write(ap->s.fd, req, strlen(req));
    while(num==-1 && errno==EINTR);
    if (num<=0) close_socket((struct socket_s *)ap);
}

