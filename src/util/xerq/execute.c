#include "defs.h"

int execute(char *buf, int buflen, char *status, int *esockets)
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
    if (esockets) {
	if(socketpair(AF_UNIX, SOCK_STREAM, 0, esockets) < 0) {
	    perror("socketpair");
	    status[0] = ERQ_E_FORKFAIL;
	    status[1] = errno;
	    return 0;
	}
	if(socketpair(AF_UNIX, SOCK_STREAM, 0, esockets + 2) < 0) {
	    perror("socketpair");
	    close(esockets[0]);
	    close(esockets[1]);
	    status[0] = ERQ_E_FORKFAIL;
	    status[1] = errno;
	    return 0;
	}
    }
    pid = fork();
    if (!pid) {
	close(0);
	close(1);
	if (esockets) {
	    dup2(esockets[0], 0);
	    dup2(esockets[0], 1);
	    dup2(esockets[2], 2);
	    close(esockets[0]);
	    close(esockets[1]);
	    close(esockets[2]);
	    close(esockets[3]);
	}
	execv(path, args);
	_exit(1);
    }
    if (esockets) {
	close(esockets[0]);
	close(esockets[2]);
    }
    if (pid < 0) {
	if (esockets) {
	    close(esockets[1]);
	    close(esockets[3]);
	}
	status[0] = ERQ_E_FORKFAIL;
	status[1] = errno;
	return 0;
    }
    return pid;
}

struct child_s *new_child()
{
    struct child_s *chp;

    chp=(struct child_s *)malloc(sizeof(struct child_s));
    chp->next=childs;
    chp->ticket.seq=(seq_number+=seq_interval);
    chp->ticket.rnd=get_ticket();
    childs=chp;
    return chp;
}

void free_child(struct child_s *chp)
{
    struct child_s **chpp;
    for (chpp=&childs; *chpp; chpp=&(*chpp)->next) {
	if (*chpp!=chp) continue;
	*chpp=chp->next;
	break;
    }
    free(chp);
}

void remove_child(struct child_s *chp)
{
    char mesg[2];
    int found=0;
    struct child_s **chpp;

    for (chpp = &childs; *chpp; chpp = &(*chpp)->next)
        if (*chpp == chp)
        {
            found = 1;
            break;
        }

    if (!found)  /* child was removed already */
        return;
    
    switch(chp->type) {
      case CHILD_EXECUTE: {
	mesg[0]=ERQ_OK;
	break;
      }
      case CHILD_SPAWN: {
	mesg[0]=ERQ_EXITED;
	if (chp->fd) close_socket(chp->fd);
	if (chp->err) close_socket(chp->err);
	break;
      }
    }
    if (WIFEXITED(chp->return_code)) {
	mesg[1]=WEXITSTATUS(chp->return_code);
    } else if (WIFSIGNALED(chp->return_code)) {
	mesg[0]=ERQ_SIGNALED;
	mesg[1]=WTERMSIG(chp->return_code);
    } else {
	mesg[0]=ERQ_E_UNKNOWN;
    }
    reply1(chp->handle, mesg, 2);
    free_child(chp);
}

void erq_fork(char *mesg, int msglen)
{
    char status[2];

    if (execute(&mesg[9], msglen-9, status, 0)) {
	status[0]=ERQ_OK;
    }
    reply1(get_handle(mesg), status, 2);
}

void erq_execute(char *mesg, int msglen)
{
    char status[2];
    pid_t pid;

    if ((pid=execute(&mesg[9], msglen-9, status, 0))) {
	struct child_s *chp;
	chp=new_child();
	chp->pid=pid;
	chp->type=CHILD_EXECUTE;
	chp->status=CHILD_RUNNING;
	chp->handle=get_handle(mesg);
	return;
    }
    reply1(get_handle(mesg), status, 2);
}

void erq_spawn(char *mesg, int msglen)
{
    int esockets[4];
    char status[2];
    pid_t pid;

    if ((pid=execute(&mesg[9], msglen-9, status, esockets))) {
	struct child_s *chp;
	chp=new_child();
	chp->pid=pid;
	chp->type=CHILD_SPAWN;
	chp->status=CHILD_RUNNING;
	chp->handle=get_handle(mesg);
	chp->fd=new_socket(esockets[1], SOCKET_STDOUT);
	memcpy(&chp->fd->ticket, &chp->ticket, TICKET_SIZE);
	fcntl(esockets[1], F_SETFD, 1);
	fcntl(esockets[1], F_SETFL, O_NONBLOCK);
	chp->err=new_socket(esockets[3], SOCKET_STDERR);
	fcntl(esockets[3], F_SETFD, 1);
	fcntl(esockets[3], F_SETFL, O_NONBLOCK);
	chp->fd->handle=chp->handle;
	status[0]=ERQ_OK;
	replyn(chp->handle, 1, 2,
	    status, 1,
	    (char *)&chp->ticket, TICKET_SIZE);
	return;
    }
    reply1(get_handle(mesg), status, 2);
}

void erq_kill(char *mesg, int msglen)
{
    struct ticket_s *ticket;
    struct child_s *chp;
    struct socket_s *sp;
    int sig;
    char status;

    switch(msglen-TICKET_SIZE) {
      case 9:
	sig=SIGKILL;
	break;
      case 13:
	sig=read_32(mesg+9+TICKET_SIZE);
	break;
      default:
	return bad_request(mesg);
    }

    ticket=(struct ticket_s *) (mesg+9);

    for (chp=childs; chp; chp=chp->next)
        if (!memcmp(ticket, &chp->ticket, TICKET_SIZE)) break;

    if (chp) {
        if (sig >= 0) sig=kill(chp->pid, sig);
	status=sig < 0 ? ERQ_E_ILLEGAL : ERQ_OK;
	reply1(get_handle(mesg), &status, 1);
	return;
    }

    for (sp=sockets; sp; sp=sp->next)
	if (!memcmp(ticket, &sp->ticket, TICKET_SIZE)) break;

    if (sp) {
	close_socket(sp);
	status=ERQ_OK;
	reply1(get_handle(mesg), &status, 1);
	return;
    }

    status=ERQ_E_TICKET;
    reply1(get_handle(mesg), &status, 1);
    return;
}
