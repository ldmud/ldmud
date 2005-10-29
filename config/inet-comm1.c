#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#define	TELOPTS
#include <arpa/telnet.h>
#include <netdb.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#ifndef sun
#include <fcntl.h>
#endif
#include "interpret.h"
#include "comm.h"
#include "object.h"
#include "config.h"
#include "sent.h"

extern char *xalloc(), *string_copy();
extern int d_flag;

extern void remove_interactive(), add_ref();

extern struct value *clone_object();

extern void logon(), debug_message(), fatal(), free_sentence();

#define MAX_PLAYERS	40

struct interactive *all_players[MAX_PLAYERS];

extern int errno;

fd_set readfds;
int nfds = 0;
int num_player;

/*
 * Interprocess communication interface to the backend.
 */

static s;
extern int port_number;

void prepare_ipc() {
    struct sockaddr_in sin;
    struct hostent *hp;
    int tmp;
    char host_name[100];

    if (gethostname(host_name, sizeof host_name) == -1) {
        perror("gethostname");
	fatal("Error in gethostname()\n");
    }
    hp = gethostbyname(host_name);
    if (hp == 0) {
	(void)fprintf(stderr, "gethostbyname: unknown host.\n");
	exit(1);
    }
    memset((char *)&sin, '\0', sizeof sin);
    memcpy((char *)&sin.sin_addr, hp->h_addr, hp->h_length);
    sin.sin_port = htons(port_number);
    sin.sin_family = hp->h_addrtype;
    sin.sin_addr.s_addr = INADDR_ANY;
    s = socket(hp->h_addrtype, SOCK_STREAM, 0);
    if (s == -1) {
	perror("socket");
	abort();
    }
    tmp = 1;
    if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR,
		    (char *) &tmp, sizeof (tmp)) < 0) {
	perror ("setsockopt");
	exit (1);
    }
    if (bind(s, &sin, sizeof sin) == -1) {
	if (errno == EADDRINUSE)
	    debug_message("Socket already bound!\n");
	else {
	    perror("bind");
	    abort();
	}
    }
    if (listen(s, 5) == -1) {
	perror("listen");
	abort();
    }
    tmp = 1;
#ifdef sun
    if (ioctl(s, FIONBIO, &tmp) == -1) {
	perror("ioctl socket FIONBIO");
	abort();
    }
#else /* sun */
    if (fcntl(s, F_SETFL, FNDELAY) == -1) {
	perror("ioctl socket FIONBIO");
	abort();
    }
#endif /* sun */
    signal(SIGPIPE, SIG_IGN);
}

/*
 * This one is called when shutting down the MUD.
 */
void ipc_remove() {
    (void)printf("Shutting down ipc...\n");
    close(s);
}

/*VARARGS1*/
void add_message(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
     char *fmt;
{
    char buff[10000];		/* Kludgy! Hope this is enough ! */
    char buff2[sizeof buff];
    struct interactive *ip;
    int n, offset, chunk, length;
    int from, to;

    if (command_giver == 0) {
	debug_message("command_giver == 0. Message:\n");
	debug_message(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
	return;
    }
    ip = command_giver->interactive;
    if (ip == 0)
	return;
    if (ip->out_portal) {
	buff[0] = ']';
	(void)sprintf(buff + 1, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    } else {
	(void)sprintf(buff, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    }
    length = strlen(buff);
    /*
     * Always check that your arrays are big enough ! :-)
     */
    if (length > sizeof buff)
	fatal("To long message!\n");
    if (ip->snoop_by) {
	struct object *save = command_giver;
	command_giver = ip->snoop_by->ob;
	add_message("%%%s", buff);
	command_giver = save;
    }
    if (d_flag)
	debug_message("[%s(%d)]: %s", command_giver->name, length, buff);
    /*
     * Insert CR after all NL.
     */
    for (from = 0, to = 0; to < sizeof buff2 && buff[from] != '\0';) {
	if (buff[from] == '\n')
	    buff2[to++] = '\r';
	buff2[to++] = buff[from++];
    }
    buff2[to++] = '\0';
    length = to-1;
    /*
     * We split up the message into something smaller than the max size.
     */
    for (offset=0; length > 0; offset += chunk, length -= chunk) {
	chunk = length;
	if (chunk > MAX_SOCKET_PACKET_SIZE)
	    chunk = MAX_SOCKET_PACKET_SIZE;
	if ((n = write(ip->socket, buff2 + offset, chunk)) == -1) {
	    if (errno == EMSGSIZE) {
		debug_message("comm1: write EMSGSIZE.\n");
		return;
	    }
	    if (errno == EINVAL) {
		debug_message("comm1: write EINVAL.\n");
		if (!ip->closing)
		    remove_interactive(ip->ob);
		return;
	    }
	    if (errno == ENETUNREACH) {
		debug_message("comm1: write ENETUNREACH.\n");
		if (!ip->closing)
		    remove_interactive(ip->ob);
		return;
	    }
	    if (errno == EHOSTUNREACH) {
		debug_message("comm1: write EHOSTUNREACH.\n");
		if (!ip->closing)
		    remove_interactive(ip->ob);
		return;
	    }
	    if (errno == EPIPE) {
		debug_message("comm1: write EPIPE detected\n");
		if (!ip->closing)
		    remove_interactive(ip->ob);
		return;
	    }
	    if (errno == EWOULDBLOCK) {
		debug_message("comm1: write EWOULDBLOCK. Message discarded.\n");
		return;
	    }
	    if (errno == ECONNREFUSED) {
		debug_message("comm1: write ECONNREFUSED.\n");
		if (!ip->closing)
		    remove_interactive(ip->ob);
		return;
	    }
	    debug_message("write: unknown errno %d\n", errno);
	    perror("write");
#ifdef DO_ABORT
	    abort();
#endif
	    return;
	}
	if (n != chunk)
	    debug_message("write socket: Size %d(%d) is too big !\n", chunk, n);
	return;
    }
}

/*
 * Get a message from any player.
 * If we get a message, set command_giver to the players object and
 * return true.
 * If we are interrupted, return false (no message yet).
 */

int get_message(buff, size)
    char *buff;
{
    int i, res;
    static int last = -1;	/* Last player number */

    /*
     * Stay in this loop until we have a message from a player.
     */
    while(1) {
	int new_socket;
	struct sockaddr_in addr;
	int length;
	struct timeval timeout;

	/* First, try to get a new player... */
	length = sizeof addr;
	new_socket = accept(s, &addr, &length);
	if (new_socket != -1)
	    new_player(new_socket, &addr, length);
	else if (new_socket == -1 && errno != EWOULDBLOCK && errno != EINTR) {
	    perror("accept");
#ifdef DO_ABORT
	    abort();
#endif
	}
	nfds = 0;
	FD_ZERO(&readfds);
	for (i=0; i<MAX_PLAYERS; i++) {
	    struct interactive *ip = all_players[i];
	    if (ip) {
		FD_SET(ip->socket, &readfds);
		if (ip->socket >= nfds)
		    nfds = ip->socket+1;
		if (ip->out_portal) {
		    FD_SET(ip->portal_socket, &readfds);
		    if (ip->portal_socket >= nfds)
			nfds = ip->portal_socket + 1;
		}
	    }
	}
	timeout.tv_sec = 1;
	timeout.tv_usec = 0;
	res = select(nfds, &readfds, 0, 0, &timeout);
	if (res == -1) {
	    if (errno == EINTR)
		return 0;
	    perror("select");
#ifdef DO_ABORT
	    abort();
#endif
	}
	if (res == 0)
	    return 0;
	i = last + 1;
	if (i > MAX_PLAYERS)
	    i = 0;
	for (i=0; i<MAX_PLAYERS; i++) {
	    struct interactive *ip = all_players[i];
	    if (ip == 0)
		continue;
	    if (ip->out_portal && FD_ISSET(ip->portal_socket, &readfds)) {
		int l;
		l = read(ip->portal_socket, buff, size);
		if (l == -1)
		    continue;
		write(ip->socket, buff, l);
		continue;
	    }
	    if (FD_ISSET(ip->socket, &readfds)) {
		char *p;
		int l, newline = 0;

		last = i;
		if ((l = read(ip->socket, buff, size)) == -1) {
		    if (errno == ENETUNREACH) {
			debug_message("Net unreachable detected.\n");
			remove_interactive(ip->ob);
			return 0;
		    }
		    if (errno == EHOSTUNREACH) {
			debug_message("Host unreachable detected.\n");
			remove_interactive(ip->ob);
			return 0;
		    }
		    if (errno == ETIMEDOUT) {
			debug_message("Connection timed out detected.\n");
			remove_interactive(ip->ob);
			return 0;
		    }
		    if (errno == ECONNRESET) {
			debug_message("Connection reset by peer detected.\n");
			remove_interactive(ip->ob);
			return 0;
		    }
		    if (errno == EWOULDBLOCK) {
			debug_message("read would block socket %d!\n",
				      ip->socket);
			continue;
		    }
		    if (errno == EMSGSIZE) {
			debug_message("read EMSGSIZE !\n");
			continue;
		    }
		    perror("read");
		    debug_message("Errno %d\n", errno);
#ifdef DO_ABORT
		    abort();
#endif
		}
		/*
		 * If the data goes through a portal, send it,
		 * but don't return any data.
		 */
		if (ip->out_portal) {
		    write(ip->portal_socket, buff, l);
		    continue;
		}
		if (l == 0) {
		    if (ip->closing)
			fatal("Tried to read from closing socket.\n");
		    remove_interactive(ip->ob);
		    return 0;
		}
		buff[l] = '\0';
		if (p = strchr(buff, '\n')) {
		    newline = 1;
		    *p = '\0';
		}
		if (p = strchr(buff, '\r'))
		    *p = '\0';
		strncat(ip->text, buff,
		    sizeof ip->text);
		ip->text[sizeof ip->text - 1] = '\0';
		if (ip->snoop_by && newline) {
		    command_giver = ip->snoop_by->ob;
		    add_message("%% %s\n", ip->text);
		}
		command_giver = ip->ob;
		if (newline) {
		    telnet_neg(buff, ip->text);
		    ip->text[0] = '\0';
		    return 1;
		}
		last = -1;
		return 0;
	    }
	}
    }
}

/*
 * Remove an interactive player immediately.
 */
void remove_interactive(ob)
    struct object *ob;
{
    struct object *save = command_giver;
    int i;

    for (i=0; i<MAX_PLAYERS; i++) {
	if (all_players[i] != ob->interactive)
	    continue;
	if (ob->interactive->closing)
	    fatal("Double call to remove_interactive()\n");
	ob->interactive->closing = 1;
	if (ob->interactive->snoop_by) {
	    ob->interactive->snoop_by->snoop_on = 0;
	    ob->interactive->snoop_by = 0;
	}
	if (ob->interactive->snoop_on) {
	    ob->interactive->snoop_on->snoop_by = 0;
	    ob->interactive->snoop_on = 0;
	}
	command_giver = ob;
	add_message("Closing down.\n");
	if (shutdown(ob->interactive->socket, 2) == -1)
	    perror("shutdown");
	close(ob->interactive->socket);
	num_player--;
	if (ob->interactive->input_to) {
	    free_sentence(ob->interactive->input_to);
	    ob->interactive->input_to = 0;
	}
	free(ob->interactive);
	ob->interactive = 0;
	all_players[i] = 0;
	free_object(ob, "remove_interactive");
	command_giver = save;
	return;
    }
    (void)fprintf(stderr, "Could not find and remove player %s\n", ob->name);
#ifdef DO_ABORT
    abort();
#endif
}

new_player(new_socket, addr, len)
    struct sockaddr_in *addr;
    int len;
{
    int i;
    char *p;
    
    if (d_flag)
	debug_message("New player at socket %d.\n", new_socket);
    for (i=0; i<MAX_PLAYERS; i++) {
	struct object *ob;
	
	if (all_players[i] != 0)
	    continue;
	current_object = 0;
	ob = (clone_object("obj/player"))->u.ob;
	add_ref(ob, "new_player");
	if (ob == 0)
	    fatal("Could not load 'obj/player'\n");
	ob->interactive =
	    (struct interactive *)xalloc(sizeof (struct interactive));
	all_players[i] = ob->interactive;
	command_giver = ob;
	ob->interactive->ob = ob;
	ob->interactive->text[0] = '\0';
	ob->interactive->input_to = 0;
	ob->interactive->closing = 0;
	ob->interactive->snoop_on = 0;
	ob->interactive->snoop_by = 0;
	ob->interactive->out_portal = 0;
	ob->interactive->portal_socket = 0;
	ob->interactive->from_portal = 0;
	set_prompt("> ");
	all_players[i]->socket = new_socket;
	/* memcpy(&all_players[i]->addr, addr, len); */
	getpeername(new_socket, &all_players[i]->addr, &len);
	current_object = 0;
	num_player++;
	logon(ob);
	return;
    }
    p = "Lpmud is full. Come back later.\r\n";
    write(new_socket, p, strlen(p));
    close(new_socket);
}

call_function_interactive(i, str)
    struct interactive *i;
    char *str;
{
    char *function;
    struct object *ob;
    struct value *val;

    if (!i->input_to)
	return 0;
    /*
     * Special feature: input_to() has been called to setup
     * a call to a function.
     */
    function = string_copy(command_giver->interactive->input_to->function);
    ob = command_giver->interactive->input_to->ob;
    val = alloc_value();
    val->type = T_STRING;
    val->u.string = string_copy(str);
    free_sentence(command_giver->interactive->input_to);
    command_giver->interactive->input_to = 0;
    /*
     * We must clear this reference before the call to apply(), because someone
     * might want to set up a new input_to().
     */
    (void)apply(function, ob, val);
    free(function);
    return 1;
}

int set_call(ob, sent)
    struct object *ob;
    struct sentence *sent;
{
    if (ob->interactive == 0 || ob->interactive->input_to)
	return 0;
    ob->interactive->input_to = sent;
    return 1;
}

void show_info_about(str, room, i)
    char *str, *room;
    struct interactive *i;
{
    struct hostent *hp = 0;

#if 0
    hp = gethostbyaddr(&i->addr.sin_addr.s_addr, 4, AF_INET);
#endif
    add_message("%-15s %-25s %s\n",
		hp ? hp->h_name : inet_ntoa(i->addr.sin_addr), str, room);
}

void remove_all_players()
{
    int i;

    for (i=0; i<MAX_PLAYERS; i++) {
	if (all_players[i] == 0)
	    continue;
	(void)apply("quit", all_players[i]->ob, 0);
    }
    /*
     * All players should be out now. However, lets try again.
     */
    for (i=0; i<MAX_PLAYERS; i++) {
	if (all_players[i] == 0)
	    continue;
	fprintf(stderr, "Still players.\n");
	remove_interactive(all_players[i]->ob);
    }
}

set_prompt(str)
    char *str;
{
    command_giver->interactive->prompt = str;
}

/*
 * Print the prompt, but only if input_to not is enabled.
 */
print_prompt()
{
    if (command_giver == 0)
	fatal("command_giver == 0.\n");
    if (command_giver->interactive->input_to == 0)
	add_message(command_giver->interactive->prompt);
}

void set_snoop(me, you)
    struct object *me, *you;
{
    struct interactive *on = 0, *by = 0;
    int i;
    
    for(i=0; i<MAX_PLAYERS && (on == 0 || by == 0); i++) {
	if (all_players[i] == 0)
	    continue;
	if (all_players[i]->ob == me)
	    by = all_players[i];
	else if (all_players[i]->ob == you)
	    on = all_players[i];
    }
    if (you == 0) {
	if (by == 0)
	    fatal("Could not find myself to stop snoop.\n");
	add_message("Ok.\n");
	if (by->snoop_on == 0)
	    return;
	by->snoop_on->snoop_by = 0;
	by->snoop_on = 0;
	return;
    }
    if (on == 0 || by == 0) {
	add_message("Failed.\n");
	return;
    }
    if (by->snoop_on)
	by->snoop_on->snoop_by = 0;
    if (on->snoop_by) {
	add_message("Busy.\n");
	return;
    }
    on->snoop_by = by;
    by->snoop_on = on;
    add_message("Ok.\n");
}

#define	TS_DATA		0
#define	TS_IAC		1
#define	TS_WILL		2
#define	TS_WONT		3
#define	TS_DO		4
#define	TS_DONT		5

telnet_neg(to, from)
    char *to, *from;
{
    int state = TS_DATA;
    int ch;
    char *first = to;

    while(1) {
	ch = (*from++ & 0xff);
	switch(state) {
	case TS_DATA:
	    switch(ch) {
	    case IAC:
		state = TS_IAC;
		continue;
	    case '\b':	/* Backspace */
	    case 0x7f:	/* Delete */
		if (to <= first)
		    continue;
		to -= 1;
		continue;
	    default:
		if (ch & 0x80) {
		    debug_message("Tel_neg: 0x%x\n", ch);
		    continue;
		}
		*to++ = ch;
		if (ch == 0)
		    return 0;
		continue;
	    }
	case TS_IAC:
	    switch(ch) {
	    case WILL:
		state = TS_WILL;
		continue;
	    case WONT:
		state = TS_WONT;
		continue;
	    case DO:
		state = TS_DO;
		continue;
	    case DONT:
		state = TS_DONT;
		continue;
	    case DM:
		break;
	    case NOP:
	    case GA:
	    default:
		break;
	    }
	    state = TS_DATA;
	    continue;
	case TS_WILL:
	    debug_message("Will %s\n", telopts[ch]);
	    state = TS_DATA;
	    continue;
	case TS_WONT:
	    debug_message("Wont %s\n", telopts[ch]);
	    state = TS_DATA;
	    continue;
	case TS_DO:
	    debug_message("Do %s\n", telopts[ch]);
	    state = TS_DATA;
	    continue;
	case TS_DONT:
	    debug_message("Dont %s\n", telopts[ch]);
	    state = TS_DATA;
	    continue;
	default:
	    debug_message("Bad state: 0x%x\n", state);
	    state = TS_DATA;
	    continue;
	}
    }
}
