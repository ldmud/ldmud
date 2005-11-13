#include "config.h"
#include "stralloc.h"

#include <sys/time.h>
#ifdef __STDC__
#include <stdarg.h>
#endif
#ifdef AMIGA
#ifndef __SASC
#include <ioctl.h>
#else
#include <sys/ioctl.h>
#endif /* __SASC */
#else
#ifndef MSDOS
#include <sys/ioctl.h>
#endif /* !MSDOS */
#endif /* AMIGA */
#define	TELOPTS
#include "telnet.h"
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <stdio.h>
#include <ctype.h>
#ifndef AMIGA
#include <signal.h>
#endif
#ifndef SIGCLD
#define SIGCLD SIGCHLD
#endif
#ifdef _AIX
#include <sys/select.h>
#endif
#include "lint.h"
#ifdef HAVE_FCNTL
#include <fcntl.h>
#endif
#include "interpret.h"
#include "comm.h"
#include "object.h"
#include "sent.h"
#include "wiz_list.h"
#include "exec.h"
#include "util/erq.h"

#ifdef AMIGA
#include "hosts/amiga/nsignal.h"
#endif

#ifdef SOCKET_INC
#include SOCKET_INC
#endif

#ifndef EPROTO
#define EPROTO EINTR
#endif

void telnet_neg PROT((struct interactive *));

extern int d_flag;
extern int current_time;
extern int trace_level;
extern int32 initial_eval_cost, eval_cost, assigned_eval_cost;
void remove_flush_entry PROT((struct interactive *ip));

void remove_interactive();

struct interactive *all_players[MAX_PLAYERS];

void new_player();

void flush_all_player_mess();

static void send_will PROT((int));
static void send_wont PROT((int));
static void send_do PROT((int));
static void send_dont PROT((int));

int num_player = 0, max_player = -1;

#ifdef ERQ_DEMON

#define MAX_PENDING_ERQ 32

SOCKET_T erq_demon = -2, erq_proto_demon = -1;
static char buf_from_erq[ERQ_MAX_REPLY], *input_from_erq = &buf_from_erq[0];

static long read_32 PROT((char *));
static int send_erq PROT((int handle, int request, char *arg, int arglen));
static void stop_erq_demon PROT((int));

/* The last entry in pending_erq is reserved for callback-free requests */
static struct svalue pending_erq[MAX_PENDING_ERQ+1], *free_erq;

#endif /* ERQ_DEMON */

#ifdef ACCESS_RESTRICTED
extern char *allow_host_access (); 
extern void release_host_access ();
#endif

static char sending_telnet_command = 0;
#define SEND_TELNET_COMMAND(TEXT) {\
	sending_telnet_command = 1;\
	TEXT\
	sending_telnet_command = 0;\
}

static struct object *first_player_for_flush=(struct object *)NULL;

/*
 * Interprocess communication interface to the backend.
 */

#ifndef MAXNUMPORTS
extern int port_number;
#else
extern int port_numbers[];
extern int numports;
#endif

#ifdef CATCH_UDP_PORT
extern int udp_port;
static SOCKET_T udp_s = -1;
#else
#define udp_port 0
#endif

#ifdef COMM_STAT
int add_message_calls=0;
int inet_packets=0;
int inet_volume=0;
#endif
#ifndef MAXNUMPORTS
static SOCKET_T s;
#define MUDSOCKET s
#else
static SOCKET_T sos[MAXNUMPORTS];
#define MUDSOCKET sos[i]
#endif

#ifndef MSDOS
void set_socket_nonblocking(new_socket)
    SOCKET_T new_socket;
{
    int tmp;

    tmp = 1;
#ifdef USE_IOCTL_FIONBIO
    if (socket_ioctl(new_socket, FIONBIO, &tmp) == -1) {
	perror("ioctl socket FIONBIO");
	abort();
    }
#else /* !USE_IOCTL_FIONBIO */
#ifdef USE_FCNTL_O_NDELAY
    if (fcntl(new_socket, F_SETFL, O_NDELAY) == -1) {
#else
    if (fcntl(new_socket, F_SETFL, FNDELAY) == -1) {
#endif
	perror("fcntl socket FNDELAY");
	abort();
    }
#endif /* !USE_IOCTL_FIONBIO */
}

void set_close_on_exec(new_socket)
    SOCKET_T new_socket;
{
#ifdef HAVE_FCNTL
    fcntl(new_socket, F_SETFD, 1L);
#endif
}
#endif /* MSDOS */

set_socket_own(new_socket)
    SOCKET_T new_socket;
{
#ifdef F_SETOWN
    if (fcntl(new_socket, F_SETOWN, getpid())) {
	perror("fcntl SETOWN");
    }
#endif
#ifdef SO_OOBINLINE
{
    int on = 1;
    if (setsockopt(new_socket, SOL_SOCKET, SO_OOBINLINE, &on, sizeof on))
	perror("setsockopt SO_OOBINLINE");
}
#endif
}

static struct in_addr host_ip_number;
static struct sockaddr_in host_ip_addr;

static int min_nfds = 0;

#ifndef MSDOS

void initialize_host_ip_number() {
    char host_name[100];
    struct hostent *hp;
    int tmp;

    if (gethostname(host_name, sizeof host_name) == -1) {
        perror("gethostname");
	fatal("Error in gethostname()\n");
    }
    hp = gethostbyname(host_name);
    if (!hp) {
	(void)fprintf(stderr, "gethostbyname: unknown host.\n");
	exit(1);
    }
    bzero((char *)&host_ip_addr, sizeof host_ip_addr);
    memcpy((char *)&host_ip_addr.sin_addr, hp->h_addr, hp->h_length);
    host_ip_addr.sin_family = hp->h_addrtype;
    host_ip_number = host_ip_addr.sin_addr;
#ifdef CATCH_UDP_PORT
    /* Initialize upd at an early stage so that the master object can use
     * it in inaugurate_master() , and the port number is known.
     */
    if (udp_port != -1) {
	host_ip_addr.sin_addr.s_addr = INADDR_ANY;
	host_ip_addr.sin_port = htons((u_short)udp_port);
	debug_message("UDP recv-socket on port: %d\n", udp_port);
	udp_s = socket(host_ip_addr.sin_family, SOCK_DGRAM, 0);
	if (udp_s == -1) 
	{
	    perror("udp_socket");
	    abort();
	}
	tmp = 1;
	if (setsockopt (udp_s, SOL_SOCKET, SO_REUSEADDR,
			(char *) &tmp, sizeof (tmp)) < 0) 
	{
	    perror ("setsockopt");
	    exit (1);
	}
	for(;;) {
	    if (bind(udp_s, (struct sockaddr *)&host_ip_addr,
		sizeof host_ip_addr) == -1) 
	    {
		if (errno == EADDRINUSE) {
		    fprintf(stderr, "UDP Socket already bound!\n");
		    debug_message("UDP Socket already bound!\n");
		    if (host_ip_addr.sin_port) {
			host_ip_addr.sin_port = 0;
			continue;
		    }
		    close(udp_s);
		    udp_s = -1;
		} else {
		    perror("udp-bind");
		    abort();
		}
	    }
	    break;
	}
    }
    if (udp_s >= 0) {
	tmp = sizeof(host_ip_addr);
	if (!getsockname(udp_s, (struct sockaddr *)&host_ip_addr, &tmp))
	    udp_port = ntohs(host_ip_addr.sin_port);
	set_socket_nonblocking(udp_s);
	set_close_on_exec(udp_s);
	if (socket_number(udp_s) >= min_nfds)
	    min_nfds = socket_number(udp_s)+1;
    }
#endif /* CATCH_UDP_PORT */
}

static volatile int urgent_data = 0;
static volatile long urgent_data_time;

void urgent_data_handler()
{
    if (d_flag)
	write(2, "received urgent data\n", 21);
    urgent_data = 1;
    urgent_data_time = current_time;
    signal(SIGURG, (RETSIGTYPE(*)PROT((int)))urgent_data_handler);
}

static void (*telopts_do  [NTELOPTS]) PROT((int));
static void (*telopts_dont[NTELOPTS]) PROT((int));
static void (*telopts_will[NTELOPTS]) PROT((int));
static void (*telopts_wont[NTELOPTS]) PROT((int));

void prepare_ipc() {
    int tmp;
    int i;

    /* Don't undo the effect of mudlib_telopts.  */
    if (!telopts_do[0])
      init_telopts();

#ifndef MAXNUMPORTS
#define PNUMBER port_number
#else
#define PNUMBER port_numbers[i]
#endif

#ifdef MAXNUMPORTS
    for (i = 0; i < numports; i++) {
#endif
    if (port_numbers[i] > 0) {
	host_ip_addr.sin_port = htons((u_short)PNUMBER);
	host_ip_addr.sin_addr.s_addr = INADDR_ANY;
	MUDSOCKET = socket(host_ip_addr.sin_family, SOCK_STREAM, 0);
	if ((int)MUDSOCKET == -1) {
	    perror("socket");
	    abort();
	}
	tmp = 1;
	if (setsockopt (MUDSOCKET, SOL_SOCKET, SO_REUSEADDR,
			(char *) &tmp, sizeof (tmp)) < 0) {
	    perror ("setsockopt");
	    exit (1);
	}
	if (bind(MUDSOCKET, (struct sockaddr *)&host_ip_addr, sizeof host_ip_addr) == -1) {
	    if (errno == EADDRINUSE) {
		fprintf(stderr, "Socket already bound!\n");
		debug_message("Socket already bound!\n");
		exit(errno);
	    } else {
		perror("bind");
		abort();
	    }
	}
    }
    else {
	MUDSOCKET = -port_numbers[i];
	tmp = sizeof(host_ip_addr);
	if (!getsockname(MUDSOCKET, (struct sockaddr *)&host_ip_addr, &tmp))
	    port_numbers[i] = ntohs(host_ip_addr.sin_port);
    }
    if (listen(MUDSOCKET, 5) == -1) {
	perror("listen");
	abort();
    }
    set_socket_nonblocking(MUDSOCKET);
    set_close_on_exec(MUDSOCKET);
    if (socket_number(MUDSOCKET) >= min_nfds)
	min_nfds = socket_number(MUDSOCKET)+1;
#ifdef MAXNUMPORTS
  } /* for */
#endif
    signal(SIGPIPE, SIG_IGN);
    signal(SIGURG, (RETSIGTYPE(*)PROT((int)))urgent_data_handler);
}

#else /* MSDOS */

void initialize_host_ip_number() {
}

void prepare_ipc() {
    c_listen();
    s = 0;
}

#endif /* MSDOS */

/*
 * This one is called when shutting down the MUD.
 */
void ipc_remove() {
    int i;
    (void)printf("Shutting down ipc...\n");
#ifndef MSDOS
#ifdef MAXNUMPORTS
    for (i = 0; i < numports; i++)
#endif
    socket_close(MUDSOCKET);
#ifdef CATCH_UDP_PORT
    if (udp_s >= 0)
	socket_close(udp_s);
#endif
#else
    c_shutdown();
#endif
}

/* to allow messages to go to a interactive player who's object is currently
 * destructed, there is the special format string destruct_add_message_format.
 * It has to be initialized as array of char, because a string could be made
 * shared.
 */
char destruct_add_message_format[] = { '%', 's', '\0' };

/*
 * Send a message to a player. If that player is shadowed, special
 * care has to be taken.
 */

#ifdef __STDC__
void add_message(char *fmt, ...)
#else
/*VARARGS1*/
void add_message(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    char *fmt;
    int a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    char buff[2130];		/* Kludgy! Hope this is enough ! */
    struct interactive *ip;
    int n, chunk, length;
    int min_length;
    int old_message_length;
    struct object *snooper;
    char *source, *end, *dest;
    int retries;
#ifdef __STDC__
    va_list va;
#endif

#ifdef __STDC__
    va_start(va, fmt);
#endif

    min_length = MAX_SOCKET_PACKET_SIZE;
    if (command_giver == 0 ||
        ( command_giver->flags & O_DESTRUCTED &&
          fmt != destruct_add_message_format && fmt != MESSAGE_FLUSH ) ||
	 !(ip = O_GET_INTERACTIVE(command_giver)) ||
	 ip->sent.type != SENT_INTERACTIVE ||
	(ip->do_close && fmt != MESSAGE_FLUSH) ) {
	putchar(']');
	if ( fmt != MESSAGE_FLUSH ) {
#ifdef __STDC__
	    vprintf(fmt, va);
#else
	    printf(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
	}
	fflush(stdout);
#ifdef __STDC__
	va_end(va);
#endif
	return;
    }
    old_message_length = ip->message_length;
    if ( fmt == MESSAGE_FLUSH ) {
	min_length = 1;
	source = "";
    } else {
#ifdef COMM_STAT
	add_message_calls++; /* We want to know how many packets the old
				version would have sent			 */
#endif
	if (fmt[0] == '%' && fmt[1] == 's' && !fmt[2]) {
#ifdef __STDC__
	    source = va_arg(va, char *);
	    va_end(va);
#else
	    source = (char *)a1;
#endif
	} else {
	    buff[(sizeof buff) - 1] = '\0';
#ifdef __STDC__
	    (void)vsprintf(buff+1,fmt,va);
	    va_end(va);
#else
	    (void)sprintf(
	      buff+1,
	      fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9
	    );
#endif
	    /*
	     * Always check that your arrays are big enough ! :-)
	     */
	    if (buff[(sizeof buff) - 1])
		fatal("To long message!\n");
	    source = buff+1;
	}
	if (!sending_telnet_command) {
	    if (shadow_catch_message(command_giver, source))
		return;
	    if (snooper = ip->snoop_by) {
		buff[0] = '%';
		if (O_GET_INTERACTIVE(snooper) &&
		    O_GET_INTERACTIVE(snooper)->sent.type == SENT_INTERACTIVE)
		{
		    struct object *save;

		    save = command_giver;
		    command_giver = snooper;
		    if (source != buff+1) {
			if (strlen(source) >= sizeof buff - 1) {
			    add_message("%s", "%");
			    add_message("%s", source);
			} else {
			    strcpy(buff+1, source);
			    add_message("%s", buff);
			}
		    } else {
			add_message("%s", buff);
		    }
		    command_giver = save;
		} else {
		    trace_level |= ip->trace_level;
		    if (source != buff+1) {
			if (strlen(source) >= sizeof buff - 1) {
			    tell_npc(snooper, "%");
			    tell_npc(snooper, source);
			} else {
			    strcpy(buff+1, source);
			    tell_npc(snooper, buff);
			}
		    } else {
			tell_npc(snooper, buff);
		    }
		}
	    }
	}
    }
#ifdef DEBUG
    if (d_flag > 1)
	debug_message("[%s(%d)]: %s", command_giver->name,
			strlen(source), source);
#endif
    dest = &ip->message_buf[old_message_length];
    end = &ip->message_buf[sizeof ip->message_buf];
#ifdef PORTALS
    if (ip->out_portal) {
	if (dest < end)
	    *dest++ = ']';
	else
	    c = ']';
    }
#endif
    do {
	char c;

	if (dest == end) {
	    c = '\0';
	} else for (;;) {
	    c = *source++;
	    if (ip->charset[(c&0xff)>>3] & 1<<(c&7) ||
		(c && sending_telnet_command) )
	    {
		*dest++ = c;
	    } else if (c == '\0') {
		source--;
		break;
	    } else if (c == '\n') {
		/* Insert CR before NL */
		*dest++ = '\r';
		if (dest == end)
		    break;
		*dest++ = c;
	    } else if ( (unsigned char)c == IAC && ip->quote_iac) {
		*dest++ = c;
		if (dest == end)
		    break;
		*dest++ = c;
	    }
	    if (dest == end) {
		c = '\0';
		break;
	    }
	}
	chunk = dest - ip->message_buf;
	if (chunk < min_length)
	    break;
	/* send */
	for (retries = 6;;) {
	    if ((n = socket_write(ip->socket, ip->message_buf, chunk)) != -1)
		break;
	    switch (errno) {
	      case EINTR:
		if (--retries)
		    continue;
		fprintf(stderr,
		  "comm1: write EINTR. Message discarded.\n");
	        if (old_message_length) remove_flush_entry(ip);
		return;
	      case EWOULDBLOCK:
		fprintf(stderr,
		  "comm1: write EWOULDBLOCK. Message discarded.\n");
	        if (old_message_length) remove_flush_entry(ip);
/*		ip->do_close = 1;  -- LA */
		return;
	      case EMSGSIZE:
		fprintf(stderr, "comm1: write EMSGSIZE.\n");
		return;
	      case EINVAL:
		fprintf(stderr, "comm1: write EINVAL.\n");
		break;
	      case ENETUNREACH:
		fprintf(stderr, "comm1: write ENETUNREACH.\n");
		break;
	      case EHOSTUNREACH:
		fprintf(stderr, "comm1: write EHOSTUNREACH.\n");
		break;
	      case EPIPE:
		fprintf(stderr, "comm1: write EPIPE detected\n");
		break;
	      default:
		fprintf(stderr, "comm1: write: unknown errno %d\n", errno);
		perror("write");
	    }
	    if (old_message_length) remove_flush_entry(ip);
	    ip->do_close = 1;
	    return;
	}
#ifdef COMM_STAT
	inet_packets++;
	inet_volume += n;
#endif
	if (n != chunk)
	    fprintf(stderr, "write socket: wrote %d, should be %d.\n",
		    n, chunk);
	dest = &ip->message_buf[0];
	if (c)
	    *dest++ = c;
    } while (*source);
    ip->message_length = length = dest - ip->message_buf;
    if ( length && !old_message_length ) { /* buffer became 'dirty' */
	if ( ip->next_player_for_flush = first_player_for_flush ) {
	    O_GET_INTERACTIVE(first_player_for_flush)->
	      previous_player_for_flush =
		command_giver;
	}
	ip->previous_player_for_flush = 0;
	first_player_for_flush = command_giver;
    }
    if ( !length && old_message_length ) { /* buffer has become empty */
	remove_flush_entry(ip);
    }
}

void remove_flush_entry(ip)
    struct interactive *ip;
{

    ip->message_length=0;
    if ( ip->previous_player_for_flush ) {
	O_GET_INTERACTIVE(ip->previous_player_for_flush)->next_player_for_flush
	  = ip->next_player_for_flush;
    } else {
	first_player_for_flush = ip->next_player_for_flush;
    }
	if ( ip->next_player_for_flush ) {
	O_GET_INTERACTIVE(ip->next_player_for_flush)->previous_player_for_flush
	  = ip->previous_player_for_flush;
    }
}

void flush_all_player_mess() {
    struct object *p,*np;
    struct object *save = command_giver;

    for ( p = first_player_for_flush; p; p=np) {
        np = O_GET_INTERACTIVE(p)->next_player_for_flush;
	/* beware of side-effects when calling add_message the first time! */
	command_giver = p;
	add_message(MESSAGE_FLUSH);
    }
    command_giver=save;
}

#define	TS_DATA		0
#define	TS_IAC		1
#define	TS_WILL		2
#define	TS_WONT		3
#define	TS_DO		4
#define	TS_DONT		5
#define TS_SB		6
#define TS_SB_IAC	7
#define TN_START_VALID(x) ((x & ~1) == TS_SB)
#define TS_READY	8
#define TS_CR		9
#define TS_SYNCH	10
#define TS_INVALID	11

/*
 * Get a message from any player.  For all players without a completed
 * cmd in their input buffer, read their socket.  Then, return the first
 * cmd of the next player in sequence that has a complete cmd in their buffer.
 * CmdsGiven is used to allow people in ED to send more cmds (if they have
 * them queued up) than normal players.  If we get a heartbeat, still read
 * all sockets; if the next cmd giver is -1, we have already cycled and
 * can go back to do the heart beat.
 */

#define	StartCmdGiver	(max_player)
#define IncCmdGiver	NextCmdGiver--

extern int time_to_call_heart_beat, comm_time_to_call_heart_beat;
static fd_set exceptfds;

int get_message(buff, size)
    char *buff;
    int size;
{
    int i;
    struct interactive * ip = 0;
    static fd_set readfds;
    static int NextCmdGiver = -1;
    static int CmdsGiven = 0;

    /*
     * Stay in this loop until we have a message from a player.
     */
    while(1)
    {
	struct sockaddr_in addr;
	int length;
	struct timeval timeout;

	if (NextCmdGiver < 0) {
	    int nfds;
	    int res;
	    int twait;		/* wait time for select() */
	    int retries;

	    flush_all_player_mess();
	    twait = !comm_time_to_call_heart_beat;
	    FD_ZERO(&readfds);
#ifdef MAXNUMPORTS
	  for (i = 0; i < numports; i++) {
#endif
	    FD_SET(MUDSOCKET, &readfds);
#ifdef MAXNUMPORTS
	  } /* for */
#endif
	    nfds = min_nfds;
	    for (i = max_player + 1; --i >= 0;) {
		ip = all_players[i];
		if (!ip)
		    continue;
		if (ip->do_close) {
		    ip->do_close &= 2;
		    remove_interactive(ip->ob);
		    continue;
		}
		if (ip->tn_state == TS_READY) {
		    twait = 0;
		} else {
		    FD_SET(ip->socket, &readfds);
		    if (socket_number(ip->socket) >= nfds)
			nfds = socket_number(ip->socket)+1;
#ifdef PORTALS
		    if (ip->out_portal) {
			FD_SET(ip->portal_socket, &readfds);
			if (ip->portal_socket >= nfds)
			    nfds = ip->portal_socket + 1;
		    }
#endif /* PORTALS */
		}
	    }
#ifdef ERQ_DEMON
	    if (erq_demon >= 0) {
		FD_SET(erq_demon, &readfds);
	    }
#endif
#ifdef CATCH_UDP_PORT
	    if (udp_s >= 0) {
		FD_SET(udp_s, &readfds);
	    }
#endif
	    for (retries = 6;;) {
		/* avoid busy waiting when no buffered cmds */
		timeout.tv_sec = twait;
		timeout.tv_usec = 0;
		res = socket_select(nfds, &readfds, 0, 0, &timeout);
		if (res == -1) {
		    if (errno == EINTR) {
			/* We got an alarm, probably need heart_beat.
			 * But finish the select call since we already have
			 * prepared readfds.
			 */
			if (comm_time_to_call_heart_beat)
			    twait = 0;
			if (--retries >= 0)
			    continue;
		    } else {
			perror("select");
		    }
		    /* pretend select suceeded with zero sockets to read,
		     * and process heart_beat / buffered commands.
		     */
		    FD_ZERO(&readfds);
		}
		break;
	    }
	    if (urgent_data) {
		urgent_data = 0;
		timeout.tv_sec = 0;
		timeout.tv_usec = 0;
		memset((char *)&exceptfds, 255, nfds + 7 >> 3);
		if (socket_select(nfds, 0, 0, &exceptfds, &timeout) > 0) {
		    for (i = max_player + 1; --i >= 0;) {
			ip = all_players[i];
			if (!ip)
			    continue;
			if (FD_ISSET(ip->socket, &exceptfds)) {
			    ip->ts_data = TS_SYNCH;
			    switch (ip->tn_state) {
			      case TS_DATA:
			      case TS_CR:
			      case TS_READY:
				ip->tn_state = TS_SYNCH;
				ip->gobble_char = '\0';
			    }
			}
		    }
		/* Maybe the data didn't arrive yet, so try again later.
		   But don't waste time doing it for too long.  */
		} else if (current_time - urgent_data_time < 600) {
		    urgent_data = 1;
		}
	    }
	    CmdsGiven = 0;
	    NextCmdGiver = StartCmdGiver;
#ifdef ERQ_DEMON
	    if (erq_demon >= 0 && FD_ISSET(erq_demon, &readfds)) {
		static void add_ip_entry PROT((long, char *));

		mp_int l, msglen, rest;
		int32 handle;
		char *rp;

		FD_CLR(erq_demon, &readfds);
		retries = 6;
		do {
		    l = socket_read(
		      erq_demon,
		      input_from_erq,
		      &buf_from_erq[sizeof buf_from_erq] - input_from_erq
		    );
		} while(l < 0 && errno == EINTR && --retries >= 0);
		if (l <= 0) {
#ifdef DEBUG_ERQ
		    fprintf(stderr, "read %ld bytes from erq demon\n", l);
		    if (l)
			perror("");
#endif /* DEBUG_ERQ */
		    stop_erq_demon(1);
		} else {
		    input_from_erq += l;
		    l = input_from_erq - &buf_from_erq[0];
		    rp = buf_from_erq;
		    for (; l >= 8 && l >= (msglen = read_32(rp));
						rp += msglen, l -= msglen)
		    {
			int keep_handle;

			if (msglen < 8) {
#ifdef DEBUG_ERQ
			    fprintf(
			      stderr,
			      "length of mesage from erq demon: %ld bytes\n",
			      msglen
			    );
#endif /* DEBUG_ERQ */
			    stop_erq_demon(1);
			    break;
			}
			handle = read_32(rp+4);
			if (handle == ERQ_HANDLE_KEEP_HANDLE && msglen >= 8) {
			    handle = read_32(rp+8);
			    keep_handle = 1;
			    msglen -= 4;
			    l -= 4;
			    rp += 4;
			} else if (handle == ERQ_HANDLE_RLOOKUP) {
			    int32 net_addr;

			    if (msglen < 13 || rp[msglen-1]) {
#ifdef DEBUG
			      if (msglen == 12) {
				if (d_flag > 1)
				  debug_message("Host lookup failed\n");
			      } else {
				debug_message("Bogus reverse name lookup.\n");
			      }
#endif
			    } else {
				memcpy((char*)&net_addr, rp+8, 4);
			        add_ip_entry(net_addr, rp+12);
			    }
			    continue;
			} else {
			    keep_handle = 0;
			}
			if ((uint32)handle < MAX_PENDING_ERQ &&
			    (rest = msglen - 8) <= MAX_ARRAY_SIZE &&
			    pending_erq[handle].type != T_INVALID)
			{
			    extern struct object *current_interactive;
			    struct svalue *erqp = &pending_erq[handle];
			    char *cp;
			    struct vector *v;
			    struct svalue *svp;
			    struct object *ob;
			    struct wiz_list *user;

			    command_giver = 0;
			    current_interactive = 0;
			    ob = !CLOSURE_MALLOCED(erqp->x.closure_type) ?
				erqp->u.ob :
				erqp->u.lambda->ob;
			    current_object = ob;
			    v = allocate_array(rest);
			    current_object = 0;
			    push_referenced_vector(v);
			    push_number(rest);
			    cp = rp + 8;
			    for (svp = v->item; --rest >=0; svp++) {
				svp->u.number = *cp++;
			    }
			    user = ob->user;
			    if (user->last_call_out != current_time) {
				user->last_call_out = current_time;
				CLEAR_EVAL_COST;
			    } else {
				assigned_eval_cost = eval_cost = user->call_out_cost;
			    }
			    secure_call_lambda(erqp, 2);
			    user->call_out_cost = eval_cost;
			    if (!keep_handle) {
				free_svalue(erqp);
				erqp->type = T_INVALID;
				erqp->u.lvalue = free_erq;
				free_erq = erqp;
			    }
			}
		    }
		    if (rp != buf_from_erq) {
			move_memory(buf_from_erq, rp, l);
			input_from_erq = &buf_from_erq[l];
		    }
		}
	    }
#endif /* ERQ_DEMON */
	    /* First, try to get a new player... */
#ifdef MAXNUMPORTS
	    for (i = 0; i < numports; i++) {
#endif
	    if (FD_ISSET(MUDSOCKET, &readfds)) {
		SOCKET_T new_socket;

		length = sizeof addr;
		new_socket = accept(MUDSOCKET, (struct sockaddr *)&addr, &length);
		if ((int)new_socket != -1)
		    new_player(new_socket, &addr, length);
		else if ((int)new_socket == -1 &&
		    errno != EWOULDBLOCK && errno != EINTR && errno != EAGAIN &&
		    errno != EPROTO
		) {
		    /* EBADF is a valid cause for an abort */
		    /* same goes for ENOTSOCK, EOPNOTSUPP, EFAULT */
		    perror("accept");
		    abort();
		}
	    }
#ifdef MAXNUMPORTS
            } /* for */
#endif
	    /* check heart beat */
	    if (comm_time_to_call_heart_beat) {
		time_to_call_heart_beat = 1;
		return 0;
	    }
	}
#ifdef CATCH_UDP_PORT
	/* see if we got any udp messages */
	/* we don't test readfds so that we can accept udp messages with
	 * short latency. But for the same reason, it was necessary to
	 * include the descriptor number in the set to be selected on.
	 */
	if (udp_s >= 0)
	{
	    extern struct object *current_interactive;

	    char udp_buf[1024+1], *st;
	    int cnt;

	    length = sizeof addr;
	    cnt = recvfrom(udp_s, udp_buf, sizeof(udp_buf)-1, 0, 
			   (struct sockaddr *)&addr, &length);
	    if (cnt != -1)
	    {
		command_giver = 0;		/*HERP*/
		current_interactive = 0;
		current_object = 0;
		trace_level = 0;
		udp_buf[sizeof(udp_buf) - 1] = 0;
		udp_buf[cnt] = 0;
		st = inet_ntoa(addr.sin_addr);
		push_string_malloced((st));
		push_string_malloced((udp_buf));
		push_number(ntohs(addr.sin_port));
		apply_master_ob("receive_imp", 3);
	    }
	}
#endif
	for (; NextCmdGiver >= 0; IncCmdGiver) {
	    struct object *snooper;

	    ip = all_players[NextCmdGiver];
	    if (ip == 0) {
		continue;
	    }
	    if (FD_ISSET(ip->socket, &readfds)) {
		int l;

		l = MAX_TEXT - ip->text_end;

		l = socket_read(ip->socket, ip->text + ip->text_end, l);
		if (l == -1) {
		    if (errno == ENETUNREACH) {
			debug_message("Net unreachable detected.\n");
			remove_interactive(ip->ob);
			continue;
		    }
		    if (errno == EHOSTUNREACH) {
			debug_message("Host unreachable detected.\n");
			remove_interactive(ip->ob);
			continue;
		    }
		    if (errno == ETIMEDOUT) {
			debug_message("Connection timed out detected.\n");
			remove_interactive(ip->ob);
			continue;
		    }
		    if (errno == ECONNRESET) {
			debug_message("Connection reset by peer detected.\n");
			remove_interactive(ip->ob);
			continue;
		    }
		    if (errno == EWOULDBLOCK) {
			debug_message("read would block socket %d!\n",
				      ip->socket);
			remove_interactive(ip->ob);
			continue;
		    }
		    if (errno == EMSGSIZE) {
			debug_message("read EMSGSIZE !\n");
			continue;
		    }
		    perror("read");
		    debug_message("Unknown errno %d\n", errno);
		    remove_interactive(ip->ob);
		    continue;
		}
		if (l == 0) {
		    if (ip->closing)
			fatal("Tried to read from closing socket.\n");
		    remove_interactive(ip->ob);
		    continue;
		}
		ip->text_end += l;
#ifdef PORTALS
		/*
		 * IF the data goes through a portal, send it,
		 * but don't return any data.
		 */
		if (ip->out_portal) {
		    socket_write(
		      ip->portal_socket, ip->text, ip->text_end
		    );
		    ip->text_end = 0;
		    continue;
		}
#endif /* PORTALS */
		telnet_neg(ip);
	    }
	    /* if ip->text[0] does not hold a valid character, the outcome
	     * of the comparison to '!' does not matter.
	     */
	    if (ip->noecho & CHARMODE_REQ)
		if (ip->text[0] != '!' || ip->noecho & IGNORE_BANG ) {
		    if (ip->tn_state != TS_READY) {
			length = (TN_START_VALID(ip->tn_state) ?
			  ip->tn_start :
			  ip->command_end) - ip->command_start;
			if (!length)
			    continue;
			ip->save_tn_state = ip->tn_state;
			ip->chars_ready = length;
			ip->tn_state = TS_READY;
		    }
		    if ( !(buff[0] = ip->text[ip->command_start++]) ) {
			buff[0] = '\n';
			ip->command_start = 0;
			ip->tn_state = TS_DATA;
			telnet_neg(ip);
		    }
		    buff[1] = '\0';
		    if (!--ip->chars_ready) {
			/* leave the first char in to make '!' possible */
			ip->tn_state = ip->save_tn_state;
			ip->save_tn_state = TS_INVALID;
			ip->tn_start -= ip->command_start - 1;
			move_memory(
			  ip->text,
			  ip->text+ip->command_start,
			  ( ip->text_end = ip->tn_end =
			      ip->command_end -= ip->command_start - 1));
			ip->command_start = 1;
		    }
		    command_giver = ip->ob;
		    trace_level = ip->trace_level;
		    IncCmdGiver;
		    CmdsGiven = 0;
		    ip->last_time = current_time;
		    return 1;
		} else if (ip->tn_state != TS_READY) {
		    length = (TN_START_VALID(ip->tn_state) ?
		      ip->tn_start : ip->command_end) - ip->command_start;
		    if (length > ip->chars_ready) {
			socket_write(ip->socket, ip->text + ip->chars_ready,
			  length - ip->chars_ready);
			ip->chars_ready = length;
		    }
		}
	    if (ip->tn_state == TS_READY) {
		strcpy(buff, ip->text);
		command_giver = ip->ob;
		trace_level = ip->trace_level;
		ip->tn_state = TS_DATA;
		telnet_neg(ip);
		/* if he is not in ed, dont let him issue another command
		 * till the poll comes again.
		 */
		if (ip->sent.ed_buffer && CmdsGiven < ALLOWED_ED_CMDS) {
		    CmdsGiven++;
		    FD_CLR(ip->socket, &readfds);
		} else {
		    IncCmdGiver;
		    CmdsGiven = 0;
		}
		/* manage snooping - should the snooper see type ahead?
		 * Well, he doesn't here.
		 */
		if ((snooper = ip->snoop_by) && !(ip->noecho & NOECHO_REQ)) {
		    if (O_GET_INTERACTIVE(snooper) &&
			O_GET_INTERACTIVE(snooper)->sent.type == SENT_INTERACTIVE)
		    {
		        command_giver = snooper;
		        add_message("%% %s\n", buff);
		    } else {
		        char *snoop_message = alloca(strlen(buff) + 4);
		        (void)sprintf(snoop_message, "%% %s\n", buff);
		        tell_npc(snooper, snoop_message);
		    }
		    command_giver = ip->ob;
		}
		ip->last_time = current_time;
		return 1;
	    }
	}
	/* no commands found; loop and select (on timeout) again */
    }
}

/*
 * Remove an interactive player immediately.
 */
void remove_interactive(ob)
    struct object *ob;
{
    extern int malloc_privilege;

    struct object *save = command_giver;
    int i;
    struct interactive *interactive;
    int save_privilege;

    interactive = O_GET_INTERACTIVE(ob);
    for (i=0; i<MAX_PLAYERS; i++) {
	if (all_players[i] != interactive)
	    continue;
	if (interactive->closing)
	    fatal("Double call to remove_interactive()\n");
	interactive->closing = 1;
	current_object = ob;
	save_privilege = malloc_privilege;
	if ( !(ob->flags & O_DESTRUCTED) ) {
	    extern struct object *current_interactive;

	    command_giver = 0;
	    current_interactive = 0;
	    push_object(ob);
	    malloc_privilege = MALLOC_MASTER;
	    apply_master_ob("disconnect", 1);
	    /* master might have used exec() */
	    ob = interactive->ob;
	}
	interactive->catch_tell_activ = 0;
	if (interactive->snoop_by) {
	    if (O_GET_INTERACTIVE(interactive->snoop_by) &&
		O_GET_INTERACTIVE(interactive->snoop_by)->sent.type ==
		SENT_INTERACTIVE)
	    {
	        O_GET_INTERACTIVE(interactive->snoop_by)->snoop_on = 0;
	    } else {
	        free_object(interactive->snoop_by, "remove_interactive");
	    }
	    interactive->snoop_by = 0;
	}
	if (interactive->snoop_on) {
	    interactive->snoop_on->snoop_by = 0;
	    interactive->snoop_on = 0;
	}
	command_giver = ob;
#ifdef ERQ_DEMON
	if (interactive->do_close & 2 &&
	    interactive->socket == erq_proto_demon)
	{
	    static char erq_welcome[] = { IAC, TELOPT_BINARY };

	    add_message(MESSAGE_FLUSH);
	    erq_demon = interactive->socket;
	    erq_proto_demon = -1;
	    socket_write(erq_demon, erq_welcome, sizeof erq_welcome);
	} else
#endif
	{
	    trace_level |= interactive->trace_level;
	    add_message(destruct_add_message_format, "Closing down.\n");
	    add_message(MESSAGE_FLUSH);
	    (void)shutdown(interactive->socket, 2);
	    socket_close(interactive->socket);
	}
#if defined(ACCESS_CONTROL) && defined(ACCESS_RESTRICTED)
        release_host_access(interactive->access_class);
#endif
	num_player--;
	if (interactive->input_to) {
	    free_input_to(interactive->input_to);
	}
	if (interactive->modify_command) {
	    free_object(interactive->modify_command, "remove_interactive");
	}
	free_svalue(&interactive->prompt);
	free_svalue(&interactive->default_err_message);
	if (interactive->sent.ed_buffer ||
	    interactive->sent.shadowing || interactive->sent.shadowed_by)
	{
	    struct shadow_sentence *shadow_sent;

	    malloc_privilege = MALLOC_SYSTEM;
	    shadow_sent = (struct shadow_sentence *)alloc_sentence();
	    *shadow_sent = interactive->sent;
	    shadow_sent->type = SENT_SHADOW;
	    ob->sent = (struct sentence *)shadow_sent;
	} else {
	    ob->sent = interactive->sent.next;
	    ob->flags &= ~O_SHADOW;
	}
	xfree((char *)interactive);
	all_players[i] = 0;
	while (max_player && !all_players[max_player])
	    max_player--;
	free_object(ob, "remove_interactive");
	command_giver = check_object(save);
	current_object = 0;
	malloc_privilege = save_privilege;
	return;
    }
    (void)fprintf(stderr, "Could not find and remove player %s\n", ob->name);
    abort();
}

#ifdef ACCESS_CONTROL
#ifndef ACCESS_RESTRICTED

char *allow_host_access(apa, idp)
    struct sockaddr_in apa;
    long *idp;
{
    int len = sizeof apa;
    char * ipname;
    static int read_access_list = 0;
    static struct access_list {
	int addr_len;
	char * addr, *name, *comment;
	struct access_list * next;
    } * access_list;
    register struct access_list * ap;

    if(!read_access_list) {
	FILE * f = fopen("ACCESS.DENY", "r");
	char buf[1024], ipn[50], hname[100], comment[1024], *p1, *p2;
	struct access_list * na;
	struct hostent * hent;

	read_access_list = 1;
	if(f) {
	    while(fgets(buf, sizeof buf - 1, f)) {
		if(*buf != '#') {
		    ipn[0] = hname[0] = comment[0] = 0;
		    if(p1 = strchr(buf, ':')) *p1 = 0;
		    if(buf[0] && buf[0] != '\n')
			strncpy(ipn, buf, sizeof ipn - 1);
		    if((p2 = p1) && *++p2) {
			if(p1 = strchr(p2, ':')) *p1 = 0;
			if(p2[0] && p2[0] != '\n')
			    strcpy(hname, p2);
			if(p1 && p1[1] && p1[1] != '\n')
			    strcpy(comment, p1+1);
		    }
			
		    if(!(na =
			  (struct access_list *)permanent_xalloc(sizeof na[0])))
		    {
			fatal("Out of mem.\n");
		    }
		    na->addr = na->name = na->comment = 0;
		    na->next = 0;
		    if (*ipn) {
			if ( !(na->addr = permanent_xalloc(strlen(ipn) + 1) ))
			    fatal("Out of mem.\n");
			strcpy(na->addr, ipn);
		    }
		    if (*hname) {
			if ( !(na->name = permanent_xalloc(strlen(hname)+1)) )
			    fatal("Out of mem.\n");
			strcpy(na->name, hname);
		    }
		    if (*comment) {
			if (!(na->comment=permanent_xalloc(strlen(comment)+1)))
			    fatal("Out of mem.\n");
			strcpy(na->comment, comment);
		    }

		    if((!(int)*ipn)
			&&
			((!*hname)
			  || (!(hent = gethostbyname(hname))) ||
				 (!(na->addr =
				   permanent_xalloc(hent->h_length+1)))||
				 !strcpy(na->addr,
					inet_ntoa(*(struct in_addr *)hent->h_addr)))) {
			if(na->name) pfree(na->name);
			if(na->comment) pfree(na->comment);
			pfree((char *)na);
			continue;
		    }
		    if(!(na->addr_len = strlen(na->addr)))
			continue;

		    /* printf("disabling: %s:%s:%s\n", na->addr,
			   na->name?na->name:"no name",
			   na->comment?na->comment:"no comment");  */

		    na->next = access_list;
		    access_list = na;
		}
	    }
	    fclose(f);
	}
    }
    
    if (!access_list)
	return 0;
	
    ipname = inet_ntoa(apa.sin_addr);
    
    for(ap = access_list; ap; ap = ap->next)
	if(!strncmp(ipname, ap->addr, ap->addr_len)){
	    printf("Stopping: %s:%s\n", ap->addr, ap->name?ap->name:"no name");
	    return ap->comment ? ap->comment : "";
	}
    return 0;
}
#else /* !ACCESS_RESTRICTED */
void refresh_access_data(add_entry)
    void (*add_entry)PROT((struct sockaddr_in *, long*));
{
    struct interactive **user;
    int n;

    user = all_players;
    for (n = max_player + 2; --n; user++) {
	if (*user)
	    (*add_entry)(&(*user)->addr, &(*user)->access_class);
    }
}
#endif /* ACCESS_RESTRICTED */
#endif /* ACCESS_CONTROL */

struct vector *users() {
    struct object *ob;
    int n, num;
    struct vector *ret;
    struct interactive **user;
    struct svalue *svp;

    num = 0;
    user = all_players;
    for (n = max_player + 2; --n; user++) {
	if (*user && !((*user)->ob->flags & O_DESTRUCTED))
	    num++;
    }
    ret = allocate_array(num);
    svp = ret->item;
    user = all_players;
    for (n = max_player + 2; --n; user++) {
	if (*user && !((ob = (*user)->ob)->flags & O_DESTRUCTED)) {
	    svp->type = T_OBJECT;
	    svp->u.ob = ob;
	    add_ref(ob, "users");
	    svp++;
	}
    }
    return ret;
}

void new_player(new_socket, addr, len)
    SOCKET_T new_socket;
    struct sockaddr_in *addr;
    int len;
{
    int i;
    char *p;
   
#ifdef ACCESS_CONTROL
    char *message;
    long class;
#endif

#ifndef MSDOS
    set_socket_nonblocking(new_socket);
    set_close_on_exec(new_socket);
    set_socket_own(new_socket);
#endif

#ifdef ACCESS_CONTROL
    message = allow_host_access(addr, &class);
#ifdef ACCESS_LOG
    {
	FILE *log_file = fopen (ACCESS_LOG, "a");

	if (log_file) {
	    fprintf(log_file, "%s: %s\n",
		inet_ntoa(addr->sin_addr), message ? "denied" : "granted");
	    fclose(log_file);
	}
    }
#endif
    if (message) {
	socket_write(new_socket, message, strlen(message));
	socket_write(new_socket, "\r\n", 2);
	socket_close(new_socket);
	return;
    }
#endif /* ACCESS_CONTROL */
    if (d_flag > 1)
	debug_message("New player at socket %d.\n", new_socket);
    for (i=0; i<MAX_PLAYERS; i++) {
	extern struct object *master_ob;

	struct object *ob;
	struct svalue *ret;
	struct interactive *new_interactive;
	struct shadow_sentence *save_shadow;
	
	if (all_players[i] != 0)
	    continue;
	assert_master_ob_loaded();
	if (O_GET_INTERACTIVE(master_ob) &&
	    O_GET_INTERACTIVE(master_ob)->sent.type == SENT_INTERACTIVE)
	{
	    p = "Cannot accept connections. Come back later.\r\n";
	    socket_write(new_socket, p, strlen(p));
	    socket_close(new_socket);
	    return;
	}
	command_giver = master_ob;
	trace_level = 0;
	new_interactive =
	    (struct interactive *)xalloc(sizeof (struct interactive));
	if (!new_interactive)
	    return;
	if (master_ob->flags & O_SHADOW) {
	    new_interactive->sent = *O_GET_SHADOW(master_ob);
	    free_shadow_sent(O_GET_SHADOW(master_ob));
	} else {
	    new_interactive->sent.next = master_ob->sent;
	    new_interactive->sent.shadowed_by = 0;
	    new_interactive->sent.shadowing = 0;
	    new_interactive->sent.ed_buffer = 0;
	}
	new_interactive->sent.type = SENT_INTERACTIVE;
	master_ob->sent = (struct sentence *)new_interactive;
	master_ob->flags |= O_ONCE_INTERACTIVE|O_SHADOW;
	new_interactive->ob = master_ob;
	new_interactive->input_to = 0;
	new_interactive->prompt.type = T_STRING;
	new_interactive->prompt.x.string_type = STRING_CONSTANT;
	new_interactive->prompt.u.string = "> ";
	new_interactive->modify_command = 0;
	new_interactive->closing = 0;
	new_interactive->do_close = 0;
	new_interactive->noecho = 0;
	new_interactive->gobble_char = 0;
	new_interactive->catch_tell_activ = 1;
	new_interactive->text_end = 0;
	new_interactive->command_start = 0;
	new_interactive->command_end = 0;
	new_interactive->chars_ready = 0;
	new_interactive->save_tn_state = TS_INVALID;
	new_interactive->tn_start = 0;
	new_interactive->tn_end = 0;
	new_interactive->tn_state = TS_DATA;
	new_interactive->ts_data = TS_DATA;
	new_interactive->snoop_on = 0;
	new_interactive->snoop_by = 0;
	new_interactive->last_time = current_time;
	new_interactive->default_err_message.type = T_INVALID;
	new_interactive->trace_level = 0;
	new_interactive->trace_prefix = 0;
	new_interactive->message_length=0;
#ifdef PORTAL
	new_interactive->from_portal = 0;
	new_interactive->portal_socket = 0;
	new_interactive->out_portal = 0;
#endif /* PORTAL */
	memset(new_interactive->charset, 255, sizeof new_interactive->charset);
	new_interactive->charset['\n'/8] &= ~(1 << '\n' % 8);
	new_interactive->charset['\0'/8] &= ~(1 << '\0' % 8);
	new_interactive->text[0] = '\0';
	all_players[i] = new_interactive;
	all_players[i]->socket = new_socket;
	if (i > max_player)
	    max_player = i;
	memcpy((char *)&all_players[i]->addr, (char *)addr, len);
#if defined(ACCESS_CONTROL) && defined(ACCESS_RESTRICTED)
	all_players[i]->access_class = class;
#endif
	num_player++;
	/*
	 * The player object has one extra reference.
	 * It is asserted that the master_ob is loaded.
	 */
	add_ref(master_ob, "new_player");
	ret = apply_master_ob("connect", 0);
	if (new_interactive != O_GET_INTERACTIVE(master_ob))
	    return;
	if (ret == 0 || ret->type != T_OBJECT ||
	    ( O_GET_INTERACTIVE(ob = ret->u.ob) &&
	      O_GET_INTERACTIVE(ob)->sent.type == SENT_INTERACTIVE) )
	{
	    remove_interactive(master_ob);
	    return;
	}
	command_giver = master_ob;
	add_message(MESSAGE_FLUSH);
	/*
	 * There was an object returned from connect(). Use this as the
	 * player object.
	 */
	save_shadow = (struct shadow_sentence *)alloc_sentence();
	*save_shadow = new_interactive->sent;
	if (ob->flags & O_SHADOW) {
	    struct ed_buffer *buf = new_interactive->sent.ed_buffer;

	    new_interactive->sent = *O_GET_SHADOW(ob);
	    if (buf) {
		save_shadow->ed_buffer = new_interactive->sent.ed_buffer;
		new_interactive->sent.ed_buffer = buf;
	    }
	    new_interactive->sent.type = SENT_INTERACTIVE;
	    free_shadow_sent(O_GET_SHADOW(ob));
	} else {
	    new_interactive->sent.next = ob->sent;
	    new_interactive->sent.shadowed_by = 0;
	    new_interactive->sent.shadowing = 0;
	    save_shadow->ed_buffer = 0;
	}
	ob->sent = (struct sentence *)new_interactive;
	new_interactive->ob = ob;
	ob->flags |= O_ONCE_INTERACTIVE|O_SHADOW;
	master_ob->flags &= ~O_ONCE_INTERACTIVE;
	if (save_shadow->shadowing || save_shadow->shadowed_by ||
	    save_shadow->ed_buffer)
	{
	    save_shadow->type = SENT_SHADOW;
	    master_ob->sent = (struct sentence *)save_shadow;
	} else {
	    master_ob->sent = save_shadow->next;
	    free_shadow_sent(save_shadow);
	    master_ob->flags &= ~O_SHADOW;
	}
	free_object(master_ob, "reconnect");
	add_ref(ob, "new_player");
	command_giver = ob;
	if (new_interactive->snoop_on) {
	    new_interactive->snoop_on->snoop_by = ob;
	}
#ifdef ERQ_DEMON
	send_erq(
	  ERQ_HANDLE_RLOOKUP, ERQ_RLOOKUP,
	  (char *)&new_interactive->addr.sin_addr.s_addr, 4
	);
#endif
	logon(ob);
	flush_all_player_mess();
	return;
    }
    p = "Lpmud is full. Come back later.\r\n";
    /* calling closures here would need special error handling */
    if (closure_hook[H_NO_IPC_SLOT].type == T_STRING) {
	p = closure_hook[H_NO_IPC_SLOT].u.string;
    }
    socket_write(new_socket, p, strlen(p));
    socket_close(new_socket);
}

#ifdef __STDC__
void set_noecho(struct interactive *i, char noecho)
#else
void set_noecho(i, noecho)
    struct interactive *i;
    char noecho;
#endif
{
    char old, confirm;
    struct object *ob;

    old = i->noecho;
    confirm =
      noecho | CHARMODE_REQ_TO_CHARMODE(noecho & (NOECHO_REQ|CHARMODE_REQ));
    i->noecho = confirm;
    confirm |= NOECHO_ACKSHIFT(confirm);
    if ((confirm ^ old) & (NOECHO_MASK|CHARMODE_MASK) ) {
	ob = i->ob;
	if (closure_hook[H_NOECHO].type == T_STRING) {
	    push_number(noecho);
	    push_object(ob);
	    secure_apply(closure_hook[H_NOECHO].u.string, ob, 2);
	} else if (closure_hook[H_NOECHO].type == T_CLOSURE) {
	    if (closure_hook[H_NOECHO].x.closure_type == CLOSURE_LAMBDA)
		closure_hook[H_NOECHO].u.lambda->ob = ob;
	    push_number(noecho);
	    push_object(ob);
	    secure_call_lambda(&closure_hook[H_NOECHO], 2);
	} else {
	    struct object *save;

	    save = command_giver;
	    command_giver = ob;
	    if (~confirm & old & NOECHO) {
		send_wont(TELOPT_ECHO);
	    } else if (confirm & ~old & NOECHO_MASK) {
		send_will(TELOPT_ECHO);
	    }
	    if (i->supress_go_ahead && !(confirm & (NOECHO|CHARMODE))) {
		i->supress_go_ahead = 0;
		send_wont(TELOPT_SGA);
	    }
	    /* Only using SGA for charmode is supported hardcoded.
	     * To make more sophisticated negotiations, e.g. using LINEMODE,
	     * use the H_NOECHO hook.
	     */
	    if (~confirm & old & CHARMODE_MASK) {
		if (old & CHARMODE)
		    send_dont(TELOPT_SGA);
		if (i->save_tn_state != TS_INVALID) {
		    i->chars_ready = 0;
		    i->tn_state = i->save_tn_state;
		}
		if (i->command_start) {
		    i->tn_start -= i->command_start;
		    move_memory(
		      i->text,
		      i->text + i->command_start,
		      ( i->text_end = i->tn_end -= i->command_start )
		    );
		    if (i->command_end)
			i->command_end = i->tn_end;
		    i->command_start = 0;
		}
	    } else if (confirm & ~old & CHARMODE_MASK) {
		send_do(TELOPT_SGA);
		/* some telnet implementations mix up DO and WILL SGA, thus
		 * we send WILL SGA as well.
		 */
		send_will(TELOPT_SGA);
		i->supress_go_ahead = 1;
	    }
	    command_giver = save;
	}
    }
}

int call_function_interactive(i, str)
    struct interactive *i;
    char *str;
{
    char *function;
    struct object *ob;
    int extra;

    if (!i->input_to)
	return 0;
    /*
     * Special feature: input_to() has been called to setup
     * a call to a function.
     */
    function = i->input_to->function;
    ob = i->input_to->ob;
    if (ob->flags & O_DESTRUCTED) {
	/* Sorry, the object has selfdestructed ! */
	set_noecho(i, 0);
	free_input_to(i->input_to);
	i->input_to = 0;
	return 0;
    }
    if (i->noecho) {
	/* if there is a series of noecho/charmode input, we should only
	 * negotiate when we know that the state actually should change.
	 */
	i->noecho |= NOECHO_STALE;
    }
    free_object(ob, "call_function_interactive");
    push_volatile_string(str);
    if (extra = i->input_to->num_arg) {
	extern struct svalue *inter_sp;

	memcpy(
	  (char *)(inter_sp + 1),
	  (char *)i->input_to->arg,
	  extra * sizeof *inter_sp
	);
	inter_sp += extra;
    }
    xfree((char *)i->input_to);
    /*
     * We must clear this reference before the call to apply(), because
     * someone might want to set up a new input_to().
     */
    i->input_to = 0;
    /*
     * Now we set current_object to this object, so that input_to will
     * work for static functions.
     */
    current_object = ob;
    (void)apply(function, ob, 1 + extra);
    free_string(function);
    if (i->noecho & NOECHO_STALE) {
	set_noecho(i, 0);
    }
    return 1;
}

int set_call(ob, it, noecho)
    struct object *ob;
    struct input_to *it;
    int noecho;
{
    struct interactive *ip;

    if (ob == 0 || it == 0)
	return 0;
    ip = O_GET_INTERACTIVE(ob);
    if (!ip || ip->sent.type != SENT_INTERACTIVE || ip->input_to ||
	ip->closing)
    {
	return 0;
    }
    ip->input_to = it;
    if (noecho) {
	set_noecho(ip, noecho);
    }
    return 1;
}

#if 0
void show_info_about(str, room, i)
    char *str, *room;
    struct interactive *i;
{
    struct hostent *hp = 0;

#if 0
    hp = gethostbyaddr(&i->addr.sin_addr.s_addr, 4, AF_INET);
#endif
    add_message("%-15s %-15s %s\n",
		hp ? hp->h_name : inet_ntoa(i->addr.sin_addr), str, room);
}
#endif

void remove_all_players()
{
    int i;

    for (i=0; i<MAX_PLAYERS; i++) {
	if (all_players[i] == 0)
	    continue;
	command_giver = all_players[i]->ob;
	trace_level |= all_players[i]->trace_level;
	CLEAR_EVAL_COST;
	push_object(all_players[i]->ob);
	(void)apply_master_ob("remove_player", 1);
	if ( !(all_players[i]->ob->flags & O_DESTRUCTED) ) {
	    emergency_destruct(all_players[i]->ob);
	}
    }
}

void set_prompt(str)
    char *str;
{
    struct svalue *promptp;

#ifdef DEBUG
    if (O_GET_INTERACTIVE(command_giver)->sent.type != SENT_INTERACTIVE)
	fatal("set_prompt() of non-interactive object\n");
#endif
    promptp = &O_GET_INTERACTIVE(command_giver)->prompt;
    free_svalue(promptp);
    promptp->type = T_STRING;
    promptp->x.string_type = STRING_CONSTANT;
    promptp->u.string = str;
}

struct svalue *query_prompt(ob)
    struct object *ob;
{
#ifdef DEBUG
    if (O_GET_INTERACTIVE(ob)->sent.type != SENT_INTERACTIVE)
	fatal("query_prompt() of non-interactive object\n");
#endif
    return &O_GET_INTERACTIVE(ob)->prompt;
}

/*
 * Print the prompt, but only if not disabled by input_to.
 */
void print_prompt()
{
    struct interactive *ip;

#ifdef DEBUG
    if (command_giver == 0)
	fatal("command_giver == 0.\n");
#endif
    ip = O_GET_INTERACTIVE(command_giver);
#ifdef DEBUG
    if (ip->sent.type != SENT_INTERACTIVE)
	fatal("print_prompt() of non-interactive object\n");
#endif
    if (ip->input_to == 0) {
	extern struct svalue *inter_sp;

	struct svalue *prompt;

	prompt = &ip->prompt;
	if (prompt->type == T_CLOSURE) {
	    call_lambda(prompt, 0);
	    prompt = inter_sp;
	    if (prompt->type != T_STRING) {
		free_svalue(prompt);
	    } else {
		/* beware: add_message() might cause an error. Thus, the LPC
		 * stack has to include the prompt to free it then.
		 */
		add_message("%s", prompt->u.string);
		free_string_svalue(prompt);
	    }
	    inter_sp--;
	} else {
	    add_message("%s", prompt->u.string);
	}
    }
}

#if 0
/*
 * Let object 'me' snoop object 'you'. If 'you' is 0, then turn off
 * snooping.
 */
void set_snoop(me, you)
    struct object *me, *you;
{
    struct interactive *on = 0, *by = 0, *tmp;
    int i;

    if (me->flags & O_DESTRUCTED)
	return;
    if (you && (you->flags & O_DESTRUCTED))
	return;
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
	    error("Could not find myself to stop snoop.\n");
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
    if (by->snoop_on) {
	by->snoop_on->snoop_by = 0;
	by->snoop_on = 0;
    }
    if (on->snoop_by) {
	add_message("Busy.\n");
	return;
    }
    /*
     * Protect against snooping loops.
     */
    for (tmp = on; tmp; tmp = tmp->snoop_on) {
	if (tmp == by) {
	    add_message("Busy.\n");
	    return;
	}
    }
    on->snoop_by = by;
    by->snoop_on = on;
    add_message("Ok.\n");
    return;
}
#endif

/*
 * Let object 'me' snoop object 'you'. If 'you' is 0, then turn off
 * snooping.
 *
 * This routine is almost identical to the old set_snoop. The main
 * difference is that the routine writes nothing to player directly,
 * all such communication is taken care of by the mudlib. It communicates
 * with master.c in order to find out if the operation is permissble or
 * not. The old routine let everyone snoop anyone. This routine also returns
 * 0 or 1 depending on success.
 * A return value of -1 designates failure due to snooping loop.
 */
int new_set_snoop(me, you)
    struct object *me, *you;
{
    struct interactive *on = 0, *by = 0, *tmp;
    struct svalue *ret;

    /* Stop if people managed to quit before we got this far */
    if (me->flags & O_DESTRUCTED)
	return 0;
    if (you && (you->flags & O_DESTRUCTED))
	return 0;

    /* Check for permissions with valid_snoop in master */
    push_object(me);
    if (you == 0)
	push_number(0);
    else
	push_object(you);
    ret = apply_master_ob("valid_snoop", 2);

    if (!ret || ret->type != T_NUMBER || ret->u.number == 0)
	return 0;

    if (me->flags & O_DESTRUCTED)
	return 0;
    if (by = O_GET_INTERACTIVE(me)) {
	if (by->sent.type != SENT_INTERACTIVE)
	    by = 0;
	else if (by->closing)
	    return 0;
    }
    if (you) {
	if (you->flags & O_DESTRUCTED)
	    return 0;
	if (!(on = O_GET_INTERACTIVE(you)) ||
	    on->sent.type != SENT_INTERACTIVE || on->closing)
	{
		return 0;
	}
    } else {
	/* Stop snoop */

	if (!by) {
	    int i;

	    for (i = max_player+1;;) {
		if (--i < 0)
		    return 0;
		if ((on = all_players[i]) && on->snoop_by == me)
		    break;
	    }
	    if (on->closing)
		return 0;
	    free_object(me, "new_set_snoop");
	} else {
	    on = by->snoop_on;
	    if (!on || on->closing)
		return 0;
	    by->snoop_on = 0;
	}
	on->snoop_by = 0;
	return 1;
    }

    /* Protect against snooping loops */
    for (tmp = on; tmp; tmp = tmp->snoop_on) 
    {
	if (tmp == by) 
	    return -1;
    }

    /* Terminate previous snoop, if any */
    if (on->snoop_by)
    {
	struct interactive *ip;

	if ((ip = O_GET_INTERACTIVE(on->snoop_by)) &&
	    ip->sent.type == SENT_INTERACTIVE)
	{
	    if (ip->closing)
		return 0;
	    ip->snoop_on = 0;
	} else {
	    free_object(on->snoop_by, "new_set_snoop");
	}
	on->snoop_by = 0;
    }
    if (by) {
        if (by->snoop_on) 
        {
	    if (by->snoop_on->closing)
		return 0;
	    by->snoop_on->snoop_by = 0;
	    by->snoop_on = 0;
        }
        by->snoop_on = on;
    } else {
	add_ref(me, "new_set_snoop");
    }

    on->snoop_by = me;
    return 1;
    
}

/* IAC == EOF . This can cause sprintf(), which is used in
 * add_message, to abort output after EOF. therefore, don't try to
 * send anything after the IAC in the same call to add_message.
 */

static void send_wont(option)
    int option;
{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", WONT, option);
      add_message(MESSAGE_FLUSH);
    )
}

static void send_dont(option)
    int option;
{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", DONT, option);
      add_message(MESSAGE_FLUSH);
    )
}

static void send_will(option)
    int option;
{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", WILL, option);
      add_message(MESSAGE_FLUSH);
    )
}

static void send_do(option)
    int option;
{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", DO, option);
      add_message(MESSAGE_FLUSH);
    )
}

static void reply_nil(option)
    int option;
{
}

static void reply_to_do_echo(option)
    int option;
{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->noecho & NOECHO_MASK) {
	if ( !(ip->noecho & NOECHO) ) {
	    /* We were previously told not to echo */
	    send_will(option);
	}
	/* If we already said that we will echo, be quiet */
	ip->noecho |= NOECHO_MASK;
    } else {
	send_wont(option);
    }
}

static void reply_to_dont_echo(option)
    int option;
{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->noecho & NOECHO_MASK) {
	if (~(ip->noecho | ~NOECHO_MASK)) {
	    /* We were granted the option before */
	    send_wont(option);
	}
	ip->noecho = (ip->noecho & ~NOECHO) | NOECHO_ACK;
    }
}

static void reply_to_do_sga(option)
    int option;
{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->noecho & (NOECHO_MASK|CHARMODE_MASK)) {
	if (!ip->supress_go_ahead) {
	    ip->supress_go_ahead = 1;
	    send_will(option);
	}
    } else {
	send_wont(option);
    }
}

static void reply_to_dont_sga(option)
    int option;
{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->supress_go_ahead) {
	ip->supress_go_ahead = 0;
	send_wont(option);
    }
}

static void reply_to_will_sga(option)
    int option;
{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->noecho & CHARMODE_MASK) {
	if ( !(ip->noecho & CHARMODE) ) {
	    send_do(option);
	}
	ip->noecho |= CHARMODE_MASK;
    } else {
	send_dont(option);
    }
}

static void reply_to_wont_sga(option)
    int option;
{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->noecho & CHARMODE_MASK) {
	if (!(ip->noecho | ~CHARMODE_MASK)) {
	    /* We were granted the option before */
	    send_dont(option);
	}
	ip->noecho = (ip->noecho & ~CHARMODE) | CHARMODE_ACK;
    }
}

struct svalue *h_telnet_neg(n)
    int n;
{
    struct svalue *svp;

    CLEAR_EVAL_COST;
    if (closure_hook[H_TELNET_NEG].type == T_STRING) {
	svp =
	  secure_apply(closure_hook[H_TELNET_NEG].u.string, command_giver, n);
    } else if (closure_hook[H_TELNET_NEG].type == T_CLOSURE) {
	if (closure_hook[H_TELNET_NEG].x.closure_type == CLOSURE_LAMBDA)
	    closure_hook[H_TELNET_NEG].u.lambda->ob = command_giver;
	svp = secure_call_lambda(&closure_hook[H_TELNET_NEG], n);
    } else {
	svp = 0;
    }
    return svp;
}

static void reply_h_telnet_neg(option)
    int option;
{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);
    int i;

    switch(ip->tn_state) {
      case TS_DO:
	i = DO;
	break;
      case TS_DONT:
	i = DONT;
	break;
      case TS_WILL:
	i = WILL;
	break;
      case TS_WONT:
	i = WONT;
	break;
    }
    push_number(i);
    push_number(option);
    if (!h_telnet_neg(2)) {
	switch(ip->tn_state) {
          case TS_DO:
	    send_wont(option);
	    break;
          case TS_WILL:
	    send_dont(option);
	    break;
	}
    }
}

void init_telopts()
{
    int i;

    for (i = NTELOPTS; --i >= 0; ) {
	telopts_do[i] = send_wont;
    }
    telopts_do[TELOPT_ECHO] = reply_to_do_echo;
    for (i = NTELOPTS; --i >= 0; ) {
	telopts_dont[i] = reply_nil;
    }
    telopts_dont[TELOPT_ECHO] = reply_to_dont_echo;
    for (i = NTELOPTS; --i >= 0; ) {
	telopts_will[i] = send_dont;
    }
    telopts_will[TELOPT_SGA] = reply_to_will_sga;
    for (i = NTELOPTS; --i >= 0; ) {
	telopts_wont[i] = reply_nil;
    }
    telopts_wont[TELOPT_SGA] = reply_to_wont_sga;
    telopts_do[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_dont[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_will[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_wont[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_do[TELOPT_TTYPE] = reply_h_telnet_neg;
    telopts_dont[TELOPT_TTYPE] = reply_h_telnet_neg;
    telopts_will[TELOPT_TTYPE] = reply_h_telnet_neg;
    telopts_wont[TELOPT_TTYPE] = reply_h_telnet_neg;
    /* Go Ahead does not make any sense when coupling multiple
     * interactive users. It is debatable if we are sending
     * Go Ahead every time it is appropriate (i.e. , never),
     * or we supress it all the time.
     * Unfortunately, SGA is also often associated with
     * character-at-a-time mode - the RFC even mandates this
     * double meaning - which we certainly don't want.
     * It might cause problems when we reject Supress Go Ahead
     * when some stupid client thinks that the ECHO option need
     * be coupled with SGA .
     * Thus, reject SGA in general, but not while
     * ip->noecho & NOECHO_MASK is true.
     */
    telopts_do[TELOPT_SGA] = reply_to_do_sga;
    telopts_dont[TELOPT_SGA] = reply_to_dont_sga;
}

void mudlib_telopts()
{
    int i;

    for (i = NTELOPTS; --i >= 0; ) {
	telopts_do[i] = telopts_dont[i] =
	  telopts_will[i] = telopts_wont[i] = reply_h_telnet_neg;
    }
}



void telnet_neg(ip)
    struct interactive *ip;
{
    char *to, *from;
    int state;
    int ch;
    char *first, *end;

    first = ip->text;
    from = &first[ip->tn_end];
    end = &first[ip->text_end];
    for (;;) {
	if (from >= end) {
#if 0 /* cannot happen with the current calling pattern */
	    if (ip->state == TS_READY) return;
#endif
	    ip->text_end = ip->tn_end = ip->command_end;
	    return;
	}
	if (ip->gobble_char) {
	    if (*from == ip->gobble_char) {
		from++;
	    }
	    ip->gobble_char = '\0';
	    continue;
	}
	break;
    }
    to = &first[ip->command_end];
    do {
	ch = (*from++ & 0xff);
	switch(ip->tn_state) {
	  case TS_READY:
	    return;
	  ts_data:
	    if (from >= end) {
	  data_exhausted:
		ip->text_end = ip->tn_end = ip->command_end = to - first;
		if (ip->text_end >= MAX_TEXT) {
		    ip->text_end = ip->tn_end = ip->command_end = 0;
		    /* this looks like a super-long command.
		     * Return the text so far as partial command.
		     */
		    *to = '\0';
		    ip->tn_state = TS_READY;
		    return;
		}
		return;
	    }
	    ch = (*from++ & 0xff);
	  case TS_DATA:
	    switch(ch) {
	      case IAC:
              new_iac:
		state = TS_IAC;
	  change_state:
		ip->tn_state = state;
		continue;
	      case '\b':	/* Backspace */
	      case 0x7f:	/* Delete */
		if ( !(ip->noecho & CHARMODE_REQ) ) {
		    if (to > first)
			to--;
		    goto ts_data;
		}
		if (ip->text[0] == '!' && ! (ip->noecho & IGNORE_BANG) ) {
		    if (to > &ip->text[ip->chars_ready]) {
			socket_write(ip->socket, &ip->text[ip->chars_ready],
			  to - &ip->text[ip->chars_ready]);
			ip->chars_ready = to - ip->text;
		    }
		    if (to > first) {
			socket_write(ip->socket, "\b \b", 3);
			to--;
			ip->chars_ready--;
		    }
		    goto ts_data;
		}
	      default:
		*to++ = ch;
	      case '\0':
		goto ts_data;
	      case '\r':
		if (from >= end) {
		    /* This might be a fragmented CR NL, CR NUL, or
		     * a broken client that ends lines with CR only.
		     * If we are just looking for a line of text, we
		     * can proceed now, else we have to wait for the
		     * next character to make our decisions.
		     */
		    if ( !(ip->noecho & CHARMODE_REQ) ||
			 ip->text[0] == '!' && ! (ip->noecho & IGNORE_BANG) )
		    {
			ip->gobble_char = '\n';
			goto full_newline;
		    }
		    ip->tn_state = TS_CR;
		    goto data_exhausted;
		} else {
		    ch = (*from++ & 0xff);
	  ts_cr:
		    if (ch != '\n') {
			from--;
			if ((ip->noecho & CHARMODE_REQ) &&
			    (ip->text[0] != '!' || ip->noecho & IGNORE_BANG))
			{
			    ip->tn_state = TS_DATA;
			    *to++ = '\r';
			    goto ts_data;
			}
		    }
		}
	  full_newline:
	      {
		ip->tn_state = TS_READY;
		ip->command_end = 0;
		ip->tn_end = from - first;
		*to = '\0';
		return;
	      }
	      case '\n':
		if ( !(ip->noecho & CHARMODE_REQ) ||
		     ip->text[0] == '!' && ! (ip->noecho & IGNORE_BANG) )
		{
		    ip->gobble_char = '\r';
		}
		goto full_newline;
	    }
	  case TS_CR:
	    goto ts_cr;
          ts_iac:
	  case TS_IAC:
	    switch(ch) {
	      case WILL:
		state = TS_WILL;
		goto change_state;
	      case WONT:
		state = TS_WONT;
		goto change_state;
	      case DO:
		state = TS_DO;
		goto change_state;
	      case DONT:
		state = TS_DONT;
		goto change_state;
	      case SB:
		ip->tn_start = to - first;
		state = TS_SB;
		goto change_state;
	      case DM:
	      data_mark:
		if (ip->ts_data == TS_SYNCH) {
		    struct timeval timeout;

		    FD_ZERO(&exceptfds);
		    FD_SET(ip->socket, &exceptfds);
		    timeout.tv_sec = 0;
		    timeout.tv_usec = 0;
		    if (! socket_select(ip->socket + 1, 0, 0, &exceptfds,
			&timeout))
		    {
			if (d_flag)
			    debug_message("Synch operation finished.\n");
			ip->ts_data = TS_DATA;
		    }
		}
		break;
	      case NOP:
	      case GA:
	      default:
		break;
	    }
	    state = ip->ts_data;
	    goto change_state;
	  case TS_WILL:
	    command_giver = ip->ob;
	    if (ch < NTELOPTS) {
		if (d_flag) debug_message("Will %s\n", telopts[ch]);
		(*telopts_will[ch])(ch);
	    } else {
		debug_message("Unknown telnet option Will %d\n", ch);
		send_dont(ch);
	    }
	    state = ip->ts_data;
	    goto change_state;
	  case TS_WONT:
	    command_giver = ip->ob;
	    if (ch < NTELOPTS) {
		if (d_flag) debug_message("Wont %s\n", telopts[ch]);
		(*telopts_wont[ch])(ch);
	    } else {
		debug_message("Unknown telnet option Wont %d\n", ch);
	    }
	    state = ip->ts_data;
	    goto change_state;
	  case TS_DO:
	    command_giver = ip->ob;
	    if (ch < NTELOPTS) {
		if (d_flag) debug_message("Do %s\n", telopts[ch]);
		(*telopts_do[ch])(ch);
	    } else {
		debug_message("Unknown telnet option Do %d\n", ch);
		send_wont(ch);
	    }
	    state = ip->ts_data;
	    goto change_state;
	  case TS_DONT:
	    command_giver = ip->ob;
	    if (ch < NTELOPTS) {
		if (d_flag) debug_message("Dont %s\n", telopts[ch]);
		(*telopts_dont[ch])(ch);
	    } else {
		debug_message("Unknown telnet option Dont %d\n", ch);
	    }
	    state = ip->ts_data;
	    goto change_state;
	  case TS_SB:
	    if (ch == IAC) {
		state = TS_SB_IAC;
		goto change_state;
	    }
	    *to++ = ch;
	    continue;
	  case TS_SB_IAC:
	  {
	    mp_int size;
	    struct vector *v;

	    if (ch == IAC) {
		*to++ = ch;
		state = TS_SB;
		goto change_state;
	    } else if ((ch == SE || ch == SB) &&
		(size = (to - first) - ip->tn_start - 1) <= MAX_ARRAY_SIZE &&
		size >= 0 &&
		(current_object = ip->ob,  v = allocate_array(size)) )
	    {
		unsigned char *str;
		struct svalue *svp;

		str = (unsigned char *)&ip->text[ip->tn_start];
		push_number(SB);
		push_number(*str++);
		svp = v->item;
		while (--size >= 0) {
		    svp->u.number = *str++;
		    svp++;
		}
		push_referenced_vector(v);
		command_giver = ip->ob;
		h_telnet_neg(3);
	    }
	    to = &first[ip->tn_start];
	    if (ch != SE)
		goto ts_iac;
	    state = ip->ts_data;
	    goto change_state;
	  }
	  case TS_SYNCH:
	    if (ch == IAC) goto new_iac;
	    if (ch == DM) goto data_mark;
	    continue;
	  default:
	    if (d_flag) debug_message("Bad state: 0x%x\n", ip->tn_state);
	    state = TS_DATA;
	    goto change_state;
	}
    } while(from < end);
    ip->text_end = ip->tn_end = ip->command_end = to - first;
    if (ip->text_end == MAX_TEXT) {
	/* telnet negotiation shouldn't have such large data chunks.
	 * Ignore all data altogether and return to text mode.
	 */
	ip->text_end = ip->tn_end = ip->command_end = 0;
	ip->tn_state = TS_DATA;
    }
    return;
}

#define IPSIZE 200
static struct ipentry {
    long addr;
    char *name;
} iptable[IPSIZE] = { {0, 0}, };

struct svalue *query_ip_name(sp, lookup)
    struct svalue *sp;
    int lookup;
{
    extern struct svalue *inter_sp;

    struct object *ob;
    int i;
    struct interactive *ip;
    char *str;

    if (sp->type != T_OBJECT) {
	struct svalue *svp;

	if (sp->type == T_NUMBER && !sp->u.number)
	    return sp;
	svp = sp;
	while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
	    svp = svp->u.lvalue;
	if (svp->type != T_OBJECT)
	    bad_xefun_arg(1, sp);
	ob = svp->u.ob;
    } else {
	ob = sp->u.ob;
	decr_object_ref(ob, "query_ip_name");
	sp->type = T_NUMBER;
    }
    if ( !(ip = O_GET_INTERACTIVE(ob)) || ip->sent.type != SENT_INTERACTIVE) {
	free_svalue(sp);
	sp->type = T_NUMBER;
	sp->u.number = 0;
	return sp;
    }
    if (sp->type == T_LVALUE) {
	extern struct svalue const0;

	struct svalue array, *svp;
	struct vector *v;
	char *cp;

	v = allocate_array(sizeof ip->addr);
	if (v) {
	    array.type = T_POINTER;
	    array.u.vec = v;
	    i = sizeof ip->addr;
	    svp = v->item;
	    cp = (char *)&ip->addr;
	    do {
		svp->u.number = *cp++;
		svp++;
	    } while(--i);
	    transfer_svalue(sp, &array);
	} else {
	    transfer_svalue(sp, &const0);
	}
    }
    if (lookup) {
	for(i = 0; i < IPSIZE; i++) {
	    if (iptable[i].addr == ip->addr.sin_addr.s_addr && iptable[i].name)
	    {
		sp->type = T_STRING;
		sp->x.string_type = STRING_SHARED;
		increment_string_ref(sp->u.string = iptable[i].name);
		return sp;
	    }
	}
    }
    str = string_copy(inet_ntoa(ip->addr.sin_addr));
    if (!str) {
	inter_sp = sp - 1;
	error("Out of memory\n");
    }
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = str;
    return sp;
}

#ifdef ERQ_DEMON

void start_erq_demon(suffix)
    char *suffix;
{
    struct svalue *erqp;
    char path[200];
    int sockets[2];
    int pid;
    char c;

    erqp = pending_erq - 1;
    do {
	erqp[1].u.lvalue = erqp;
	erqp++;
	erqp->type = T_INVALID;
    } while (erqp < &pending_erq[MAX_PENDING_ERQ]);
    free_erq = erqp - 1;
    pending_erq[0].u.lvalue = 0;
    if(socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0) {
	perror("socketpair");
        return;
    }
    (void)signal(SIGCLD, SIG_IGN); /* don't create zombie processes */
    if((pid = fork()) == 0) {
        /* Child */
        dup2(sockets[0], 0);
        dup2(sockets[0], 1);
        close(sockets[0]);
        close(sockets[1]);
	if (strlen(BINDIR) + 5 <= sizeof path) {
	    sprintf(path, "%.95s/erq%.100s", BINDIR, suffix);
	    execl((char *)path, "erq", "--forked", 0);
	}
	write(1, "0", 1);	/* indicate failure */
        fprintf(stderr, "exec of erq demon failed.\n");
        _exit(1);
    }
    close(sockets[0]);
    if(pid == -1) {
        close(sockets[1]);
        return;
    }
    read(sockets[1], &c, 1);
    if (c == '0') {
        close(sockets[1]);
	return;
    }
    erq_demon = sockets[1];
    set_socket_nonblocking(erq_demon);
    if (socket_number(erq_demon) >= min_nfds)
	min_nfds = socket_number(erq_demon)+1;
}

static int erq_pending_len = 0;
/* erq_pending_len is used by send_erq, but needs
 * to be cleared by stop_erq_demon .
 */

static void stop_erq_demon(notify)
    int notify;
{
    extern struct svalue *inter_sp;

    struct svalue *erqp;
    int i;

    if (erq_demon < 0)
	return;
    socket_close(erq_demon);
    erq_demon = -2;
    erq_pending_len = 0;
    input_from_erq = &buf_from_erq[0];
    erqp = pending_erq;
    i = MAX_PENDING_ERQ;
    do {
	if (erqp->type == T_CLOSURE) {
	    *++inter_sp = *erqp;
	    erqp->type = T_INVALID;
	    erqp->u.lvalue = free_erq;
	    free_erq = erqp;
	     CLEAR_EVAL_COST;
	    apply_master_ob("stale_erq", 1);
	}
	erqp++;
    } while (--i);
    if (notify) {
	CLEAR_EVAL_COST;
	if (closure_hook[H_ERQ_STOP].type == T_CLOSURE) {
	    secure_call_lambda(&closure_hook[H_ERQ_STOP], 0);
	}
    }
}

struct svalue *f_attach_erq_demon(sp)
    struct svalue *sp;
{
    struct object *ob;
    struct interactive *ip;
    char *suffix;

    if (sp[-1].type == T_OBJECT &&
	(ip = O_GET_INTERACTIVE(ob=sp[-1].u.ob)) &&
	ip->sent.type == SENT_INTERACTIVE)
    {
	if (sp->type != T_NUMBER)
	    bad_xefun_arg(2, sp);
	sp--;
	decr_object_ref(ob, "attach_erq_demon");
	sp->type = T_NUMBER;
	sp->u.number = 0;
	/* we need to read sp[1] below, thus don't overwrite it now. */
	if (privilege_violation4("attach_erq_demon",
	    ob, 0, sp[1].u.number, sp+1))
	{
	    if (erq_demon > -2) {
		if (sp[1].u.number & 1) {
		    stop_erq_demon(0);
		    erq_demon = -1;
		} else {
		    return sp;
		}
	    }
	    erq_proto_demon = ip->socket;
	    ip->do_close = 2;
	    sp->u.number = 1;
	}
	return sp;
    } else if (sp[-1].type == T_STRING &&
	       !strstr((suffix = sp[-1].u.string), "/.."))
    {
	int n;

	if (sp->type != T_NUMBER)
	    bad_xefun_arg(2, sp);
	sp--;
	n = 0;
	if (privilege_violation4("attach_erq_demon",
	    0, suffix, sp[1].u.number, sp+1))
	{
	    if (erq_demon > -2) {
		if (sp[1].u.number & 1) {
		    stop_erq_demon(0);
		} else {
		    goto return_result;
		}
		erq_proto_demon = -1;
	    }
	    start_erq_demon(suffix);
	    n = 1;
	}
return_result:
	free_svalue(sp);
	sp->type = T_NUMBER;
	sp->u.number = n;
	return sp;
    } else {
	bad_xefun_arg(1, sp);
    }
}

/* SOCK_SEQPACKET is not portale enough, thus make special provisions
 * to deliver messages in an atomic fashion.
 */
int send_erq(handle, request, arg, arglen)
    int handle, request, arglen;
    char *arg;
{
    static char buf[256], *pending;

    int wrote;

    if (erq_demon < 0)
	return 0;
    if (erq_pending_len) {
	wrote = socket_write(erq_demon, pending, erq_pending_len);
	if (wrote > 0) {
	    pending += wrote;
	    erq_pending_len -= wrote;
	}
	if (erq_pending_len)
	    return 0;
    }
    if (arglen + 9 > sizeof buf)
	return 0;
    *(int32*)buf = htonl(erq_pending_len = arglen + 9);
    *(int32*)(buf+4) = htonl(handle);
    buf[8] = request;
    memcpy(buf + 9, arg, arglen);
    pending = buf;
    wrote = socket_write(erq_demon, buf, erq_pending_len);
    if (wrote > 0) {
	pending += wrote;
	erq_pending_len -= wrote;
    }
    return 1;
}

struct svalue *f_send_erq(sp)
    struct svalue *sp;
{
    char *arg;
    mp_int arglen;
    struct svalue *new_erq;
    int i;

    if (sp[-2].type != T_NUMBER)
	bad_xefun_arg(1, sp);
    if (sp[-1].type == T_STRING) {
	arg = sp[-1].u.string;
	arglen = strlen(arg);
    } else if (sp[-1].type == T_POINTER) {
	struct vector *v;
	struct svalue *svp;
	char *cp;
	mp_int j;

	v = sp[-1].u.vec;
	arglen = VEC_SIZE(v);
	cp = arg = alloca(arglen);
	svp = &v->item[0];
	for (j = arglen; --j >= 0; )
	    *cp++ = (*svp++).u.number;
    } else bad_xefun_arg(2, sp);
    if (!privilege_violation4("erq", 0, "", sp[-2].u.number, sp)) {
	goto failure;
    }
    if (sp->type == T_NUMBER && !sp->u.number) {
	new_erq = &pending_erq[MAX_PENDING_ERQ];
	new_erq->u.lvalue = free_erq;
    } else if (sp->type == T_CLOSURE &&
	       sp->x.closure_type != CLOSURE_UNBOUND_LAMBDA)
    {
	new_erq = free_erq;
    } else bad_xefun_arg(3, sp);
    if (new_erq &&
	(i = send_erq(new_erq - pending_erq, sp[-2].u.number, arg, arglen)) )
    {
	free_erq = new_erq->u.lvalue;
	*new_erq = *sp;
    } else {
failure:
	i = 0;
	free_svalue(sp);
    }
    free_svalue(--sp);
    (*--sp).u.number = i;
    return sp;
}

/* read a 32 bit value from (possibly unaligned)
 * network byte order representation
 */
static long read_32(str)
    char *str;
{
    unsigned char *p = (unsigned char *)str;

    return (long)p[0]<<24 | (long)p[1]<<16 | (long)p[2]<<8 | p[3];
}

static void add_ip_entry(addr, name)
long addr;
char *name;
{
    static int ipcur = 0;
    int i;

    for(i = 0; i < IPSIZE; i++) {
	if (iptable[i].addr == addr)
	    return;
    }
    iptable[ipcur].addr = addr;
    if (iptable[ipcur].name)
	free_string(iptable[ipcur].name);
    iptable[ipcur].name = make_shared_string(name);
    ipcur = (ipcur+1) % IPSIZE;
}
#else /* !ERQ_DEMON */
void start_erq_demon() {}
#endif /* ERQ_DEMON */

#ifdef MALLOC_smalloc

void clear_comm_refs() {
#ifdef ERQ_DEMON
    clear_ref_in_vector(
      pending_erq, sizeof pending_erq / sizeof (struct svalue)
    );
#endif ERQ_DEMON
}

void count_comm_refs() {
    int i;

    for(i = 0; i < IPSIZE; i++) {
	if (iptable[i].name)
	    count_ref_from_string(iptable[i].name);
    }
#ifdef ERQ_DEMON
    count_ref_in_vector(
      pending_erq, sizeof pending_erq / sizeof (struct svalue)
    );
#endif /* ERQ_DEMON */
}

#endif /* MALLOC_smalloc */


#ifndef INET_NTOA_OK

char *inet_ntoa(ad)
    struct in_addr ad;
{
    unsigned char * p;
    int a, b, c, d;
    static char addr[20]; /* 16 + 1 should be enough */

    p = (unsigned char *)&ad.s_addr;
    a = p[0];
    b = p[1];
    c = p[2];
    d = p[3];
    sprintf(addr, "%d.%d.%d.%d", a, b, c, d);
    return addr;
}
#endif /* INET_NTOA_OK */

char *query_host_name() {
    static char name[20];
    char *p;
    
    gethostname(name, sizeof name);
    name[sizeof name - 1] = '\0';	/* Just to make sure */
    /* some platforms return the FQHN, but we don't want it. */
    p = strchr(name, '.');
    if (p)
	*p = '\0';
    return name;
}

char *get_host_ip_number() {
    char buf[20];

    sprintf(buf, "\"%s\"", inet_ntoa(host_ip_number));
    return string_copy(buf);
}

struct svalue *f_query_snoop(sp)
    struct svalue *sp;
{
    extern struct svalue *inter_sp;
    extern int out_of_memory;
    extern struct object *master_ob;

    extern void zero_object_svalue PROT((struct svalue *));

    struct svalue *arg1;
    struct object *ob;

    if (sp->type != T_OBJECT)
	bad_xefun_arg(1, sp);
    switch (0) {
      default:
	ob = sp->u.ob;
	if ((ob->flags & (O_DESTRUCTED|O_SHADOW)) != O_SHADOW ||
	    ob->sent->type != SENT_INTERACTIVE)
	{
	    zero_object_svalue(sp);
	    return sp;
	}
	inter_sp = sp;
	assert_master_ob_loaded();
	if (current_object != master_ob) {
	    assign_eval_cost();
	    arg1 = apply_master_ob("valid_query_snoop", 1);
	    if (arg1 == 0 || arg1->type != T_NUMBER || !arg1->u.number) {
		if (out_of_memory) {
		    error("Out of memory\n");
		}
		ob = 0;
		break;
	    }
	} else {
	    decr_object_ref(ob, "query_snoop");
	}
	ob = O_GET_INTERACTIVE(ob)->snoop_by;
    }
    if (ob) {
	add_ref(ob, "query_snoop");
	sp->type = T_OBJECT;
	sp->u.ob = ob;
    } else {
	sp->type = T_NUMBER;
	sp->u.number = 0;
    }
    return sp;
}

struct svalue *f_query_idle(sp)
    struct svalue *sp;
{
    extern struct svalue *inter_sp;

    int i;
    struct object *ob;

    if (sp->type != T_OBJECT)
	bad_xefun_arg(1, sp);
    ob = sp->u.ob;
    if (!ob->sent || ob->sent->type != SENT_INTERACTIVE) {
	inter_sp = sp;
	error("query_idle() of non-interactive object.\n");
	return sp;
    }
    i = current_time - O_GET_INTERACTIVE(ob)->last_time;
    decr_object_ref(ob, "query_idle");
    sp->type = T_NUMBER;
    sp->u.number = i;
    return sp;
}

struct svalue *f_remove_interactive(sp)
    struct svalue *sp;
{
    /* Don't call remove_interactive() here, there is some code
     * that can get upset if the interactice vanishes unexpectedly.
     */
    struct interactive *victim;

    if (sp->type != T_OBJECT)
	bad_xefun_arg(1, sp);
    if ((victim = O_GET_INTERACTIVE(sp->u.ob)) &&
	victim->sent.type == SENT_INTERACTIVE &&
	!victim->closing &&
	!victim->do_close)
    {
	if (victim->message_length) {
	    command_giver = victim->ob;
	    add_message(MESSAGE_FLUSH);

	    /* MESSAGE_FLUSH takes always directly effect on the
	     * socket. No apply() is involved.
	     */
	}
	victim->do_close = 1;
    }
    free_svalue(sp);
    return sp - 1;
}

void notify_no_command(command)
    char *command;
{
    extern struct svalue *inter_sp;

    struct svalue *svp;
    struct interactive *ip;

    if (!(ip = O_GET_INTERACTIVE(command_giver)) ||
	ip->sent.type != SENT_INTERACTIVE)
    {
	return;
    }
    svp = &ip->default_err_message;
    if (svp->type == T_STRING) {
	add_message("%s", svp->u.string);
	free_svalue(svp);
	svp->type = T_INVALID;
    } else if (svp->type == T_CLOSURE) {
	call_lambda(svp, 0);
	/* add_message might cause an error, thus, we free the closure first. */
	free_svalue(svp);
	svp->type = T_INVALID;
	if (inter_sp->type == T_STRING)
	    add_message("%s", inter_sp->u.string);
	pop_stack();
    } else if (closure_hook[H_NOTIFY_FAIL].type == T_STRING) {
	add_message("%s", closure_hook[H_NOTIFY_FAIL].u.string);
    } else if (closure_hook[H_NOTIFY_FAIL].type == T_CLOSURE) {
	if (closure_hook[H_NOTIFY_FAIL].x.closure_type == CLOSURE_LAMBDA)
	    closure_hook[H_NOTIFY_FAIL].u.lambda->ob = command_giver;
	push_volatile_string(command);
	call_lambda(&closure_hook[H_NOTIFY_FAIL], 1);
	if (inter_sp->type == T_STRING)
	    add_message("%s", inter_sp->u.string);
	pop_stack();
    }
}

void clear_notify() {
    struct interactive *ip;

    if (!(ip = O_GET_INTERACTIVE(command_giver)) ||
	ip->sent.type != SENT_INTERACTIVE)
    {
	return;
    }
    if (ip->default_err_message.type != T_INVALID) {
	free_svalue(&ip->default_err_message);
	ip->default_err_message.type = T_INVALID;
    }
}

void set_notify_fail_message(svp)
    struct svalue *svp;
{
    struct interactive *ip;

    if (!command_giver ||
	!(ip = O_GET_INTERACTIVE(command_giver)) ||
	ip->sent.type != SENT_INTERACTIVE ||
	ip->closing)
    {
	free_svalue(svp);
	return;
    }
    transfer_svalue(&ip->default_err_message, svp);
}

void free_notifys() {
    int i;
    struct svalue *svp;

    for (i = MAX_PLAYERS; --i >= 0; ) {
	if (!all_players[i]) continue;
	svp = &all_players[i]->default_err_message;
	free_svalue(svp);
	svp->type = T_INVALID;
    }
}

int replace_interactive(ob, obfrom, /*IGN*/name)
    struct object *ob;
    struct object *obfrom;
    char *name;
{
    /* marion
     * i see no reason why to restrict this, besides - the length
     * (was) missing to strncmp()
     * JnA: There is every reason to restrict this.
     *      Otherwise I can write my own player object without any security
     *      at all!
     */
    extern void prompt_from_ed_buffer PROT((struct interactive *));
    extern void prompt_to_ed_buffer PROT((struct interactive *));

    struct svalue *v;
    struct interactive *stale_interactive, *ip;
    struct object *save_command;
    struct shadow_sentence *save_shadow;
  
    push_constant_string(name);
    push_object(ob);
    push_object(obfrom);
    v = apply_master_ob("valid_exec", 3);
    if (!v || v->type != T_NUMBER || v->u.number == 0)
	return 0;
/*
    if (strcmp(name, "secure/login.c") != 0)
	return 0;
*/
    /* fprintf(stderr,"DEBUG: %s,%s\n",ob->name,obfrom->name); */
    if ((stale_interactive = O_GET_INTERACTIVE(ob)) &&
	stale_interactive->sent.type != SENT_INTERACTIVE)
    {
	stale_interactive = 0;
    }
    if (!(ip = O_GET_INTERACTIVE(obfrom)) || ip->sent.type != SENT_INTERACTIVE)
	error("Bad argument2 to exec()\n");
    /* When we have to have an out of memory error, have it before pointers
     * get changed.
     */
    save_shadow = (struct shadow_sentence *)alloc_sentence();
    save_command = command_giver;
    if (stale_interactive) {
	prompt_from_ed_buffer(stale_interactive);
	if (stale_interactive->message_length) {
            command_giver = ob;
            add_message(MESSAGE_FLUSH);
	}
    }
    prompt_from_ed_buffer(ip);
    if (ip->message_length) {
        command_giver = obfrom;
        add_message(MESSAGE_FLUSH);
    }
    command_giver = save_command;
    if (ip->snoop_on) {
        ip->snoop_on->snoop_by = ob;
    }
    *save_shadow = ip->sent;
    if (ob->flags & O_SHADOW) {
	struct ed_buffer *buf = ip->sent.ed_buffer;

	ip->sent = *O_GET_SHADOW(ob);
	if (buf) {
	    save_shadow->ed_buffer = ip->sent.ed_buffer;
	    ip->sent.ed_buffer = buf;
	}
	if (ip->sent.type != SENT_INTERACTIVE) {
	    ip->sent.type = SENT_INTERACTIVE;
	    free_shadow_sent(O_GET_SHADOW(ob));
	}
    } else {
	ip->sent.shadowed_by = 0;
	ip->sent.shadowing = 0;
	save_shadow->ed_buffer = 0;
	ip->sent.next = ob->sent;
    }
    ob->sent = (struct sentence *)ip;
    ip->ob = ob;
    ip->catch_tell_activ = 1;
    if (stale_interactive) {
	stale_interactive->sent = *save_shadow;
	free_shadow_sent(save_shadow);
	obfrom->sent = (struct sentence *)stale_interactive;
	if (stale_interactive->snoop_on) {
            stale_interactive->snoop_on->snoop_by = obfrom;
	}
	stale_interactive->ob = obfrom;
	stale_interactive->catch_tell_activ = 1;
	prompt_to_ed_buffer(stale_interactive);
    } else {
	add_ref(ob, "exec");
	free_object(obfrom, "exec");
	ob->flags |= O_ONCE_INTERACTIVE|O_SHADOW;
	obfrom->flags &= ~O_ONCE_INTERACTIVE;
	if (save_shadow->shadowed_by || save_shadow->shadowing ||
	    save_shadow->ed_buffer)
	{
	    save_shadow->type = SENT_SHADOW;
	    obfrom->sent = (struct sentence *)save_shadow;
	} else {
	    obfrom->sent = save_shadow->next;
	    free_shadow_sent(save_shadow);
	    obfrom->flags &= ~O_SHADOW;
	}
    }
    prompt_to_ed_buffer(ip);
    if (obfrom == command_giver)
	command_giver = ob;
    else if (ob == command_giver)
	command_giver = obfrom;
    return 1;
}

#ifdef DEBUG
/*
 * This is used for debugging reference counts.
 */

void count_comm_extra_refs() {
    int i;

#ifdef ERQ_DEMON
    count_extra_ref_in_vector(
      pending_erq, sizeof pending_erq / sizeof (struct svalue)
    );
#endif /* ERQ_DEMON */
    for (i=0; i<MAX_PLAYERS; i++) {
	struct object *ob;
	struct input_to *it;

	if (all_players[i] == 0)
	    continue;
	all_players[i]->ob->extra_ref++;
	if (ob = all_players[i]->snoop_by) {
	    struct interactive *ip;

	    if (!(ip = O_GET_INTERACTIVE(current_object)) ||
		ip->sent.type != SENT_INTERACTIVE)
	    {
		/* snooping monster */
		ob->extra_ref++;
	    }
	} /* end of snoop-processing */

	if (it = all_players[i]->input_to) {
	    it->ob->extra_ref++;
	    count_extra_ref_in_vector(it->arg, it->num_arg);
	}
	if (ob = all_players[i]->modify_command)
	    count_extra_ref_in_object(ob);
        count_extra_ref_in_vector(&all_players[i]->prompt, 1);
        count_extra_ref_in_vector(&all_players[i]->default_err_message, 1);
    }
}
#endif /* DEBUG */

#ifdef UDP_SEND
/*
 * Send messages to other muds.
 *
 * The message is sent with udp. If it gets there it gets there...
 */
struct svalue *f_send_imp(sp)
    struct svalue *sp;
{
    char *to_host;
    int to_port;
    char *msg;
    mp_int msglen;
    int ip1, ip2, ip3, ip4;
    struct sockaddr_in name;
    struct hostent *hp;
    int ret = 0;

    if ((sp-2)->type != T_STRING) bad_xefun_arg(1, sp);
    if ((sp-1)->type != T_NUMBER) bad_xefun_arg(2, sp);
    switch(0) { default:
	if (sp->type == T_STRING) {
	    msg = sp->u.string;
	    msglen = strlen(msg);
	} else if (sp->type == T_POINTER) {
	    struct vector *v;
	    struct svalue *svp;
	    char *cp;
	    mp_int j;

	    v = sp->u.vec;
	    msglen = VEC_SIZE(v);
	    cp = msg = alloca(msglen);
	    if (!msg)
		break;
	    svp = &v->item[0];
	    for (j = msglen; --j >= 0; )
		*cp++ = (*svp++).u.number;
	} else bad_xefun_arg(3, sp);
	if (_privilege_violation("send_imp", sp-2, sp) <= 0)
	    break;
	if (udp_s < 0)
	    break;
	to_host = (sp-2)->u.string;
	to_port = (sp-1)->u.number;

	if (sscanf(to_host, "%d.%d.%d.%d", &ip1, &ip2, &ip3, &ip4) == 4) {
	    name.sin_addr.s_addr = inet_addr(to_host);
	    name.sin_family = AF_INET;
	} else {
	    hp = gethostbyname(to_host);
	    if (hp == 0) 
		break;
	    memcpy(&name.sin_addr, hp->h_addr, hp->h_length);
	    name.sin_family = AF_INET;
	}
	name.sin_port = htons(to_port);
	/* Send message. */
#ifndef SENDTO_BROKEN
	if (sendto(udp_s, msg, msglen, 0, 
	       (struct sockaddr *)&name, sizeof(name)) != msglen)
#endif
	    break;
	ret = 1;
    }
    free_svalue(sp);
    free_svalue(--sp);
    free_svalue(--sp);
    sp->type = T_NUMBER;
    sp->u.number = ret;
    return sp;
}    
#endif /* UDP_SEND */

struct svalue *f_set_buffer_size(sp)
    struct svalue *sp;
{
    struct interactive *ip;
    int old, new;
    int optlen;

    if (sp->type != T_NUMBER || sp->u.number > SET_BUFFER_SIZE_MAX)
	bad_xefun_arg(1, sp);
    new = sp->u.number;
    sp->u.number = -1;
#ifdef SO_SNDBUF
    if (!(ip = O_GET_INTERACTIVE(current_object)) ||
	ip->sent.type != SENT_INTERACTIVE ||
	ip->do_close)
    {
	return sp;
    }
    optlen = sizeof old;
    if (getsockopt(ip->socket, SOL_SOCKET, SO_SNDBUF, (char *)&old, &optlen) < 0)
	return sp;
    if (setsockopt(ip->socket, SOL_SOCKET, SO_SNDBUF, (char *)&new, sizeof new) < 0)
	return sp;
    sp->u.number = old;
#endif /* SO_SNDBUF */
    return sp;
}

struct svalue *f_binary_message(sp)
    struct svalue *sp;
{
    char *message, *p;
    mp_int size, wrote, i;
    struct svalue *svp;
    struct interactive *ip;
    struct object *save_command_giver;

    if (sp[-1].type == T_POINTER) {
	size = VEC_SIZE(sp[-1].u.vec);
	message = alloca(size + 1);
	for (i = size, svp = sp[-1].u.vec->item, p = message; --i >= 0; svp++)
	{
	    if (svp->type != T_NUMBER)
		bad_xefun_arg(1, sp);
	    *p++ = svp->u.number;
	}
	*p = '\0';
    } else if (sp[-1].type == T_STRING) {
	message = sp[-1].u.string;
	size = strlen(message);
    } else {
	bad_xefun_arg(1, sp);
    }
    if (sp->type != T_NUMBER)
	bad_xefun_arg(2, sp);
    i = 0;
    if ((ip = O_GET_INTERACTIVE(current_object)) &&
	ip->sent.type == SENT_INTERACTIVE &&
	!ip->do_close)
    {
	save_command_giver = command_giver;
	command_giver = current_object;
	if (sp->u.number & 1) {
	    sending_telnet_command = 1;
	    while (size) {
		if (*message) {
		    add_message("%s", message);
		    if (ip->do_close)
			break;
		    size -= wrote = strlen(message);
		    message += wrote;
		} else {
		    if (ip->message_length >= MAX_SOCKET_PACKET_SIZE) {
			add_message(MESSAGE_FLUSH);
			if (ip->do_close)
			    break;
		    }
		    if (!ip->message_length ) {
			if (ip->next_player_for_flush = first_player_for_flush)
			{
			    O_GET_INTERACTIVE(first_player_for_flush)->
			      previous_player_for_flush =
				command_giver;
			}
			ip->previous_player_for_flush = 0;
			first_player_for_flush = command_giver;
		    }
		    ip->message_buf[ip->message_length++] = '\0';
		    size--;
		}
	    }
	    sending_telnet_command = 0;
	    if (sp->u.number & 2)
		add_message(MESSAGE_FLUSH);
	    wrote = 0;
	} else {
	    add_message(MESSAGE_FLUSH);
	    for (i = 6;;) {
		wrote = socket_write(ip->socket, message, size);
		if (wrote != -1)
		    break;
		switch(errno) {
		  case EINTR:
		    if (--i)
			continue;
		    fprintf(stderr, "comm1: write EINTR. Message discarded.\n");
		    break;
		  case EWOULDBLOCK:
		    fprintf(stderr,
		      "comm1: write EWOULDBLOCK. Message discarded.\n");
		    size = 0;
		    break;
		  case EMSGSIZE:
		    fprintf(stderr, "comm1: write EMSGSIZE.\n");
		    break;
		  default:
		    perror("write");
		    ip->do_close = 1;
		    break;
		}
		break;
	    } /* end for on retry count */
	}
	command_giver = save_command_giver;
    } /* end if interactive */
    sp--;
    free_svalue(sp);
    sp->type = T_NUMBER;
    sp->u.number = wrote;
    return sp;
}

struct svalue *f_set_connection_charset(sp)
    struct svalue *sp;
{
    int i;
    struct svalue *svp;
    char *p;
    struct interactive *ip;

    if (sp[-1].type != T_POINTER || (i = VEC_SIZE(sp[-1].u.vec)) > 32)
	bad_xefun_arg(1, sp);
    if (sp->type != T_NUMBER)
	bad_xefun_arg(2, sp);
    if ((ip = O_GET_INTERACTIVE(current_object)) &&
	ip->sent.type == SENT_INTERACTIVE)
    {
	for (svp = sp[-1].u.vec->item, p = ip->charset; --i >= 0; svp++, p++) {
	    if (svp->type == T_NUMBER)
		*p = svp->u.number;
	}
	bzero(p, &ip->charset[sizeof ip->charset] - p);
	ip->charset['\n'/8] &= ~(1 << '\n' % 8);
	ip->charset['\0'/8] &= ~(1 << '\0' % 8);
	if (ip->quote_iac = sp->u.number) {
	    if (ip->charset[IAC/8] & (1 << IAC % 8))
		ip->charset[IAC/8] &= ~(1 << IAC % 8);
	    else
		ip->quote_iac = 0;
	}
    }
    sp--;
    free_svalue(sp);
    sp--;
    return sp;
}

#ifdef MAXNUMPORTS
struct svalue *query_ip_port(sp)
    struct svalue *sp;
{
    struct object *ob;
    struct interactive *ip;
    struct sockaddr_in addr;
    int length;

    length = sizeof(addr);

    if (sp->type != T_OBJECT) {
	if (   sp->type != T_NUMBER 
            || sp->u.number < -1 || sp->u.number >= numports
           )
	  bad_xefun_arg(1, sp);
        sp->u.number = sp->u.number < 0 ? numports : port_numbers[sp->u.number];
	return sp;
    }
    ob = sp->u.ob;
    decr_object_ref(ob, "query_ip_port");
    if ( !(ip = O_GET_INTERACTIVE(ob)) || ip->sent.type != SENT_INTERACTIVE) {
	sp->type = T_NUMBER;
	sp->u.number = port_numbers[0];
	return sp;
    }
    getsockname(ip->socket, &addr, &length);
    sp->type = T_NUMBER;
    sp->u.number = ntohs(addr.sin_port);
    return sp;
}
#endif
