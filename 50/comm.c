/*---------------------------------------------------------------------------
 * Gamedriver Communications module.
 *
 *---------------------------------------------------------------------------
 * Throughout the module the fact is used that valid socket numbers
 * are always >= 0. Unused sockets are therefore marked with negative
 * numbers.
 *
 * All information needed for an interactive object are stored in
 * a 'struct interactive'. This struct is a special kind of shadow_sentence
 * and as such is stored in the first place of an object->sent list.
 * If the object is shadowed as well, these shadows are linked to the
 * interactive struct as head of the shadow list.
 *
 * Sending data is performed through the function add_message().
 * The function collects the data in interactive.message_buf[] until
 * it is filled (or a special 'flush' message is passed), then the
 * data is written en-bloc to the network.
 *
 * Incoming data is collected in interactive.text[]. interactive.text_end
 * indexes the first free character in the buffer where new data is to
 * be appended. The new data is not passed directly to the command parser,
 * instead it is processed by a dfa implementing the important parts
 * of the telnet protocol. The dfa analyses the data read, interprets
 * any telnet commands and stores the remaining 'pure' data starting
 * from the beginning of .text[].
 *
 * Initialized to start working in the state TS_DATA, the dfa does its
 * thing until it either reaches a line end or the end of the current
 * data. If it is a line end, it terminates the pure data collected so
 * far with a '\0', goes into state TS_READY, and lets .tn_end index
 * the next unprocessed raw data char. If it is the end of the current
 * data, the dfa stays in whatever state it was and indexes the current end
 * of the pure data gathered so far with interactive.command_end. Once
 * a new chunk of data has been read, the dfa simply continues where it
 * took off.
 *
 * There are some variations to this scheme, but you get the idea. It is
 * possible to do all telnet negotiations on mudlib level, but can only
 * be either the driver or the mudlib doing it.
 *
 * To understand get_message() itself fully, think of it as a coroutine
 * with its own state. It does not really return to the caller (though
 * that is how it is implemented), in merely yields control back to
 * caller in order to process the found command or the pending heartbeat.
 * TODO: Obvious possibility for implementation multi-threading.
 *
 * Timing is implemented this way: The driver usually stays in the input
 * loop, waiting in 1 second intervals for incoming data. An alarm() is
 * triggered by backend.c every 2 seconds and sets the flag variable
 * comm_time_to_call_heart_beat. The loop checks this variable every second
 * and, it it is set, aborts its input loop and returns to the backend.
 * To mark the cause of the return, the variable time_to_call_heart_beat is
 * set before return.
 *
 * TODO: The noecho/charmode logic, especially in combination with
 * TODO:: the telnet machine is frustratingly underdocumented.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include "my-alloca.h"

#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#include <stdarg.h>

#if defined(AMIGA) && !defined(__SASC)
#    include <ioctl.h>
#else
#    include <sys/ioctl.h>
#endif

#define TELOPTS
#include "telnet.h"

#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif

#ifndef AMIGA
#    include <signal.h>
#else
#    include "hosts/amiga/nsignal.h"
#endif

#ifdef _AIX
#    include <sys/select.h>
#endif

#ifdef HAVE_FCNTL
#    include <fcntl.h>
#endif

#ifdef SOCKET_INC
#    include SOCKET_INC
#endif

#include "comm.h"
#include "access_check.h"
#include "array.h"
#include "backend.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "object.h"
#include "sent.h"
#include "simulate.h"
#include "stralloc.h"
#include "wiz_list.h"
#include "util/erq.h"

/* When no special networking code is needed, define the
 * socket function to their normal Unix names.
 */
#if !defined (SOCKET_LIB) && !defined(SOCKET_INC)
#    define socket_number(s) (s)
#    define socket_ioctl  ioctl
#    define socket_select select
#    define socket_read   read
#    define socket_write  write
#    define socket_close  close
#endif /* SOCKET_LIB */

#if defined(SunOS4) || defined(atarist)
struct timeval;
extern SOCKET_T socket(int, int, int);
extern int getpeername(SOCKET_T, struct sockaddr *, int *);
extern void  shutdown(SOCKET_T, int);
extern int setsockopt(SOCKET_T, int, int, char *, int);
extern int bind(SOCKET_T, struct sockaddr *, int);
extern int listen(SOCKET_T, int);
extern SOCKET_T accept(SOCKET_T, struct sockaddr *, int *);
extern int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
#endif /* SunOS4 */

#if defined(__CYGWIN32__)
extern int socketpair(int, int, int, int[2]);
#endif

#ifndef EPROTO
#    define EPROTO EINTR
#endif
#ifndef SIGCLD
#    define SIGCLD SIGCHLD
#endif

/* Amazing how complicated networking can be, hm? */

/*-------------------------------------------------------------------------*/

struct interactive *all_players[MAX_PLAYERS];
  /* Pointers to the structures of the interactive users.
   * Unused entries are NULL.
   * TODO: A list would be nicer
   */

static int max_player = -1;
  /* Index of the last used entry in all_players[]. */

int num_player = 0;
  /* The current number of active users */

char *message_flush = NULL;
  /* Special flush message for add_message().
   * It is a variable instead of a define to keep gcc from complaining about
   * a 'null format string'.
   */

#ifdef COMM_STAT

/* The statistics were originally introduced to measure the efficiency
 * of the message buffering in comparison to the unbuffered sending of
 * data. Nowadays, it's just interesting to know how much bandwidth you
 * use.
 */

int add_message_calls = 0;
  /* Number of calls to add_message() */

int inet_packets = 0;
  /* Number packets sent to the users */

int inet_volume = 0;
  /* Amount of data sent to the users */

#endif

/*-------------------------------------------------------------------------*/

#ifdef ERQ_DEMON

#define MAX_PENDING_ERQ  32  /* Max number of pending requests */

#define FLAG_NO_ERQ   -2    /* No ERQ connected */
#define FLAG_ERQ_STOP -1    /* Severing connection */

static SOCKET_T erq_demon = FLAG_NO_ERQ;
  /* Socket of the connection to the erq demon. */

static SOCKET_T erq_proto_demon = -1;
  /* Socket to hold the connection to an aspiring new erq demon
   * while the connection to the current one is being severed.
   */

static char buf_from_erq[ERQ_MAX_REPLY];
  /* Buffer for the data received from the erq */

static char * input_from_erq = &buf_from_erq[0];
  /* Pointer to the first free byte in buf_from_erq. */

static int erq_pending_len = 0;
  /* erq_pending_len is used by send_erq(), but needs
   * to be cleared by stop_erq_demon().
   */

static struct svalue pending_erq[MAX_PENDING_ERQ+1];
  /* ERQ callback handles. The last one is reserved for callback-free
   * requests.
   * The free entries are organised in a singly linked list of
   * T_INVALID svalues, using the u.lvalue to point to the next
   * free entry.
   */

static struct svalue *free_erq;
  /* The first free entry in the freelist in pending_erq[] */

#define IPSIZE 200
static struct ipentry {
    long addr;  /* TODO: make this an struct in_addr */
    char *name;  /* shared string with the hostname for <addr> */
} iptable[IPSIZE] = { {0, 0}, };
  /* Cache of known names for given IP addresses.
   * It is used as a ringbuffer.
   */

#endif /* ERQ_DEMON */

/*-------------------------------------------------------------------------*/

/* --- Communication sockets --- */

#ifndef MAXNUMPORTS
  /* TODO: Make MAXNUMPORTS mandatory */

static SOCKET_T s;
#define MUDSOCKET s

#else

static SOCKET_T sos[MAXNUMPORTS];
#define MUDSOCKET sos[i]

#endif

#ifdef CATCH_UDP_PORT

static SOCKET_T udp_s = -1;
  /* The UDP socket */

#endif

/* --- Networking information --- */

static struct in_addr host_ip_number;
  /* This computer's numeric IP address only, used for
   * the query_host_name() efun. */

static struct sockaddr_in host_ip_addr;
  /* This computer's full IP address, used with varying port numbers
   * to open the driver's ports.
   * TODO: Can't this be local?
   */

static int min_nfds = 0;
  /* The number of fds used by the driver's sockets (udp, erq, login).
   * It is the number of the highest fd plus one.
   */

/* --- Telnet handling --- */

static /* BOOL */ char sending_telnet_command = 0;
  /* Mutex queried in add_message() to hide telnet commands
   * from snoopers and shadows.
   */

#define SEND_TELNET_COMMAND(TEXT) {\
        sending_telnet_command = MY_TRUE;\
        TEXT\
        sending_telnet_command = MY_FALSE;\
}
  /* Use this macro to safely send telnet commands with TEXT
   */

static void (*telopts_do  [NTELOPTS])(int);
static void (*telopts_dont[NTELOPTS])(int);
static void (*telopts_will[NTELOPTS])(int);
static void (*telopts_wont[NTELOPTS])(int);

  /* Tables with the telnet statemachine handlers.
   */

#define TS_DATA       0
#define TS_IAC        1
#define TS_WILL       2
#define TS_WONT       3
#define TS_DO         4
#define TS_DONT       5
#define TS_SB         6
#define TS_SB_IAC     7
#define TS_READY      8
#define TS_CR         9
#define TS_SYNCH     10
#define TS_INVALID   11

  /* Telnet states
   */

#define TN_START_VALID(x) ((x & ~1) == TS_SB)


/* --- Misc --- */

static volatile /* TODO: BOOL */ int urgent_data = MY_FALSE;
  /* Flag set when a SIGURG/SIGIO announces the arrival of
   * OOB data.
   */

static volatile mp_int urgent_data_time;
  /* The backend::current_time when urgent_data was set last.
   */

static struct object *first_player_for_flush = NULL;
  /* First interactive user object to flush. Marks the head
   * of the list formed by interactive.{next,previous}_player_for_flush
   */

static char destruct_add_message_format[]
  = { '%', 's', '\0' };
  /* To allow messages to go to a interactive player who's object
   * is currently destructed, there is this special format string which
   * is guaranteed to be an unshareable string.
   */


/* Bitflags for interactive.do_close
 *
 * Putting PROTO_ERQ into do_close looks strange, but actually makes
 * sense because some of the steps to be taken for both are the
 * same.
 */

#define FLAG_DO_CLOSE   0x1
#define FLAG_PROTO_ERQ  0x2


/*-------------------------------------------------------------------------*/

/* Forward declarations */

static void free_input_to(struct input_to *);
static void telnet_neg(struct interactive *);
static void send_will(int);
static void send_wont(int);
static void send_do(int);
static void send_dont(int);
static void remove_flush_entry(struct interactive *ip);
static void new_player(SOCKET_T new_socket, struct sockaddr_in *addr, int len);
static void add_ip_entry(long addr, char *name);

#ifdef ERQ_DEMON

static long read_32(char *);
static /* TODO: BOOL */ int send_erq(int handle, int request, char *arg, int arglen);
static void stop_erq_demon(/* TODO: BOOL */ int);

#endif /* ERQ_DEMON */


/*-------------------------------------------------------------------------*/
static void
set_socket_nonblocking (SOCKET_T new_socket)

/* Set the <new_socket> into non-blocking mode.
 * Abort on error.
 */

{
    int tmp;

#ifndef __BEOS__

    tmp = 1;

# ifdef USE_IOCTL_FIONBIO
    if (socket_ioctl(new_socket, FIONBIO, &tmp) == -1) {
        perror("ioctl socket FIONBIO");
        abort();
    }
# else /* !USE_IOCTL_FIONBIO */
# ifdef USE_FCNTL_O_NDELAY
    if (fcntl(new_socket, F_SETFL, O_NDELAY) == -1) {
# else
    if (fcntl(new_socket, F_SETFL, FNDELAY) == -1) {
# endif
        perror("fcntl socket FNDELAY");
        abort();
    }
# endif /* !USE_IOCTL_FIONBIO */

#else /* if __BEOS__ */

    /* BeOS up to R4 uses different filedescriptors for files and sockets;
     * so a fcntl() modifies the regular file with the number <new_socket>,
     * but not the socket itself. setsockopt() comes to our rescue.
     * TODO: Add setsockopt() to configure to test?
     */
    tmp = 1;
    if (setsockopt(new_socket, SOL_SOCKET, SO_NONBLOCK, &tmp, sizeof tmp))
        perror("setsockopt SO_NONBLOCK");

#endif /* if !__BEOS__ */

}

/*-------------------------------------------------------------------------*/
static void
set_close_on_exec (SOCKET_T new_socket)

/* Set that <new_socket> is closed when the driver performs an exec()
 * (i.e. when starting the ERQ).
 * Failure is acceptable as this is just a nicety.
 */

{
#ifdef HAVE_FCNTL
    fcntl(new_socket, F_SETFD, 1L);
#endif
}

/*-------------------------------------------------------------------------*/
static void
set_socket_own (SOCKET_T new_socket)

/* Enable OOB communication on <new_socket>: the driver is set to
 * receive SIGIO and SIGURG signals, and OOBINLINE is enabled.
 * Failure is acceptable as both facilities are not available everywhere.
 */

{
#ifdef F_SETOWN
    if (fcntl(new_socket, F_SETOWN, getpid())) {
        perror("fcntl SETOWN");
    }
#endif
#ifdef SO_OOBINLINE
    {
        int on = 1;
        if (setsockopt(new_socket, SOL_SOCKET, SO_OOBINLINE, (char *)&on, sizeof on))
            perror("setsockopt SO_OOBINLINE");
    }
#endif
    new_socket = 0; /* Prevent 'not used' warning */
}

/*-------------------------------------------------------------------------*/
void
initialize_host_ip_number (void)

/* Initialise the globals host_ip_number and host_ip_address.
 * Open the UDP port if requested so that it can be used
 * in inaugurate_master().
 * exit() on failure.
 */

{
    char host_name[100];
    struct hostent *hp;
    int tmp;

    if (gethostname(host_name, sizeof host_name) == -1) {
        perror("gethostname");
        exit(1);
    }
    hp = gethostbyname(host_name);
    if (!hp) {
        fprintf(stderr, "gethostbyname: unknown host '%s'.\n", host_name);
        exit(1);
    }
    memset(&host_ip_addr, 0, sizeof host_ip_addr);
    memcpy(&host_ip_addr.sin_addr, hp->h_addr, hp->h_length);
    host_ip_addr.sin_family = hp->h_addrtype;
    host_ip_number = host_ip_addr.sin_addr;

#ifdef CATCH_UDP_PORT
    /* Initialize upd at an early stage so that the master object can use
     * it in inaugurate_master() , and the port number is known.
     */
    if (udp_port != -1)
    {
        host_ip_addr.sin_addr.s_addr = INADDR_ANY;
        host_ip_addr.sin_port = htons((u_short)udp_port);
        debug_message("UDP recv-socket requested for port: %d\n", udp_port);
        udp_s = socket(host_ip_addr.sin_family, SOCK_DGRAM, 0);
        if (udp_s == -1)
        {
            perror("socket(udp_socket)");
            exit(1);
        }
        tmp = 1;
        if (setsockopt (udp_s, SOL_SOCKET, SO_REUSEADDR,
                        (char *) &tmp, sizeof (tmp)) < 0)
        {
            perror ("setsockopt(udp_s, SO_REUSEADDR)");
            exit(1);
        }

        /* Bind the UDP socket to an address.
         * First, try the given port number, if that one is in use
         * already, let bind() select one. If that one is in use, too,
         * close the socket again and pretend that we never had one.
         * Other errors abort the driver.
         */
        for(;;) {
            if (-1 == bind(udp_s, (struct sockaddr *)&host_ip_addr
                          , sizeof host_ip_addr))
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
                    exit(1);
                }
            }
            break;
        }
    }

    /* If we got the UDP socket, get query it's real address and
     * initialise it.
     */
    if (udp_s >= 0) {
        tmp = sizeof(host_ip_addr);
        if (!getsockname(udp_s, (struct sockaddr *)&host_ip_addr, &tmp))
        {
            int oldport = udp_port;

            udp_port = ntohs(host_ip_addr.sin_port);
            if (oldport != udp_port)
                debug_message("UDP recv-socket on port: %d\n", udp_port);
        }
        set_socket_nonblocking(udp_s);
        set_close_on_exec(udp_s);
        if (socket_number(udp_s) >= min_nfds)
            min_nfds = socket_number(udp_s)+1;
    }
#endif /* CATCH_UDP_PORT */

} /* initialize_host_ip_number() */


/*-------------------------------------------------------------------------*/
static void
ignore_handler (int signo)

/* Signal handler for ignored signals: it just reinitializes the signal
 * handler for this signal. It is used for OS where a signal(,SIG_IGN)
 * is implemented with a one-shot handler (e.g. Linux).
 */

{
#ifdef DEBUG
    if (signo != SIGPIPE) /* the only ignored signal so far */
    {
        fprintf(stderr, "Error: OS passes signo %d instead of SIGPIPE (%d) to handler.\n", signo, SIGPIPE);
        signo = SIGPIPE;
    }
#endif
    signal(signo, (RETSIGTYPE(*)(int))ignore_handler);
}

/*-------------------------------------------------------------------------*/
static void
urgent_data_handler (int signo)

/* Signal handler for SIGURG/SIGIO: set the urgent_data flag and
 * note the time.
 */

{
    if (d_flag)
        write(2, "received urgent data\n", 21);
    urgent_data = MY_TRUE;
    urgent_data_time = current_time;
    signal(signo, (RETSIGTYPE(*)(int))urgent_data_handler);
}

/*-------------------------------------------------------------------------*/
void
prepare_ipc(void)

/* Open all login sockets on driver startup, exit() on a failure.
 */

{
    int tmp;
#ifdef MAXNUMPORTS
    int i;
#endif

    /* Initialize the telnet machine unless mudlib_telopts() already
     * did that.
     */
    if (!telopts_do[0])
      init_telopts();

#ifndef MAXNUMPORTS
#define PNUMBER port_number
#else
#define PNUMBER port_numbers[i]
#endif

    /* Loop over all given port numbers.
     * Remember: positive number are actual port numbers to be opened,
     * negative numbers are the fd numbers of already existing sockets.
     */
#ifdef MAXNUMPORTS
    for (i = 0; i < numports; i++) {
#endif
        if (PNUMBER > 0) {

            /* Real port number */

            host_ip_addr.sin_port = htons((u_short)PNUMBER);
            host_ip_addr.sin_addr.s_addr = INADDR_ANY;
            MUDSOCKET = socket(host_ip_addr.sin_family, SOCK_STREAM, 0);
            if ((int)MUDSOCKET == -1) {
                perror("socket");
                exit(1);
            }
            tmp = 1;
            if (setsockopt (MUDSOCKET, SOL_SOCKET, SO_REUSEADDR
                           , (char *) &tmp, sizeof (tmp)) < 0) {
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
                    exit(1);
                }
            }
        }
        else {

            /* Existing socket */

            MUDSOCKET = -PNUMBER;
            tmp = sizeof(host_ip_addr);
            if (!getsockname(MUDSOCKET, (struct sockaddr *)&host_ip_addr, &tmp))
                PNUMBER = ntohs(host_ip_addr.sin_port);
        }

        /* Initialise the socket */
        if (listen(MUDSOCKET, 5) == -1) {
            perror("listen");
            exit(1);
        }
        set_socket_nonblocking(MUDSOCKET);
        set_close_on_exec(MUDSOCKET);

        if (socket_number(MUDSOCKET) >= min_nfds)
            min_nfds = socket_number(MUDSOCKET)+1;
#ifdef MAXNUMPORTS
    } /* for(i = 0..numports) */
#endif

    /* We handle SIGPIPEs ourself */
#if defined(__linux__)
    signal(SIGPIPE, (RETSIGTYPE(*)(int))ignore_handler);
#else
    signal(SIGPIPE, SIG_IGN);
#endif

#if defined(SIGURG)
    signal(SIGURG, (RETSIGTYPE(*)(int))urgent_data_handler);
#endif
#if defined(SIGIO)
    signal(SIGIO, (RETSIGTYPE(*)(int))urgent_data_handler);
#endif
} /* prepare_ipc() */

/*-------------------------------------------------------------------------*/
/*
 * This one is called when shutting down the MUD.
 */
void
ipc_remove (void)

/* Called when the driver is shutting down, this function closes all
 * open sockets.
 */

{
#ifdef MAXNUMPORTS
    int i;
#endif

    printf("Shutting down ipc...\n");
#ifdef MAXNUMPORTS
    for (i = 0; i < numports; i++)
#endif
        socket_close(MUDSOCKET);

#ifdef CATCH_UDP_PORT
    if (udp_s >= 0)
        socket_close(udp_s);
#endif
}

/*-------------------------------------------------------------------------*/
void
add_message(char *fmt, ...)

/* Send a message to the current command_giver. The message is composed
 * from the <fmt> string and the following arguments using the normal
 * printf() semantics. The format string '%s' is special in that it
 * bypasses the normal printf() handling and uses the given string
 * argument directly as data source, allowing to send strings of
 * arbitrary length. All other format strings compose the message to
 * send in a local buffer and are therefore subject to a length
 * restriction.
 *
 * This function also does the telnet, snooping, and shadow handling.
 *
 * All messages are accumulated in interactive.message_buf, which is
 * flushed when it is full. This flush can be forced by passing the
 * special 'string' message_flush (which is actually just a NULL pointer)
 * as <fmt> string to this function.
 *
 * Messages which can't be send (e.g. because the command_giver was
 * destructed or disconnected) are printed on stdout, preceeded by ']'.
 *
 * If an error other than EINTR occured while sending the data to
 * the network, the message is discarded and the socket is marked
 * for disconnection.
 *
 * Note that add_message() might be called recursively.
 */

{
    char  buff[2130];
      /* Composition buffer for the final message.
       * We hope that it's big enough, but to be sure the code will
       * check for buffer overruns.
       * Message is composed starting from buff[1] on, buff[0] is
       * set to '%' for easier snooper-message generation.
       */
    int   length;
    int   min_length;
      /* When accumulating data in ip.message_buf[], this is the
       * threshold over which the buffer will be written to the
       * socket.
       * TODO: Actually, it is used just as a flag for flush/non-flush.
       */
    int   old_message_length;  /* accumulated message length so far */
    char *source;              /* Pointer to the final message to add */
    char *end;                 /* One char past the end of .message_buf[] */
    char *dest;                /* First free char in .message_buf[] */
    va_list va;
    struct interactive *ip;       /* The interactive user */
    struct object      *snooper;  /* Snooper of <ip> */
    int   n;

    va_start(va, fmt);

    /* Test if the command_giver is a real, living, undestructed user,
     * and not disconnected, closing or actually a new ERQ demon.
     * If the command_giver fails the test, the message is printed
     * to stdout and the function returns.
     */
    if ( command_giver == NULL
     || (   command_giver->flags & O_DESTRUCTED
         && fmt != destruct_add_message_format
         && fmt != message_flush )
     || !(ip = O_GET_INTERACTIVE(command_giver))
     || ip->sent.type != SENT_INTERACTIVE
     || (ip->do_close && fmt != message_flush)
       )
    {
        putchar(']');
        if ( fmt != message_flush ) {
            vprintf(fmt, va);
        }
        fflush(stdout);
        va_end(va);
        return;
    }

    old_message_length = ip->message_length;

    /* --- Compose the final message --- */

    /* Create the final message and handle snoopers and shadows.
     */

    min_length = MAX_SOCKET_PACKET_SIZE;
    if ( fmt == message_flush )
    {
        /* Just flush, nothing to add */

        min_length = 1;
        source = "";
    }
    else /* add the message */
    {
#ifdef COMM_STAT
        add_message_calls++;
#endif

        /* Compose the final message in buff[] (while checking for overruns)
         * and point source to it.
         * Recognize the special format '%s' to bypass buff[] for messages
         * of arbitrary length.
         */

        if (fmt[0] == '%' && fmt[1] == 's' && !fmt[2])
        {
            source = va_arg(va, char *);
            va_end(va);
        }
        else
        {
            buff[(sizeof buff) - 1] = '\0'; /* Overrun sentinel */
            vsprintf(buff+1,fmt,va);
            va_end(va);
            if (buff[(sizeof buff) - 1])
                fatal("To long message!\n");
            source = buff+1;
        }

        /* If we're not sending a telnet command with this message,
         * pass on the new data to any snooper and/or shadow
         */

        if (!sending_telnet_command)
        {

            /* If there's a shadow successfully handling the
             * message, return.
             * This may cause a recursive call to add_message()!.
             */

            if (shadow_catch_message(command_giver, source))
                return;

            /* If there's a snooper, send it the new message prepended
             * with a '%'.
             * For interactive snoopers this means a recursion with
             * the command_giver set to the snooper, for non-interactive
             * snoopers it's a simple call to tell_npc(), with an
             * adaption of the global trace_level to this users trace
             * settings.
             */

            if ( NULL != (snooper = ip->snoop_by) 
             && !(snooper->flags & O_DESTRUCTED))
            {
                buff[0] = '%';
                if (NULL != O_GET_INTERACTIVE(snooper)
                 && O_GET_INTERACTIVE(snooper)->sent.type == SENT_INTERACTIVE
                   )
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
                    }
                    else
                    {
                        add_message("%s", buff);
                    }
                    command_giver = save;
                }
                else
                {
                    trace_level |= ip->trace_level;
                    if (source != buff+1) {
                        if (strlen(source) >= sizeof buff - 1)
                        {
                            tell_npc(snooper, "%");
                            tell_npc(snooper, source);
                        } else
                        {
                            strcpy(buff+1, source);
                            tell_npc(snooper, buff);
                        }
                    } else
                    {
                        tell_npc(snooper, buff);
                    }
                }
            } /* if (snooper) */
        } /* if (!sending_telnet_command */
    } /* if (flush or not) */

#ifdef DEBUG
    if (d_flag > 1)
        debug_message("[%s(%d)]: %s", command_giver->name,
                        strlen(source), source);
#endif

    /* --- Send the final message --- */

    /* Append the final message to the .message_buf[], taking
     * care of all the necessary charset and telnet translations.
     */

    dest = &ip->message_buf[old_message_length];
    end  = &ip->message_buf[sizeof ip->message_buf];

    /* This loop advances source until it reaches a '\0' character.
     * Every character encountered is copied, translated or fed
     * into the telnet machine.
     */

    do {
        int    retries; /* Number of retries left when sending data */
        size_t chunk;   /* Current size of data in .message_buf[] */
        char   c;       /* Currently processed character */

        /* TODO: a simple while (dest != end) should do */

        if (dest == end)
        {
            c = '\0';  /* buffer (again) full: just send the data */
        }
        else for (;;) {
            c = *source++;

            /* Process the character:
             *  - copy it if the corresponding .charset bit is set,
             *    or if it's part of a telnet command.
             *  - translate a '\n' into '\r\n'
             *  - double an IAC if quote_iac is active.
             *  - stop this loop if a '\0' is encountered of if
             *    the buffer is full.
             *
             * If c is not '\0' after this loop, it will be added
             * immediately to .message_buf[] after data has been sent.
             */
            if (ip->charset[(c&0xff)>>3] & 1<<(c&7)
             || (c && sending_telnet_command)
               )
            {
                *dest++ = c;
            }
            else if (c == '\0')
            {
                source--; /* Recreate exit condition for outer loop */
                break;
            }
            else if (c == '\n')
            {
                /* Insert CR before NL */
                *dest++ = '\r';
                if (dest == end)
                    break;
                *dest++ = c;
            }
            else if ( (unsigned char)c == IAC && ip->quote_iac)
            {
                *dest++ = c;
                if (dest == end)
                    break;
                *dest++ = c;
            }

            /* If the buffer is full: exit loop and send the data */
            if (dest == end)
            {
                c = '\0';
                break;
            }
        } /* for() */

        /* Check how much data there is in .message_buf[].
         * If it is enough, send it, else terminate the outer loop
         * (because *source must be exhausted for this to happen).
         */
        chunk = dest - ip->message_buf;
        if (chunk < min_length)
            break;

        /* Write .message_buf[] to the network. */

        for (retries = 6;;) {
            if ((n = socket_write(ip->socket, ip->message_buf, chunk)) != -1)
                break;

            switch (errno) {
              case EINTR:
                if (--retries)
                    continue;
                fprintf(stderr,
                  "comm: write EINTR. Message discarded.\n");
                if (old_message_length)
                    remove_flush_entry(ip);
                return;

              case EWOULDBLOCK:
                fprintf(stderr,
                  "comm: write EWOULDBLOCK. Message discarded.\n");
                if (old_message_length)
                    remove_flush_entry(ip);
                return;

              case EMSGSIZE:
                fprintf(stderr, "comm: write EMSGSIZE.\n");
                return;

              case EINVAL:
                fprintf(stderr, "comm: write EINVAL.\n");
                break;

              case ENETUNREACH:
                fprintf(stderr, "comm: write ENETUNREACH.\n");
                break;

              case EHOSTUNREACH:
                fprintf(stderr, "comm: write EHOSTUNREACH.\n");
                break;

              case EPIPE:
                fprintf(stderr, "comm: write EPIPE detected\n");
                break;

              default:
                {
                  int e = errno;
                  perror("write");
                  fprintf(stderr, "comm: write: unknown errno %d\n", e);
                }
            }
            if (old_message_length)
                remove_flush_entry(ip);
            ip->do_close = FLAG_DO_CLOSE;
            return;

        } /* for (retries) */

#ifdef COMM_STAT
        inet_packets++;
        inet_volume += n;
#endif
        if (n != chunk)
            fprintf(stderr, "write socket: wrote %d, should be %d.\n",
                    n, chunk);

        /* Continue with the processing of source */
        dest = &ip->message_buf[0];
        if (c)
            *dest++ = c;
    } while (*source);

    /* --- Final touches --- */

    ip->message_length = length = dest - ip->message_buf;

    /* Update the ring of interactives with pending data */

    if ( length && !old_message_length )
    {
        /* Buffer became 'dirty': add this interactive to the list */

        if ( NULL != (ip->next_player_for_flush = first_player_for_flush) )
        {
            O_GET_INTERACTIVE(first_player_for_flush)->
              previous_player_for_flush =
                command_giver;
        }
        ip->previous_player_for_flush = 0;
        first_player_for_flush = command_giver;
    }
    if ( !length && old_message_length ) /* buffer has become empty */
    {
        remove_flush_entry(ip);
    }
}

/*-------------------------------------------------------------------------*/
static void
remove_flush_entry (struct interactive *ip)

/* Remove the given interactive <ip> from the list of 'dirty' interactives
 * and make sure it is really clean.
 *
 * This function is called after an interactive sent all pending data (or
 * failing while doing so).
 */

{
    ip->message_length = 0;
    if ( ip->previous_player_for_flush )
    {
        O_GET_INTERACTIVE(ip->previous_player_for_flush)->next_player_for_flush
          = ip->next_player_for_flush;
    }
    else
    {
        first_player_for_flush = ip->next_player_for_flush;
    }

    if ( ip->next_player_for_flush )
    {
        O_GET_INTERACTIVE(ip->next_player_for_flush)->previous_player_for_flush
          = ip->previous_player_for_flush;
    }
}

/*-------------------------------------------------------------------------*/
void
flush_all_player_mess (void)

/* Flush all pending data from the interactives. Usually called before
 * every input loop, after a user logged in, or after an LPC runtime
 * error was processed.
 */

{
    struct object *p, *np;
    struct object *save = command_giver;

    for ( p = first_player_for_flush; p != NULL; p = np)
    {
        np = O_GET_INTERACTIVE(p)->next_player_for_flush;
          /* add_message() will clobber (p)->next_player_for_flush! */
        command_giver = p;
        add_message(message_flush);
    }
    command_giver = save;
}

/*-------------------------------------------------------------------------*/
/* TODO: BOOL */ int
get_message (char *buff)

/* Get a message from a user, or wait until it is time for the next
 * heartbeat/callout. You can tell this apart by the result:
 *
 *   true: a user message was received and placed into buff; the user
 *         object is set as command_giver.
 *   false: it is just time to call the heart_beat.
 *
 * In both cases, time_to_call_heart_beat is set if a heartbeat is due.
 *
 * Internally, get_message() scans the array of interactive users in
 * search for one with a complete message in its incoming buffer. If
 * an earlier select() marked the socket for the current user as pending
 * with data, this data is read into the buffer before the check for
 * a message is performed. get_message() returns for the first user found
 * with a complete message. Since get_message() keeps its own
 * status of which user was looked at last, the next call to get_message()
 * will continue the scan where it left off.
 *
 * If no user has a complete message, a call to select() waits for more
 * incoming data. If this succeeds (and no heartbeat requires an
 * immediate return), the cycle begins again.
 *
 * Normally, users can give only one command per cycle. The exception
 * is when they are editing, then they can give up to ALLOWED_ED_CMDS.
 *
 * Heartbeats are detected by checking the backend variable comm_time_-
 * to_call_heart_beat, which is set by the SIGALRM handler. If it is
 * true, get_message() sets the variable time_to_call_heart_beat to
 * inform the backend (TODO: the handler should set both) and returns.
 *
 * For short latency, the UDP socket is checked on every call to
 * get_message(), even if a previous select() did not mark it as ready
 * (this is disabled under BeOS).
 */

{
    /* State information: */
    static fd_set readfds;
      /* List of sockets with pending data.
       * You can ignore a 'could be used uninitialized' warning.
       */
    static int NextCmdGiver = -1;
      /* Index of current user to check */
    static int CmdsGiven = 0;
      /* Number of commands the current user gave in this cycle. */

#   define StartCmdGiver  (max_player)
#   define IncCmdGiver    NextCmdGiver--

    int    i;
    struct interactive * ip = NULL;
    fd_set exceptfds;


    /* The endless loop */

    while(MY_TRUE)
    {
        struct sockaddr_in addr;
        int                length; /* length of <addr> */
        struct timeval timeout;

        /* --- select() on the sockets and handle ERQ --- */

        /* This also removes users which connection is marked
         * as to be closed.
         */

        if (NextCmdGiver < 0)
        {
            int nfds;     /* number of fds for select() */
            int res;      /* result from select() */
            int twait;    /* wait time in seconds for select() */
            int retries;  /* retries of select() after EINTR */

            flush_all_player_mess();
            twait = comm_time_to_call_heart_beat ? 0 : 1;
              /* If the heart_beat is due, just check the state
               * of the sockets, but don't wait.
               */

            /* Set up readfds */

            FD_ZERO(&readfds);
#ifdef MAXNUMPORTS
            for (i = 0; i < numports; i++) {
#endif
                FD_SET(MUDSOCKET, &readfds);
#ifdef MAXNUMPORTS
            } /* for */
#endif
            nfds = min_nfds;
            for (i = max_player + 1; --i >= 0;)
            {
                ip = all_players[i];
                if (!ip)
                    continue;

                if (ip->do_close)
                {
                    ip->do_close &= FLAG_PROTO_ERQ;
                    remove_interactive(ip->ob);
                    continue;
                }

                if (ip->tn_state == TS_READY)
                {
                    /* If telnet is ready for commands, react quickly. */
                    twait = 0;
                }
                else
                {
                    FD_SET(ip->socket, &readfds);
                    if (socket_number(ip->socket) >= nfds)
                        nfds = socket_number(ip->socket)+1;
                }
            } /* for (all players) */
#ifdef ERQ_DEMON
            if (erq_demon >= 0)
            {
                FD_SET(erq_demon, &readfds);
            }
#endif
#ifdef CATCH_UDP_PORT
            if (udp_s >= 0)
            {
                FD_SET(udp_s, &readfds);
            }
#endif

            /* select() until time is up or there is data */

            for (retries = 6;;)
            {
                timeout.tv_sec = twait;
                timeout.tv_usec = 0;
                res = socket_select(nfds, &readfds, 0, 0, &timeout);
                if (res == -1)
                {
                    /* BeOS <= PR2 returns errno -1 instead of EINTR :-( */
                    if (errno == EINTR || errno == -1)
                    {
                        /* We got an alarm, probably need heart_beat.
                         * But finish the select call since we already have
                         * prepared readfds.
                         */
                        if (comm_time_to_call_heart_beat)
                            twait = 0;
                        if (--retries >= 0)
                            continue;
                    }
                    else
                    {
                        perror("select");
                    }

                    /* Despite the failure, pretend select() suceeded with
                     * zero sockets to read, and process heart_beat / buffered
                     * commands.
                     */
                    FD_ZERO(&readfds);
                }
                break;
            } /* for (retries) */

            /* If we got a SIGIO/SIGURG, telnet wants to synch with us.
             */
            if (urgent_data)
            {
                urgent_data = MY_FALSE;
                timeout.tv_sec = 0;
                timeout.tv_usec = 0;
                memset((char *)&exceptfds, 255, (nfds + 7) >> 3);
                if (socket_select(nfds, 0, 0, &exceptfds, &timeout) > 0)
                {
                    for (i = max_player + 1; --i >= 0;)
                    {
                        ip = all_players[i];
                        if (!ip)
                            continue;
                        if (FD_ISSET(ip->socket, &exceptfds))
                        {
                            ip->ts_data = TS_SYNCH;
                            switch (ip->tn_state)
                            {
                              case TS_DATA:
                              case TS_CR:
                              case TS_READY:
                                ip->tn_state = TS_SYNCH;
                                ip->gobble_char = '\0';
                            }
                        }
                    } /* for (all players) */
                }
                /* Maybe the data didn't arrive yet, so try again later.
                 * But don't waste time doing it for too long.
                 */
                else if (current_time - urgent_data_time < 600)
                {
                    urgent_data = MY_TRUE;
                }
            } /* if (urgent_data) */

            /* Initialise the user scan */
            CmdsGiven = 0;
            NextCmdGiver = StartCmdGiver;

#ifdef ERQ_DEMON

            /* --- Handle data from the ERQ ---
             * TODO: This should be a function on its own.
             * TODO: Define the erq messages as structs.
             */
            if (erq_demon >= 0 && FD_ISSET(erq_demon, &readfds))
            {
                mp_int l;
                mp_int msglen;  /* Length of the current erq message */
                mp_int rest;
                int32  handle;
                char  *rp;      /* Read pointer into buf_from_erq[] */

                FD_CLR(erq_demon, &readfds);

                /* Try six times to read data from the ERQ, appending
                 * it to what is already in buf_from_erq[].
                 */
                retries = 6;
                do {
                    l = socket_read(
                      erq_demon,
                      input_from_erq,
                      &buf_from_erq[sizeof buf_from_erq] - input_from_erq
                    );
                } while(l < 0 && errno == EINTR && --retries >= 0);

                /* If there is no data, stop the erq, else handle it. */

                if (l <= 0)
                {
#ifdef DEBUG_ERQ
                    fprintf(stderr, "read %ld bytes from erq demon\n", l);
                    if (l)
                        perror("");
#endif /* DEBUG_ERQ */
                    stop_erq_demon(MY_TRUE);
                }
                else
                {
                    input_from_erq += l;
                    l = input_from_erq - &buf_from_erq[0];
                    rp = buf_from_erq;

                    /* l is the amount of data left to consider,
                     * rp points to the data to be considered next.
                     *
                     * Loop while there are messages left in the buffer.
                     */

                    for (; l >= 8 && l >= (msglen = read_32(rp))
                         ; rp += msglen, l -= msglen)
                    {
                        /* TODO: BOOL */ int keep_handle;

                        /* Is the message length valid?
                         * TODO: use sizeof(struct) here
                         */
                        if (msglen < 8) {
#ifdef DEBUG_ERQ
                            fprintf( stderr
                                   , "invalid length of message from"
                                     "erq demon: %ld bytes\n"
                                   , msglen);
#endif /* DEBUG_ERQ */
                            stop_erq_demon(MY_TRUE);
                            break;
                        }

                        handle = read_32(rp+4); /* get the handle */

                        if (handle == ERQ_HANDLE_KEEP_HANDLE
                         && msglen >= 8)
                        {
                            /* _KEEP_HANDLE wrapper are used when
                             * more messages for the (real) handle
                             * are to be expected. The real message
                             * follows after the _KEEP_HANDLE.
                             */
                            handle = read_32(rp+8); /* the real handle */
                            keep_handle = MY_TRUE;
                            msglen -= 4;  /* adjust the message parameters */
                            l -= 4;
                            rp += 4;
                        }
                        else if (handle == ERQ_HANDLE_RLOOKUP)
                        {
                            /* The result of a hostname lookup. */

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
                        }
                        else
                        {
                            /* remove the callback handle after processing
                             * the message.
                             */
                            keep_handle = MY_FALSE;
                        }

                        /* We have an ERQ message for a user supplied
                         * handle - execute it (after some sanity checks).
                         */

                        if ((uint32)handle < MAX_PENDING_ERQ
                         && (rest = msglen - 8) <= MAX_ARRAY_SIZE
                         && pending_erq[handle].type != T_INVALID)
                        {
                            struct svalue *erqp = &pending_erq[handle];
                            char *cp;
                            struct vector *v;
                            struct svalue *svp;
                            struct object *ob;
                            struct wiz_list *user;

                            command_giver = 0;
                            current_interactive = 0;
                            ob = !CLOSURE_MALLOCED(erqp->x.closure_type)
                                 ? erqp->u.ob
                                 : erqp->u.lambda->ob;
                            current_object = ob;
                            v = allocate_array(rest);
                            current_object = 0;
                            push_referenced_vector(v);
                            push_number(rest);
                            cp = rp + 8;
                            for (svp = v->item; --rest >=0; svp++)
                            {
                                svp->u.number = *cp++;
                            }
                            user = ob->user;
                            if (user->last_call_out != current_time)
                            {
                                user->last_call_out = current_time;
                                CLEAR_EVAL_COST;
                            } else {
                                assigned_eval_cost = eval_cost = user->call_out_cost;
                            }
                            secure_call_lambda(erqp, 2);
                            user->call_out_cost = eval_cost;
                            if (!keep_handle)
                            {
                                free_svalue(erqp);
                                erqp->type = T_INVALID;
                                erqp->u.lvalue = free_erq;
                                free_erq = erqp;
                            }
                        } /* if(valid handle) */

                        /* Messages for invalid handles are no error: e.g. the
                         * object could have gone away unexpectantly before
                         * the erq had time to answer.
                         */

                    } /* for (l,rp in buf_from_erq) */

                    /* Delete the processed data from the buffer */
                    if (rp != buf_from_erq)
                    {
                        move_memory(buf_from_erq, rp, l);
                        input_from_erq = &buf_from_erq[l];
                    }
                } /* if (read data from erq) */
            } /* if (erq socket ready) */

#endif /* ERQ_DEMON */

            /* --- Try to get a new player --- */
#ifdef MAXNUMPORTS
            for (i = 0; i < numports; i++)
            {
#endif
                if (FD_ISSET(MUDSOCKET, &readfds))
                {
                    SOCKET_T new_socket;

                    length = sizeof addr;
                    new_socket = accept(MUDSOCKET, (struct sockaddr *)&addr
                                                 , &length);
                    if ((int)new_socket != -1)
                        new_player(new_socket, &addr, length);
                    else if ((int)new_socket == -1
                      && errno != EWOULDBLOCK && errno != EINTR
                      && errno != EAGAIN && errno != EPROTO )
                    {
                        /* EBADF is a valid cause for an abort,
                         * same goes for ENOTSOCK, EOPNOTSUPP, EFAULT
                         */
                        perror("accept");
                        abort();
                    }
                }
#ifdef MAXNUMPORTS
            } /* for */
#endif
            /* check for alarm signal (heart beat) */
            if (comm_time_to_call_heart_beat)
            {
                time_to_call_heart_beat = MY_TRUE;
                return 0;
            }
        } /* if (no NextCmdGiver) */

#ifdef CATCH_UDP_PORT
        /* See if we got any udp messages.
         * We don't test readfds so that we can accept udp messages with
         * short latency. But for the same reason, it was necessary to
         * include the descriptor number in the set to be selected on.
         * Note for BeOS: since making sockets non-blocking is a bit
         *   tricky, we check if the socket is actually ready, to prevent
         *   freezing.
         */
#ifndef __BEOS__
        if (udp_s >= 0)
#else
        if (udp_s >= 0 && FD_ISSET(udp_s, &readfds))
#endif
        {
            char udp_buf[1024+1], *st;
            int cnt;

            length = sizeof addr;
            cnt = recvfrom(udp_s, udp_buf, sizeof(udp_buf)-1, 0
                          , (struct sockaddr *)&addr, &length);
            if (cnt != -1)
            {
                command_giver = NULL;
                current_interactive = NULL;
                current_object = NULL;
                trace_level = 0;
                udp_buf[sizeof(udp_buf) - 1] = '\0';
                udp_buf[cnt] = '\0';
                st = inet_ntoa(addr.sin_addr);
                push_string_malloced(st);
                push_string_malloced(udp_buf);
                push_number(ntohs(addr.sin_port));
                apply_master_ob(STR_RECEIVE_IMP, 3);
                CLEAR_EVAL_COST;
            }
        } /* if (upd_s) */
#endif
        /* --- The Scan for User Commands --- */

        for (; NextCmdGiver >= 0; IncCmdGiver)
        {
            struct object *snooper;

            ip = all_players[NextCmdGiver];
            if (ip == 0) {
                continue;
            }

            /* Get the data (if any), at max enough to fill .text[] */

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

                /* Here would be the place to send data through an
                 * outportal instead of returning it.
                 */

                telnet_neg(ip);
            } /* if (cmdgiver socket ready) */

            /* if ip->text[0] does not hold a valid character, the outcome
             * of the comparison to '!' does not matter.
             */
            if (ip->noecho & CHARMODE_REQ)
            {
                if (ip->text[0] != '!' || ip->noecho & IGNORE_BANG )
                {
                    /* Unescaped input.
                     * Puts the next character (addressed by
                     * .command_start) into buff[0] and returns it.
                     */
                    if (ip->tn_state != TS_READY)
                    {
                        /* .text[] contains an incomplete negotiation.
                         * Set .chars_ready the amount of pure data available
                         * and temporarily suspend the telnet machine.
                         */
                        length = (TN_START_VALID(ip->tn_state)
                                   ? ip->tn_start
                                   : ip->command_end
                                 ) - ip->command_start;
                        if (!length)
                            continue;
                        ip->save_tn_state = ip->tn_state;
                        ip->chars_ready = length;
                        ip->tn_state = TS_READY;
                    }

                    if ( !(buff[0] = ip->text[ip->command_start++]) )
                    {
                        /* End of line. Reinitialise the telnet machine
                         */
                        buff[0] = '\n';
                        ip->command_start = 0;
                        ip->tn_state = TS_DATA;
                        telnet_neg(ip);
                    }

                    buff[1] = '\0';

                    if (!--ip->chars_ready)
                    {
                        /* All the pure data was read, now restore the
                         * old telnet machine state.
                         * Leave the first char in to make '!' possible
                         */
                        ip->tn_state = ip->save_tn_state;
                        ip->save_tn_state = TS_INVALID;
                        ip->tn_start -= ip->command_start - 1;
                        move_memory( ip->text
                                   , ip->text+ip->command_start
                                   , ( ip->text_end = ip->tn_end =
                              ip->command_end -= ip->command_start - 1)
                                   );
                        ip->command_start = 1;
                    }

                    command_giver = ip->ob;
                    trace_level = ip->trace_level;
                    IncCmdGiver;
                    CmdsGiven = 0;
                    ip->last_time = current_time;
                    return MY_TRUE;
                }
                else if (ip->tn_state != TS_READY)
                {
                    length = (TN_START_VALID(ip->tn_state)
                              ? ip->tn_start
                              : ip->command_end
                             ) - ip->command_start;
                    if (length > ip->chars_ready)
                    {
                        socket_write(ip->socket, ip->text + ip->chars_ready
                                    , length - ip->chars_ready);
                        ip->chars_ready = length;
                    }
                }
            } /* if (CHARMODE_REQ) */

            /* The telnet negotiation produces the commands starting at
             * the beginning of .text[] and terminated with a '\0'. Whenever
             * a command is complete, the tn_state is TS_READY.
             */
            if (ip->tn_state == TS_READY)
            {
                /* We have a command: copy it into buff, handle a
                 * possible snooper and return.
                 */

                strcpy(buff, ip->text);
                command_giver = ip->ob;
                trace_level = ip->trace_level;

                /* Reinitialize the telnet machine, possibly already
                 * producing the next command in .text[].
                 */
                ip->tn_state = TS_DATA;
                telnet_neg(ip);

                /* If he is not in ed, don't let him issue another command
                 * till the poll comes again.
                 */
                if (ip->sent.ed_buffer && CmdsGiven < ALLOWED_ED_CMDS)
                {
                    CmdsGiven++;
                    FD_CLR(ip->socket, &readfds);
                }
                else
                {
                    IncCmdGiver;
                    CmdsGiven = 0;
                }

                /* Manage snooping - should the snooper see type ahead?
                 * Well, he doesn't here.
                 */
                if (NULL != (snooper = ip->snoop_by)
                 && !(snooper->flags & O_DESTRUCTED)
                 && !(ip->noecho & NOECHO_REQ)
                   )
                {
                    if (O_GET_INTERACTIVE(snooper)
                     && O_GET_INTERACTIVE(snooper)->sent.type == SENT_INTERACTIVE)
                    {
                        command_giver = snooper;
                        add_message("%% %s\n", buff);
                    }
                    else
                    {
                        char *snoop_message = alloca(strlen(buff) + 4);
                        (void)sprintf(snoop_message, "%% %s\n", buff);
                        tell_npc(snooper, snoop_message);
                    }
                    command_giver = ip->ob;
                }
                ip->last_time = current_time;
                return MY_TRUE;
            } /* if (have a command) */

        } /* for (NextCmdGiver) */

        /* If we come here, we couldn't find any commandsd:
         * loop and select (on timeout) again.
         */

    } /* while(forever) */

    /* NOTREACHED */
#   undef StartCmdGiver
#   undef IncCmdGiver

} /* get_message() */

/*-------------------------------------------------------------------------*/
void
remove_interactive (struct object *ob)

/* Remove the interactive user <ob> immediately.
 * This function should not be called from within a LPC command execution.
 */

{
    struct object *save = command_giver;
    int i;
    struct interactive *interactive;
    int save_privilege;

    interactive = O_GET_INTERACTIVE(ob);

    /* Proper call? */
    for (i = 0; i < MAX_PLAYERS && all_players[i] != interactive; i++) NOOP;
    if (i >= MAX_PLAYERS)
    {
        fatal("Could not find and remove player %s\n", ob->name);
        abort();
    }
    if (interactive->closing)
        fatal("Double call to remove_interactive()\n");

    interactive->closing = MY_TRUE;
    current_object = ob;
    save_privilege = malloc_privilege;

    /* If the object is not destruct, inform the master */

    if ( !(ob->flags & O_DESTRUCTED) )
    {
        command_giver = 0;
        current_interactive = 0;
        push_object(ob);
        malloc_privilege = MALLOC_MASTER;
        apply_master_ob(STR_DISCONNECT, 1);
        /* master might have used exec() */
        ob = interactive->ob;
    }

    interactive->catch_tell_activ = MY_FALSE;

    /* Untie eventual snooping relations */

    if (interactive->snoop_by)
    {
        if (O_GET_INTERACTIVE(interactive->snoop_by)
         && O_GET_INTERACTIVE(interactive->snoop_by)->sent.type ==
            SENT_INTERACTIVE)
        {
            O_GET_INTERACTIVE(interactive->snoop_by)->snoop_on = NULL;
        }
        else
        {
            free_object(interactive->snoop_by, "remove_interactive");
        }
        interactive->snoop_by = NULL;
    }
    if (interactive->snoop_on)
    {
        interactive->snoop_on->snoop_by = NULL;
        interactive->snoop_on = NULL;
    }

    command_giver = ob;

#ifdef ERQ_DEMON
    /* If this object is disconnected because it was used to connect
     * a new ERQ, put the connection into place and greet the ERQ.
     */
    if (interactive->do_close & FLAG_PROTO_ERQ
     && interactive->socket == erq_proto_demon)
    {
        static unsigned char erq_welcome[] = { IAC, TELOPT_BINARY };

        add_message(message_flush);
        erq_demon = interactive->socket;
        erq_proto_demon = -1;
        socket_write(erq_demon, erq_welcome, sizeof erq_welcome);
    }
    else
#endif
    {
        /* Say goodbye to the user. */
        trace_level |= interactive->trace_level;
#if 0
        add_message(destruct_add_message_format, "Closing down.\n");
#endif
        add_message(message_flush);
        shutdown(interactive->socket, 2);
        socket_close(interactive->socket);
    } /* if (erq or user) */

#if defined(ACCESS_CONTROL)
    release_host_access(interactive->access_class);
      /* One user less in this class */
#endif

    num_player--;

    /* Release all associated resources */

    if (interactive->input_to)
    {
        free_input_to(interactive->input_to);
    }

    if (interactive->modify_command)
    {
        free_object(interactive->modify_command, "remove_interactive");
    }

    free_svalue(&interactive->prompt);
    free_svalue(&interactive->default_err_message);

    /* If the shadow_sentence in the interactive structure is used
     * for its real purposes, or if there is still an ed buffer open,
     * create a copy of the shadow_sentence and put it into the place
     * of the interactive structure.
     * Else, just remove the interactive structure.
     */
    if (interactive->sent.ed_buffer
     || interactive->sent.shadowing || interactive->sent.shadowed_by)
    {
        struct shadow_sentence *shadow_sent;

        malloc_privilege = MALLOC_SYSTEM;
        shadow_sent = (struct shadow_sentence *)alloc_sentence();
        *shadow_sent = interactive->sent;
        shadow_sent->type = SENT_SHADOW;
        ob->sent = (struct sentence *)shadow_sent;
    }
    else
    {
        ob->sent = interactive->sent.next;
        ob->flags &= ~O_SHADOW;
    }

    xfree((char *)interactive);
    all_players[i] = NULL;
    while (max_player && !all_players[max_player])
        max_player--;
    free_object(ob, "remove_interactive");

    command_giver = check_object(save);
    current_object = NULL;
    malloc_privilege = save_privilege;
}

#ifdef ACCESS_CONTROL

/*-------------------------------------------------------------------------*/
void
refresh_access_data(void (*add_entry)(struct sockaddr_in *, long*) )

/* Called from access_check after the ACCESS_FILE has been (re)read, this
 * function has to call the passed callback function add_entry for every
 * user currently logged in.
 */

{
    struct interactive **user;
    int n;

    user = all_players;
    for (n = max_player + 2; --n; user++) {
        if (*user)
            (*add_entry)(&(*user)->addr, &(*user)->access_class);
    }
}

#endif /* ACCESS_CONTROL */

/*-------------------------------------------------------------------------*/
struct vector *
users (void)

/* EFUN users()
 *
 * Return a (possibly empty) vector of all interactive user objects.
 */

{
    struct object *ob;
    int n, num;
    struct vector *ret;
    struct interactive **user;
    struct svalue *svp;

    /* Count the active users */
    num = 0;
    user = all_players;
    for (n = max_player + 2; --n; user++)
    {
        if (*user && !((*user)->ob->flags & O_DESTRUCTED))
            num++;
    }

    /* Get the result array and fill it */

    ret = allocate_array(num);
    svp = ret->item;
    user = all_players;
    for (n = max_player + 2; --n; user++)
    {
        if (*user && !((ob = (*user)->ob)->flags & O_DESTRUCTED))
        {
            svp->type = T_OBJECT;
            svp->u.ob = ob;
            add_ref(ob, "users");
            svp++;
        }
    }

    return ret;
}

/*-------------------------------------------------------------------------*/
static void
new_player (SOCKET_T new_socket, struct sockaddr_in *addr, int addrlen)

/* Accept (or reject) a new connection on <new_socket> from <addr> (length
 * of structure is <addrlen>).
 *
 * Called when get_message() detects a new connection on one of the
 * login ports, this function checks if the user may access the mud.
 * If yes, a new interactive structure is generated and bound to the
 * master, then master->connect() is called. This call is expected
 * to return an object and the interactive structure is rebound to
 * this object. Finally, logon() is called in this object. Alternatively,
 * master->connect() may exec() the connection away from the master,
 * in which case no further action will be taken after the return
 * from that call.
 *
 * If the connection can't be accepted for some reason, a failure
 * message will be send back to the user and the socket will be
 * closed.
 */

{
    int   i;             /* Index of free slot in all_players[] */
    char *message;       /* Failure message */
    struct object *ob;   /* Login object */
    struct svalue *ret;  /* LPC call results */
    struct interactive *new_interactive;
                         /* The new interactive structure */
    struct shadow_sentence *save_shadow;
#ifdef ACCESS_CONTROL
    long class;     /* Access class */
#endif

    /* Set some useful socket options */
    set_socket_nonblocking(new_socket);
    set_close_on_exec(new_socket);
    set_socket_own(new_socket);

#ifdef ACCESS_CONTROL
    /* Check for access restrictions for this connection */
    message = allow_host_access(addr, &class);
#ifdef ACCESS_LOG
    {
        FILE *log_file = fopen (ACCESS_LOG, "a");

        if (log_file) {
            FCOUNT_WRITE(log_file);
            fprintf(log_file, "%s: %s\n",
                inet_ntoa(addr->sin_addr), message ? "denied" : "granted");
            fclose(log_file);
        }
    }
#endif
    if (message)
    {
        socket_write(new_socket, message, strlen(message));
        socket_write(new_socket, "\r\n", 2);
        socket_close(new_socket);
        return;
    }
#endif /* ACCESS_CONTROL */

    if (d_flag > 1)
        debug_message("New player at socket %d.\n", new_socket);

    /* Look for an empty slot in all_players[] */
    for (i = 0; i < MAX_PLAYERS && all_players[i] != NULL; i++) NOOP;
    if (i >= MAX_PLAYERS)
    {
        message = "The mud is full. Come back later.\r\n";
        /* calling closures here would need special error handling */
        if (closure_hook[H_NO_IPC_SLOT].type == T_STRING)
        {
            message = closure_hook[H_NO_IPC_SLOT].u.string;
        }
        socket_write(new_socket, message, strlen(message));
        socket_close(new_socket);
        return;
    }

    /* The master must be loaded and free to accept a login */
    assert_master_ob_loaded();
    if (O_GET_INTERACTIVE(master_ob)
     && O_GET_INTERACTIVE(master_ob)->sent.type == SENT_INTERACTIVE)
    {
        message = "Cannot accept connections. Come back later.\r\n";
        socket_write(new_socket, message, strlen(message));
        socket_close(new_socket);
        return;
    }

    command_giver = master_ob;
    trace_level = 0;
    new_interactive = xalloc(sizeof (struct interactive));
    if (!new_interactive)
    {
        message = "Cannot accept connection (out of memory). Come back later.\r\n";
        socket_write(new_socket, message, strlen(message));
        socket_close(new_socket);
        return;
    }

    /* If the master is shadowed, copy the shadow into the interactive
     * structure, else enter the structure as _the_ shadow to the master.
     */
    if (master_ob->flags & O_SHADOW)
    {
        new_interactive->sent = *O_GET_SHADOW(master_ob);
        free_shadow_sent(O_GET_SHADOW(master_ob));
    }
    else
    {
        new_interactive->sent.next = master_ob->sent;
        new_interactive->sent.shadowed_by = NULL;
        new_interactive->sent.shadowing = NULL;
        new_interactive->sent.ed_buffer = NULL;
    }
    new_interactive->sent.type = SENT_INTERACTIVE;
    master_ob->sent = (struct sentence *)new_interactive;
    master_ob->flags |= O_ONCE_INTERACTIVE|O_SHADOW;
    new_interactive->ob = master_ob;

    /* Initialize the rest of the interactive structure */

    new_interactive->input_to = NULL;
    new_interactive->prompt.type = T_STRING;
    new_interactive->prompt.x.string_type = STRING_CONSTANT;
    new_interactive->prompt.u.string = "> ";
    new_interactive->modify_command = NULL;
    new_interactive->closing = MY_FALSE;
    new_interactive->do_close = 0;
    new_interactive->noecho = 0;
    new_interactive->gobble_char = 0;
    new_interactive->catch_tell_activ = MY_TRUE;
    new_interactive->text_end = 0;
    new_interactive->command_start = 0;
    new_interactive->command_end = 0;
    new_interactive->chars_ready = 0;
    new_interactive->save_tn_state = TS_INVALID;
    new_interactive->tn_start = 0;
    new_interactive->tn_end = 0;
    new_interactive->tn_state = TS_DATA;
    new_interactive->ts_data = TS_DATA;
    new_interactive->snoop_on = NULL;
    new_interactive->snoop_by = NULL;
    new_interactive->last_time = current_time;
    new_interactive->default_err_message.type = T_INVALID;
    new_interactive->trace_level = 0;
    new_interactive->trace_prefix = 0;
    new_interactive->message_length = 0;
    memset(new_interactive->charset, 255, sizeof new_interactive->charset);
    new_interactive->charset['\n'/8] &= ~(1 << '\n' % 8);
    new_interactive->charset['\0'/8] &= ~(1 << '\0' % 8);
    new_interactive->text[0] = '\0';
    memcpy((char *)&new_interactive->addr, (char *)addr, addrlen);
#if defined(ACCESS_CONTROL)
    new_interactive->access_class = class;
#endif
    new_interactive->socket = new_socket;

    /* Add the new interactive structure to the list of users */

    all_players[i] = new_interactive;
    if (i > max_player)
        max_player = i;
    num_player++;

    /* The player object has one extra reference. */
    add_ref(master_ob, "new_player");

    /* Call master->connect() and evaluate the result.
     */
    ret = apply_master_ob(STR_CONNECT, 0);
    if (new_interactive != O_GET_INTERACTIVE(master_ob))
        return;
    if (ret == NULL
     || ret->type != T_OBJECT
     || ( O_GET_INTERACTIVE(ob = ret->u.ob) &&
          O_GET_INTERACTIVE(ob)->sent.type == SENT_INTERACTIVE) )
    {
        remove_interactive(master_ob);
        return;
    }
    command_giver = master_ob;
    add_message(message_flush);

    /* There was an object returned from connect(). Use this as the
     * user object.
     */

    /* Remember that this shadow sentence belongs to the master! */
    save_shadow = (struct shadow_sentence *)alloc_sentence();
    *save_shadow = new_interactive->sent;

    /* If the new user object already has a shadow, copy its data
     * into the interactive structure, else use the interactive
     * structure as the first shadow of the user object.
     * Any existing ed buffer is preserved.
     */
    if (ob->flags & O_SHADOW)
    {
        struct ed_buffer *buf = new_interactive->sent.ed_buffer;

        new_interactive->sent = *O_GET_SHADOW(ob);
        if (buf)
        {
            save_shadow->ed_buffer = new_interactive->sent.ed_buffer;
            new_interactive->sent.ed_buffer = buf;
        }
        new_interactive->sent.type = SENT_INTERACTIVE;
        free_shadow_sent(O_GET_SHADOW(ob));
    }
    else
    {
        new_interactive->sent.next = ob->sent;
        new_interactive->sent.shadowed_by = NULL;
        new_interactive->sent.shadowing = NULL;
        save_shadow->ed_buffer = NULL;
    }
    ob->sent = (struct sentence *)new_interactive;
    new_interactive->ob = ob;
    ob->flags |= O_ONCE_INTERACTIVE|O_SHADOW;

    /* Restore the shadow setting of the master.
     */
    master_ob->flags &= ~O_ONCE_INTERACTIVE;
    if (save_shadow->shadowing
     || save_shadow->shadowed_by
     || save_shadow->ed_buffer)
    {
        save_shadow->type = SENT_SHADOW;
        master_ob->sent = (struct sentence *)save_shadow;
    }
    else
    {
        master_ob->sent = save_shadow->next;
        free_shadow_sent(save_shadow);
        master_ob->flags &= ~O_SHADOW;
    }
    free_object(master_ob, "reconnect");

    /* Prepare to call logon() in the new user object.
     */
    add_ref(ob, "new_player");
    command_giver = ob;
    current_interactive = ob;
    if (new_interactive->snoop_on)
    {
        new_interactive->snoop_on->snoop_by = ob;
    }
#ifdef ERQ_DEMON
    send_erq(ERQ_HANDLE_RLOOKUP, ERQ_RLOOKUP,
      (char *)&new_interactive->addr.sin_addr.s_addr, 4
    );
#endif
    logon(ob);
    flush_all_player_mess();
}

/*-------------------------------------------------------------------------*/
void
set_noecho (struct interactive *i, char noecho)

/* Change the input mode <i>->noecho to the given <noecho>, performing all
 * necessary telnet negotiations. If the driverhook H_NOECHO is set,
 * the hook function is expected to do all the negotiations.
 */

{
    char old, confirm;
    struct object *ob;

    old = i->noecho;

    confirm =
      noecho | CHARMODE_REQ_TO_CHARMODE(noecho & (NOECHO_REQ|CHARMODE_REQ));
    i->noecho = confirm;

    confirm |= NOECHO_ACKSHIFT(confirm);
    if ((confirm ^ old) & (NOECHO_MASK|CHARMODE_MASK) )
    {
        ob = i->ob;
        if (closure_hook[H_NOECHO].type == T_STRING)
        {
            push_number(noecho);
            push_object(ob);
            secure_apply(closure_hook[H_NOECHO].u.string, ob, 2);
        }
        else if (closure_hook[H_NOECHO].type == T_CLOSURE)
        {
            if (closure_hook[H_NOECHO].x.closure_type == CLOSURE_LAMBDA)
                closure_hook[H_NOECHO].u.lambda->ob = ob;
            push_number(noecho);
            push_object(ob);
            secure_call_lambda(&closure_hook[H_NOECHO], 2);
        }
        else
        {
            struct object *save;

            save = command_giver;
            command_giver = ob;
            if (~confirm & old & NOECHO)
            {
                send_wont(TELOPT_ECHO);
            }
            else if (confirm & ~old & NOECHO_MASK)
            {
                send_will(TELOPT_ECHO);
            }
            if (i->supress_go_ahead && !(confirm & (NOECHO|CHARMODE)))
            {
                i->supress_go_ahead = MY_FALSE;
                send_wont(TELOPT_SGA);
            }
            /* Only using SGA for charmode is supported hardcoded.
             * To make more sophisticated negotiations, e.g. using LINEMODE,
             * use the H_NOECHO hook.
             */
            if (~confirm & old & CHARMODE_MASK)
            {
                if (old & CHARMODE)
                    send_dont(TELOPT_SGA);
                if (i->save_tn_state != TS_INVALID)
                {
                    i->chars_ready = 0;
                    i->tn_state = i->save_tn_state;
                }
                if (i->command_start)
                {
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
            }
            else if (confirm & ~old & CHARMODE_MASK)
            {
                send_do(TELOPT_SGA);
                /* some telnet implementations mix up DO and WILL SGA, thus
                 * we send WILL SGA as well.
                 */
                send_will(TELOPT_SGA);
                i->supress_go_ahead = MY_TRUE;
            }
            command_giver = save;
        }
    }
}

/*-------------------------------------------------------------------------*/
/* TODO: BOOL */ int
call_function_interactive (struct interactive *i, char *str)

/* Perform a pending input_to() for this user <i> and the input <str>
 * Return TRUE if an input_to() was pending and executed, and FALSE
 * if the input was not processed.
 *
 * This function is called by the backend as part of the input processing.
 */

{
    char *function;     /* actual input_to function */
    struct object *ob;  /* object holding <function> */
    int extra;

    /* _Is_ there an input_to() pending? */
    if (!i->input_to)
        return MY_FALSE;

    /* Yes, there is. Check if it's still valid. */

    function = i->input_to->function;
    ob = i->input_to->ob;
    if (ob->flags & O_DESTRUCTED)
    {
        /* Sorry, the object has selfdestructed ! */
        set_noecho(i, 0);
        free_input_to(i->input_to);
        i->input_to = NULL;
        return MY_FALSE;
    }

    if (i->noecho) {
        /* if there is a series of noecho/charmode input, we should only
         * negotiate when we know that the state actually should change.
         * In other words: should the input_to function request NOECHO
         * again, the NOECHO_STALE bit will be cleared and we will not
         * turn NOECHO off after the call.
         */
        i->noecho |= NOECHO_STALE;
    }
    free_object(ob, "call_function_interactive");

    /* Setup the call to the input_to function. */

    push_volatile_string(str);
    if ( 0 != (extra = i->input_to->num_arg) ) {
        memcpy(
          (char *)(inter_sp + 1),
          (char *)i->input_to->arg,
          extra * sizeof *inter_sp
        );
        inter_sp += extra;
    }

    /* Clear the input_to() reference in case the function called
     * sets up a new one.
     */

    xfree((char *)i->input_to);
    i->input_to = NULL;

    /* Call the function. current_object is set to the object holding
     * the function so that static functions can be used.
     */
    current_object = ob;
    (void)apply(function, ob, 1 + extra);
    free_string(function);

    /* If NOECHO is no longer needed, turn it off. */

    if (i->noecho & NOECHO_STALE)
    {
        set_noecho(i, 0);
    }

    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
static /* TODO: BOOL */ int
set_call (struct object *ob, struct input_to *it, /* TODO: BOOL */ int noecho)

/* Set a a new input_to <it> with <noecho> on or off to the interactive
 * object <ob>. It is assumed that <ob> has no input_to pending.
 * Return TRUE on success.
 */

{
    struct interactive *ip;

    if (ob == NULL || it == NULL)
        return MY_FALSE;
    ip = O_GET_INTERACTIVE(ob);
    if (!ip || ip->sent.type != SENT_INTERACTIVE || ip->input_to
     || ip->closing)
    {
        return MY_FALSE;
    }
    ip->input_to = it;
    if (noecho)
    {
        set_noecho(ip, noecho);
    }
    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
void
remove_all_players (void)

/* Destruct all user objects. This is first tried by calling master->remove()
 * for every object. If this doesn't destruct the user object,
 * emergency_destruct() is used.
 * The function is called when the game is shut down.
 */

{
    int i;

    for (i = 0; i < MAX_PLAYERS; i++) {
        if (all_players[i] == 0)
            continue;
        command_giver = all_players[i]->ob;
        trace_level |= all_players[i]->trace_level;
        CLEAR_EVAL_COST;
        push_object(all_players[i]->ob);
        (void)apply_master_ob(STR_REMOVE_PL, 1);
        if ( !(all_players[i]->ob->flags & O_DESTRUCTED) ) {
            emergency_destruct(all_players[i]->ob);
        }
    }
}

/*-------------------------------------------------------------------------*/
void
set_prompt (char *str)

/* Set the prompt of the current command_giver to <str>.
 */

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

/*-------------------------------------------------------------------------*/
struct svalue *
query_prompt (struct object *ob)

/* Return the prompt setting of interactive object <ob>.
 * Note that you will get a pointer to the very svalue the object
 * uses.
 */

{
#ifdef DEBUG
    if (O_GET_INTERACTIVE(ob)->sent.type != SENT_INTERACTIVE)
        fatal("query_prompt() of non-interactive object\n");
#endif
    return &O_GET_INTERACTIVE(ob)->prompt;
}

/*-------------------------------------------------------------------------*/
void
print_prompt (void)

/* Print the prompt of the current command_giver, unless disabled
 * by input_to. If the prompt is set to a closure, the closure
 * is called and expected to return the actual prompt string or
 * to print the prompt itself.
 */

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
    if (ip->input_to == NULL)
    {
        struct svalue *prompt;

        prompt = &ip->prompt;
        if (prompt->type == T_CLOSURE)
        {
            call_lambda(prompt, 0);
            prompt = inter_sp;
            if (prompt->type != T_STRING)
            {
                free_svalue(prompt);
            }
            else
            {
                /* beware: add_message() might cause an error. Thus, the LPC
                 * stack has to include the prompt to free it then.
                 */
                add_message("%s", prompt->u.string);
                free_string_svalue(prompt);
            }
            inter_sp--;
        }
        else
        {
            add_message("%s", prompt->u.string);
        }
    } /* if (no input_to) */
}

/*-------------------------------------------------------------------------*/
int
set_snoop (struct object *me, struct object *you)

/* Set a snoop from <me> on the IO of <you>. If <you> is NULL, an
 * existing snoop is terminated. <me> need not to be an interactive
 * user.
 *
 * Return 1 on success, -1 if a snooping loop would be caused, 0 for
 * any other failure.
 *
 * The function calls master->valid_snoop() to test if the snoop
 * is allowed.
 */

{
    struct interactive *on = NULL; /* interactive struct of <you> */
    struct interactive *by = NULL; /* interactive struct of <me> */
    struct interactive *tmp;
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
    ret = apply_master_ob(STR_VALID_SNOOP, 2);

    if (!ret || ret->type != T_NUMBER || ret->u.number == 0)
        return 0;

    if (me->flags & O_DESTRUCTED)
        return 0;


    /* Test is <me> is able to snoop anyway.
     * Set <by> to <me>'s interactive struct if yes.
     */
    if ( NULL != (by = O_GET_INTERACTIVE(me)) ) {
        if (by->sent.type != SENT_INTERACTIVE)
            by = NULL;
        else if (by->closing)
            return 0;
    }

    if (you)
    {
        /* Test if <you> can be snooped at all.
         * Set <on> to <you>'s interactive struct if yes.
         */
        if (you->flags & O_DESTRUCTED)
            return 0;
        if (!(on = O_GET_INTERACTIVE(you))
         || on->sent.type != SENT_INTERACTIVE || on->closing)
        {
            return 0;
        }
    }
    else
    {
        /* Stop snoop.
         * For this, set <on> to the interactive struct of the snoops
         * victim. If <by> is NULL, <me> is propably a netdead user
         * and we have to scan the list of users for the victim.
         */

        if (!by)
        {
            int i;

            for (i = max_player+1;;)
            {
                if (--i < 0)
                    return 0;
                if (NULL != (on = all_players[i]) && on->snoop_by == me)
                    break;
            }
            if (on->closing)
                return 0;
            free_object(me, "new_set_snoop");
        }
        else
        {
            on = by->snoop_on;
            if (!on || on->closing)
                return 0;
            by->snoop_on = 0;
        }
        on->snoop_by = 0;
        return 1;
    }

    /* If we come here, a snoop on <you> by <me> is possible.
     * Now protect against snooping loops.
     */

    for (tmp = on; tmp; tmp = tmp->snoop_on)
    {
        if (tmp == by)
            return -1;
    }

    /* Terminate previous snoop, if any */
    if (on->snoop_by)
    {
        struct interactive *ip;

        if (NULL != (ip = O_GET_INTERACTIVE(on->snoop_by))
         && ip->sent.type == SENT_INTERACTIVE)
        {
            if (ip->closing)
                return 0;
            ip->snoop_on = 0;
        }
        else
        {
            free_object(on->snoop_by, "new_set_snoop");
        }
        on->snoop_by = NULL;
    }

    /* Initialise the new snoop */
    if (by)
    {
        if (by->snoop_on)
        {
            if (by->snoop_on->closing)
                return 0;
            by->snoop_on->snoop_by = 0;
            by->snoop_on = 0;
        }
        by->snoop_on = on;
    }
    else
    {
        add_ref(me, "new_set_snoop");
    }

    on->snoop_by = me;
    return 1;
}

/*=========================================================================*/
/*                      Telnet Support
 */

/* Note: when stored in char variables, IAC can be equal to EOF.
 * This can cause sprintf(), which is used in add_message(), to abort
 * output after EOF. Therefore, don't try to send anything after the IAC
 * in the same call to add_message().
 */

/*-------------------------------------------------------------------------*/
static void
send_wont (int option)

/* Send IAC WONT <option> */

{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", WONT, option);
      add_message(message_flush);
    )
}

/*-------------------------------------------------------------------------*/
static void
send_dont (int option)

/* Send IAC DONT <option> */

{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", DONT, option);
      add_message(message_flush);
    )
}

/*-------------------------------------------------------------------------*/
static void
send_will (int option)

/* Send IAC WILL <option> */

{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", WILL, option);
      add_message(message_flush);
    )
}

/*-------------------------------------------------------------------------*/
static void
send_do (int option)

/* Send IAC DO <option> */

{
    SEND_TELNET_COMMAND(
      add_message("%c", IAC);
      add_message("%c%c", DO, option);
      add_message(message_flush);
    )
}

/*-------------------------------------------------------------------------*/
static void
reply_nil (int option UNUSED)

/* Dummy function which does nothing. */

{
#ifdef __MWERKS__
#    pragma unused(option)
#endif
}

/*-------------------------------------------------------------------------*/
static void
reply_to_do_echo (int option)

/* Send IAC WILL <option> if we were told before to not echo, or
 * send IAC WONT <option> if we are echoing.
 */

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

/*-------------------------------------------------------------------------*/
static void
reply_to_dont_echo (int option)

/* Send IAC WONT <option> if we were granted the option before.
 */

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

/*-------------------------------------------------------------------------*/
static void
reply_to_do_sga (int option)

/* Send IAC WILL <option> if Suppress Go Ahead is not already active and
 * mark it as active, send IAC WONT <option> if neither in NOECHO or
 * CHARMODE mode.
 */

{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->noecho & (NOECHO_MASK|CHARMODE_MASK)) {
        if (!ip->supress_go_ahead) {
            ip->supress_go_ahead = MY_TRUE;
            send_will(option);
        }
    } else {
        send_wont(option);
    }
}

/*-------------------------------------------------------------------------*/
static void
reply_to_dont_sga (int option)

/* Send IAC WONT <option> if Suppress Go Ahead is active and mark it as
 * inactive.
 */

{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);

    if (ip->supress_go_ahead) {
        ip->supress_go_ahead = 0;
        send_wont(option);
    }
}

/*-------------------------------------------------------------------------*/
static void
reply_to_will_sga (int option)

/* Send IAC DO <option> if CHARMODE is requested but not active yet,
 * send IAC DONT <option> if CHARMODE is neither requested nor active.
 */

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

/*-------------------------------------------------------------------------*/
static void
reply_to_wont_sga (int option)

/* Send IAC DONT <option> if CHARMODE was granted before.
 */

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

/*-------------------------------------------------------------------------*/
static struct svalue *
h_telnet_neg (int n)

/* Call the H_TELNET_NEG driverhook with <n> arguments on the interpreter
 * stack. Return the result from that call, or NULL if the hook isn't
 * set.
 */

{
    struct svalue *svp;

    CLEAR_EVAL_COST;
    if (closure_hook[H_TELNET_NEG].type == T_STRING)
    {
        svp =
          secure_apply(closure_hook[H_TELNET_NEG].u.string, command_giver, n);
    }
    else if (closure_hook[H_TELNET_NEG].type == T_CLOSURE)
    {
        if (closure_hook[H_TELNET_NEG].x.closure_type == CLOSURE_LAMBDA)
            closure_hook[H_TELNET_NEG].u.lambda->ob = command_giver;
        svp = secure_call_lambda(&closure_hook[H_TELNET_NEG], n);
    }
    else
    {
        svp = NULL;
    }
    return svp;
}

/*-------------------------------------------------------------------------*/
static void
reply_h_telnet_neg (int option)

/* Call the H_TELNET_NEG driver hook with <tn_state> <option> as
 * arguments. If the hook is not defined, send WONT <option> if
 * the state is TS_DO, or send DONT <option> if the state is TS_WILL.
 */

{
    struct interactive *ip = O_GET_INTERACTIVE(command_giver);
    int i = 0;

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

/*-------------------------------------------------------------------------*/
void
init_telopts (void)

/* Initialise the telopts_xxx[] tables.
 * The default setting is such that requests are ignored or rejected.
 */

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
    telopts_do[TELOPT_NEWENV] = reply_h_telnet_neg;
    telopts_dont[TELOPT_NEWENV] = reply_h_telnet_neg;
    telopts_will[TELOPT_NEWENV] = reply_h_telnet_neg;
    telopts_wont[TELOPT_NEWENV] = reply_h_telnet_neg;
    telopts_do[TELOPT_ENVIRON] = reply_h_telnet_neg;
    telopts_dont[TELOPT_ENVIRON] = reply_h_telnet_neg;
    telopts_will[TELOPT_ENVIRON] = reply_h_telnet_neg;
    telopts_wont[TELOPT_ENVIRON] = reply_h_telnet_neg;
    telopts_do[TELOPT_XDISPLOC] = reply_h_telnet_neg;
    telopts_dont[TELOPT_XDISPLOC] = reply_h_telnet_neg;
    telopts_will[TELOPT_XDISPLOC] = reply_h_telnet_neg;
    telopts_wont[TELOPT_XDISPLOC] = reply_h_telnet_neg;
    telopts_do[TELOPT_LINEMODE] = reply_h_telnet_neg;
    telopts_dont[TELOPT_LINEMODE] = reply_h_telnet_neg;
    telopts_will[TELOPT_LINEMODE] = reply_h_telnet_neg;
    telopts_wont[TELOPT_LINEMODE] = reply_h_telnet_neg;
    telopts_do[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_dont[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_will[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_wont[TELOPT_NAWS] = reply_h_telnet_neg;
    telopts_do[TELOPT_TTYPE] = reply_h_telnet_neg;
    telopts_dont[TELOPT_TTYPE] = reply_h_telnet_neg;
    telopts_will[TELOPT_TTYPE] = reply_h_telnet_neg;
    telopts_wont[TELOPT_TTYPE] = reply_h_telnet_neg;
    telopts_do[TELOPT_TSPEED] = reply_h_telnet_neg;
    telopts_dont[TELOPT_TSPEED] = reply_h_telnet_neg;
    telopts_will[TELOPT_TSPEED] = reply_h_telnet_neg;
    telopts_wont[TELOPT_TSPEED] = reply_h_telnet_neg;

    telopts_do[TELOPT_EOR] = reply_h_telnet_neg;
    telopts_dont[TELOPT_EOR] = reply_h_telnet_neg;
    telopts_will[TELOPT_EOR] = reply_h_telnet_neg;
    telopts_wont[TELOPT_EOR] = reply_h_telnet_neg;

    /* Tinyfugue can do bad things to your health */
    telopts_do[EOR] = reply_h_telnet_neg;
    telopts_dont[EOR] = reply_h_telnet_neg;
    telopts_will[EOR] = reply_h_telnet_neg;
    telopts_wont[EOR] = reply_h_telnet_neg;

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

/*-------------------------------------------------------------------------*/
void
mudlib_telopts (void)

/* Set all telopts_xxx[] entries to reply_h_telnet_neg().
 * This means that the mudlib does all the telnet negotiation.
 */

{
    int i;

    for (i = NTELOPTS; --i >= 0; ) {
        telopts_do[i] = telopts_dont[i] =
          telopts_will[i] = telopts_wont[i] = reply_h_telnet_neg;
    }
}

/*-------------------------------------------------------------------------*/
static void
telnet_neg (struct interactive *ip)

/* Process the data read from the socket, performing any telnet negotiations
 * necessary, and extract the 'pure' command text. When the function returns,
 * all new data in .text[] has been used and .text_end set back as far
 * as possible.
 *
 * The start state for the telnet machine is TS_DATA, and whenever a command
 * text has been completed, it assumes the TS_READY state.
 */

{
    fd_set exceptfds;
    char *from;   /* Next char to process */
    char *to;     /* Where to store the extracted command text */
    int   state;
    int   ch;     /* Current character */
    char *first;  /* Begin of the last pure command text */
    char *end;    /* End of data in text[] */

    first = ip->text;
    from = &first[ip->tn_end];
    end = &first[ip->text_end];

    /* Gobble the character *from if gobble_char is set.
     * Also test for the end of current buffer content.
     */
    for (;;)
    {
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

    /* The processing loop */

    do {
        ch = (*from++ & 0xff);
        switch(ip->tn_state)
        {
        case TS_READY:
            /* Previous command hasn't been read yet - don't clobber it! */
            return;

        ts_data:
            /* Most state functions end with a jump here to check if they
             * exhausted their input.
             */
            if (from >= end)
            {
        data_exhausted:
                ip->text_end = ip->tn_end = ip->command_end = to - first;
                if (ip->text_end >= MAX_TEXT)
                {
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
            /* FALLTHROUGH */

        case TS_DATA: /* --- Copy/interpret plain data --- */
            switch(ch)
            {
            case IAC:
            new_iac:
                  state = TS_IAC;
        change_state:
                  ip->tn_state = state;
                  continue;

            case '\b':        /* Backspace */
            case 0x7f:        /* Delete */
                /* In Linemode, just move to one char back.
                 * In Charmode with escaped input, write the data gathered
                 * so far and add a rubout sequence ('\b \b').
                 * In Charmode with unescaped input, just pass it on to
                 * the mudlib.
                 */
                if ( !(ip->noecho & CHARMODE_REQ) )
                {
                    if (to > first)
                        to--;
                    goto ts_data;
                }

                if (ip->text[0] == '!' && ! (ip->noecho & IGNORE_BANG) )
                {
                    if (to > &ip->text[ip->chars_ready])
                    {
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
                /* FALLTHROUGH */

            case '\0':
                goto ts_data;

              case '\r':
                if (from >= end)
                {
                    /* This might be a fragmented CR NL, CR NUL, or
                     * a broken client that ends lines with CR only.
                     * If we are just looking for a line of text, we
                     * can proceed now, else we have to wait for the
                     * next character to make our decisions.
                     */
                    if ( !(ip->noecho & CHARMODE_REQ) ||
                         (ip->text[0] == '!' && ! (ip->noecho & IGNORE_BANG)) )
                    {
                        ip->gobble_char = '\n';
                        goto full_newline;
                    }
                    ip->tn_state = TS_CR;
                    goto data_exhausted;
                }
                else
                {
                    ch = (*from++ & 0xff);
        ts_cr:
                    if (ch != '\n')
                    {
                        from--;
                        if ((ip->noecho & CHARMODE_REQ) &&
                            (ip->text[0] != '!' || ip->noecho & IGNORE_BANG))
                        {
                            ip->tn_state = TS_DATA;
                            *to++ = '\r';
                            goto ts_data;
                        }
                    }
                } /* if (from >= end) or not */

        full_newline:
                /* Proper line end found: set telnet machine into TS_READY,
                 * terminate the command with \0 and return.
                 */
                {
                    ip->tn_state = TS_READY;
                    ip->command_end = 0;
                    ip->tn_end = from - first;
                    *to = '\0';
                    return;
                }

            case '\n':
                if ( !(ip->noecho & CHARMODE_REQ) ||
                     (ip->text[0] == '!' && ! (ip->noecho & IGNORE_BANG)) )
                {
                    ip->gobble_char = '\r';
                }
                goto full_newline;
            } /* switch(ch) */

            /* NOTREACHED */

        case TS_CR:
            /* Complete a CR-?? combination. */
            goto ts_cr;

        ts_iac:
        case TS_IAC:
            /* Begin a telnet negotiation */
            switch(ch)
            {
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
                if (ip->ts_data == TS_SYNCH)
                {
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
            } /* switch(ch) */
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
        } /* switch (ip->tn_state) */

    } while(from < end);

    /* We used all the new data in .text[] but found no complete command.
     * Reset all pointers necessary to read new data.
     */

    ip->text_end = ip->tn_end = ip->command_end = to - first;
    if (ip->text_end == MAX_TEXT)
    {
        /* telnet negotiation shouldn't have such large data chunks.
         * Ignore all data altogether and return to text mode.
         */
        ip->text_end = ip->tn_end = ip->command_end = 0;
        ip->tn_state = TS_DATA;
    }
} /* telnet_neg() */

/* End of Telnet support */
/*=========================================================================*/
/*                      ERQ Support
 */

#ifdef ERQ_DEMON
/*-------------------------------------------------------------------------*/
void
start_erq_demon (char *suffix)

/* Start the ERQ demon from the path 'BINDIR/erq.<suffix>' and setup
 * the pending_erq[] array.
 */

{
    struct svalue *erqp;
    char path[200];
    int sockets[2];
    int pid;
    char c;

    /* Create the freelist in pending_erq[] */
    erqp = pending_erq - 1;
    do {
        erqp[1].u.lvalue = erqp;
        erqp++;
        erqp->type = T_INVALID;
    } while (erqp < &pending_erq[MAX_PENDING_ERQ]);
    free_erq = erqp - 1;
    pending_erq[0].u.lvalue = 0;

    /* Create the sockets to talk to the ERQ */
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0)
    {
        perror("socketpair");
        return;
    }

    (void)signal(SIGCLD, SIG_IGN); /* don't create zombie processes */

    if ((pid = fork()) == 0)
    {
        /* Child */
        dup2(sockets[0], 0);
        dup2(sockets[0], 1);
        close(sockets[0]);
        close(sockets[1]);
        if (strlen(BINDIR) + 5 <= sizeof path)
        {
            sprintf(path, "%.95s/erq%.100s", BINDIR, suffix);
            execl((char *)path, "erq", "--forked", 0);
        }
        write(1, "0", 1);  /* indicate failure back to the driver */
        fprintf(stderr, "exec of erq demon failed.\n");
        _exit(1);
    }

    close(sockets[0]);
    if (pid == -1) {
        close(sockets[1]);
        return;
    }

    /* Read the first character from the ERQ. If it's '0', the ERQ
     * didn't start.
     */
    read(sockets[1], &c, 1);
    if (c == '0') {
        close(sockets[1]);
        return;
    }

    /* ERQ is up and running */
    erq_demon = sockets[1];
    set_socket_nonblocking(erq_demon);
    if (socket_number(erq_demon) >= min_nfds)
        min_nfds = socket_number(erq_demon)+1;
}

/*-------------------------------------------------------------------------*/
static void
stop_erq_demon (/* TODO: BOOL */ int notify)

/* Close the connection to the ERQ and inform all pending requests
 * about this. If <notify> is set, the hook H_ERQ_STOP is called.
 */

{
    struct svalue *erqp;
    int i;

    if (erq_demon < 0)
        return;

    socket_close(erq_demon);
    erq_demon = FLAG_NO_ERQ;
    erq_pending_len = 0;
    input_from_erq = &buf_from_erq[0];

    /* Inform all pending requests about the loss.
     */
    erqp = pending_erq;
    i = MAX_PENDING_ERQ;
    do {
        if (erqp->type == T_CLOSURE)
        {
            *++inter_sp = *erqp;
            erqp->type = T_INVALID;
            erqp->u.lvalue = free_erq;
            free_erq = erqp;
             CLEAR_EVAL_COST;
            apply_master_ob(STR_STALE_ERQ, 1);
        }
        erqp++;
    } while (--i);

    /* If desired, call H_ERQ_STOP to notify the situation.
     */
    if (notify)
    {
        CLEAR_EVAL_COST;
        if (closure_hook[H_ERQ_STOP].type == T_CLOSURE) {
            secure_call_lambda(&closure_hook[H_ERQ_STOP], 0);
        }
    }
} /* stop_erq_demon() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_attach_erq_demon (struct svalue *sp)

/* EFUN: attach_erq_demon()
 *
 *   int attach_erq_demon(object ob, int do_close)
 *   int attach_erq_demon(string name, int do_close)
 *
 * In the first form, take away the connection from <ob> and store it as
 * _the_ erq connection. <ob> thus becomes a normal non-interactive object.
 * In the second form, try to start the ERQ demon from the path
 * 'BINDIR/erq.<name>'. <name> must not contain '/..' sequences.
 *
 * If there is already an ERQ demon connected to the driver, the function
 * will fail unless <do_close> is set to 1 or any other odd integer; in
 * this case the connection to the old ERQ will be closed first.
 *
 * Return svalue.number 1 on success, 0 else.
 */

{
    struct object *ob;
    struct interactive *ip;
    char *suffix;

    /* Test for the first form: (object ob, int do_close) */
    if (sp[-1].type == T_OBJECT
     && NULL != (ip = O_GET_INTERACTIVE(ob=sp[-1].u.ob))
     && ip->sent.type == SENT_INTERACTIVE)
    {
        if (sp->type != T_NUMBER)
        {
            bad_xefun_arg(2, sp);
            /* NOTREACHED */
        }
        sp--;
        decr_object_ref(ob, "attach_erq_demon");
        sp->type = T_NUMBER;
        sp->u.number = 0;
        /* we need to read sp[1] below, thus don't overwrite it now. */
        if (privilege_violation4("attach_erq_demon",
            ob, 0, sp[1].u.number, sp+1))
        {
            if (erq_demon != FLAG_NO_ERQ) {
                if (sp[1].u.number & 1) {
                    stop_erq_demon(0);
                    erq_demon = FLAG_ERQ_STOP;
                } else {
                    return sp;
                }
            }
            erq_proto_demon = ip->socket;
            ip->do_close = FLAG_PROTO_ERQ;
            sp->u.number = 1;
        }
        return sp;
    }
    else

    /* Test for the second form: (string name, int do_close) */
    if (sp[-1].type == T_STRING
          && !strstr((suffix = sp[-1].u.string), "/.."))
    {
        int n;

        if (sp->type != T_NUMBER)
        {
            bad_xefun_arg(2, sp);
            /* NOTREACHED */
        }
        sp--;
        n = 0;
        if (privilege_violation4("attach_erq_demon",
            0, suffix, sp[1].u.number, sp+1))
        {
            if (erq_demon != FLAG_NO_ERQ)
            {
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
    }
    else
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }

    /* NOTREACHED */
    return NULL;
}

/*-------------------------------------------------------------------------*/
static /* TODO: BOOL */ int
send_erq (int handle, int request, char *arg, int arglen)

/* Send compose an ERQ message out of <handle>, <request> and <arg>
 * and send it to the ERQ. If all the data can't be sent now, the
 * next call to send_erq() will send the rest.
 *
 * Return FALSE if the data couldn't be sent, TRUE on success.
 *
 * SOCK_SEQPACKET is not portable enough, thus make special provisions
 * to deliver messages in an atomic fashion.
 */

{
    static char buf[ERQ_MAX_SEND], *pending;
    int wrote;

    if (erq_demon < 0)
        return MY_FALSE;

    /* Try to send the pending data */
    if (erq_pending_len) {
        wrote = socket_write(erq_demon, pending, erq_pending_len);
        if (wrote > 0) {
            pending += wrote;
            erq_pending_len -= wrote;
        }
        if (erq_pending_len)
            return MY_FALSE;
    }

    if (arglen + 9 > (int) sizeof buf)
        return MY_FALSE;

    /* Create the message and add it to buf[] */
    *(int32*)buf = htonl(erq_pending_len = arglen + 9);
    *(int32*)(buf+4) = htonl(handle);
    buf[8] = request;
    memcpy(buf + 9, arg, arglen);

    /* Send as much of buf[] as possible */
    pending = buf;
    wrote = socket_write(erq_demon, buf, erq_pending_len);
    if (wrote > 0) {
        pending += wrote;
        erq_pending_len -= wrote;
    }

    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_send_erq (struct svalue *sp)

/* EFUN: send_erq()
 *
 *   int send_erq(int request, string|int* data, closure callback)
 *
 * Send a request of type <request> and the data <data> to the ERQ>
 * If <callback> is set to a closure, it will be called with the
 * response from the ERQ.
 *
 * The function returns svalue.number 1 on success, and 0 on failure.
 *
 * The function causes a privilege violation "erq".
 */

{
    char  *arg;
    mp_int arglen;
    struct svalue *new_erq;
    int i;

    if (sp[-2].type != T_NUMBER)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }

    /* Set arg with the data to send. */

    if (sp[-1].type == T_STRING) {
        arg = sp[-1].u.string;
        arglen = strlen(arg);
    }
    else if (sp[-1].type == T_POINTER)
    {
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
    }
    else
    {
        bad_xefun_arg(2, sp);
        /* NOTREACHED */
    }

    /* Test if this call is allowed. */

    if (!privilege_violation4("erq", 0, "", sp[-2].u.number, sp)) {
        goto failure;
    }

    /* Store the callback closure. If none is given, use the
     * default callback.
     */

    if (sp->type == T_NUMBER && !sp->u.number) {
        new_erq = &pending_erq[MAX_PENDING_ERQ];
        new_erq->u.lvalue = free_erq;
    }
    else if (sp->type == T_CLOSURE
          && sp->x.closure_type != CLOSURE_UNBOUND_LAMBDA)
    {
        new_erq = free_erq;
    }
    else
    {
        bad_xefun_arg(3, sp);
        /* NOTREACHED */
    }

    /* Send the request and make up the result. */

    if (new_erq
     && 0 != (i = send_erq(new_erq - pending_erq, sp[-2].u.number, arg, arglen)) )
    {
        free_erq = new_erq->u.lvalue;
        *new_erq = *sp;
    }
    else
    {
failure:
        i = 0;
        free_svalue(sp);
    }

    free_svalue(--sp);
    (*--sp).u.number = i;

    return sp;
}

/*-------------------------------------------------------------------------*/
/* read a 32 bit value from (possibly unaligned)
 * network byte order representation
 */
static long
read_32 (char *str)

/* Read a 32 bit value from a possibly unaligned network byte order
 * representation.
 */

{
    unsigned char *p = (unsigned char *)str;

    return (long)p[0]<<24 | (long)p[1]<<16 | (long)p[2]<<8 | p[3];
}

/*-------------------------------------------------------------------------*/
static void
add_ip_entry (long addr, char *name)

/* Add a new IP address <addr>/hostname <name> pair to the cache iptable[].
 */

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

/*-------------------------------------------------------------------------*/
#else /* !ERQ_DEMON */

void start_erq_demon() {}  /* Just a dummy */

#endif /* ERQ_DEMON */

/* End of ERQ Support */
/*=========================================================================*/

#ifdef MALLOC_smalloc

/*-------------------------------------------------------------------------*/
void
clear_comm_refs (void)

/* GC support: Clear all refs the module might have.
 */

{
#ifdef ERQ_DEMON
    clear_ref_in_vector(
      pending_erq, sizeof pending_erq / sizeof (struct svalue)
    );
#endif /* ERQ_DEMON */
}

/*-------------------------------------------------------------------------*/
void
count_comm_refs (void)

/* GC support: count any ref the module has.
 */
{
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


/*=========================================================================*/

/*-------------------------------------------------------------------------*/
struct svalue *
query_ip_name (struct svalue *sp, /* TODO: BOOL */ int lookup)

/* Lookup the IP address (<lookup> is false) or IP hostname (<lookup> is
 * true) of object <sp> and return it. If <sp> is the number 0 or a
 * non-interactive object, the number 0 is returned.
 *
 * The hostname is read from the iptable[], so if it hasn't been
 * resolved yet, we return the number in any case.
 *
 * If <sp> is a reference to an interactive object, it will be replaced
 * on return with an array of integers with the full sockaddr_in:
 *    array[0.. 1]: sin_family
 *    array[2.. 3]: sin_port
 *    array[4.. 7]: sin_addr
 *    array[8..15]: undefined (ideally 0).
 *
 * The function is used to implement the efuns query_ip_number() and
 * query_ip_name().
 */

{
    struct object *ob;
    int i;
    struct interactive *ip;
    char *str;

    /* Set <ob> to the object passed on the stack. */

    if (sp->type != T_OBJECT)
    {
        struct svalue *svp;

        if (sp->type == T_NUMBER && !sp->u.number)
            return sp;
        svp = sp;
        while (svp->type == T_LVALUE || svp->type == T_PROTECTED_LVALUE)
            svp = svp->u.lvalue;
        if (svp->type != T_OBJECT)
        {
            bad_xefun_arg(1, sp);
            /* NOTREACHED */
        }
        ob = svp->u.ob;
    }
    else
    {
        ob = sp->u.ob;
        decr_object_ref(ob, "query_ip_name");
        sp->type = T_NUMBER;
    }

    /* Return 0 for non-interactive objects */
    if ( !(ip = O_GET_INTERACTIVE(ob)) || ip->sent.type != SENT_INTERACTIVE)
    {
        free_svalue(sp);
        sp->type = T_NUMBER;
        sp->u.number = 0;
        return sp;
    }

    /* If the object was passed as reference, replace it with an array
     * with the full sockaddr_in.
     */
    if (sp->type == T_LVALUE)
    {
        struct svalue array, *svp;
        struct vector *v;
        char *cp;

        v = allocate_array(sizeof ip->addr);
        if (v)
        {
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
        }
        else
        {
            transfer_svalue(sp, &const0);
        }
    }

    /* If the hostname is requested and we indeed have it in our table,
     * return it.
     */
    if (lookup)
    {
        for (i = 0; i < IPSIZE; i++)
        {
            if (iptable[i].addr == (long)(ip->addr.sin_addr.s_addr) && iptable[i].name)
            {
                sp->type = T_STRING;
                sp->x.string_type = STRING_SHARED;
                increment_string_ref(sp->u.string = iptable[i].name);
                return sp;
            }
        }
    }

    /* Return the IP address as string.
     */

    str = string_copy(inet_ntoa(ip->addr.sin_addr));
    if (!str)
    {
        inter_sp = sp - 1;
        error("Out of memory\n");
    }
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = str;
    return sp;
}

/*-------------------------------------------------------------------------*/
char *
query_host_name (void)

/* Return the hostname (and just the hostname, not the full domain name).
 * The result is a pointer to a static array!
 */

{
    static char name[20];
    char *p;

    gethostname(name, sizeof name);
    name[sizeof name - 1] = '\0';        /* Just to make sure */
    /* some platforms return the FQHN, but we don't want it. */
    p = strchr(name, '.');
    if (p)
        *p = '\0';
    return name;
}

/*-------------------------------------------------------------------------*/
char *
get_host_ip_number (void)

/* Return the IP address of the host.
 * The result is a newly allocated string.
 */

{
    char buf[20];

    sprintf(buf, "\"%s\"", inet_ntoa(host_ip_number));
    return string_copy(buf);
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_query_snoop (struct svalue *sp)

/* EFUN: query_snoop()
 *
 *   object query_snoop(object victim)
 *
 * Return the object which is snooping <victim>, or 0 if there is none.
 * The call must be allowed by master->valid_query_snoop().
 */

{
    struct svalue *arg1;
    struct object *ob;

    if (sp->type != T_OBJECT)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }

    /* Do some test and set ob to the snooper (if any) */
    switch (0) /* try {...} */
    {
      default:
        ob = sp->u.ob;
        if ((ob->flags & (O_DESTRUCTED|O_SHADOW)) != O_SHADOW
         || ob->sent->type != SENT_INTERACTIVE)
        {
            zero_object_svalue(sp);
            return sp;
        }
        inter_sp = sp;
        assert_master_ob_loaded();
        if (current_object != master_ob)
        {
            assign_eval_cost();
            arg1 = apply_master_ob(STR_VALID_QSNOOP, 1);
            if (arg1 == 0 || arg1->type != T_NUMBER || !arg1->u.number)
            {
                if (out_of_memory)
                {
                    error("Out of memory\n");
                }
                ob = NULL;
                break;
            }
        }
        else
        {
            decr_object_ref(ob, "query_snoop");
        }
        ob = O_GET_INTERACTIVE(ob)->snoop_by;
    }

    /* Return the result */
    if (ob)
    {
        add_ref(ob, "query_snoop");
        sp->type = T_OBJECT;
        sp->u.ob = ob;
    }
    else
    {
        sp->type = T_NUMBER;
        sp->u.number = 0;
    }
    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_query_idle (struct svalue *sp)


/* EFUN: query_idle()
 *
 *   int query_idle(object ob)
 *
 * Return how many seconds a user object has been idle.
 */

{
    int i;
    struct object *ob;

    if (sp->type != T_OBJECT)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }

    ob = sp->u.ob;
    if (!ob->sent || ob->sent->type != SENT_INTERACTIVE)
    {
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

/*-------------------------------------------------------------------------*/
struct svalue *
f_remove_interactive (struct svalue *sp)

/* EFUN: remove_interactive()
 *
 *   void remove_interactive(object ob)
 *
 * Close the connection to the interactive object ob.
 *
 * In fact, the connection is only flushed and marked for closing,
 * as a remove_interactive() here can upset some other code.
 * The actual remove will be done by get_message().
 */

{
    struct interactive *victim;

    if (sp->type != T_OBJECT)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }

    if (NULL != (victim = O_GET_INTERACTIVE(sp->u.ob))
     && victim->sent.type == SENT_INTERACTIVE
     && !victim->closing
     && !victim->do_close)
    {
        if (victim->message_length) {
            command_giver = victim->ob;
            add_message(message_flush);

            /* message_flush takes always directly effect on the
             * socket. No apply() is involved.
             */
        }
        victim->do_close = FLAG_DO_CLOSE;
    }
    free_svalue(sp);
    return sp - 1;
}

/*-------------------------------------------------------------------------*/
void
notify_no_command (char *command)

/* No action could be found for <command>, thus print a failure notice
 * to the command_giver.
 *
 * Called by the command parser, this function evaluates the H_NOTIFY_FAIL
 * hook to do its job. If the hook is not set, the default_err_message
 * is printed.
 */

{
    struct svalue *svp;
    struct interactive *ip;

    if (!(ip = O_GET_INTERACTIVE(command_giver))
     || ip->sent.type != SENT_INTERACTIVE)
    {
        return;
    }

    svp = &ip->default_err_message;
    if (svp->type == T_STRING)
    {
        add_message("%s", svp->u.string);
        free_svalue(svp);
        svp->type = T_INVALID;
    }
    else if (svp->type == T_CLOSURE)
    {
        call_lambda(svp, 0);
        /* add_message might cause an error, thus, we free the closure first. */
        free_svalue(svp);
        svp->type = T_INVALID;
        if (inter_sp->type == T_STRING)
            add_message("%s", inter_sp->u.string);
        pop_stack();
    }
    else if (closure_hook[H_NOTIFY_FAIL].type == T_STRING)
    {
        add_message("%s", closure_hook[H_NOTIFY_FAIL].u.string);
    }
    else if (closure_hook[H_NOTIFY_FAIL].type == T_CLOSURE)
    {
        if (closure_hook[H_NOTIFY_FAIL].x.closure_type == CLOSURE_LAMBDA)
            closure_hook[H_NOTIFY_FAIL].u.lambda->ob = command_giver;
        push_volatile_string(command);
        call_lambda(&closure_hook[H_NOTIFY_FAIL], 1);
        if (inter_sp->type == T_STRING)
            add_message("%s", inter_sp->u.string);
        pop_stack();
    }
}

/*-------------------------------------------------------------------------*/
void
clear_notify (void)

/* Clear the default error message for command giver.
 */

{
    struct interactive *ip;

    if (!(ip = O_GET_INTERACTIVE(command_giver))
     || ip->sent.type != SENT_INTERACTIVE)
    {
        return;
    }

    if (ip->default_err_message.type != T_INVALID)
    {
        free_svalue(&ip->default_err_message);
        ip->default_err_message.type = T_INVALID;
    }
}

/*-------------------------------------------------------------------------*/
void
set_notify_fail_message (struct svalue *svp)

/* Set the default notify_fail message of command_giver to <svp>.
 */

{
    struct interactive *ip;

    if (!command_giver
     || !(ip = O_GET_INTERACTIVE(command_giver))
     || ip->sent.type != SENT_INTERACTIVE
     || ip->closing)
    {
        free_svalue(svp);
        return;
    }
    transfer_svalue(&ip->default_err_message, svp);
}

/*-------------------------------------------------------------------------*/
void
free_notifys (void)

/* Remove all default notify_fail messages.
 * This function is called from the garbage collector.
 */

{
    int i;
    struct svalue *svp;

    for (i = MAX_PLAYERS; --i >= 0; )
    {
        if (!all_players[i]) continue;
        svp = &all_players[i]->default_err_message;
        free_svalue(svp);
        svp->type = T_INVALID;
    }
}

/*-------------------------------------------------------------------------*/
int
replace_interactive (struct object *ob, struct object * obfrom, char *name)

/* EFUN: exec()
 *
 * Switch the network connection from <obfrom> to <ob>. If <ob> is already
 * interactive, its connection will be switched to <obfrom>. The efun is
 * called from the program <name>.
 *
 * If <obfrom> was command_giver, <ob> will be the new command_giver.
 *
 * The call is validated by master->valid_exec() and has to return 0 on
 * failure, and 1 on success.
 */

{
    struct svalue *v;
    struct interactive *stale_interactive, *ip;
    struct object *save_command;
    struct shadow_sentence *save_shadow;

    /* Ask the master if this exec() is ok. */
    push_constant_string(name);
    push_object(ob);
    push_object(obfrom);
    v = apply_master_ob(STR_VALID_EXEC, 3);
    if (!v || v->type != T_NUMBER || v->u.number == 0)
        return 0;

    if (NULL != (stale_interactive = O_GET_INTERACTIVE(ob))
     && stale_interactive->sent.type != SENT_INTERACTIVE)
    {
        stale_interactive = NULL;
    }
    if (!(ip = O_GET_INTERACTIVE(obfrom))
     || ip->sent.type != SENT_INTERACTIVE)
        error("Bad argument 2 to exec()\n");

    /* When we have to have an out of memory error, have it before pointers
     * get changed.
     */
    save_shadow = (struct shadow_sentence *)alloc_sentence();
    save_command = command_giver;

    /* If <ob> has a connection, flush it */
    if (stale_interactive)
    {
        prompt_from_ed_buffer(stale_interactive);
        if (stale_interactive->message_length)
        {
            command_giver = ob;
            add_message(message_flush);
        }
    }

    /* Flush the connection of <obfrom> */

    prompt_from_ed_buffer(ip);
    if (ip->message_length) {
        command_giver = obfrom;
        add_message(message_flush);
    }
    command_giver = save_command;

    /* Switch a possible snooper */

    if (ip->snoop_on)
    {
        ip->snoop_on->snoop_by = ob;
    }

    /* The usual shadow_sentence juggling. */

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
    }
    else
    {
        ip->sent.shadowed_by = NULL;
        ip->sent.shadowing = NULL;
        save_shadow->ed_buffer = NULL;
        ip->sent.next = ob->sent;
    }
    ob->sent = (struct sentence *)ip;

    ip->ob = ob;
    ip->catch_tell_activ = MY_TRUE;

    if (stale_interactive)
    {
        /* Tie <ob>s stale connection to <obfrom>. */

        stale_interactive->sent = *save_shadow;
        free_shadow_sent(save_shadow);
        obfrom->sent = (struct sentence *)stale_interactive;
        if (stale_interactive->snoop_on)
        {
            stale_interactive->snoop_on->snoop_by = obfrom;
        }
        stale_interactive->ob = obfrom;
        stale_interactive->catch_tell_activ = MY_TRUE;
        prompt_to_ed_buffer(stale_interactive);
    }
    else
    {
        /* Clean up <obfrom> after the loss of connection */

        add_ref(ob, "exec");
        free_object(obfrom, "exec");
        ob->flags |= O_ONCE_INTERACTIVE|O_SHADOW;
        obfrom->flags &= ~O_ONCE_INTERACTIVE;
        if (save_shadow->shadowed_by || save_shadow->shadowing
         || save_shadow->ed_buffer)
        {
            save_shadow->type = SENT_SHADOW;
            obfrom->sent = (struct sentence *)save_shadow;
        }
        else
        {
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

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

void
count_comm_extra_refs (void)

/* Count all the refs to verify the normal refcounting. */

{
    int i;

#ifdef ERQ_DEMON
    count_extra_ref_in_vector(
      pending_erq, sizeof pending_erq / sizeof (struct svalue)
    );
#endif /* ERQ_DEMON */

    for (i = 0; i < MAX_PLAYERS; i++)
    {
        struct object *ob;
        struct input_to *it;

        if (all_players[i] == 0)
            continue;
        all_players[i]->ob->extra_ref++;
        if ( NULL != (ob = all_players[i]->snoop_by) ) {
            struct interactive *ip;

            if (!(ip = O_GET_INTERACTIVE(current_object)) ||
                ip->sent.type != SENT_INTERACTIVE)
            {
                /* snooping monster */
                ob->extra_ref++;
            }
        } /* end of snoop-processing */

        if ( NULL != (it = all_players[i]->input_to) ) {
            it->ob->extra_ref++;
            count_extra_ref_in_vector(it->arg, it->num_arg);
        }
        if ( NULL != (ob = all_players[i]->modify_command) )
            count_extra_ref_in_object(ob);
        count_extra_ref_in_vector(&all_players[i]->prompt, 1);
        count_extra_ref_in_vector(&all_players[i]->default_err_message, 1);
    }
}

#endif /* DEBUG */

/*-------------------------------------------------------------------------*/
#ifdef UDP_SEND

struct svalue *
f_send_imp (struct svalue *sp)

/* EFUN: send_imp()
 *
 *   int send_imp(string host, int port, string message)
 *   int send_imp(string host, int port, int * message)
 *
 * Sends The message in an UDP packet to the given host and port
 * number. Causes a privilege violation.
 * The message can be given either as string, or as array of
 * bytes. The latter variant allows to send binary data as well.
 * Returns 1 on success, 0 on failure.
 */

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

    switch(0) { default: /* try {...} */

        /* Set msg/msglen to the data of the message to send */

        if (sp->type == T_STRING)
        {
            msg = sp->u.string;
            msglen = strlen(msg);
        }
        else if (sp->type == T_POINTER)
        {
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
        }
        else
        {
            bad_xefun_arg(3, sp);
            /* NOTREACHED */
        }

        /* Is this call valid? */

        if (_privilege_violation("send_imp", sp-2, sp) <= 0)
            break;
        if (udp_s < 0)
            break;

        /* Determine the destination address */

        to_host = (sp-2)->u.string;
        to_port = (sp-1)->u.number;

        if (sscanf(to_host, "%d.%d.%d.%d", &ip1, &ip2, &ip3, &ip4) == 4)
        {
            name.sin_addr.s_addr = inet_addr(to_host);
            name.sin_family = AF_INET;
        }
        else
        {
            hp = gethostbyname(to_host);
            if (hp == 0)
                break;
            memcpy(&name.sin_addr, hp->h_addr, hp->h_length);
            name.sin_family = AF_INET;
        }
        name.sin_port = htons(to_port);

        /* Send the message. */
#ifndef SENDTO_BROKEN
        if (sendto(udp_s, msg, msglen, 0,
               (struct sockaddr *)&name, sizeof(name)) != msglen)
#endif
            break;
        ret = 1;
    }

    /* Return the result */

    free_svalue(sp);
    free_svalue(--sp);
    free_svalue(--sp);
    sp->type = T_NUMBER;
    sp->u.number = ret;
    return sp;
}
#endif /* UDP_SEND */

/*-------------------------------------------------------------------------*/
struct svalue *
f_set_buffer_size (struct svalue *sp)

/* EFUN: set_buffer_size()
 *
 *   int set_buffer_size(int size)
 *
 * Changes the socket buffer size for this_interactive() to size,
 * up to a preconfigured maximum, result is the old buffer size
 * (or -1 on systems which aren't able to change the socket
 * buffer).
 * Modifying the buffer size may result in a better IO
 * throughput, but can also worsen it.
 */

{
    int new;

    /* Get the desired buffer size */

    if (sp->type != T_NUMBER || sp->u.number > SET_BUFFER_SIZE_MAX)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }
    new = sp->u.number;

    sp->u.number = -1; /* Default result */

#ifdef SO_SNDBUF
    {
        int old, optlen;
        struct interactive *ip;

        if (!(ip = O_GET_INTERACTIVE(current_object))
         || ip->sent.type != SENT_INTERACTIVE
         || ip->do_close)
        {
            return sp;
        }


        optlen = sizeof old;
        if (getsockopt(ip->socket, SOL_SOCKET, SO_SNDBUF, (char *)&old, &optlen) < 0)
            return sp;
        if (setsockopt(ip->socket, SOL_SOCKET, SO_SNDBUF, (char *)&new, sizeof new) < 0)
            return sp;
        sp->u.number = old;
    }
#endif /* SO_SNDBUF */

    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_binary_message (struct svalue *sp)

/* EFUN: binary_message()
 *
 *   int binary_message(int *|string message, int flags)
 *
 * Flush output and send output directly with write WITHOUT IAC QUOTING.
 * The message may contain zeroes if given as int *.
 * The messages goes to this_object(), but only if interactive.
 * return value: number of characters actually written.
 * Any 'allowed charset' setting is ignored.
 *
 * Flag settings are interpreted bitwise and may be ored
 * together:
 *
 *   Bit 0 (value 1): when set, add_message() is used instead of
 *     write(). Thus no previous flushing of the buffer is
 *     needed, but the output is not immediate, nor can the
 *     number of bytes actually sent be determined - the return
 *     value is undefined.
 *   Bit 1 (value 2): The buffer is flushed _after_ adding the
 *     message. Useful only in conjunction with Bit 0.
 *
 * The idea behind the flag settings is that sending command
 * codes for colours and other things needs to bypass the allowed
 * charset filters, but isn't important enough to waste bandwith
 * on a synchronous trasmission.
 */

{
    char *message, *p;
    mp_int size, wrote = 0, i;
    struct svalue *svp;
    struct interactive *ip;
    struct object *save_command_giver;

    /* Set message to the data to be sent, and size to its length. */

    if (sp[-1].type == T_POINTER)
    {
        size = VEC_SIZE(sp[-1].u.vec);
        message = alloca(size + 1);
        if (!message)
            fatal("Stack overflow in binary_message()");
        for (i = size, svp = sp[-1].u.vec->item, p = message; --i >= 0; svp++)
        {
            if (svp->type != T_NUMBER)
            {
                bad_xefun_arg(1, sp);
                /* NOTREACHED */
            }
            *p++ = svp->u.number;
        }
        *p = '\0';
    }
    else if (sp[-1].type == T_STRING)
    {
        message = sp[-1].u.string;
        size = strlen(message);
    }
    else
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }

    if (sp->type != T_NUMBER)
    {
        bad_xefun_arg(2, sp);
        /* NOTREACHED */
    }

    /* Send the message */

    i = 0;
    if (NULL != (ip = O_GET_INTERACTIVE(current_object))
     && ip->sent.type == SENT_INTERACTIVE
     && !ip->do_close)
    {
        save_command_giver = command_giver;
        command_giver = current_object;

        if (sp->u.number & 1)
        {
            /* Write before flush... */

            sending_telnet_command = MY_TRUE; /* turn of IAC quoting */

            /* Loop until the whole message is sent.
             * The loop is necessary because the message may contain
             * embedded '\0' which the normal add_message() doesn't
             * add.
             */
            while (size)
            {
                if (*message) {
                    add_message("%s", message);
                    if (ip->do_close)
                        break;
                    size -= wrote = strlen(message);
                    message += wrote;
                }
                else
                {
                    /* Possibly an embedded '\0'. Fake the basics
                     * of add_message()
                     */

                    if (ip->message_length >= MAX_SOCKET_PACKET_SIZE)
                    {
                        add_message(message_flush);
                        if (ip->do_close)
                            break;
                    }
                    if (!ip->message_length )
                    {
                        if ( NULL != (ip->next_player_for_flush = first_player_for_flush) )
                        {
                            O_GET_INTERACTIVE(first_player_for_flush)->
                              previous_player_for_flush =
                                command_giver;
                        }
                        ip->previous_player_for_flush = NULL;
                        first_player_for_flush = command_giver;
                    }
                    ip->message_buf[ip->message_length++] = '\0';
                    size--;
                }
            } /* while(size) */
            sending_telnet_command = MY_FALSE;

            if (sp->u.number & 2)
                add_message(message_flush);
            wrote = 0;

        }
        else
        {
            /* Flush, then write. */
            add_message(message_flush);

            /* Since all pending data was flushed, we can write directly
             * to the socket now.
             */

            for (i = 6;;) {
                wrote = socket_write(ip->socket, message, size);
                if (wrote != -1)
                    break;
                switch(errno)
                {
                  case EINTR:
                    if (--i)
                        continue;
                    fprintf(stderr, "comm: write EINTR. Message discarded.\n");
                    break;
                  case EWOULDBLOCK:
                    fprintf(stderr,
                      "comm: write EWOULDBLOCK. Message discarded.\n");
                    size = 0;
                    break;
                  case EMSGSIZE:
                    fprintf(stderr, "comm: write EMSGSIZE.\n");
                    break;
                  default:
                    perror("write");
                    ip->do_close = FLAG_DO_CLOSE;
                    break;
                }
                break;
            } /* end for on retry count */
        } /* if (type of write) */

        command_giver = save_command_giver;
    } /* end if interactive */

    sp--;
    free_svalue(sp);
    sp->type = T_NUMBER;
    sp->u.number = wrote;
    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_set_connection_charset (struct svalue *sp)

/* EFUN: set_connection_charset()
 *
 *   void set_connection_charset(int* bitvector, int quote_iac)
 *
 * Set the set of characters that can be output to the
 * interactive user (this does not apply to binary_message() ) .
 * bitvector is interpreted as an array of 8-bit-values and might
 * contain up to 32 elements. Character n is allowed to be output
 * if sizeof(bitvector) > n/8 && bitvector[n/8] & (1 << n%8) .
 * This affects the interactive struct of this_object(), if
 * present.
 * If quote_iac is 0 and char 255 is allowed to be output, IAC
 * will be output unmodified.
 * If quote_iac is 1 and char 255 is allowed to be output,
 * char 255 will be quoted so that it is not interpreted as IAC
 * by the telnet protocol.
 */

{
    int i;
    struct svalue *svp;
    char *p;
    struct interactive *ip;

    if (sp[-1].type != T_POINTER || (i = VEC_SIZE(sp[-1].u.vec)) > 32)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
    }
    if (sp->type != T_NUMBER)
    {
        bad_xefun_arg(2, sp);
        /* NOTREACHED */
    }

    if (NULL != (ip = O_GET_INTERACTIVE(current_object))
     && ip->sent.type == SENT_INTERACTIVE)
    {
        for (svp = sp[-1].u.vec->item, p = ip->charset; --i >= 0; svp++, p++) {
            if (svp->type == T_NUMBER)
                *p = svp->u.number;
        }
        memset(p, 0, &ip->charset[sizeof ip->charset] - p);
        ip->charset['\n'/8] &= ~(1 << '\n' % 8);
        ip->charset['\0'/8] &= ~(1 << '\0' % 8);
        if ( 0 != (ip->quote_iac = sp->u.number) )
        {
            if (ip->charset[IAC/8] & (1 << IAC % 8))
                ip->charset[IAC/8] &= ~(1 << IAC % 8);
            else
                ip->quote_iac = MY_FALSE;
        }
    }
    sp--;
    free_svalue(sp);
    sp--;
    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
input_to (struct svalue *sp, int num_arg)

/* EFUN: input_to()
 *
 * Set up a function in the current object to be called with the
 * next user input.
 *
 * The function can do a lot - I won't repeat the external
 * man page here :-)
 */

{
    struct svalue *arg;  /* Pointer to the arguments of the efun */
    struct svalue *dest;
    int flags;           /* The flags passed to input_to() */
    struct input_to *it;
    int extra;           /* Number of extra arguments */

    arg = sp - num_arg + 1;
    if (arg[0].type != T_STRING)
    {
        bad_efun_vararg(1, sp);
        /* NOTREACHED */
    }

    /* Extract the arguments */

    flags = 0;
    extra = 0;
    if (num_arg > 1)
    {
        if (arg[1].type != T_NUMBER)
        {
            bad_efun_vararg(2, sp);
            /* NOTREACHED */
        }
        flags = arg[1].u.number & (NOECHO_REQ|CHARMODE_REQ|IGNORE_BANG);
        extra = num_arg - 2;
    }

    /* Allocate and setup the input_to structure */

    it = (struct input_to *)xalloc(
        sizeof *it - sizeof *sp + sizeof *sp * extra );
    if (!it)
        fatal("Out of memory.");

    it->function = make_shared_string(arg[0].u.string);
    free_string_svalue(arg);
    it->num_arg = extra;
    it->ob = current_object;
    add_ref(current_object, "input_to");

    sp = arg;
    arg += 2;
    dest = it->arg;
    while (--extra >= 0)
    {
        if (arg->type == T_LVALUE)
        {
            int error_index = arg - sp + 1;
            do {
                free_svalue(arg++);
                (dest++)->type = T_INVALID;
            } while (--extra >= 0);
            free_input_to(it);
            bad_efun_vararg(error_index, sp - 1);
            /* NOTREACHED */
        }
        transfer_svalue_no_free(dest++, arg++);
    }

    /* If the master agrees (only in case of IGNORE_BANG) the
     * the input_to could be set - return 1.
     */

    sp->type = T_NUMBER;
    if (!(flags & IGNORE_BANG)
     || privilege_violation4("input_to", command_giver, 0, flags, sp))
    {
        if (set_call(command_giver, it, flags)) {
                sp->u.number = 1;
            return sp;
        }
    }

    /* input_to() was not allowed - return 0. */

    free_input_to(it);
    sp->u.number = 0;
    return sp;
}

/*-------------------------------------------------------------------------*/
static void
free_input_to (struct input_to *it)

/* Deallocate the input_to structure <it> and all referenced memory.
 */

{
    struct svalue *arg;
    int i;

    free_object(it->ob, "free_input_to");
    free_string(it->function);
    for (arg = it->arg, i = it->num_arg; --i >= 0; ) {
        free_svalue(arg++);
    }
    xfree((char *)it);
}

/*-------------------------------------------------------------------------*/
#ifdef MAXNUMPORTS

struct svalue *
query_ip_port (struct svalue *sp)

/* EFUN: query_mud_port()
 *
 * Returns the port number the parser uses for user connections.
 *
 *   int query_mud_port(void)
 *
 * If no argument is given, the port for this_player() is
 * returned. If this_player() is not existing or not interactive,
 * the first port number open for connections is returned.
 *
 *   int query_mud_port(object user)
 *   int query_mud_port(int num)
 *
 * If an user object is given, the port used for its connection
 * is returned.
 * If a positive number is given, the <num>th port number the
 * parser uses for connections is returned (given that there are
 * that many ports).
 * If -1 is given, the number of ports open for connections is
 * returned.
 */

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
        {
            bad_xefun_arg(1, sp);
            /* NOTREACHED */
        }
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

    getsockname(ip->socket, (struct sockaddr *)&addr, &length);
    sp->type = T_NUMBER;
    sp->u.number = ntohs(addr.sin_port);
    return sp;
}
#endif

/***************************************************************************/

