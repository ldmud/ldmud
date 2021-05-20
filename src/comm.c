/*---------------------------------------------------------------------------
 * Gamedriver Communications module.
 *
 *---------------------------------------------------------------------------
 * Throughout the module the fact is used that valid socket numbers
 * are always >= 0. Unused sockets are therefore marked with negative
 * numbers.
 *
 * All information needed for an interactive object are stored in
 * a 'interactive_t'. This struct is linked to by the shadow sentence
 * of the interactive object.
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
 * possible to do all telnet negotiations on mudlib level, but it must
 * be either the driver or the mudlib, not both.
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
 * comm_time_to_call_heart_beat and comm_return_to_backend. The loop checks
 * the later variable every second and, if it is set, aborts its input loop
 * and returns to the backend. To mark the cause of the return, the variable
 * time_to_call_heart_beat is set before return when
 * comm_time_to_call_heart_beat was set as well.
 *
 * TODO: The noecho/charmode logic, especially in combination with
 * TODO:: the telnet machine is frustratingly underdocumented.
 *
 * The data that can not written directly to the sockets is put instead into
 * an intermediate buffer, from which the backend loop will continue writing
 * when possible. The buffers are stored in a linked list in the interactive_s
 * structure.
 *
 * TODO: Fiona says: The telnet code is frustrating. It would be better if
 * TODO:: the common handling of e.g. TELNET_NAWS is offered by hooks,
 * TODO:: as the Anarres version of MudOS does. This would mean a rewrite.
 *---------------------------------------------------------------------------
 */

#define SAVE_NOECHO       /* TODO: Define to enable safe NOECHO mode */
#define SIMULATE_CHARMODE /* TODO: Even linemode clients stay in charmode */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"

#include <assert.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#include <stdarg.h>
#include <stddef.h>
#include <sys/ioctl.h>
#include <poll.h>

#define TELOPTS
#include "../mudlib/sys/telnet.h"

#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>   /* inet_ functions / structs */
#endif
#ifdef HAVE_ARPA_NAMESER_H
#  include <arpa/nameser.h> /* DNS HEADER struct */
#endif
#ifdef HAVE_NETDB_H
#  include <netdb.h>
#endif
#include <resolv.h>

#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif

#if defined(_AIX)
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
#include "actions.h"
#include "array.h"
#include "closure.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "pkg-mccp.h"
#include "pkg-pgsql.h"
#include "pkg-python.h"
#ifdef USE_TLS
#include "pkg-tls.h"
#endif
#include "sent.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "swap.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-eval_cost.h"

#include "../mudlib/sys/comm.h"
#include "../mudlib/sys/configuration.h"
#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/input_to.h"
#include "../mudlib/sys/interactive_info.h"

/* if driver is compiled for ERQ demon then include the necessary file
 */
#ifdef ERQ_DEMON
#    include <sys/wait.h>
#    ifdef ERQ_INCLUDE
#        include ERQ_INCLUDE
#    else
#        include "util/erq/erq.h"
#    endif
#endif

/* When no special networking code is needed, define the
 * socket function to their normal Unix names.
 */
#if !defined (SOCKET_LIB) && !defined(SOCKET_INC)
#    define socket_number(s) (s)
#    define socket_ioctl  ioctl
#    ifndef hpux
#        define socket_select select
#    else
#        define socket_select(n,r,w,x,t) select(n, (int *)r, (int *)w, (int *)x, t)
         /* Silences the compiler */
#    endif
#    define socket_read   read
#    define socket_write  write
#    define socket_close  close
#endif /* SOCKET_LIB */

#if defined(_AIX)
typedef unsigned long length_t;
#elif defined(__INTEL_COMPILER) || defined (__GNUC__)
typedef socklen_t length_t;
#else
typedef int length_t;
#endif

#if defined(CYGWIN)
extern int socketpair(int, int, int, int[2]);
#endif

#ifndef EPROTO
#    define EPROTO EINTR
#endif

#ifndef MAXHOSTNAMELEN
#    define MAXHOSTNAMELEN 64
#endif

#ifndef INET_ADDRSTRLEN
#    define INET_ADDRSTRLEN 16
#endif

/* Amazing how complicated networking can be, hm? */

/*-------------------------------------------------------------------------*/
/* Types */

/* --- struct input_to_s: input_to() datastructure
 *
 * input-to structures describe a pending input_to() for a given
 * interactive object. It is a specialization of input_s,
 * which therefore must be the first element.
 */
typedef struct input_to_s input_to_t;

struct input_to_s {
    input_t     input;
    callback_t  fun;        /* The function to call, and its args */
    p_uint      eval_nr;    /* The thread number where this started. */
};

/*-------------------------------------------------------------------------*/
/* Variables */

interactive_t *all_players[MAX_PLAYERS];
  /* Pointers to the structures of the interactive users.
   * Unused entries are NULL.
   * TODO: A list would be nicer
   */

static int max_player = -1;
  /* Index of the last used entry in all_players[]. */

int num_player = 0;
  /* The current number of active users */

p_int write_buffer_max_size = WRITE_BUFFER_MAX_SIZE;
  /* Amount of data held pending in the write fifo queue.
   *   0: No queue.
   *  -1: Infinite queue.
   */

char default_player_encoding[sizeof(((interactive_t*)0)->encoding)] = "ISO-8859-1//TRANSLIT";
  /* Default encoding for interactives. */

static char udp_buf[65536];
  /* Buffer for incoming UDP datagrams in get_message(). 
   * If it is too small, the rest of the to be received datagram will be
   * discarded. As UDP datagrams get be as large as (65536 - UDP-Header -
   * IP-Header) we will just play safe here.
   * static (file-scope) buffer, because we don't want to allocate such a
   * large buffer on the stack or by xalloc (it is used often, we're not
   * multi-threaded nor is get_message() re-entrant).
   * Note for IPv6: IPv6 supports so-called jumbograms with payloads up to
   * 2^32-1 octets in length (rfc2675). Obviously, we don't support that. ;-)
   */

#ifdef COMM_STAT

/* The statistics were originally introduced to measure the efficiency
 * of the message buffering in comparison to the unbuffered sending of
 * data. Nowadays, it's just interesting to know how much bandwidth you
 * use.
 */

statcounter_t add_message_calls = 0;
  /* Number of calls to add_message() */

statcounter_t inet_packets = 0;
  /* Number packets sent to the users */

statcounter_t inet_volume = 0;
  /* Amount of data sent to the users */

statcounter_t inet_packets_in = 0;
  /* Number packets received from the users */

statcounter_t inet_volume_in = 0;
  /* Amount of data received from the users */

#endif

/*-------------------------------------------------------------------------*/

#ifdef ERQ_DEMON

#define MAX_PENDING_ERQ  32  /* Max number of pending requests */
#define MAX_ERQ_PIDS      5  /* Maximum number of ERQ processes to wait. */

#define FLAG_NO_ERQ   -2    /* No ERQ connected */
#define FLAG_ERQ_STOP -1    /* Severing connection */


static pid_t erq_pids[MAX_ERQ_PIDS];
  /* PIDs of child processes we started. To prevent zombie processes we
   * have to wait() for them after they terminated. And because there may
   * be some overlap between starting a new one and killing the old one
   * we remember more than one PID.
   */

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

static unsigned long erq_pending_len = 0;
  /* erq_pending_len is used by send_erq(), but needs
   * to be cleared by stop_erq_demon().
   */

typedef struct erq_callback_s {
    svalue_t fun;
    Bool     string_arg;
} erq_callback_t;

static erq_callback_t pending_erq[MAX_PENDING_ERQ+1];
  /* ERQ callback handles. The last one is reserved for callback-free
   * requests.
   * .fun is the callback closure, .string_arg is TRUE if the closure
   * takes its data as a string instead of an array as argument.
   *
   * The free entries are organised in a singly linked list of
   * T_INVALID .fun svalues, using the .fun.u.generic to point to the next
   * free entry.
   */

static erq_callback_t *free_erq;
  /* The first free entry in the freelist in pending_erq[] */

/* The size of the IPTABLE depends on the number of users,
 * and is at least 200.
 */
#if MAX_PLAYERS > 700
#    define IPSIZE MAX_PLAYERS
#else
#    if MAX_PLAYERS > 100
#        define IPSIZE (MAX_PLAYERS*2)
#    else
#        define IPSIZE 200
#    endif
#endif

static struct ipentry {
    struct in_addr  addr;  /* The address (only .s_addr is significant) */
    string_t       *name;  /* tabled string with the hostname for <addr> */
} iptable[IPSIZE];
  /* Cache of known names for given IP addresses.
   * It is used as a ringbuffer, indexed by ipcur.
   * TODO: Instead of a simple circular buffer, the lookup should be
   * TODO:: hashed over the IP address. Worst case would still be O(IPSIZE),
   * TODO:: but best case would be O(1).
   * TODO: Allow unlimited numbers of entries, but give them a max lifetime
   * TODO:: of a day. After that, even existing entries need to be
   * TODO:: re-resolved. Maybe an additional size limit. (suggested by Coogan)
   */

static int ipcur = 0;
  /* Index of the next entry to use in the iptable[].
   */

#endif /* ERQ_DEMON */

/*-------------------------------------------------------------------------*/

/* --- Communication sockets --- */

static SOCKET_T sos[MAXNUMPORTS];
  /* The login sockets.
   */

static SOCKET_T udp_s = -1;
  /* The UDP socket */

/* --- Networking information --- */

static char host_name[MAXHOSTNAMELEN+1];
  /* This computer's hostname, used for query_host_name() efun.
   */

static struct in_addr host_ip_number;
  /* This computer's numeric IP address only, used for
   * the query_host_ip_number() efun.
   */

static struct sockaddr_in host_ip_addr_template;
  /* The template address of this computer. It is copied locally
   * and augmented with varying port numbers to open the driver's ports.
   */

char * domain_name = NULL;
  /* This computer's domain name, as needed by lex.c::get_domainname().
   */

static int min_nfds = 0;
  /* The number of fds used by the driver's sockets (udp, erq, login).
   * It is the number of the highest fd plus one.
   */

/* --- Telnet handling --- */

static void (*telopts_do  [NTELOPTS])(int);
static void (*telopts_dont[NTELOPTS])(int);
static void (*telopts_will[NTELOPTS])(int);
static void (*telopts_wont[NTELOPTS])(int);

  /* Tables with the telnet statemachine handlers.
   */

enum telnet_states {
  TS_DATA = 0,      /* Initial state, we expect regular data or the begin of a telnet sequence. */
  TS_IAC,           /* Received an IAC, expect telnet command or another IAC. */
  TS_WILL,          /* Received IAC WILL, expect telnet option code. */
  TS_WONT,          /* Received IAC WONT, expect telnet option code. */
  TS_DO,            /* Received IAC DO, expect telnet option code. */
  TS_DONT,          /* Received IAC DONT, expect telnet option code. */
  TS_SB,            /* Received IAC SB and any number of bytes != IAC for the subnegotiation. */
  TS_SB_IAC,        /* Received IAC SB <contents> IAC. */
  TS_READY,         /* Received a full line, that is now in the .command[] buffer (without the newline). */
  TS_CHAR_READY,    /* Received at least one character in charmode, no line endings, yet. */
  TS_SYNCH,         /* Received SIGURG (out-of-band data) on the TCP channel and shall discard
                       all data until an IAC DM is received. */
  TS_INVALID
};

  /* Telnet states
   */

static inline int TN_START_VALID(int x)
{
    return x == TS_SB || x == TS_SB_IAC;
}


/* --- Misc --- */

static volatile Bool urgent_data = MY_FALSE;
  /* Flag set when a SIGURG/SIGIO announces the arrival of
   * OOB data.
   */

static volatile mp_int urgent_data_time;
  /* The backend::current_time when urgent_data was set last.
   */

static interactive_t *first_player_for_flush = NULL;
  /* First interactive user object to flush. Marks the head
   * of the list formed by interactive.{next,previous}_player_for_flush
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

/* Outgoing connections in-progress */

typedef enum {
      ocNotUsed    /* Entry not used */
    , ocUsed       /* Entry holds pending connection */
    , ocLoggingOn  /* Entry is doing the LPC logon protocol.
                    * This value should not appear outside of
                    * check_for_out_connections(), if it does, it
                    * means that LPC logon threw an error.
                    */
} OutConnStatus;

struct OutConn {
    struct sockaddr_in   target;   /* Address connected to (allocated) */
    object_t           * curr_obj; /* Associated object */
    int                  socket;   /* Socket on our side */
    OutConnStatus        status;   /* Status of this entry */
} outconn[MAX_OUTCONN];

/*-------------------------------------------------------------------------*/

/* Forward declarations */

static void mccp_telnet_neg(int);

static void free_input_to(input_to_t *);
static void free_input_handler(input_t *);
static void telnet_neg(interactive_t *);
static void telnet_neg_reset(interactive_t *);
static void send_will(int);
static void send_wont(int);
static void send_do(int);
static void send_dont(int);
static void add_flush_entry(interactive_t *ip);
static void remove_flush_entry(interactive_t *ip);
static void clear_message_buf(interactive_t *ip);
static void new_player(object_t *receiver, SOCKET_T new_socket, struct sockaddr_in *addr, size_t len, int login_port);

#ifdef ERQ_DEMON

static long read_32(char *);
static Bool send_erq(int handle, int request, const char *arg, size_t arglen);
static void shutdown_erq_demon(void);
static void stop_erq_demon(Bool);
static string_t * lookup_ip_entry (struct in_addr addr, Bool useErq);
static void add_ip_entry(struct in_addr addr, const char *name);
#ifdef USE_IPV6
static void update_ip_entry(const char *oldname, const char *newname);
#endif

#endif /* ERQ_DEMON */

static INLINE ssize_t comm_send_buf(char *msg, size_t size, interactive_t *ip);

#ifdef USE_IPV6

/*-------------------------------------------------------------------------*/

/* Not every IPv6 supporting platform has all the defines (like AIX 4.3) */

#ifndef IPV6_ADDR_SCOPE_GLOBAL
#  define IPV6_ADDR_SCOPE_GLOBAL 0x0e
#endif

#ifndef s6_addr8
#  define s6_addr8  __u6_addr.__u6_addr8
#endif

#ifndef s6_addr16
#  define s6_addr16 __u6_addr.__u6_addr16
#endif

#ifndef s6_addr32
#  define s6_addr32 __u6_addr.__u6_addr32
#endif

static inline void CREATE_IPV6_MAPPED(struct in_addr *v6, uint32 v4) {
  v6->s6_addr32[0] = 0;
  v6->s6_addr32[1] = 0;
  v6->s6_addr32[2] = htonl(0x0000ffff);
  v6->s6_addr32[3] = v4;
}

/* These are the typical IPv6 structures - we use them transparently.
 *
 * --- arpa/inet.h ---
 *
 * struct in6_addr {
 *         union {
 *                 u_int32_t u6_addr32[4];
 * #ifdef notyet
 *                 u_int64_t u6_addr64[2];
 * #endif
 *                 u_int16_t u6_addr16[8];
 *                 u_int8_t  u6_addr8[16];
 *         } u6_addr;
 * };
 * #define s6_addr32       u6_addr.u6_addr32
 * #ifdef notyet
 * #define s6_addr64       u6_addr.u6_addr64
 * #endif
 * #define s6_addr16       u6_addr.u6_addr16
 * #define s6_addr8        u6_addr.u6_addr8
 * #define s6_addr         u6_addr.u6_addr8
 *
 * --- netinet/in.h ---
 *
 * struct sockaddr_in6 {
 *    u_char                sin6_len;
 *    u_char                sin6_family;
 *    u_int16_t        sin6_port;
 *    u_int32_t        sin6_flowinfo;
 *    struct                 in6_addr        sin6_addr;
 * };
 *
 */

/*-------------------------------------------------------------------------*/
static char *
inet6_ntoa (struct in6_addr in)

/* Convert the ipv6 address <in> into a string and return it.
 * Note: the string is stored in a local buffer.
 */

{
    static char str[INET6_ADDRSTRLEN+1];

    if (NULL == inet_ntop(AF_INET6, &in, str, INET6_ADDRSTRLEN))
    {
        perror("inet_ntop");
    }
    return str;
} /* inet6_ntoa() */

/*-------------------------------------------------------------------------*/
static struct in6_addr
inet6_addr (const char *to_host)

/* Convert the name <to_host> into a ipv6 address and return it.
 */

{
    struct in6_addr addr;

    inet_pton(AF_INET6, to_host, &addr);
    return addr;
} /* inet6_addr() */

#endif /* USE_IPV6 */

/*-------------------------------------------------------------------------*/
static char *
decode_noecho (char noecho)

/* Decode the <noecho> flag byte into a string.
 * Result is a pointer to a static buffer.
 */

{
    static char buf[100];
    strcpy(buf, "(");
    if (noecho & NOECHO_REQ) strcat(buf, "NOECHO_REQ, ");
    if (noecho & CHARMODE_REQ) strcat(buf, "CHARMODE_REQ, ");
    if (noecho & NOECHO) strcat(buf, "NOECHO, ");
    if (noecho & CHARMODE) strcat(buf, "CHARMODE, ");
    if (noecho & NOECHO_ACK) strcat(buf, "NOECHO_ACK, ");
    if (noecho & CHARMODE_ACK) strcat(buf, "CHARMODE_ACK, ");
#ifdef SAVE_NOECHO
    if (noecho & NOECHO_DELAYED) strcat(buf, "NOECHO_DELAYED, ");
#endif
    if (noecho & NOECHO_STALE) strcat(buf, "NOECHO_STALE, ");
    if (noecho & IGNORE_BANG) strcat(buf, "IGNORE_BANG");
    strcat(buf, ")");

    return buf;
} /* decode_noecho() */

/*-------------------------------------------------------------------------*/
static void
dump_bytes (void * data, size_t length, int indent)

/* Write the datablock starting at <data> of size <length> to stderr.
 * If it spans more than one line, indent the following lines by <indent>.
 */

{
    int cur_indent = 0;
    unsigned char * datap = (unsigned char *)data;

    while (length > 0)
    {
        size_t count;

        if (cur_indent)
            fprintf(stderr, "%*.*s", cur_indent, cur_indent, " ");
        else
            cur_indent = indent;
        fprintf(stderr, " %p:", datap);

        for (count = 0; count < 16 && length > 0; ++count, --length, ++datap)
        {
            fprintf(stderr, " %02x", *datap);
        }
        putc('\n', stderr);
    }
} /* dump_bytes() */

/*-------------------------------------------------------------------------*/
static void comm_fatal (interactive_t *ip, char *fmt, ...)
                          FORMATDEBUG(printf,2,3) ;
static void
comm_fatal (interactive_t *ip, char *fmt, ...)

/* The telnet code ran into a fatal error.
 * Dump the data from the current interactive structure and disconnect
 * the user (we have to assume that the interactive structure is
 * irrecoverably hosed).
 * TODO: Make similar functions comm_error(), comm_perror() which prefix
 * TODO:: the error message with the ip %p and obj name.
 */

{
    va_list va;
    static Bool in_fatal = MY_FALSE;
    char * ts;
    char * msg = "\r\n=== Internal communications error in mud driver.\r\n"
                   "=== Please log back in and inform the administration.\r\n"
                   "\r\n";

    /* Prevent double fatal. */
    if (in_fatal)
        fatal("Recursive call to comm_fatal().");
    in_fatal = MY_TRUE;
    ts = time_stamp();

    /* Print the error message */

    va_start(va, fmt);

    fflush(stdout);
    fprintf(stderr, "%s ", ts);
    vfprintf(stderr, fmt, va);
    fflush(stderr);
    if (current_object)
        fprintf(stderr, "%s Current object was %s\n"
                      , ts, current_object->name
                            ? get_txt(current_object->name) : "<null>");
    debug_message("%s ", ts);
    vdebug_message(fmt, va);
    if (current_object)
        debug_message("%s Current object was %s\n"
                     , ts, current_object->name
                           ? get_txt(current_object->name) : "<null>");
    debug_message("%s Dump of the call chain:\n", ts);
    (void)dump_trace(MY_TRUE, NULL, NULL); fflush(stdout);

    va_end(va);

    /* Dump the interactive structure */

    fprintf(stderr, "--- Dump of current interactive structure (%p..%p) --- \n"
                  , ip, ip + sizeof(*ip) - 1);
    fprintf(stderr, "  .socket:            %d\n", ip->socket);
    fprintf(stderr, "  .ob:                %p", ip->ob);
      if (ip->ob)  fprintf(stderr, " (%s)", get_txt(ip->ob->name));
      putc('\n', stderr);
    fprintf(stderr, "  .input_handler:     %p\n", ip->input_handler);
    fprintf(stderr, "  .modify_command:    %p", ip->modify_command);
      if (ip->modify_command)  fprintf(stderr, " (%s)", get_txt(ip->modify_command->name));
      putc('\n', stderr);
    fprintf(stderr, "  .prompt:           ");
      dump_bytes(&(ip->prompt), sizeof(ip->prompt), 21);
    fprintf(stderr, "  .addr:             ");
      dump_bytes(&(ip->addr), sizeof(ip->addr), 21);
    fprintf(stderr, "  .closing:           %02hhx\n", (unsigned char)ip->closing);
    fprintf(stderr, "  .tn_enabled:        %02hhx\n", (unsigned char)ip->tn_enabled);
    fprintf(stderr, "  .do_close:          %02hhx", (unsigned char)ip->do_close);
      if (ip->do_close & (FLAG_DO_CLOSE|FLAG_PROTO_ERQ)) fprintf(stderr, " (");
      if (ip->do_close & FLAG_DO_CLOSE) fprintf(stderr, "DO_CLOSE");
      if (ip->do_close & (FLAG_DO_CLOSE|FLAG_PROTO_ERQ)) fprintf(stderr, ", ");
      if (ip->do_close & FLAG_PROTO_ERQ) fprintf(stderr, "PROTO_ERQ");
      if (ip->do_close & (FLAG_DO_CLOSE|FLAG_PROTO_ERQ)) fprintf(stderr, ")");
      putc('\n', stderr);
    fprintf(stderr, "  .noecho:            %02hhx", (unsigned char)ip->noecho);
      fprintf(stderr, " %s\n", decode_noecho(ip->noecho));
    fprintf(stderr, "  .tn_state:          %hhd", ip->tn_state);
      switch(ip->tn_state) {
      case TS_DATA:       fprintf(stderr, " (TS_DATA)\n"); break;
      case TS_IAC:        fprintf(stderr, " (TS_IAC)\n"); break;
      case TS_WILL:       fprintf(stderr, " (TS_WILL)\n"); break;
      case TS_WONT:       fprintf(stderr, " (TS_WONT)\n"); break;
      case TS_DO:         fprintf(stderr, " (TS_DO)\n"); break;
      case TS_DONT:       fprintf(stderr, " (TS_DONT)\n"); break;
      case TS_SB:         fprintf(stderr, " (TS_SB)\n"); break;
      case TS_SB_IAC:     fprintf(stderr, " (TS_SB_IAC)\n"); break;
      case TS_READY:      fprintf(stderr, " (TS_READY)\n"); break;
      case TS_CHAR_READY: fprintf(stderr, " (TS_CHAR_READY)\n"); break;
      case TS_SYNCH:      fprintf(stderr, " (TS_SYNCH)\n"); break;
      case TS_INVALID:    fprintf(stderr, " (TS_INVALID)\n"); break;
      default: putc('\n', stderr);
      }
    fprintf(stderr, "  .supress_go_ahead:  %02hhx\n", (unsigned char)ip->supress_go_ahead);
    fprintf(stderr, "  .text_prefix:       %hd (%p)\n", ip->text_prefix, ip->text+ip->text_prefix);
    fprintf(stderr, "  .tn_start:          %hd (%p)\n", ip->tn_start, ip->text+ip->tn_start);
    fprintf(stderr, "  .tn_end:            %hd (%p)\n", ip->tn_end, ip->text+ip->tn_end);
    fprintf(stderr, "  .text_end:          %hd (%p)\n", ip->text_end, ip->text+ip->text_end);
    fprintf(stderr, "  .command_start:     %hd (%p)\n", ip->command_start, ip->command+ip->command_start);
    fprintf(stderr, "  .command_end:       %hd (%p)\n", ip->command_end, ip->command+ip->command_end);
    fprintf(stderr, "  .command_unprc_end: %hd (%p)\n", ip->command_unprocessed_end, ip->command+ip->command_unprocessed_end);
    fprintf(stderr, "  .command_printed:   %hd (%p)\n", ip->command_printed, ip->command+ip->command_printed);
    fprintf(stderr, "  .snoop_on:          %p", ip->snoop_on);
      if (ip->snoop_on && ip->snoop_on->ob) fprintf(stderr, " (%s)", get_txt(ip->snoop_on->ob->name));
      putc('\n', stderr);
    fprintf(stderr, "  .snoop_by:          %p", ip->snoop_by);
      if (ip->snoop_by) fprintf(stderr, " (%s)", get_txt(ip->snoop_by->name));
      putc('\n', stderr);
    fprintf(stderr, "  .last_time:         %"PRIdMPINT"\n", ip->last_time);
    fprintf(stderr, "  .numCmds:           %ld\n", ip->numCmds);
    fprintf(stderr, "  .maxNumCmds:        %ld\n", ip->maxNumCmds);
    fprintf(stderr, "  .trace_level:       %d\n", ip->trace_level);
    fprintf(stderr, "  .trace_prefix:      %p", ip->trace_prefix);
      if (ip->trace_prefix) fprintf(stderr, " '%s'", get_txt(ip->trace_prefix));
      putc('\n', stderr);
    fprintf(stderr, "  .message_length:    %d (%p)\n", ip->message_length, ip->message_buf+ip->message_length);
    fprintf(stderr, "  .next_for_flush:    %p", ip->next_player_for_flush);
      if (ip->next_player_for_flush) fprintf(stderr, " (%s)", get_txt(ip->next_player_for_flush->ob->name));
      putc('\n', stderr);
    fprintf(stderr, "  .prev_for_flush:    %p", ip->previous_player_for_flush);
      if (ip->previous_player_for_flush) fprintf(stderr, " (%s)", get_txt(ip->previous_player_for_flush->ob->name));
      putc('\n', stderr);
    fprintf(stderr, "  .access_class:      %ld\n", ip->access_class);
    fprintf(stderr, "  .charset:          ");
      dump_bytes(&(ip->charset), sizeof(ip->charset), 21);
    fprintf(stderr, "  .combine_cset:     ");
      dump_bytes(&(ip->combine_cset), sizeof(ip->combine_cset), 21);
    fprintf(stderr, "  .quote_iac:         %02hhx\n", (unsigned char)ip->quote_iac);
    fprintf(stderr, "  .catch_tell_activ:  %02hhx\n", (unsigned char)ip->catch_tell_activ);
    fprintf(stderr, "  .gobble_char:       %02hhx\n", (unsigned char)ip->gobble_char);
    fprintf(stderr, "  .syncing:           %02hhx\n", (unsigned char)ip->syncing);
    fprintf(stderr, "  .text:             ");
      dump_bytes(&(ip->text), sizeof(ip->text), 21);
    fprintf(stderr, "  .command:          ");
      dump_bytes(&(ip->command), sizeof(ip->command), 21);
    fprintf(stderr, "  .message_buf:      ");
      dump_bytes(&(ip->message_buf), sizeof(ip->message_buf), 21);
    fprintf(stderr, "------\n");

    /* Disconnect the user */
    comm_send_buf(msg, strlen(msg), ip);
    remove_interactive(ip->ob, MY_TRUE);

    /* Unset mutex */
    in_fatal = MY_FALSE;
} /* comm_fatal() */

/*-------------------------------------------------------------------------*/
static void
set_socket_nosigpipe (SOCKET_T new_socket)

/* Set the <new_socket> to not generate SIGPIPE signals (we anyway ignore them).
 * Failure is acceptable, the option is only available on some systems.
 */

{
#ifdef SO_NOSIGPIPE
    const int nopipe = 1;
    if (setsockopt(new_socket, SOL_SOCKET, SO_NOSIGPIPE, &nopipe, sizeof(nopipe)) != 0)
    {
        perror("Failure setting SO_NOSIGPIPE");
    }
#endif
}

/*-------------------------------------------------------------------------*/
static void
set_socket_nonblocking (SOCKET_T new_socket)

/* Set the <new_socket> into non-blocking mode.
 * Abort on error.
 */

{
    int tmp;

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

} /* set_socket_nonblocking() */

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
} /* set_close_on_exec() */

/*-------------------------------------------------------------------------*/
static void
set_socket_own (SOCKET_T new_socket)

/* Enable OOB communication on <new_socket>: the driver is set to
 * receive SIGIO and SIGURG signals, and OOBINLINE is enabled.
 * Failure is acceptable as both facilities are not available everywhere.
 */

{
#if defined(F_SETOWN) && defined(USE_FCNTL_SETOWN)
    if (0 > fcntl(new_socket, F_SETOWN, getpid()))
    {
        perror("fcntl SETOWN");
    }
#endif
#if defined(SO_OOBINLINE) && defined(USE_OOBINLINE)
    {
        int on = 1;
        if (0 > setsockopt(new_socket, SOL_SOCKET, SO_OOBINLINE, (char *)&on, sizeof on))
        {
            perror("setsockopt SO_OOBINLINE");
        }
    }
#endif
    new_socket = 0; /* Prevent 'not used' warning */
} /* set_socket_own() */

/*-------------------------------------------------------------------------*/
void
initialize_host_name (const char *hname)

/* This function is called at an early stage of the driver startup in order
 * to initialise the global host_name with a useful value (specifically so
 * that we can open the debug.log file).
 * If <hname> is given, the hostname is parsed from the string, otherwise it
 * is queried from the system.
 *
 * The value set in this function will later be overwritten by the
 * call to initialize_host_ip_number().
 *
 * exit() on failure.
 */

{
    char *domain;

    /* Get the (possibly qualified) hostname */
    if (hname != NULL)
    {
        if (strlen(hname) > MAXHOSTNAMELEN)
        {
            fprintf(stderr, "%s Given hostname '%s' too long.\n"
                          , time_stamp(), hname);
            exit(1);
        }
        else
            strcpy(host_name, hname);
    }
    else
    {
        if (gethostname(host_name, sizeof host_name) == -1) {
            herror("gethostname");
            exit(1);
        }
    }

    /* Cut off the domain name part of the hostname, if any.
     */
    domain = strchr(host_name, '.');
    if (domain)
        *domain = '\0';
} /* initialize_host_name() */

/*-------------------------------------------------------------------------*/
void
initialize_host_ip_number (const char *hname, const char * haddr)

/* Initialise the globals host_ip_number and host_ip_addr_template.
 * If <hname> or <haddr> are given, the hostname/hostaddr are parsed
 * from the strings, otherwise they are queried from the system.
 *
 * Open the UDP port if requested so that it can be used in inaugurate_master().
 * exit() on failure.
 */

{
    char *domain;
    length_t tmp;

    /* Get the (possibly qualified) hostname */
    if (hname != NULL)
    {
        if (strlen(hname) > MAXHOSTNAMELEN)
        {
            fprintf(stderr, "%s Given hostname '%s' too long.\n"
                          , time_stamp(), hname);
            exit(1);
        }
        else
            strcpy(host_name, hname);
    }
    else
    {
        if (gethostname(host_name, sizeof host_name) == -1) {
            herror("gethostname");
            exit(1);
        }
    }

    /* Get the host address */
    memset(&host_ip_addr_template, 0, sizeof host_ip_addr_template);
    if (haddr != NULL)
    {
#ifndef USE_IPV6
        host_ip_number.s_addr = inet_addr(haddr);
        host_ip_addr_template.sin_family = AF_INET;
        host_ip_addr_template.sin_addr = host_ip_number;
#else
        host_ip_number = inet6_addr(haddr);
        host_ip_addr_template.sin_family = AF_INET6;
        host_ip_addr_template.sin_addr = host_ip_number;
#endif

        /* Find the domain part of the hostname */
        domain = strchr(host_name, '.');
    }
    else
    {
        struct hostent *hp;

#ifndef USE_IPV6
        hp = gethostbyname(host_name);
        if (!hp) {
            fprintf(stderr, "%s gethostbyname: unknown host '%s'.\n"
                          , time_stamp(), host_name);
            exit(1);
        }
        memcpy(&host_ip_addr_template.sin_addr, hp->h_addr, (size_t)hp->h_length);
        host_ip_addr_template.sin_family = (unsigned short)hp->h_addrtype;
#else
        hp = gethostbyname2(host_name, AF_INET6);
        if (!hp)
            hp = gethostbyname2(host_name, AF_INET);
        if (!hp)
        {
            fprintf(stderr, "%s gethostbyname2: unknown host '%s'.\n"
                          , time_stamp(), host_name);
            exit(1);
        }
        memcpy(&host_ip_addr_template.sin_addr, hp->h_addr, (size_t)hp->h_length);

        if (hp->h_addrtype == AF_INET)
        {
            CREATE_IPV6_MAPPED(&host_ip_addr_template.sin_addr, *(u_int32_t*)hp->h_addr_list[0]);
        }
        host_ip_addr_template.sin_family = AF_INET6;
#endif

        host_ip_number = host_ip_addr_template.sin_addr;

        /* Now set the template to the proper _ANY value */
        memset(&host_ip_addr_template.sin_addr, 0, sizeof(host_ip_addr_template.sin_addr));
#ifndef USE_IPV6
        host_ip_addr_template.sin_addr.s_addr = INADDR_ANY;
        host_ip_addr_template.sin_family = AF_INET;
#else
        host_ip_addr_template.sin_addr = in6addr_any;
        host_ip_addr_template.sin_family = AF_INET6;
#endif

        /* Find the domain part of the hostname */
        if (hname == NULL)
            domain = strchr(hp->h_name, '.');
        else
            domain = strchr(host_name, '.');
    }

#ifndef USE_IPV6
    printf("%s Hostname '%s' address '%s'\n"
          , time_stamp(), host_name, inet_ntoa(host_ip_number));
    debug_message("%s Hostname '%s' address '%s'\n"
                 , time_stamp(), host_name, inet_ntoa(host_ip_number));
#else
    printf("%s Hostname '%s' address '%s'\n"
          , time_stamp(), host_name, inet6_ntoa(host_ip_number));
    debug_message("%s Hostname '%s' address '%s'\n"
                 , time_stamp(), host_name, inet6_ntoa(host_ip_number));
#endif

    /* Put the domain name part of the hostname into domain_name, then
     * strip it off the host_name[] (as only query_host_name() is going
     * to need it).
     * Note that domain might not point into host_name[] here, so we
     * can't just stomp '\0' in there.
     */
    if (domain)
    {
        domain_name = strdup(domain+1);
    }
    else
        domain_name = strdup("unknown");

    domain = strchr(host_name, '.');
    if (domain)
        *domain = '\0';

    /* Initialize udp at an early stage so that the master object can use
     * it in inaugurate_master() , and the port number is known.
     */
    if (udp_port != -1)
    {
        struct sockaddr_in host_ip_addr;

        memcpy(&host_ip_addr, &host_ip_addr_template, sizeof(host_ip_addr));

        host_ip_addr.sin_port = htons((u_short)udp_port);
        debug_message("%s UDP recv-socket requested for port: %d\n"
                     , time_stamp(), udp_port);
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
                          , sizeof(host_ip_addr)))
            {
                if (errno == EADDRINUSE) {
                    fprintf(stderr, "%s UDP port %d already bound!\n"
                                  , time_stamp(), udp_port);
                    debug_message("%s UDP port %d already bound!\n"
                                  , time_stamp(), udp_port);
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
        struct sockaddr_in host_ip_addr;

        tmp = sizeof(host_ip_addr);
        if (!getsockname(udp_s, (struct sockaddr *)&host_ip_addr, &tmp))
        {
            int oldport = udp_port;

            udp_port = ntohs(host_ip_addr.sin_port);
            if (oldport != udp_port)
                debug_message("%s UDP recv-socket on port: %d\n"
                             , time_stamp(), udp_port);
        }
        set_socket_nonblocking(udp_s);
        set_close_on_exec(udp_s);
        if (socket_number(udp_s) >= min_nfds)
            min_nfds = socket_number(udp_s)+1;
    }

} /* initialize_host_ip_number() */


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
}

/*-------------------------------------------------------------------------*/
void
prepare_ipc(void)

/* Open all login sockets on driver startup, exit() on a failure.
 */

{
    length_t tmp;
    int i;
    struct sigaction sa; // for installing the signal handlers

#ifdef ERQ_DEMON
    /* Initialize the IP name lookup table */
    memset(iptable, 0, sizeof(iptable));
#endif

    for (i = 0; i < MAX_OUTCONN; i++)
        outconn[i].status = ocNotUsed;

    /* Initialize the telnet machine unless mudlib_telopts() already
     * did that.
     */
    if (!telopts_do[0])
      init_telopts();

    /* Loop over all given port numbers.
     * Remember: positive number are actual port numbers to be opened,
     * negative numbers are the fd numbers of already existing sockets.
     */
    for (i = 0; i < numports; i++)
    {
        struct sockaddr_in host_ip_addr;

        memcpy(&host_ip_addr, &host_ip_addr_template, sizeof(host_ip_addr));

        if (port_numbers[i] > 0)
        {
            /* Real port number */

            host_ip_addr.sin_port = htons((u_short)port_numbers[i]);
            sos[i] = socket(host_ip_addr.sin_family, SOCK_STREAM, 0);
            if ((int)sos[i] == -1) {
                perror("socket");
                exit(1);
            }
            tmp = 1;
            if (setsockopt(sos[i], SOL_SOCKET, SO_REUSEADDR
                          , (char *) &tmp, sizeof (tmp)) < 0) {
                perror ("setsockopt");
                exit (1);
            }
            if (bind(sos[i], (struct sockaddr *)&host_ip_addr, sizeof host_ip_addr) == -1) {
                if (errno == EADDRINUSE) {
                    fprintf(stderr, "%s Port %d already bound!\n"
                                  , time_stamp(), port_numbers[i]);
                    debug_message("%s Port %d already bound!\n"
                                 , time_stamp(), port_numbers[i]);
                    exit(errno);
                } else {
                    perror("bind");
                    exit(1);
                }
            }
        }
        else {

            /* Existing socket */

            sos[i] = -port_numbers[i];
            tmp = sizeof(host_ip_addr);
            if (!getsockname(sos[i], (struct sockaddr *)&host_ip_addr, &tmp))
                port_numbers[i] = ntohs(host_ip_addr.sin_port);
        }

        /* Initialise the socket */
        if (listen(sos[i], 5) == -1) {
            perror("listen");
            exit(1);
        }
        set_socket_nonblocking(sos[i]);
        set_close_on_exec(sos[i]);
        set_socket_nosigpipe(sos[i]);

        if (socket_number(sos[i]) >= min_nfds)
            min_nfds = socket_number(sos[i])+1;
    } /* for(i = 0..numports) */

    // install some signal handlers
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART; // restart syscalls after handling a signal

    /* We ignore SIGPIPE and handle the errors on write/send ourself */
    sa.sa_handler = SIG_IGN;
    if (sigaction(SIGPIPE, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGPIPE");   // uhoh. SIGPIPE terminates...

    sa.sa_handler = urgent_data_handler;
    if (sigaction(SIGURG, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGURG");
    if (sigaction(SIGIO, &sa, NULL) == -1)
        perror("Unable to install signal handler for SIGIO");   // uhoh. SIGIO terminates...

} /* prepare_ipc() */

/*-------------------------------------------------------------------------*/
void
ipc_remove (void)

/* Called when the driver is shutting down, this function closes all
 * open sockets.
 */

{
    int i;

    printf("%s Shutting down ipc...\n", time_stamp());
    for (i = 0; i < numports; i++)
        socket_close(sos[i]);

    if (udp_s >= 0)
        socket_close(udp_s);

#ifdef ERQ_DEMON
    shutdown_erq_demon();
#endif

} /* ipc_remove() */

/*-------------------------------------------------------------------------*/
static INLINE ssize_t
comm_send_buf (char *msg, size_t size, interactive_t *ip)

/* Low level send routine, just tries to send the data without buffering. 
   A return value -1 indicates a total failure (i.e. the connection should
   be dropped), otherwise the amount of data sent (even if it's 0).
*/
{
    int     retries;   /* Number of retries left when sending data */
    ssize_t n;         /* Bytes that have been sent */

    for (retries = 6;;)
    {
#ifdef USE_TLS
        if ((n = (ip->tls_status == TLS_INACTIVE ? 
                  (int)socket_write(ip->socket, msg, size):
                  (int)tls_write(ip, msg, size))) != -1)
#else
        if ((n = (int)socket_write(ip->socket, msg, size)) != -1)
#endif
        {
            break;
        }

        switch (errno)
        {

        case EINTR:
            if (--retries)
                continue;
            n = 0;
            break;

        case EWOULDBLOCK:
            n = 0;
            break;

        case EMSGSIZE:
            fprintf(stderr, "%s comm: write EMSGSIZE.\n", time_stamp());
            break;

        case EINVAL:
            fprintf(stderr, "%s comm: write EINVAL.\n", time_stamp());
            break;

        case ENETUNREACH:
            fprintf(stderr, "%s comm: write ENETUNREACH.\n", time_stamp());
            break;

        case EHOSTUNREACH:
            fprintf(stderr, "%s comm: write EHOSTUNREACH.\n", time_stamp());
            break;

        case EPIPE:
            fprintf(stderr, "%s comm: write EPIPE detected\n", time_stamp());
            break;

        case ECONNRESET:
            fprintf(stderr, "%s comm: write ECONNRESET detected\n", time_stamp());
            break;

        default:
            {
                int e = errno;
                fprintf(stderr, "%s comm: write: unknown errno %d (%s)\n"
                                  , time_stamp(), e, strerror(e));
            }
        }

        if (n == 0) // don't close for EWOULDBLOCK/EINTR
            return 0;

        ip->do_close = FLAG_DO_CLOSE;
        return -1;

    } /* for (retries) */

#ifdef COMM_STAT
    inet_packets++;
    inet_volume += n;
#endif

    return n;
} /* comm_send_buf() */

/*-------------------------------------------------------------------------*/
Bool
comm_socket_write (char *msg, size_t size, interactive_t *ip, write_buffer_flag_t flags)

/* Stand in for socket_write(): take the data to be written, compress and
 * encrypt them if needed and send them to <ip>. If no data can be send
 * right now append them to the buffer. Returns false if no more data
 * is accepted at this time (messages are discarded or the connection
 * was dropped).
 */

{
    struct write_buffer_s *b;
    char *buf;
    size_t length;

    if (size == 0)
        return MY_TRUE;

#ifdef USE_MCCP
    /* We cannot discard already compressed packets,
     * because the zlib will generate a checksum
     * over all bytes. So we have to check before
     * compressing the message whether we can send 
     * or put them in the write buffer.
     *
     * To provide a consistent behavior we do this
     * also for uncompressed connections.
     */
#endif

    if (!(flags & WB_NONDISCARDABLE) && ip->write_first)
    {
        p_int max_size;

        max_size = ip->write_max_size;
        if (max_size == -2)
            max_size = write_buffer_max_size;

        if (max_size >= 0 && ip->write_size >= (p_uint) max_size)
        {
            /* Buffer overflow. */
            if (ip->msg_discarded != DM_NONE)
                return MY_FALSE; /* Message will be or was sent. */

            /* Notify the master about it. */
            ip->msg_discarded = DM_SEND_INFO;
            add_flush_entry(ip);

            return MY_FALSE;
        }
    }

#ifdef USE_MCCP
    if (ip->out_compress)
    {
        int status;

        ip->out_compress->next_in = (unsigned char *) msg;
        ip->out_compress->avail_in = size;

        ip->out_compress->next_out = ip->out_compress_buf;
        ip->out_compress->avail_out = COMPRESS_BUF_SIZE;

        status = deflate(ip->out_compress, Z_SYNC_FLUSH);

        if (status != Z_OK)
        {
            fprintf(stderr, "%s comm: MCCP compression error: %d\n"
                          , time_stamp(), status);
            return MY_FALSE;
        }

        /* ok.. perhaps i should take care that all data in message_buf
         * is compressed, but i guess there is no chance that 1024 byte
         * compressed won't fit into the 8192 byte buffer
         */

        length = ip->out_compress->next_out - ip->out_compress_buf;
        buf = (char *) ip->out_compress_buf;
    }
    else
#endif /* USE_MCCP */
    {
        buf = msg;
        length = size;
    }

    /* now sending the buffer... */
    if (ip->write_first == NULL)
    {
        /* Try writing to the socket first. */
        ssize_t n = comm_send_buf(buf, length, ip);

        if (n == -1)
            return MY_FALSE;
        else if (n == length)
        {
            /* We're done. */
            return MY_TRUE;
        }
        else if (n > 0)
        {
            buf += n;
            length -= n;
        }
    }

    /* We have to enqueue the message. */

    b = xalloc(sizeof(struct write_buffer_s) + length - 1);
    if (!b)
        outofmem(sizeof(struct write_buffer_s) + length - 1, "comm_socket_write()");

    b->length = length;
    b->pos = 0;
    b->flags = flags;
    b->next = NULL;
    memcpy(b->buffer, buf, length);

     /* Chain in the new buffer */
    if (ip->write_first)
        ip->write_last = ip->write_last->next = b;
    else
        ip->write_last = ip->write_first = b;

    ip->write_size += length;
    ip->msg_discarded = DM_NONE;

    return MY_TRUE;
} /* comm_socket_write() */

/*-------------------------------------------------------------------------*/
static void
comm_write_pending (interactive_t * ip)

/* Send as much from the write buffer of <ip> as possible.
 */

{
    while (ip->write_first != NULL)
    {
        struct write_buffer_s *buf;
        ssize_t n;

        buf = ip->write_first;
        n = comm_send_buf(buf->buffer + buf->pos, buf->length - buf->pos, ip);

        if (n == -1)
            return;

        buf->pos += n;
        if (buf->pos < buf->length)
            return;

        ip->write_first = buf->next;
        ip->write_size -= buf->length;
        xfree(buf);
    }
} /* comm_write_pending() */

/*-------------------------------------------------------------------------*/
static void
add_discarded_message (interactive_t *ip)

/* Calls the H_MSG_DISCARDED driver hook and adds the
 * message to the write buffer of <ip>. <ip> is removed
 * from the list of dirty interactives if it's clean afterwards.
 */
{
    string_t *discarded_msg;

    if (driver_hook[H_MSG_DISCARDED].type == T_CLOSURE)
    {
        if (driver_hook[H_MSG_DISCARDED].x.closure_type == CLOSURE_LAMBDA)
        {
            free_object(driver_hook[H_MSG_DISCARDED].u.lambda->ob, "add_discarded_message");
            driver_hook[H_MSG_DISCARDED].u.lambda->ob = ref_object(ip->ob, "add_discarded_message");
        }

        push_ref_valid_object(inter_sp, ip->ob, "add_discarded_message");

        call_lambda(&driver_hook[H_MSG_DISCARDED], 1);

        if (inter_sp->type == T_STRING)
        {
            /* The new discarded_msg. Adopt the reference. */
            discarded_msg = inter_sp->u.str;

            inter_sp->type = T_INVALID;
            inter_sp--;
        }
        else
        {
            pop_stack();
            discarded_msg = NULL;
        }
    }
    else if (driver_hook[H_MSG_DISCARDED].type == T_STRING)
    {
        discarded_msg = ref_mstring(driver_hook[H_MSG_DISCARDED].u.str);
    }
    else
    {
        discarded_msg = ref_mstring(STR_DISCARDED_MSG);
    }

    if (discarded_msg)
    {
        /* Append it to the write buffer. */
        comm_socket_write(get_txt(discarded_msg), mstrsize(discarded_msg)
                        , ip, WB_NONDISCARDABLE);
        free_mstring(discarded_msg);
    }

    ip->msg_discarded = DM_INFO_WAS_SENT;

    if (!ip->message_length)
        remove_flush_entry(ip);
}

/*-------------------------------------------------------------------------*/
void
add_message_flush ()

/* Flush the message buffer of the current command giver.
 *
 * If an error other than EINTR occurred while sending the data to
 * the network, the message is discarded and the socket is marked
 * for disconnection.
 */

{
    interactive_t *ip;       /* The interactive user */
    int length;

    if ( command_giver == NULL
     || !(O_SET_INTERACTIVE(ip, command_giver)))
        return;

    length = ip->message_length;
    if (!length)
        return;

    ip->message_length = 0; /* In case of any recursion. */
    comm_socket_write(ip->message_buf, length, ip, 0);
    clear_message_buf(ip);
} /* add_messagee_flush() */

/*-------------------------------------------------------------------------*/
void
add_message_bytes (const char* bytes, size_t len)

/* Send the given bytes without encoding to the current command giver.
 * Also there is no IAC quoting.
 *
 * All messages are accumulated in interactive.message_buf, which is
 * flushed when it is full. This flush can be forced by calling
 * add_message_flush().
 */
{
    interactive_t *ip;       /* The interactive user */
    int old_message_length;  /* accumulated message length so far */

    if ( command_giver == NULL
     || (command_giver->flags & O_DESTRUCTED)
     || !(O_SET_INTERACTIVE(ip, command_giver)))
        return;

#ifdef COMM_STAT
        add_message_calls++;
#endif

    old_message_length = ip->message_length;

    while (len != 0)
    {
        if (len + ip->message_length <= MAX_SOCKET_PACKET_SIZE)
        {
            memcpy(ip->message_buf + ip->message_length, bytes, len);
            ip->message_length += len;
            break;
        }
        else
        {
            size_t remaining = MAX_SOCKET_PACKET_SIZE - ip->message_length;

            memcpy(ip->message_buf + ip->message_length, bytes, remaining);
            len -= remaining;
            bytes += remaining;

            ip->message_length = 0;

            /* Write buffer is full, send it. */
            if (!comm_socket_write(ip->message_buf, MAX_SOCKET_PACKET_SIZE, ip, 0))
            {
                if (old_message_length)
                    clear_message_buf(ip);
                return;
            }
        }
    }

    /* --- Final touches --- */

    /* Update the list of interactives with pending data */

    if ( ip->message_length && !old_message_length )
    {
        /* Buffer became 'dirty': add this interactive to the list. */
        add_flush_entry(ip);
    }

    if ( !ip->message_length && old_message_length )
    {
        /* buffer has become empty */
        clear_message_buf(ip);
    }
} /* add_message_bytes() */

/*-------------------------------------------------------------------------*/
void
add_message_text (const char* str, size_t len)

/* Send the given UTF-8 string to the current command giver.
 *
 * This function also does the telnet, snooping, and shadow handling.
 * If an interactive player is shadowed, object.c::shadow_catch_message()
 * is called to give the shadows the opportunity to intercept the message.
 *
 * The message is converted to the current encoding of the interactive
 * and IAC bytes will be escaped.
 *
 * Messages which can't be send (e.g. because the command_giver was
 * destructed or disconnected) are printed on stdout, preceded by ']'.
 */

{
    interactive_t *ip;                  /* The interactive user */
    object_t      *snooper;             /* Snooper of <ip> */
    const char    *end = str + len;     /* End of the string */
    char buf[MAX_SOCKET_PACKET_SIZE];   /* Conversion buffer */

    /* Test if the command_giver is a real, living, undestructed user,
     * and not disconnected, closing or actually a new ERQ demon.
     * If the command_giver fails the test, the message is printed
     * to stdout and the function returns.
     */
    if ( command_giver == NULL
     || (command_giver->flags & O_DESTRUCTED)
     || !(O_SET_INTERACTIVE(ip, command_giver))
     || (ip->do_close)
       )
    {
        putchar(']');
        fputs(str, stdout);
        fflush(stdout);
        return;
    }

    /* If there's a shadow successfully handling the
     * message, return.
     * This may cause a recursive call to add_message()!.
     */

    if (shadow_catch_message(command_giver, str))
    {
        return;
    }

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
        if (O_IS_INTERACTIVE(snooper))
        {
            object_t *save;

            save = command_giver;
            command_giver = snooper;

            add_message_text("%", 1);
            add_message_text(str, len);

            command_giver = save;
        }
        else
        {
            trace_level |= ip->trace_level;

            tell_npc(snooper, STR_PERCENT);
            tell_npc_str(snooper, str);
        }
    } /* if (snooper) */

    while (true)
    {
        char *start, *dest;
        size_t inleft, outleft, rc;
        bool at_end;

        /* Skip any characters not in the charset.
         * We also handle \n here (before converting to an arbitrary encoding).
         */
        for (; str != end; str++)
        {
            char c = *str;

            /* Non-ASCII characters are always in the charset. */
            if (c & ~0x7f)
                break;

            /* Lookup in the charset. */
            if (ip->charset[(c&0x78)>>3] & 1<<(c&7))
                break;

            continue;
        }

        start = (char*) str;

        /* Search for the next character we shouldn't send. */
        for (; str != end; str++)
        {
            char c = *str;

            if (c == '\n')
                break;

            /* Same procedure as last loop. */
            if (c & ~0x7f)
                continue;

            if (ip->charset[(c&0x78)>>3] & 1<<(c&7))
                continue;

            break;
        }

        /* Now convert the range between start and str. */
        at_end = (start == str);
        inleft = str - start;
        dest = buf;
        outleft = sizeof(buf);

        if (str != end && at_end && *str == '\n' && (ip->charset[1] & 4))
        {
            start = "\r\n";
            inleft = 2;
            at_end = false;

            rc = iconv(ip->send_cd, &start, &inleft, &dest, &outleft);
            if (rc == (size_t)-1)
                start = (char*) str;
            else
                start = (char*) str + 1;
        }
        else if (at_end)
            rc = iconv(ip->send_cd, NULL, NULL, &dest, &outleft);
        else
            rc = iconv(ip->send_cd, &start, &inleft, &dest, &outleft);

        /* Regardless of any error, we send what we managed to get. */
        if (ip->quote_iac)
        {
            for (char* deststart = buf; deststart != dest;)
            {
                char* destend = deststart;
                for (; destend != dest && *(unsigned char*)destend != IAC; destend++);

                if (destend == dest)
                {
                    add_message_bytes(deststart, destend - deststart);
                    break;
                }
                else
                {
                    /* Send the IAC twice. */
                    add_message_bytes(deststart, destend - deststart + 1);
                    add_message_bytes(destend, 1);
                    deststart = destend + 1;
                }
            }
        }
        else
            add_message_bytes(buf, dest - buf);

        if (at_end)
            break;

        str = start;

        if (rc == (size_t)-1 && errno != E2BIG && inleft)
        {
            /* There was an invalid or incomplete multibyte sequence.
             * Skip to the next character.
             */
            str++;
        }
    }
} /* add_message_text() */

/*-------------------------------------------------------------------------*/
void
add_message_str (string_t *str)

/* Send a string to the current command_giver.
 * The string can be binary (will be sent without encoding)
 * or a unicode string (will be converted).
 */

{
    if (str->info.unicode == STRING_BYTES)
        add_message_bytes(get_txt(str), mstrsize(str));
    else
        add_message_text(get_txt(str), mstrsize(str));
} /* add_message_str() */

/*-------------------------------------------------------------------------*/
void
add_message (const char *fmt, ...)

/* Send a message to the current command_giver. The message is composed
 * from the <fmt> string and the following arguments using the normal
 * printf() semantics.
 *
 * The message will be composed in a local buffer and is therefore subject
 * to a length restriction.
 *
 * Note that add_message() might be called recursively.
 */

{
    char  buff[MAX_TEXT + MAX_TEXT/2];
      /* Composition buffer for the final message.
       * We hope that it's big enough, but to be sure the code will
       * check for buffer overruns.
       */
    va_list va;
    size_t len;

    va_start(va, fmt);
    len = vsnprintf(buff, sizeof(buff), fmt,va);
    va_end(va);

    /* old sprintf() implementations returned -1 if the output was
     * truncated. Since size_t is an unsigned type, the check for 
     * len == -1 is implicitly included by >= sizeof(...)-1, because
     * -1 will be wrapped to SIZE_T_MAX which is the maximum sizeof()
     * can return and can never be valid as return value here. */
    if (len >= sizeof(buff))
    {
        char err[] = "\n*** Message truncated ***\n";
        debug_message("%s Message too long (Length: %zu): '%.200s...'\n"
                     , time_stamp(), len, buff);

        /* We need to find the end of a character. */
        add_message_text(buff, utf8_prev(buff + sizeof(buff) - 1, sizeof(buff) - 1) - buff);
        add_message_text(err, sizeof(err)-1);
    }
    else
        add_message_text(buff, len);
} /* add_message() */

/*-------------------------------------------------------------------------*/
static INLINE void
reset_input_buffer (interactive_t *ip)

/* When returning from CHARMODE to LINEMODE, the input buffer variables
 * need to be reset. This function takes care of it.
 * Note that there might be additional lines pending between .tn_end and
 * .text_end.
 */

{
    if (ip->command_start)
    {
        DTN(("reset input buffer: cmd_start %hd, cmd_end %hd\n", 
             ip->command_start, ip->command_end));

        memmove(ip->command, ip->command + ip->command_start, ip->command_unprocessed_end - ip->command_start);
        ip->command_end -= ip->command_start;
        ip->command_unprocessed_end -= ip->command_start;
        if (ip->command_printed > ip->command_start)
            ip->command_printed -= ip->command_start;
        else
            ip->command_printed = 0;
        ip->command_start = 0;
    }
} /* reset_input_buffer() */

/*-------------------------------------------------------------------------*/
static void
add_flush_entry (interactive_t *ip)

/* Add the given interactive <ip> to the list of 'dirty' interactives.
 * The function is safe to call for interactives already in the list.
 */

{
    if ( ip->previous_player_for_flush || first_player_for_flush == ip)
        return;

    if ( NULL != (ip->next_player_for_flush = first_player_for_flush) )
    {
        first_player_for_flush->previous_player_for_flush = ip;
    }
    ip->previous_player_for_flush = NULL;
    first_player_for_flush = ip;
} /* add_flush_entry() */

/*-------------------------------------------------------------------------*/
static void
remove_flush_entry (interactive_t *ip)

/* Remove the given interactive <ip> from the list of 'dirty' interactives
 * and make sure it is really clean. The function is safe to call for
 * interactives not in the list.
 */

{
    /* To make it safe for calling the function even for interactives
     * not in the flush list, we check that <ip> is either in the middle
     * or at the end of the flush list (one or both of the .previous
     * and .next pointers is !NULL), or if .previous is NULL, that it is
     * the first entry in the list.
     */

    if ( ip->previous_player_for_flush )
    {
        ip->previous_player_for_flush->next_player_for_flush
          = ip->next_player_for_flush;
    }
    else if (first_player_for_flush == ip)
    {
        first_player_for_flush = ip->next_player_for_flush;
    }

    if ( ip->next_player_for_flush )
    {
        ip->next_player_for_flush->previous_player_for_flush
          = ip->previous_player_for_flush;
    }

    ip->previous_player_for_flush = NULL;
    ip->next_player_for_flush = NULL;
} /* remove_flush_entry() */

/*-------------------------------------------------------------------------*/
void
flush_all_player_mess (void)

/* Flush all pending data from the interactives. Usually called before
 * every input loop, after a user logged in, or after an LPC runtime
 * error was processed.
 */

{
    interactive_t *ip, *nip;
    object_t *save = command_giver;

    for ( ip = first_player_for_flush; ip != NULL; ip = nip)
    {
        nip = ip->next_player_for_flush;
          /* add_message() will clobber (ip)->next_player_for_flush! */
        command_giver = ip->ob;
        add_message_flush();

        if(ip->msg_discarded == DM_SEND_INFO)
            add_discarded_message(ip);
    }
    command_giver = save;
} /* flush_all_player_mess() */

/*-------------------------------------------------------------------------*/
static void
clear_message_buf (interactive_t *ip)

/* Clear the buffer of the given interactive <ip> and remove it from
 * the list of 'dirty' interactives if there is nothing else to do.
 * The function is safe to call for interactives not in the list.
 *
 * This function is called after an interactive sent all pending data (or
 * failing while doing so).
 */

{
    ip->message_length = 0;

    if (ip->msg_discarded != DM_SEND_INFO)
        remove_flush_entry(ip);
} /* clear_message_buf() */

/*-------------------------------------------------------------------------*/
Bool
get_message (char *buff, size_t *bufflength)

/* Get a message from a user, or wait until it is time for the next
 * heartbeat/callout. You can tell this apart by the result:
 *
 *   true: a user message was received and placed into buff (its size
 *         into bufflength); the user object is set as command_giver.
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
 * immediate return), the cycle begins again. If a heart_beat is due
 * even before select() executed, the waiting time for select() is
 * set to 0 so that only the status of the sockets is recorded and
 * get_message returns (almost) immediately.
 *
 * Normally, users can give only one command per cycle. The exception
 * is when they are editing, then they can give up to ALLOWED_ED_CMDS.
 *
 * Heartbeats are detected by checking the backend variable comm_time_-
 * to_call_heart_beat, which is set by the SIGALRM handler. If it is
 * true, get_message() sets the variable time_to_call_heart_beat to
 * inform the backend and returns.
 *
 * If a heart_beat occurs during the reading and returning of player
 * commands, the comm_time_c_h_b variable is set, but not evaluated.
 * This evaluation happens only when a select() is performed (therefore
 * the second variable time_to_c_h_b). This way every user can issure
 * at least one command in one backend cycle, even if that takes longer
 * than one heart_beat time. This makes it also legal for comm_to_c_h_b
 * to be set upon entering get_message().
 *
 * For short latency, the UDP socket is checked on every call to
 * get_message(), even if a previous select() did not mark it as ready
 * (this is disabled under BeOS and Windows).
 */

{
    /* State information: */
    static fd_set readfds, writefds, pexceptfds;
      /* List of sockets with pending data.
       * You can ignore a 'could be used uninitialized' warning.
       */
    static int NextCmdGiver = -1;
      /* Index of current user to check */
    static int CmdsGiven = 0;
      /* Number of commands the current user gave in this cycle. */

#   define StartCmdGiver       (max_player)
#   define DecreaseCmdGiver    NextCmdGiver--

    int    i;
    interactive_t * ip = NULL;
    fd_set exceptfds;


    /* The endless loop */

    while(MY_TRUE)
    {
        struct sockaddr_in addr;
        length_t length; /* length of <addr> */
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
            twait = comm_return_to_backend ? 0 : 1;
              /* If the heart_beat is due, just check the state
               * of the sockets, but don't wait.
               */

            /* Set up fd-sets. */

            FD_ZERO(&readfds);
            FD_ZERO(&writefds);
            FD_ZERO(&pexceptfds);
            for (i = 0; i < numports; i++) {
                FD_SET(sos[i], &readfds);
            } /* for */
            nfds = min_nfds;
            for (i = max_player + 1; --i >= 0;)
            {
                ip = all_players[i];
                if (!ip)
                    continue;

                if (ip->do_close)
                {
                    ip->do_close &= FLAG_PROTO_ERQ;
                    remove_interactive(ip->ob, MY_FALSE);
                    continue;
                }

                /* Process telnet negotiations up to the point where the
                 * next input is ready, if possible.
                 */
                telnet_neg(ip);

                if (ip->tn_state == TS_READY || ip->tn_state == TS_CHAR_READY)
                {
                    /* If telnet is ready for commands, react quickly. */
                    twait = 0;
                }

                if (ip->tn_state != TS_READY)
                {
                    FD_SET(ip->socket, &readfds);
                    if (socket_number(ip->socket) >= nfds)
                        nfds = socket_number(ip->socket)+1;
                }

                if (ip->write_first != NULL)
                {
                    /* There is something to write. */
                    FD_SET(ip->socket, &writefds);
                }

            } /* for (all players) */
#ifdef ERQ_DEMON
            if (erq_demon >= 0)
            {
                FD_SET(erq_demon, &readfds);
            }
#endif
            if (udp_s >= 0)
            {
                FD_SET(udp_s, &readfds);
            }

#ifdef USE_PGSQL
            pg_setfds(&readfds, &writefds, &nfds);
#endif
#ifdef USE_PYTHON
           python_set_fds(&readfds, &writefds, &pexceptfds, &nfds);
#endif

            /* select() until time is up or there is data */

            for (retries = 6;;)
            {
                check_alarm();
                timeout.tv_sec = twait;
                timeout.tv_usec = 0;
                res = socket_select(nfds, &readfds, &writefds, &pexceptfds, &timeout);
                if (res == -1)
                {
                    if (errno == EINTR)
                    {
                        /* We got an alarm, probably need heart_beat.
                         * But finish the select call since we already have
                         * prepared readfds.
                         */
                        if (comm_return_to_backend)
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
                DTN(("telnet wants to sync\n"));
                check_alarm();
                urgent_data = MY_FALSE;
                timeout.tv_sec = 0;
                timeout.tv_usec = 0;
                memset((char *)&exceptfds, 255, (size_t)(nfds + 7) >> 3);
                if (socket_select(nfds, 0, 0, &exceptfds, &timeout) > 0)
                {
                    for (i = max_player + 1; --i >= 0;)
                    {
                        ip = all_players[i];
                        if (!ip)
                            continue;
                        if (FD_ISSET(ip->socket, &exceptfds))
                        {
                            DTN(("ts_syncing = true\n"));
                            ip->syncing = true;
                            switch (ip->tn_state)
                            {
                              case TS_DATA:
                              case TS_READY:
                              case TS_CHAR_READY:
                                ip->tn_state = TS_SYNCH;
                                ip->gobble_char = '\0';
                                DTN(("tn_state = TS_SYNCH\n"));
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

#ifdef USE_PGSQL
            pg_process_all();
#endif
#ifdef USE_PYTHON
            python_handle_fds(&readfds, &writefds, &exceptfds, nfds);
#endif

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
                      (size_t)(&buf_from_erq[sizeof buf_from_erq] - input_from_erq)
                    );
                } while(l < 0 && errno == EINTR && --retries >= 0);

                /* If there is no data, stop the erq, else handle it. */

                if (l <= 0)
                {
#ifdef DEBUG_ERQ
                    fprintf(stderr, "%s read %"PRIdMPINT" bytes from erq demon\n"
                                  , time_stamp(), l);
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
                        Bool keep_handle;

                        /* Is the message length valid?
                         * TODO: use sizeof(struct) here
                         */
                        if (msglen < 8) {
#ifdef DEBUG_ERQ
                            fprintf( stderr
                                   , "%s invalid length of message from"
                                     "erq demon: %"PRIdMPINT" bytes\n"
                                   , time_stamp(), msglen);
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

                            if (msglen < 13 || rp[msglen-1]) {
#ifdef DEBUG
                              if (msglen == 12) {
                                if (d_flag > 1)
                                  debug_message("%s Host lookup failed\n"
                                               , time_stamp());
                              } else {
                                debug_message("%s Bogus reverse name lookup.\n"
                                             , time_stamp());
                              }
#endif
                            } else {
                                uint32 naddr;
                                struct in_addr net_addr;

                                memcpy((char*)&naddr, rp+8, sizeof(naddr));
#ifndef USE_IPV6
                                net_addr.s_addr = naddr;
#else
                                CREATE_IPV6_MAPPED(&net_addr, naddr);
#endif
                                add_ip_entry(net_addr, rp+12);
                            }
                            continue;
                        }
#ifdef USE_IPV6
                        else if (handle == ERQ_HANDLE_RLOOKUPV6)
                        {
                            /* The result of a hostname lookup. */

                            if (msglen < 9 || rp[msglen-1]) {
#ifdef DEBUG
                                debug_message("%s Bogus reverse name lookup.\n"
                                             , time_stamp());
#else
                                NOOP;
#endif
                            } else {
                                char * space;

                                space = strchr(rp+8, ' ');

                                if (space == NULL)
                                {
                                    debug_message("%s IP6 Host lookup failed: %s\n"
                                                 , time_stamp(), rp+8);
                                }
                                else if (strlen(space+1))
                                {
                                    *space = '\0';
                                    update_ip_entry(rp+8, space+1);
                                }
                            }
                            continue;
                        }
#endif /* USE_IPV6 */
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

                        rest = msglen - 8;
                        if ((uint32)handle < MAX_PENDING_ERQ
                         && pending_erq[handle].fun.type != T_INVALID
                         && (   rest <= (mp_int)max_array_size
                             || !max_array_size
                             || pending_erq[handle].string_arg
                            )
                           )
                        {
                            svalue_t *erqp = &pending_erq[handle].fun;
                            object_t *ob;
                            wiz_list_t *user;
                            int num_arg;

                            command_giver = 0;
                            current_interactive = 0;
                            ob = !CLOSURE_MALLOCED(erqp->x.closure_type)
                                 ? erqp->u.ob
                                 : erqp->u.lambda->ob;
                            if (pending_erq[handle].string_arg)
                            {
                                string_t * str;

                                str = new_n_mstring(rp + 8, rest, STRING_BYTES);
                                push_bytes(inter_sp, str);

                                num_arg = 1;
                            }
                            else
                            {
                                unsigned char *cp;
                                vector_t *v;
                                svalue_t *svp;

                                current_object = ob;
                                v = allocate_array(rest);
                                current_object = NULL;
                                push_array(inter_sp, v);
                                push_number(inter_sp, rest);
                                cp = (unsigned char *)rp + 8;
                                for (svp = v->item; --rest >=0; svp++)
                                {
                                    svp->u.number = *cp++;
                                }

                                num_arg = 2;
                            }

                            user = ob->user;
                            if (user->last_call_out != current_time)
                            {
                                user->last_call_out = current_time;
                                CLEAR_EVAL_COST;
                            } else {
                                assigned_eval_cost = eval_cost = user->call_out_cost;
                            }
                            RESET_LIMITS;
                            secure_callback_lambda(erqp, num_arg);
                            user->call_out_cost = eval_cost;
                            if (!keep_handle || (ob->flags & O_DESTRUCTED))
                            {
                                free_svalue(erqp);
                                erqp->type = T_INVALID;
                                erqp->u.generic = (void *)free_erq;
                                free_erq = &pending_erq[handle];
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
                        memmove(buf_from_erq, rp, (size_t)l);
                        input_from_erq = &buf_from_erq[l];
                    }
                } /* if (read data from erq) */
            } /* if (erq socket ready) */

#endif /* ERQ_DEMON */

            /* --- Try to get a new player --- */
            for (i = 0; i < numports; i++)
            {
                if (FD_ISSET(sos[i], &readfds))
                {
                    SOCKET_T new_socket;

                    length = sizeof addr;
                    new_socket = accept(sos[i], (struct sockaddr *)&addr
                                              , &length);
                    if ((int)new_socket != -1)
                        new_player( NULL, new_socket, &addr, (size_t)length
                                  , port_numbers[i]);
                    else if ((int)new_socket == -1
                      && errno != EWOULDBLOCK && errno != EINTR
                      && errno != EAGAIN && errno != EPROTO )
                    {
                        /* EBADF would be a valid cause for an abort,
                         * same goes for ENOTSOCK, EOPNOTSUPP, EFAULT.
                         * However, don't abort() because that tends to
                         * leave Mud admins baffled (and would opens the
                         * door for DoS attacks).
                         */
                        int errorno = errno;
                        fprintf( stderr
                               , "%s comm: Can't accept on socket %d "
                                 "(port %d): %s\n"
                               , time_stamp(), sos[i], port_numbers[i]
                               , strerror(errorno)
                               );
                        debug_message("%s comm: Can't accept on socket %d "
                                      "(port %d): %s\n"
                                     , time_stamp(), sos[i], port_numbers[i]
                                     , strerror(errorno)
                                     );
                        /* TODO: Was: perror(); abort(); */
                    }
                }
            } /* for */
            /* check for alarm signal (heart beat) */
            if (comm_time_to_call_heart_beat)
            {
                time_to_call_heart_beat = MY_TRUE;
                return MY_FALSE;
            }
            if (comm_return_to_backend)
                return MY_FALSE;
        } /* if (no NextCmdGiver) */

        /* See if we got any udp messages.
         * We don't test readfds so that we can accept udp messages with
         * short latency. But for the same reason, it was necessary to
         * include the descriptor number in the set to be selected on.
         * Note for Cygwin: since making sockets non-blocking
         *   is a bit tricky, we check if the socket is actually ready,
         *   to prevent freezing.
         * TODO: Always use the readfds information.
         */
#if !defined(CYGWIN)
        if (udp_s >= 0)
#else
        if (udp_s >= 0 && FD_ISSET(udp_s, &readfds))
#endif
        {
            char *ipaddr_str;
            int cnt;

            length = sizeof addr;
            cnt = recvfrom(udp_s, udp_buf, sizeof(udp_buf)-1, 0
                          , (struct sockaddr *)&addr, &length);
            if (cnt != -1)
            {
                string_t *udp_data;

                udp_data = new_n_mstring(udp_buf, cnt, STRING_BYTES);
                if (!udp_data)
                {
                    debug_message("%s Out of memory (%d bytes) for UDP message.\n"
                                 , time_stamp(), cnt);
                }
                else
                {
                    command_giver = NULL;
                    current_interactive = NULL;
                    current_object = NULL;
                    trace_level = 0;
#ifndef USE_IPV6
                    ipaddr_str = inet_ntoa(addr.sin_addr);
#else
                    ipaddr_str = inet6_ntoa(addr.sin_addr);
#endif
                    push_c_string(inter_sp, ipaddr_str);
                    push_bytes(inter_sp, udp_data); /* adopts the ref */
                    push_number(inter_sp, ntohs(addr.sin_port));
                    RESET_LIMITS;
                    callback_master(STR_RECEIVE_UDP, 3);
                    CLEAR_EVAL_COST;
                }
            }
        } /* if (upd_s) */

        /* --- The Scan for User Commands --- */

        for (; NextCmdGiver >= 0; DecreaseCmdGiver)
        {
            object_t *snooper;

            ip = all_players[NextCmdGiver];

            if (ip == 0)
                continue;

#ifdef USE_TLS
            /* Special case for setting up a TLS connection: don't
             * attempt IO if the connection is still being set up.
             */
            if (ip->tls_status == TLS_HANDSHAKING)
            {
                tls_continue_handshake(ip);
                continue;
            }
#endif

            if (FD_ISSET(ip->socket, &writefds))
            {
                comm_write_pending(ip);
            }
            /* Skip players which have reached the ip->maxNumCmds limit
             * for this second. We let the data accumulate on the socket.
             */
            if (ip->last_time == current_time
             && ip->maxNumCmds >= 0
             && ip->numCmds >= ip->maxNumCmds)
                continue;

            /* Get the data (if any), at max enough to fill .text[] */

            if (FD_ISSET(ip->socket, &readfds))
            {
                int l;

                /* Normally destructed objects will be removed before calling
                 * get_message(), but wenn accepting new connections we may
                 * well have a newly created & destructed object here.
                 */
                if (ip->ob->flags & O_DESTRUCTED)
                {
                    /* If this object is destructed, discard the input and skip to the next one. */
                    char buf[MAX_TEXT];
#ifdef USE_TLS
                    if (ip->tls_status != TLS_INACTIVE)
                        tls_read(ip, buf, MAX_TEXT);
                    else
#endif
                        socket_read(ip->socket, buf, MAX_TEXT);

                    continue;
                }

                l = MAX_TEXT - ip->text_end;

                /* In CHARMODE with combine-charset, the driver gets
                 * confused when receiving MAX_TEXT or more combinable
                 * characters.
                 * I couldn't quite figure out what and why, but
                 * reading less than MAX_TEXT characters works around
                 * the issue.
                 */
#ifndef SIMULATE_CHARMODE
                if ((ip->noecho & (CHARMODE_REQ|CHARMODE)) == (CHARMODE_REQ|CHARMODE))
#else
                if (ip->noecho & (CHARMODE_REQ|CHARMODE))
#endif
                {
                    l -= 2;
                }

                DTN(("text_end %hd, can read %d chars\n", ip->text_end, l));

#ifdef USE_TLS
                if (ip->tls_status != TLS_INACTIVE)
                    l = tls_read(ip, ip->text + ip->text_end, (size_t)l);
                else
#endif
                    l = socket_read(ip->socket, ip->text + ip->text_end, (size_t)l);
                DTN(("# chars read: %d\n", l));
                if (l == -1)
                {
                    switch (errno)
                    {
                        case EAGAIN:
                            // There was no data for available for immediate read.
                            // This should not happen for plain TCP connections, but
                            // may for TLS connections.
#ifdef USE_TLS
                            if (ip->tls_status == TLS_INACTIVE)
#endif
                                debug_message("%s Got unexpected EAGAIN upon socket read. Retrying later.\n",
                                              time_stamp());
                            // Fall-through
                        case EINTR:
                            // read was interrupted by a signal. Ignore and retry later again.
                            continue;
                        default:
                            // we regard other errors as non-recoverable.
                            debug_message("%s Error (%d) upon reading socket %d (ip %p '%s'), closing connection: %s\n"
                                          , time_stamp(), errno, ip->socket, ip,
                                          (ip->ob ? get_txt(ip->ob->name) : "<no name>"),
                                          strerror(errno));
                            remove_interactive(ip->ob, MY_FALSE);
                            continue;
                    }
                }
                if (l == 0) // TODO: is this really such a severe error?
                {
                    if (ip->closing)
                        comm_fatal(ip, "Tried to read from closing socket.\n");
                        /* This will forcefully disconnect the user */
                    else
                        remove_interactive(ip->ob, MY_FALSE);
                    continue;
                }
#ifdef COMM_STAT
                inet_packets_in++;
                inet_volume_in += l;
#endif

                ip->text_end += l;

                /* Here would be the place to send data through an
                 * outportal instead of returning it.
                 */

                /* Process telnet negotiations up to the point where the
                 * next input is ready, if possible.
                 */
                telnet_neg(ip);
            } /* if (cmdgiver socket ready) */

            /* telnet_neg() might have destroyed this one. */
            if (ip->ob->flags & O_DESTRUCTED)
                continue;

            /* ----- CHARMODE -----
             * command_start is 0 at the beginning.
             * Received chars start at command[0]. After the first character
             * is processed, command_start will be 1. Chars are in command[1]
             * then. Only after a full_newline is command_start reset to 0.
             * This is important for bang-escape, the first char in a 'line'
             * is stored in command[0], subsequent chars are in command[1..].
             *
             * command_end points beyond the last character in the text buffer.
             * If the user is slow this will be 1. If the user pastes data it
             * could be more. The chars are processed then one at a time (or
             * if combine-charset is used that many until a non-combinable
             * char is reached).
             *
             * The processed char(s) are copied to buff and handled in the
             * backend.
             *
#ifndef SIMULATE_CHARMODE
             * TODO: I dont think that it is neccesary to disable charmode if
             * TODO:: the client refuses to use it. The disadvantage of the
             * TODO:: present behaviour is a confused lpc object (which could
             * TODO:: not know if it gets linemode-lines). The charmode code
             * TODO:: does work with clients in linemode.
#endif
             */

#ifndef SIMULATE_CHARMODE
            if ((ip->noecho & (CHARMODE_REQ|CHARMODE)) == (CHARMODE_REQ|CHARMODE))
#else
            if (ip->noecho & (CHARMODE_REQ|CHARMODE))
#endif
            {
                DTN(("CHARMODE_REQ\n"));

                /* if ip->command[0] does not hold a valid character, the
                 * outcome of the comparison to input_escape does not matter.
                 */
                if (ip->command[0] != input_escape
                 || find_no_bang(ip) & IGNORE_BANG )
                {
                    /* Unescaped input.
                     * Puts the next character(s) (addressed by
                     * .command_start) into buff[0] and return the data.
                     */

                    size_t destix;  /* Save index */
                    size_t cmdlength = ip->command_end - ip->command_start;

                    if (!cmdlength)
                        continue;

                    DTN(("  Unescaped input\n"));

                    /* Copy as many characters from the text[] into
                     * the buff[] as possible.
                     */
                    DTN(("  %zd chars ready\n", cmdlength));
                    for (destix = 0; destix < cmdlength; )
                    {
                        p_int ch;
                        size_t off = utf8_to_unicode(ip->command + ip->command_start + destix, cmdlength - destix, &ch);

                        if(!off)
                            break;

                        if (ch & ~0x7f || !(ip->combine_cset[(ch&0x78) >> 3] & 1<<(ch&7)))
                        {
                            /* This character can't be combined.
                             * If it is not the first character encountered,
                             * take only this character. In either case break
                             * the loop.
                             */
                            if (!destix)
                                destix += off;
                            break;
                        }

                        destix++;
                    }

                    if (!destix)
                        continue;

                    memcpy(buff, ip->command + ip->command_start, destix);
                    *bufflength = destix;
                    ip->command_start += destix;

                    /* destix is now the number of characters stored in
                     * buff[], and is at least 1.
                     */

                    if (ip->command_start == ip->command_end)
                    {
                        /* Out of characters to process.
                         * Continue the telnet machine.
                         */
                        DTN(("    out of chars: reinit telnet machine\n"));
                        telnet_neg_reset(ip);
                    }

                    command_giver = ip->ob;
                    trace_level = ip->trace_level;
                    DecreaseCmdGiver;
                    CmdsGiven = 0;

                    if (ip->last_time != current_time)
                    {
                        ip->last_time = current_time;
                        ip->numCmds = 0;
                    }
                    else
                        ip->numCmds++;

                    DTN(("--- return with char command %02x '%c' length %d ---\n", buff[0], buff[0], destix));

                    return MY_TRUE;
                }
                else if (ip->tn_state != TS_READY)
                {
                    DT(("'%s'   Escaped input\n", get_txt(ip->ob->name)));
                    DTN(("  data length %zd\n", ip->command_end - ip->command_start));

                    if (ip->command_printed < ip->command_end)
                    {
                        comm_socket_write(ip->command + ip->command_printed
                                        , (size_t)(ip->command_end - ip->command_printed)
                                        , ip, 0);
                        ip->command_printed = ip->command_end;
                    }
                }
            } /* if (CHARMODE_REQ) */

            /* The telnet negotiation produces the commands starting at
             * the beginning of .text[] and terminated with a '\0'. Whenever
             * a command is complete, the tn_state is TS_READY.
             */
            DTN(("tn complete, telnet machine state: %hhd\n", ip->tn_state));
            if (ip->tn_state == TS_READY)
            {
                /* We have a command: copy it into buff, handle a
                 * possible snooper and return.
                 */
                *bufflength = ip->command_end - ip->command_start;

                DTN(("telnet machine ready\n"));
                /* buffer overflows here are impossible even with strcpy(),
                 * because buff is allocated in backend() as MAX_TEXT+4 and
                 * ip->command is allocated as MAX_TEXT. Ok, as long as nobody
                 * changes buff in backend() withour changing ip->text ... */
                memcpy(buff, ip->command, *bufflength);
                command_giver = ip->ob;
                trace_level = ip->trace_level;

                /* Reinitialize the telnet machine, possibly already
                 * producing the next command in .text[].
                 */
                telnet_neg_reset(ip);

                /* telnet_neg() might have destroyed this one. */
                if (ip->ob->flags & O_DESTRUCTED)
                    continue;

                /* If the user is not in ed, don't let him issue another command
                 * before the poll comes again.
                 */
                if (ip->input_handler
                 && ip->input_handler->type == INPUT_ED
                 && CmdsGiven < ALLOWED_ED_CMDS)
                {
                    CmdsGiven++;
                    FD_CLR(ip->socket, &readfds);
                }
                else
                {
                    DecreaseCmdGiver;
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
                    if (O_IS_INTERACTIVE(snooper))
                    {
                        command_giver = snooper;
                        add_message("%% %.*s\n", (int)*bufflength, buff);
                    }
                    else
                    {
                        /* buff is limited to MAX_TEXT+4. Additionally,
                         * get_message() is usually not called recursively or
                         * from a very high stack depth, so alloca() is 
                         * probably ok. */
                        char *snoop_message = alloca(strlen(buff) + 4);
                        sprintf(snoop_message, "%% %.*s\n", (int)*bufflength, buff);
                        tell_npc_str(snooper, snoop_message);
                    }
                    command_giver = ip->ob;
                }

                if (ip->last_time != current_time)
                {
                    ip->last_time = current_time;
                    ip->numCmds = 0;
                }
                else
                    ip->numCmds++;

#ifndef SIMULATE_CHARMODE
                if ((ip->noecho & (CHARMODE_REQ|CHARMODE)) == CHARMODE_REQ)
                {
                    DTN(("   clear CHARMODE as it was refused anyway\n"));
                    ip->noecho &= ~(CHARMODE_REQ|CHARMODE|CHARMODE_ACK);
                    reset_input_buffer(ip);
                }
#endif /* SIMULATE_CHARMODE */

                DTN(("--- return with line command ---\n"));
                DTN(("--- '%.*s'\n", (int)*bufflength, buff));
                return MY_TRUE;
            } /* if (have a command) */

        } /* for (NextCmdGiver) */

        /* If we got here, we couldn't find any commands:
         * loop and select (on timeout) again.
         */

    } /* while(forever) */

    /* NOTREACHED */
#   undef StartCmdGiver
#   undef DecreaseCmdGiver

} /* get_message() */

/*-------------------------------------------------------------------------*/
void
remove_interactive (object_t *ob, Bool force)

/* Remove the interactive user <ob> immediately.
 * If <force> is true, the user is removed under all circumstances and
 * without even flushing the outgoing buffer.
 * This function should not be called from within a LPC command execution.
 */

{
    object_t *save = command_giver;
    int i;
    interactive_t *interactive;
    object_t * curobj_save = current_object;
    int save_privilege;

    interactive = O_GET_INTERACTIVE(ob);

    /* Proper call? */
    for (i = 0; i < MAX_PLAYERS && all_players[i] != interactive; i++) NOOP;
    if (i >= MAX_PLAYERS)
    {
        fatal("Could not find and remove player %s\n", get_txt(ob->name));
        abort();
    }
    if (interactive->closing && !force)
        fatal("Double call to remove_interactive()\n");

    interactive->closing = MY_TRUE;
    current_object = ob;

    /* If the object is not destructed, save any ed buffers. */

    if ( !(ob->flags & O_DESTRUCTED) )
    {
        command_giver = ob;
        abort_input_handler(interactive);
    }

    save_privilege = malloc_privilege;

    /* If the object is still not destructed, inform the master */

    if ( !(ob->flags & O_DESTRUCTED) )
    {
        int numRemaining = interactive->command_unprocessed_end - interactive->command_start;

        command_giver = NULL;
        current_interactive = NULL;
        push_ref_object(inter_sp, ob, "remove_interactive");

        if (numRemaining > 0)
        {
            string_t * remaining = NULL;
            memsafe( remaining = new_n_unicode_mstring(interactive->command+interactive->command_start, numRemaining)
                   , numRemaining, "buffer for remaining data from socket");
            push_string(inter_sp, remaining);
        }
        else
            push_ref_string(inter_sp, STR_EMPTY);

        malloc_privilege = MALLOC_MASTER;
        callback_master(STR_DISCONNECT, 2);
        /* master might have used exec() */
        ob = interactive->ob;
    }

    interactive->catch_tell_activ = MY_FALSE;

    /* Untie eventual snooping relations */

    if (interactive->snoop_by)
    {
        if (O_IS_INTERACTIVE(interactive->snoop_by))
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
     && interactive->socket == erq_proto_demon
     && !force)
    {
        static unsigned char erq_welcome[] = { IAC, TELOPT_BINARY };

        add_message_flush();
        remove_flush_entry(interactive); /* To be sure */

        erq_demon = interactive->socket;
        erq_proto_demon = -1;
        socket_write(erq_demon, erq_welcome, sizeof erq_welcome);
    }
    else
#endif
    {
        if (!force)
        {
            /* Say goodbye to the user. */
            trace_level |= interactive->trace_level;
            add_message_flush();
        }

        remove_flush_entry(interactive); /* To be sure */

#ifdef USE_MCCP
        if (interactive->out_compress)
            end_compress(interactive, MY_TRUE);
            /* Always force the compression end as we won't get
             * a second chance at it.
             */
#endif

        /* If there is anything left, try now. */
        comm_write_pending(interactive);

#ifdef USE_TLS
        tls_deinit_connection(interactive);
#endif
        shutdown(interactive->socket, 2);
        socket_close(interactive->socket);
    } /* if (erq or user) */

    release_host_access(interactive->access_class);
      /* One user less in this class */

    num_player--;

    /* Release all associated resources */

    while (interactive->input_handler)
    {
        input_t * ih = interactive->input_handler;

        interactive->input_handler = ih->next;
        free_input_handler(ih);
    }

    if (interactive->modify_command)
    {
        free_object(interactive->modify_command, "remove_interactive");
    }

#ifdef USE_MCCP
    if (interactive->out_compress_buf)
       xfree(interactive->out_compress_buf);
    if (interactive->out_compress)
       xfree(interactive->out_compress);
#endif
    free_svalue(&interactive->prompt);

    if (interactive->trace_prefix)
        free_mstring(interactive->trace_prefix);

    while (interactive->write_first)
    {
        struct write_buffer_s *tmp = interactive->write_first;
        interactive->write_first = tmp->next;
        xfree(tmp);
    }

    if (iconv_valid(interactive->receive_cd))
        iconv_close(interactive->receive_cd);
    if (iconv_valid(interactive->send_cd))
        iconv_close(interactive->send_cd);

    /* Unlink the interactive structure from the shadow sentence
     * of the object.
     */
    O_GET_INTERACTIVE(ob) = NULL;
    check_shadow_sent(ob);

    xfree(interactive);
    if (i < MAX_PLAYERS)
        all_players[i] = NULL;
    while (max_player && !all_players[max_player])
        max_player--;
    free_object(ob, "remove_interactive");

    command_giver = check_object(save);
    current_object = curobj_save;
    malloc_privilege = save_privilege;
} /* remove_interactive() */

/*-------------------------------------------------------------------------*/
void
refresh_access_data(void (*add_entry)(struct sockaddr_in *, int, long*) )

/* Called from access_check after the ACCESS_FILE has been (re)read, this
 * function has to call the passed callback function add_entry for every
 * user currently logged in.
 */

{
    interactive_t **user, *this;
    int n;

    user = all_players;
    for (n = max_player + 2; --n; user++)
    {
        this = *user;
        if (this)
        {
            struct sockaddr_in addr;
            int port;
            length_t length;

            length = sizeof(addr);
            getsockname(this->socket, (struct sockaddr *)&addr, &length);
            port = ntohs(addr.sin_port);
            (*add_entry)(&this->addr, port, &this->access_class);
        }
    }
}

/*-------------------------------------------------------------------------*/
static INLINE void
set_default_conn_charset (char charset[32])

/* Set the default connection charset bitmask in <charset>.
 */

{
    memset(charset, 255, 16);
    charset['\0'/8] &= ~(1 << '\0' % 8);
} /* set_default_conn_charset() */

/*-------------------------------------------------------------------------*/
static INLINE void
set_default_combine_charset (char charset[32])

/* Set the default combine charset bitmask in <charset>.
 */

{
    memset(charset, 0, 16);
    charset['\n'/8] &= ~(1 << '\n' % 8);
    charset['\0'/8] &= ~(1 << '\0' % 8);
} /* set_default_combine_charset() */

/*-------------------------------------------------------------------------*/
static bool
set_encoding (interactive_t *ip, const char* encoding)

/* Sets the encoding for the interactive <ip> to <encoding>.
 * Returns true upon success, false otherwise (errno is set accordingly).
 */
{
    iconv_t receiving, sending;
    char base_encoding[sizeof(default_player_encoding)];
    const char *end = encoding;

    assert(strlen(encoding) < sizeof(base_encoding));

    /* Find a double slash and remove it for
     * the receiving encoding.
     */
    while ((end = strchr(end, '/')))
    {
        if (end[1] == '/')
            break;
        else
            end++;
    }

    if (end != NULL)
    {
        memcpy(base_encoding, encoding, end - encoding);
        base_encoding[end - encoding] = '\0';

        receiving = iconv_open("UTF-8", base_encoding);
    }
    else
        receiving = iconv_open("UTF-8", encoding);

    if (!iconv_valid(receiving))
        return false;

    sending = iconv_open(encoding, "UTF-8");
    if (!iconv_valid(sending))
    {
        iconv_close(receiving);
        return false;
    }

    strcpy(ip->encoding, encoding);

    if (iconv_valid(ip->receive_cd))
    {
        iconv_close(ip->receive_cd);
    }
    if (iconv_valid(ip->send_cd))
    {
        iconv_close(ip->send_cd);
    }

    ip->receive_cd = receiving;
    ip->send_cd = sending;

    return true;
} /* set_encoding() */

/*-------------------------------------------------------------------------*/
static void
new_player ( object_t *ob, SOCKET_T new_socket
           , struct sockaddr_in *addr, size_t addrlen
           , int login_port
           )

/* Accept (or reject) a new connection on <new_socket> from <addr> (length
 * of structure is <addrlen>), accepted on port <login_port>.
 *
 * Called when get_message() detects a new connection on one of the
 * login ports, this function checks if the user may access the mud.
 *
 * If yes and <ob> is NULL, a new interactive structure is generated and
 * bound to the master, then master->connect() is called. This call is
 * expected to return an object and the interactive structure is rebound to
 * that object.
 * If yes and <ob> is an object, a new interactive structure is generated
 * and bound to <ob>
 *
 * Finally, logon() is called in the newly-interactive object.
 * Alternatively if no <ob> is given, master->connect() may exec() the
 * connection away from the master, in which case no further action will be
 * taken after the return from that call.
 *
 * If the connection can't be accepted for some reason, a failure
 * message will be send back to the user and the socket will be
 * closed.
 */

{
    int   i;             /* Index of free slot in all_players[] */
    char *message;       /* Failure message */
    svalue_t *ret;       /* LPC call results */
    interactive_t *new_interactive;
                         /* The new interactive structure */
    long class;     /* Access class */

    /* Set some useful socket options */
    set_socket_nonblocking(new_socket);
    set_close_on_exec(new_socket);
    set_socket_own(new_socket);
    set_socket_nosigpipe(new_socket);
    
    /* Check for access restrictions for this connection */
    message = allow_host_access(addr, login_port, &class);

    if (access_log != NULL)
    {
        FILE *log_file = fopen (access_log, "a");

        if (log_file) {
            FCOUNT_WRITE(log_file);
            fprintf(log_file, "%s %s: %s\n"
                   , time_stamp()
#ifndef USE_IPV6
                   , inet_ntoa(addr->sin_addr)
#else
                   , inet6_ntoa(addr->sin_addr)
#endif
                   , message ? "denied" : "granted");
            fclose(log_file);
        }
    }

    if (message)
    {
        socket_write(new_socket, message, strlen(message));
        socket_write(new_socket, "\r\n", 2);
        socket_close(new_socket);
        return;
    }

    if (d_flag)
        debug_message("%s New player at socket %d.\n"
                     , time_stamp(), new_socket);

    /* Look for an empty slot in all_players[] */
    for (i = 0; i < MAX_PLAYERS && all_players[i] != NULL; i++) NOOP;
    if (i >= MAX_PLAYERS)
    {
        /* calling closures here would need special error handling */
        if (driver_hook[H_NO_IPC_SLOT].type == T_STRING)
        {
            string_t *msg;

            msg = driver_hook[H_NO_IPC_SLOT].u.str;
            socket_write(new_socket, get_txt(msg), mstrsize(msg));
        }
        else
        {
            message = "The mud is full. Come back later.\r\n";
            socket_write(new_socket, message, strlen(message));
        }
        socket_close(new_socket);
        debug_message("%s Out of IPC slots for new connection.\n"
                     , time_stamp());
        return;
    }

    /* The master must be loaded and free to accept a login */
    assert_master_ob_loaded();
    if (O_IS_INTERACTIVE(master_ob))
    {
        message = "Cannot accept connections. Come back later.\r\n";
        socket_write(new_socket, message, strlen(message));
        socket_close(new_socket);
        debug_message("%s Master still busy with previous new connection.\n"
                     , time_stamp());
        return;
    }

    command_giver = master_ob;
    trace_level = 0;
    new_interactive = xalloc(sizeof (interactive_t));
    if (!new_interactive)
    {
        message = "Cannot accept connection (out of memory). Come back later.\r\n";
        socket_write(new_socket, message, strlen(message));
        socket_close(new_socket);
        debug_message("%s Out of memory (%zu bytes) for new connection.\n"
                     , time_stamp(), sizeof(interactive_t));
        return;
    }

    if (ob && O_IS_INTERACTIVE(ob))
    {
        /* The caller provided an object to connect to. But since
         * it is already interactive, we have to terminate that
         * old connection.
         */
        remove_interactive(ob, MY_FALSE);
    }

    /* Link the interactive to the master */

    assert_shadow_sent(master_ob);
    O_GET_INTERACTIVE(master_ob) = new_interactive;
    master_ob->flags |= O_ONCE_INTERACTIVE;
    new_interactive->ob = ref_object(master_ob, "new_player");
    new_interactive->outgoing_conn = (ob != NULL);

    /* Initialize the rest of the interactive structure */

#ifdef USE_MCCP
    new_interactive->compressing = 0;
    new_interactive->out_compress = NULL;
    new_interactive->out_compress_buf=NULL;
#endif
#ifdef USE_TLS
    new_interactive->tls_status = TLS_INACTIVE;
    new_interactive->tls_session = NULL;
    new_interactive->tls_cb = NULL;
#endif
    new_interactive->input_handler = NULL;
    put_number(&new_interactive->prompt, 0);
    new_interactive->modify_command = NULL;
    new_interactive->closing = MY_FALSE;
    new_interactive->tn_enabled = MY_TRUE;
    new_interactive->do_close = 0;
    new_interactive->noecho = 0;
    new_interactive->supress_go_ahead = MY_FALSE;
    new_interactive->gobble_char = 0;
    new_interactive->catch_tell_activ = MY_TRUE;
    new_interactive->text_prefix = 0;
    new_interactive->tn_start = 0;
    new_interactive->tn_end = 0;
    new_interactive->text_end = 0;
    new_interactive->command_start = 0;
    new_interactive->command_printed = 0;
    new_interactive->command_end = 0;
    new_interactive->command_unprocessed_end = 0;
    new_interactive->tn_state = TS_DATA;
    new_interactive->syncing = false;
    new_interactive->snoop_on = NULL;
    new_interactive->snoop_by = NULL;
    new_interactive->last_time = current_time;
    new_interactive->numCmds = 0;
    new_interactive->maxNumCmds = -1;
    new_interactive->trace_level = 0;
    new_interactive->trace_prefix = NULL;
    new_interactive->message_length = 0;
    new_interactive->quote_iac = MY_TRUE;
    set_default_conn_charset(new_interactive->charset);
    set_default_combine_charset(new_interactive->combine_cset);
    new_interactive->command[0] = '\0';
    memcpy(&new_interactive->addr, addr, addrlen);
    new_interactive->access_class = class;
    new_interactive->socket = new_socket;
    new_interactive->next_player_for_flush = NULL;
    new_interactive->previous_player_for_flush = NULL;
    new_interactive->msg_discarded = DM_NONE;

    new_interactive->write_first = new_interactive->write_last = NULL;
    new_interactive->write_size = 0;
    new_interactive->write_max_size = -2;

    new_interactive->receive_cd = iconv_init();
    new_interactive->send_cd = iconv_init();
    if (!set_encoding(new_interactive, default_player_encoding))
    {
        if (errno == EINVAL)
            debug_message("%s Error setting up initial encoding: unsupported encoding '%s'.\n", time_stamp(), default_player_encoding);
        else
            debug_message("%s Error setting up initial encoding: %s.\n", time_stamp(), strerror(errno));

        message = "Error setting up encoding.\r\n";
        socket_write(new_socket, message, strlen(message));
        socket_close(new_socket);

        O_GET_INTERACTIVE(master_ob) = NULL;
        xfree(new_interactive);
        free_object(master_ob, "new_player");

        return;
    }

    /* Add the new interactive structure to the list of users */

    all_players[i] = new_interactive;
    if (i > max_player)
        max_player = i;
    num_player++;

    current_interactive = master_ob;

    if (!ob)
    {
        /* The caller did not provide an object to connect to.
         * Call master->connect() and evaluate the result.
         */
        ret = callback_master(STR_CONNECT, 0);
        if (new_interactive != O_GET_INTERACTIVE(master_ob))
            return;
        if (ret == NULL
         || ret->type != T_OBJECT
         || (ob = ret->u.ob, O_IS_INTERACTIVE(ob)))
        {
            remove_interactive(master_ob, MY_FALSE);
            return;
        }
        command_giver = master_ob;
        add_message_flush();
    }

    /* ob is now a non-interactive object, either passed in from the caller
     * or returned from connect().
     * Relink the interactive from the master to this as the user object.
     */

    O_GET_INTERACTIVE(master_ob) = NULL;
    master_ob->flags &= ~O_ONCE_INTERACTIVE;
    check_shadow_sent(master_ob);
    free_object(master_ob, "new_player");

    assert_shadow_sent(ob);
    O_GET_INTERACTIVE(ob) = new_interactive;
    new_interactive->ob = ref_object(ob, "new_player");
    ob->flags |= O_ONCE_INTERACTIVE;

    /* Prepare to call logon() in the new user object.
     */
    command_giver = ob;
    current_interactive = ob;
    if (new_interactive->snoop_on)
    {
        new_interactive->snoop_on->snoop_by = ob;
    }
#ifdef ERQ_DEMON
    (void) lookup_ip_entry(new_interactive->addr.sin_addr, MY_TRUE);
    /* TODO: We could pass the retrieved hostname right to login */
#endif
#ifdef USE_TLS
    /* If we're using secure connections and the connect() triggered
     * a handshake which is still going on, we call logon() as
     * the default TLS callback. This way, logon() is callled only
     * if the connection could be established, secure or not.
     */
    if (new_interactive->tls_status != TLS_HANDSHAKING)
    {
        /* Connection not secure, or already established: logon. */
        logon_object(ob, 0);
    }
    else if (new_interactive->tls_cb == NULL)
    {
        callback_t * cb;

        /* Connection in TLS handshake, but not callback: set a callback
         * to the logon function.
         */
        if (find_function(STR_LOGON, current_interactive->prog) < 0)
        {
            errorf("Could not find %s() on the player %s\n", get_txt(STR_LOGON), get_txt(current_interactive->name));
            /* NOTREACHED */
        }

        xallocate(cb, sizeof(*cb), "logon tls-callback structure");
        setup_function_callback(cb, current_interactive, STR_LOGON, 0, NULL);
        new_interactive->tls_cb = cb;

    }
    /* else: Connection in TLS handshake and callback set by connect(). */
#else
    logon_object(ob, 0);
#endif /* USE_TLS */
    if (!(ob->flags & O_DESTRUCTED))
        print_prompt();
    flush_all_player_mess();
} /* new_player() */

/*-------------------------------------------------------------------------*/
void
set_noecho (interactive_t *ip, char noecho, Bool local_change, Bool external)

/* Change the input mode <i>->noecho to the given <noecho>, performing all
 * necessary telnet negotiations. If the driverhook H_NOECHO is set,
 * the hook function is expected to do all the negotiations.
 * If <local_change> is TRUE, the driver will not send out any telnet
 * commands for CHARMODE/LINEMODE changes.
 */

{
    char old, confirm;
    object_t *ob;

    if (!ip->tn_enabled)
    {
        DTN(("set_noecho(%02hhx) old %02hhx: TN disabled\n"
            , noecho, ip->noecho));
    }

    old = ip->noecho;

    confirm = (char)(
      noecho | CHARMODE_REQ_TO_CHARMODE(noecho & (NOECHO_REQ|CHARMODE_REQ)));
    DTN(("set_noecho(%02hhx%s) old %02hhx %s\n"
       , noecho, local_change ? " local" : "", old, decode_noecho(old)));
    DTN(("  -> confirm: %02hhx %s\n"
       , confirm, decode_noecho(confirm)));
    DTN(("           -> %02hhx %s\n"
       , confirm | NOECHO_ACKSHIFT(confirm & (NOECHO|CHARMODE))
       , decode_noecho(confirm | NOECHO_ACKSHIFT(confirm & (NOECHO|CHARMODE)))
       ));

    ip->noecho = confirm;

    confirm |= NOECHO_ACKSHIFT(confirm & (NOECHO|CHARMODE));
    if (((confirm ^ old) & (NOECHO_MASK|CHARMODE_MASK)) && ip->tn_enabled )
    {
        DTN(("set_noecho(): Mode changes\n"));
        ob = ip->ob;
        if (!(ob->flags & O_DESTRUCTED))
        {
            if (driver_hook[H_NOECHO].type == T_STRING
             || driver_hook[H_NOECHO].type == T_CLOSURE
               )
            {
                DTN(("set_noecho():   calling H_NOECHO\n"));
                push_number(inter_sp, noecho);
                push_ref_valid_object(inter_sp, ob, "set_no_echo");
                push_number(inter_sp,  local_change ? 1 : 0);
                if (driver_hook[H_NOECHO].type == T_STRING)
                    secure_apply_ob(driver_hook[H_NOECHO].u.str, ob, 3, external);
                else 
                {
                    if (driver_hook[H_NOECHO].x.closure_type == CLOSURE_LAMBDA)
                    {
                        free_object(driver_hook[H_NOECHO].u.lambda->ob
                                   , "set_noecho");
                        driver_hook[H_NOECHO].u.lambda->ob
                          = ref_object(ob, "set_noecho");
                    }
                    secure_call_lambda(&driver_hook[H_NOECHO], 3, external);
                }
                if (~confirm & old & CHARMODE_MASK)
                {
                    reset_input_buffer(ip);
                }
            }
            else
            {
                object_t *save;

                save = command_giver;
                command_giver = ob;
#ifdef SAVE_NOECHO
                ip->noecho &= ~NOECHO_DELAYED;
#endif
                if (~confirm & old & NOECHO)
                {
                    DTN(("set_noecho():   WONT TELOPT_ECHO\n"));
                    send_wont(TELOPT_ECHO);
                }
                else if (confirm & ~old & NOECHO_MASK)
                {
#ifdef SAVE_NOECHO
                    if (confirm & ~old & CHARMODE_MASK)
                    {
                        ip->noecho |= NOECHO_DELAYED;
                        ip->noecho &= ~(NOECHO | NOECHO_REQ);
                        DTN(("set_noecho():   delaying WILL TELOPT_ECHO\n"));
                    }
                    else
                    {
#endif
                        DTN(("set_noecho():   WILL TELOPT_ECHO\n"));
                        send_will(TELOPT_ECHO);
#ifdef SAVE_NOECHO
                    }
#endif
                }
                else /* No change in NOECHO mode */ if (confirm & NOECHO)
                {
                    /* Since we stay in NOECHO mode, we need the ACK flag set. */
                    DTN(("set_noecho():   Staying in NOECHO mode\n"));
                    ip->noecho |= NOECHO_ACKSHIFT(NOECHO);
                }

                if (ip->supress_go_ahead && !(confirm & (NOECHO|CHARMODE)))
                {
                    DTN(("set_noecho():   WONT TELOPT_SGA\n"));
                    ip->supress_go_ahead = MY_FALSE;
                    send_wont(TELOPT_SGA);
                }
                /* Only using SGA for charmode is supported hardcoded.
                 * To make more sophisticated negotiations, e.g. using LINEMODE,
                 * use the H_NOECHO hook.
                 */
                if ((~confirm & old & CHARMODE_MASK)
                ||  ((~confirm & old & NOECHO_STALE) && (old & CHARMODE_MASK))
                   )
                {
                    if(~confirm & old & CHARMODE_MASK)
                    {
                        DTN(("set_noecho():   turn off charmode\n"));
                        if ((old & CHARMODE) && !local_change)
                        {
                            DTN(("set_noecho():     DONT TELOPT_SGA\n"));
                            send_dont(TELOPT_SGA);
                        }
                    }

                    reset_input_buffer(ip);
                }
                else if (confirm & ~old & CHARMODE_MASK)
                {
                    DTN(("set_noecho():   turn on charmode\n"));
                    if (!local_change)
                    {
                        DTN(("set_noecho():     DO+WILL TELOPT_SGA\n"));
                        send_do(TELOPT_SGA);
                        /* some telnet implementations (Windows' telnet is one) mix
                         * up DO and WILL SGA, thus we send WILL SGA as well.
                         */
                        send_will(TELOPT_SGA);
                        ip->supress_go_ahead = MY_TRUE;
                    }
                    else
                        /* Since there won't be any telnet negotiation
                         * we can assume that CHARMODE is acknowledged.
                         */
                        ip->noecho |= NOECHO_ACKSHIFT(CHARMODE);

                }
                else /* No change in CHARMODE mode */ if (confirm & CHARMODE)
                {
                    /* Since we stay in CHARMODE mode, we need the ACK flag set. */
                    DTN(("set_noecho():   Staying in CHARMODE mode\n"));
                    ip->noecho |= NOECHO_ACKSHIFT(CHARMODE);
                }

                command_giver = save;
            }
        } /* if (!(ob->flags & O_DESTRUCTED)) */
    }
    else
    {
        /* No change in modes.
         * However, if we stay in NOECHO/CHARMODE, we need to set
         * the ACK flags.
         */
        if (confirm & CHARMODE)
        {
            /* Since we stay in CHARMODE mode, we need the ACK flag set. */
            DTN(("set_noecho():   Staying in CHARMODE mode\n"));
            ip->noecho |= NOECHO_ACKSHIFT(CHARMODE);
        }
        if (confirm & NOECHO)
        {
            /* Since we stay in NOECHO mode, we need the ACK flag set. */
            DTN(("set_noecho():   Staying in NOECHO mode\n"));
            ip->noecho |= NOECHO_ACKSHIFT(NOECHO);
        }
    }

} /* set_noecho() */

/*-------------------------------------------------------------------------*/
int
find_no_bang (interactive_t *ip)

/* Find the most recent input_to in *<ip> which specified "IGNORE_BANG" and
 * return its full "noecho" flags. This may be the ip->noecho itself!
 * If there is none, return 0.
 */

{
    input_t *ih;

    if (ip->noecho & IGNORE_BANG)
        return ip->noecho;

    for (ih = ip->input_handler; ih; ih = ih->next)
        if (ih->noecho & IGNORE_BANG)
            return ih->noecho;
    return 0;
} /* find_no_bang() */

/*-------------------------------------------------------------------------*/
static Bool
call_input_to (interactive_t *i, char *str, size_t length, input_to_t *it)

/* Call the input_to handler <it> for this user <i> and the input <str>.
 * Return TRUE if this handler was executed successfully, and FALSE
 * if the input was not processed.
 *
 * This function is only called by call_function_interactive().
 */

{
    static input_to_t current_it;
      /* Current input_to, static so that longjmp() won't clobber it. */

    struct error_recovery_info error_recovery_info;
    object_t   *ob;   /* object holding <function> */


    /* We got the right input_to_t. Check if it's still valid. */
    ob = callback_object(&(it->fun));
    if (!ob)
    {
        /* Sorry, the object has selfdestructed ! */
        free_input_to(it);
        return MY_FALSE;
    }

    if (O_PROG_SWAPPED(ob)
     && load_ob_from_swap(ob) < 0)
    {
        free_input_to(it);
        errorf("Out of memory: unswap object '%s'.\n", get_txt(ob->name));
        return MY_FALSE;
    }

    /* Clear the input_to() reference in case the function called
     * sets up a new one.
     */
    current_it = *it;
    xfree(it);
    free_svalue(&current_it.input.prompt); /* Don't need this anymore */

    /* Activate the local error recovery context */

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_BACKEND;
    rt_context = (rt_context_t *)&error_recovery_info.rt;

    if (setjmp(error_recovery_info.con.text))
    {
        /* An error occurred: free the remaining data,
         * restore the error stack and return
         */

        clear_state();
        debug_message("%s Error in input_to().\n", time_stamp());
        free_callback(&(current_it.fun));
        rt_context = error_recovery_info.rt.last;
        return MY_TRUE;
    }

    /* Call the input_to() function with the newly input string */

    push_c_n_string(inter_sp, str, length);
    (void)backend_callback(&(current_it.fun), 1);

    rt_context = error_recovery_info.rt.last;

    /* Done */
    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
Bool
call_function_interactive (interactive_t *i, char *str, size_t length)

/* Execute a pending input handler for this user <i> and the input <str>
 * Return TRUE if an input_to() or ed() was pending and executed, and FALSE
 * if the input was not processed.
 *
 * This function is called by the backend as part of the input processing.
 */

{
    input_t    *ih;
    ih = i->input_handler;

    /* _Are_ there an input_to() pending? */
    if (!ih)
        return MY_FALSE;

    /* Yes, there are. Check if we have to handle input escape. */
    if (length > 1 && *str == input_escape)
    {
        input_t * prev;

        for (prev = NULL
            ; ih && !(ih->noecho & IGNORE_BANG)
            ; prev = ih, ih = ih->next)
            NOOP;

        if (ih)
        {
            /* Move this 'IGNORE_BANG' input_to to the top of list
             * since it's the one we're going to execute.
             */
            if (prev)
            {
                prev->next = ih->next;
                ih->next = i->input_handler;
                i->input_handler = ih;
            }

            if (!(i->noecho & NOECHO) != !(ih->noecho & NOECHO_REQ)) {
                /* !message for ECHO-context  while in NOECHO - simulate the
                 * echo by sending the (remaining) raw data we got.
                 */
                add_message("%s\n", str + i->command_printed);
                i->command_printed = i->command_end;
            }

            /* Don't hide the leading input escape */
        }
        else
        {
            /* Bang-input but no matching input_to(): return */
            return MY_FALSE;
        }
    }

    switch (ih->type)
    {
    case INPUT_TO:
        {
            Bool res;

            i->input_handler = ih->next;

            /* if there is a series of noecho/charmode input, we should only
             * negotiate when we know that the state actually should change.
             * In other words: should the input_to function request NOECHO
             * again, the NOECHO_STALE bit will be cleared and we will not
             * turn NOECHO off after the call.
             */
            if (i->noecho)
            {
                i->noecho |= NOECHO_STALE;
            }

            res = call_input_to(i, str, length, (input_to_t*) ih);

            /* If NOECHO is no longer needed, turn it off. */

            if (i->noecho & NOECHO_STALE)
            {
                set_noecho(i, i->input_handler ? i->input_handler->noecho : 0
                            , i->input_handler ? i->input_handler->local : MY_FALSE
                            , MY_TRUE);
            }

            return res;
        }

    case INPUT_ED:
        str[length] = 0;
        ed_cmd(str, ih);
        return MY_TRUE;
    }

    return MY_FALSE;

} /* call_function_interactive() */

/*-------------------------------------------------------------------------*/
static Bool
set_call ( object_t *ob, input_to_t *it, char noecho
         , Bool local_change, Bool append)

/* Set a a new input_to <it> with the flags <noecho> (mainly really NOECHO,
 * but also IGNORE_BANG or not) to the interactive object <ob>.
 * If <local_change> is TRUE, the driver will not send out any telnet
 * commands for CHARMODE/LINEMODE changes.
 * If <append> is TRUE, the call is appended to the list of existing
 * input_to's, if any.
 * Return TRUE on success.
 *
 * Called for efun input_to().
 */

{
    interactive_t *ip;

    if (ob == NULL || it == NULL)
        return MY_FALSE;
    if (!(O_SET_INTERACTIVE(ip, ob))
     || ip->closing
       )
    {
        return MY_FALSE;
    }

    if (!append && ip->input_handler != NULL)
    {
        input_t * ih = ip->input_handler;

        while (ih && ih->type != INPUT_TO)
            ih = ih->next;

         if (ih && ((input_to_t*)ih)->eval_nr == eval_number)
             return MY_FALSE;
    }

    it->input.noecho = noecho;
    it->input.local = local_change;
    it->input.type = INPUT_TO;

    /* Appended input_tos never count. */
    it->eval_nr = eval_number - (append ? 1 : 0);

    add_input_handler(ip, &(it->input), append);
    return MY_TRUE;
} /* set_call() */

/*-------------------------------------------------------------------------*/
void
remove_all_players (void)

/* Destruct all user objects. This is first tried by calling master->remove()
 * for every object. If this doesn't destruct the user object,
 * destruct() is used.
 * The function is called when the game is shut down.
 */

{
    int i;

    for (i = 0; i < MAX_PLAYERS; i++) {
        if (all_players[i] == 0 || (all_players[i]->ob->flags & O_DESTRUCTED))
            continue;
        command_giver = all_players[i]->ob;
        trace_level |= all_players[i]->trace_level;
        RESET_LIMITS;
        CLEAR_EVAL_COST;
        push_ref_object(inter_sp, all_players[i]->ob, "remove_all_players");
        (void)callback_master(STR_REMOVE_PL, 1);
        if ( !(all_players[i]->ob->flags & O_DESTRUCTED) ) {
            destruct(all_players[i]->ob);
        }
    }
}

/*-------------------------------------------------------------------------*/
static void
print_prompt_string (string_t *prompt)

/* Print the string <prompt> to the current command_giver.
 * This function checks if the driver hook H_PRINT_PROMPT is set and in that
 * case passes the string through the set function. If it is not set,
 * the prompt is printed via add_message().
#ifdef USE_TLS
 * The prompt is not printed at all if the interactive is currently
 * negotiating the TLS handshake.
#endif
 */

{
    svalue_t *hook = &driver_hook[H_PRINT_PROMPT];

#ifdef USE_TLS
    {
        interactive_t *ip = O_GET_INTERACTIVE(command_giver);
        if (NULL != ip && TLS_HANDSHAKING == ip->tls_status)
            return;
    }
#endif /* USE_TLS */

    if (hook->type == T_CLOSURE)
    {
        object_t *ob;

        /* Needed for clean error recovery */

        previous_ob = 0;
        current_object = command_giver;

        /* Check if the object the closure is bound to still exists.
         * If not, erase the hook, print the prompt using add_message(),
         * then throw an error.
         */
        ob = !CLOSURE_MALLOCED(hook->x.closure_type)
             ? hook->u.ob
             : hook->u.lambda->ob;

        if (ob->flags & O_DESTRUCTED)
        {
            free_svalue(hook);
            put_number(hook, 0);
            current_object = NULL; /* So that catch_tell() can see it */
            add_message_str(prompt);
            errorf("H_PRINT_PROMPT for %s was a closure bound to a "
                   "now-destructed object - hook removed.\n", 
                   get_txt(command_giver->name));
            /* NOTREACHED */
        }

        push_ref_string(inter_sp, prompt);
        call_lambda(hook, 1);
        free_svalue(inter_sp--);
    }
    else if (hook->type == T_STRING)
    {
        push_ref_string(inter_sp, prompt);
        (void)sapply(hook->u.str, command_giver, 1);
    }
    else
    {
        current_object = NULL; /* So that catch_tell() can see it */
        add_message_str(prompt);
    }
} /* print_prompt_string() */

/*-------------------------------------------------------------------------*/
void
print_prompt (void)

/* Print the prompt of the current command_giver, unless disabled
 * by input_to. If the prompt is set to a closure, the closure
 * is called and expected to return the actual prompt string or
 * to print the prompt itself.
 */

{
    interactive_t *ip;
    svalue_t *prompt = NULL;
    object_t * save_current = current_object;
    object_t * save_previous = previous_ob;
    Bool usingDefaultPrompt = MY_FALSE;

#ifdef DEBUG
    if (command_giver == 0)
        fatal("command_giver == 0.\n");
#endif

    if (!(O_SET_INTERACTIVE(ip, command_giver)))
        fatal("print_prompt() of non-interactive object\n");

    if (ip->input_handler != NULL)
    {
        prompt = &ip->input_handler->prompt;
    }
    else
    {
        prompt = &ip->prompt;
        if (prompt->type != T_CLOSURE && prompt->type != T_STRING)
        {
            prompt = &driver_hook[H_DEFAULT_PROMPT];
            usingDefaultPrompt = MY_TRUE;
        }
    }

    if (prompt->type == T_CLOSURE)
    {
        object_t *ob;

        /* Needed for clean error recovery */

        previous_ob = 0;
        current_object = command_giver;

        /* Check if the object the closure is bound to still exists.
         * If not, restore the prompt to the default (this also works with
         * the default prompt driver hook), then throw an error.
         */
        ob = !CLOSURE_MALLOCED(prompt->x.closure_type)
             ? prompt->u.ob
             : prompt->u.lambda->ob;

        if (ob && ob->flags & O_DESTRUCTED)
        {
            free_svalue(prompt);
            put_ref_string(prompt, STR_DEFAULT_PROMPT);
            print_prompt_string(prompt->u.str);
            errorf("Prompt of %s was a closure bound to a now-destructed "
                   "object - default prompt restored.\n", 
                   get_txt(command_giver->name));
            /* NOTREACHED */
        }

        call_lambda(prompt, 0);
        prompt = inter_sp;
        if (prompt->type != T_STRING)
        {
            free_svalue(prompt);
        }
        else
        {
            /* beware: print_prompt_string() might cause an error.
             * Thus, the LPC stack has to include the prompt to free it then.
             */
            print_prompt_string(prompt->u.str);
            free_svalue(prompt);
        }
        inter_sp--;
    }
    else if (prompt->type == T_STRING)
    {
        print_prompt_string(prompt->u.str);
    }
    else if (usingDefaultPrompt)
    {
        /* No prompt nor default prompt given, and it's not an input_to:
         * print the usual prompt.
         */
        print_prompt_string(STR_DEFAULT_PROMPT);
    }

    current_object = save_current;
    previous_ob = save_previous;
} /* print_prompt() */

/*-------------------------------------------------------------------------*/
static int
set_snoop (object_t *me, object_t *you)

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
    interactive_t *on = NULL; /* interactive struct of <you> */
    interactive_t *by = NULL; /* interactive struct of <me> */
    interactive_t *tmp;
    svalue_t *ret;

    /* Stop if people managed to quit before we got this far */
    if (me->flags & O_DESTRUCTED)
        return 0;
    if (you && (you->flags & O_DESTRUCTED))
        return 0;

    /* Check for permissions with valid_snoop in master */
    push_ref_object(inter_sp, me, "snoop");
    if (you == NULL)
        push_number(inter_sp, 0);
    else
        push_ref_object(inter_sp, you, "snoop");
    ret = apply_master(STR_VALID_SNOOP, 2);

    if (!ret || ret->type != T_NUMBER || ret->u.number == 0)
        return 0;

    if (me->flags & O_DESTRUCTED)
        return 0;

    /* Test is <me> is able to snoop anyway.
     * Set <by> to <me>'s interactive struct if yes.
     */
    if (O_SET_INTERACTIVE(by, me) && by->closing)
        return 0;

    if (you)
    {
        /* Test if <you> can be snooped at all.
         * Set <on> to <you>'s interactive struct if yes.
         */
        if (you->flags & O_DESTRUCTED)
            return 0;
        if (!(O_SET_INTERACTIVE(on, you)) || on->closing)
            return 0;
    }
    else
    {
        /* Stop snoop.
         * For this, set <on> to the interactive struct of the snoops
         * victim. If <by> is NULL, <me> is propably a netdead user
         * or a NPC and we have to scan the list of users for the victim.
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
            free_object(me, "set_snoop");
        }
        else
        {
            on = by->snoop_on;
            if (!on || on->closing)
                return 0;
            by->snoop_on = NULL;
        }
        on->snoop_by = NULL;
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
        interactive_t *ip;

        if (O_SET_INTERACTIVE(ip, on->snoop_by))
        {
            if (ip->closing)
                return 0;
            ip->snoop_on = 0;
        }
        else
        {
            free_object(on->snoop_by, "set_snoop");
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
        ref_object(me, "set_snoop");
    }

    on->snoop_by = me;
    return 1;
} /* set_snoop() */

/*=========================================================================*/
/*                      Telnet Support
 */

/* Note: when stored in char variables, IAC can be equal to EOF.
 * This can cause sprintf(), which is used in add_message(), to abort
 * output after EOF. Therefore, don't try to send anything after the IAC
 * in the same call to add_message().
 */

/*-------------------------------------------------------------------------*/
static INLINE void
send_telnet_option (char action, char option)

/* Send IAC <action> <option> */

{
    char msg[3];

    msg[0] = (char) IAC;
    msg[1] = action;
    msg[2] = option;

    add_message_bytes(msg, 3);
    add_message_flush();
} /* send_telnet_option() */

/*-------------------------------------------------------------------------*/
static void
send_wont (int option)

/* Send IAC WONT <option> */

{
    DTF(("%s TDEBUG: send IAC WONT %02x\n", time_stamp(), option));
    send_telnet_option((char) WONT, option);
}

/*-------------------------------------------------------------------------*/
static void
send_dont (int option)

/* Send IAC DONT <option> */

{
    DTF(("%s TDEBUG: send IAC DONT %02x\n", time_stamp(), option));
    send_telnet_option((char) DONT, option);
}

/*-------------------------------------------------------------------------*/
static void
send_will (int option)

/* Send IAC WILL <option> */

{
    DTF(("%s TDEBUG: send IAC WILL %02x\n", time_stamp(), option));
    send_telnet_option((char) WILL, option);
}

/*-------------------------------------------------------------------------*/
static void
send_do (int option)

/* Send IAC DO <option> */

{
    DTF(("%s TDEBUG: send IAC DO %02x\n", time_stamp(), option));
    send_telnet_option((char) DO, option);
}

/*-------------------------------------------------------------------------*/
static void
reply_to_do_echo (int option)

/* Send IAC WONT <option> if we don't want noecho mode.
 * If we requested WILL ECHO this is the client's reply. Set NOECHO_ACK. Send
 * no reply.  Send WILL ECHO if we want noecho but where told not to echo
 * (reactivate noecho mode).
 */

{
    interactive_t *ip = O_GET_INTERACTIVE(command_giver);

    DTN(("reply to DO ECHO\n"));
    if (ip->noecho & NOECHO_MASK) {
        if ( !(ip->noecho & NOECHO) ) {
            /* We were previously told not to echo */
            send_will(option);
        }
        else DTN(("  we don't need to say WILL\n"));
        /* If we already said that we will echo, be quiet */
        ip->noecho |= NOECHO_MASK;
    } else {
        send_wont(option);
    }
}

/*-------------------------------------------------------------------------*/
static void
reply_to_dont_echo (int option)

/* If we requested WONT ECHO this is the client's reply. Do nothing.
 * If client requests us to not echo while we want to, send WONT ECHO and
 * delete NOECHO flag. The client may turn the option on again later.
 */

{
    interactive_t *ip = O_GET_INTERACTIVE(command_giver);

    DTN(("reply to DONT ECHO\n"));
    if (ip->noecho & NOECHO_MASK) {
        if (!~(ip->noecho | ~NOECHO_MASK)) {
            /* We were granted the option before */
            send_wont(option);
        }
        else DTN(("  we don't need to say WONT\n"));
        ip->noecho = (char)((ip->noecho & ~NOECHO) | NOECHO_ACK);
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
    interactive_t *ip = O_GET_INTERACTIVE(command_giver);

    DTN(("reply to DO SGA\n"));
    if (ip->noecho & (NOECHO_MASK|CHARMODE_MASK)) {
        if (!ip->supress_go_ahead) {
            ip->supress_go_ahead = MY_TRUE;
            send_will(option);
        }
        else DTN(("  we don't need to say WILL\n"));
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
    interactive_t *ip = O_GET_INTERACTIVE(command_giver);

    DTN(("reply to DONT SGA\n"));
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
    interactive_t *ip = O_GET_INTERACTIVE(command_giver);

    DTN(("reply to WILL SGA\n"));
    if (ip->noecho & CHARMODE_MASK) {
        if ( !(ip->noecho & CHARMODE) ) {
            send_do(option);
        }
        else DTN(("  we don't need to say DO\n"));
        DTN(("  noecho: %02hhx -> %02hhx\n", ip->noecho, (char)(ip->noecho  | CHARMODE_MASK)));
        ip->noecho |= CHARMODE_MASK;
    } else {
        send_dont(option);
    }
#ifdef SAVE_NOECHO
    if (ip->noecho & NOECHO_DELAYED)
    {
        DT(("'%s' set_noecho():   sending delayed WILL TELOPT_ECHO\n",
            get_txt(ip->ob->name)));
        ip->noecho &= ~NOECHO_DELAYED;
        if (!(ip->noecho & NOECHO_MASK))
        {
            send_will(TELOPT_ECHO);
            ip->noecho |= NOECHO_REQ | NOECHO;
        }
        else DT(("'%s'   we don't need to say WILL\n", get_txt(ip->ob->name)));
    }
#endif /* SAVE_NOECHO */
} /* reply_to_will_sga() */

/*-------------------------------------------------------------------------*/
static void
reply_to_wont_sga (int option)

/* Send IAC DONT <option> if CHARMODE was granted before.
 */

{
    interactive_t *ip = O_GET_INTERACTIVE(command_giver);

    DTN(("reply to WONT SGA\n"));
    if (ip->noecho & CHARMODE_MASK) {
        if (!~(ip->noecho | ~CHARMODE_MASK)) {
            /* We were granted the option before */
            send_dont(option);
        }
        else DTN(("  we don't need to say DONT\n"));
        DTN(("  noecho: %02hhx -> %02hhx\n", ip->noecho, 
             (unsigned char)((ip->noecho & ~CHARMODE) | CHARMODE_ACK)));
        ip->noecho = (char)((ip->noecho & ~CHARMODE) | CHARMODE_ACK);
          /* Don't reset CHARMODE_REQ here: this WONT can be the answer
           * to the DO SGA we sent before, and the client can still answer
           * with DO SGA to the WILL SGA we sent as well (Windows' telnet
           * for example does this).
           * Besides, the variables are now set up to treat the input
           * in charmode, and changing the flag without the variables
           * will do Bad Things(tm).
           */
    }
} /* reply_to_wont_sga() */

/*-------------------------------------------------------------------------*/
static void
mccp_telnet_neg (int option)
{
    interactive_t *ip = O_GET_INTERACTIVE (command_giver);
    
     switch (ip->tn_state)
     {
     case TS_WILL:
         DTF(("MCCP NEG (%d) STATE (WILL)\n", option));
         break;
     case TS_WONT:
         DTF(("MCCP NEG (%d) STATE (WONT)\n", option));
         break;
     case TS_DO:
         DTF(("MCCP NEG (%d) STATE (DO)\n", option));
#ifdef USE_MCCP
         if (!ip->compressing)
             start_compress(ip, option);
#endif
         break;
     case TS_DONT:
         DTF(("MCCP NEG (%d) STATE (DONT)\n", option));
#ifdef USE_MCCP
         if (ip->compressing==option)
             end_compress(ip, MY_FALSE);
#endif
        break;
     default:
         DTF(("MCCP NEG (%d) STATE (%hhd)\n", option, ip->tn_state));
     }
} /* mccp_telnet_neg() */

/*-------------------------------------------------------------------------*/
static svalue_t *
h_telnet_neg (int n)

/* Call the H_TELNET_NEG driverhook with <n> arguments on the interpreter
 * stack. Return the result from that call, or NULL if the hook isn't
 * set. The arguments are removed from the stack in any case.
 */

{
    svalue_t *svp;

    RESET_LIMITS;
    CLEAR_EVAL_COST;
    if (driver_hook[H_TELNET_NEG].type == T_STRING)
    {
        svp =
          secure_callback(driver_hook[H_TELNET_NEG].u.str, command_giver, n);
    }
    else if (driver_hook[H_TELNET_NEG].type == T_CLOSURE)
    {
        if (driver_hook[H_TELNET_NEG].x.closure_type == CLOSURE_LAMBDA)
        {
            free_object(driver_hook[H_TELNET_NEG].u.lambda->ob, "h_telnet_neg");
            driver_hook[H_TELNET_NEG].u.lambda->ob = ref_object(command_giver, "h_telnet_neg");
        }
        svp = secure_callback_lambda(&driver_hook[H_TELNET_NEG], n);
    }
    else
    {
        while (--n >= 0)
            pop_stack();
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
    interactive_t *ip = O_GET_INTERACTIVE(command_giver);
    int i = 0;

    switch(ip->tn_state) {
      case TS_DO:
        DTN(("reply to telnet_neg: DO %02x\n", option));
        i = DO;
        break;
      case TS_DONT:
        DTN(("reply to telnet_neg: DONT %02x\n", option));
        i = DONT;
        break;
      case TS_WILL:
        DTN(("reply to telnet_neg: WILL %02x\n", option));
        i = WILL;
        break;
      case TS_WONT:
        DTN(("reply to telnet_neg: WONT %02x\n", option));
        i = WONT;
        break;
      default:
        debug_message("%s Invalid tn_state %hhd for interactive '%s'\n"
             , time_stamp(), ip->tn_state, get_txt(ip->ob->name));
        break;
    }
    push_number(inter_sp, i);
    push_number(inter_sp, option);
    if (!h_telnet_neg(2)) {
        DTN(("  using default methods\n"));
        switch(ip->tn_state) {
          case TS_DO:
            DTN(("    -> WONT %02x\n", option));
            send_wont(option);
            break;
          case TS_WILL:
            DTN(("    -> DONT %02x\n", option));
            send_dont(option);
            break;
        }
    }
} /* reply_h_telnet_neg() */

/*-------------------------------------------------------------------------*/
void
init_telopts (void)

/* Initialise the telopts_xxx[] tables.
 * The default setting is such that requests are ignored or rejected.
 */

{
    int i;

    /* Pass all telnet options that we're not
     * able to handle to the mudlib.
     */
    for (i = NTELOPTS; --i >= 0; ) {
        telopts_do[i] = reply_h_telnet_neg;
        telopts_dont[i] = reply_h_telnet_neg;
        telopts_will[i] = reply_h_telnet_neg;
        telopts_wont[i] = reply_h_telnet_neg;
    }

    telopts_do[TELOPT_ECHO] = reply_to_do_echo;
    telopts_dont[TELOPT_ECHO] = reply_to_dont_echo;

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
    telopts_will[TELOPT_SGA] = reply_to_will_sga;
    telopts_wont[TELOPT_SGA] = reply_to_wont_sga;

    /* Mud specific protocols */
    telopts_do[TELOPT_COMPRESS] = mccp_telnet_neg;
    telopts_dont[TELOPT_COMPRESS] = mccp_telnet_neg;
    telopts_will[TELOPT_COMPRESS] = mccp_telnet_neg;
    telopts_wont[TELOPT_COMPRESS] = mccp_telnet_neg;

    telopts_do[TELOPT_COMPRESS2] = mccp_telnet_neg;
    telopts_dont[TELOPT_COMPRESS2] = mccp_telnet_neg;
    telopts_will[TELOPT_COMPRESS2] = mccp_telnet_neg;
    telopts_wont[TELOPT_COMPRESS2] = mccp_telnet_neg;
} /* init_telopts() */

/*-------------------------------------------------------------------------*/
void
mudlib_telopts (void)

/* Set all telopts_xxx[] entries to reply_h_telnet_neg().
 * This means that the mudlib does all the telnet negotiation.
 * It is called whenever driver hook H_NOECHO is set.
 */

{
    int i;

    DT(("All telnet options set to the mudlib.\n"));
    for (i = NTELOPTS; --i >= 0; ) {
        telopts_do[i] = telopts_dont[i] =
          telopts_will[i] = telopts_wont[i] = reply_h_telnet_neg;
    }
} /* mudlib_telopts() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
is_charmode (interactive_t * ip)

/* Return TRUE if <ip> has charmode enabled, and FALSE if not.
 */

{
    return (ip->noecho & CHARMODE_REQ)
            && (   ip->command[0] != input_escape
                || find_no_bang(ip) & IGNORE_BANG
               )
               
    ;
} /* is_charmode() */

/*-------------------------------------------------------------------------*/
static INLINE void
set_tn_state (interactive_t * ip, enum telnet_states state)

/* Set the new telnet state for <ip> to <state>.
 */

{
    ip->tn_state = state;
    DTN(("t_n: new state %d\n", state));
} /* set_tn_state() */

/*-------------------------------------------------------------------------*/
static void
telnet_neg (interactive_t *ip)

/* Process the data read from the socket, performing any telnet negotiations
 * necessary, and extract the 'pure' command text converting it to UTF-8.
 * When the function returns, all new data in .text[] has been used and .text_end
 * set back as far as possible. The command text will be in .command[] up
 * to .command_end.
 *
 * The start state for the telnet machine is TS_DATA, and whenever a command
 * text has been completed (when a line ending was found), it assumes the
 * TS_READY state.
 * In Charmode all input will be processed regardless of line endings,
 * as soon as characters are available for processing, the state is changed
 * to TS_CHAR_READY, when a newline es encountered it is changed to TS_READY.
 *
 * A call to telnet_neg_reset() (see below) is required to return from the
 * TS_*READY states to TS_DATA. This is to avoid accidentally processing
 * data in line mode when the mudlib is about to switch to char mode, and
 * vice versa.
 *
 * The function starts at .tn_end and goes on until it reaches .text_end or
 * a full newline.
 *
 * When it returns:
 *   tn_end is set to the first unprocessed character.
 * When a full newline is found:
 *   Processed commands start at command_start and are \0 terminated strings,
 *   tn_state is set to TS_READY.
 * else
 *   Processed commands start at command_start and end at command_end-1,
 *   tn_state is set to TS_DATA (or something else if we got a fragmented
 *     telnet negotiation).
 *
 * Because the command is held in another buffer (.command[]) than the raw
 * input data (.text[]), unprocessed data can be removed afterwards to
 * the beginning of .text[] and therefore .text_end can move to the start.
 *
 * If gobble_char is set, that char is removed from a fresh text packet.
 * Removing of unwanted chars inside of a packet is done at the appropriate
 * place (case '\r':). There is no gobbling of <LN><CR> sequences in
 * character mode (why not?). Code would have to be added at case '\n':
 * to gobble them in-packet.
 *
 * Example:
 *   text = "say hello\r\nsay hi\r\n";
 *   tn_end = 0
 *   text_end = 19
 *   command_start = 0
 *   command_end = 0
 *   tn_state = TS_DATA
 *
 * Output would be:
 *   text = "say hello\r\nsay hi\r\n";
 *   command = "say hello\r\nsay hi\r\n";
 *
 *   command_start = 0
 *   command_end = 9
 *   command_unprocessed_end = 19
 *   tn_end = 0
 *   text_end = 0
 *   tn_state = TS_READY
 *
 * After a second call of telnet_neg (almost always done by get_message())
 * will pre process the second command:
 *   command_start = 9
 *
 * Output would be:
 *   text = "say hello\r\nsay hi\r\n";
 *   command = "say hi\r\no\0\nsay hi\r\n";
 *
 *   command_start = 0
 *   command_end = 6
 *   command_unprocessed_end = 8
 *   tn_end = 0
 *   text_end = 0
 *   state = TS_READY
 */

{
    char *source = ip->text + ip->tn_end;       /* Next char to process. */
    char *last = ip->text + ip->text_end;       /* End of the data to process. */

    DTN(("telnet_neg: state %hhd\n", ip->tn_state));

    while (source < last || (ip->tn_state == TS_DATA && ip->command_end != ip->command_unprocessed_end))
    {
        switch (ip->tn_state)
        {
            case TS_READY:
            case TS_CHAR_READY:
                /* This shouldn't happen. We can't go into the loop with TS_READY,
                 * and when switching to TS_READY we should exit this function.
                 */
                DTN(("t_n: somehow in TS_READY - return\n"));
                return;

            case TS_DATA:
            {
                char *command_from = ip->command + ip->command_end;             /* Next char to process. */
                char *command_to = command_from;                                /* Where to put the processed chars. */
                char *command_end = ip->command + ip->command_unprocessed_end;  /* End of the unprocessed chars. */
                bool ready = false;                                             /* Whether we have a command to return. */

                if (ip->command_start > 1)
                {
                    /* Move the already processed characters to the beginning.
                     * But leave any escape chars at the first position.
                     */
                    memmove(ip->command + 1, ip->command + ip->command_start, ip->command_end - ip->command_start);
                    command_to -= ip->command_start - 1;
                    if (ip->command_printed >= ip->command_start)
                        ip->command_printed -= ip->command_start - 1;
                    else if (ip->command_printed > 1)
                        ip->command_printed = 1;
                    ip->command_start = 1;
                }

                if (ip->gobble_char && command_from != command_end)
                {
                    if (*command_from == ip->gobble_char
                     || (*command_from == '\0' && ip->gobble_char == '\n'))
                        command_from++;

                    ip->gobble_char = 0;
                }

                while (command_from != command_end)
                {
                    char ch = *command_from;

                    switch (ch)
                    {
                        case '\b':              /* Backspace */
                        case 0x7f:              /* Delete */
                            /* In Linemode, just move to one char back.
                             * In Charmode with escaped input, write the data gathered
                             * so far and add a rubout sequence ('\b \b').
                             * In Charmode with unescaped input, just pass it on to
                             * the mudlib.
                             *
                             * If telnet is disabled, pass it as it is.
                             */
                            if (!ip->tn_enabled)
                            {
                                *command_to++ = ch;
                                break;
                            }

                            if (!(ip->noecho & CHARMODE_REQ))
                            {
                                /* Linemode */
                                if (command_to > ip->command)
                                    command_to = utf8_prev(command_to, command_to - ip->command);
                                break;
                            }

                            if (ip->command[0] == input_escape
                             && ! (find_no_bang(ip) & IGNORE_BANG) )
                            {
                                /* Charmode with escaped input.
                                 * If we have already printed the last character,
                                 * we need to overwrite that with a space.
                                 */
                                if (command_to > ip->command)
                                {
                                    bool unprint = (ip->command + ip->command_printed == command_to);

                                    command_to = utf8_prev(command_to, command_to - ip->command);
                                    if (unprint)
                                    {
                                        comm_socket_write("\b \b", 3, ip, 0);
                                        ip->command_printed = command_to - ip->command;
                                    }
                                }
                                break;
                            }

                            /* Charmode, leave the character as it is. */
                            *command_to++ = ch;
                            break;

                        /* Handle line endings:
                         * In Charmode we pass them as they are.
                         * In Linemode we handle combinations of CR, NL and NUL
                         * (usually CR LF, but also CR NUL or single LF or NUL)
                         * as the end of the command.
                         */
                        case '\0':
                        case '\r':
                        case '\n':
                        {
                            bool charmode = is_charmode(ip);
                            if (charmode) /* We pass this on. */
                                *command_to++ = ch;

                            command_from++;

                            /* Let's see if there's a second byte to skip. */
                            if (command_from != command_end)
                            {
                                if ((ch == '\r' && (*command_from == '\n' || *command_from == '\0'))
                                 || (ch == '\n' && *command_from == '\r'))
                                {
                                    if (charmode)
                                        *command_to++ = *command_from;
                                    command_from++;
                                }
                            }
                            else if (!charmode)
                            {
                                if (ch == '\r')
                                    ip->gobble_char = '\n';
                                else if (ch == '\n')
                                    ip->gobble_char = '\r';
                            }

                            set_tn_state(ip, TS_READY);

                            ready = true;
                            break;
                        }

                        default:
                            *command_to++ = ch;
                            break;
                    }

                    if (ready)
                        break;

                    command_from++;

                }

                /* It is charmode and we have at least one character ready and
                 * either we are then out of characters or a telnet sequence
                 * is coming, then handle the characters first. */
                if (!ready
                 && command_to != ip->command + ip->command_start && is_charmode(ip)
                 && (source == last || *(unsigned char*)(source) == IAC))
                {
                    set_tn_state(ip, TS_CHAR_READY);
                    ready = true;
                }

                if (ready)
                {
                    /* Consolidate the .text[] buffer. */
                    ip->tn_end = ip->text_prefix;
                    ip->text_end = ip->text_prefix + last - source;
                    memmove(ip->text + ip->text_prefix, source, last - source);

                    /* Consolidate the .command[] buffer. */
                    ip->command_end = command_to - ip->command;
                    ip->command_unprocessed_end = command_end - command_from + command_to - ip->command;
                    memmove(command_to, command_from, command_end - command_from);
                    return;
                }

                /* Finished processing all converted data in .command[],
                 * now look through any received bytes in .text[].
                 */
                command_from = command_end = command_to;
                ip->command_unprocessed_end = ip->command_end = command_to - ip->command;

                if (source != last)
                {
                    /* We convert all non-telnet bytes acoording to the encoding
                     * into the .command[] buffer. Therefore look for the next IAC.
                     */
                    size_t sourceleft, sourceleftorig, commandleft;
                    char *next_tn = memchr(source, IAC, last - source);
                    if (next_tn == NULL)
                        next_tn = last;
                    else if (next_tn == source)
                    {
                        source++;
                        set_tn_state(ip, TS_IAC);
                        continue;
                    }

                    if (ip->text_prefix > 0)
                    {
                        /* Move the pending multi-bytes just before the next ones. */
                        source -= ip->text_prefix;
                        if (source != ip->text)
                            memmove(source, ip->text, ip->text_prefix);
                    }

                    sourceleftorig = sourceleft = next_tn - source;
                    commandleft = ip->command + sizeof(ip->command) - command_end;

                    iconv(ip->receive_cd, &source, &sourceleft, &command_end, &commandleft);
                    ip->tn_end = source - ip->text;
                    ip->command_unprocessed_end = command_end - ip->command;

                    if (ip->text_prefix > sourceleftorig - sourceleft)
                    {
                        /* Some bytes of the prefix weren't processed. */
                        ip->text_prefix -= sourceleftorig - sourceleft;
                        memmove(ip->text, source, ip->text_prefix);
                        source += ip->text_prefix;
                    }
                    else
                        ip->text_prefix = 0;

                    if (sourceleftorig == sourceleft)
                    {
                        /* We didn't process a new byte from the input.
                         * Then we need to look at the error code.
                         */
                        switch (errno)
                        {
                            case E2BIG:
                                /* Our command buffer is full and we have seen no newline.
                                 * Return the text so far as partial command and restart
                                 * input from the beginning.
                                 */
                                set_tn_state(ip, TS_READY);

                                /* Consolidate the .text[] buffer. */
                                ip->tn_end = ip->text_prefix;
                                ip->text_end = ip->text_prefix + last - source;
                                memmove(ip->text + ip->text_prefix, source, last - source);

                                ip->command_end = ip->command_unprocessed_end = command_to - ip->command;
                                return;

                            case EINVAL:
                                /* An incomplete multi-byte sequence found. */
                                if (next_tn == last)
                                {
                                    /* We didn't receive any further bytes. Wait for them. */
                                    ip->tn_end = 0;
                                    ip->text_end = last - source;
                                    memmove(ip->text, source, last - source);
                                    return;
                                }
                                else
                                {
                                    /* There is a telnet sequence in the multi-byte sequence.
                                     * We remember the bytes for later. And handle the IAC.
                                     */
                                    if (source != ip->text)
                                        memmove(ip->text + ip->text_prefix, source, sourceleft);

                                    ip->text_prefix += sourceleft;
                                    set_tn_state(ip, TS_IAC);
                                    continue;
                                }

                            case EILSEQ:
                            default:
                                /* An invalid byte sequence found. We just skip this byte. */
                                source++;
                                continue;
                        }
                    }
                }
                break;
            } /* case TS_DATA */

            case TS_IAC:
            {
                /* Begin a telnet negotiation */
                unsigned char ch = *(unsigned char*)(source++);
                switch (ch)
                {
                    case IAC:
                        DTN(("t_n: got IAC\n"));
                        /* We put this into the prefix, these are already telnet processed bytes. */
                        *(ip->text + ip->text_prefix) = ch;
                        ip->text_prefix++;
                        set_tn_state(ip, TS_DATA);
                        continue;

                    case WILL:
                        DTN(("t_n: got WILL\n"));
                        set_tn_state(ip, TS_WILL);
                        continue;

                    case WONT:
                        DTN(("t_n: got WONT\n"));
                        set_tn_state(ip, TS_WONT);
                        continue;

                    case DO:
                        DTN(("t_n: got DO\n"));
                        set_tn_state(ip, TS_DO);
                        continue;

                    case DONT:
                        DTN(("t_n: got DONT\n"));
                        set_tn_state(ip, TS_DONT);
                        continue;

                    case SB:
                        DTN(("t_n: got SB\n"));
                        ip->tn_start = (short)(source - ip->text);
                        set_tn_state(ip, TS_SB);
                        continue;

                    case DM:
                        DTN(("t_n: got DM\n"));
                        if (ip->syncing)
                        {
                            struct timeval timeout;
                            fd_set exceptfds;

                            FD_ZERO(&exceptfds);
                            FD_SET(ip->socket, &exceptfds);
                            timeout.tv_sec = 0;
                            timeout.tv_usec = 0;
                            if (!socket_select(ip->socket + 1, 0, 0, &exceptfds, &timeout))
                            {
                                if (d_flag)
                                    debug_message("%s Synch operation finished.\n", time_stamp());
                                ip->syncing = false;
                            }
                        }
                        break;

                    case NOP:
                        DTN(("t_n: got NOP\n"));
                        break;

                    case GA:
                        DTN(("t_n: got GA\n"));
                        break;

                    default:
                        DTN(("t_n: got %02hhx\n", ch));
                        break;
                } /* switch(ch) */
                set_tn_state(ip, ip->syncing ? TS_SYNCH : TS_DATA);
                break;
            }

            case TS_WILL:
            {
                int ch = *(unsigned char*)(source++);

                command_giver = ip->ob;
                if (ch < NTELOPTS)
                {
                    DTN(("t_n: state WILL got %s (%02x)\n", telopts[ch], ch));
                    if (d_flag)
                        debug_message("%s Will %s\n", time_stamp(), telopts[ch]);
                    (*telopts_will[ch])(ch);
                }
                else
                {
                    debug_message("%s Unknown telnet option Will %d\n", time_stamp(), ch);
                    send_dont(ch);
                }
                set_tn_state(ip, ip->syncing ? TS_SYNCH : TS_DATA);
                break;
            }

            case TS_WONT:
            {
                int ch = *(unsigned char*)(source++);

                command_giver = ip->ob;
                if (ch < NTELOPTS)
                {
                    DTN(("t_n: state WONT got %s (%02x)\n", telopts[ch], ch));
                    if (d_flag)
                        debug_message("%s Wont %s\n", time_stamp(), telopts[ch]);
                    (*telopts_wont[ch])(ch);
                }
                else
                {
                    debug_message("%s Unknown telnet option Wont %d\n", time_stamp(), ch);
                }
                set_tn_state(ip, ip->syncing ? TS_SYNCH : TS_DATA);
                break;
            }

            case TS_DO:
            {
                int ch = *(unsigned char*)(source++);

                command_giver = ip->ob;
                if (ch < NTELOPTS)
                {
                    DTN(("t_n: state DO got %s (%02x)\n", telopts[ch], ch));
                    if (d_flag)
                        debug_message("%s Do %s\n", time_stamp(), telopts[ch]);
                    (*telopts_do[ch])(ch);
                }
                else
                {
                    debug_message("%s Unknown telnet option Do %d\n", time_stamp(), ch);
                    send_wont(ch);
                }
                set_tn_state(ip, ip->syncing ? TS_SYNCH : TS_DATA);
                break;
            }

            case TS_DONT:
            {
                int ch = *(unsigned char*)(source++);

                command_giver = ip->ob;
                if (ch < NTELOPTS)
                {
                    DTN(("t_n: state DONT got %s (%02x)\n", telopts[ch], ch));
                    if (d_flag)
                        debug_message("%s Dont %s\n", time_stamp(), telopts[ch]);
                    (*telopts_dont[ch])(ch);
                }
                else
                {
                    debug_message("%s Unknown telnet option Dont %d\n", time_stamp(), ch);
                }
                set_tn_state(ip, ip->syncing ? TS_SYNCH : TS_DATA);
                break;
            }

            case TS_SB:
            {
                unsigned char ch = *(unsigned char*)(source++);
                DTN(("t_n: state TS_SB got %02hhx\n", ch));
                if (ch == IAC)
                    set_tn_state(ip, TS_SB_IAC);
                continue;
            }

            case TS_SB_IAC:
            {
                unsigned char ch = *(unsigned char*)(source);
                DTN(("t_n: state TS_SB_IAC got %02hhx\n", ch));
                switch (ch)
                {
                    case IAC:
                        /* We had two IAC, compress them into one. */
                        memmove(source, source+1, last - source);
                        last--;
                        set_tn_state(ip, TS_SB);
                        continue;

                    case SE:
                    case SB:
                    {
                        vector_t *v;
                        mp_int size = (source - ip->text) - ip->tn_start - 2;

                        if (size >= 0
                         && (size <= (mp_int)max_array_size || !max_array_size)
                         && (current_object = ip->ob,  v = allocate_array(size)))
                        {
                            unsigned char *str = (unsigned char *)ip->text + ip->tn_start;
                            svalue_t *svp;

                            DTN(("t_n: that is: state TS_SB_IAC got useful SE or SB: neg SB %02hhx (%"PRIdMPINT" bytes)\n", *str, size));
                            push_number(inter_sp, SB);
                            push_number(inter_sp, *str++);
                            svp = v->item;
                            while (--size >= 0)
                            {
                                svp->u.number = *str++;
                                svp++;
                            }
                            push_array(inter_sp, v);
                            command_giver = ip->ob;
                            h_telnet_neg(3);
                        }

                        if (ch == SE)
                        {
                            source++;
                            set_tn_state(ip, ip->syncing ? TS_SYNCH : TS_DATA);
                            continue;
                        }
                        // else FALLTHROUGH
                    }

                    default:
                        set_tn_state(ip, TS_IAC);
                        continue;
                }
                break;
            }

            case TS_SYNCH:
            {
                unsigned char ch = *(unsigned char*)(source++);
                DTN(("t_n: state TS_SYNCH got %02hhx\n", ch));
                switch (ch)
                {
                    case IAC:
                        set_tn_state(ip, TS_IAC);
                        continue;

                    case DM:
                        /* Let that by handled by the TS_IAC code. */
                        set_tn_state(ip, TS_IAC);
                        source--;
                        continue;
                }
                /* All other chars we just ignore. */
                continue;
            }

            default:
                if (d_flag)
                    debug_message("%s Bad state: 0x%hhx\n", time_stamp(), ip->tn_state);
                set_tn_state(ip, TS_DATA);
                break;
        } /* switch() */
    } /* while() */

    /* We processed all the input. */
    if (TN_START_VALID(ip->tn_state))
    {
        /* There is a subnegotation in progress.
         * We are not allowed to reset the buffer.
         */
        ip->text_end = ip->tn_end = source - ip->text;
        if (ip->tn_start > ip->text_prefix)
        {
            /* Consolidate the .text[] buffer. */
            memmove(ip->text + ip->text_prefix, ip->text + ip->tn_start, ip->tn_end - ip->tn_start);
            ip->text_end = ip->tn_end -= ip->tn_start - ip->text_prefix;
            ip->tn_start = ip->text_prefix;
        }
        else if (ip->text_end == MAX_TEXT)
        {
            /* Telnet negotiation shouldn't have such large data chunks.
             * Ignore all data altogether and return to text mode.
             */
            ip->text_end = ip->tn_end = 0;
            ip->tn_state = TS_DATA;
        }
    }
    else
    {
        /* Reset the buffers. */
        ip->text_end = ip->tn_end = ip->text_prefix;
    }
} /* telnet_neg() */

/*-------------------------------------------------------------------------*/
static void
telnet_neg_reset (interactive_t *ip)

/* Reset telnet state back to TS_DATA. This function is invoked when
 * the processed command buffer is consumed, allowing telnet_neg()
 * (see above) to proceed with the unprocessed part.
 */

{
    /* When called after a TS_READY reset the pointers
     * to process the next command.
     */
    if (ip->tn_state == TS_READY)
    {
        memmove(ip->command, ip->command + ip->command_end, ip->command_unprocessed_end - ip->command_end);
        ip->command_unprocessed_end -= ip->command_end;
        ip->command_start = ip->command_end = ip->command_printed = 0;
        set_tn_state(ip, TS_DATA);
    }
    else if (ip->tn_state == TS_CHAR_READY)
    {
        /* Remove any processed characters, but leave the first character
         * to make input escape last.
         */
        if (ip->command_start > 1)
        {
            memmove(ip->command + 1, ip->command + ip->command_start, ip->command_unprocessed_end - ip->command_start);
            ip->command_unprocessed_end -= ip->command_start - 1;
            ip->command_end -= ip->command_start - 1;
            ip->command_printed -= ip->command_start - 1;
            ip->command_start = 1;
        }

        set_tn_state(ip, TS_DATA);
    }
}

/* End of Telnet support */
/*=========================================================================*/
/*                      ERQ Support
 */

#ifdef ERQ_DEMON
/*-------------------------------------------------------------------------*/
void
start_erq_demon (const char *suffix, size_t suffixlen)

/* Start the ERQ demon from the path 'ERQFILE<suffix>' and setup
 * the pending_erq[] array.
 */

{
    erq_callback_t *erqp;
    char path[MAXPATHLEN+1];
    int sockets[2];
    int pid, i;
    char c = 0;

    /* Create the freelist in pending_erq[] */
    pending_erq[0].fun.type = T_INVALID;
    pending_erq[0].fun.u.generic = NULL;

    erqp = pending_erq + 1;
    while (erqp < &pending_erq[MAX_PENDING_ERQ])
    {
        erqp->fun.u.generic = (void *)(erqp - 1);
        erqp->fun.type = T_INVALID;
        erqp++;
    }
    free_erq = &pending_erq[MAX_PENDING_ERQ-1];

    /* Create the sockets to talk to the ERQ */
/* TODO: Add tests to configure if the system really implements AF_UNIX or socketpair() */
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) < 0)
    {
        perror("socketpair");
        return;
    }

    printf("%s Attempting to start erq '%s%s'.\n"
          , time_stamp(), erq_file, suffix);
    debug_message("%s Attempting to start erq '%s%s'.\n"
                 , time_stamp(), erq_file, suffix);

    /* Close inherited sockets first. */
    for (i = 0; i < numports; i++)
        if (port_numbers[i] < 0)
            set_close_on_exec(-port_numbers[i]);

    if ((pid = fork()) == 0)
    {
        /* Child */
        dup2(sockets[0], 0);
        dup2(sockets[0], 1);
        close(sockets[0]);
        close(sockets[1]);

        if (strlen(erq_file) + 1 + suffixlen <= sizeof path)
        {
            sprintf(path, "%s%.*s", erq_file, (int)suffixlen, suffix);
            if (erq_args)
                execv((char *)path, erq_args);
            else
                execl((char *)path, "erq", "--forked", (char*)0);
        }
        write(1, "0", 1);  /* indicate failure back to the driver */
        _exit(1);
    }

    close(sockets[0]);
    if (pid == -1)
    {
        close(sockets[1]);
        return;
    }
    else
    {
        bool found = false;
        for (i = 0; i < MAX_ERQ_PIDS; i++)
            if (!erq_pids[i])
            {
                erq_pids[i] = pid;
                found = true;
                break;
            }
        if (!found)
            erq_pids[0] = pid;
    }

    /* Read the first character from the ERQ. If it's '0', the ERQ
     * didn't start.
     */
    read(sockets[1], &c, 1);
    if (c == '0') {
        close(sockets[1]);

        printf("%s Failed to start erq.\n", time_stamp());
        debug_message("%s Failed to start erq.\n", time_stamp());
        return;
    }

    /* ERQ is up and running */
    erq_demon = sockets[1];
    set_socket_nonblocking(erq_demon);
    set_socket_nosigpipe(erq_demon);
    
    if (socket_number(erq_demon) >= min_nfds)
        min_nfds = socket_number(erq_demon)+1;
} /* start_erq_demon() */

/*-------------------------------------------------------------------------*/
void
wait_erq_demon ()

/* Calls wait() upon the ERQ demon. This function is called whenever we
 * we received a SIGCHLD to check whether there is a zombie ERQ process.
 */

{
    for (int i = 0; i < MAX_ERQ_PIDS; i++)
        if (erq_pids[i])
        {
            pid_t result =  waitpid(erq_pids[i], NULL, WNOHANG);
            if (result < 0 || result == erq_pids[i])
                erq_pids[i] = 0;
        }
} /* wait_erq_demon() */

/*-------------------------------------------------------------------------*/
static void
shutdown_erq_demon (void)

/* Close the connection to the ERQ.
 * This method is to be used directly only on game shutdown, otherwise
 * use stop_erq_demon() instead.
 */

{
    if (erq_demon < 0)
        return;

    socket_close(erq_demon);
    erq_demon = FLAG_NO_ERQ;
    erq_pending_len = 0;
    input_from_erq = &buf_from_erq[0];
} /* shutdown_erq_demon() */

/*-------------------------------------------------------------------------*/
static void
stop_erq_demon (Bool notify)

/* Close the connection to the ERQ and inform all pending requests
 * about this. If <notify> is set, the hook H_ERQ_STOP is called.
 */

{
    erq_callback_t *erqp;
    int i;

    if (erq_demon < 0)
        return;

    shutdown_erq_demon();

    /* Inform all pending requests about the loss.
     */
    erqp = pending_erq;
    i = MAX_PENDING_ERQ;
    do {
        if (erqp->fun.type == T_CLOSURE)
        {
            *++inter_sp = erqp->fun;
            erqp->fun.type = T_INVALID;
            erqp->fun.u.generic = (void *)free_erq;
            free_erq = erqp;
            CLEAR_EVAL_COST;
            RESET_LIMITS;
            callback_master(STR_STALE_ERQ, 1);
        }
        erqp++;
    } while (--i);

    /* If desired, call H_ERQ_STOP to notify the situation.
     */
    if (notify)
    {
        RESET_LIMITS;
        CLEAR_EVAL_COST;
        if (driver_hook[H_ERQ_STOP].type == T_CLOSURE) {
            secure_callback_lambda(&driver_hook[H_ERQ_STOP], 0);
        }
    }
} /* stop_erq_demon() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_attach_erq_demon (svalue_t *sp)

/* EFUN: attach_erq_demon()
 *
 *   int attach_erq_demon(object ob, int do_close)
 *   int attach_erq_demon(string name, int do_close)
 *
 * In the first form, take away the connection from <ob> and store it as
 * _the_ erq connection. <ob> thus becomes a normal non-interactive object.
 * In the second form, try to start the ERQ demon from the path
 * 'ERQFILE<name>' (ERQFILE defaults to BINDIR/erq). <name> must not
 * contain '/..' sequences.
 *
 * If there is already an ERQ demon connected to the driver, the function
 * will fail unless <do_close> is set to 1 or any other odd integer; in
 * this case the connection to the old ERQ will be closed first.
 *
 * Return svalue.number 1 on success, 0 else.
 */

{
    object_t *ob;
    interactive_t *ip;
    string_t *suffix;

    /* Test for the first form: (object ob, int do_close) */
    if (sp[-1].type == T_OBJECT)
    {
        ob = sp[-1].u.ob;
        if (!O_SET_INTERACTIVE(ip, ob))
        {
            errorf("Bad arg 1 to attach_erq_demon(): object is not interactive.\n");
            /* NOTREACHED */
            return sp;
        }

        sp--;
        deref_object(ob, "attach_erq_demon");
        put_number(sp, 0);
        /* we need to read sp[1] below, thus don't overwrite it now. */
        if (privilege_violation4(STR_ATTACH_ERQ_DEMON,
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

    /* Otherwise the argument is a string */

    suffix = sp[-1].u.str;
    if (mstrstr(suffix, "/.."))
    {
        errorf("Bad arg 1 to attach_erq_demon(): illegal path.\n");
        /* NOTREACHED */
        return sp;
    }

    {
        int n;

        sp--;
        n = 0;
        if (privilege_violation4(STR_ATTACH_ERQ_DEMON,
            0, suffix, sp[1].u.number, sp+1))
        {
            char *native = convert_path_str_to_native_or_throw(suffix);
            if (erq_demon != FLAG_NO_ERQ)
            {
                if (sp[1].u.number & 1) {
                    stop_erq_demon(0);
                } else {
                    goto return_result;
                }
                erq_proto_demon = -1;
            }
            start_erq_demon(native, strlen(native));
            n = 1;
        }
return_result:
        free_svalue(sp);
        put_number(sp, n);
        return sp;
    }

    /* NOTREACHED */
    return NULL;
} /* f_attach_erq_demon() */

/*-------------------------------------------------------------------------*/
static Bool
send_erq (int handle, int request, const char *arg, size_t arglen)

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
    long wrote;

    if (erq_demon < 0)
        return MY_FALSE;

    /* Try to send the pending data */
    if (erq_pending_len)
    {
        wrote = socket_write(erq_demon, pending, erq_pending_len);
        if (wrote > 0) {
            pending += wrote;
            erq_pending_len -= wrote;
        }
        if (erq_pending_len)
            return MY_FALSE;
    }

    if (arglen + 9 > sizeof buf)
        return MY_FALSE;

    /* Create the message and add it to buf[] */
    erq_pending_len = arglen + 9;
    /* Write erq_pending_len and handle in network byte order. */
    buf[0] = (erq_pending_len >> 24) & 0xff;
    buf[1] = (erq_pending_len >> 16) & 0xff;
    buf[2] = (erq_pending_len >>  8) & 0xff;
    buf[3] = (erq_pending_len      ) & 0xff;
    buf[4] = (handle >> 24) & 0xff;
    buf[5] = (handle >> 16) & 0xff;
    buf[6] = (handle >>  8) & 0xff;
    buf[7] = (handle      ) & 0xff;
    buf[8] = (char)request;
    memcpy(buf + 9, arg, arglen);

    /* Send as much of buf[] as possible */
    pending = buf;
    wrote = socket_write(erq_demon, buf, erq_pending_len);
    if (wrote > 0) {
        pending += wrote;
        erq_pending_len -= wrote;
    }

    return MY_TRUE;
} /* send_erq() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_send_erq (svalue_t *sp)

/* EFUN: send_erq()
 *
 *   int send_erq(int request, bytes|int* data, closure callback)
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
    size_t arglen;
    erq_callback_t *new_erq;
    int i;
    p_int erq_request;

    /* Set arg with the data to send. */

    if (sp[-1].type == T_BYTES)
    {
        arg = get_txt(sp[-1].u.str);
        arglen = mstrsize(sp[-1].u.str);
    }
    else /* it's a pointer */
    {
        vector_t *v;
        svalue_t *svp;
        char *cp;
        p_int j;

        v = sp[-1].u.vec;
        arglen = VEC_SIZE(v);
        cp = arg = xalloc(arglen);
        if (!arg) {
            errorf("Out of memory (%zu bytes) in send_erq() for allocating "
                   "temporary buffer.\n", arglen);
        }
        svp = &v->item[0];
        for (j = (p_int)arglen; --j >= 0; )
        {
            svalue_t* item = get_rvalue(svp++, NULL);
            if (item == NULL || item->type != T_NUMBER)
            {
                xfree(arg);
                errorf("Bad arg 2 to send_erq(): got %s*, "
                      "expected string/int*.\n", typename(svp[-1].type));
                /* NOTREACHED */
                return sp;
            }
            *cp++ = (char)item->u.number;
        }
    }

    erq_request = sp[-2].u.number;

    /* Test if this call is allowed. */

    if (!privilege_violation4(STR_SEND_ERQ, 0, STR_EMPTY
                             , erq_request & (~ERQ_CB_STRING)
                             , sp))
    {
        goto failure;
    }

    /* Store the callback closure. If none is given, use the
     * default callback.
     */

    new_erq = NULL;

    if (sp->type == T_NUMBER) { /* it's the number 0 */
        new_erq = &pending_erq[MAX_PENDING_ERQ];
        new_erq->fun.u.generic = (void *)free_erq;
    }
    else if (sp->type == T_CLOSURE
          && sp->x.closure_type != CLOSURE_UNBOUND_LAMBDA)
    {
        new_erq = free_erq;
    }

    /* Send the request and make up the result. */

    if (new_erq
     && 0 != (i = send_erq(new_erq - pending_erq, erq_request & (~ERQ_CB_STRING)
                          , arg, arglen))
       )
    {
        free_erq = (erq_callback_t *)new_erq->fun.u.generic;
        new_erq->fun = *sp;
        new_erq->string_arg = (erq_request & ERQ_CB_STRING) != 0;
    }
    else
    {
failure:
        i = 0;
        free_svalue(sp);
    }
    /* cleanup */
    if (sp[-1].type != T_BYTES)
    {
        /* free arg only if sp-1 is not a byte sequence */
        xfree(arg);
    }
    free_svalue(--sp);
    (*--sp).u.number = i;

    return sp;
} /* f_send_erq() */

/*-------------------------------------------------------------------------*/
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
add_ip_entry (struct in_addr addr, const char *name)

/* Add a new IP address <addr>/hostname <name> pair to the cache iptable[].
 * If the <addr> already exists in the table, replace the old tabled name
 * with the new one.
 */

{
    int i, ix;
    Bool new_entry;

    ix = -1;
    new_entry = MY_FALSE;
    for (i = 0; i < IPSIZE; i++)
    {
        if (!memcmp(&(iptable[i].addr.s_addr), &addr.s_addr, sizeof(iptable[i].addr.s_addr)))
        {
            ix = i;
            break;
        }
    }

    if (ix < 0)
    {
        ix = ipcur;
        new_entry = MY_TRUE;
    }

    iptable[ix].addr = addr;
    if (iptable[ix].name)
        free_mstring(iptable[ix].name);
    iptable[ix].name = new_tabled(name, STRING_ASCII);

    if (new_entry)
        ipcur = (ipcur+1) % IPSIZE;
} /* add_ip_entry() */

/*-------------------------------------------------------------------------*/
#ifdef USE_IPV6

static void
update_ip_entry (const char *oldname, const char *newname)

/* Change the IP name <oldname> in the iptable[] to <newname>.
 * If the <oldname> is not in the table, nothing happens.
 */

{
    for (int i = 0; i < IPSIZE; i++)
    {
        if (iptable[i].name
         && !strncmp(get_txt(iptable[i].name), oldname, mstrsize(iptable[i].name))
           )
        {
            free_mstring(iptable[i].name);
            iptable[i].name = new_tabled(newname, STRING_ASCII);
        }
    }
} /* update_ip_entry() */

/*-------------------------------------------------------------------------*/

#endif /* USE_IPV6 */
    
/*-------------------------------------------------------------------------*/
static string_t *
lookup_ip_entry (struct in_addr addr, Bool useErq)

/* Lookup the IP address <addr> and return an uncounted pointer to
 * a shared string with the hostname. The function looks first in the
 * iptable[], then, if not found there and <useErq> is true, asks the ERQ.
 * If the hostname can not be found, NULL is returned.
 */

{
    int i;
    string_t *ipname;
    struct in_addr tmp;

    /* Search for the address backwards from the last added entry,
     * hoping that its one of the more recently added ones.
     */
    i = ipcur;
    do {
        i--;
        if (i < 0)
            i += IPSIZE;

        if (!memcmp(&(iptable[i].addr.s_addr), &addr.s_addr, sizeof(iptable[i].addr.s_addr))
         && iptable[i].name)
        {
            return iptable[i].name;
        }
    } while (i != ipcur );

    /* The address is new to us.
     * Add a temporary entry into the iptable[] to bridge
     * the time until the erq has finished the lookup.
     * This also handles the case of an unresolvable hostname.
     */

    iptable[ipcur].addr = addr;
    if (iptable[ipcur].name)
        free_mstring(iptable[ipcur].name);

    memcpy(&tmp, &addr, sizeof(tmp));
#ifndef USE_IPV6
    ipname = new_tabled(inet_ntoa(tmp), STRING_ASCII);
#else
    ipname = new_tabled(inet6_ntoa(tmp), STRING_ASCII);
#endif

    iptable[ipcur].name = ipname;

    ipcur = (ipcur+1) % IPSIZE;

    /* If we have the erq and may use it, lookup the real hostname */
    if (erq_demon >= 0 && useErq)
    {
#ifndef USE_IPV6
        send_erq(ERQ_HANDLE_RLOOKUP, ERQ_RLOOKUP, (char *)&addr.s_addr, sizeof(addr.s_addr));
#else
        send_erq(ERQ_HANDLE_RLOOKUPV6, ERQ_RLOOKUPV6, get_txt(ipname)
                , mstrsize(ipname));
#endif
    }

    return iptable[ipcur].name;
}

#endif /* ERQ_DEMON */

/* End of ERQ Support */
/*=========================================================================*/

/*-------------------------------------------------------------------------*/
void
remove_stale_player_data (void)

/* GC and statistics support: Remove all input_to and prompt infos
 * referencing destructed objects.
 */

{
    int i;

    for(i = 0 ; i < MAX_PLAYERS; i++)
    {
        input_t * ih, * prev;
        object_t *ob;

        if (all_players[i] == NULL)
            continue;

        /* Remove stale input_to data */
        for ( prev = NULL, ih = all_players[i]->input_handler; ih != NULL; )
            if (ih->type == INPUT_TO)
            {
                input_to_t *tmp = (input_to_t*) ih;
                ob = callback_object(&(tmp->fun));
                if (ob)
                {
                    prev = ih;
                    ih = ih->next;
                }
                else
                {
                    /* The object has selfdestructed */

                    if (prev == NULL)
                    {
                        set_noecho(all_players[i]
                                  , ih->next ? ih->next->noecho : 0
                                  , ih->next ? ih->next->local : MY_FALSE
                                  , MY_TRUE);
                        all_players[i]->input_handler = ih->next;
                    }
                    else
                    {
                        prev->next = ih->next;
                    }

                    ih = ih->next;

                    free_input_to(tmp);
                }
            }
            else
            {
                prev = ih;
                ih = ih->next;
            }

        /* Remove stale snooping monsters */
        ob = all_players[i]->snoop_by;
        if (ob && !O_IS_INTERACTIVE(ob) && !check_object(ob))
        {
            free_object(ob, "remove_stale_player_data");
            all_players[i]->snoop_by = NULL;
        }

        /* Remove a stale modify_command object */
        ob = all_players[i]->modify_command;
        if (ob && !check_object(ob))
        {
            free_object(ob, "remove_stale_player_data");
            all_players[i]->modify_command = NULL;
        }
    } /* for (i) */
} /* remove_stale_player_data() */

/*-------------------------------------------------------------------------*/
size_t
show_comm_status (strbuf_t * sbuf, Bool verbose UNUSED)

/* Return the amount of memory used by the comm module.
 */

{
#if defined(__MWERKS__)
#    pragma unused(verbose)
#endif
    size_t sum;
    int i;

    remove_stale_player_data();

    sum = 0;

    for (i = 0; i <= max_player; i++)
    {
        interactive_t *pl;
        input_t *ih;

        pl = all_players[i];
        if (!pl)
            continue;

        sum += sizeof(*pl);

        for (ih = pl->input_handler; ih != NULL; ih = ih->next)
            switch(ih->type)
            {
            case INPUT_TO:
                sum += sizeof(input_to_t);
                break;
            case INPUT_ED:
                sum += ed_buffer_size(ih);
                break;
            }
    }

    if (sbuf)
        strbuf_addf(sbuf, "Comm structures\t\t\t\t %9zu\n", sum);
    return sum;
} /* show_comm_status() */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
clear_comm_refs (void)

/* GC support: Clear all refs the module might have.
 */

{
    int i;

    for (i = 0; i < MAX_OUTCONN; i++)
    {
        if (outconn[i].status != ocNotUsed)
        {
            if (outconn[i].curr_obj)
                clear_object_ref(outconn[i].curr_obj);
        }
    }

#ifdef ERQ_DEMON
    for (i = sizeof (pending_erq) / sizeof (*pending_erq); --i >= 0;)
    {
        clear_ref_in_vector(&pending_erq[i].fun, 1);
    }
#endif /* ERQ_DEMON */
} /* clear_comm_refs() */

/*-------------------------------------------------------------------------*/
void
count_comm_refs (void)

/* GC support: count any ref the module has.
 */

{
    int i;

    for (i = 0; i < MAX_OUTCONN; i++)
    {
        if (outconn[i].status != ocNotUsed)
        {
            if (outconn[i].curr_obj)
            {
                svalue_t sv;

                put_object(&sv, outconn[i].curr_obj);
                count_ref_in_vector(&sv, 1);
            }
        }
    }

#ifdef ERQ_DEMON
    for(i = 0; i < IPSIZE; i++) {
        if (iptable[i].name)
            count_ref_from_string(iptable[i].name);
    }

    for (i = sizeof (pending_erq) / sizeof (*pending_erq); --i >= 0;)
    {
        count_ref_in_vector(&pending_erq[i].fun, 1);
    }
#endif /* ERQ_DEMON */
} /* count_comm_refs() */

/*-------------------------------------------------------------------------*/
void
clear_input_refs (input_t *i)

/* GC Support: Clear all references from input_t <i>. 
 */

{
    switch (i->type)
    {
    case INPUT_TO:
        {
            input_to_t *it = (input_to_t*) i;

            clear_ref_in_callback(&(it->fun));
            clear_ref_in_vector(&(it->input.prompt), 1);

            break;
        }
    case INPUT_ED:
       clear_ed_buffer_refs(i);
       break;
    }
} /* clear_input_refs() */

/*-------------------------------------------------------------------------*/
void
count_input_refs (input_t *i)

/* GC Support: Count  all references from input_t <i>. 
 */

{
    switch (i->type)
    {
    case INPUT_TO:
        {
            input_to_t *it = (input_to_t*) i;

            count_ref_in_callback(&(it->fun));
            count_ref_in_vector(&(it->input.prompt), 1);

            break;
        }
    case INPUT_ED:
        count_ed_buffer_refs(i);
        break;
    }
} /* count_input_refs() */

#endif /* GC_SUPPORT */


/*=========================================================================*/

/*-------------------------------------------------------------------------*/
char *
query_host_name (void)

/* Return the hostname (and just the hostname, not the full domain name).
 * The result is a pointer to a static array!
 * Called by lex.c, main.c and swap.c .
 */

{
    return host_name;
} /* query_host_name() */

/*-------------------------------------------------------------------------*/
char *
get_host_ip_number (void)

/* Return the IP address of the host.
 * The result is a newly allocated string.
 * Called by lex.c .
 */

{
#ifndef USE_IPV6
    char buf[INET_ADDRSTRLEN+3];

    sprintf(buf, "\"%s\"", inet_ntoa(host_ip_number));
#else
    char buf[INET6_ADDRSTRLEN+3];

    sprintf(buf, "\"%s\"", inet6_ntoa(host_ip_number));
#endif
    return string_copy(buf);
} /* query_host_ip_number() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_remove_interactive (svalue_t *sp)

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
    interactive_t *victim;

    if (O_SET_INTERACTIVE(victim, sp->u.ob)
     && !victim->closing
     && !victim->do_close)
    {
        if (victim->message_length) {
            command_giver = victim->ob;
            add_message_flush();

            /* message_flush takes always directly effect on the
             * socket. No apply() is involved.
             */
        }
        victim->do_close = FLAG_DO_CLOSE;
    }
    free_svalue(sp);
    return sp - 1;
} /* f_remove_interactive() */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

void
count_comm_extra_refs (void)

/* Count all the refs to verify the normal refcounting. */

{
    int i;

#ifdef ERQ_DEMON
    for (i = sizeof(pending_erq) / sizeof(*pending_erq); --i >= 0; )
        count_extra_ref_in_vector(&pending_erq[i].fun, 1);
#endif /* ERQ_DEMON */

    for (i = 0; i < MAX_PLAYERS; i++)
    {
        object_t *ob;
        input_t *ih;

        if (all_players[i] == 0)
            continue;
        all_players[i]->ob->extra_ref++;
        if ( NULL != (ob = all_players[i]->snoop_by) ) {
            interactive_t *ip;

            if (!(O_SET_INTERACTIVE(ip, ob)))
            {
                /* snooping monster */
                ob->extra_ref++;
            }
        } /* end of snoop-processing */

        for ( ih = all_players[i]->input_handler; ih; ih = ih->next)
            switch(ih->type)
            {
            case INPUT_TO:
                {
                    input_to_t *it = (input_to_t*) ih;
                    count_callback_extra_refs(&(it->fun));
                    count_extra_ref_in_vector(&it->input.prompt, 1);
                    break;
                }
            case INPUT_ED:
                count_ed_buffer_extra_refs(ih);
                break;
            }

        if ( NULL != (ob = all_players[i]->modify_command) )
            count_extra_ref_in_object(ob);
        count_extra_ref_in_vector(&all_players[i]->prompt, 1);
    }
} /* count_comm_extra_refs() */

#endif /* DEBUG */

/*-------------------------------------------------------------------------*/
svalue_t *
f_send_udp (svalue_t *sp)

/* EFUN: send_udp()
 *
 *   int send_udp(string host, int port, bytes message)
 *   int send_udp(string host, int port, int * message)
 *
 * Sends The message in an UDP packet to the given host and port
 * number. Causes a privilege violation.
 * The message can be given either as string, or as array of
 * bytes. The latter variant allows to send binary data as well.
 * Returns 1 on success, 0 on failure.
 *
 * Note: On some machines a failed send_imp() will not be registered
 * until the next send_imp() - the latter one might return '0' even
 * if itself was successful.
 */

{
    char *to_host = NULL;
    int to_port;
    char *msg;
    size_t msglen;
#ifndef USE_IPV6
    int ip1, ip2, ip3, ip4;
#endif /* USE_IPV6 */
    struct sockaddr_in name;
    struct hostent *hp;
    int ret = 0;
    svalue_t *firstarg; /* store the first argument */
    
    firstarg = sp-2;  /* hostname */
    
    switch(0) { default: /* try {...} */

        /* Set msg/msglen to the data of the message to send */

        if (sp->type == T_BYTES)
        {
            msg = get_txt(sp->u.str);
            msglen = mstrsize(sp->u.str);
        }
        else /* it's an array */
        {
            vector_t *v;
            svalue_t *svp;
            char *cp;
            p_int j;

            v = sp->u.vec;
            msglen = VEC_SIZE(v);
            /* allocate memory and push error handler onto stack */
            cp = msg = xalloc_with_error_handler(msglen);
            if (!msg) {
                errorf("Out of memory (%zu bytes) in send_udp() for "
                       "temporary buffer.\n", msglen);
            }
            sp = inter_sp;
            
            svp = &v->item[0];
            for (j = (p_int)msglen; --j >= 0; )
            {
                svalue_t* item = get_rvalue(svp++, NULL);
                if (item == NULL || item->type != T_NUMBER)
                {
                    errorf("Bad arg 3 to send_udp(): got %s*, "
                          "expected string/int*.\n", typename(svp[-1].type));
                    /* NOTREACHED */
                    return sp;
                }

                *cp++ = (char)item->u.number;
            }
        }

        /* Is this call valid? */

        if (!privilege_violation(STR_SEND_UDP, firstarg, sp))
            break;

        if (udp_s < 0)
            break;

        /* Determine the destination address */

        {
            size_t adrlen;

            adrlen = mstrsize(firstarg->u.str);
            /* as there are no runtime error raised below, we just xallocate
             * and don't bother with an error handler. */
            to_host = xalloc(adrlen+1);
            if (!to_host)
            {
                errorf("Out of memory (%zu bytes) in send_udp() for host address\n"
                     , (adrlen+1));
                /* NOTREACHED */
            }
            memcpy(to_host, get_txt(firstarg->u.str), adrlen);
            to_host[adrlen] = '\0';
        }
        to_port = (sp-1)->u.number;

#ifndef USE_IPV6
        if (sscanf(to_host, "%d.%d.%d.%d", &ip1, &ip2, &ip3, &ip4) == 4)
        {
            name.sin_addr.s_addr = inet_addr(to_host);
            name.sin_family = AF_INET;
        }
        else
        {
            /* TODO: Uh-oh, blocking DNS in the execution thread */
            hp = gethostbyname(to_host);
            if (hp == 0)
                break;            
            memcpy(&name.sin_addr, hp->h_addr, (size_t)hp->h_length);
            name.sin_family = AF_INET;
        }

#else /* USE_IPV6 */

        /* TODO: Uh-oh, blocking DNS in the execution thread */
        hp = gethostbyname2(to_host, AF_INET6);
        if (hp == 0) hp = gethostbyname2(to_host, AF_INET);
        if (hp == 0) break;
        memcpy(&name.sin_addr, hp->h_addr, (size_t)hp->h_length);

        if (hp->h_addrtype == AF_INET)
        {
            CREATE_IPV6_MAPPED(&name.sin_addr, *(u_int32_t*)hp->h_addr_list[0]);
        }
        name.sin_family = AF_INET6;
#endif /* USE_IPV6 */

        name.sin_port = htons(to_port);

        /* Send the message. */
#ifndef SENDTO_BROKEN
        if (sendto(udp_s, msg, msglen, 0,
               (struct sockaddr *)&name, sizeof(name)) != (int)msglen)
#endif
            break;
        ret = 1;
    }
    /* Cleanup - an allocated buffer for the message will be on the stack 
     * above the arguments, therefore clean everything from the first argument
     * (including) to sp.
     */
    sp = pop_n_elems((sp-firstarg)+1, sp);
    xfree(to_host);
    
    /*Return the result */
    sp++;
    put_number(sp, ret);
    return sp;
    
} /* f_send_udp() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_binary_message (svalue_t *sp)

/* EFUN: binary_message()
 *
 *   int binary_message(int *|bytes message, int flags)
 *
 * Flush output and send output directly with write WITHOUT IAC QUOTING.
 * The message may contain zeroes if given as int *.
 * The messages goes to this_object(), but only if interactive.
 * Returned value: number of characters actually written.
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
 * on a synchronous transmission.
 */

{
    string_t *msg;
    mp_int wrote = 0, i;
    svalue_t *svp;
    interactive_t *ip;
    object_t *save_command_giver;

    /* Set message to the data to be sent, and size to its length. */

    if (sp[-1].type == T_POINTER)
    {
        char *p;
        size_t size;

        size = VEC_SIZE(sp[-1].u.vec);
        msg = alloc_mstring(size);
        if (!msg)
          errorf("Out of memory (%zu bytes) in binary_message()",size);
        for (i = (mp_int)size, svp = sp[-1].u.vec->item, p = get_txt(msg)
            ; --i >= 0; svp++)
        {
            svalue_t* item = get_rvalue(svp, NULL);
            if (item == NULL || item->type != T_NUMBER)
            {
                free_mstring(msg);
                errorf("Bad arg 1 to binary_message(): got %s*, "
                      "expected string/int*.\n", typename(svp->type));
                /* NOTREACHED */
                return sp;
            }
            *p++ = (char)item->u.number;
        }
        msg->info.unicode = STRING_BYTES;
    }
    else /* it's a byte sequence */
    {
        msg = ref_mstring(sp[-1].u.str);
    }

    /* Send the message */

    i = 0;
    if (O_SET_INTERACTIVE(ip, current_object)
     && !ip->do_close)
    {
        save_command_giver = command_giver;
        command_giver = current_object;

        if (sp->u.number & 1)
        {
            /* Write before flush... */

            add_message_str(msg);

            if (sp->u.number & 2)
                add_message_flush();

            wrote = mstrsize(msg);
        }
        else
        {
            /* Flush, then write. */
            add_message_flush();

            /* Since all pending data was flushed, we can write directly
             * to the socket now.
             */

            if (comm_socket_write(get_txt(msg), mstrsize(msg), ip, 0))
                wrote = mstrsize(msg);

        } /* if (type of write) */

        command_giver = save_command_giver;
    } /* end if interactive */

    sp--;
    free_mstring(msg);
    free_svalue(sp);
    put_number(sp, wrote);
    return sp;
} /* f_binary_message() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_exec (svalue_t *sp)

/* EFUN exec()
 *
 *   object exec(object new, object old)
 *
 * Switch the network connection from <old> to <new>. If <new> is already
 * interactive, its connection will be switched to <old>.
 *
 * It is used to load different "user objects" or to reconnect
 * link dead users.
 *
 * If <old> was command_giver, <news> will be the new command_giver.
 *
 * The call is validated by master->valid_exec() and returns 0 on
 * failure, and 1 on success.
 */

{
    int rc;
    object_t *ob;
    object_t *obfrom;

    rc = 0;

    ob = sp[-1].u.ob;
    obfrom = sp[0].u.ob;

    do {
        svalue_t *v;
        interactive_t *stale_interactive, *ip;
        object_t *save_command;

        /* Ask the master if this exec() is ok. */
        push_ref_string(inter_sp, current_prog->name);
          /* TODO: FinalFrontier suggests 'current_object->prog->name' */
        push_ref_object(inter_sp, ob, "exec");
        push_ref_object(inter_sp, obfrom, "exec");
        v = apply_master(STR_VALID_EXEC, 3);
        if (!v || v->type != T_NUMBER || v->u.number == 0)
            break;

        /* stale_interactive becomes the former interactive _if_ it
         * still is an interactive_t.
         */
        if (!(O_SET_INTERACTIVE(stale_interactive, ob)))
        {
            stale_interactive = NULL;
        }

        if (!(O_SET_INTERACTIVE(ip, obfrom)))
            errorf("Bad argument 2 to exec(): not interactive.\n");

        /* When we have to have an out of memory error, have it before pointers
         * get changed.
         */
        assert_shadow_sent(ob);

        save_command = command_giver;

        /* If <ob> has a connection, flush it */
        if (stale_interactive)
        {
            if (stale_interactive->message_length)
            {
                command_giver = ob;
                add_message_flush();
            }
        }

        /* Flush the connection of <obfrom> */

        if (ip->message_length) {
            command_giver = obfrom;
            add_message_flush();
        }
        command_giver = save_command;

        /* Switch a possible snooper */

        if (ip->snoop_on)
            ip->snoop_on->snoop_by = ob;

        /* Switch the interactive */

        O_GET_INTERACTIVE(ob) = ip;
        O_GET_INTERACTIVE(obfrom) = NULL;
        ob->flags |= O_ONCE_INTERACTIVE;
        ip->ob = ob;
        ip->catch_tell_activ = MY_TRUE;

        if (stale_interactive)
        {
            /* Tie <ob>s stale connection to <obfrom>. */

            O_GET_INTERACTIVE(obfrom) = stale_interactive;
            stale_interactive->ob = obfrom;
            if (stale_interactive->snoop_on)
                stale_interactive->snoop_on->snoop_by = obfrom;
            stale_interactive->catch_tell_activ = MY_TRUE;
        }
        else
        {
            /* Clean up <obfrom> after the loss of connection */

            obfrom->flags &= ~O_ONCE_INTERACTIVE;
            check_shadow_sent(obfrom);

            ref_object(ob, "exec");
            free_object(obfrom, "exec");
        }

        /* If this_player() or this_interactive() point to one of the
         * involved objects, switch it too.
         */
        if (obfrom == command_giver)
            command_giver = ob;
        else if (ob == command_giver)
            command_giver = obfrom;

        if (obfrom == current_interactive)
            current_interactive = ob;
        else if (ob == current_interactive)
            current_interactive = obfrom;

        rc = 1;
    }while(0);

    free_svalue(sp--);
    free_svalue(sp); /* object might have been destructed */
    put_number(sp, rc);

    return sp;
} /* f_exec() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_interactive (svalue_t *sp)

/* EFUN interactive()
 *
 *   int interactive(object ob)
 *
 * Return non-zero if ob, or when the argument is omitted, this
 * object(), is an interactive user. Will return 1 if the
 * object is interactive, else 0.
 */

{
    int i;
    object_t *ob;
    interactive_t *ip;

    ob = sp->u.ob;
    (void)O_SET_INTERACTIVE(ip, ob);
    i = ip && !ip->do_close;
    deref_object(ob, "interactive");
    put_number(sp, i);

    return sp;
} /* f_interactive() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_input_to (svalue_t *sp, int num_arg)

/* EFUN input_to()
 *
 *   void input_to(string fun)
 *   void input_to(string fun, int flag, ...)
 *
 * Enable next line of user input to be sent to the local
 * function fun as an argument. The input line will not be
 * parsed, only when it starts with a "!" (like a kind of shell
 * escape) (this feature may be disabled).
 * The function <fun> may be static, but must not be private (or
 * it won't be found).
 *
 * Note that fun is not called immediately but after pressing the
 * RETURN key.
 *
 * If input_to() is called more than once in the same execution,
 * only the first call has any effect.
 *
 * The optional 3rd and following args will be passed as second and
 * subsequent args to the function fun. (This feature is was
 * added only recently, to avoid the need for global variables)
 */

{
    svalue_t *arg;       /* Pointer to the arguments of the efun */
    svalue_t *extra_arg; /* Pointer to the extra arguments of the efun */
    int iflags;          /* The flags passed to input_to() */
    int flags;           /* The flags as required for .noecho */
    input_to_t *it;
    int extra;           /* Number of extra arguments */
    int error_index;

    arg = sp - num_arg + 1;

    /* Extract the arguments */

    iflags = 0;
    extra = 0;
    extra_arg = arg + 1;

    if (arg[0].type != T_CLOSURE && arg[0].type != T_STRING)
    {
        vefun_bad_arg(1, sp);
        /* NOTREACHED */
        return NULL;
    }

    if (num_arg > 1)
    {
        if (arg[1].type != T_NUMBER)
        {
            vefun_bad_arg(2, sp);
            /* NOTREACHED */
            return NULL;
        }

        iflags = arg[1].u.number;
        extra = num_arg - 2;
        extra_arg = arg + 2;
    }

    {
        interactive_t *ip;

        if (command_giver != NULL
         && O_SET_INTERACTIVE(ip, command_giver)
         && !ip->tn_enabled
           )
        {
            if (!(iflags & INPUT_NOECHO) != !(ip->noecho & NOECHO_MASK))
            {
                warnf("input_to(): Change in NOECHO mode requested for object '%s' "
                      "with telnet disabled.\n"
                     , get_txt(command_giver->name)
                     );
            }

            if (!(iflags & INPUT_CHARMODE) != !(ip->noecho & CHARMODE_MASK)
             && (iflags & INPUT_NO_TELNET) == 0
               )
            {
                warnf("input_to(): Change in CHARMODE mode requested for object '%s' "
                      "with telnet disabled.\n"
                     , get_txt(command_giver->name)
                     );
            }
        }
    }

    /* Setup the flags required for 'noecho' */
    flags =   ((iflags & INPUT_NOECHO)      ? NOECHO_REQ   : 0)
            | ((iflags & INPUT_CHARMODE)    ? CHARMODE_REQ : 0)
            | ((iflags & INPUT_IGNORE_BANG) ? IGNORE_BANG  : 0)
          ;

    /* Check the arguments */

    if (iflags & INPUT_PROMPT)
    {
        if (num_arg <= 2)
        {
            errorf("Missing prompt argument to input_to().\n");
            /* NOTREACHED */
        }

        if (arg[2].type != T_STRING && arg[2].type != T_CLOSURE)
        {
            vefun_bad_arg(3, sp);
            /* NOTREACHED */
        }
    }

    if ((flags & IGNORE_BANG)
     && !privilege_violation4(STR_INPUT_TO, command_giver, 0, flags, sp))
    {
        do
        {
            free_svalue(sp--);
        } while (--num_arg);
        
        put_number(arg, 0); /* arg should equal sp+1 */
        return arg;
    }

    /* There is a chance that the privilege_violation() method destructed
     * the current object or the command_giver - return as if the call was
     * denied.
     */
    if (!check_object(current_object) || !check_object(command_giver))
    {
	do
	{
	    free_svalue(sp--);
	} while (--num_arg);
	
	put_number(arg, 0); /* arg should equal sp+1 */
	return arg;
    }

    /* Allocate and setup the input_to structure */

    xallocate(it, sizeof *it, "new input_to");
    init_empty_callback(&(it->fun));
    put_number(&(it->input.prompt), 0);

    /* If SET_PROMPT was specified, collect it */

    if (iflags & INPUT_PROMPT)
    {
        transfer_svalue(&(it->input.prompt), arg+2);
        extra--;
        extra_arg++;
    }

    /* Parse the extra args for the call */

    if (arg[0].type == T_STRING)
    {
        error_index = setup_function_callback(&(it->fun), current_object
                                             , arg[0].u.str
                                             , extra, extra_arg
                                             );
        free_string_svalue(arg);
    }
    else if (arg[0].type == T_CLOSURE)
        error_index = setup_closure_callback(&(it->fun), arg
                                            , extra, extra_arg
                                            );
    else
        error_index = 1;

    if (error_index >= 0)
    {
        free_input_to(it);
        vefun_bad_arg(error_index + num_arg - extra + 1, arg-1);
        /* NOTREACHED */
        return arg-1;
    }

    /* At this point the call back should be bound to an existing object - but
     * as a sanity check we test it.
     */
    if (NULL == callback_object(&(it->fun)))
    {
        free_input_to(it);
	put_number(arg, 0); /* arg should equal sp+1 */
	return arg;
    }

    /* Try setting the input_to. On success, return 1. */

    if (set_call( command_giver, it, (char)flags
                , (iflags & INPUT_NO_TELNET) != 0
                , (iflags & INPUT_APPEND) != 0
                )
       )
    {
        put_number(arg, 1);
        return arg;
    }

    /* input_to() was not allowed - return 0. */

    free_input_to(it);
    put_number(arg, 0);
    return arg;
} /* v_input_to() */

/*-------------------------------------------------------------------------*/
static void
free_input_to (input_to_t *it)

/* Deallocate the input_to structure <it> and all referenced memory.
 */

{
    free_callback(&(it->fun));
    free_svalue(&(it->input.prompt));
    xfree(it);
} /* free_input_to() */

/*-------------------------------------------------------------------------*/
static void
free_input_handler (input_t *ih)

/* Deallocate the input_t structure <ih> and all referenced memory.
 */

{
    switch (ih->type)
    {
    case INPUT_TO:
        free_input_to((input_to_t*) ih);
        break;

    case INPUT_ED:
        free_ed_buffer(ih);
        break;
    }

} /* free_input_handler() */

/*-------------------------------------------------------------------------*/
void
abort_input_handler (interactive_t *ip)

/* Called from destruct_object to finish some input handlers,
 * specifically save all ed sessions.
 */
{
    input_t ** ptr = &(ip->input_handler);

    while (*ptr)
    {
        switch ((*ptr)->type)
        {
        case INPUT_ED:
            {
                input_t * ed_buf = *ptr;

                *ptr = (*ptr)->next;
                save_ed_buffer(ed_buf);
                break;
            }

        default:
            ptr = &((*ptr)->next);
            break;
        }
    }
} /* abort_input_handler() */

/*-------------------------------------------------------------------------*/
void
add_input_handler (interactive_t *ip, input_t *ih, Bool append)

/* Put the input handler <ih> in front of the input handler list of
 * the interactive <ip>.
 * If <append> is TRUE, the handler is appended to the list.
 */
{
    if (!append || ip->input_handler == NULL)
    {
        ih->next = ip->input_handler;
        ip->input_handler = ih;
    }
    else
    {
        input_t * ptr = ip->input_handler;

        while (ptr->next != NULL)
            ptr = ptr->next;

        ptr->next = ih;
        ih->next = NULL;
    }

    if (ih->noecho || ip->noecho)
        set_noecho(ip, ih->noecho, ih->local, MY_FALSE);

} /* add_input_handler() */

/*-------------------------------------------------------------------------*/
void
remove_input_handler (interactive_t *ip, input_t *ih)

/* Remove the input handler <ih> from the input handler list of
 * the interactive <ip>. <ih> is not freed.
 */
{
    input_t * ptr = ip->input_handler;

    if (ptr == ih)
    {
        ip->input_handler = ih->next;
        return;
    }

    while (ptr)
    {
        if (ptr->next == ih)
        {
            ptr->next = ih->next;
            break;
        }
        ptr = ptr->next;
    }

} /* remove_input_handler() */

/*-------------------------------------------------------------------------*/
input_t *
get_input_handler (interactive_t *ip, input_type_t type)

/* Returns the first input handler from <ip> of type <type>.
 */
{
    input_t *ih;

    for (ih = ip->input_handler; ih; ih = ih->next)
        if (ih->type == type)
            return ih;

    return NULL;
} /* get_input_handler */

/*-------------------------------------------------------------------------*/
svalue_t *
v_find_input_to (svalue_t *sp, int num_arg)

/* EFUN: find_input_to()
 *
 *   int find_input_to (object player, string|closure|object fun)
 *   int find_input_to (object player, object ob, string fun)
 *
 * Find the input_to most recently added to the interactive <player> object
 * matching the <fun> argument:
 *  - <fun> is a string: the input_to functionname has to match
 *  - <fun> is an object: the object the input_to function is bound to has
 *                        to match
 *  - <fun> is a closure: the input_to closure has to match.
 * If both <ob> and <fun> are specified, both the object and the function name
 * have to match.
 *
 * Return -1 if not found, or the position in the input_to stack (0 being
 * _least_ recently added input_to).
 */

{
    svalue_t *arg;  /* Pointer to the arguments of the efun */
    int       rc;   /* Resultvalue */

    arg = sp - num_arg + 1;

    if (num_arg > 2)
    {
        if (arg[1].type == T_OBJECT && num_arg > 2 && arg[2].type != T_STRING)
        {
            vefun_bad_arg(3, sp);
            /* NOTREACHED */
            return NULL;
        }

        if (arg[1].type != T_OBJECT
           )
        {
            vefun_bad_arg(2, sp);
            /* NOTREACHED */
            return NULL;
        }
    }

    /* Process the command, terminating out when possible */
    do
    {
        input_t       *ih;
        interactive_t *ip;

        /* Get the interactive object.
         * If there is none, or if it is closing down or doesn't have
         * an input_to set, fail.
         */
        if (!(O_SET_INTERACTIVE(ip, arg[0].u.ob))
         || ip->closing || ip->input_handler == NULL
           )
        {
            rc = -1;
            break;
        }

        /* Search for the right input_to */

        for ( ih = ip->input_handler
            ; ih != NULL
            ; ih = ih->next)
        {
            input_to_t *it;
            Bool found = MY_FALSE;

            if (ih->type != INPUT_TO)
                continue;
            it = (input_to_t*) ih;

            switch (arg[1].type)
            {
            case T_STRING:
                if (!it->fun.is_lambda
                 && mstreq(it->fun.function.named.name, arg[1].u.str))
                    found = MY_TRUE;
                break;

            case T_OBJECT:
                if (num_arg > 2)
                {
                    if (callback_object(&(it->fun)) == arg[1].u.ob
                     && !it->fun.is_lambda
                     && it->fun.function.named.name == arg[2].u.str
                       )
                        found = MY_TRUE;
                }
                else
                {
                    if (callback_object(&(it->fun)) == arg[1].u.ob)
                        found = MY_TRUE;
                }
                break;

            case T_CLOSURE:
                if (it->fun.is_lambda
                 && closure_eq(&(it->fun.function.lambda), arg+1))
                    found = MY_TRUE;
                break;

            default:
                /* TODO: Wouldn't errorf() suffice? */
                fatal("Unsupported argument type %d\n", arg[1].type);
                break;
            }

            if (found)
                break;
        }

        if (ih != NULL)
        {
            /* We found the input_to: now count at which position it is */
            for ( rc = 0; ih->next != NULL; ih = ih->next)
                if (ih->type == INPUT_TO)
                    rc++;
            break;
        }

        /* At this point, we didn't find the input_to */
        rc = -1;
    } while (0);

    /* Return the result */
    sp = pop_n_elems(num_arg, sp);
    sp++;
    put_number(sp, rc);

    return sp;
} /* f_find_input_to() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_remove_input_to (svalue_t *sp, int num_arg)

/* EFUN: remove_input_to()
 *
 *   int remove_input_to (object player)
 *   int remove_input_to (object player, string|closure|object fun)
 *   int remove_input_to (object player, object ob, string fun)
 *
 * Remove a pending input_to from the interactive <player> object.
 * If the optional <fun> is not given, the most recently added input_to
 * is removed.
 *
 * If the optional <fun> is given, the efun tries to find and remove the
 * most recently added input_to matching the <fun> argument:
 *  - <fun> is a string: the input_to functionname has to match
 *  - <fun> is an object: the object the input_to function is bound to has
 *                        to match
 *  - <fun> is a closure: the input_to closure has to match.
 * If both <ob> and <fun> are specified, both the object and the function name
 * have to match.
 *
 * Return 1 on success, or 0 on failure (no input_to found, object is not
 * interactive or has no input_to pending).
 */

{
    svalue_t      *arg;  /* Pointer to the arguments of the efun */
    int            rc;   /* Resultvalue */
    interactive_t *ip;
    Bool           removedFirst;

    /* Get the arguments */
    arg = sp - num_arg + 1;

    if (num_arg > 2)
    {
        if (arg[1].type == T_OBJECT && arg[2].type != T_STRING)
        {
            vefun_bad_arg(3, sp);
            /* NOTREACHED */
            return NULL;
        }
        if (arg[1].type != T_OBJECT)
        {
            vefun_bad_arg(2, sp);
            /* NOTREACHED */
            return NULL;
        }
    }


    /* Process the command, bailing out whenever necessary */
    do
    {
        input_t * prev;
        input_t    *ih;

        removedFirst = MY_FALSE;

        /* Get the interactive object.
         * If there is none, or if it is closing down or doesn't have
         * an input_to set, fail.
         */
        if (!(O_SET_INTERACTIVE(ip, arg[0].u.ob))
         || ip->closing || ip->input_handler == NULL
           )
        {
            rc = 0;
            break;
        }

        /* Search for the right input_to */

        for (prev = NULL, ih = ip->input_handler
            ; ih != NULL
            ; prev = ih, ih = ih->next)
        {
            input_to_t *it;
            Bool found = MY_FALSE;

            if (ih->type != INPUT_TO)
                continue;
            it = (input_to_t*) ih;

            /* If no filter argument has been given, just remove
             * the first input to.
             */
            if (num_arg < 2)
                break;

            switch (arg[1].type)
            {
            case T_STRING:
                if (!it->fun.is_lambda
                 && mstreq(it->fun.function.named.name, arg[1].u.str))
                    found = MY_TRUE;
                break;

            case T_OBJECT:
                if (num_arg > 2)
                {
                    if (callback_object(&(it->fun)) == arg[1].u.ob
                     && !it->fun.is_lambda
                     && it->fun.function.named.name == arg[2].u.str
                       )
                        found = MY_TRUE;
                }
                else
                {
                    if (callback_object(&(it->fun)) == arg[1].u.ob)
                        found = MY_TRUE;
                }
                break;

            case T_CLOSURE:
                if (it->fun.is_lambda
                 && closure_eq(&(it->fun.function.lambda), arg+1))
                    found = MY_TRUE;
                break;

            default:
                /* TODO: Wouldn't errorf() suffice? */
                fatal("Unsupported argument type %d\n", arg[1].type);
                break;
            }

            if (found)
                break;
        }

        if (ih != NULL)
        {
            /* We found the input_to: remove it */
            if (prev == NULL)
            {
                ip->input_handler = ih->next;
                removedFirst = MY_TRUE;
            }
            else
                prev->next = ih->next;

            free_input_to((input_to_t*)ih);
            rc = 1;
            break;
        }

        /* At this point, nothing worked: failure */
        rc = 0;
    } while (0);

    if (rc && removedFirst)
    {
        if (ip->noecho)
            ip->noecho |= NOECHO_STALE;
        set_noecho(ip, ip->input_handler ? ip->input_handler->noecho : ip->noecho
                     , ip->input_handler ? ip->input_handler->local : MY_FALSE
                     , MY_FALSE
                  );
    }

    /* Return the result */
    sp = pop_n_elems(num_arg, sp);
    push_number(sp, rc);

    return sp;
} /* v_remove_input_to() */

/*-------------------------------------------------------------------------*/
svalue_t  *
f_input_to_info (svalue_t *sp)

/* EFUN: input_to_info()
 *
 *   mixed * input_to_info (object player)
 *
 * Construct an array of all input_to's pending for this interactive <player>.
 * The first entry in the array is the least recently added input_to, the
 * last element the most recently added one.
 * Every item in the array is itself an array of 2 or more entries:
 *  0:   The object (only if the function is a string).
 *  1:   The function (string or closure).
 *  2..: The argument(s).
 */
{
    vector_t      *v;
    int            num_pending;
    input_t       *ih;
    interactive_t *ip;

    /* Get the interactive object.
     * If there is none, or if it is closing down or doesn't have
     * an input_to set, the efun will return the empty array.
     */
    if (!(O_SET_INTERACTIVE(ip, sp->u.ob))
     || ip->closing || ip->input_handler == NULL
       )
    {
        num_pending = 0;
    }
    else
    {
        /* Count the number of pending input_tos.
         */
        for ( num_pending = 0, ih = ip->input_handler
            ; ih != NULL
            ; ih = ih->next)
            if (ih->type == INPUT_TO)
                num_pending++;
    }

    /* Allocate the result arrray and fill it in */
    v = allocate_array(num_pending);

    if (num_pending > 0)
    {
        int i;

        for (i = num_pending, ih = ip->input_handler
            ; --i >= 0
            ; ih = ih->next
            )
        {
            vector_t   *vv;
            object_t   *ob;
            input_to_t *it;

            if (ih->type != INPUT_TO)
            {
                i++;
                continue;
            }

            it = (input_to_t*) ih;

            ob = callback_object(&(it->fun));
            if (!ob)
                continue;

            /* Get the subarray */

            vv = allocate_array(2 + it->fun.num_arg);

            if (it->fun.is_lambda)
            {
                if (it->fun.function.lambda.x.closure_type == CLOSURE_LFUN)
                    put_ref_object( vv->item
                                  , it->fun.function.lambda.u.lambda->function.lfun.ob
                                  , "input_to_info");
                else
                    put_ref_object(vv->item, ob, "input_to_info");
                assign_svalue_no_free(&vv->item[1], &it->fun.function.lambda);
            }
            else
            {
                put_ref_object(vv->item, ob, "input_to_info");
                put_ref_string(vv->item + 1, it->fun.function.named.name);
            }

            if (it->fun.num_arg > 0)
            {
                svalue_t *source, *dest;
                int nargs;

                nargs = it->fun.num_arg;
                if (nargs > 1)
                    source = it->fun.arg.u.lvalue;
                else
                    source = &(it->fun.arg);
                dest = &vv->item[2];
                do {
                    assign_svalue_no_free(dest++, source++);
                } while (--nargs);
            }

            put_array(v->item + i, vv);
        }
    }

    /* Return the result */
    free_svalue(sp);
    put_array(sp, v);

    return sp;
} /* f_input_to_info() */

/*-------------------------------------------------------------------------*/


static inline void translate_bit(char c, int i, int *length, string_t *rc, unsigned int bitno)
/* static helper function to translatin bits to characters in get_charset */
{
  if (c & (1 << bitno))
    get_txt(rc)[(*length)++] = (char)(i * 8 + bitno);
} /* translate_bit */

static void
get_charset (svalue_t * sp, bool as_string, char charset[16])

/* Translate the <charset> into an svalue and store it into <sp>:
 *   <as_string> == false: result is a bitvector array
 *   <as_string> == true:  result is a string.
 */

{
    put_number(sp, 0);

    if (!as_string)
    {
        vector_t * rc;
        int i;

        rc = allocate_uninit_array(16);
        if (!rc)
        {
            outofmemory("result array");
            /* NOTREACHED */
            return;
        }

        for (i = 0; i < 16; i++)
            put_number(rc->item+i, (unsigned char)charset[i]);

        put_array(sp, rc);
    }
    else
    {
        string_t * rc;
        int length, i;

        /* Count the number of bits set in the charset */
        for (i = length = 0; i < 16; i++)
        {
            char c = charset[i];
            length +=   ((c & 0x80) ? 1 : 0)
                      + ((c & 0x40) ? 1 : 0)
                      + ((c & 0x20) ? 1 : 0)
                      + ((c & 0x10) ? 1 : 0)
                      + ((c & 0x08) ? 1 : 0)
                      + ((c & 0x04) ? 1 : 0)
                      + ((c & 0x02) ? 1 : 0)
                      + ((c & 0x01) ? 1 : 0);
        }

        rc = alloc_mstring(length);
        if (!rc)
        {
            outofmemory("result string");
            /* NOTREACHED */
            return;
        }

        /* Translate the bits into characters */
        for (i = length = 0; i < 16; i++)
        {
            char c = charset[i];

            translate_bit(c, i, &length, rc, 0);
            translate_bit(c, i, &length, rc, 1);
            translate_bit(c, i, &length, rc, 2);
            translate_bit(c, i, &length, rc, 3);
            translate_bit(c, i, &length, rc, 4);
            translate_bit(c, i, &length, rc, 5);
            translate_bit(c, i, &length, rc, 6);
            translate_bit(c, i, &length, rc, 7);
        }
        rc->info.unicode = STRING_ASCII;

        put_string(sp, rc);
    } /* as_string */
} /* get_charset() */

/*-------------------------------------------------------------------------*/
static void
set_charset_from_vector (vector_t* vec, char charset[16], const char* fun, int argnum)

/* Reads the charset from an vector of int <vec>,
 * where each element represents a byte in the charset.
 * <fun> and <argnum> is the function name and argument
 * position for error messages.
 */

{
    mp_int i = (mp_int)VEC_SIZE(vec);
    svalue_t *svp;
    char *p;

    /* For compatibility reasons we accept arrays > 16 bytes as long
     * as they are <= 32 bytes long.
     */
    if (i > 32)
        errorf("Bad arg %d to %s: int[] too long (%"PRIdMPINT")\n", argnum, fun, i);
    if (i > 16)
        i = 16;

    for ( svp = vec->item, p = charset
        ; --i >= 0
        ; svp++, p++)
    {
        if (svp->type == T_NUMBER)
            *p = (char)svp->u.number;
    }
    memset(p, 0, (size_t)(charset + 16 - p));

} /* set_charset_from_vector */

/*-------------------------------------------------------------------------*/
static void
set_charset_from_string (string_t* str, char charset[16], const char* fun, int argnum)

/* Reads the charset from a string <str> containing all
 * chars of the charset.
 * <fun> and <argnum> is the function name and argument
 * position for error messages.
 */

{
    int i;
    char *p;

    memset(charset, 0, 16);
    for ( i = mstrsize(str), p = get_txt(str)
        ; i > 0
        ; i--, p++)
    {
        p_int ch;
        size_t clen = utf8_to_unicode(p, i, &ch);

        if (!clen)
        {
            i--;
            p++;
            continue;
        }

        i -= clen;
        p += clen;

        if (ch < 128)
            charset[ch / 8] |= 1 << (ch % 8);
    }

} /* set_charset_from_string */

/*-------------------------------------------------------------------------*/
svalue_t *
v_snoop (svalue_t *sp, int num_arg)

/* EFUN snoop()
 *
 *   object snoop(object snooper)
 *   object snoop(object snooper, object snoopee)
 *
 * Starts a snoop from 'snooper' on 'snoopee', or if 'snoopee' is not
 * given, terminates any snoop from 'snooper'.
 * On success, 'snoopee' is returned, else 0.
 *
 * The snoop is checked with the master object for validity.
 * It will also fail if the 'snoopee' is being snooped already or
 * if a snoop would result in a recursive snoop action.
 */

{
    int i;

    if (num_arg == 1)
    {
        i = set_snoop(sp->u.ob, 0);
    }
    else
    {
        i = set_snoop((sp-1)->u.ob, sp->u.ob);
        free_svalue(sp--);
    }
    free_svalue(sp);
    put_number(sp, i);

    return sp;
} /* v_snoop() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_users (svalue_t *sp)

/* EFUN users()
 *
 * Return a (possibly empty) vector of all interactive user objects.
 */

{
    object_t *ob;
    int n, num;
    vector_t *ret;
    interactive_t **user;
    svalue_t *svp;

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
            put_ref_object(svp, ob, "users");
            svp++;
        }
    }

    push_array(sp, ret);

    return sp;
} /* f_users() */

/*-------------------------------------------------------------------------*/
void
check_for_out_connections (void)

/* Check the list of pending outgoing connections if the connections
 * are still pending.
 * Activate those who succeeded, remove those which failed.
 *
 * To be called regularily from the backend.
 */

{
    int i, ret, err;
    socklen_t errlen;
    object_t *user;
    struct pollfd pfd;

    for (i = 0; i < MAX_OUTCONN; i++)
    {
        if (outconn[i].status == ocNotUsed)
            continue;

        if (!outconn[i].curr_obj) /* shouldn't happen */
        {
            socket_close(outconn[i].socket);
            outconn[i].status = ocNotUsed;
            continue;
        }

        if (outconn[i].status == ocLoggingOn)
        {
            /* LPC logon threw an error - clean up */
            debug_message("%s Error in net_connect(): logon "
                          "object '%s' threw an error.\n"
                         , time_stamp()
                         , outconn[i].curr_obj
                           ? get_txt(outconn[i].curr_obj->name)
                           : "<null>"
                         );

            outconn[i].status = ocNotUsed;
            if (outconn[i].curr_obj)
            {
                if (O_IS_INTERACTIVE(outconn[i].curr_obj))
                    remove_interactive(outconn[i].curr_obj, MY_FALSE);
                free_object(outconn[i].curr_obj, "net_connect");
            }
            socket_close(outconn[i].socket);
            continue;
        }

        if (outconn[i].curr_obj && (outconn[i].curr_obj->flags & O_DESTRUCTED))
        {
            socket_close(outconn[i].socket);
            free_object(outconn[i].curr_obj, "net_connect");
            outconn[i].status = ocNotUsed;
            continue;
        }

        /* Check whether the socket is ready.
         * (Otherwise SO_ERROR won't give meaningful answers on some systems.)
         */
        pfd.fd = outconn[i].socket;
        pfd.events = POLLOUT;
        ret = poll(&pfd, 1, 0);

        if (ret == -1)
            err = errno;
        else if (ret == 0)
            continue; /* Not yet ready */
        else
        {
            /* Check for connect errors */
            errlen = sizeof(err);
            ret = getsockopt(outconn[i].socket, SOL_SOCKET, SO_ERROR, &err, &errlen);
            if (ret == -1)
                err = errno;
        }

        if (err)
        {
            switch(err)
            {
            case EALREADY: /* still trying */
            case EINTR:
                continue;
            case EISCONN: /* we are connected! */
                break;
            default:
                /* Error with connection, call logon() with the failure flag
                 */
                outconn[i].status = ocLoggingOn;
                logon_object(outconn[i].curr_obj, -1);

                outconn[i].status = ocNotUsed;
                free_object(outconn[i].curr_obj, "net_connect");
                socket_close(outconn[i].socket);

                continue;
            }
        }

        /* connection successful */
        outconn[i].status = ocLoggingOn;

        user = command_giver;
        new_player( outconn[i].curr_obj, outconn[i].socket
                  , &outconn[i].target, sizeof(outconn[i].target), 0);
        command_giver = user;

        free_object(outconn[i].curr_obj, "net_connect");
        outconn[i].status = ocNotUsed;
    }
} /* check_for_out_connections() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_net_connect (svalue_t *sp)

/* EFUN net_connect()
 *   
 *   int net_connect(string host, int port)
 *
 * Open a non-blocking TCP network connection to <host> and <port>.
 * On success, the connection is bound to the current object and the
 * lfun logon() is called in the object.
 *
 * If the connection can't be established immediately, the efun returns
 * 'success' and the driver will check in the background for the progress
 * of the connection. When it is established, logon() will be called in
 * the object. If the connection fails, logon(-1) will be called in the
 * object.
 *
 * The efun raises a privilege violation ("net_connect", host, port).
 *
 * Return 0 on success, and a Unix ERRNO on failure.
 */

{
    char * host;
    int    port;
    int    rc;

    /* get the arguments */
    
    host = get_txt(sp[-1].u.str);
    port = sp->u.number;

    if (!privilege_violation4(STR_NET_CONNECT, NULL, sp[-1].u.str, port, sp))
    {
        sp = pop_n_elems(2, sp);
        push_number(sp, -1);
        return sp;
    }

    /* Try the connection */
    rc = 0;
    do {
        int sfd, n, ret;
        object_t *user;
        struct addrinfo hints;
        struct addrinfo *result, *rp;
        char port_string[6];

        // port is needed as a string
        snprintf(port_string, sizeof(port_string), "%u", port);
        
        /* Find a place to store the pending connection,
         * store the index in n
         */
        Bool stored = MY_FALSE;
        for (n = 0; n < MAX_OUTCONN; n++)
        {
            if (outconn[n].status == ocNotUsed)
            {
                stored = MY_TRUE;
                break;
            }
        }

        if (!stored)
        {
            rc = NC_EMCONN;
            break;
        }

        /* Attempt the connection */
        
        memset(&hints, 0, sizeof(struct addrinfo));
#ifndef USE_IPV6
        hints.ai_family = AF_INET;      // Allow only IPv4
        // only IPv4 if at least one interface with IPv4 and
        // only IPv6 if at least one interface with IPv6
        hints.ai_flags = AI_ADDRCONFIG|AI_NUMERICSERV;
#else
        hints.ai_family = AF_INET6;      // Allow only IPv6
        // only IPv6 if at least one interface with IPv6.
        // And list IPv4 addresses as IPv4-mapped IPv6 addresses if no IPv6 addresses could be found.
        hints.ai_flags = AI_ADDRCONFIG|AI_V4MAPPED|AI_NUMERICSERV;
#endif
        hints.ai_socktype = SOCK_STREAM; // TCP socket
        hints.ai_protocol = 0;          // Any protocol

#ifdef INSIDE_TRAVIS
        // The Travis-CI environment only has loopback devices activated,
        // the AI_ADDRCONFIG would therefore never find a name.
        hints.ai_flags &= ~AI_ADDRCONFIG;
#endif

        /* TODO: Uh-oh, blocking DNS in the execution thread.
         * TODO:: Better would be to start an ERQ lookup and fill in the
         * TODO:: data in the background.
         */
        ret = getaddrinfo(host, port_string, &hints, &result);
        if (ret != 0)
        {
            rc = NC_EUNKNOWNHOST;
            break;
        }
        // try each address until we successfully connect or run out of
        // addresses. In case of errors, close the socket.
        for (rp = result; rp != NULL; rp = rp->ai_next)
        {
            sfd = socket (rp->ai_family, rp->ai_socktype, rp->ai_protocol);
            if (sfd==-1)
            {
                if (errno == EMFILE || errno == ENFILE
                    || errno == ENOBUFS || errno == ENOMEM)
                {
                    // insufficient system ressources, probably transient error,
                    // but it is unlikely to be different for the next address.
                    // We exit here, the caller should try again.
                    rc = NC_ENORESSOURCES;
                    rp = NULL;
                    break;
                }
                else
                {
                    // store only last error
                    rc = NC_ENOSOCKET;
                }
                continue; // try next address
            }
            
            set_socket_nonblocking(sfd);
            set_socket_nosigpipe(sfd);

            /* On multihomed machines it is important to bind the socket to
             * the proper IP address.
             */
            ret = bind(sfd, (struct sockaddr *) &host_ip_addr_template, sizeof(host_ip_addr_template));
            if (ret==-1)
            {
                if (errno==ENOBUFS)
                {
                    // insufficient system ressources, probably transient error,
                    // but unlikely to be different for the next address. Caller
                    // should try again later.
                    rc = NC_ENORESSOURCES;
                    rp = NULL;
                    socket_close(sfd);
                    break;
                }
                else
                {
                    // store only last error.
                    rc = NC_ENOBIND;
                }
                socket_close(sfd);
                continue;
            }
            
            ret = connect(sfd, rp->ai_addr, rp->ai_addrlen);
            if (ret != -1 || errno == EINPROGRESS)
                break;  // success! no need to try further

            // no succes connecting. :-( Store last error in rc.
            if (errno==ECONNREFUSED)
            {
                // no one listening at remote end... happens, no real error...
                rc = NC_ECONNREFUSED;
            }
            else
            {
                rc = NC_ENOCONNECT;
            }
            socket_close(sfd);
        }

        if (rp == NULL)
        {
            // No address succeeded, rc contains the last 'error', we just
            // exit here.
            freeaddrinfo(result);
            break;
        }
        // at this point we a connected socket

        rc = NC_SUCCESS;

        /* Store the connection in the outconn[] table even if
         * we can complete it immediately. For the reason see below.
         */
        outconn[n].socket = sfd;
        outconn[n].target = *((struct sockaddr_in *)rp->ai_addr);
        outconn[n].curr_obj = ref_object(current_object, "net_conect");

        // no longer need the results from getaddrinfo()
        freeaddrinfo(result);

        if (errno == EINPROGRESS)
        {
            /* Can't complete right now */
            outconn[n].status = ocUsed;
            break;
        }

        /* Attempt the logon. By setting the outconn[].status to
         * ocLoggingOn, any subsequent call to check_for_out_connections()
         * will clean up for us.
         */
        outconn[n].status = ocLoggingOn;

        user = command_giver;
        inter_sp = sp;
        new_player(current_object, sfd, (struct sockaddr_in *)rp->ai_addr, sizeof(struct sockaddr_in), 0);
        command_giver = user;

        /* All done - clean up */
        outconn[n].status = ocNotUsed;
        free_object(outconn[n].curr_obj, "net_connect");
    }while(0);

    /* Return the result */
    sp = pop_n_elems(2, sp);
    push_number(sp, rc);

    return sp;
} /* f_net_connect() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_configure_interactive (svalue_t *sp)

/* EFUN configure_interactive()
 *
 *   void configure_interactive(object ob, int what, mixed data)
 *
 * Sets the option <what> to the value <data> on the interactive <ob>
 * or the default for all interactives if <ob> is 0.
 *
 * <what> == IC_MAX_WRITE_BUFFER_SIZE
 *
 * If the first argument <ob> is not this_object(), the privilege violation
 * ("configure_interactive", this_object(), ob, what, data) occurs.
 */

{
    object_t *ob;
    interactive_t *ip;

    if (sp[-2].type == T_OBJECT)
    {
        ob = sp[-2].u.ob;

        if (!O_SET_INTERACTIVE(ip, ob))
        {
            errorf("Bad arg 1 to configure_interactive(): "
                   "Object '%s' is not interactive.\n"
                 , get_txt(ob->name)
                 );
            return sp; /* NOTREACHED */
        }
    }
    else
    {
        ob = NULL;
        ip = NULL;
    }

    if ((current_object->flags & O_DESTRUCTED)
     || (ob != current_object
      && !privilege_violation_n(STR_CONFIGURE_INTERACTIVE, ob, sp, 2)))
    {
        sp = pop_n_elems(3, sp);
        return sp;
    }

    switch(sp[-1].u.number)
    {
    default:
        errorf("Illegal value %"PRIdPINT" for configure_interactive().\n", sp[-1].u.number);
        return sp; /* NOTREACHED */

    case IC_MAX_WRITE_BUFFER_SIZE:
        {
            int max;

            if (sp->type != T_NUMBER)
                efun_exp_arg_error(3, TF_NUMBER, sp->type, sp);

            max = sp->u.number;
            if (max < 0)
                max = -1;

            if (!ip)
                write_buffer_max_size = max;
            else
                ip->write_max_size = max;
            break;
        }

    case IC_SOCKET_BUFFER_SIZE:
        if (!ip)
            errorf("Default value for IC_SOCKET_BUFFER_SIZE is not supported.\n");
        if (sp->type != T_NUMBER)
            efun_exp_arg_error(3, TF_NUMBER, sp->type, sp);
#ifdef SO_SNDBUF
        {
            int size = sp->u.number;
            setsockopt(ip->socket, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(size));
        }
#endif
        break;

    case IC_COMBINE_CHARSET_AS_STRING:
        if (!ip)
            errorf("Default value for IC_COMBINE_CHARSET_AS_STRING is not supported.\n");
        switch (sp->type)
        {
            case T_NUMBER:
                if (sp->u.number != 0)
                    efun_exp_arg_error(3, TF_STRING, sp->type, sp);
                set_default_combine_charset(ip->combine_cset);
                break;

            case T_STRING:
                set_charset_from_string(sp->u.str, ip->combine_cset, "configure_interactive with IC_COMBINE_CHARSET_AS_STRING", 3);
                break;

            default:
                efun_exp_arg_error(3, TF_STRING, sp->type, sp);
        }

        /* Never combine \n or \0. */
        ip->combine_cset['\n'/8] &= ~(1 << '\n' % 8);
        ip->combine_cset['\0'/8] &= ~(1 << '\0' % 8);
        break;

    case IC_COMBINE_CHARSET_AS_ARRAY:
        if (!ip)
            errorf("Default value for IC_COMBINE_CHARSET_AS_ARRAY is not supported.\n");
        switch (sp->type)
        {
            case T_NUMBER:
                if (sp->u.number != 0)
                    efun_exp_arg_error(3, TF_POINTER, sp->type, sp);
                set_default_combine_charset(ip->combine_cset);
                break;

            case T_POINTER:
                set_charset_from_vector(sp->u.vec, ip->combine_cset, "configure_interactive with IC_COMBINE_CHARSET_AS_ARRAY", 3);
                break;

            default:
                efun_exp_arg_error(3, TF_POINTER, sp->type, sp);
        }

        /* Never combine \n or \0. */
        ip->combine_cset['\n'/8] &= ~(1 << '\n' % 8);
        ip->combine_cset['\0'/8] &= ~(1 << '\0' % 8);
        break;

    case IC_CONNECTION_CHARSET_AS_STRING:
        if (!ip)
            errorf("Default value for IC_CONNECTION_CHARSET_AS_STRING is not supported.\n");
        switch (sp->type)
        {
            case T_NUMBER:
                if (sp->u.number != 0)
                    efun_exp_arg_error(3, TF_STRING, sp->type, sp);
                set_default_conn_charset(ip->charset);
                break;

            case T_STRING:
                set_charset_from_string(sp->u.str, ip->charset, "configure_interactive with IC_CONNECTION_CHARSET_AS_STRING", 3);
                break;

            default:
                efun_exp_arg_error(3, TF_STRING, sp->type, sp);
        }
        break;

    case IC_CONNECTION_CHARSET_AS_ARRAY:
        if (!ip)
            errorf("Default value for IC_CONNECTION_CHARSET_AS_ARRAY is not supported.\n");
        switch (sp->type)
        {
            case T_NUMBER:
                if (sp->u.number != 0)
                    efun_exp_arg_error(3, TF_POINTER, sp->type, sp);
                set_default_conn_charset(ip->charset);
                break;

            case T_POINTER:
                set_charset_from_vector(sp->u.vec, ip->charset, "configure_interactive with IC_CONNECTION_CHARSET_AS_ARRAY", 3);
                break;

            default:
                efun_exp_arg_error(3, TF_POINTER, sp->type, sp);
        }
        break;

    case IC_QUOTE_IAC:
        if (!ip)
            errorf("Default value for IC_QUOTE_IAC is not supported.\n");

        if (sp->type != T_NUMBER)
            efun_exp_arg_error(3, TF_NUMBER, sp->type, sp);

        ip->quote_iac = (char)sp->u.number;
        break;

    case IC_TELNET_ENABLED:
        if (!ip)
            errorf("Default value for IC_TELNET_ENABLED is not supported.\n");

        if (sp->type != T_NUMBER)
            efun_exp_arg_error(3, TF_NUMBER, sp->type, sp);

        ip->tn_enabled = (sp->u.number != 0);
        break;

#ifdef USE_MCCP
    case IC_MCCP:
        if (!ip)
            errorf("Default value for IC_MCCP is not supported.\n");

        if (sp->type != T_NUMBER)
            efun_exp_arg_error(3, TF_NUMBER, sp->type, sp);
        else if(sp->u.number == 0)
        {
            /* Deactivating MCCP */
            end_compress(ip, MY_FALSE);
        }
        else
        {
            /* Activating MCCP */
            p_int mccpver;

            if (!ip->tn_enabled)
                mccpver = -1;
            else
            {
                mccpver = sp->u.number;

                if ((mccpver != TELOPT_COMPRESS)
                 && (mccpver != TELOPT_COMPRESS2)
                   )
                {
                    errorf("Illegal value to arg 3 of configure_interactive with IC_MCCP: %ld, "
                          "expected TELOPT_COMPRESS (%d) or TELOPT_COMPRESS2 (%d).\n"
                         , (long)mccpver, TELOPT_COMPRESS, TELOPT_COMPRESS2
                         );
                }
            }

            start_compress(ip, mccpver);
        }
        break;
#endif /* USE_MCCP*/

    case IC_PROMPT:
        if (!ip)
            errorf("Default value for IC_PROMPT is not supported.\n");

        if (sp->type != T_STRING && sp->type != T_CLOSURE)
            efun_exp_arg_error(3, TF_STRING|TF_CLOSURE, sp->type, sp);

        if (sp->type == T_CLOSURE && sp->x.closure_type == CLOSURE_UNBOUND_LAMBDA)
            errorf("Bad arg 3 for configure_interactive with IC_PROMPT: lambda closure not bound\n");

        if (sp->type == T_STRING)
        {
            string_t *str = make_tabled_from(sp->u.str);

            if (!str)
                errorf("Out of memory (%zu bytes) for prompt\n", mstrsize(sp->u.str));
            else
            {
                free_mstring(sp->u.str);
                sp->u.str = str;
            }
        }

        free_svalue(&ip->prompt);
        assign_svalue_no_free(&ip->prompt, sp);
        break;

    case IC_MAX_COMMANDS:
        if (!ip)
            errorf("Default value for IC_MAX_COMMANDS is not supported.\n");

        if (sp->type != T_NUMBER)
            efun_exp_arg_error(3, TF_NUMBER, sp->type, sp);

        if (sp->u.number < 0)
            ip->maxNumCmds = -1;
        else
            ip->maxNumCmds = sp->u.number;
        break;

    case IC_MODIFY_COMMAND:
        if (!ip)
            errorf("Default value for IC_MODIFY_COMMAND is not supported.\n");

        if (sp->type != T_OBJECT && (sp->type != T_NUMBER || sp->u.number != 0))
            efun_exp_arg_error(3, TF_OBJECT, sp->type, sp);

        if (ip->modify_command)
            free_object(ip->modify_command, "configure_interactive(IC_MODIFY_COMMAND)");

        if (sp->type == T_OBJECT)
            ip->modify_command = ref_object(sp->u.ob, "configure_interactive(IC_MODIFY_COMMAND)");
        else
            ip->modify_command = NULL;
        break;

    case IC_ENCODING:
        if (sp->type != T_STRING)
            efun_exp_arg_error(3, TF_STRING, sp->type, sp);

        if (mstrsize(sp->u.str) >= sizeof(default_player_encoding))
            errorf("Illegal value to arg 3 of configure_interactive with IC_ENCODING: Encoding name too long.\n");

        if (!ip)
        {
            /* Let's check the encoding before setting it as the default. */
            iconv_t cd = iconv_open(get_txt(sp->u.str), "UTF-8");
            if (!iconv_valid(cd))
            {
                if (errno == EINVAL)
                    errorf("Error setting default encoding: unsupported encoding '%s'.\n", get_txt(sp->u.str));
                else
                    errorf("Error setting default encoding: %s.\n", strerror(errno));
            }

            iconv_close(cd);

            memcpy(default_player_encoding, get_txt(sp->u.str), mstrsize(sp->u.str) + 1);
        }
        else if (!set_encoding(ip, get_txt(sp->u.str)))
        {
            if (errno == EINVAL)
                errorf("Error setting encoding: unsupported encoding '%s'.\n", get_txt(sp->u.str));
            else
                errorf("Error setting encoding: %s.\n", strerror(errno));
        }

        break;
    }

    sp = pop_n_elems(3, sp);
    return sp;
} /* f_configure_interactive() */

/*-------------------------------------------------------------------------*/
static bool
valid_query_snoop (object_t *ob)

/* Checks with the master, if it is
 * allowed to query who is snooping on <ob>.
 */
{
    assert_master_ob_loaded();
    if (current_object != master_ob)
    {
        svalue_t *valid;

        assign_eval_cost();
        push_ref_object(inter_sp, ob, "valid_query_snoop");
        valid = apply_master(STR_VALID_QSNOOP, 1);
        if (!valid || valid->type != T_NUMBER || !valid->u.number)
            return false;
    }

    return true;
} /* valid_query_snoop() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_interactive_info (svalue_t *sp)

/* EFUN interactive_info()
 *
 *   mixed interactive_info (object ob, int what)
 *
 * Returns information about the interactive <ob>.
 * <what> can either be a configuration option as given to
 * configure_interactive() or one of the following options:
 *
 * <what> == II_...
 *
 * <ob> can be 0 to query default configuration options.
 */

{
    object_t *ob;
    interactive_t *ip;
    svalue_t result;

    if (sp[-1].type == T_OBJECT)
    {
        ob = sp[-1].u.ob;

        if (!O_SET_INTERACTIVE(ip, ob))
        {
            errorf("Bad arg 1 to interactive_info(): "
                   "Object '%s' is not interactive.\n"
                 , get_txt(ob->name)
                 );
            return sp; /* NOTREACHED */
        }
    }
    else
    {
        ob = NULL;
        ip = NULL;
    }

    if (!ob && sp[0].u.number < 0)
        errorf("There is no default value for non-configuration values.\n");

    switch(sp[0].u.number)
    {
    default:
        errorf("Illegal value %"PRIdPINT" for interactive_info().\n", sp[0].u.number);
        return sp; /* NOTREACHED */

    case IC_MAX_WRITE_BUFFER_SIZE:
        if (!ip || ip->write_max_size == -2)
            put_number(&result, write_buffer_max_size);
        else
            put_number(&result, ip->write_max_size);
        break;

    case IC_SOCKET_BUFFER_SIZE:
        if (!ip)
            errorf("Default value for IC_SOCKET_BUFFER_SIZE is not supported.\n");
#ifdef SO_SNDBUF
        {
            int size;
            length_t sizelen = sizeof(size);

            if (!getsockopt(ip->socket, SOL_SOCKET, SO_SNDBUF, (char *)&size, &sizelen))
                put_number(&result, size);
            else
                put_number(&result, -1);
        }
#else
        put_number(&result, -1);
#endif
        break;

    case IC_COMBINE_CHARSET_AS_STRING:
        if (!ip)
            errorf("Default value for IC_COMBINE_CHARSET_AS_STRING is not supported.\n");
        get_charset(&result, true, ip->combine_cset);
        break;

    case IC_COMBINE_CHARSET_AS_ARRAY:
        if (!ip)
            errorf("Default value for IC_COMBINE_CHARSET_AS_ARRAY is not supported.\n");
        get_charset(&result, false, ip->combine_cset);
        break;

    case IC_CONNECTION_CHARSET_AS_STRING:
        if (!ip)
            errorf("Default value for IC_CONNECTION_CHARSET_AS_STRING is not supported.\n");
        get_charset(&result, true, ip->charset);
        break;

    case IC_CONNECTION_CHARSET_AS_ARRAY:
        if (!ip)
            errorf("Default value for IC_CONNECTION_CHARSET_AS_ARRAY is not supported.\n");
        get_charset(&result, false, ip->charset);
        break;

    case IC_QUOTE_IAC:
        if (!ip)
            errorf("Default value for IC_QUOTE_IAC is not supported.\n");
        put_number(&result, ip->quote_iac != 0);
        break;

    case IC_TELNET_ENABLED:
        if (!ip)
            errorf("Default value for IC_TELNET_ENABLED is not supported.\n");
        put_number(&result, ip->tn_enabled != 0);
        break;

#ifdef USE_MCCP
    case IC_MCCP:
        if (!ip)
            errorf("Default value for IC_MCCP is not supported.\n");

        put_number(&result, ip->compressing);
        break;
#endif /* USE_MCCP*/

    case IC_PROMPT:
        if (!ip)
            errorf("Default value for IC_PROMPT is not supported.\n");

        assign_svalue_no_free(&result, &ip->prompt);
        break;

    case IC_MAX_COMMANDS:
        if (!ip)
            errorf("Default value for IC_MAX_COMMANDS is not supported.\n");

        put_number(&result, ip->maxNumCmds);
        break;

    case IC_MODIFY_COMMAND:
        if (!ip)
            errorf("Default value for IC_MODIFY_COMMAND is not supported.\n");

        if (ip->modify_command)
            put_ref_object(&result, ip->modify_command, "interactive_info(IC_MODIFY_COMMAND)");
        else
            put_number(&result, 0);
        break;

    case IC_ENCODING:
        if (!ip)
            put_c_string(&result, default_player_encoding);
        else
            put_c_string(&result, ip->encoding);
        break;

    /* Connection information */
    case II_IP_NAME:
#ifdef ERQ_DEMON
        {
            string_t * hname;

            hname = lookup_ip_entry(ip->addr.sin_addr, MY_FALSE);
            if (hname)
            {
                put_ref_string(&result, hname);
                break;
            }
        }
#endif
        /* FALLTHROUGH */

    case II_IP_NUMBER:
        {
            string_t *haddr;

#ifndef USE_IPV6
            haddr = new_mstring(inet_ntoa(ip->addr.sin_addr), STRING_ASCII);
#else
            haddr = new_mstring(inet6_ntoa(ip->addr.sin_addr), STRING_ASCII);
#endif

            if (!haddr)
                errorf("Out of memory for IP address\n");

            put_string(&result, haddr);
            break;
        }

    case II_IP_PORT:
        put_number(&result, ntohs(ip->addr.sin_port));
        break;

    case II_IP_ADDRESS:
        {
            vector_t *v;
            svalue_t *svp;
            unsigned char *cp;
            int i;

            v = allocate_array(sizeof ip->addr);

            if (!v)
                errorf("Out of memory for IP address\n");

            svp = v->item;
            cp = (unsigned char*)&ip->addr;
            for (i = sizeof ip->addr; --i;)
            {
                put_number(svp, *cp);
                cp++;
                svp++;
            }
            put_array(&result, v);
            break;
        }

    case II_MUD_PORT:
        {
            struct sockaddr_in addr;
            length_t length = sizeof(addr);

            getsockname(ip->socket, (struct sockaddr *)&addr, &length);
            put_number(&result, ntohs(addr.sin_port));
            break;
        }

#ifdef USE_MCCP
    /* Telnet related information */
    case II_MCCP_STATS:
        if (ip->compressing > 0)
        {
            vector_t *mccp_stats;
            mccp_stats = allocate_uninit_array(2);
            put_number(mccp_stats->item,     ip->out_compress->total_in);
            put_number(mccp_stats->item + 1, ip->out_compress->total_out);
            put_array(&result, mccp_stats);
        }
        else
        {
            put_number(&result, 0);
        }
        break;
#endif /* USE_MCCP*/

    /* Input handling */
    case II_INPUT_PENDING:
        {
            input_t *ih = get_input_handler(ip, INPUT_TO);
            put_number(&result, 0);

            if (ih)
            {
                object_t *cb = callback_object(&(((input_to_t*)ih)->fun));
                if (cb)
                    put_ref_object(&result, cb, "interactive_info(II_INPUT_PENDING)");
            }
        }
        break;

    case II_EDITING:
        {
            input_t *ih = get_input_handler(ip, INPUT_ED);
            put_number(&result, 0);

            if (ih)
            {
                object_t *ed = get_ed_object(ih);
                if (ed)
                    put_ref_object(&result, ed, "interactive_info(II_EDITING)");
                else
                    put_number(&result, 1);
            }
        }
        break;

    case II_IDLE:
        put_number(&result, current_time - ip->last_time);
        break;

    case II_NOECHO:
    case II_CHARMODE:
    {
        int flag = (sp[0].u.number == II_NOECHO) ? NOECHO : CHARMODE;

        put_number(&result,
            !(ip->noecho & flag) ? 0 :
            (ip->noecho & NOECHO_ACKSHIFT(flag)) ? 2 : 1);
        break;
    }

    /* Output handling */
    case II_SNOOP_NEXT:
        put_number(&result, 0);

        if (!valid_query_snoop(ob))
            break;

        if (ip->snoop_by)
            put_ref_object(&result, ip->snoop_by, "interactive_info(II_SNOOP_NEXT)");
        break;

    case II_SNOOP_PREV:
        put_number(&result, 0);

        if (!ip->snoop_on)
            break;

        if (!valid_query_snoop(ip->snoop_on->ob))
            break;

        if (ip->snoop_on)
            put_ref_object(&result, ip->snoop_on->ob, "interactive_info(II_SNOOP_PREV)");
        break;

    case II_SNOOP_ALL:
        {
            int num = 0, pos = 0;
            object_t *snooper = ip->snoop_by;
            object_t *victim = ob;
            vector_t *vec;

            while (snooper && valid_query_snoop(victim))
            {
                interactive_t * snooper_ip;
                num++;

                if (!O_SET_INTERACTIVE(snooper_ip, snooper))
                    break;
                victim = snooper;
                snooper = snooper_ip->snoop_by;
            }

            vec = allocate_array(num);
            snooper = ip->snoop_by;
            while (snooper && pos < num)
            {
                interactive_t * snooper_ip;
                put_ref_object(vec->item + pos, snooper, "interactive_info(II_SNOOP_ALL)");
                pos++;

                if (!O_SET_INTERACTIVE(snooper_ip, snooper))
                    break;
                snooper = snooper_ip->snoop_by;
            }

            put_array(&result, vec);
            break;
        }
    }

    sp = pop_n_elems(2, sp);

    sp++;
    *sp = result;
    return sp;
} /* f_interactive_info() */

/***************************************************************************/
