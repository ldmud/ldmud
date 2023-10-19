#ifndef COMM_H__
#define COMM_H__ 1

#include "driver.h"
#include "typedefs.h"
#include <sys/types.h>
#ifdef USE_MCCP
#    include <zlib.h>
#endif

#include "iconv_opt.h"
#include "simulate.h"   /* callback_t for TLS */
#include "svalue.h"
#include "pkg-tls.h"

/* TODO: Make the following a separate "my-socket.h" include, also
 * TODO:: to be used in access_check.h instead of comm.h.
 */
#ifdef SOCKET_HEADER
#    include SOCKET_HEADER
#endif

#if !defined (SOCKET_LIB) && !defined(SOCKET_INC)
#    include <sys/socket.h>
#    include <netinet/in.h>
#    include <arpa/inet.h>
#    define SOCKET_T int
#endif /* SOCKET_LIB */

#ifndef MAX_SOCKET_PACKET_SIZE
#    define MAX_SOCKET_PACKET_SIZE  1024  /* Wild guess. */
#endif /* MAX_SOCKET_PACKET_SIXE */


/* --- IPv6 --- */

#ifdef USE_IPV6

#define AF_INET4OR6         AF_INET6
#define INET4OR6_ADDRSTRLEN INET6_ADDRSTRLEN
#define IN4OR6ADDR_ANY      in6addr_any

typedef struct sockaddr_in6 sockaddr_in4or6;
typedef struct in6_addr in4or6_addr;

/* sockaddr_in6 members */
#define sin4or6_port    sin6_port
#define sin4or6_addr    sin6_addr
#define sin4or6_family  sin6_family

/* in6_addr member */
#define s4or6_addr      s6_addr


#if defined(__APPLE__) && defined(__MACH__) && !defined(s6_addr32)

/* These are non-standard defines, and some OS don't offer them
 * by default: OS-X
 */

#  define s6_addr8  __u6_addr.__u6_addr8
#  define s6_addr16 __u6_addr.__u6_addr16
#  define s6_addr32 __u6_addr.__u6_addr32
#endif

#else

#define AF_INET4OR6         AF_INET
#define INET4OR6_ADDRSTRLEN INET_ADDRSTRLEN
#define IN4OR6ADDR_ANY      (struct in_addr){.s_addr = INADDR_ANY}

typedef struct sockaddr_in sockaddr_in4or6;
typedef struct in_addr in4or6_addr;

/* sockaddr_in6 members */
#define sin4or6_port    sin_port
#define sin4or6_addr    sin_addr
#define sin4or6_family  sin_family

/* in6_addr member */
#define s4or6_addr      s_addr

#endif /* USE_IPV6 */


/* --- Macros --- */

/* Size of a users text buffer for incoming data.
 * This is also the maximum size for one command.
 */

#define MAX_TEXT  20480

/* --- Telnet Handling --- */

#ifdef DEBUG_TELNET

#define DT(x) printf("%s TDEBUG: ", time_stamp()); printf x
#define DTN(x) printf("%s TDEBUG: '%s' ", time_stamp(), get_txt(ip->ob->name)); printf x
#define DTF(x) printf x

#else

#define DT(x)
#define DTN(x)
#define DTF(x)

#endif

/* --- Types --- */

/* --- struct write_buffer_s: async write datastructure
 *
 * This data structure holds all the information for pending messages
 * which are to be written by a background thread. The structure is
 * allocated to the necessary length to hold the full message.
 * The instances are kept in a linked list from the interactive_t
 * structure.
 */
enum write_buffer_flags
{
    WB_NONDISCARDABLE = 0x0001, /* This message must be sent. */
};

typedef uint32 write_buffer_flag_t;

struct write_buffer_s
{
    struct write_buffer_s *next;
    size_t length;
    size_t pos;
    write_buffer_flag_t flags;
    char buffer[1 /* .length */ ];
};

/* Indicates discarded messages. */
enum discarded_msg_states
{
    DM_NONE = 0,
    DM_SEND_INFO,
    DM_INFO_WAS_SENT,
};

typedef char discarded_msg_state_t;

enum input_type_e {
    INPUT_TO    /* A normal input_to. */
 ,  INPUT_ED    /* An ed() session.   */
};

typedef enum input_type_e input_type_t;

/* --- struct listen_port_s: Port designation.
 *
 * A port to listen is either an IPv4/v6 port or an inherited socket.
 * An IP port can optionally be limited to an address.
 */
struct listen_port_s
{
    enum { LISTEN_PORT_ANY, LISTEN_PORT_ADDR, LISTEN_PORT_INHERITED } type;
    const char* str;
    int port;
    in4or6_addr addr;
};


/* --- struct input_s: Stack of input handlers.
 *
 * input_s represents all pending input handlers, which
 * are at this time either normal input_to()s or ed() sessions.
 * Every interactive object can have these input handlers,
 * the pointer to the structure is stored in the interactive
 * sentence structure.
 */
struct input_s {
    input_t      *next;
    input_type_t  type;
    svalue_t      prompt;     /* the prompt, may be 0 */
    char          noecho;     /* the requested "noecho" state */
    Bool          local;      /* TRUE if a CHARMODE change is local only */
};

/* --- struct interactive_s: an interactive connection
 *
 * The structure is linked to by a shadow sentence of the interactive
 * object.
 *
 * When changing struct members, take care that you don't introduce
 * unnecessary padding.
 *
 * If the CBool members should ever be changed to C99's _Bool, take care of
 * checking who is relying on a specific size (e.g. some printf())
 *
 */

struct interactive_s {
    SOCKET_T socket;            /* The socket structure */
    object_t *ob;               /* Points back to the associated object */
    input_t  *input_handler;    /* != NULL: defines function to be
                                   called with next input line */
    object_t *modify_command;   /* modify_command() handler() */
    svalue_t prompt;            /* The prompt to print. */
    sockaddr_in4or6 addr;       /* Address of connected user */

    CBool closing;              /* True when closing this socket. */
    CBool tn_enabled;           /* True: telnet machine enabled */
    char do_close;              /* Bitflags: Close this down; Proto-ERQ. */
    char noecho;                /* Input mode bitflags */

    char tn_state;              /* current state of telnet machine */
    CBool supress_go_ahead;     /* Keep track of the WILL SGA negotiation state
                                 * as some clients mix that up with DO SGA.
                                 * Other than that, this is of no concern.
                                 */
    CBool outgoing_conn;        /* TRUE if the connection was created by 
                                 * net_connect().
                                 */
    discarded_msg_state_t msg_discarded;
                                /* Indicates if an earlier message had
                                 * been discarded. */
    short text_prefix;          /* Number of bytes at the beginning of .text that belong
                                 * to the next multi-byte sequence.
                                 */
    short tn_start;             /* first char in .text of pending telnet subneg
                                   (points to the byte after IAC SB) */
    short tn_end;               /* first char in .text to check for telnet negotiation */
    short text_end;             /* first free char in .text buffer */
    short command_start;        /* used for charmode: The next char in .command
                                 * to send to the input_to handler. */
    short command_printed;      /* For charmode: When having escaped input, points beyond
                                 * the last character that was echoed back by the driver. */
    short command_end;          /* end of the parsed command in .command buffer
                                 * (In charmode points beyond the line ending when TS_READY,
                                 * otherwise doesn't include the line ending) */
    short command_unprocessed_end;
                                /* end of unprocessed chars in .command buffer */
    interactive_t *snoop_on;    /* whom we're snooping */
    object_t *snoop_by;         /* by whom we're snooped */
    mp_int last_time;           /* Time of last command executed */
    long  numCmds;              /* Number of commands executed with the same
                                 * .last_time */
    long  maxNumCmds;           /* Maximum number of commands (char or line)
                                 * to execute per second. A value < 0
                                 * means 'unlimited'.
                                 */
    int trace_level;            /* Trace flags. 0 means no tracing */
    string_t *trace_prefix;     /* Trace only objects which have this string
                                   as name prefix. NULL traces everything. */
    int message_length;         /* Current length of message in message_buf[] */

    interactive_t *next_player_for_flush;
    interactive_t *previous_player_for_flush;
      /* Double linked list of all active user objects with data pending
       * in message_buf[].
       */

    long access_class;
      /* represents a "cluster" where this player comes from
       */
    char charset[16];
      /* A bitflag array: every non-zero flag allows the corresponding
       * character to be sent. Characters whose flag is 0 are excluded
       * from the sent data.
       */
    char combine_cset[16];
      /* A bitflag array: all characters with their corresponding flag
       * set to non-zero flag may be combined into one string in
       * char-mode when received en-bloc anyway. Characters whose flag
       * is 0 are always returned in charmode in separate strings.
       */
    char encoding[128];
      /* The encoding of the connection. */

    CBool quote_iac;
    CBool catch_tell_activ;
    bool syncing;               /* Received a TCP Urgend notification. */
    char gobble_char;           /* Char to ignore at the next telnet_neg() */

    char text[MAX_TEXT];
      /* The receive buffer. These are the raw bytes received from
       * the network connection.
       */

    char command[MAX_TEXT];
      /* The command read from network. Contains the text from .text
       * after having telnet data removed and being converted from
       * the source encoding to UTF-8.
       */

    char message_buf[MAX_SOCKET_PACKET_SIZE];
      /* The send buffer. */

    iconv_t receive_cd;
      /* The iconv conversion descriptor for receiving text.
       * It is always a valid descriptor.
       */

    iconv_t send_cd;
      /* The iconv conversion descriptor for sending text.
       * It is always a valid descriptor.
       */

#ifdef USE_MCCP
    unsigned char   compressing;
    z_stream      * out_compress;
    unsigned char * out_compress_buf;
#endif

    struct write_buffer_s *write_first;  /* List of buffers to write */
    struct write_buffer_s *write_last;
    p_uint                 write_size;
    p_int                  write_max_size; 
      /* Maximum write_size.
       *   0: No write buffer.
       *  -1: Infinite write buffer.
       *  -2: Use global variable write_buffer_max_size.
       */

#ifdef USE_TLS
    tls_session_t          tls_session;
    int                    tls_status;
#     define TLS_INACTIVE     0  /* Session is not secure */
#     define TLS_HANDSHAKING  1  /* TLS is being negotiated */
#     define TLS_ACTIVE       2  /* Session is secure */
    callback_t            *tls_cb;
#endif
};

/* --- Bitflags and masks for interactive.noecho ---
 *
 * 'noecho' is a historical misnomer as it actually represents several
 * input modes: echo/noecho, linemode/charmode, and ignore '!' escape.
 * Echo and Charmode additionally distinguish between 'required'
 * and 'granted' ('want yes' and 'yes').
 *
 * xxx_REQ is set when the lib wants this state (independend from telnet state)
 * xxx     is set when we sent out the appropirate negotiations
 * xxx_ACK is set when the client agrees
 *
 *     xxx   xxx_ACK    telnet state
 *      0       0        NO
 *      1       0        WANT YES
 *      1       1        YES
 *      0       1        WANT NO (not implemented)
 */

#define CHARMODE_REQ_TO_CHARMODE(x) ((x) << 2)
  /* Transform {CHARMODE, NOECHO}_REQ into {CHARMODE, NOECHO}
   */
#define NOECHO_ACKSHIFT(x)   ((x) << 2)
  /* Transform {CHARMODE, NOECHO} into {CHARMODE, NOECHO}_ACK
   */
#define NOECHO_ACKRSHIFT(x)  ((x) >> 2)
  /* Transform {CHARMODE, NOECHO}_ACK into {CHARMODE, NOECHO}
   */

#define IGNORE_BANG    128
  /* Disable input escape with leading '!'.
   */

#define NOECHO_REQ        1
  /* noecho requested
   */
#define NOECHO         /* 4 */ CHARMODE_REQ_TO_CHARMODE(NOECHO_REQ)
  /* noecho active (requested via telnet negotiation)
   */
#define NOECHO_ACK    /* 16 */ NOECHO_ACKSHIFT(NOECHO)
  /* noecho negotiation complete (acknowledged)
   * TODO: We need a _NACK flag, too, for when the negotiation is complete
   * TODO:: but the client refused to go into NOECHO. For the time being
   * TODO:: we might use (NOECHO_REQ|NOECHO) == (NOECHO_REQ) as check.
   */
#define NOECHO_STALE     64
  /* Set prior to performing a noecho input, this bit causes the deactivation
   * of noecho if still set after the input (ie if noecho was not requested
   * again).
   */
#define NOECHO_MASK      (NOECHO|NOECHO_ACK)
  /* Mask for active noecho states.
   */

#define CHARMODE_REQ      2
  /* Charmode requested
   * The driver orients its behaviour according to this flag so that it
   * can act correctly even before the negotiation is complete.
   * Moreover, this flag serves as indicator that the input buffer
   * variables are set up to 'charmode', so it should not be reset except
   * through a call to set_noecho().
   */
#define CHARMODE       /* 8 */ CHARMODE_REQ_TO_CHARMODE(CHARMODE_REQ)
  /* Charmode active (requested via telnet negotiation)
   */
#define CHARMODE_ACK  /* 32 */ NOECHO_ACKSHIFT(CHARMODE)
  /* Charmode negotiation complete (acknowledged)
   * TODO: We need a _NACK flag, too, for when the negotiation is complete
   * TODO:: but the client refused to go into CHARMODE. For the time being
   * TODO:: we can use (CHARMODE_REQ|CHARMODE) == CHARMODE_REQ as check.
   */
#define NOECHO_DELAYED  128
  /* If NOECHO and CHARMODE is requested, we first try to establish
   * CHARMODE and set this flag. If CHARMODE is acknoledged we try to
   * negotiate for NOECHO. This modus operandi is enabled with SAVE_NOECHO.
   * pro: If the user has a linemode-only client she is not left in
   *      useless NOECHO mode
   * con: The NOECHO is delayed by the round trip time for the negotiation
   *      which could be imense on very slow lines.
   * TODO: Need to defined SAVE_NOECHO in comm.c for this.
   */
#define CHARMODE_MASK    (CHARMODE|CHARMODE_ACK)
  /* Mask for active charmode states.
   */

/* --- Variables --- */

extern interactive_t *all_players[MAX_PLAYERS];
extern int num_player;
extern char *domain_name;
extern struct listen_port_s port_numbers[];
extern int numports;

extern p_int write_buffer_max_size;

#ifdef COMM_STAT
extern statcounter_t add_message_calls;
extern statcounter_t inet_packets;
extern statcounter_t inet_volume;
extern statcounter_t inet_packets_in;
extern statcounter_t inet_volume_in;
#endif

/* --- Prototypes --- */

extern void initialize_host_name (const char *hname);
extern void initialize_host_ip_number(const char *, const char *);
extern bool add_listen_port(const char *port);
extern bool add_inherited_port(const char *port);
extern void  prepare_ipc(void);
extern void  ipc_remove(void);
extern Bool comm_socket_write (char *msg, size_t size, interactive_t *ip, uint32 flags);
extern void  add_message_flush();
extern void  add_message_bytes (const char* bytes, size_t len);
extern void  add_message_text (const char* str, size_t len);
extern void  add_message_str (string_t *str);
extern void  add_message VARPROT((const char *, ...), printf, 1, 2);
extern void  flush_all_player_mess(void);
extern Bool get_message(char *buff, size_t *bufflength);
extern void remove_interactive(object_t *ob, Bool force);
extern void set_noecho(interactive_t *i, char noecho, Bool local_change, Bool external);
extern int  find_no_bang (interactive_t *ip);
extern Bool call_function_interactive(interactive_t *i, char *str, size_t length);
extern void remove_all_players(void);
extern void  print_prompt(void);
extern void  init_telopts(void);
extern void  mudlib_telopts(void);
extern svalue_t *v_input_to (svalue_t *sp, int num_arg);

#ifdef ERQ_DEMON
extern void  start_erq_demon(const char *suffix, size_t suffixlen);
extern void  wait_erq_demon();
extern svalue_t *f_attach_erq_demon(svalue_t *sp);
extern svalue_t *f_send_erq(svalue_t *sp);
#endif

extern size_t show_comm_status (strbuf_t * sbuf, Bool verbose);
extern void remove_stale_player_data (void);
extern void check_for_out_connections (void);

#ifdef GC_SUPPORT
extern void  clear_comm_refs(void);
extern void  count_comm_refs(void);
extern void  clear_input_refs(input_t *it);
extern void  count_input_refs(input_t *it);
#endif /* GC_SUPPORT */

extern void add_input_handler(interactive_t *ip, input_t *ih, Bool append);
extern void remove_input_handler(interactive_t *ip, input_t *ih);
extern void abort_input_handler(interactive_t *ip);
extern input_t *get_input_handler(interactive_t *ip, input_type_t type);

extern char *query_host_name(void);
extern char *get_host_ip_number(void);
extern svalue_t *f_remove_interactive(svalue_t *sp);

#ifdef DEBUG
extern void  count_comm_extra_refs(void);
#endif /* DEBUG */

extern svalue_t *f_send_udp(svalue_t *sp);

extern svalue_t *f_binary_message(svalue_t *sp);
extern svalue_t *f_exec(svalue_t *sp);
extern svalue_t *f_interactive(svalue_t *sp);
extern svalue_t *v_find_input_to (svalue_t *sp, int num_arg);
extern svalue_t *v_remove_input_to (svalue_t *sp, int num_arg);
extern svalue_t *f_input_to_info (svalue_t *sp);
extern svalue_t *v_snoop(svalue_t *sp, int num_arg);
extern svalue_t *f_users(svalue_t *sp);
extern svalue_t *f_net_connect (svalue_t *sp);
extern svalue_t *f_configure_interactive(svalue_t *sp);

extern void refresh_access_data(void (*add_entry)(sockaddr_in4or6 *, int, long*) );

#endif /* COMM_H__ */
