#ifndef __COMM_H__
#define __COMM_H__ 1

#include "driver.h"
#include <sys/types.h>

#include "interpret.h"  /* struct svalue, struct vector */
#include "object.h"     /* struct object */
#include "sent.h"       /* struct shadow_sentence */


#ifdef SOCKET_HEADER
#    include SOCKET_HEADER
#endif

#if !defined (SOCKET_LIB) && !defined(SOCKET_INC)
#    include <sys/socket.h>
#    ifdef _AIX
#        include <sys/socketvar.h>
#    endif
#    include <netinet/in.h>
#    include <arpa/inet.h>
#    define SOCKET_T int
#endif /* SOCKET_LIB */

#ifndef MAX_SOCKET_PACKET_SIZE
#    if defined(sgi) && defined(mips) && defined(unix)
         /* Work around for an irix kernel bug that can leave the
          * process unresponsive and unkillable.
          */
#        define MAX_SOCKET_PACKET_SIZE   200
#    else
#        define MAX_SOCKET_PACKET_SIZE  1024  /* Wild guess. */
#    endif /* irix */
#endif /* MAX_SOCKET_PACKET_SIXE */


/* --- Macros --- */

/* Size of a users text buffer for incoming data.
 * This is also the maximum size for one command.
 */

#define MAX_TEXT  2048


/* --- Types --- */

/* --- struct input_to: input_to() datastructure
 *
 * input-to structures describe a pending input_to() for a given
 * interactive object. Every object can have one input-to pending, the
 * pointer to the structure is stored in the interactive sentence structure.
 *
 * The structure is allocated big enough to hold all the arguments
 * to the function.
 */

struct input_to {
    struct object *ob;         /* Object to call */
    char          *function;   /* Name of the function to call */
    int            num_arg;    /* Number of arguments */
    struct svalue  arg[1];     /* Arguments to pass */
};

/* --- struct interactive: an interactive connection
 *
 * The structure is an expanded shadow_sentence, and as such is
 * always the first of an objects shadow_sentences as well as the first
 * of its sentences in general.
 *
 * When changing struct members, take care that you don't introduce
 * unnecessary padding.
 */

struct interactive {
    struct shadow_sentence sent;

    SOCKET_T socket;            /* The socket structure */
    struct object *ob;          /* Points back to the associated object */
    struct input_to *input_to;  /* != NULL: defines function to be
                                   called with next input line */
    struct object *modify_command;  /* modify_command() handler() */
    struct svalue prompt;       /* The prompt to print. */
    struct sockaddr_in addr;    /* Address of connected user */

    /* TODO: BOOL */ char closing;                /* True when closing this socket. */
    char do_close;              /* Bitflags: Close this down; Proto-ERQ. */
    char noecho;                /* Input mode bitflags */

    char tn_state;              /* current state of telnet machine */
    char save_tn_state;         /* saved state of telnet machine */
    /* TODO: BOOL */ char supress_go_ahead;

    short text_end;             /* first free char in buffer */
    short command_start;        /* used for charmode */
    short command_end;          /* where we are up to in player cmd buffer */
    short tn_start;             /* first char of pending telnet neg */
    short tn_end;               /* first char to check for telnet negotiation */
    int32 chars_ready;          /* 32 bits so that it won't underflow twice */
    struct interactive *snoop_on; /* whom we're snooping */
    struct object      *snoop_by; /* by whom we're snooped */
    struct svalue default_err_message;
                                /* This or "What?" is printed when error */
    mp_int last_time;           /* Time of last command executed */
    int trace_level;            /* Trace flags. 0 means no tracing */
    char *trace_prefix;         /* Trace only objects which have this string
                                   as name prefix. NULL traces everything. */
    int message_length;         /* Current length of message in message_buf[] */

    struct object *next_player_for_flush;
    struct object *previous_player_for_flush;
      /* Double linked list of all active user objects with data pending
       * in message_buf[].
       */

    long access_class;
      /* represents a "cluster" where this player comes from
       */
    char charset[32];
      /* A bitflag array: every non-zero flag allows the corresponding
       * character to be sent. Characters whose flag is 0 are excluded
       * from the sent data.
       */
    /* TODO: BOOL */ char quote_iac;
    /* TODO: BOOL */ char catch_tell_activ;
    char gobble_char;           /* Char to ignore at the next telnet_neg() */
    char ts_data;               /* Telnet suboption? */

    char text[MAX_TEXT+2];
      /* The receive buffer. It can contain two extra characters:
       * a '\r' that is recognized only after another character is read,
       * and a terminating '\0'.
       */

    char message_buf[MAX_SOCKET_PACKET_SIZE];
      /* The send buffer. */
};

/* --- Bitflags and masks for interactive.noecho ---
 *
 * 'noecho' is a historical misnomer as it actually represents several
 * input modes: echo/noecho, linemode/charmode, and ignore '!' escape.
 * Echo and Charmode additionally distinguish between 'required'
 * and 'granted'.
 *
 * TODO: I admit that I'm not completely sure what the xxx_REQ, xxx and
 * xxx_ACK really mean - but I'm too tired to find out. Until it
 * becomes important, I simply accept that it works.
 */

#define CHARMODE_REQ_TO_CHARMODE(x) ((x) << 2)
  /* Transform {CHARMODE, NOECHO}_REQ into {CHARMODE, NOECHO}
   */
#define NOECHO_ACKSHIFT(x)   ((x) << 2)
#define NOECHO_ACKRSHIFT(x)  ((x) >> 2)
  /* TODO: ???
   */

#define IGNORE_BANG    128
  /* Disable input escape with leading '!'.
   */

#define NOECHO_REQ        1
  /* noecho required
   */
#define NOECHO         /* 4 */ CHARMODE_REQ_TO_CHARMODE(NOECHO_REQ)
  /* noecho active (requested via telnet negotiation)
   */
#define NOECHO_ACK    /* 16 */ NOECHO_ACKSHIFT(NOECHO)
  /* noecho acknowledged (by telnet negotiation)
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
  /* charmode required
   */
#define CHARMODE       /* 8 */ CHARMODE_REQ_TO_CHARMODE(CHARMODE_REQ)
  /* charmode active (requested via telnet negotiation)
   */
#define CHARMODE_ACK  /* 32 */ NOECHO_ACKSHIFT(CHARMODE)
  /* charmode acknowledged (by telnet negotiation)
   */
#define CHARMODE_MASK    (CHARMODE|CHARMODE_ACK)
  /* Mask for active charmode states.
   */

/* --- Variables --- */

extern struct interactive *all_players[MAX_PLAYERS];
extern int num_player;
extern char *message_flush;
extern char *domain_name;

#ifdef COMM_STAT
extern int add_message_calls;
extern int inet_packets;
extern int inet_volume;
#endif

/* --- Prototypes --- */

extern void  initialize_host_ip_number(void);
extern void  prepare_ipc(void);
extern void  ipc_remove(void);
extern void  add_message VARPROT((char *, ...), printf, 1, 2);
extern void  flush_all_player_mess(void);
extern /* TODO: BOOL */ int   get_message(char *buff);
extern void  remove_interactive(struct object *ob);
extern struct vector *users(void);
extern void  set_noecho(struct interactive *i, char noecho);
extern /* TODO: BOOL */ int   call_function_interactive(struct interactive *i, char *str);
extern void  remove_all_players(void);
extern void  set_prompt(char *str);
extern struct svalue *query_prompt(struct object *ob);
extern void  print_prompt(void);
extern int   set_snoop(struct object *me, struct object *you);
extern void  init_telopts(void);
extern void  mudlib_telopts(void);
extern struct svalue *query_ip_name(struct svalue *sp, /* TODO: BOOL */ int lookup);
extern struct svalue *input_to (struct svalue *sp, int num_arg);

#ifdef ERQ_DEMON
extern void  start_erq_demon(char *suffix);
extern struct svalue *f_attach_erq_demon(struct svalue *sp);
extern struct svalue *f_send_erq(struct svalue *sp);
#endif

#ifdef MALLOC_smalloc
extern void  clear_comm_refs(void);
extern void  count_comm_refs(void);
#endif /* MALLOC_smalloc */

extern char *query_host_name(void);
extern char *get_host_ip_number(void);
extern struct svalue *f_query_snoop(struct svalue *sp);
extern struct svalue *f_query_idle(struct svalue *sp);
extern struct svalue *f_remove_interactive(struct svalue *sp);
extern void  notify_no_command(char *command);
extern void  clear_notify(void);
extern void  set_notify_fail_message(struct svalue *svp);
extern void  free_notifys(void);
extern int   replace_interactive(struct object *ob, struct object *obfrom, char *name);

#ifdef DEBUG
extern void  count_comm_extra_refs(void);
#endif /* DEBUG */

#ifdef UDP_SEND
extern struct svalue *f_send_imp(struct svalue *sp);
#endif /* UDP_SEND */

extern struct svalue *f_set_buffer_size(struct svalue *sp);
extern struct svalue *f_binary_message(struct svalue *sp);
extern struct svalue *f_set_connection_charset(struct svalue *sp);
extern struct svalue *query_ip_port(struct svalue *sp);

#if defined(ACCESS_CONTROL)
extern void refresh_access_data(void (*add_entry)(struct sockaddr_in *, long*) );
#endif /* ACCESS_CONTROL */

#if !defined(INET_NTOA_OK)
extern char * inet_ntoa (struct in_addr ad);
#endif

#endif /* __COMM_H__ */
