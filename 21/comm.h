#ifndef __COMM_H__
#define __COMM_H__ 1

/*---------------------------------------------------------------------------
 * 
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include <sys/types.h>

#include "interpret.h"  /* struct svalue, struct vector */
#include "object.h"     /* struct object */
#include "sent.h"       /* struct shadow_sentence */


#ifdef SOCKET_HEADER
#include SOCKET_HEADER
#endif

#if !defined (SOCKET_LIB) && !defined(SOCKET_INC)
#include <sys/socket.h>
#ifdef _AIX
#include <sys/socketvar.h>
#endif
#include <netinet/in.h>
#include <arpa/inet.h>
#define SOCKET_T int
#endif /* SOCKET_LIB */

#define MAX_TEXT	2048

#ifndef MAX_SOCKET_PACKET_SIZE
#if defined(sgi) && defined(mips) && defined(unix)
/* Work around for an irix kernel bug that can leave the process unresponsive
 * and unkillable.
 */
#define MAX_SOCKET_PACKET_SIZE	 200
#else
#define MAX_SOCKET_PACKET_SIZE	1024	/* Wild guess. */
#endif /* irix */
#endif /* MAX_SOCKET_PACKET_SIXE */

/* --- Types --- */
/* TODO: Document the struct input_to members */

/* input_to structures describe a pending input_to() for a given
 * interactive object. Every object can have one input-to pending, the
 * pointer to the structure is stored in the interactive sentence structure.
 */
 
struct input_to {
    struct object *ob;
    char *function;
    int num_arg;
    struct svalue arg[1];
};

struct interactive {
    struct shadow_sentence sent;
    SOCKET_T socket;
    struct object *ob;		/* Points to the associated object */
    struct input_to *input_to;	/* To be called with next input line ! */
    struct object *modify_command;
    struct svalue prompt;
    struct sockaddr_in addr;
    char closing;		/* True when closing this socket. */
    char do_close;		/* This is to be closed down. */
    char noecho;		/* Don't echo lines
				 * 0: unset 1:fresh 4:NAK, 5:ACK received */
#define NOECHO_REQ  	1
#define CHARMODE_REQ	2
#define CHARMODE_REQ_TO_CHARMODE(x) ((x) << 2)
#define NOECHO CHARMODE_REQ_TO_CHARMODE(NOECHO_REQ)
#define CHARMODE CHARMODE_REQ_TO_CHARMODE(CHARMODE_REQ)
#define NOECHO_ACKSHIFT(x) ((x) << 2)
#define NOECHO_ACKRSHIFT(x) ((x) >> 2)
#define NOECHO_ACK	NOECHO_ACKSHIFT(NOECHO)
#define CHARMODE_ACK	NOECHO_ACKSHIFT(CHARMODE)
#define NOECHO_STALE	64
#define IGNORE_BANG	128
#define NOECHO_MASK	(NOECHO|NOECHO_ACK)
#define CHARMODE_MASK	(CHARMODE|CHARMODE_ACK)

    char tn_state;
    char save_tn_state;
    char supress_go_ahead;
    short text_end;		/* first free char in buffer */
    short command_start;	/* used for charmode */
    short command_end;		/* where we are up to in player cmd buffer */
    short tn_start;		/* first char of pending telnet neg */
    short tn_end;		/* first char to check for telnet negotiation */
    int32 chars_ready;		/* 32 bits so that it won't underflow twice */
    struct interactive *snoop_on;
    struct object      *snoop_by;
    struct svalue default_err_message;	/* This or What ? is printed when error */
    int last_time;		/* Time of last command executed */
    int trace_level;		/* Debug flags. 0 means no debugging */
    char *trace_prefix;		/* Trace only object which has this as name prefix */
    int message_length;
    struct object *next_player_for_flush, *previous_player_for_flush;
#ifdef PORTALS
    int from_portal;		/* True if this player has arrived through a portal */
    int portal_socket;		/* All messages should go through this socket, if open */
    int out_portal;		/* True if going out through a portal. */
#endif /* PORTALS */
    long access_class;		/* represents a "cluster" where this player comes from */
    char charset[32];
    char quote_iac;
    char catch_tell_activ;
    char gobble_char;
    char ts_data;
    /* text can receive two extra characters: a '\r' that is recognized only
     * after another character is read, and a terminating '\0'
     */
    char text[MAX_TEXT+2];
    char message_buf[MAX_SOCKET_PACKET_SIZE];
};

/* --- Variables --- */
extern struct interactive *all_players[MAX_PLAYERS];
extern int num_player;
extern char *message_flush;

#ifdef COMM_STAT
extern int add_message_calls;
extern int inet_packets;
extern int inet_volume;
#endif

/* --- Prototypes --- */
extern void  initialize_host_ip_number PROT((void));
extern void  prepare_ipc PROT((void));
extern void  ipc_remove PROT((void));
extern void  add_message VARPROT((char *, ...), printf, 1, 2);
extern void  flush_all_player_mess PROT((void));
extern int   get_message PROT((char *buff));
extern void  remove_interactive PROT((struct object *ob));
extern struct vector *users PROT((void));
extern void  set_noecho PROT((struct interactive *i, char noecho));
extern int   call_function_interactive PROT((struct interactive *i, char *str));
extern int   set_call PROT((struct object *ob, struct input_to *it, int noecho));
extern void  remove_all_players PROT((void));
extern void  set_prompt PROT((char *str));
extern struct svalue *query_prompt PROT((struct object *ob));
extern void  print_prompt PROT((void));
extern int   new_set_snoop PROT((struct object *me, struct object *you));
extern void  init_telopts PROT((void));
extern void  mudlib_telopts PROT((void));
extern struct svalue *query_ip_name PROT((struct svalue *sp, int lookup));
extern struct svalue *input_to (struct svalue *sp, int num_arg);

#ifdef ERQ_DEMON
extern void  start_erq_demon PROT((char *suffix));
extern struct svalue *f_attach_erq_demon PROT((struct svalue *sp));
extern struct svalue *f_send_erq PROT((struct svalue *sp));
#endif

#ifdef MALLOC_smalloc
extern void  clear_comm_refs PROT((void));
extern void  count_comm_refs PROT((void));
#endif /* MALLOC_smalloc */


#ifndef INET_NTOA_OK
extern char *inet_ntoa PROT((struct in_addr ad));
#endif /* INET_NTOA_OK */

extern char *query_host_name PROT((void));
extern char *get_host_ip_number PROT((void));
extern struct svalue *f_query_snoop PROT((struct svalue *sp));
extern struct svalue *f_query_idle PROT((struct svalue *sp));
extern struct svalue *f_remove_interactive PROT((struct svalue *sp));
extern void  notify_no_command PROT((char *command));
extern void  clear_notify PROT((void));
extern void  set_notify_fail_message PROT((struct svalue *svp));
extern void  free_notifys PROT((void));
extern int   replace_interactive PROT((struct object *ob, struct object *obfrom, /*IGN*/ char *name));

#ifdef DEBUG
extern void  count_comm_extra_refs PROT((void));
#endif /* DEBUG */

#ifdef UDP_SEND
extern struct svalue *f_send_imp PROT((struct svalue *sp));
#endif /* UDP_SEND */

extern struct svalue *f_set_buffer_size PROT((struct svalue *sp));
extern struct svalue *f_binary_message PROT((struct svalue *sp));
extern struct svalue *f_set_connection_charset PROT((struct svalue *sp));

#ifdef MAXNUMPORTS
extern struct svalue *query_ip_port PROT((struct svalue *sp));
#endif

#if defined(ACCESS_CONTROL)
extern void refresh_access_data(void (*add_entry)(struct sockaddr_in *, long*) );
#endif

#endif /* __COMM_H__ */
