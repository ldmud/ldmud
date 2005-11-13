#ifndef COMM_H
#define COMM_H

#include <sys/types.h>

#include "sent.h"

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
#endif /* SOCKET_LIB */

#include "interpret.h"

#if !defined (SOCKET_LIB) && !defined(SOCKET_INC)
#define SOCKET_T int
#define socket_number(s) (s)
#define socket_ioctl  ioctl
#define socket_select select
#define socket_read   read
#define socket_write  write
#define socket_close  close
#endif /* SOCKET_LIB */

#if defined(SunOS4) || defined(atarist)
SOCKET_T socket PROT((int, int, int));
int getpeername PROT((SOCKET_T, struct sockaddr *, int *));
void  shutdown PROT((SOCKET_T, int));
int setsockopt PROT((SOCKET_T, int, int, char *, int));
int bind PROT((SOCKET_T, struct sockaddr *, int));
int listen PROT((SOCKET_T, int));
SOCKET_T accept PROT((SOCKET_T, struct sockaddr *, int *));
struct timeval;
int select PROT((int, fd_set *, fd_set *, fd_set *, struct timeval *));
#endif /* SunOS4 */

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

#define MESSAGE_FLUSH ((char*)NULL)

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
#ifdef ACCESS_RESTRICTED
    long access_class;		/* represents a "cluster" where this player comes from */
#endif
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

void set_prompt PROT((char *));
struct svalue *query_prompt PROT((struct object *));
void set_noecho PROT((struct interactive *, char));

#endif /* COMM_H */
