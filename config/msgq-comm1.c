#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "lnode.h"	/* Only for sake of -g flag to cc (SYSV) ! */
#include "interpret.h"	/* Only for sake of -g flag to cc (SYSV) ! */
#include "comm.h"
#include "object.h"
#include "config.h"

/* Kludge because of cc -g ! */
struct sentence {
    int dummy;
};

extern char *malloc();
extern int d_flag;

#define MAX_PLAYERS	30

struct interactive *all_players[MAX_PLAYERS];

extern int errno;

/*
 * Interprocess communication interface to the backend.
 */

static msgqid;

/*
 * Use a lock file as the unique id for this message queue.
 * If the lock file is not present, then we was not started by
 * a frontend program, so we create the lock file ourselfs.
 */
prepare_ipc() {
    key_t key;
    int fd;

    key = ftok(IPC_IDENT_FILE, 0);
    if (key == (key_t)-1) {
	fd = creat(IPC_IDENT_FILE, 0666);
	if (fd == -1) {
	    perror(IPC_IDENT_FILE);
	    exit(1);
	}
	close(fd);
	key = ftok(IPC_IDENT_FILE, 0);
	if (key == (key_t)-1) {
	    perror(IPC_IDENT_FILE);
	    exit(1);
	}
	/* We have to create the message queue. */
	msgqid = msgget(key, IPC_CREAT|0666);
	if (msgqid == -1) {
	    perror("msgget");
	    exit(1);
	}
	return;
    }
    /* Find the already existing message queue. */
    msgqid = msgget(key, 0);
    if (msgqid == -1) {
	perror("msgget");
	exit(1);
    }
}

/*
 * This one is called when shutting down the MUD.
 */
ipc_remove() {
    int res;

    printf("Shutting down ipc...\n");
    res = msgctl(msgqid, IPC_RMID);
    if (res == -1)
	perror("msgctl IPC_RMID");
    if (unlink(IPC_IDENT_FILE) == -1) {
	perror(IPC_IDENT_FILE);
	fprintf(stderr, "Warning could not unlink %s\n", IPC_IDENT_FILE);
    }
}

add_message(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
{
    char buff[1000], length;
    struct interactive *ip;

    ip = command_giver->interactive;
    if (ip == 0)
	error("Add_message to a non-interactive object: %s!\n",
	      command_giver->name);
    sprintf(buff, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    if (d_flag)
	debug_message("[%s]: %s", command_giver->name, buff);
    length = strlen(buff);
    if (length + ip->mess_size > MAX_TEXT)
	flush_message();
    strcpy(ip->message + ip->mess_size, buff);
    ip->mess_size += length;
}

flush_message()
{
    struct interactive *ip = command_giver->interactive;
    int res;

    if (ip == 0)
	error("Add_message to a non-interactive object: %s!\n",
	      command_giver->name);
    if (ip->mess_size == 0)
	return;
    if (ip->mess_size == -1)
	ip->mess_size = 0;	/* Indicates a null message. */
    ip->mtype = ip->pid;
    while(1) {
	res = msgsnd(msgqid, (struct msgbuf *)ip,
		     ip->mess_size, 0);
	if (res == -1) {
	    if (errno == EINTR)
		continue;
	    perror("msgsnd");
	    abort();
	    exit(1);
	}
	ip->mess_size = 0;
	ip->message[0] = '\0';
	return;
    }
}

struct rec_buffer rec_buffer;

/*
 * Get a message from any player.
 * If we get a message, set command_giver to the players object and
 * return true.
 * If we are interrupted, return false (no message yet).
 */

get_message(buff, size)
    char *buff;
{
    int i, res;

    /*
     * Stay in this loop until we have a message to a logged on player.
     */
    while(1) {
	char *p;

	/* First flush all send messages. */
	for (i=0; i<MAX_PLAYERS; i++) {
	    if (all_players[i] != 0 && all_players[i]->mess_size > 0) {
		command_giver = all_players[i]->ob;
		flush_message();
	    }
	}
	res = msgrcv(msgqid, &rec_buffer, sizeof rec_buffer.text, 1, 0);
	if (res == -1 && errno == EINTR)
	    return 0;
	if (p = strchr(rec_buffer.text, '\n'))
	    *p = '\0';
	if (res == -1) {
	    perror("msgrcv");
	    exit(1);
	}
	/*
	 * Now we have a message. Find which object it is associated with
	 * this message by looking at the pid.
	 */
	for (i=0; i<MAX_PLAYERS; i++) {
	    if (all_players[i] == 0)
		continue;
	    if (all_players[i]->pid != rec_buffer.pid)
		continue;
	    command_giver = all_players[i]->ob;
	    strcpy(buff, rec_buffer.text);
	    return 1;
	}
	/*
	 * We come here when no object was associated with this pid.
	 * Then we add a new player !
	 */
	for (i=0; i<MAX_PLAYERS; i++) {
	    struct object *ob;
	    
	    if (all_players[i] != 0)
		continue;
	    current_object = 0;
	    ob = (clone_object("obj/player"))->u.ob;
	    if (ob == 0)
		error("Could not load 'obj/player'\n");
	    ob->enable_commands = 1;
	    ob->interactive =
		(struct interactive *)malloc(sizeof (struct interactive));
	    ob->interactive->logon_state = 0;
	    ob->interactive->pid = rec_buffer.pid;
	    ob->interactive->mess_size = 0;
	    all_players[i] = ob->interactive;
	    command_giver = ob;
	    ob->interactive->ob = ob;
	    (void)logon(ob, rec_buffer.text);
	    break;
	}
	/* Did we not find any free player data structure ? */
	if (i == MAX_PLAYERS)
	    send_null_message(rec_buffer.pid);
    }
}

remove_interactive(ob)
    struct object *ob;
{
    struct object *save = command_giver;
    int i;

    for (i=0; i<MAX_PLAYERS; i++) {
	if (all_players[i] != ob->interactive)
	    continue;
	command_giver = ob;
	flush_message();
	/* Send a null message to his frontend, to indicate
	 * that he is logged of.
	 */
	send_null_message(ob->interactive->pid);
	free(ob->interactive);
	all_players[i] = 0;
	ob->interactive = 0;
	command_giver = save;
	return 0;
    }
    fprintf(stderr, "Could not find and remove player %s\n", ob->name);
    exit(1);
}

static send_null_message(pid) {
    long mtype;
    int res;

    mtype = pid;
    res = msgsnd(msgqid, (struct msgbuf *)&mtype, 0, 0);
    if (res == -1) {
	perror("msgsnd null message");
	abort();
	exit(1);
    }
}
