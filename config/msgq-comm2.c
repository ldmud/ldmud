#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <errno.h>
#include "config.h"
#include "comm.h"

static msgqid;
static our_pid;

/* Kludgy dummy to fool the -g flag to cc ! */
struct object {
    int dummy;
};

/*
 * Use a lock file as the unique id for this message queue.
 * If the lock file is not present, then we are the first to play,
 * so we create the lock file ourselfs.
 */
prepare_ipc() {
    key_t key;
    int fd;

    our_pid = getpid();
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
	    abort();
	}
	return;
    }
    /* Find the already existing message queue. */
    msgqid = msgget(key, 0);
    if (msgqid == -1) {
	perror("msgget");
	abort();
    }
}

send_message(str)
    char *str;
{
    struct rec_buffer rec_buffer;
    int length, res;

    length = strlen(str);
    rec_buffer.mtype = 1;
    rec_buffer.pid = our_pid;
    if (length >= sizeof rec_buffer.text)
	length = sizeof rec_buffer - 1;
    strncpy(rec_buffer.text, str, sizeof rec_buffer.text);
    rec_buffer.text[length] = '\0';
    res = msgsnd(msgqid, &rec_buffer, length + sizeof rec_buffer.pid, 0);
    if (res == -1) {
	perror("msgsnd");
	abort();
    }
}

struct buffer {
    int mtype;
    char text[MAX_TEXT];
};

char *rcv_message() {
    static struct buffer buffer;
    int res;

    res = msgrcv(msgqid, &buffer, sizeof buffer.text, our_pid, 0);
    if (res == -1) {
	if (errno == ENOMSG)
	    return 0;
	perror("msgrcv");
	abort();
    }
    buffer.text[res] = '\0';
    return buffer.text;
}
