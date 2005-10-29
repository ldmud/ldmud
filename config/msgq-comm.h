#define MAX_TEXT	1024

struct message {
    long mtype;
    char text[MAX_TEXT];
};

struct interactive {
    long mtype;			/* This is used by message queues */
    char message[MAX_TEXT];
    int logon_state;		/* How far have we come on the logon ? */
    int mess_size;
    int pid;			/* Process id of frontend. */
    struct object *ob;		/* Points to the associated object */
    struct sentence *input_to;	/* To be called with next input line ! */
};

#define L_WAITING_FOR_NAME	1
#define L_LOGGED_ON		2

/* The receive buffer. */

struct rec_buffer {
    long mtype;			/* Used as dest address. 1 == backend. */
    int pid;			/* Identifies who it is from. */
    char text[MAX_TEXT];
};

#define BACKEND_IN_BACKGROUND
#define IPC_IDENT_FILE		"/tmp/lpmud_lock"
