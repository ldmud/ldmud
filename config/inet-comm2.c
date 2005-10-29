#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <errno.h>
#include "config.h"
#include "comm.h"

static s;		/* The main socket. */

/* Kludgy dummy to fool the -g flag to cc ! */
struct object {
    int dummy;
};

prepare_ipc(port)
    int port;
{
    struct sockaddr_in sin;
    struct sockaddr_in server;
    struct hostent *hp;

    hp = gethostbyname(HOST_NAME);
    if (hp == 0) {
	fprintf(stderr, "rlogin: unknown host.\n");
	exit(1);
    }
    memset((char *)&sin, '\0', sizeof sin);
    memcpy((char *)&sin.sin_addr, hp->h_addr, hp->h_length);
    sin.sin_port = htons(port);
    sin.sin_family = hp->h_addrtype;
    s = socket(hp->h_addrtype, SOCK_STREAM, 0);
    if (s == -1) {
	perror("socket");
	abort();
    }
    server = sin;
    if (connect(s, &server, sizeof server) == -1) {
	perror("connect");
	exit(1);
    }
}

send_message(str)
    char *str;
{
    int res;
    res = send(s, str, strlen(str), 0);
    if (res == -1) {
	if (res == EINTR) {
	    printf("Command not sent!\n");
	    return;
	}
	perror("write socket");
    }
}

fd_set readfds;
int nfds = 0;

char *rcv_message() {
    static char buffer[MAX_TEXT];
    int res;
    struct timeval timeout;
    
#if 0
    FD_ZERO(&readfds);
    FD_SET(s, &readfds);
    nfds = s + 1;
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    res = select(nfds, &readfds, 0, 0, &timeout);
    if (res == -1) {
	if (errno == EINTR)
	    return 0;
	perror("select");
	abort();
    }
    if (res == 0)
	return 0;
#endif
    res = read(s, buffer, sizeof buffer);
    if (res == -1) {
	perror("read socket");
	abort();
    }
    buffer[res] = '\0';
    return buffer;
}
