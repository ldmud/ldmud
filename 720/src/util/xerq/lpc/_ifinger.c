#include "socket.h"
#include <errno.h>

#define LUSER      0
#define RUSER      1
#define HOST       2
#define CONTINUED  3

mapping sockets;

void callback(int fd, int act, mixed a, mixed b);

status main(string str)
{
    string user, host;
    int sock;
    if (!sockets) sockets = allocate_mapping(0, 4);
    if (!adminp(this_player())) return 0;
    if (!str) str="";
    host = __HOST_NAME__;
    user = str;
    sscanf(str, "%s@%s", user, host);

    sock = SOCKETD->socket_connect(host, 79, #'callback);
    sockets[sock, LUSER] = this_player();
    sockets[sock, RUSER] = user;
    sockets[sock, HOST]  = host;
    printf("[finger to: %s@%s]\n", user, host);
    call_out("time_out", 90, sock);
    return 1;
}

void callback(int sock, int act, mixed a, mixed b)
{
    if (!sockets[sock, LUSER] && act != SOCKET_CLOSE) {
	SOCKETD->socket_close(sock);
	return;
    }
    switch(act) {
      case SOCKET_READY:
	SOCKETD->socket_write(sock, sprintf("%s\r\n", sockets[sock, RUSER]));
	break;
      case SOCKET_READ:
	if (!sockets[sock, CONTINUED]) {
            tell_object(sockets[sock, LUSER], sprintf("[%s@%s]\n",
	      sockets[sock, RUSER],
	      sockets[sock, HOST]));
	    sockets[sock, CONTINUED] = 1;
	}
	tell_object(sockets[sock, LUSER], a);
	break;
      case SOCKET_CLOSE:
	m_delete(sockets, sock);
	break;
      case SOCKET_ERROR: {
	string err;
	switch(a) {
          case ECONNREFUSED:
	    err = "Connection Refused";
            break;
          case EHOSTUNREACH:
            err = "Host Unreachable";
            break;
          case ENETUNREACH:
            err = "Net Unreachable";
            break;
          default:
            err = sprintf("Code %d", a);
            break;
	}
	tell_object(sockets[sock, LUSER],
          sprintf("ifinger: Error in connection to %s: %s.\n",
            sockets[sock, HOST],
            err));
	m_delete(sockets, sock);
      }
      break;
    }
    return;
}

void help()
{
    write("Usage: ifinger [[<user>]@<host>]\n"
          "Send an internet finger request.\n");
    return;
}

void time_out(int sock)
{
    if (sockets[sock, LUSER])
	tell_object(sockets[sock, LUSER],
          sprintf("Finger to %s@%s timed out.\n",
            sockets[sock, RUSER],
            sockets[sock, HOST]));
	m_delete(sockets, sock);
	return;
}

mapping query_sockets() { return copy_mapping(sockets); }
