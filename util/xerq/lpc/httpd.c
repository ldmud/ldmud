#include "socket.h"

#if 0
#define debug(x) if (find_player("garion")) \
		    tell_object(find_player("garion"), (x))
#else
#define debug(x)
#endif

#define WWW_PATH "secure/sockets/www"
#define ERROR "<PLAINTEXT>Error\n"

int sock;

static string parse_request(string file);
static void listen_call(int fd, int act, mixed a, mixed b);
static void callback(int fd, int act, mixed a, mixed b);

void reset(int arg)
{
    if (arg) return;
    sock=SOCKETD->socket_listen(5152, #'listen_call);
    debug("httpd started.\n");
}

static void listen_call(int fd, int act, mixed a, mixed b)
{
    debug(sprintf("listen_call(%d,%d,%O,%O)\n",fd,act,a,b));
    switch(act) {
      case SOCKET_READY:
	return;
      case SOCKET_ACCEPT:
	SOCKETD->socket_accept(fd, #'callback);
	return;
      case SOCKET_ERROR:
	sock=-1;
      case SOCKET_CLOSE:
	destruct(this_object());
	return;
    }
}

static void callback(int fd, int act, mixed a, mixed b)
{
    debug(sprintf("callback(%d,%d,%O,%O)\n",fd,act,a,b));
    switch(act) {
      case SOCKET_READ: {
	string file, *tmp;
	tmp=explode(a, "\r\n");
	tmp=explode(tmp[0], " ");
	if (tmp[0]!="GET") {
	    SOCKETD->socket_close(fd);
	    return;
	}
	file=tmp[1];
	a=parse_request(file);
	SOCKETD->socket_write(fd, a);
	SOCKETD->socket_close(fd);
	return;
      }
      case SOCKET_ERROR: {
	SOCKETD->socket_close(fd);
      }
    }
}

static string parse_request(string file)
{
    string data, tmp, *args;

    args=explode(file, "/")-({""});
    if (!sizeof(args)) return ERROR;

    file=sprintf("%s/%s.c", WWW_PATH, args[0]);

    if (file_size(file) > 0) {
	if (tmp=catch(data=call_other(file, "www_main", args[1..]))) {
	    return ERROR;
	}
	return data;
    }
    return ERROR;
}

void destructor()
{
    if (sock<0) return;
    SOCKETD->socket_close(sock);
}
