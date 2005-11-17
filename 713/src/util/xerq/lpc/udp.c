#define PORT 6000

#include "socket.h"

int fd;
object tp;

#define reply(X) tell_object(tp, X);

void cb(int fd, int act, mixed a, mixed b, mixed c)
{
  switch(act) {
    case SOCKET_READY:
      reply("Ready.\n");
      return;
    case SOCKET_READ:
      reply(sprintf("From %O %O:\n", b, c));
      reply(a);
      return;
    case SOCKET_ERROR:
      reply(sprintf("Error: %d %d\n", a, b));
      return;
    case SOCKET_CLOSE:
      destruct(this_object());
      return;
  }
}

void reset(int arg) {
  if (arg) return;
  tp=this_player();
  fd=SOCKETD->socket_udp(PORT, #'cb);
}

send(string str)
{
    SOCKETD->socket_sendto(fd, "127.0.0.1", PORT, str);
}

destructor()
{
    SOCKETD->socket_close(fd);
}
