#include "socket.h"
#include <errno.h>

#define DEFAULT_PORT 23

object user;
string buff="",
       host,
       log_file;
int    sock,
       port;
status attached,
       connected;

static void callback(int fd, int act, mixed a, mixed b);
static void timeout();

void start(string h, int p)
{
    if (!adminp(this_player())) return 0;
    sock=(int)SOCKETD->socket_connect(h, p, #'callback);
    connected=0;
    printf("Connecting to %s port %d\n", h, p);
    user=this_player();
    host=h;
    port=p;
    attached=1;
    call_out(#'timeout, 15);
    input_to("input");
    return;
}

void reattach()
{
    printf("[telnet: Reattaching to %s%c%s]\n",
      host,
      port == DEFAULT_PORT ? 0 :' ',
      port == DEFAULT_PORT ? "":to_string(port));
    attached=1;
    write(buff);
    buff="";
    input_to("input");
    return;
}

void tell_user(string msg)
{
    if (attached)
	tell_object(user, msg);
    else
	buff=sprintf("%s%s", buff, msg);
    if (!attached && member(msg, '') != -1)
	tell_object(user, sprintf("[telnet: Beep from %s%c%s]\n",
	    host,
	    port == DEFAULT_PORT ? 0 :' ',
	    port == DEFAULT_PORT ? "":to_string(port)));
    if (log_file)
	write_file(log_file, msg);
    return;
}

static void timeout()
{
    if (connected) return;
    tell_user("telnet: connection timed out.\n");
    if (!attached)
	tell_object(user, buff);
    sock=-1;
    user=0;
    destruct(this_object());
}

void callback(int fd, int act, mixed a, mixed b)
{
    switch(act) {
      case SOCKET_READY:
	tell_user("telnet: connected.\n");
	remove_call_out(#'timeout);
	connected=1;
      break;
      case SOCKET_READ:
	tell_user(a);
	break;
      case SOCKET_CLOSE:
	tell_user("telnet: connection closed.\n");
	if (!attached)
	    tell_object(user, buff);
	sock=-1;
	user=0;
	connected=0;
	destruct(this_object());
	break;
      case SOCKET_ERROR: {
	string err;
	if (a == ERQ_E_NOTFOUND) err="Unknown host";
	else switch(b) {
	  case ECONNREFUSED:
            err="Connection Refused";
	    break;
          case EHOSTUNREACH:
            err="Host Unreachable";
            break;
          case ENETUNREACH:
            err="Net Unreachable";
            break;
          default:
            err=sprintf("Error %d", b);
	    break;
	}
	tell_user(sprintf("telnet: %s\nclosing connection.\n", err));
	if (!attached)
	    tell_object(user, buff);
	sock=-1;
	user=0;
	destruct(this_object());
      }
      break;
    }
    return;
}

void input(string str)
{
    if (sock<0) return;
    if (str[0]=='') {
      switch(str[1]) {
	case 'c': {
	  if (!log_file)
	      write("telnet: Not logging session.\n");
	  else {
	      printf("telnet: Stopped logging session to: %s.\n", log_file);
	      log_file=0;
          }
	  input_to("input");
	  return;
	}
	case 'd': {
	  tell_object(user, "Detached.  Type 'telnet' to resume.\n");
	  attached=0;
	  return;
	}
	case 'l': {
	  if (!str[2]) {
	      if (log_file)
		  printf("telnet: Logging session to: %s.\n", log_file);
	      else
		  write("telnet: Not logging session.\n");
	  } else {
	      str=eval_path(trimstr(str[2..]));
	      if (file_size(str) == -2)
		  str=sprintf("%s/telnet_log", str);
	      printf("telnet: Logging session to: %s.\n", str);
	      log_file=str;
	  }
	  input_to("input");
	  return;
	}
	case 'q': {
	  SOCKETD->socket_close(sock);
	  tell_user("telnet: connection closed.\n");
	  if (!attached)
	      tell_object(user, buff);
	  user=0;
	  sock=-1;
	  destruct(this_object());
	  return;
	}
	case 's': {
	  str=eval_path(trimstr(str[2..]));
	  if (file_size(str) < 0)
	      printf("telnet: Could not find file: %s.\n", str);
	  else if (connected) {
	      string *lines;
	      lines=explode(read_file(str), "\n");
	      tell_user(sprintf("telnet: Sending file: %s.\n", str));
	      for(; sizeof(lines); lines=lines[1..])
	          SOCKETD->socket_write(sock, sprintf("%s\n", lines[0]));
	      tell_user("telnet: File sent.\n");
	  }
	  input_to("input");
	  return;
	}
	case '!': {
	  str=str[1..];
	}
	case '?': {
	  write("telnet: Escape commands:\n"
		"All commands are preceeded by the escape (^[) character.\n"
		"\n"
		"c          Close session log.\n"
		"d          Detach telnet session.\n"
		"l<file>    Open session log into <file>.\n"
		"q          Forcefully close telnet session.\n"
		"s<file>    Send a file to telnet session.\n"
		"!<arg>     Send '!<arg>' to remote host.\n"
		"?          This help page.\n");
	  input_to("input");
	  return;
	}
      }
    }
    if (connected) {
	SOCKETD->socket_write(sock, str=sprintf("%s\n", str));
	if (log_file)
	    write_file(log_file, str);
    }
    input_to("input");
    return;
}

status clean_up(status arg)
{
    if (!user)
	destruct(this_object());
    return 1;
}
