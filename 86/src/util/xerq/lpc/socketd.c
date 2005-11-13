// LPC socket daemon
// written by Brian Gerst (Garion@Timewarp)

#include <erq.h>
#include "socket.h"

#define S_TICKET	0
#define S_STATE		1
#define S_OPTS		2
#define S_HOST		3
#define S_PORT		4
#define S_CALLBACK	5
#define S_OBJECT	6

#define S_UNCONNECTED	0
#define S_CONNECTED	1
#define S_LISTEN	2
#define S_UDP		3
#define S_CLOSING	4
#define S_ACCEPT	5

#define WRITE_MAX	__ERQ_MAX_SEND__ - 9

private mapping sockets=([ ]);
int *used_fd;
mixed *output_queue;

void socket_close(int fd);
static void sendto2(int fd, int *host, int port, mixed mesg);
int valid_socket(object ob);

// this is to fix the returning of signed numbers
#define FIX_REPLY

#if 1
#define debug(X) if (find_player("garion")) \
		 tell_object(find_player("garion"), X)
#endif

#ifdef COMPAT_FLAG
void reset(int arg)
{
    if (arg) return;
#else
void create()
{
#endif
    if (!sockets) sockets=allocate_mapping(0,7);
    if (!used_fd) used_fd=allocate(MAX_SOCKETS);
}

static void callback(int fd, int action, mixed args)
{
    if (sockets[fd, S_STATE] == S_CLOSING) return;
    if (!sockets[fd, S_OBJECT]) { /* bound object has been dested, close */
	sockets[fd, S_STATE]=S_CLOSING;
	socket_close(fd);
	return;
    }
    apply(sockets[fd, S_CALLBACK], fd, action, args);
}

int flush_output()
{
    int res, i;

    for (i=0; i<sizeof(output_queue); i++) {
	res=apply(#'efun::send_erq, output_queue[i]);
	if (!res) {
	  output_queue=output_queue[i..];
	  call_out(#'flush_output, 1);
	  return 0;
	}
    }
    output_queue=0;
    return res;
}

static int send_erq(int req, mixed data, closure cb)
{
    int res;

    if (output_queue) {
	output_queue+=({ ({ req, data, cb }) });
	return 0;
    }
    res=efun::send_erq(req, data, cb);
    if (!res) {
	output_queue=({ ({ req, data, cb }) });
	call_out(#'flush_output, 1);
    }
    return res;
}

static void lookup(string host, closure cb)
{
    int *a;
    a=allocate(4);
    if (sscanf(host, "%d.%d.%d.%d", a[0], a[1], a[2], a[3])==4) {
	funcall(cb, a);
	return;
    }
    send_erq(ERQ_LOOKUP, host, lambda(({'a}),
      ({ #'?,
        ({ #'==, ({ #'[, 'a, 0 }), ERQ_OK }),
	({ #'funcall, cb,
#ifdef FIX_REPLY
	  ({ #'map_array, ({ #'[..], 'a, 1, 4 }), #'&, 255 })
#else
	  ({ #'[..], 'a, 1, 4 })
#endif
	}),
	({ #'funcall, cb, 0 })
      }) ));
}

static void socket_cb(int *a, int fd)
{
    int state, opts, port;
    string host;

#ifdef FIX_REPLY
    a=map_array(a, #'&, 255);
#endif
    state=sockets[fd, S_STATE];
    opts=sockets[fd, S_OPTS];
    switch(a[0]) {
      case ERQ_OK: {
	if (sockets[fd, S_TICKET]) return;
	switch(state) {
	  case S_ACCEPT: {
	    sockets[fd, S_TICKET]=a[7..];
	    host=sockets[fd, S_HOST]=sprintf("%d.%d.%d.%d",
	      a[1], a[2], a[3], a[4]);
	    port=sockets[fd, S_PORT]=a[5]*256+a[6];
	    sockets[fd, S_STATE]=S_CONNECTED;
	    callback(fd, SOCKET_READY, ({ host, port }));
	    break;
	  }
	  default:
	    sockets[fd, S_TICKET]=a[1..];
	    callback(fd, SOCKET_READY, 0);
	}
	break;
      }
      case ERQ_STDOUT: {
	switch(state) {
	  case S_LISTEN:
	    callback(fd, SOCKET_ACCEPT, 0);
	    break;
	  case S_CONNECTED:
	    a=a[1..];
	    callback(fd, SOCKET_READ,
	      opts & SOCKET_BINARY ? quote(a) : to_string(a));
	    break;
	  case S_UDP: {
	    host=sprintf("%d.%d.%d.%d", a[1], a[2], a[3], a[4]);
	    port=a[5]*256+a[6];
	    a=a[7..];
	    callback(fd, SOCKET_READ, ({opts & SOCKET_BINARY ?
	      quote(a) : to_string(a), host, port }));
	    break;
	  }
	}
	break;
      }
      case ERQ_EXITED:
        callback(fd, SOCKET_CLOSE, 0);
	m_delete(sockets, fd);
	used_fd[fd]=0;
	break;
      default:
	callback(fd, SOCKET_ERROR, a);
	break;
    }
}

int socket_listen(int port, closure cb)
{
    int fd;
    object ob;

    if (!intp(port)) raise_error("Bad arg 1 to socket_listen().\n");
    if (!closurep(cb) || !(ob=to_object(cb)))
	raise_error("Bad arg 2 to socket_listen().\n");
    if (!valid_socket(ob))
	raise_error("privilege violation : socket_listen()\n");

    fd=member(used_fd, 0);
    if (fd < 0) return -1;
    used_fd[fd]=1;
    sockets+=([ fd:0; S_LISTEN; 0; "127.0.0.1"; port; cb; ob ]);
    send_erq(ERQ_LISTEN, ({ port/256, port&255 }), lambda(({'a}),
      ({ #'socket_cb, 'a, fd })));
    return fd;
}

varargs int socket_accept(int fd, closure cb, int opts)
{
    int new;
    object ob;

    if (!intp(fd)) raise_error("Bad arg 1 to socket_accept().\n");
    if (!closurep(cb) || !(ob=to_object(cb)))
	raise_error("Bad arg 2 to socket_accept().\n");
    if (!intp(opts)) raise_error("Bad arg 3 to socket_accept().\n");
    if (ob!=sockets[fd, S_OBJECT])
	raise_error("privilege violation : socket_accept()\n");

    if (sockets[fd, S_STATE] != S_LISTEN)
	raise_error("socket_accept() called on non-listening socket.\n");
    new=member(used_fd, 0);
    if (new < 0) return -1;
    used_fd[new]=1;
    sockets+=([ new:0; S_ACCEPT; opts; 0; 0; cb; ob ]);
    send_erq(ERQ_ACCEPT, sockets[fd, S_TICKET], lambda(({'a}),
      ({ #'socket_cb, 'a, new })));
    return new;
}

int socket_write(int fd, mixed mesg)
{
    int size, chunk, *ticket, *out, pos;

    if (!intp(fd)) raise_error("Bad arg 1 to socket_write().\n");
    if (!stringp(mesg) && !pointerp(mesg))
	raise_error("Bad arg 2 to socket_write().\n");

    if (sockets[fd, S_STATE] != S_CONNECTED)
	raise_error("socket_write on an unconnected socket.\n");
    if (previous_object()!=sockets[fd, S_OBJECT])
	raise_error("privilege violation : socket_write()\n");

    size=(stringp(mesg)) ? strlen(mesg) : sizeof(mesg);
    ticket=sockets[fd, S_TICKET];
    chunk=WRITE_MAX-sizeof(ticket);
    for (; pos<size; pos+=chunk) {
	out=ticket+to_array(mesg[pos..(pos+chunk-1)]);
	if (stringp(mesg)) out=out[0..<2];
	send_erq(ERQ_SEND, out, 0);
    }
}

varargs int socket_connect(string host, int port, closure cb, int opts)
{
    int fd, *a;
    object ob;

    a=allocate(6);
    if (!stringp(host)) raise_error("Bad arg 1 to socket_connect().\n");
    if (!intp(port)) raise_error("Bad arg 2 to socket_listen().\n");
    a[4]=port/256;
    a[5]=port&255;
    if (!closurep(cb) || !(ob=to_object(cb)))
	raise_error("Bad arg 3 to socket_listen().\n");
    if (!intp(opts)) raise_error("Bad arg 4 to socket_listen().\n");
    if (!valid_socket(ob))
	raise_error("privilege violation : socket_connect()\n");

    fd=member(used_fd, 0);
    if (fd < 0) return -1;
    used_fd[fd]=1;
    sockets+=([ fd:0; S_CONNECTED; opts; host; port; cb; ob ]);

    lookup(host, lambda(({'a}),
      ({ #'?, 'a,
        ({ #',,
	  ({ #'send_erq, ERQ_OPEN_TCP,
	    ({ #'+, 'a, '({ port/256, port&255 }) }), lambda(({'a}),
	      ({ #'socket_cb, 'a, fd })) }),
          ({ #'=,
	    ({ #'[, ({ #'sockets }), fd, S_HOST }),
	    ({ #'apply, #'sprintf, "%d.%d.%d.%d", 'a }) }) }),
	({ #'callback, fd, SOCKET_ERROR, '({ ERQ_E_NOTFOUND, 0 }) })
      }) ));
    return fd;
}

int socket_udp(int port, closure cb)
{
    int fd;
    object ob;

    if (!intp(port)) raise_error("Bad arg 1 to socket_udp().\n");
    if (!closurep(cb) || !(ob=to_object(cb)))
	raise_error("Bad arg 2 to socket_udp().\n");
    if (!valid_socket(ob))
	raise_error("privilege violation : socket_udp()\n");

    fd=member(used_fd, 0);
    if (fd < 0) return -1;
    used_fd[fd]=1;
    sockets+=([ fd:0; S_UDP; 0; "127.0.0.1"; port; cb; ob ]);
    send_erq(ERQ_OPEN_UDP, ({ port/256, port&255 }), lambda(({'a}),
      ({ #'socket_cb, 'a, fd })));
    return fd;
}

varargs int socket_sendto(int fd, string host, int port, mixed mesg)
{
    if (!intp(fd)) raise_error("Bad arg 1 to socket_sendto().\n");
    if (!stringp(host)) raise_error("Bad arg 2 to socket_sendto().\n");
    if (!intp(port)) raise_error("Bad arg 3 to socket_sendto().\n");
    if (!stringp(mesg) && !pointerp(mesg))
	raise_error("Bad arg 4 to socket_udp().\n");
    if (sockets[fd, S_STATE] != S_UDP)
	raise_error("socket_sendto on a non-udp socket.\n");
    if (!valid_socket(previous_object()))
	raise_error("privilege violation : socket_sendto()\n");

    lookup(host, lambda(({'a}),
      ({ #'sendto2, fd, 'a, port, mesg })));
    return fd;
}

static void sendto2(int fd, int *host, int port, mixed mesg)
{
    int size, *ticket, chunk, *out, pos;
    if (!host) {
	callback(fd, SOCKET_ERROR, ({ ERQ_E_NOTFOUND, 0 }));
	return;
    }
    size=(stringp(mesg)) ? strlen(mesg) : sizeof(mesg);
    ticket=sockets[fd, S_TICKET]+host+({ port/256, port&255 });
    chunk=WRITE_MAX-sizeof(ticket);
    for (; pos<size; pos+=chunk) {
	out=ticket+to_array(mesg[pos..(pos+chunk-1)]);
	if (stringp(mesg)) out=out[0..<2];
	send_erq(ERQ_SEND, out, 0);
    }
}

int socket_transfer(int fd, closure cb)
{
    object ob;

    if (!intp(fd)) raise_error("Bad arg 1 to socket_transfer().\n");
    if (!closurep(cb)) raise_error("Bad arg 2 to socket_transfer().\n");

    if (!member(sockets, fd)) return 0;
    if (previous_object() != sockets[fd, S_OBJECT] || !valid_socket(ob))
	raise_error("privilege violation : socket_transfer()\n");
    cb=funcall(cb, fd, SOCKET_TRANSFER, previous_object());
    if (!closurep(cb) || !(ob=to_object(cb)))
	raise_error("Bad return value from callback\n");
    sockets[fd, S_OBJECT]=ob;
    sockets[fd, S_CALLBACK]=cb;
    callback(fd, SOCKET_READY, 1);
    return 1;
}

mixed socket_address(int fd)
{
    if (!intp(fd)) raise_error("Bad arg 1 to socket_address().\n");
    if (!member(sockets, fd)) return 0;

    return ({ sockets[fd, S_HOST], sockets[fd, S_PORT] });
}

int socket_close(int fd)
{
    int *ticket;
    if (!intp(fd)) raise_error("Bad arg 1 to socket_close().\n");
    if (extern_call() && previous_object()!=sockets[fd, S_OBJECT])
	raise_error("privilege violation : socket_close()\n");

    if (!member(sockets, fd)) return 0;
    ticket=sockets[fd, S_TICKET];
    if (!ticket) return 0;
    send_erq(ERQ_KILL, sockets[fd, S_TICKET], 0);
    return 1;
}

#ifdef debug
mapping query_sockets() { return copy_mapping(sockets); }
#endif

void destructor()
{
    if ((string)previous_object() != __MASTER_OBJECT__) return;
    map_array(m_indices(sockets), #'socket_close);
}

int valid_socket(object ob)
{
    return (file_name(ob)[0..6]=="secure/");
}
