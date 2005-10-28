//
// Wunderland Mudlib
//
// global/daemon/socketd.c  --  internet-sockets (needs xerq)
//
// This file comes in two guises - the blueprint is the master
// actual managing all data transfers and the clones representin
// the connections for the using wizards. They get the socket object
// from the master and handle all subsequent issues with object.
//
// Far based on socketd from Brian Gerst (Garion@Timewarp)
// Inspired by the Msg modul from Sriram Srinivasan
// Mudmode conversion based on code from Skylight (pli@shell.portal.com)
//
// Find the newest version of this file at
//   http://wl.mud.de/mud/doc/wwwstd/standardobjekte.html
//
// $Log: socketd.c,v $
// Revision 1.15  2004/11/10 07:25:06  Fiona
// Fixed warning problem with 3.3 in State() (Joseph Graham, Lars Duening)
//

/*****
 *
 * How does the socketd work internally?
 *
 * Each connection has an associated socket object which handles all
 * communication with the wizards object. These objects stay in the
 * inventory of the socketd.
 *
 * To destringuish the different connections when a messages comes
 * in from the erq the clone number of the socket object is used.
 * The number is closured with the callback function. The number
 * is used instead of the object itself because the object may be
 * destructed meanwhile: the connection is to be killed then but the
 * ticket is needed for that. So we need some unique identifier that
 * is still usable after the object's destruction.
 * 
 * All opened connections are stored in sockets with the unique identifier
 * (the clone number of the socket object) as key. Furthermore there is
 * used_fd which translates unique identifieres to file descriptor numbers
 * which are used outside the socketd to identify a connection. The fds
 * are numbered from 0 to n using old numbers if that fd is not active
 * anymore.
 *
 * If Write() or Send() is issued on a connection that is not open yet
 * the data will be buffered and sent when the socket becomes writabe.
 * It is even possible to Close() the socket wich will be delayed until
 * all buffered data is sent. Bear in mind that a Close() will not destruct
 * the socket object in that case and you will get the callbacks if the
 * issuing object still exists. If it does not exist anymore you cannot
 * tell if the data is really sent or some timeout occured. So in general
 * you should wait with Send()s until the connection is established.
 *
 */

#pragma strict_types
#pragma no_inherit
#pragma no_shadow

#include <wizlevels.h>
#include <erq.h>

#define NEED_PROTOTYPES
#include <daemon/socket.h>

#ifdef ME
#undef ME
#endif
#define ME this_object()
#ifdef PO
#undef PO
#endif
#define PO previous_object()

// Access for internal output cache
#define OUT_UFD  0  // destination FD
#define OUT_DATA 1  // the data itself
#define OUT_SIZE 2  // size of all previous chunks of this Write()

mapping sockets;    // all the open sockets
mixed*  output;     // things to be sent
int     sending;    // answer from erq pending
mapping used_fd;    // the 'file descriptor' translation
int     so_fd;      // fd of socket object
int     so_ufd;     // socket object's clone number
string  so_buf;     // mudmode buffer
mixed   so_bufs;    // expected packet size
int     sent, rcvt; // number of sent and received bytes
object  debugger;
private string* valids;

void create() {
  // _debug(1);
  if (clonep()) {
    if (!PO || object_name(PO) != SOCKETD) {
      destruct(ME);
      return;
    }
    move_object(ME, PO);
    so_fd = -1;
    return;
  }
  seteuid(getuid());
  if (!sockets) sockets = m_allocate(0, 17);
  if (!used_fd) used_fd = ([]);
  if (!output)  output  = ({});
  debug("*** created socket daemon");
}

//////
// functions used in the socket objects only

// socket object is initialized from the daemon here
object so_init(mapping socks, mapping usedfd, int fd) {
  if (!clonep()) return 0;
  if (object_name(PO) != SOCKETD) return 0;
  if (sockets) return ME;
  sockets  = socks;  // local reference to global mapping
  used_fd  = usedfd; // dito
  so_fd    = fd;
  so_ufd   = UNIQUE_ID(ME);
  return ME;
}

// some checks for the user functions in the socket objects
static int so_check(string fun) {
  if (!clonep()) return 0;
  if (so_fd < 0 || !member(sockets, so_fd)) {
    remove();
    RAISE_ERROR("uninitialized socket object\n");
  }
  if (!member(used_fd, so_ufd) || used_fd[so_ufd] != so_fd) {
    remove();
    RAISE_ERROR("filehandle mismatch\n");
  }
  if (!sockets[so_fd, S_OBJECT]) {
    remove();
    return 0;
  }
  if (PO != sockets[so_fd, S_OBJECT])
    RAISE_ERROR("privilege violation: "+fun+"()\n");
  return 1;
}

// write data over the socket (tcp only) (user function)
public int Write(mixed data) {
  int size;
  if (!so_check("Write")) return 0;
  if (sockets[so_fd, S_OPTS] & SOCKET_MUDMODE) {
    data = to_mudmode(data);
    data = to_array(data)[0..strlen(data)-1];
    size = sizeof(data);
    data = ({ (size & 0x7f000000) >> 24, (size & 0xff0000) >> 16,
              (size & 0xff00) >> 8, size & 0xff }) + data;
  } else {
    if (!stringp(data) && !pointerp(data))
      RAISE_ERROR("Bad argument 1 to Write()\n");
  }

  return (int) SOCKETD->socket_write(so_ufd, data, 0);
}

// send data over the socket (udp only) (user function)
public int Send(string host, int port, mixed msg) {
  if (!so_check("Send")) return 0;
  if (!stringp(host)) RAISE_ERROR("Bad argument 1 to Send()\n");
  if (!intp(port)) RAISE_ERROR("Bad argument 2 to Send()\n");
  if (!stringp(msg) && !pointerp(msg))
    RAISE_ERROR("Bad argument 3 to Send()\n");
  if (sockets[so_fd, S_STATE] != S_UDP)
    RAISE_ERROR("Send() on a non-udp socket\n");
  
  return (int) SOCKETD->udp_write(so_ufd, host, port, msg);
}

// accepts a new connection (tcp_listen only) (user function)
public object Accept(closure cb) {
  object ob;
  if (!so_check("Accept")) return 0;
  if (sockets[so_fd, S_TYPE] != TCP_LISTEN)
    RAISE_ERROR("Accept() called on non-listening socket\n");
  if (!closurep(cb) || !(ob=to_object(cb)))
    RAISE_ERROR("Bad argument 1 to Accept()\n");

  return (object) SOCKETD->socket_accept(so_ufd, cb);
}

// close the opened socket (user function)
public int Close() {
  int i;
  if (!so_check("Close")) return 0;
  return (int) SOCKETD->socket_close(so_ufd);
}

// get all connection information on this socket (user function)
public mixed* State() {
  mixed* data;
  if (!so_check("State")) return 0;
  map(sockets, (: if ($1 == $3) $4 = copy($2); return 0; :), so_fd, &data);
  data[S_TICKET] = copy(data[S_TICKET]);
  data[S_WAIT_DATA] = 0; // not visible for user
  return data;
}

//////
// functions used in the daemon only

// create a new socket (user function)
public object New(int type, mixed host, int port, closure cb, int opts) {
  int fd, *fds, ufd;
  object cb_o, so;

  debug("sockD::New(%O, %O, %O, %O, %O)", type, host, port, cb, opts);

  // TCP_ACCEPT could be generated internally only
  if (!intp(type) || type < 0 || type > (extern_call() ? 3 : 5))
    RAISE_ERROR("Bad argument 1 to New()\n");
  if (!(type & 4)) {
    if ((type & 1 && host) || (!(type & 1) && !stringp(host)))
      RAISE_ERROR("Bad argument 2 to New()\n");
  } else if (!objectp(host))
    RAISE_ERROR("Bad argument 2 to New()\n");
  if (!intp(port))
    RAISE_ERROR("Bad argument 3 to New()\n");
  if (!(type & 4) && (!closurep(cb) || !(cb_o=to_object(cb))))
    RAISE_ERROR("Bad argument 4 to New()\n");
  if (!intp(opts))
    RAISE_ERROR("Bad argument 5 to New()\n");

  if (extern_call() && !valid_socket(cb_o))
    RAISE_ERROR("privilege violation: New()\n");

  if (opts & SOCKET_MUDMODE) {
    opts |= SOCKET_BINARY;
    if (type == UDP) RAISE_ERROR("mudmode is tcp only\n");
  }

  // find empty fd
  fds = m_values(used_fd);
  for (fd = 0; fd < MAX_SOCKETS; fd++) {
    if (member(fds, fd) < 0) break;
  }
  if (fd >= MAX_SOCKETS) return 0; // no slot available

  if (type & 4) cb_o = sockets[port, S_OBJECT];

  while (1) {
    so  = (object) clone_object(program_name())->so_init(sockets, used_fd, fd);
    ufd = UNIQUE_ID(so);
    if (!member(used_fd, ufd)) break;
    // could happen but is very unlikely, just try again
    destruct(so);
  }

  used_fd[ufd] = fd;
  sockets[fd, S_SOCKOBJ]     = so;
  sockets[fd, S_TYPE]        = type;
  sockets[fd, S_TICKET]      = 0;
  sockets[fd, S_STATE]       = S_UNCONNECTED;
  sockets[fd, S_OPTS]        = opts;
  sockets[fd, S_HOST]        = host;
  sockets[fd, S_PORT]        = port;
  sockets[fd, S_CALLBACK]    = cb;
  sockets[fd, S_OBJECT]      = cb_o;
  sockets[fd, S_PENDING]     = 0;
  sockets[fd, S_OWNER]       = getuid(cb_o);

  if (type & 1)
    sockets[fd, S_LPORT] = port;
  if (type & 4)
    sockets[fd, S_LPORT] = sockets[sockets[fd, S_LISTEN_FD], S_PORT];
    
  switch (type) {
    case TCP_OPEN:
      lookup(host, lambda(({ 'msg }), ({ #'looked_up, 'msg, ufd })));
      break;

    case TCP_LISTEN:
      send_erq(ERQ_LISTEN, ({ port/256, port&255 }),
        lambda(({ 'msg }), ({ #'socket_cb, 'msg, ufd })));
      break;
      
    case UDP:
      send_erq(ERQ_OPEN_UDP, ({ port/256, port&255 }),
        lambda(({ 'msg }), ({ #'socket_cb, 'msg, ufd })));
  }

  return so;
}

// write data over a socket
int socket_write(int ufd, mixed msg, int* udp_addr) {
  int size, chunk, *ticket, *out, pos, is_udp, fd;
  string snipsel;
  mixed* data;

  debug("sockD::socket_write(%O, %O)", ufd, udp_addr);
  //debug("sockD::socket_write(%O, %O, %O)", ufd, msg, udp_addr);
  
  if (!intp(ufd)) RAISE_ERROR("Bad argument 1 to socket_write()\n");
  if (PO && PO != ME) {
    if (!m_contains(&fd, used_fd, ufd) || PO != sockets[fd, S_SOCKOBJ])
      RAISE_ERROR("privilege violation: socket_write()\n");
  } else {
    if (!m_contains(&fd, used_fd, ufd)) return 0; //vanished
  }
  if (!stringp(msg) && !pointerp(msg))
    RAISE_ERROR("Bad argument 2 to socket_write()\n");
  is_udp = sockets[fd, S_STATE] == S_UDP;
  if (is_udp && !pointerp(udp_addr) || !is_udp && udp_addr)
    RAISE_ERROR("Bad argument 3 to socket_write()\n");
  if ((!udp_addr && sockets[fd, S_STATE] != S_CONNECTED)
    || !sockets[fd, S_TICKET]) {
    if (sockets[fd, S_STATE] == S_UNCONNECTED) {
      data = sockets[fd, S_PENDING];
      if (!data) data = ({});
      data += ({ ({ msg, udp_addr }) });
      return 2;
    }
    else
      RAISE_ERROR("socket_write() on an unconnected socket\n");
  }

  if (!udp_addr) udp_addr = ({});
  ticket = sockets[fd, S_TICKET];
  chunk = WRITE_MAX - sizeof(ticket) - sizeof(udp_addr);

  if (pointerp(msg)) {
    size = sizeof(msg);
    for (; pos < size; pos += chunk) {
      sendbuf(ufd, udp_addr + msg[pos .. pos+chunk-1], pos);
      ++sockets[fd, S_TO_WRITE];
    }

  } else {
    size = strlen(msg);
    for (; pos < size; pos += chunk) {
      snipsel = msg[pos .. pos+chunk-1];
      sendbuf(ufd, udp_addr + to_array(snipsel)[0..strlen(snipsel)-1], pos);
      ++sockets[fd, S_TO_WRITE];
    }
  }
  if (!sockets[fd, S_WAITS]) flush(ufd);

  return 1;
}

// accepts a pending connection from a listened to port
object socket_accept(int ufd, closure cb) {
  object so, ob;
  int fd;

  debug("sockD::socket_accept(%O, %O)", ufd, cb);
  if (!intp(ufd)) RAISE_ERROR("Bad argument 1 to socket_accept()\n");
  if (!m_contains(&fd, used_fd, ufd) || PO != sockets[fd, S_SOCKOBJ])
      RAISE_ERROR("privilege violation: socket_accept()\n");
  if (sockets[fd, S_TYPE] != TCP_LISTEN)
    RAISE_ERROR("socket_accept() called on non-listening socket\n");
  if (!closurep(cb) || !(ob=to_object(cb)))
    RAISE_ERROR("Bad argument 2 to socket_accept()\n");

  so = New(TCP_ACCEPT, sockets[fd, S_SOCKOBJ], fd, cb, sockets[fd, S_OPTS]);
  if (!so) { // no more sockets left
    debug("too many connections, closing accepted connection");
    return 0;
  }

  send_erq(ERQ_ACCEPT, sockets[fd, S_TICKET],
    lambda(({ 'msg }), ({ #'accept_cb, 'msg, UNIQUE_ID(so) })));

  return so;
}

// write data via udp
int udp_write(int ufd, string host, int port, mixed umsg) {
  int fd;
  debug("sockD::udp_write(%O, %O)", ufd, host, port, umsg);
  if (!intp(ufd)) RAISE_ERROR("Bad argument 1 to udp_write()\n");
  if (!m_contains(&fd, used_fd, ufd) || PO != sockets[fd, S_SOCKOBJ])
      RAISE_ERROR("privilege violation: udp_write()\n");
  if (!stringp(host)) RAISE_ERROR("Bad argument 2 to udp_write()\n");
  if (!intp(port)) RAISE_ERROR("Bad argument 3 to udp_write()\n");
  if (!stringp(umsg) && !pointerp(umsg))
    RAISE_ERROR("Bad argument 4 to udp_write()\n");
  if (sockets[fd, S_TYPE] != UDP)
    RAISE_ERROR("udp_write() on a non-udp socket\n");

  lookup(host, lambda(({ 'msg }),
    ({ #'socket_write, ufd, umsg,
      ({ #'+, ({ #'[..], 'msg, 1, 4 }),
              ({ #'({, port / 256, port & 255 })
      })
    }) ));
  return 1;
}

// a socket is closed here. it can only be done from the socket object
int socket_close(int ufd) {
  int i, fd, fd2, ufd2;
  object so;

  debug("sockD::socket_close(%O)", ufd);
  if (!intp(ufd)) RAISE_ERROR("Bad argument 1 to socket_close()\n");
  if (PO && PO != ME) {
    if (!m_contains(&fd, used_fd, ufd) || PO != sockets[fd, S_SOCKOBJ])
      RAISE_ERROR("privilege violation: socket_close()\n");
  } else {
    if (!m_contains(&fd, used_fd, ufd)) return 0; //vanished
  }
  if (!member(sockets, fd)) return 0;

  // delete associated data
  i = sizeof(output);
  for (; i--;)
    if (output[i][OUT_UFD] == ufd)
      output[i..i] = ({});

  if (sockets[fd, S_TICKET] && sockets[fd, S_STATE] != S_CLOSING) {
    // never kill twice
    sockets[fd, S_STATE] = S_CLOSING;
    send_erq(ERQ_KILL, sockets[fd, S_TICKET] + ({ 0, 0, 0, 0 }),
      lambda(({ 'msg }), ({ #'socket_cb, 'msg, ufd })));
  }
  else {
    so = sockets[fd, S_SOCKOBJ];
    m_delete(sockets, fd);
    m_delete(used_fd, ufd);
    if (so) destruct(so);
  }
  return 1;
}

// looks up a DNS name
static void lookup(string host, closure cb) {
  int *msg;

  debug("sockD::lookup(%O, %O)", host, cb);
  msg = allocate(5);
  if (sscanf(host, "%d.%d.%d.%d", msg[1], msg[2], msg[3], msg[4]) == 4) {
    funcall(cb, msg);
    return;
  }

  send_erq(ERQ_LOOKUP, host, cb);
}

// the DNS answer for TCP is processed here
static void looked_up(int* msg, int ufd) {
  int fd;

  debug("sockD::looked_up(%O, %O)", msg, ufd);
  if (!m_contains(&fd, used_fd, ufd)) return; // vanished

  if (msg[0] != ERQ_OK) {
    callback(ufd, SOCKET_ERROR, note_err("unknown response", msg, fd));
    return;
  }

  sockets[fd, S_HOST] = sprintf("%d.%d.%d.%d",
    msg[1] & 255, msg[2] & 255, msg[3] & 255, msg[4] & 255);

  send_erq(ERQ_OPEN_TCP, msg[1..4] +
    ({ sockets[fd, S_PORT] / 256, sockets[fd, S_PORT] & 255 }),
    lambda(({ 'mess }), ({ #'socket_cb, 'mess, ufd })));
}

// most of data gets here from the erq (see write_cb)
static void socket_cb(int* msg, int ufd) {
  int state, opts, port, type, fd;
  string host;
  object so;

  debug("sockD::socket_cb(%O, %O)", msg, ufd);
  if (!m_contains(&fd, used_fd, ufd)) return; // should not happen
  msg   = map(msg, #'&, 255);
  type  = sockets[fd, S_TYPE];
  state = sockets[fd, S_STATE];
  opts  = sockets[fd, S_OPTS];

  switch (msg[0]) {

    case ERQ_OK: {
      debug("ERQ_OK");
      if (!sockets[fd, S_TICKET]) {
        switch (type) {
          case TCP_OPEN:   state = S_CONNECTED; break;
          case TCP_LISTEN: state = S_LISTEN;    break;
          case UDP:        state = S_UDP;       break;
          default:         debug("unexpected type", type); break;

          case TCP_ACCEPT:
            debug("accepting");
            host = sprintf("%d.%d.%d.%d", msg[1], msg[2], msg[3], msg[4]);
            port = msg[5] * 256 + msg[6];
            if (member(sockets, sockets[fd, S_LISTEN_FD]))
              sockets[sockets[fd, S_LISTEN_FD], S_PENDING] = 0;
            sockets[fd, S_TICKET] = msg[7..];
            sockets[fd, S_STATE]  = S_CONNECTED;
            sockets[fd, S_HOST]   = host;
            sockets[fd, S_PORT]   = port;
            callback(ufd, SOCKET_READY,
              ({ sockets[fd, S_SOCKOBJ], host, port }));
            return;
        }
        sockets[fd, S_TICKET] = msg[1..];
        sockets[fd, S_STATE]  = state;
        callback(ufd, SOCKET_READY, 0);
      }
      else {
        debug("socket has a ticket");
      }
      return;
    }

    case ERQ_STDOUT: {
      debug("ERQ_STDOUT");
      switch (state) {

        case S_LISTEN:
          debug("incoming connection (tcp)");
          if (sockets[fd, S_SOCKOBJ] && sockets[fd, S_OBJECT]) {
            sockets[fd, S_PENDING] = 1;
            callback(ufd, SOCKET_ACCEPT, 0);
          }
          else socket_close(ufd); // weird
          break;

        case S_CONNECTED:
          msg = msg[1..];
          rcvt += sizeof(msg);
          ++sockets[fd, S_RECEIVED];
          if (opts & SOCKET_BINARY)
            callback(ufd, SOCKET_READ, quote(msg));
          else
            callback(ufd, SOCKET_READ, to_ascii(msg));
          break;

        case S_UDP:
          debug("incoming connection (udp)");
          host = sprintf("%d.%d.%d.%d", msg[1], msg[2], msg[3], msg[4]);
          port = msg[5] * 256 + msg[6];
          msg = msg[7..];
          rcvt += sizeof(msg);
          if (opts & SOCKET_BINARY)
            callback(ufd, SOCKET_READ, ({ msg, host, port }));
          else
            callback(ufd, SOCKET_READ, ({ to_ascii(msg), host, port }));
          break;

        default:
          debug("unexpected state", state);
          break;
      }
      return;
    }

    case ERQ_EXITED:
      debug("ERQ_EXITED");
      so = sockets[fd, S_SOCKOBJ];
      sockets[fd, S_STATE] = S_CLOSING;
      callback(ufd, SOCKET_CLOSE, 0);
      m_delete(sockets, fd);
      m_delete(used_fd, ufd);
      if (so) destruct(so);
      return;

    case ERQ_E_ARGLENGTH:
      // last message ignored by xerq
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_ARGLENGTH", msg, fd));
      return;

    case ERQ_E_NSLOTS:
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_NSLOTS", msg, fd));
      break;

    case ERQ_E_WOULDBLOCK:
      callback(ufd, SOCKET_ERROR, note_err("ERQ_WOULDBLOCK", msg, fd));
      debug("ERQ_E_WOULDBLOCK socket cb"); 
      break;

    case ERQ_E_TICKET:
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_TICKET", msg, fd));
      break;

    case ERQ_E_ILLEGAL:
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_ILLEGAL", msg, fd));
      break;

    case ERQ_E_UNKNOWN:
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_UNKNOWN", msg, fd));
      break;

    default:
      callback(ufd, SOCKET_ERROR, note_err("unknown response", msg, fd));
      break;
  }

  // if we get here an error occured. the erq closed the socket already
  if (member(sockets, fd)) sockets[fd, S_STATE] = S_CLOSING;
  callback(ufd, SOCKET_CLOSE, 0);
  socket_close(ufd);
}

// answers from the erq upon writes get here to handle the
// data buffer (output) according to its answers
static void write_cb(int* msg, int ufd, int offset) {
  int fd, x;
  sending = 0;
  debug("sockD::write_cb(%O, %O)", msg, ufd);
  if (!m_contains(&fd, used_fd, ufd)) return; // vanished

  switch (msg[0]) {

    case ERQ_E_INCOMPLETE:
      debug("ERQ_E_INCOMPLETE");
      sockets[fd, S_WAITS] = S_WAITS_INCOM;
      msg = map(msg, #'&, 255);
      x = (((msg[1] << 8) + msg[2] << 8) + msg[3] << 8) + msg[4];
      callback(ufd, SOCKET_INCOMPLETE, ({ x, offset + x }) );
      return;

    case ERQ_OK:
      debug("ERQ_OK");
      sockets[fd, S_WAITS] = S_WAITS_NOT;
      sockets[fd, S_WAIT_DATA] = 0;
      ++sockets[fd, S_WRITTEN];
      if (sizeof(output)) flush(ufd);
      callback(ufd, SOCKET_WRITTEN,
        sockets[fd, S_TO_WRITE] == sockets[fd, S_WRITTEN]);
      return;

    case ERQ_E_WOULDBLOCK:
      // The xerq could not handle the sent data - this condition
      // could never happen. However, if it happens we have to resend
      // the last packet after some time. We just put it back in the
      // output queue and wait for another ERQ_OK.
      // If you have problems with sockets that seem to freeze after a
      // wouldblock (which indicates a bad erq implementation), you could
      // try and start a resend callout here.
      if (!sockets[fd, S_WAIT_DATA]) {
        // something went extremly wrong, there were several packets
        // sent without waiting for erq response. Original data is
        // lost already. Mind, this is a 'could never happen' case
        // within a 'could never happen' case ;o))
        //
        // Just do nothing.
      } else
        output = ({ sockets[fd, S_WAIT_DATA] }) + output; // back on stack
      sockets[fd, S_WAITS] = S_WAITS_BLOCK;
      sockets[fd, S_WAIT_DATA] = 0;
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_WOULDBLOCK", msg, fd));
      return;

    case ERQ_E_TICKET:
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_TICKET", msg, fd));
      break;

    case ERQ_E_PIPE:
      // should only happen with childs
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_PIPE", msg, fd));
      break;

    case ERQ_E_UNKNOWN:
      callback(ufd, SOCKET_ERROR, note_err("ERQ_E_UNKNOWN", msg, fd));
      break;

    default:
      callback(ufd, SOCKET_ERROR, note_err("unknown response", msg, fd));
      break;
  }

  // if we get here an error occured. the erq closed the socket already
  if (member(sockets, fd)) sockets[fd, S_STATE] = S_CLOSING;
  callback(ufd, SOCKET_CLOSE, 0);
  socket_close(ufd);
}

// special cb for accept
// the xerq sends ERQ_E_TICKET also if the socket has no pending
// connection :-/ because we cannot distinguish that to a wrong
// ticket we just assume here it must be the first.
static void accept_cb(int* msg, int ufd) {
  int fd;
  if (msg[0] != ERQ_E_TICKET) return socket_cb(msg, ufd);
  if (!m_contains(&fd, used_fd, ufd)) return; // vanished
  if (member(sockets, sockets[fd, S_LISTEN_FD]))
    sockets[sockets[fd, S_LISTEN_FD], S_PENDING] = 0;
  sockets[fd, S_STATE] = S_CLOSING;
  callback(ufd, SOCKET_NOPEND, 0);
  note_err("SOCKET_NOPEND", msg, fd);
  callback(ufd, SOCKET_CLOSE, 0);
  socket_close(ufd);
}

// the function calls the user object on new data or errors
static void callback(int ufd, int action, mixed args) {
  int fd;
  mixed data, item;

  debug("sockD::callback(%O, %O, %O)", fd, action, args);
  if (!m_contains(&fd, used_fd, ufd) || !member(sockets, fd))
    return; // vanished

  // automatic delayed write
  data = sockets[fd, S_PENDING];
  if (pointerp(data) && action == SOCKET_READY) {
    foreach (item : data) socket_write(ufd, item[0], item[1]);
    sockets[fd, S_PENDING] = sizeof(item);
  }
  else if (intp(data) && data > 0 && action == SOCKET_WRITTEN) {
    sockets[fd, S_PENDING] = data - 1;
  }
  if (!sockets[fd, S_SOCKOBJ] || !sockets[fd, S_OBJECT]) {
    if (!data && sockets[fd, S_STATE] != S_CLOSING) socket_close(ufd);
    return;
  }

  if (action == SOCKET_READ && sockets[fd, S_OPTS] & SOCKET_MUDMODE) {
    args = to_array(args); // unquote args
    sockets[fd, S_SOCKOBJ]->so_get_mudmode(args);
    return;
  }
  apply(sockets[fd, S_CALLBACK], sockets[fd, S_SOCKOBJ], action, args);
}


// send a message trough the network (buffered)
static void sendbuf(int ufd, int* data, int offset) {
  int fd;
  // debug("sockD::sendbuf(%O, %O)", ufd, data);
  debug("sockD::sendbuf(%O)", ufd);
  if (!m_contains(&fd, used_fd, ufd)) return; // vanished
  output += ({ ({ ufd, data, offset }) });
}

// Flush the message buffer for socket ufd.
//
// Xerq works as follows:
//   1 Erq gets new data
//   2 Erq tries to send data packet
//   3 Erq returns OK if successfull -=> continue at 1
//   4 Erq returns INCOMPLETE. Rest of packet is buffered and sent
//     when possible.
//   5 Erq returns OK if all buffered data could be written (*new*)
//   6 Continue at 1
//
//   There is only one packet buffered inside the erq for every socket.
//   If you try to send a packet while one is in the queue already, the erq
//   returns WOULDBLOCK and the new packet is discarded!
static void flush(int ufd) {
  int ret, i, n, fd;
  int* data;
    
  debug("sockD::flush(%O)", ufd);
  // debug(output);

  n = sizeof(output);
  if (!n) {
    debug("nothing to send");
    return;
  }

  // remove buffered data if socket is gone
  if (!m_contains(&fd, used_fd, ufd) || !member(sockets, fd)) {
    for (;n--;) {
      if (output[n][OUT_UFD] != ufd) continue;
      debug("discarding %O -> %O", ufd, output[0][OUT_DATA]);
      output[n..n] = ({});
    }
    return;
  }

  if (sockets[fd, S_WAITS]) {
    debug("socket busy");
    return;
  }

  // find socket's data
  for (; i < n; i++) {
    if (output[i][OUT_UFD] == ufd) break;
  }
  if (i == n) {
    debug("no data pending");
    return;
  }

  sent += sizeof(output[i][OUT_DATA]);
  data = sockets[fd, S_TICKET] + output[i][OUT_DATA];
  debug("sending");
  ret = send_erq(ERQ_SEND, data,
    lambda(({ 'msg }), ({ #'write_cb, 'msg, ufd, output[i][OUT_SIZE] })));
  if (!ret) {
    debug("got send_erg() error, retry in 1 second");
    call_out("flush", 1, ufd);
    return;
  }
  sockets[fd, S_WAITS] = S_WAITS_ERQ;
  sockets[fd, S_WAIT_DATA] = output[i..i];
  output[i..i] = ({});
}

// mudmode is an invention of MudOS. We have to simulate it here in
// some way or another. Its quite similar to ldmuds save_value() but
// does not change "\n" to "\\n" etc.
static string to_mudmode(mixed data) {
  int num, i, len;

  // use save_value(data)[5..<2] with pre 3.2.10 drivers
  data = save_value(data, 0)[5..<2];
  len = strlen(data);

  for (i=0; i<len; i++) {
    if (data[i] == '\\') {
      num++;
      continue;
    }
    if (!num) continue;
    if (num & 1) {
      switch (data[i]) {
        case 'r': data[i-1..i] = "\r"; len--; i--; break;
        case 'f': data[i-1..i] = "\f"; len--; i--; break;
        case 'v': data[i-1..i] = "\v"; len--; i--; break;
        case 'n': data[i-1..i] = "\n"; len--; i--; break;
        case 't': data[i-1..i] = "\t"; len--; i--; break;
        case 'b': data[i-1..i] = "\b"; len--; i--; break;
        case 'a': data[i-1..i] = "\a"; len--; i--; break;
      }
    }
    num = 0;
  }

  return data;
}

mixed from_mudmode(string data) {
  return restore_value("#0:0\n"+data+"\n");
}

// this is inside the socket object to collect full mudmode packets
void so_get_mudmode(int* data) {
  mixed mix;
  string err;
  int i;
  if (object_name(PO) != SOCKETD)
    RAISE_ERROR("privilege violation: so_get_mudmode()\n");
  while (sizeof(data)) {
    if (!so_bufs || !intp(so_bufs)) {
      if (!so_bufs) so_bufs = ({});
      i = min(sizeof(data), 4);
      i -= sizeof(so_bufs);
      so_bufs += data[0..i-1];
      data = data[i..];
      if (sizeof(so_bufs) == 4) {
        so_bufs = (((((so_bufs[0] & 0xff)) << 8
                    | (so_bufs[1] & 0xff)) << 8
                    | (so_bufs[2] & 0xff)) << 8
                    | (so_bufs[3] & 0xff));
        debug("mudmode packet size ", so_bufs);
        // String length is critical here, one string has to hold the
        // complete packet. It seems that modmode is working with rather
        // small packets (< 1kb) which fit nicely in a chararray, but
        // we dont want do depend on that (and use strings).
      }
      else break;
      if (so_bufs == 0)
        debug("strange size, isn't it?");
    }
    if (!so_buf) so_buf = "";
    i = sizeof(data);
    if (i >= so_bufs) {
      if (data[so_bufs-1] == 0)
        so_buf += to_string(data[0..so_bufs-2]);
      else
        so_buf += to_string(data[0..so_bufs-1]);
      data = data[so_bufs..];
      err = catch(mix = from_mudmode(so_buf));
      so_buf = "";
      so_bufs = 0;
      if (err) { // invalid packet
        funcall(sockets[so_fd, S_CALLBACK], ME, SOCKET_ERROR,
          "mudmode protocoll violation");
        SOCKETD->socket_close(so_ufd);
        return;
      }
      funcall(sockets[so_fd, S_CALLBACK], ME, SOCKET_READ, mix);
    } else {
      so_buf += to_string(data);
      so_bufs -= i;
      debug("... still data pending");
      break; // no data left
    }
  }
}

// different handling of removal for master and clones
int remove() {
  if (clonep()) {
    SOCKETD->socket_close(so_ufd);
    destruct(ME);
    return 1;
  }

  if (object_name(PO) != __MASTER_OBJECT__ && !SPECIAL_PREVILEGE) {
    printf("DONT DO THAT!\n");
    return 0;
  }

  if (first_inventory()) {
    debug("*** going to remove socketd... (3 sec)");
    map(m_indices(used_fd), #'socket_close);
    call_out("do_remove", 3);
  } else destruct(ME);
  return 1;
}

static void do_remove() {
  debug("*** done");
  destruct(ME);
}

//////
// internal helper functions

// which objects may create sockets
static int valid_socket(object ob) {
  string on, v;

  on = object_name(ob);
  if (!valids) valids = VALID_SOCKETS;
  foreach (v : valids)
    if (!strstr(on, v)) return 1;
  return 0;
}

// convert message to ascii
// values are signed -128 .. 127
static string to_ascii(int* msg) {
  msg -= ({ 0 });
  return to_string(msg);
}

// some erq-error occured, log it
static mixed* note_err(string str, int* msg, int fd) {
  string who, extra, e2;

  if (sockets[fd, S_OBJECT]) who = to_string(sockets[fd, S_OBJECT]);
  else who = "destructed object";

  if (msg[0] == ERQ_E_UNKNOWN) {
    extra = ({
          // all errno-codes
          // use list according to your system (this is linux)
          "??? not used",
          "EPERM Operation not permitted",
          "ENOENT No such file or directory",
          "ESRCH No such process",
          "EINTR Interrupted system call",
          "EIO I/O error",
          "ENXIO No such device or address",
          "E2BIG Arg list too long",
          "ENOEXEC Exec format error",
          "EBADF Bad file number",
          "ECHILD No child processes", // 10
          "EAGAIN Try again",
          "ENOMEM Out of memory",
          "EACCES Permission denied",
          "EFAULT Bad address",
          "ENOTBLK Block device required",
          "EBUSY Device or resource busy",
          "EEXIST File exists",
          "EXDEV Cross-device link",
          "ENODEV No such device",
          "ENOTDIR Not a directory", // 20
          "EISDIR Is a directory",
          "EINVAL Invalid argument",
          "ENFILE File table overflow",
          "EMFILE Too many open files",
          "ENOTTY Not a typewriter",
          "ETXTBSY Text file busy",
          "EFBIG File too large",
          "ENOSPC No space left on device",
          "ESPIPE Illegal seek",
          "EROFS Read-only file system", // 30
          "EMLINK Too many links",
          "EPIPE Broken pipe",
          "EDOM Math argument out of domain of func",
          "ERANGE Math result not representable",
          "EDEADLK Resource deadlock would occur",
          "ENAMETOOLONG File name too long",
          "ENOLCK No record locks available",
          "ENOSYS Function not implemented",
          "ENOTEMPTY Directory not empty",
          "ELOOP Too many symbolic links encountered", // 40
          "??? not used",
          "ENOMSG No message of desired type",
          "EIDRM Identifier removed",
          "ECHRNG Channel number out of range",
          "EL2NSYNC Level 2 not synchronized",
          "EL3HLT Level 3 halted",
          "EL3RST Level 3 reset",
          "ELNRNG Link number out of range",
          "EUNATCH Protocol driver not attached",
          "ENOCSI No CSI structure available", // 50
          "EL2HLT Level 2 halted",
          "EBADE Invalid exchange",
          "EBADR Invalid request descriptor",
          "EXFULL Exchange full",
          "ENOANO No anode",
          "EBADRQC Invalid request code",
          "EBADSLT Invalid slot",
          "??? not used",
          "EBFONT Bad font file format",
          "ENOSTR Device not a stream", // 60
          "ENODATA No data available",
          "ETIME Timer expired",
          "ENOSR Out of streams resources",
          "ENONET Machine is not on the network",
          "ENOPKG Package not installed",
          "EREMOTE Object is remote",
          "ENOLINK Link has been severed",
          "EADV Advertise error",
          "ESRMNT Srmount error",
          "ECOMM Communication error on send", // 70
          "EPROTO Protocol error",
          "EMULTIHOP Multihop attempted",
          "EDOTDOT RFS specific error",
          "EBADMSG Not a data message",
          "EOVERFLOW Value too large for defined data type",
          "ENOTUNIQ Name not unique on network",
          "EBADFD File descriptor in bad state",
          "EREMCHG Remote address changed",
          "ELIBACC Can not access a needed shared library",
          "ELIBBAD Accessing a corrupted shared library", // 80
          "ELIBSCN .lib section in a.out corrupted",
          "ELIBMAX Attempting to link in too many shared libraries",
          "ELIBEXEC Cannot exec a shared library directly",
          "EILSEQ Illegal byte sequence",
          "ERESTART Interrupted system call should be restarted",
          "ESTRPIPE Streams pipe error",
          "EUSERS Too many users",
          "ENOTSOCK Socket operation on non-socket",
          "EDESTADDRREQ Destination address required",
          "EMSGSIZE Message too long", // 90
          "EPROTOTYPE Protocol wrong type for socket",
          "ENOPROTOOPT Protocol not available",
          "EPROTONOSUPPORT Protocol not supported",
          "ESOCKTNOSUPPORT Socket type not supported",
          "EOPNOTSUPP Operation not supported on transport endpoint",
          "EPFNOSUPPORT Protocol family not supported",
          "EAFNOSUPPORT Address family not supported by protocol",
          "EADDRINUSE Address already in use",
          "EADDRNOTAVAIL Cannot assign requested address",
          "ENETDOWN Network is down", // 100
          "ENETUNREACH Network is unreachable",
          "ENETRESET Network dropped connection because of reset",
          "ECONNABORTED Software caused connection abort",
          "ECONNRESET Connection reset by peer",
          "ENOBUFS No buffer space available",
          "EISCONN Transport endpoint is already connected",
          "ENOTCONN Transport endpoint is not connected",
          "ESHUTDOWN Cannot send after transport endpoint shutdown",
          "ETOOMANYREFS Too many references: cannot splice",
          "ETIMEDOUT Connection timed out", // 110
          "ECONNREFUSED Connection refused",
          "EHOSTDOWN Host is down",
          "EHOSTUNREACH No route to host",
          "EALREADY Operation already in progress",
          "EINPROGRESS Operation now in progress",
          "ESTALE Stale NFS file handle",
          "EUCLEAN Structure needs cleaning",
          "ENOTNAM Not a XENIX named type file",
          "ENAVAIL No XENIX semaphores available",
          "EISNAM Is a named type file", // 120
          "EREMOTEIO Remote I/O error",
          "EDQUOT Quota exceeded",
          "ENOMEDIUM No medium found",
          "EMEDIUMTYPE Wrong medium type", })[msg[1]];

    debug(str + " (%O) %O (%O) for %O", msg[0], e2, msg[1], who);
  }
  else debug(str + " (%O) for %O", msg[0], who);


  log_file(SOCKETLOG, ctime()[4..19] + str + " (" + msg[0] + ") " + extra||"" +
    "for " + who + "\n");

  return ({ str, msg, extra });
}

object _debug(int on) {
  if (!SPECIAL_PREVILEGE) return 0;
  if (!on) return debugger = 0;
  return debugger = this_interactive();
}

static void debug(varargs mixed* xx) {
  string txt;
  mixed y, x;
  int i;

  if (!debugger) return;

  x = xx + ({}); // strange behavior in 3.3's foreach
  if (stringp(x[0])) {
    txt = x[0];
    x = x[1..];
  }
  else txt = "";

  foreach (y : x) {
    i = strstr(txt, "%O");
    if (i >= 0) txt[i..i+1] = MIXED2STR(y);
    else txt += " " + MIXED2STR(y);
  }

  tell_object(debugger, txt + "\n");
}

// you could send a wizcommand here
public int netstat(string arg) {
  string proto, mode, state, h_p;
  object so;
  int fd;

  if (stringp(arg)) {
    arg = trim(arg);
    fd = to_int(arg);
    if (!member(sockets, fd)) {
      printf("No socket with file descriptor '" + arg + "'.\n");
      return 1;
    }
    printf("Full information on socket fd " + fd + "\n");
    printf("  object:   " + to_string(sockets[fd, S_SOCKOBJ]) + "\n");
    printf("  type:     " + ({ "TCP_OPEN", "TCP_LISTEN", "?", "UDP", "?",
      "TCP_ACCEPT"})[sockets[fd, S_TYPE]] + "\n");
    printf("  ticket:   " +
      implode(map(sockets[fd, S_TICKET], #'to_string), ",") + "\n");
    printf("  state:    "+ ({ "UNCONNECTED", "CONNECTED", "LISTEN", "LISTEN",
      "CLOSING", "ACCEPT" })[sockets[fd, S_STATE]] + "\n");
    printf("  option:   " + ({ "ASCII", "BINARY", "?",
      "MUDMODE" })[sockets[fd, S_OPTS]] + "\n");
    printf("  callback: %O\n", sockets[fd, S_CALLBACK]);
    printf("  creater:  " + to_string(sockets[fd, S_OBJECT]) + "\n");
    printf("  created:  " + ctime(object_time(sockets[fd, S_SOCKOBJ])) + "\n");
    printf("  owner id: " + sockets[fd, S_OWNER] + "\n");
    printf("  packets to write: " + sockets[fd, S_TO_WRITE] + "\n");
    printf("  packets written:  " + sockets[fd, S_WRITTEN] +
      " (" + (sockets[fd, S_TO_WRITE] - sockets[fd, S_WRITTEN]) +
      " remaining)\n");
    printf("  packets received: " + sockets[fd, S_RECEIVED] + "\n");
    printf("  if applicable:\n");
    printf("    host:        " + sockets[fd, S_HOST] + "\n");
    printf("    port:        " + sockets[fd, S_PORT] + "\n");
    printf("    listen fd:   " + sockets[fd, S_LISTEN_FD] + "\n");
    printf("    listen port: " + sockets[fd, S_LPORT] + "\n");
    return 1;
  }

  printf("fd  proto      mode    address               local   state     "
    "pend  owner\n");
  foreach (fd : sort_array(m_indices(sockets),#'>)) {
    if (!sockets[fd, S_OBJECT]) {
      m_delete(sockets, fd);
      continue;
    }
    proto = ({ "TCP_OPEN", "TCP_LISTEN", 0, "UDP", 0,
      "TCP_ACCEPT"})[sockets[fd, S_TYPE]];
    if (sockets[fd, S_OPTS] == 0) mode = "ASCII";
    else if (sockets[fd, S_OPTS] & 2) mode = "MUDMODE";
    else mode = "BINARY";

    state = ({ "UNCONNECTED", "CONNECTED", "LISTEN", "LISTEN", "CLOSING",
      "ACCEPT" })[sockets[fd, S_STATE]];
    if (sockets[fd, S_TYPE] == TCP_ACCEPT
      && sockets[fd, S_STATE] != S_CONNECTED) h_p = "(unknown)";
    else {
      h_p = sockets[fd, S_HOST];
      if (h_p) h_p += " " + sockets[fd, S_PORT];
      else h_p = "---";
    }

    printf("%2d  %-10s %-7s %-21s %-7s %-11s %c %-10s\n",
      fd, proto, mode, h_p,
      sockets[fd, S_LPORT] ? "("+sockets[fd, S_LPORT]+")" : "",
      state, sockets[fd, S_PENDING] ? '*' : ' ',
      sockets[fd, S_OWNER] + regreplace(object_name(sockets[fd, S_OBJECT]),
      ".*/([^/#]+).*", ":\\1", 0));
  }

  if (!sizeof(sockets)) printf(" <--  empty list  -->\n");
  printf("%d socket objects for %d sockets\n", sizeof(all_inventory()),
    sizeof(sockets));
  printf("%d bytes send, %d bytes received\n", sent, rcvt);
  return 1;
}
