//
// Wunderland Mudlib
//
// secure/telnetneg.c  --  Manage telnet negotations
//
// This telnet implementation is 'unconditionally compliant'
// to RFC 1143 and so follows all rules of the telnet protocol RFC 854.
// The driver itself is not quite compliant.
//
// Find the newest version of this file at
//   http://wl.mud.de/mud/doc/wwwstd/standardobjekte.html
// 
// $Log: telnetneg.c,v $
// Revision 1.25  2003/07/07 07:08:52  Fiona
// LM_SLC more save against protocol violations
//

#pragma strict_types

#include <input_to.h>            // INPUT_*
#include <config.h>              // ROOTID
#include <properties.h>          // P_* macros
#define NEED_PROTOTYPES
#include <thing/properties.h>    // SetProp() proto
#define NEED_PRIVATE_PROTOTYPES
#include <telnetneg.h>

#define GET_ALL_TTYPES

// ******************** Telnet State Machine ********************
//
// The following code is the implementation of the telnet state
// machine as suggested with RFC 1143.
//
// The machine is used with three functions:
//   set_telnet()    requests a change in the telnet state 
//   set_callback()  sets our prefered telnet states and a callback
//                   function which is called on telnet state changes
//   query_telnet()  query state and sb info
//
// The driver communicates with the engine through the H_TELNET_NEG
// and got_telnet().
// 
// Do this in logon() to turn IAC quoting on:
//   set_connection_charset(({255})*32, 1);
//
// Do this in logon() to start telnet negotiation:
//   set_telnet(WILL, TELOPT_ECHO);
//   set_telnet(WONT, TELOPT_ECHO);
// alternatively you could do the following, which is not as robust
//   set_telnet(WILL, TELOPT_EOR);

nosave mapping ts; // Complete telnet negotation state

// Set preferences and callbacks
// 
// r_a_cb is the preference for the state on the remote side. It could be
//   DO, DONT or a callback closure which decides if we get a request.
// l_a_cb is the same for the local side.
// change_cb is called if the state changes to NO or YES (real change)
// sb_cb is called with incoming subnegotiations.
private void set_callback(int opt, mixed r_a_cb, mixed l_a_cb,
    closure change_cb, closure sb_cb) {
  if (r_a_cb == DO) r_a_cb = 1;
  else if (r_a_cb == DONT) r_a_cb = 0;
  if (l_a_cb == WILL) l_a_cb = 1;
  else if (l_a_cb == WONT) l_a_cb = 0;

  ts[opt, TS_R_AGREE] = r_a_cb;
  ts[opt, TS_L_AGREE] = l_a_cb;
  ts[opt, TS_CB] = change_cb;
  ts[opt, TS_SBCB] = sb_cb;
}

// Try to change an option
// True is returned if the option indeed changes
static int set_telnet(int command, int option) {
  int state, ok;

  state = ts[option, TS_STATE];
  switch (command) {
    case DO:
      if (Q_REMOTER(state) == REJECTED) break;
      switch (Q_REMOTE(state)) {
        case YES:
          break; // ignore, it's already enabled
        case NO:
          state = S_REMOTE(state, WANT_YES);
          send(({ IAC, DO, option }));
          ok = 1;
          break;
        case WANT_NO:
          if (Q_REMOTEQ(state) == Q_EMPTY) {
            state = S_REMOTEQ(state, Q_OPPOSITE);
            ok = 1;
          } else {
	    // ignore, request sent already
          }
          break;
        case WANT_YES:
          if (Q_REMOTEQ(state) == Q_EMPTY) {
	    // ignore, request sent already
          } else {
            state = S_REMOTEQ(state, Q_EMPTY);
            ok = 1;
          }
          break;
      }
      break;

    case DONT:
      switch (Q_REMOTE(state)) {
        case NO:
          break; // ignore, it's already disabled
        case YES:
          state = S_REMOTE(state, WANT_NO);
          send(({ IAC, DONT, option }));
          ok = 1;
          break;
        case WANT_NO:
          if (Q_REMOTEQ(state) == Q_EMPTY) {
	    // ignore, request sent already
          } else {
            state = S_REMOTEQ(state, Q_EMPTY);
            ok = 1;
          }
          break;
        case WANT_YES:
          if (Q_REMOTEQ(state) == Q_EMPTY) {
            state = S_REMOTEQ(state, Q_OPPOSITE);
            ok = 1;
          } else {
	    // ignore, request sent already
          }
          break; 
      }
      break;

    case WILL:
      if (Q_LOCALR(state) == REJECTED) break;
      switch (Q_LOCAL(state)) {
        case NO:
          state = S_LOCAL(state, WANT_YES);
          send(({ IAC, WILL, option }));
          ok = 1;
          break;
        case YES:
          break; // ignore, it's already enabled
        case WANT_NO:
          if (Q_LOCALQ(state) == Q_EMPTY) {
            state = S_LOCALQ(state, Q_OPPOSITE);
            ok = 1;
          } else {
	    // ignore, request sent already
          }
          break;
        case WANT_YES:
          if (Q_LOCALQ(state) == Q_EMPTY) {
	    // ignore, request sent already
          } else {
            state = S_LOCALQ(state, Q_EMPTY);
            ok = 1;
          }
          break;
      }
      break;

    case WONT:
      switch (Q_LOCAL(state)) {
        case NO:
          break; // ignore, it's already disabled
        case YES:
          state = S_LOCAL(state, WANT_NO);
          send(({ IAC, WONT, option }));
          ok = 1;
          break;
        case WANT_NO:
          if (Q_LOCALQ(state) == Q_EMPTY) {
	    // ignore, request sent already
          } else {
            state = S_LOCALQ(state, Q_EMPTY);
            ok = 1;
          }
          break;
        case WANT_YES:
          if (Q_LOCALQ(state) == Q_EMPTY) {
            state = S_LOCALQ(state, Q_OPPOSITE);
            ok = 1;
          } else {
	    // ignore, request sent already
          }
          break; 
      }
      break;
  }
  ts[option, TS_STATE] = state;
  return ok;
}

// Request information on an option. sb has to be called by reference.
public int query_telnet(int option, mixed sb) {
  if (!intp(option) || option < 0 || !member(ts, option)) {
    sb = 0;
    return 0;
  }
  sb = copy(ts[option, TS_SB]);
  return ts[option, TS_STATE];
}

// Got telnet negotations from the driver
void got_telnet(int command, int option, int *optargs) {
  int state, i, j, c, l, *xx, full;
  mixed agree;
  string log;

  // Set the hook as follows.
  //   set_driver_hook(H_TELNET_NEG, "got_telnet");

  // Make shure it's a driver apply. Hope this never breaks ;)
  if (caller_stack_depth()) return;

  
  log = ts[TS_EXTRA, TSE_LOG];
  if (strlen(log) > 4000) log = "..." + log[<3800..];
  log += "got  " + telnet_to_text(command, option, optargs) + "\n";
  ts[TS_EXTRA, TSE_LOG] = log;

  state = ts[option, TS_STATE];
  switch (command) {
    case WILL:
      switch (Q_REMOTE(state)) {
        case NO:
          agree = ts[option, TS_R_AGREE];
          if (closurep(agree)) agree = funcall(agree, command, option);
          if (agree) {
            state = S_REMOTE(state, YES);
            ts[option, TS_STATE] = state;
            send(({ IAC, DO, option }));
            agree = ts[option, TS_CB];
            if (closurep(agree)) funcall(agree, command, option);
          } else {
            send(({ IAC, DONT, option }));
          }
          break;
        case YES:
          break; // ignore
        case WANT_NO:
          if (Q_REMOTEQ(state) == Q_EMPTY) {
            tel_error("DONT answered by WILL");
            state = S_REMOTE(state, NO);
          } else {
            tel_error("DONT answered by WILL");
            state = S_REMOTE(state, YES);
            state = S_REMOTEQ(state, Q_EMPTY);
          }
          ts[option, TS_STATE] = state;
          break;
        case WANT_YES:
          state = S_REMOTE(state, YES);
          ts[option, TS_STATE] = state;
          agree = ts[option, TS_CB];
          if (closurep(agree)) funcall(agree, command, option);
          if (Q_REMOTEQ(state) != Q_EMPTY) {
            state = S_REMOTE(state, WANT_NO);
            state = S_REMOTEQ(state, Q_EMPTY);
            ts[option, TS_STATE] = state;
            send(({ IAC, DONT, option }));
          }
          break;
      }
      break;

    case WONT:
      switch (Q_REMOTE(state)) {
        case NO:
          break; // ignore
        case YES:
          state = S_REMOTE(state, NO);
          ts[option, TS_STATE] = state;
          send(({ IAC, DONT, option }));
          agree = ts[option, TS_CB];
          if (closurep(agree)) agree = funcall(agree, command, option);
          break;
        case WANT_NO:
          state = S_REMOTE(state, NO);
          ts[option, TS_STATE] = state;
          agree = ts[option, TS_CB];
          if (closurep(agree)) funcall(agree, command, option);
          if (Q_REMOTEQ(state) != Q_EMPTY) {
            state = S_REMOTE(state, WANT_YES);
            state = S_REMOTEQ(state, Q_EMPTY);
            ts[option, TS_STATE] = state;
            send(({ IAC, DO, option }));
          }
          break;
        case WANT_YES:
          if (option == TELOPT_TM) {
            // *sigh* TM is not as all other options are
            agree = ts[option, TS_CB];
            if (closurep(agree)) funcall(agree, command, option);
          }
          else state = S_REMOTER(state, REJECTED);
          state = S_REMOTE(state, NO);
          state = S_REMOTEQ(state, Q_EMPTY);
          ts[option, TS_STATE] = state;
          break;
      }
      break;

    case DO:
      switch (Q_LOCAL(state)) {
        case NO:
          agree = ts[option, TS_L_AGREE];
          if (closurep(agree)) agree = funcall(agree, command, option);
          if (agree) {
            state = S_LOCAL(state, YES);
            ts[option, TS_STATE] = state;
            send(({ IAC, WILL, option }));
            agree = ts[option, TS_CB];
            if (closurep(agree)) funcall(agree, command, option);
          } else {
            send(({ IAC, WONT, option }));
          }
          break;
        case YES:
          break; // ignore
        case WANT_NO:
          if (Q_LOCALQ(state) == Q_EMPTY) {
            tel_error("WONT answered by DO");
            state = S_LOCAL(state, NO);
          } else {
            tel_error("WONT answered by DO");
            state = S_LOCAL(state, YES);
            state = S_LOCALQ(state, Q_EMPTY);
          }
          ts[option, TS_STATE] = state;
          break;
        case WANT_YES:
          state = S_LOCAL(state, YES);
          ts[option, TS_STATE] = state;
          agree = ts[option, TS_CB];
          if (closurep(agree)) funcall(agree, command, option);
          if (Q_LOCALQ(state) != Q_EMPTY) {
            state = S_LOCAL(state, WANT_NO);
            state = S_LOCALQ(state, Q_EMPTY);
            ts[option, TS_STATE] = state;
            send(({ IAC, WONT, option }));
          }
          break;
      }
      break;

    case DONT:
      switch (Q_LOCAL(state)) {
        case NO:
          break; // ignore
        case YES:
          state = S_LOCAL(state, NO);
          ts[option, TS_STATE] = state;
          send(({ IAC, WONT, option }));
          agree = ts[option, TS_CB];
          if (closurep(agree)) agree = funcall(agree, command, option);
          break;
        case WANT_NO:
          state = S_LOCAL(state, NO);
          ts[option, TS_STATE] = state;
          agree = ts[option, TS_CB];
          if (closurep(agree)) funcall(agree, command, option);
          if (Q_LOCALQ(state) != Q_EMPTY) {
            state = S_LOCAL(state, WANT_YES);
            state = S_LOCALQ(state, Q_EMPTY);
            ts[option, TS_STATE] = state;
            send(({ IAC, WILL, option }));
          }
          break;
        case WANT_YES:
          state = S_LOCAL(state, NO);
          state = S_LOCALQ(state, Q_EMPTY);
          state = S_LOCALR(state, REJECTED);
          ts[option, TS_STATE] = state;
          break;
      }
      break;

  case SB:
    agree = ts[option, TS_SBCB];
    if (closurep(agree)) funcall(agree, command, option, optargs);
    break;
      
  }

  // Do full negotation only if client can telnetneg, means we got some
  // telnetneg message from it. If we never got one we assume it can't
  // do it. The negotiation is triggered by one single telnetneg request
  // in the login process.
  if (!ts[TS_EXTRA, TSE_TELNETNEG]) start_telnetneg();
}

// This function is used to transfer the telnet state engine's state
// from one object to an other, used for player renew and so on
//
// The transfer has to be done before the exec(E), so that both objects
// work with the same copy of ts
mapping transfer_ts(mapping old_ts) {
  int opt;
  if (!previous_object() || getuid(previous_object()) != ROOTID) return 0;
  if (!old_ts) return ts;

  // use callbacks of THIS object
  foreach (opt : ts) {
    if (opt < 0) continue;
    old_ts[opt, TS_R_AGREE] = ts[opt, TS_R_AGREE];
    old_ts[opt, TS_L_AGREE] = ts[opt, TS_L_AGREE];
    old_ts[opt, TS_CB]      = ts[opt, TS_CB];
    old_ts[opt, TS_SBCB]    = ts[opt, TS_SBCB];
  }

  ts = old_ts;

  return 0;
}

// All telnet negotations are sent through this function
private int send(int* x) {
  string log;
  int *y, i, j;

  if (x[1] != SB) y = x[3..];
  else {
    y = x[3..<3];
    j = sizeof(y) - 1;
    for (i = 0; i < j; ++i) { // undo 0xff quoting
      if (y[i] == IAC && y[i+1] == 0xff) {
        y[i..i+1] = ({ 0xff });
        --j;
      }
    } 
  }

  log = ts[TS_EXTRA, TSE_LOG];
  if (strlen(log) > 4000) log = "..." + log[<3800..];
  log += "sent " + telnet_to_text(x[1], x[2], y) +"\n";
  ts[TS_EXTRA, TSE_LOG] = log;

  return efun::binary_message(x);
}

// TODO: it's not quite complete
private string telnet_to_text(int command, int option, int* args) {
  string d_txt;
  int i, j;

  d_txt = TELCMD2STRING(IAC) + " " +
    TELCMD2STRING(command) + " " + TELOPT2STRING(option);
  if (args && sizeof(args)) {
    if (command == SB && option == TELOPT_LINEMODE) {
      switch (args[0]) {
        case LM_MODE:
          if (sizeof(args) > 1) {
            d_txt += " MODE" +
              (args[1] & MODE_EDIT ? " EDIT" : " NOEDIT") +
              (args[1] & MODE_TRAPSIG ? " TRAPSIG" : "") +
              (args[1] & MODE_SOFT_TAB ? " SOFT_TAB" : "") +
              (args[1] & MODE_LIT_ECHO ? " LIT_ECHO" : "") +
              (args[1] & MODE_ACK ? " ACK" : "");
            if (sizeof(args) == 2) return d_txt;
            args = args[2..];
          }
          break;

        case WILL: case WONT: case DO: case DONT:
          d_txt += " " + TELCMD2STRING(args[0]);
          args = args[1..];
          if (sizeof(args) > 0 && args[0] == LM_FORWARDMASK) {
            d_txt += " FORWARDMASK";
            args = args[1..];
          }
          break;

        case LM_SLC:
          j = sizeof(args) - 2;
          d_txt += " SLC";
          for (i = 1; i < j; i += 3) {
            d_txt += "\n          ";
            d_txt += sprintf("%-6s %-23s %02x",
              SLC2STRING(args[i]),
               SLC_FLAGNAME[args[i+1] & SLC_LEVELBITS] +
               (args[i+1] & SLC_FLUSHOUT ? " FOUT" : "") +
               (args[i+1] & SLC_FLUSHIN  ? " FIN"  : "") +
               (args[i+1] & SLC_ACK      ? " ACK"      : ""),
               args[i+2]);
          }
          if (i > j) return d_txt;
          args = args[i-3..]; // dump rest (is error)
          break;
          
      }

    }

    if (command == SB && option != TELOPT_NAWS && option != TELOPT_LINEMODE) {
      d_txt += " " + TELQUAL2STRING(args[0]);
      if (sizeof(args) > 1) d_txt += " (" + 
        implode(map(args[1..], (: sprintf("%02x", $1) :)), ",") + ")";
    }
    else if (sizeof(args)) d_txt +=
      " (" + implode(map(args, (: sprintf("%02x", $1) :)), ",") + ")";
  }
  return d_txt;
}    

// Full negotiation tries to set all our preferences
// Options with closures as preference are not actively enabled.
private void start_telnetneg() {
  int opt, *options;

  ts[TS_EXTRA, TSE_TELNETNEG] = 1;

  options = m_indices(ts) - ({ TS_EXTRA });
  foreach (opt : options) {
    if (ts[opt, TS_R_AGREE] == 1) set_telnet(DO, opt);
    if (ts[opt, TS_L_AGREE] == 1) set_telnet(WILL, opt);
  }

}

// Telnet protocoll violations end up here
private void tel_error(string err) {
  string log;

  log = ts[TS_EXTRA, TSE_LOG];
  if (strlen(log) > 4000) log = "..." + log[<3800..];
  log += "ERR " + err + "\n";
  ts[TS_EXTRA, TSE_LOG] = log;
}

// ******************** Telnet Server Engine ********************
//
// The following code is the telnet server using the telnet state machine.
// It has to follow the some basic rules that cannot be assured by the
// state machine to be complient to RFC 854. The most important is to
// enable only options which are fully implemented.
//
// State changes are requested by the driver with the H_NOECHO hook
// and set_noecho().
//
// Here is defined how the mud will act in the negotiation.

nosave int* tm_t;

void create() {
  if (!ts) {
    ts = m_allocate(7, TS_SIZE);
    ts[TS_EXTRA, TSE_LOG] = "";
  }

  // set how we would like the options' states
  set_callback(TELOPT_NAWS,     DO,   WONT, 0,           #'sb_naws);
  set_callback(TELOPT_STATUS  , DONT, WILL, 0,           #'sb_status);
  set_callback(TELOPT_TTYPE,    DO,   WONT, #'start_sb,  #'sb_ttype);
  set_callback(TELOPT_TSPEED,   DO,   WONT, #'start_sb,  #'sb_tspeed);
  set_callback(TELOPT_NEWENV,   DO,   WONT, #'start_sb,  #'sb_env);
  set_callback(TELOPT_ENVIRON,  DO,   WONT, #'start_sb,  #'sb_env);
  set_callback(TELOPT_XDISPLOC, DO,   WONT, #'start_sb,  #'sb_xdisp);

  set_callback(TELOPT_EOR,      DONT, WILL, #'start_eor, 0);
  set_callback(TELOPT_LINEMODE, DO,   WONT, #'start_lm,  #'sb_line);
  set_callback(TELOPT_TM ,      #'neg_tm, #'neg_tm, #'neg_tm, 0);
  set_callback(TELOPT_BINARY,   #'neg_bin, #'neg_bin, 0, 0);

  set_callback(TELOPT_SGA,      #'neg_sga, #'neg_sga, #'cb_sga, 0);
  set_callback(TELOPT_ECHO,     #'neg_echo, WONT, #'cb_echo, 0);
}

// Change the NOECHO and CHARMODE state, called indirectly thru input_to()
void set_noecho(int flag) {
  // Security check needs H_NOECHO to be specified as follows,
  // with lfun in the master:
  //
  //   set_driver_hook(H_NOECHO, #'noecho_hook);
  //
  //   void noecho_hook(int flag, int ob) {
  //     if (ob) ob->set_noecho(flag);
  //   } 
  int i, j;

  if (object_name(previous_object()) != __MASTER_OBJECT__) return;
  if (!ts[TS_EXTRA, TSE_TELNETNEG]) return; // client does not telnetneg

  flag &= 255;

  i = Q_TSE_NOECHO;

  // Check if a change in NOECHO is requested
  if (flag & INPUT_NOECHO) {
    if (i == NO || i == WANT_NO) {
      S_TSE_NOECHO(WANT_YES);
      // If CHARMODE and NOECHO are requested (as usual), enter NOECHO
      // only if we could enter CHARMODE, else the player would be blind.
      if (!(flag & INPUT_CHARMODE)) {
        ts[TS_EXTRA, TSE_LOG] += "* trying to get NOECHO\n";
        set_telnet(WILL, TELOPT_ECHO);
      }
    }
  } else {
    if (i == YES || i == WANT_YES) {
      ts[TS_EXTRA, TSE_LOG] += "* trying to leave NOECHO\n";
      S_TSE_NOECHO(WANT_NO);
      set_telnet(WONT, TELOPT_ECHO);
    }
  }

  i = Q_TSE_SGA_CHAR;
  j = Q_TSE_LM_CHAR;

  // Check if a change in CHARMODE is requested
  // When the client knows LINEMODE, use it. If not use old (sga) charmode.
  if (flag & INPUT_CHARMODE) {
    if (i != YES && j != YES && i != WANT_YES && j != WANT_YES) {
      ts[TS_EXTRA, TSE_LOG] += "* trying to get CHARMODE\n";

      if (Q_REMOTE(ts[TELOPT_LINEMODE, TS_STATE] == YES)) {
        S_TSE_LM_CHAR(WANT_YES);
        send(({ IAC, SB, TELOPT_LINEMODE, LM_MODE,
          MODE_SOFT_TAB | MODE_LIT_ECHO, IAC, SE }));
      } else {
        S_TSE_SGA_CHAR(WANT_YES);
        set_telnet(WILL, TELOPT_SGA);
        set_telnet(DO, TELOPT_SGA);
      }
    }
  } else {
    if (i == YES || j == YES || i == WANT_YES || j == WANT_YES) {
      ts[TS_EXTRA, TSE_LOG] += "* trying to leave CHARMODE\n";
      // check both flags independently
      if (j == YES || j == WANT_YES) {
        S_TSE_LM_CHAR(WANT_NO);
        send(({ IAC, SB, TELOPT_LINEMODE, LM_MODE,
          MODE_EDIT | MODE_SOFT_TAB | MODE_LIT_ECHO, IAC, SE }));
      }
      if (i == YES || i == WANT_YES) {
        S_TSE_SGA_CHAR(WANT_NO);
        set_telnet(WONT, TELOPT_SGA);
        set_telnet(DONT, TELOPT_SGA);
      }
    }
  }
}

//
// The callback functions
//

// neg_*() are called to decide if we say 'no' or 'yes' to an incoming
// option change request

private int neg_sga(int command, int option) {
  // Remote SGA   is allowed for old charmode.
  // Local SGA    is allowed for noecho mode (due to the SGA/ECHO ambiguity,
  //              see RFC 857). It is also allowed if we are in old charmode
  //              because some clients think it's needed.
  int i;

  if (command == DO) {
    i = Q_TSE_NOECHO;
    if (i == WANT_YES || i == YES) return 1;
  }

  // else WILL
  i = Q_TSE_SGA_CHAR;
  if (i == WANT_YES || i == YES) return 1;
  return 0;
}

private int neg_echo(int command, int option) {
  // Remote ECHO  is never allowed (why should we like to get our own text)
  // Local ECHO   is allowed for noecho mode

  // only called for DO
  int i;
  i = Q_TSE_NOECHO;
  if (i == WANT_YES || i == YES) return 1;
  return 0;
}

private int neg_bin(int command, int option) {
  // We like to have it enabled, but we don't want to have it enabled
  // with start_telnetneg().

  // BINARY actively enabled only it the TTYPE is "xterm".

  // Some clients cant even answer a BINARY request properly (they have
  // to embed a '\0' into the stream). The RFC says, that it is needed
  // for 8 bit data path. Most clients just use this anyway and send
  // for example german umlauts. Strictly it's forbidden, though. Some
  // clients (BSD telnet for example) do honor this state and need
  // BINARY to send umlauts.
  return 1;
}

private int neg_tm(int command, int option) {
  int *t, i;

  // reset state immediately
  if (command == DO || command == DONT) {
    i = ts[option, TS_STATE];
    i = S_LOCAL(i, NO);
    i = S_REMOTE(i, NO);
    i = S_REMOTER(i, 0);
    ts[option, TS_STATE] = i;
  }

  if (command == WILL || command == WONT) {
    i = ts[option, TS_STATE];
    i = S_LOCAL(i, NO);
    i = S_REMOTE(i, NO);
    i = S_REMOTER(i, 0);
    ts[option, TS_STATE] = i;

    // got an answer
    t = utime();
    i = (t[0]-tm_t[0])*1000 + (t[1]-tm_t[1])/1000;
    tm_t = 0;
    i -= 11; // interne Verzoegerung (muss man messen!)
    if (i < 1) i = 1;
    tell_object(this_object(), "Hurrikap teilt Dir mit: "
      "Daten benoetigen von Dir zum Mud und zurueck "+i+" ms.\n");
  }

  return 1;
}

// start_*() and cb_*() are called on/after succesfull option changes
// We do the appropriate things then.

private void start_sb(int command, int option) {
  // If we got a WILL, request subnegotiation
  if (command == WILL) send(({ IAC, SB, option, TELQUAL_SEND, IAC, SE }));
}

private void start_lm(int command, int option) {
  if (command != WILL) return;
  if (!ts[option, TS_SB]) ts[option, TS_SB] = allocate(LM_SLC+1);

  send(({ IAC, SB, TELOPT_LINEMODE, LM_MODE,
    MODE_EDIT | MODE_SOFT_TAB | MODE_LIT_ECHO, IAC, SE }));
  ts[option, TS_SB][LM_MODE] = MODE_EDIT | MODE_SOFT_TAB | MODE_LIT_ECHO;

  // Flush on every control character
  // remember to prepend 0xff with IACs
  send(({ IAC, SB, TELOPT_LINEMODE, DO,
    LM_FORWARDMASK, IAC, 0xff, IAC, 0xff, IAC, 0xff, IAC, 0xff, IAC, SE }));
  ts[option, TS_SB][LM_FORWARDMASK] = WANT_YES;
  
}

private void start_eor(int command, int option) {
  // If we are allowed to use EOR whilst displaying a possible prompt
  // line, mark is as such. This happens unually only at the login.
  if (command == DO) efun::binary_message(({ IAC, EOR }));

  // Adjust prompt method to appropriate function
  if (command == DO || command == DONT) modify_prompt();
}

private void cb_sga(int command, int option) {
  switch (command) {
    case WILL:
      if (Q_TSE_SGA_CHAR == WANT_YES) {
        S_TSE_SGA_CHAR(YES);
        ts[TS_EXTRA, TSE_LOG] += "* CHARMODE established\n";
        if (Q_TSE_NOECHO == WANT_YES) {
          ts[TS_EXTRA, TSE_LOG] += "* trying to get NOECHO\n";
          set_telnet(WILL, TELOPT_ECHO);
        }
      }
      break;
    case WONT:
      switch (Q_TSE_SGA_CHAR) {
        case WANT_NO:
          ts[TS_EXTRA, TSE_LOG] += "* CHARMODE left\n";       break;
        case WANT_YES:
          ts[TS_EXTRA, TSE_LOG] += "* CHARMODE denied\n";          break;
        case YES:
          ts[TS_EXTRA, TSE_LOG] += "* CHARMODE forcefully left\n"; break;
      }
      S_TSE_SGA_CHAR(NO);
      break;
  }
}

private void cb_echo(int command, int option) {
  switch (command) {
    case DO:
      if (Q_TSE_NOECHO == WANT_YES) {
        S_TSE_NOECHO(YES);
        ts[TS_EXTRA, TSE_LOG] += "* NOECHO established\n";
      }
      break;
    case DONT:
      switch (Q_TSE_NOECHO) {
        case WANT_NO:
          ts[TS_EXTRA, TSE_LOG] += "* NOECHO mode left\n";       break;
        case WANT_YES:
          ts[TS_EXTRA, TSE_LOG] += "* NOECHO denied\n";          break;
        case YES:
          ts[TS_EXTRA, TSE_LOG] += "* NOECHO forcefully left\n"; break;
      }
      S_TSE_NOECHO(NO);
      break;
  }
}

// sb_*() handle the different incoming subnegotiations

private void sb_status(int command, int option, int* optargs) {
  int *ret, opt, state, s, end;

  if (sizeof(optargs) != 1) return;
  if (optargs[0] != TELQUAL_SEND) return;

  ret = ({});
  foreach (opt : ts) {
    if (opt < 0) continue;
    state = ts[opt, TS_STATE];
    s = Q_LOCAL(state);
    if (s == YES || s == WANT_NO) ret += ({ WILL, opt });
    s = Q_REMOTE(state);
    if (s == YES || s == WANT_NO) ret += ({ DO, opt });
  }

  // some driver include the string end '\0'
  end = sizeof(to_array("")) + 1;

  if (ts[TELOPT_TSPEED, TS_SB])
    ret += ({ SB, TELOPT_TSPEED, TELQUAL_IS }) +
	    to_array(ts[TELOPT_TSPEED, TS_SB][0]+","+
	    ts[TELOPT_TSPEED, TS_SB][1])[0..<end] + ({ -SE });

  if (ts[TELOPT_XDISPLOC, TS_SB])
    ret += ({ SB, TELOPT_XDISPLOC, TELQUAL_IS }) +
	    to_array(ts[TELOPT_XDISPLOC, TS_SB])[0..<end] + ({ -SE });

  if (ts[TELOPT_TTYPE, TS_SB])
    ret += ({ SB, TELOPT_TTYPE, TELQUAL_IS }) +
	    to_array(ts[TELOPT_TTYPE, TS_SB][1][0])[0..<end] + ({ -SE });

  if (ts[TELOPT_NAWS, TS_SB])
    ret += ({ SB, TELOPT_NAWS, ts[TELOPT_NAWS, TS_SB][0]/0x100,
		    ts[TELOPT_NAWS, TS_SB][0]%0x100,
		    ts[TELOPT_NAWS, TS_SB][1]/0x100,
		    ts[TELOPT_NAWS, TS_SB][1]%0x100, -SE });

  if (ts[TELOPT_LINEMODE, TS_SB]) {
    ret += ({ SB, TELOPT_LINEMODE, LM_MODE,
            ts[TELOPT_LINEMODE, TS_SB][LM_MODE], -SE });
    if (ts[TELOPT_LINEMODE, TS_SB][LM_FORWARDMASK] == YES)
      ret += ({ SB, TELOPT_LINEMODE, DO, LM_FORWARDMASK, 0, 0x24, -SE });
    if (sizeof(ts[TELOPT_LINEMODE, TS_SB][LM_SLC])) {
      ret += ({ SB, TELOPT_LINEMODE, LM_SLC });
      foreach (int i, int j, int k : ts[TELOPT_LINEMODE, TS_SB][LM_SLC])
        ret += ({ i, j, k });
      ret += ({ -SE });
    }
  }

  // We dont send SB ENVIRON or SB NEWENV

  // quote IAC and SE
  // we use -SE for unquotable SEs
  for (s = sizeof(ret); s--;) {
    if (ret[s] == IAC) ret[s..s] = ({ IAC, IAC });
    if (ret[s] ==  SE) ret[s..s] = ({ SE, SE });
    if (ret[s] == -SE) ret[s]    = SE;
  }

  send(({ IAC, SB, option, TELQUAL_IS }) + ret + ({ IAC, SE }));

}

private void sb_naws(int command, int option, int* optargs) {
  int cols, lines, *old;

  if (sizeof(optargs) != 4) return tel_error("SB NAWS with wrong size");

  cols  = optargs[1] + optargs[0] * 256;
  lines = optargs[3] + optargs[2] * 256;
  old = ts[option, TS_SB];
  ts[option, TS_SB] = ({ cols, lines });

  ts[TS_EXTRA, TSE_LOG] +=
    "     Window size: " + cols + " cols, " + lines + " lines\n";

  // inform mudlib
  if (pointerp(old)) {
    if (old[0] != cols)  SetProp(P_TTY_COLS, cols);
    if (old[1] != lines) SetProp(P_TTY_ROWS, lines);
  }
}

private void sb_xdisp(int command, int option, int* optargs) {
  string value;
  int len;

  if (sizeof(optargs) < 2) return;
  if (optargs[0] != TELQUAL_IS) return;
  value = to_string(filter(optargs[1..], (: $1 >= ' ' && $1 <= '~' :)));
  len = strlen(value);

  if (strlen(value) != sizeof(optargs)-1)
    tel_error("SB XDISPLOC contained bad characters");
  if (!len)
    return tel_error("SB XDISPLOC empty, ignoring");

  ts[option, TS_SB] = value;
  ts[TS_EXTRA, TSE_LOG] += "     XDISPLOC is " + value + "\n";
}

private void sb_ttype(int command, int option, int* optargs) {
  string value;
  int len, i;
  mixed* all;

  if (sizeof(optargs) < 2) return;
  if (optargs[0] != TELQUAL_IS) return;
  value = to_string(filter(optargs[1..], (: $1 >= ' ' && $1 <= '~' :)));
  len = strlen(value);

  if (strlen(value) != sizeof(optargs)-1)
    tel_error("SB TTYPE contained bad characters");
  if (!len)
    return tel_error("SB TTYPE empty, ignoring");
  value = lower_case(value);
  ts[TS_EXTRA, TSE_LOG] += "     TTYPE is " + value + "\n";

  all = ts[option, TS_SB];
  if (!all) {
    all = ({ 0, ({}) });
    // inform mudlib
    SetProp(P_TTY_TYPE, value);
  }

#ifdef GET_ALL_TTYPES
  // Clients may provide different terminal types which are synonyms to
  // the native one (RFC 930). It is also possible to have a client which
  // switches terminal emulation along with the given (non synonymous)
  // terminal type names (RFC 1091). All these types are collected if
  // GET_ALL_TTYPES is defined.
  //
  // The drawback is, that the cycling thru all terminal emulations takes
  // some time, especially on slow links. MS windows 2000's and XP's
  // telnet have an emulation with key on-off sequences rather that chars.
  // Commands typed while TTYPE is negotiated may be wrong with that clients.
 
  i = member(all[1], value);
  if (i < 0) {
    all[1] += ({ value });
    // get next TTYPE
    if (Q_REMOTE(ts[option, TS_STATE]) == YES) start_sb(WILL, option);
  } else {
    // switch back to first
    if (i == 0) {
      // ok, initial type re-established
      ts[TS_EXTRA, TSE_LOG] += "     TTYPE in client set\n";
      return;
    }
    switch (++all[0]) {
      case 1:
        if (Q_REMOTE(ts[option, TS_STATE]) == YES) start_sb(WILL, option);
        break; // just retry
      case 2: 
        tel_error("could not get initial TTYPE, may be rfc930 client");
        if (value != "vtnt") break;
        tel_error("trying trick to restart TTYPE list on windows client");
        set_telnet(DONT, TELOPT_TTYPE);
        set_telnet(DO, TELOPT_TTYPE);
        break;
      case 3:
        tel_error("giving up on TTYPE");
        break;
    }
  }
#endif

  ts[option, TS_SB] = all;

  // Some clients cannot answer BINARY correctly. Xterm should be
  // able to. Also most clients ignore this option and just use a 8 bit
  // data path; only BSD telnet is known to need BINARY in order to
  // send umlauts.
  if (lower_case(value) == "xterm")
    set_telnet(DO, TELOPT_BINARY);
}

private void sb_tspeed(int command, int option, int* optargs) {            
  int i, j, os;

  os = sizeof(optargs);
  if (os < 2) return;
  if (optargs[0] != TELQUAL_IS) return;

  for (i = 1; i < os; ++i) {
    if (optargs[i]==',') break;
    if (optargs[i] < '0' || optargs[i] > '9')
      return tel_error("SB TSPEED invalid character");
  }

  if (i == 1) return tel_error("SB TSPEED empty transmit speed");
  if (i == os) return tel_error("SB TSPEED no comma found");

  for (j = i+1; j < os; ++j) {
    if (optargs[j] < '0' || optargs[j] > '9')
      return tel_error("SB TSPEED invalid character");
  }

  if (i+1 >= os) return tel_error("SB TSPEED empty receive speed");

  ts[option, TS_SB] = ({ to_int(to_string(optargs[1..i-1])),
                         to_int(to_string(optargs[i+1..])) });

  ts[TS_EXTRA, TSE_LOG] +=
    "     Transmit speed " + ts[option, TS_SB][0] +
    ", receive speed " + ts[option, TS_SB][1] + "\n";

}

private void sb_env(int command, int option, int* optargs) {
  int i, j, os;
  mixed mix;
  mapping env;
  string s;

  os = sizeof(optargs);
  if (os < 2) return;
  if (optargs[0] != TELQUAL_IS && optargs[0] != TELQUAL_INFO) return;

  mix = ({});
  for (i = j = 1; j < os; ++j) {
    if (optargs[j] == ENV_ESC) {
      ++j;
      continue;
    }
    if (optargs[j] <= ENV_USERVAR) {
      if (i != j) {
        s = to_string(filter(optargs[i..j-1], #'>=, ' '));
        if (strlen(s) != j-i)
          tel_error("SB ENV contained bad characters");
        mix += ({ s });
      }
      mix += ({ optargs[j] });
      i = j + 1;
      continue;
    }
  }
  if (i < j-1) {
    s = to_string(filter(optargs[i..j-1], #'>=, ' '));
    if (strlen(s) != j-i)
      tel_error("SB ENV contained bad characters");
    mix += ({ s });
  }
        
  // Due to some weird implementation mistakes (see RFC 1571)
  // we can't be shure if VALUE or VAR are valid or swapped.
  // So we don't distuingush VAR and USERVAR and assume that every
  // variable's name is followed by its value.
        
  s = 0;
  env = ([]);
  os = sizeof(mix);
  for (i = 0; i < os; ++i) {
    if (!s && stringp(mix[i])) s = mix[i];
    else if (s) {
      if (stringp(mix[i])) {
        env[s] = mix[i];
        s = 0;
      } else if (!stringp(mix[i-1])) {
        env[s] = 1; // defined but no value assigned
        s = 0;
      }
    }
  }

  ts[option, TS_SB] = env;

  foreach (s, mix : env) {
    ts[TS_EXTRA, TSE_LOG] += "     Environment " + s +" => " +
      (stringp(mix) ? mix : "defined") + "\n";
  }

}

private void sb_line(int command, int option, int* optargs) {
  // Maybe LINEMODE should be turned of on errors, but then... I never saw any.
  int state, *xx, i, j;

  if (!ts[option, TS_SB]) ts[option, TS_SB] = allocate(LM_SLC+1);

  if (sizeof(optargs) < 2) return;
  switch (optargs[0]) {
    case LM_MODE:
      if (!(optargs[1] & MODE_ACK))
        return tel_error("SB LINEMODE MODE with no ACK set (ignoring)");

      if (optargs[1] & MODE_EDIT) switch (Q_TSE_LM_CHAR) {
        case YES:
          return tel_error("SB LINEMODE MODE EDIT unexpected");
        case WANT_YES:
          return tel_error("SB LINEMODE MODE EDIT invalid answer");
        case WANT_NO:
          S_TSE_LM_CHAR(NO);
          ts[TS_EXTRA, TSE_LOG] += "* CHARMODE left\n";
          break;
      }
      else switch (Q_TSE_LM_CHAR) {
        case NO:
          return tel_error("SB LINEMODE MODE EDIT unexpected");
        case WANT_NO:
          return tel_error("SB LINEMODE MODE EDIT invalid answer");
        case WANT_YES:
          S_TSE_LM_CHAR(YES);
          ts[TS_EXTRA, TSE_LOG] += "* CHARMODE established\n";
          if (Q_TSE_NOECHO == WANT_YES) {
            ts[TS_EXTRA, TSE_LOG] += "* trying to get NOECHO\n";
            set_telnet(WILL, TELOPT_ECHO);
          }
          break;
      }
      ts[option, TS_SB][LM_MODE] = optargs[1] & ~MODE_ACK;

      break;

    // server does not use forwardmask
    case DO:
    case DONT:
      if (optargs[1] != LM_FORWARDMASK) {
        return tel_error("SB LINEMODE " + TELCMD2STRING(optargs[0]) +
          sprintf(" %02x unknown (ignored)", optargs[1]));
      }
      return tel_error("SB LINEMODE DO/DONT FORWARDMASK not "
        "allowed for client");
      return;
    // client is requested to use forwardmask
    case WILL:
      if (optargs[1] != LM_FORWARDMASK) {
        return tel_error("SB LINEMODE " + TELCMD2STRING(optargs[0]) +
          sprintf(" %02x unknown (ignored)", optargs[1]));
      }
      state = ts[option, TS_SB][LM_FORWARDMASK];
      switch (state) {
        case NO: 
          tel_error("got spontan WILL FORWARDMASK (denied)");
          break;
        case YES:
          // is already set, ignore
          break;
        case WANT_NO:
          tel_error("got WILL on DONT");
          break;
        case WANT_YES:
          ts[option, TS_SB][LM_FORWARDMASK] = YES;
          break;
      }
      return;
    case WONT:
      if (optargs[1] != LM_FORWARDMASK) {
        return tel_error("SB LINEMODE " + TELCMD2STRING(optargs[0]) +
          sprintf(" %02x unknown (ignored)", optargs[1]));
      }
      state = ts[option, TS_SB][LM_FORWARDMASK];
      switch (state) {
        case YES: 
          send(({ IAC, SB, option, DONT, LM_FORWARDMASK, IAC, SE }));
          ts[option, TS_SB][LM_FORWARDMASK] = NO;
          break;
        case NO:
          // is already set, ignore
          break;
        case WANT_YES:
        case WANT_NO:
          ts[option, TS_SB][LM_FORWARDMASK] = NO;
          break;
      }
      return;

    case LM_SLC:
        if (sizeof(optargs) < 4) return tel_error("SB too short");
        if (!ts[option, TS_SB][LM_SLC]) ts[option, TS_SB][LM_SLC] = ([ :2 ]);
        xx = ({ IAC, SB, TELOPT_LINEMODE, LM_SLC });

        // Request for our preferences?
        // All the rest (especially the cursor movement) we would like
        // to be escape sequences, so no SLCs for that
        if (optargs[1] == 0 && optargs[2] == SLC_DEFAULT && optargs[3] == 0) {
          for (i = SLC_SYNCH; i <= SLC_SUSP; ++i)
            xx += ({ i, SLC_NOSUPPORT, 0 });
          xx += ({ SLC_EC,    SLC_DEFAULT,   127,
                   SLC_EL,    SLC_DEFAULT,    21,
                   SLC_EW,    SLC_DEFAULT,    23,
                   SLC_XON,   SLC_NOSUPPORT,   0,
                   SLC_XOFF,  SLC_NOSUPPORT,   0,
                   SLC_FORW1, SLC_NOSUPPORT,   0,
                   SLC_FORW2, SLC_NOSUPPORT,   0, IAC, SE });
          send(xx);
          break;
        }

        // Answer to client's preferences
        // We dont want any of the session control keys (Synch - Susp)
        // Everything else is just acknoledged
        optargs = optargs[1..];
        j = sizeof(optargs) - 3;
        for (i = 0; i < j; i+=3) {
          if (optargs[i+1] & SLC_ACK) {
            m_add(ts[option, TS_SB][LM_SLC], optargs[i],
              optargs[i+1] & ~SLC_ACK, optargs[i+2]);
            continue;
          }
          if (optargs[i+1] == SLC_NOSUPPORT) continue;
          if (optargs[i] > SLC_SUSP) {
            m_add(ts[option, TS_SB][LM_SLC], optargs[i],
              optargs[i+1] & ~SLC_ACK, optargs[i+2]);
            xx += ({ optargs[i], optargs[i+1] | SLC_ACK, optargs[i+2] });
          } else
            xx += ({ optargs[i], SLC_NOSUPPORT, 0 });
        }
        i = sizeof(xx);
        if (i > 4) {
          for (; --i;) if (xx[i] == 0xff) xx[i..i] = ({ IAC, 0xff });
          send(xx + ({ IAC, SE }));
        }

        return;
  }
}

// helper functions, called from mudlib
void send_telopt_tm() {
  tm_t = utime();
  set_telnet(DO, TELOPT_TM);
}   

void _dumptelnegs() {
  printf(ts[TS_EXTRA, TSE_LOG]);
}
