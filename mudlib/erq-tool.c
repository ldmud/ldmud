#include <erq.h>

object command_giver;
int *ticket;
int verbose = 1;

short() {
    return "Erq tool";
}

id(s) {
    switch(s) {
      case "erq-tool":
      case "erq tool":
      case "erq":
	return 1;
      default:
	return 0;
    }
}

long() {
    return "\
This tool allows you to use the ERQ_SPAWN / ERQ_SEND / ERQ_KILL requests\n\
of the external command demon.\n\
\n\
Usage:\n\
  e <program> <arguments>\n\
    start <program> with <arguments>. Blanks, tabs and backslashes have to be\n\
    quoted if desired as a part of the program name and / or an argument.\n\
    The text is directly sent to the ERQ_SPAWN request, which does the\n\
    commandline interpretation wrt. blanks and whitespace.\n\
    You can only start a new program with this tool while it is not\n\
    currently controlling another one.\n\
  e <text>\n\
    send <text> to the standard input of the running program.\n\
  er\n\
    send a single newline to the standard input of the running program.\n\
  ekill\n\
    kill the running program\n\
  ekill <signal>\n\
    send <signal> to the running program.\n\
  everbose\n\
    toggle verbose mode.\n\
  econnect\n\
    send all input to stdin of the running program.\n\
\n\
  output from stdout will be displayed with a prepended single quote,\n\
  output from stderr will be displayed with a prepended double quote.\n\
";
}

get() {
    return 1;
}

drop(silently) {
    return query_verb() != "give";
}

init() {
    add_action("erq", "e");
    add_action("erq", "er");
    add_action("ekill", "ekill");
    add_action("everbose", "everbose");
    add_action("econnect", "econnect");
}

receive_output(a) {
    if (!ticket) {
	switch(a[0]) {
	  case ERQ_OK:
	    write("Command accepted.\n");
	    ticket = a[1..];
	    return;
	  case ERQ_E_ARGLENGTH:
	    write("Too long argument\n");
	    break;
	  case ERQ_E_ARGNUMBER:
	    write("Too many arguments\n");
	    break;
	  case ERQ_E_PATHLEN:
	    write("Too long pathname\n");
	    break;
	  case ERQ_E_ARGFORMAT:
	    write("Syntax error\n");
	    break;
	  case ERQ_E_ILLEGAL:
	    write("Illegal pathname\n");
	    break;
	  case ERQ_E_FORKFAIL:
	    write("Fork failed\n");
	    break;
	  default:
	    write("Unknown error, command rejected\n");
	    break;
	}
	command_giver = 0;
	return;
    }
    switch(a[0]) {
      case ERQ_STDOUT:
	a[0] = '\'';
	tell_object(command_giver, to_string(a));
	return;
      case ERQ_STDERR:
	a[0] = '\"';
	tell_object(command_giver, to_string(a));
	return;
      case ERQ_EXITED:
	tell_object(command_giver,
	  sprintf("Program exited with status %d.\n", a[1]));
	break;
      case ERQ_SIGNALED:
	tell_object(command_giver,
	  sprintf("Program caught signal %d.\n", a[1]));
	break;
      case ERQ_E_UNKNOWN:
	tell_object(command_giver, "Unknown error.\n");
	break;
    }
    command_giver = 0;
    ticket = 0;
}

send_reply(a, p, s) {
    switch (a[0]) {
      case ERQ_OK:
	if (verbose)
	    tell_object(p, "Ok.\n");
	break;
      default:
	tell_object(p, "Input not fully accepted.\n");
	break;
    }
}

kill_reply(a, p, s) {
    switch (a[0]) {
      case ERQ_OK:
	if (verbose)
	    tell_object(p, "Ok.\n");
	break;
      case ERQ_E_TICKET:
	tell_object(p, "Trying to kill a dead process\n");
	break;
      case ERQ_E_ILLEGAL:
	tell_object(p, "Illegal signal\n");
	break;
      default:
	tell_object(p, "Kill error.\n");
	break;
    }
}

erq(s) {
    if (!s)
	s = "";
    if (ticket) {
	send_erq(ERQ_SEND, ticket+to_array(s)+({'\n'}), lambda(({'a}),
	  ({#'send_reply, 'a, command_giver, s})));
	return 1;
    } else if (!command_giver) {
	command_giver = this_player();
	send_erq(ERQ_SPAWN, s, #'receive_output);
	return 1;
    } else {
	write("Waiting for command to be accepted\n");
	return 1;
    }
}

ekill(s) {
    int signal;

    if (!ticket) {
	write("No program running\n");
	return 1;
    }
    if ( !(s && sscanf(s, "%d", signal)) )
	signal = 9;
    send_erq(ERQ_KILL, ticket+({0,0,0,signal}), lambda(({'a}),
	  ({#'kill_reply, 'a, command_giver, signal})));
    return 1;
}

everbose(s) {
    switch(s) {
      case "on":
	verbose = 1;
	return 1;
      case "off":
	verbose = 0;
	return 1;
      case 0:
	verbose = !verbose;
	return 1;
    }
}

econnect() {
    write("Use 'edisconnect' to stop.\n");
    add_action("erq", "", 2);
    add_action("edisconnect", "edisconnect");
    add_action("erq", "er");
    return 1;
}

edisconnect() {
    remove_action("");
    remove_action("edisconnect");
    remove_action("er");
    return 1;
}
