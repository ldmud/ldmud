#define TELNET_OB     "secure/sockets/telnet_ob"

mapping sessions = ([ ]);

status main(string str) {
  string host;
  int port;
  if (!adminp(this_player()))
   return 0;
  if (!str) {
    if (sessions[this_player()] == 0)
     return
      notify_fail("telnet: Could not find detatched session.\n");
    (void)sessions[this_player()]->reattach();
    return 1;
    }
  if (str == "-q") {
    if (sessions[this_player()] == 0)
     return
      notify_fail("telnet: Could not find session.\n");
    destruct(sessions[this_player()]);
    return 1;
    }
  if (sessions[this_player()])
   return
    notify_fail("telnet: You have a detatched session.\n"
                "Type 'telnet' to reattach to it.\n");
  if (sscanf(str, "%s %d", host, port) != 2)
   host = str, port = 23;
  sessions[this_player()] = clone_object(TELNET_OB);
  (void)sessions[this_player()]->start(host, port);
  return 1;
  }

void help() {
  write("telnet [<address> [<port>] | -q]\n"
        "Telnet to the address given, or reactivate a dormant session.\n"
        "You may only have one session at any given time.\n"
        "Several telnet commands are available, a telnet\n"
        "command starts with the escape character (^[).\n"
        "Use \"^[?\" to get a list of available telnet commands.\n"
        "The \"-q\" option allows you to close a telnet session\n"
        "from the commandline.\n");
  return;
  }

mapping query_sessions() { return copy_mapping(sessions); }
