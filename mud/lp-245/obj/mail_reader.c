#define NAME "room/post_dir/"

string messages;		/* The big string of all messages. */
int new_mail;			/* Flag if there is any new mail. */

static string * arr_messages;	/* the result of explode on 'messages' */
static int loaded;		/* Flag if this users mailbox is loaded */
static int curr_mess;		/* Current message number */
static string new_message, new_subject, new_dest, new_cc;

static int is_reading;

int id(string str) {
    return str == "mailread";
}

int get() {
    return 1;
}

void init() {
    add_action("read", "read");
    add_action("mail_to", "mail");
    add_action("headers", "from");
}

void load_player() {
    loaded = 1;
    if (!restore_object(NAME + this_player()->query_real_name()) ||
      messages == "" || messages == 0) {
	arr_messages = 0;
	messages = "";
	return;
    }
    if (new_mail) {
	new_mail = 0;
	save_object(NAME + this_player()->query_real_name());
    }
    arr_messages = explode(messages, "\n**\n");
}

void loop() {
    string tmp;

    if (!loaded)
	load_player();
    if (arr_messages == 0) {
	write("No messages.\n");
	is_reading = 0;
	return;
    }
    if (curr_mess < 1 || curr_mess > sizeof(arr_messages)/2)
	tmp = " (no current) ";
    else
	tmp = " (current: " + curr_mess + ") ";
    write("[1 - " + (sizeof(arr_messages)/2) + " h d r x ?]" + tmp);
    input_to("get_cmd");
}

int read() {
    is_reading = 1;
    if (!loaded) {
	load_player();
	if (arr_messages == 0) {
	    write("No mail.\n");
	    return 1;
	}
    }
    loop();
    return 1;
}

int headers() {
    int i, n;
    string tmp_str, tmp_str2, rest_of_mess;
    string name;

    if (!loaded)
	load_player();
    if (arr_messages == 0) {
	write("No mail.\n");
	return 1;
    }
    for (i=0; i<sizeof(arr_messages); i+=2) {
	name = i/2+1 + " " + arr_messages[i];
	if (strlen(name) < 8)
	    name += "\t";
	write(name + "\t");
        n = sscanf(arr_messages[i+1],"%sRe:   %s\n%s",
		   tmp_str,tmp_str2,rest_of_mess);
        if (n == 3) write(" Re:   " + tmp_str2);
        n = sscanf(arr_messages[i+1],"%sSubj: %s\n%s",
		   tmp_str,tmp_str2,rest_of_mess);
        if (n == 3) write(" Subj: " + tmp_str2);
        n = sscanf(arr_messages[i+1],"%sDate: %s\n%s",
		   tmp_str,tmp_str2, rest_of_mess);
        if (n == 3) write(" Date: " + tmp_str2);
	write("\n");
    }
    return 1;
}

int mail_to_continue(string str) {
    str = lower_case(str);
    if (!restore_object("players/" + str)) {
	write("No such player.\n");
	if (is_reading)
	    loop();
	return 1;
    }
    new_dest = str;
    new_message = "";
    if (new_subject)
	write("Return to get '" + new_subject + "'\n");
    write("Subject: ");
    input_to("get_subject");
    return 1;
}

int mail_to(string str) {
    if (!str || str == "") {
	write("No destination.\n");
	if (is_reading)
	    loop();
	return 1;
    }
    new_subject = 0;
    new_cc = "";
    mail_to_continue(str);
    return 1;
}

void reply_to_message() {
    int n;
    string tmp_str, tmp_str2, rest_of_mess;
    string name;

    name = lower_case(arr_messages[curr_mess*2-2]);
    n = sscanf(arr_messages[curr_mess*2-1],"%sRe:   %s\n%s",
	       tmp_str,tmp_str2,rest_of_mess);
    new_subject = " Re:   " + tmp_str2;
    n = sscanf(arr_messages[curr_mess*2-1],"%sSubj: %s\n%s",
	       tmp_str,tmp_str2,rest_of_mess);
    new_subject = " Re:   " + tmp_str2;
    new_cc = "";
    mail_to_continue(name);
}

void print_message() {
    if (curr_mess > sizeof(arr_messages)/2 || curr_mess < 1) {
	write("Illegal message number " + curr_mess + "\n");
	return;
    }
    write("Message " + curr_mess + ":\n");
    write("From: " + arr_messages[curr_mess*2-2] + "\n");
    write(arr_messages[curr_mess*2-1] + "\n");
}

void save_message() {
    string name;

    if (curr_mess > sizeof(arr_messages)/2 || curr_mess < 1) {
	write("Illegal message number " + curr_mess + "\n");
	return;
    }
    name = this_player()->query_real_name() + ".mbox";
    log_file(name, "From: " + arr_messages[curr_mess*2-2] + "\n");
    log_file(name, arr_messages[curr_mess*2-1] + "\n");
}

void delete() {
    if (curr_mess > sizeof(arr_messages)/2 || curr_mess < 1) {
	write("Illegal message number " + curr_mess + "\n");
	return;
    }
    arr_messages[curr_mess*2-2] = 0;
    arr_messages[curr_mess*2-1] = 0;
    if (sizeof(arr_messages) == 2)
	messages = "";
    else
	messages = implode(arr_messages, "\n**\n") + "\n**\n";
    new_mail = 0;
    save_object(NAME + this_player()->query_real_name());
    if (messages == "")
	arr_messages = 0;
    else
	arr_messages = explode(messages, "\n**\n");
}

void get_subject(string str) {
    if (str && str != "")
	new_subject = "Subj: " + str + "\n";
    if (new_subject == 0) {
	write("No subject.\n");
	if (is_reading)
	    loop();
	return;
    }
    write("Give message.  Finish message with '**', or '~q' to cancel\n");
    write("]");
    input_to("more_mail");
    return;
}

void more_mail(string str) {
    if (str == "~q") {
	write("Aborted.\n");
	if (is_reading)
	    loop();
	return;
    }
    if (str == "**") {
	write("Cc:");
	input_to("get_cc");
	return;
    }
    new_message += str + "\n";
    write("]");
    input_to("more_mail");
}

/*
 * Send a complete mail. If the receiver is reading mail, be sure to
 * tell his mail reader that new mail has arrived.
 */
void notify_new_mail(string dest, string subj, string name) {
    object ob;
    if (ob = find_player(dest)) {
	tell_object(ob,"You have new mail from " +
		    name + ", " + subj + "\n");
	ob = present("mailread", ob);
	if (ob)
	    ob->invalidate();
    }
    write("Sending mail to " + capitalize(dest) + ".\n");
}

void send_mail(string dest, string subj, string mess, string cc) {
    if (cc != "")
	cc = "Cc: " + cc + "\n";
    notify_new_mail(dest, subj, this_player()->query_name());
    messages = "";
    restore_object(NAME + dest);
    if (messages == 0)
	messages = "";
    messages += this_player()->query_name() + "\n**\n" + subj + cc +
	"Date: " + ctime(time())[4..9] + "\n\n" + mess + "\n**\n";
    new_mail = 1;
    save_object(NAME + dest);
}

void get_cc(string cc) {
    if (cc != "")
	new_cc = cc;
    send_mail(new_dest, new_subject + "\n", new_message, new_cc);
    if (new_cc != "")
	send_mail(new_cc, new_subject + "\n", new_message, new_dest);
    if (is_reading)
	loop();
}

int help_msg() {
    write("x	Exit from mail reading mode.\n");
    write("d	Delete current message.\n");
    write("d <num>	Delete message number 'num'.\n");
    write("?	This help message.\n");
    write("r	Reply to current message.\n");
    write("r <num>	Reply to message number 'num'.\n");
    write("m <name> Mail a message to player 'name'.\n");
    write("h	Print all headers.\n");
    write("p	Print previous message.\n");
    write("n	Print next message.\n");
    write("l	Append current message to /log/name.mbox\n");
    return 1;
}

void get_cmd(string str) {
    int n, new_has_arrived;
    string tmp;

    /*
     * First, test all nondestructive commands. The destructive commands
     * will be tested after check if new mail has arrived.
     */
    if (!loaded) {
	load_player();
	new_has_arrived = 1;
    }
    if (str == "r") { reply_to_message(); return; }
    if (sscanf(str, "r %d", curr_mess) == 1) { reply_to_message(); return; }
    if (str == "h") { headers(); loop(); return; }
    if (str == "n") { curr_mess++; print_message(); loop(); return; }
    if (str == ".") { print_message(); loop(); return; }
    if (str == "l") { save_message(); loop(); return; }
    if (sscanf(str, "m %s", tmp) == 1) { mail_to(tmp); return; }
    if (str == "?") { help_msg(); loop(); return; }
    if (str == "p" || str == "-") {
	curr_mess--; print_message(); loop(); return;
    }
    if (sscanf(str, "%d", n) == 1) {
	curr_mess = n; print_message(); loop(); return;
    }
    if (new_has_arrived) {
	write("New mail has arrived. Command ignored\n");
	loop();
	return;
    }
    if (str == "x") { is_reading = 0; return; }
    if (str == "d") { delete(); loop(); return; }
    if (sscanf(str, "d %d", curr_mess) == 1) { delete(); loop(); return; }
    loop();
}

void invalidate() {
    loaded = 0;
}

int drop() {
    return 1;
}
