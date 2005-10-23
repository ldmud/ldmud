/* Groo 90-12-02 */
/**********************************
 * The new, great bulletin board! *
 * (The manic wanderer - version) *
 * (with the nasty bug #1 fixed.) *
 **********************************/

#define ERROR_LOG "/log/BOARD"
#define BOARD_NAME "wiz_bull2"

string new_head, new_text, tmp_head, tmp_text;
int msg_num;

static string  *messages, *headers;
static int line, looked_at;
static object curr_writer;

int id(string str) {
	return str == "board" || str == "bulletin board" || str == "bulletinboard";
}

void long() {
	int ind;
	write("This is a bulletin board.\n");
	write("Usage : note <headline>, read/remove <message number>\n");
	write("        store <message number> <file name>\n");
	if (!msg_num) {
		write("The board is empty.\n");
		return;
	}
	write("The bulletin board contains " + msg_num);
	if (msg_num == 1)
		write(" note :\n\n");
	else
		write(" notes :\n\n");
	/*
	say(this_player()->query_name() + " studies the bulletin board.\n");
	*/
	ind = 0;
	while (ind < msg_num) {
		write(ind + 1 + ":\t" + headers[ind] + "\n");
		ind++;
	}
}

string short() {
	return "A bulletin board";
}

int get() {
	write("It is firmly secured to the ground.\n");
	return 0;
}

void init() {
	add_action("new_msg", "note");
	add_action("read_msg", "read");
	add_action("remove_msg", "remove");
	add_action("store_msg", "store");
	if (!looked_at) {
	        int i;
		string * arr;
		messages = allocate(30);
		headers = allocate(30);
		looked_at = 1;
		if (!restore_object(BOARD_NAME))
		    return;
		arr = explode(tmp_head, "\n**\n");
		i = 0;
		while(i < sizeof(arr)) {
		    headers[i] = arr[i];
		    i++;
		}
		arr = explode(tmp_text, "\n**\n");
		i = 0;
		while(i < sizeof(arr)) {
		    messages[i] = arr[i];
		    i++;
		}
		tmp_text = "";
		tmp_head = "";
	}
}

void reset(int arg) {
	if (arg)
		if (!random(5)) {
			say("A small gnome appears and secures some " +
			"notes on the board that were loose.\n");
			say("The gnome leaves again.\n");
		}
}

int save_board() {
	int ind;
	ind = 1;
	tmp_head = implode(headers, "\n**\n") + "\n**\n";
	tmp_text = implode(messages, "\n**\n") + "\n**\n";
	save_object(BOARD_NAME);
	tmp_head = "";
	tmp_text = "";
	return 1;
}


void error_log(string str) {
	tell_room(environment(this_object()), "Board says '" + str + "'.\n");
	log_file(ERROR_LOG, "Board : " + str);
	return;
}

int new_msg(string msg_head) {
	line = 1;
	if (!msg_head)
		return 0;
	if (curr_writer && environment(curr_writer) ==
		environment(this_object())) {
		write(this_player()->query_name() + " is busy writing.\n");
		return 1;
	}
	if (msg_num == 30) {
		write("You have to remove an old message first.\n");
		return 1;
	}
	if (strlen(msg_head) > 50) {
		write("Message header too long. Try again.\n");
		return 1;
	}
	curr_writer = this_player();
	say(curr_writer->query_name() + " starts writing a note.\n");
	new_head = msg_head;
	new_text = "";
	input_to("get_msg");
	write("Enter message text. End with '**', abort with '~q'.\n");
	write("1>>");
	return 1;
}

void get_msg(string str) {
	if (str == "~q") {
		say(curr_writer->query_name() + " aborts writing a note.\n");
		write("Note aborted.\n");
		curr_writer = 0;
		new_head = "";
		new_text = "";
		return;
	}
	if (str == "**") {
		if (line == 1) {
			write("No text entered. Message discarded.\n");
			say(curr_writer->query_name() + " quits writing.\n");
			curr_writer = 0;
			new_head = "";
			new_text = "";
			return;
		}
		say(curr_writer->query_name() + " has completed a note : " +
			new_head + "\n");
		headers[msg_num] = new_head + "(" + this_player()->query_name() +
			", " + ctime(time())[4..9] +
			", " + this_player()->query_level() + ")";
		messages[msg_num] = new_text + "\n";
		msg_num++;
		save_board();
		write("Ok.\n");
		curr_writer = 0;
		return;
	}
	new_text = new_text + str + "\n";
	line++;
	write(line + ">>");
	input_to("get_msg");
}

int read_msg(string what_msg) {
	int note;
	if (!sscanf(what_msg, "%d", note))
		if (!sscanf(what_msg, "note %d", note))
			return 0;
	if (note < 1 || note > msg_num) {
		write("Not that many messages.\n");
		return 1;
	}
	note -= 1;
	/*
	say(this_player()->query_name() + " reads a note titled '" +
		headers[note] + "'.\n");
	*/
	write("The note is titled '" + headers[note] + "':\n\n");
	write(messages[note]);
	return 1;
}

int remove_msg(string what_msg) {
	string player, title, date;
	int note, ind;
	if (!sscanf(what_msg, "%d", note))
		if (!sscanf(what_msg, "note %d", note))
			return 0;
	if (note < 1 || note > msg_num) {
		write("That message doesn't exist.\n");
		return 1;
	}
	note -= 1;
	if (sscanf(headers[note], "%s(%s,%s", title, player, date) != 3) {
		error_log("Header error");
		write("Board : error - header corrupt.\n");
		return 1;
	}
	if ((this_player()->query_name() != player &&
		this_player()->query_level() < 24) ||
		!query_ip_number(this_player())) {
		write("Only Archwizards may remove other wizard's notes.\n");
		say(this_player()->query_name() + " failed to remove " +
			"a note.\n");
		return 1;
	}
	say(this_player()->query_name() + " removes a note titled '" +
		headers[note] + "'.\n");
	messages[note] = 0;
	headers[note] = 0;
	ind = note;
	while (ind < msg_num - 1) {
		messages[ind] = messages[ind + 1];
		headers[ind] = headers[ind + 1];
		ind++;
	}
	msg_num -= 1;
	save_board();
	write("Ok.\n");
	return 1;
}
int store_msg(string str) {
	int note;
	string file;
	if (!str)
		return 0;
	if (sscanf(str, "%d %s", note, file) != 2)
		if (sscanf(str, "note %d %s", note, file) != 2)
			return 0;
	if (note < 1 || note > msg_num) {
		write("That message doesn't exist.\n");
		return 1;
	}
	note -= 1;
	file = file + ".note";
	write_file(file, headers[note] + "\n" + messages[note] + "\n");
	return 1;
}

