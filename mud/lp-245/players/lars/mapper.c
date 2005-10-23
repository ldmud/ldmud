string current_room;
int num_room;
string data;
/*
 * Data about current room;
 */
string data_this_room, room_complete, last_fail;

int id(string str) {
    return str == "mapper" || str == "robot mapper" || str == "robot";
}

string short() {
    return "A mapper robot";
}

void long() {
    write("A small and highly movable robot. It constantly looks in all\n");
    write("directions, storing all information.\n");
}

void reset(int arg) {
    if (arg)
	return;
    set_heart_beat(1);
    enable_commands();
    current_room = "start";
    data = "";
    data_this_room = "";
    room_complete = 0;
}

string get_dir(string dir) {
    string rest, tmp_dir, tmp_dest;

    rest = data_this_room;
    while(rest != "") {
	sscanf(rest, "%s-%s,%s", tmp_dir, tmp_dest, rest);
	if (tmp_dir == "!" + dir)
	    return "!";
	if (tmp_dir == dir)
	    return tmp_dest;
    }
    return "";
}

void mark_dir_complete(string dir) {
    string rest, tmp_dir, tmp_dest;

    rest = data_this_room;
    data_this_room = "";
    room_complete += 1;
    while(rest != "") {
	sscanf(rest, "%s-%s,%s", tmp_dir, tmp_dest, rest);
	if (tmp_dir == dir) {
	    data_this_room = data_this_room + "!" + tmp_dir + "-" +
		tmp_dest + "," + rest;
	    return;
	}
	data_this_room = data_this_room + tmp_dir + "-" +
	    tmp_dest + ",";
    }
    data_this_room = data_this_room + "!" + dir + "-" + ",";
}

/*
 * Extract current room out of data base.
 */
void get_this_room() {
    string rest, tmp;
    string scratch;

    rest = data;
    data = "";
    while(rest != "") {
	sscanf(rest, "%s\n%s", tmp, rest);
	if (sscanf(tmp, current_room + ":%s(%d)", data_this_room,
		   room_complete) == 2) {
	    data = data + rest;
	    return;
	}
	data = data + tmp + "\n";
    }
    data_this_room = "";
    num_room += 1;
    room_complete = 0;
}

void heart_beat() {
    int i;
    string cmd, scratch;
    object here;

    i = random(6);
    if (i == 0)
	cmd = "north";
    else if (i == 1)
	cmd = "west";
    else if (i == 2)
	cmd = "south";
    else if (i == 3)
	cmd = "east";
    else if (i == 4)
	cmd = "up";
    else if (i == 5)
	cmd = "down";
    scratch = get_dir(cmd);
    if (scratch[0] == '!') {
	last_fail = cmd;
	return;
    }
    last_fail = "";
    here = environment(this_object());
    command(cmd);
    if (here == environment(this_object()))
	mark_dir_complete(cmd);
}

void insert() {
    if (current_room == "start")
	return;
    data = data + current_room + ":" + data_this_room +
	"(" + room_complete + ")\n";
}

void move_player(string dir_dest)
{
    string dir, dest;
    object ob;
    int is_light;

    if (sscanf(dir_dest, "%s#%s", dir, dest) != 2)
	return;
    if (get_dir(dir) == "")
	data_this_room = data_this_room + dir + "-" + dest + ",";
    insert();
    log_file("mapper", current_room + ":\t" + dir + "\t" + dest + "\n");
    current_room = dest;
    if (dir == "X")
	say("The mapper robot falls through a singularity\n");
    else
	say("The mapper robot leaves " + dir + ".\n");
    move_object(this_object(), dest);
    is_light = set_light(0);
    if(is_light < 0)
	is_light = 0;
    say("The mapper robot arrives.\n");
    get_this_room();
}

string query_name() {
    return "Mapper robot";
}

void show_stats() {
    write("This is the mapper robot\n");
    write("Current room: " + current_room + "\n");
    write("Number of rooms visited: " + num_room + "\n");
    write("Current room complete: " + room_complete + "\n");
    write("Room data: " + data_this_room + "\n");
    write("Data:\n" + data + "\n");
    write("Last failed dir: " + last_fail + "\n");
}


void force_us(string str) {
    command(str);
}
