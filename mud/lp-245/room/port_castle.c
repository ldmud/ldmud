/*
 * This "room" is a special object which can be dropped once.
 */
int dropped;
int grow_stage;
string owner;

void set_owner(string n) {
    owner = n;
}

void reset(int arg) {
    if (arg)
	return;
    dropped = 0;
    grow_stage = 5;
}

string short() {
    return "portable castle";
}

void long() {
    write(short() + ".\n");
}

void heart_beat() {
    if (!dropped)
	return;
    if (grow_stage > 0) {
	say("The castle grows...\n");
	grow_stage -= 1;
	return;
    }
    if (grow_stage == 0) {
	string name;
	say("The portable castle has grown into a full castle !\n");
	shout("Something in the world has changed.\n");
	name = create_wizard(lower_case(owner), /* domain: */ 0);
	if (name)
	    move_object(name, environment());
	destruct(this_object());
	return;
    }
}

int id(string str) {
    return str == "castle";
}

int drop() {
    if (environment(this_player())->query_drop_castle()) {
	write("Not this close to the city!\n");
	return 1;
    }
    dropped = 1;
    shout("There is a mighty crash, and thunder.\n");
    set_heart_beat(1);
    return 0;
}

int get() {
    if (dropped) {
	write("You can't take it anymore !\n");
	return 0;
    }
    return 1;
}
