/*
 * This "room" is a special object which can be dropped once.
 */
int dropped;
int grow_stage;
string owner;

init() {
    add_action("quit"); add_verb("quit");
}

quit() {
    write("You cannot quit until you place your portable castle!\n");
    return 1;
}

set_owner(n) {
    owner = n;
}

reset(arg) {
    if (arg)
	return;
    dropped = 0;
    grow_stage = 5;
}

short() {
    return (owner + "'s portable castle");
}

long() {
    write(short() + ".\n");
}

get() {
    write("Once dropped, can not be moved again.\n");
    return 0;
}

heart_beat() {
    if (!dropped)
	return;
    if (grow_stage > 0) {
	say("The castle grows...\n");
	grow_stage -= 1;
	return;
    }
    if (grow_stage == 0) {
	string name;
	say("The portable castle has grown into a full castle!\n");
	shout("Something in the world has changed.\n");
	name = create_wizard(lower_case(owner));
	if (name)
	    move_object(name, environment());
	else
	    say("Castle creation failed.");
	destruct(this_object());
	return;
    }
}

id(str) {
    return str == "castle";
}

drop() {
    dropped = 1;
    shout("There is a mighty crash, and thunder.\n");
    set_heart_beat(1);
    return 0;
}
