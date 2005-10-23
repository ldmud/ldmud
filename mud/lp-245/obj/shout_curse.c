/*
 * This is a curse that the player can't get rid of.
 * It prevents you from shouting.
 */

int start_time;

string query_auto_load() {
    return "obj/shout_curse:" + start_time;
}

void start(object ob) {
    move_object(this_object(), ob);
    start_time = time();
    tell_object(ob, "You get a sore throat suddenly, without any reason.\n");
}

int id(string str) {
    return str == "shout_curse";
}

void long() {
    write("How can you look at a curse ?\n");
}

int drop() { return 1; }

void init() {
    add_action("do_shout", "shout");
}

int do_shout() {
    if (time() < start_time + 3600) {
	write("You can't shout with a sore throat !\n");
	say(this_player()->query_name() +
	    " makes croaking sounds.\n");
	return 1;
    } else {
	destruct(this_object());
	return 0;
    }
}

void init_arg(string str) {
    sscanf(str, "%d", start_time);
}

string extra_look() {
    return "the throat seems to be sore";
}
