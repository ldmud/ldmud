/*
 * This is a curse that the player can't get rid of.
 * It prevents you from shouting.
 */

int start_time;

query_auto_load() {
    return "obj/shout_curse:" + start_time;
}

start(ob) {
    move_object(this_object(), ob);
    start_time = time();
    tell_object(ob, "You get a sore throat suddenly, without any reason.\n");
}

id(str) {
    return str == "shout_curse";
}

long() {
    write("How can you look at a curse ?\n");
}

drop() { return 1; }

init() {
    add_action("do_shout", "shout");
}

do_shout() {
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

init_arg(str) {
    sscanf(str, "%d", start_time);
}

extra_look() {
    return "the throat seems to be sore";
}
