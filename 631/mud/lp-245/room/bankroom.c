#include "std.h"

reset(arg) {
    if (!arg) {
	set_light(1);
	move_object(clone_object("obj/safe"), this_object());
    }
}

long(str) {
    if (str == "door") {
	if ("room/bank"->query_door())
	    write("The door is closed.\n");
	else
	    write("The door is open.\n");
	return;
    }
    write("You are in the backroom of the bank.\n");
}

short() {
    return "backroom of bank";
}

init() {
    add_action("west", "west");
    add_action("open", "open");
}

west() {
    if ("room/bank"->query_door()) {
	write("The door is closed.\n");
	return 1;
    }
    this_player()->move_player("west#room/bank");
    return 1;
}

open(str) {
    if (!str) return 0;
    if (!"room/bank"->query_door())
	return 0;
    "room/bank"->open_door_inside();
    say(this_player()->query_name() +
	" opens the door.\n");
    write("Ok.\n");
    return 1;
}
