#include "std.h"
#undef EXTRA_LONG
#define EXTRA_LONG\
    if (str == "lever") {\
	write("The lever can be moved between two positions.\n");\
	return;\
    }\
    if (str == "door") {\
	if ("room/sub/door_trap"->query_west_door())\
	    write("The door is closed.\n");\
	else\
	    write("The door is open\n");\
    }
#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("west", "west");\
    add_action("open", "open");\
    add_action("close", "close");\
    add_action("pull", "pull");\
    add_action("pull", "turn");\
    add_action("pull", "move");

TWO_EXIT("room/narr_alley", "up",
	 "room/maze1/maze1", "north",
	 "Down the well",
	 "You are down the well. It is wet and slippery.\n" +
	 "There is a lever beside a door to the west.\n", 0)

int west() {
    if ("room/sub/door_trap"->query_west_door() == 0) {
	this_player()->move_player("west#room/sub/door_trap");
	return 1;
    }
    write("The door is closed.\n");
    return 1;
}

int close(string str) {
    if (!str && str != "door")
	return 0;
    write("You can't.\n");
    return 1;
}

int open(string str) {
    if (!str && str != "door")
	return 0;
    write("You can't.\n");
    return 1;
}

int pull(string str) {
    if (!str || str != "lever")
	return 0;
    "room/sub/door_trap"->toggle_door();
    return 1;
}

int id(string str) {
    return str == "lever" || str == "door";
}
