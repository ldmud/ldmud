#include "std.h"
#undef EXTRA_LONG
#define EXTRA_LONG\
    if (str == "lever") {\
	write("The lever can be moved between two positions.\n");\
	return;\
    }\
    if (str == "door") {\
	if (call_other("room/sub/door_trap", "query_west_door"))\
	    write("The door is closed.\n");\
	else\
	    write("The door is open\n");\
	return;\
    }
#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("west"); add_verb("west");\
    add_action("open"); add_verb("open");\
    add_action("close"); add_verb("close");\
    add_action("pull"); add_verb("pull");\
    add_action("pull"); add_verb("turn");\
    add_action("pull"); add_verb("move");

TWO_EXIT("room/narr_alley", "up",
	 "room/maze1/maze1", "north",
	 "Down the well",
	 "You are down the well. It is wet and slippery.\n" +
	 "There is a lever beside a door to the west.\n", 0)

west() {
    if (call_other("room/sub/door_trap", "query_west_door") == 0) {
	call_other(this_player(), "move_player", "west#room/sub/door_trap");
	return 1;
    }
    write("The door is closed.\n");
    return 1;
}

close(str) {
    if (!str && str != "door")
	return 0;
    write("You can't.\n");
    return 1;
}

open(str) {
    if (!str && str != "door")
	return 0;
    write("You can't.\n");
    return 1;
}

pull(str) {
    if (!str || str != "lever")
	return 0;
    call_other("room/sub/door_trap", "toggle_door");
    return 1;
}

id(str) {
    return str == "lever" || str == "door";
}
