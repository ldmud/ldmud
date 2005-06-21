#include "../std.h"

int west_door_open;

#undef EXTRA_RESET
#define EXTRA_RESET\
    west_door_open = 1;

#undef EXTRA_MOVE1
#define EXTRA_MOVE1\
    if (west_door_open == 1) {\
	write("The door is closed.\n");\
	return 1;\
    }

#undef EXTRA_MOVE2
#define EXTRA_MOVE2\
    if (west_door_open == 0) {\
	write("The door is closed.\n");\
	return 1;\
    }

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("open", "close");\
    add_action("close", "close");

TWO_EXIT("room/well", "east",
	 "room/sub/after_trap", "west",
	 "Room with black walls",
	 "A room with black walls. There is a door to the east,\n" +
	 "and a door to the west.\n", 0)

int open(string str) {
    if (str != "door" && str != "west door" && str != "east door")
	return 0;
    write("There is no handle, and you can't push it up.\n");
    return 1;
}

int close(string str) {
    if (str != "door" && str != "west door" && str != "east door")
	return 0;
    write("There is no handle, and you can't push it closed.\n");
    return 1;
}

void toggle_door() {
    write("You move the lever.\n");
    say(this_player()->query_name() + " pulled the lever.\n");
    if (west_door_open) {
	tell_room(this_object(), "The west door closed.\n" +
	    "The east door opened.\n");
	tell_room(environment(this_player()), "The west door opened.\n");
	west_door_open = 0;
    } else {
	tell_room(this_object(), "The west door opens.\n" +
	    "The east door closed.\n");
	tell_room(environment(this_player()), "The west door closed.\n");
	west_door_open = 1;
    }
}

int query_west_door() {
    return west_door_open;
}
