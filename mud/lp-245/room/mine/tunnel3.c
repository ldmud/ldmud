#include "../std.h"

string rope;

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("down", "down");\
    add_action("down", "climb");

#undef EXTRA_LONG
#define EXTRA_LONG\
    if (str == "ring" || str == "rings") {\
	write("A sturdy iron ring, fastened to the wall.\n");\
	return;\
    }

TWO_EXIT("room/mine/tunnel2", "south",
	 "room/mine/tunnel4", "north",
	 "Hole",
	 "There is a big hole here, and some kind of iron rings in the wall.\n" +
	 "It is should be possible to pass the hole.\n", 0)

down() {
    if (!rope) {
        write("You would fall down the hole and possible hurt yourself.\n");
	return 1;
    }
    this_player()->move_player("down#room/mine/tunnel8");
    return 1;
}

tie(str)
{
    if (str != "ring" && str != "rings")
        return 0;
    rope = 1;
    return 1;
}

id(str) {
    return str == "ring" || str == "rings";
}

untie(str) {
    rope = 0;
    return 1;
}

query_rope() {
    return rope;
}
