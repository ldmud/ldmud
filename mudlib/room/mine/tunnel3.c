#include "../std.h"

string rope;

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("down"); add_verb("down");\
    add_action("down"); add_verb("climb");

#undef EXTRA_LONG
#define EXTRA_LONG\
    if (str == "ring" || str == "rings") {\
	write("A sturdy iron ring, fastened to the wall.\n");\
	return;\
    } else\
    if (str == "hole") {\
	write("You can barely make out the floor about 10 meters down.\n");\
	write("The walls of the hole are very smooth and offer no handholds.\n");\
	return;\
    }

TWO_EXIT("room/mine/tunnel2", "south",
	 "room/mine/tunnel4", "north",
	 "Hole",
	 "There is a big hole here, and some kind of iron ring in the wall.\n" +
	 "It should be possible to walk around the hole.\n", 0)

down() {
    if (!rope) {
        write("You would fall down the hole and probably hurt yourself.\n");
	return 1;
    }
    call_other(this_player(), "move_player", "down#room/mine/tunnel8");
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
    return str == "ring" || str == "rings" || str == "hole";
}

untie(str) {
    rope = 0;
    return 1;
}

query_rope() {
    return rope;
}
