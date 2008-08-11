#include "../std.h"

int rope;

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("go_up", "up");

#undef EXTRA_LONG
#define EXTRA_LONG\
    if ("room/mine/tunnel3"->query_rope())\
	write("There is a rope hanging down through the hole.\n");

TWO_EXIT("room/mine/tunnel10", "west",
	 "room/mine/tunnel14", "east",
	 "Hole in ceiling",
	 "There is a big hole in the ceiling.\n", 0)

int go_up()
{
    if (!"room/mine/tunnel3"->query_rope()) {
        write("You can't go stright up with some kind of support.\n");
	return 1;
    }
    this_player()->move_player("up#room/mine/tunnel8");
    return 1;
}

int tie(string str)
{
    if (str != "ring" && str != "rings")
        return 0;
    rope = 1;
    return 1;
}

int id(string str) {
    return str == "ring" || str == "rings";
}

int untie(string str) {
    rope = 0;
    return 1;
}
