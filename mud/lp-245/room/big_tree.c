#include "std.h"

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("climb", "climb");

#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!present("rope"))\
	move_object(clone_object("obj/rope"), this_object());

TWO_EXIT("room/plane7", "east",
	 "room/giant_path", "west",
	 "Big tree",
	 "A big single tree on the plain.\n", 1)

int id(string str) {
    if (str == "tree" || str == "big tree")
	return 1;
    return 0;
}

int tie(string str) {
    if (str == "tree" || str == "big tree") {
	write("The branches are very high up.\n");
	return 0;
    }
    return 0;
}

int climb(string str)
{
    if (!id(str))
	return 0;
    write("There are no low branches.\n");
    return 1;
}
