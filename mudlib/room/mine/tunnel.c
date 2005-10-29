#include "../std.h"

#undef EXTRA_INIT
#define EXTRA_INIT\
	add_action("read"); add_verb("read");\
	add_action("read"); add_verb("look");

TWO_EXIT("room/mount_pass", "south",
	 "room/mine/tunnel2", "north",
	 "Mine entrance",
	 "This is the entrance to the mines.\nThere is a sign on a pole.\n", 1)

read(str) {
	if (str == "sign" || str == "at sign")
		{
		write("WARNING !!\n\n"+
		      "The mines are closed due to risk of falling rock.\n");
		return 1;
		}
	else	{
		return 0;
		}
}

id(str) {
    return str == "sign" || str == "pole";
}
