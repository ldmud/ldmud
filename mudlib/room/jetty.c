#include "std.h"

#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!present("bag"))\
        move_object(clone_object("obj/bag"), this_object());
TWO_EXIT("room/vill_shore", "west",
	 "room/sea", "east",
	 "Jetty",
	 "You are at a jetty. The waves rolls in from east.\nA small path leads back to west.\n", 1)
