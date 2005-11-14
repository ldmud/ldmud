#include "room.h"

#undef EXTRA_RESET
#define EXTRA_RESET no_castle_flag = 1;

TWO_EXIT("room/hump", "east",
	 "room/forest1", "west",
	 "Wilderness",
	 "You are in the wilderness outside the village.\n" +
	 "There is a big forest to the west.\n", 1)
