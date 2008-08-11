#include "std.h"

int id(string str)
{
    if (str == "ruin")
	return 1;
    else
	return 0;
}

FOUR_EXIT("room/plane4", "south",
	  "room/plane8", "north",
	  "room/plane9", "east",
	  "room/plane3", "west",
	  "Ruin",
	  "A very old looking ruin. There is no roof, and no door.\n",
	  1)
