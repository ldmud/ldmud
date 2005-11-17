#include "room.h"

#undef EXTRA_RESET
#define EXTRA_RESET no_castle_flag = 1;
FOUR_EXIT("room/vill_track","west",
	   "room/yard","north",
	   "room/narr_alley","south",
	   "room/vill_road2","east",
	   "Village road",
"A long road going east through the village. The road narrows to a\n" +
"track to the west. There is an alley to the north and the south.\n", 1)
