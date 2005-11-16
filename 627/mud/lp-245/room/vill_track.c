#include "room.h"

#undef EXTRA_RESET
#define EXTRA_RESET no_castle_flag = 1;

TWO_EXIT("room/vill_green","west",
	 "room/vill_road1","east",
	 "Village track",
"A track going into the village. The track opens up to a road to the east\n" +
"and ends with a green lawn to the west.\n", 1)
