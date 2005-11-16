#include "room.h"

#undef EXTRA_RESET
#define EXTRA_RESET no_castle_flag=1;
TWO_EXIT("room/vill_shore","west",
         "room/vill_shore2","east",
         "Road",
"You are on a road going out of the village. To the east the road widens out\n"+
"as it leads down to the shore. To the west lies the city.\n",
1)
