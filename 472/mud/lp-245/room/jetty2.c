#include "room.h"

/* no castle drop here... its a jetty, how can anything be placed north &
   south of here... there is nothing but water around, place it in sea */
#undef EXTRA_RESET
#define EXTRA_RESET\
    no_castle_flag=1;\
    if (!present("bag"))\
        move_object(clone_object("obj/bag"), this_object());

TWO_EXIT("room/vill_shore2", "west",
	 "room/sea", "east",
	 "Jetty",
"You are at a jetty. The waves rolls in from east.\nA small path leads back to west.\n", 1)
