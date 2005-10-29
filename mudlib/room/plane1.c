#include "std.h"
object wolf;

#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!wolf || !living(wolf)) {\
	wolf = clone_object("obj/monster");\
	call_other(wolf, "set_name", "wolf");\
	call_other(wolf, "set_level", 3);\
	call_other(wolf, "set_short", "A wolf");\
	call_other(wolf, "set_long", "A grey wolf running around. It has\n" +\
		   "some big dangerous teeth.\n");\
	call_other(wolf, "set_wc", 7);\
	call_other(wolf, "set_move_at_reset");\
	call_other(wolf, "set_whimpy");\
	move_object(wolf, "room/ruin");\
    }

TWO_EXIT("room/clearing", "south",
	 "room/plane2", "north",
	 "A large open plain",
	 "A large open plain, extending to north and south.\n", 1)
