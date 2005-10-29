#include "std.h"

#undef EXTRA_RESET
#define EXTRA_RESET\
        if (!present("stick")) {\
            object stick;\
	    stick = clone_object("obj/torch");\
	    move_object(stick, "room/hump");\
	    call_other(stick, "set_name", "stick");\
	    call_other(stick, "set_fuel", 500);\
	    call_other(stick, "set_weight", 1);\
        }\
        if (!present("money")) {\
	    object money;\
            money = clone_object("obj/money");\
	    move_object(money, "room/hump");\
	    call_other(money, "set_money", 10);\
        }

TWO_EXIT("room/vill_green", "east",
	 "room/wild1", "west",
	 "Humpbacked bridge",
	 "An old humpbacked bridge.\n", 1)
