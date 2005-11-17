#include "room.h"

#undef EXTRA_RESET
#define EXTRA_RESET\
        no_castle_flag = 1;\
        if (!present("stick")) {\
            object stick;\
	    stick = clone_object("obj/torch");\
	    move_object(stick, "room/hump");\
	    stick->set_name("stick");\
	    stick->set_fuel(500);\
	    stick->set_weight(1);\
        }\
        if (!present("money")) {\
	    object money;\
            money = clone_object("obj/money");\
	    move_object(money, "room/hump");\
	    money->set_money(10);\
        }

TWO_EXIT("room/vill_green", "east",
	 "room/wild1", "west",
	 "Humpbacked bridge",
	 "An old humpbacked bridge.\n", 1)
