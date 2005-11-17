#include "../std.h"

object dwarf;

#undef EXTRA_MOVE1
#define EXTRA_MOVE1\
    if (dwarf && present(dwarf)) {\
	write("The dwarf bars the way !\n");\
	return 1;\
    }
#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!dwarf || !living(dwarf)) {\
	dwarf = clone_object("obj/monster");\
	dwarf->set_name("dwarf");\
	dwarf->set_level(10);\
	dwarf->set_al(-100);\
	dwarf->set_short("A short and sturdy dwarf");\
	dwarf->set_wc(10);\
	dwarf->set_aci(1);\
	move_object(dwarf, this_object());\
    }
TWO_EXIT("room/mine/tunnel17", "north",
	 "room/mine/tunnel15", "west",
	 "Tunnel",
	 "In the tunnel into the mines.\n", 0)
