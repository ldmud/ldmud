#include "../room.h"
#undef EXTRA_RESET

object dragon;

#define EXTRA_RESET\
    if (!dragon || !living(dragon)) {\
	object treas;\
	dragon = clone_object("obj/monster");\
	dragon->set_name("dragon");\
	dragon->set_level(17);\
	dragon->set_al(-900);\
	dragon->set_shorti("The cave dragon");\
	dragon->set_wc(25);\
	dragon->set_ac(4);\
	treas = clone_object("obj/treasure");\
	treas->set_id("sapphire");\
	treas->set_alias("stone");\
	treas->set_short("A sapphire");\
	treas->set_value(250);\
	move_object(treas, dragon);\
	treas = clone_object("obj/treasure");\
	treas->set_id("diamond");\
	treas->set_alias("stone");\
	treas->set_short("A diamond");\
	treas->set_value(250);\
	move_object(treas, dragon);\
	move_object(dragon, this_object());\
    }
ONE_EXIT("room/mine/tunnel28", "west",
	 "Dead end",
	 "Dead end.\n", 0)
