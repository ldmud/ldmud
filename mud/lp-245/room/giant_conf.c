#include "room.h"

object giant1, giant2, giant3;

void extra_reset() {
    object weapon;
    if (!giant2 || !living(giant2)) {
	giant2 = clone_object("obj/monster");
	giant2->set_name("giant");
	giant2->set_alias("frostgiant");
	giant2->set_level(15);
	giant2->set_short("A frost giant");
	giant2->set_wc(20);
	giant2->set_ac(2);
	giant2->set_al(-150);
	giant2->set_aggressive(1);
	move_object(giant2, this_object());
	weapon = clone_object("obj/weapon");
	weapon->set_name("sword");
	weapon->set_alias("sword of frost");
	weapon->set_short("sword of frost");
	weapon->set_class(20);
	weapon->set_weight(3);
	weapon->set_value(2000);
	move_object(weapon, giant2);
    }
    if (!giant3 || !living(giant3)) {
	giant3 = clone_object("obj/monster");
	giant3->set_name("giant");
	giant3->set_alias("stonegiant");
	giant3->set_level(15);
	giant3->set_short("A stone giant");
	giant3->set_wc(20);
	giant3->set_ac(2);
	giant3->set_al(-150);
	giant3->set_aggressive(1);
	move_object(giant3, this_object());
	weapon = clone_object("obj/weapon");
	weapon->set_name("sword");
	weapon->set_alias("stone cutter sword");
	weapon->set_short("stone cutter sword");
	weapon->set_class(20);
	weapon->set_weight(3);
	weapon->set_value(2000);
	move_object(weapon, giant3);
    }
    if (!giant1 || !living(giant1)) {
	giant1 = clone_object("obj/monster");
	giant1->set_name("giant");
	giant1->set_alias("firegiant");
	giant1->set_level(15);
	giant1->set_short("A fire giant");
	giant1->set_wc(20);
	giant1->set_ac(2);
	giant1->set_al(-150);
	giant1->set_aggressive(1);
	move_object(giant1, this_object());
	weapon = clone_object("obj/weapon");
	weapon->set_name("sword");
	weapon->set_alias("sword of fire");
	weapon->set_short("sword of fire");
	weapon->set_class(20);
	weapon->set_weight(3);
	weapon->set_value(2000);
	move_object(weapon, giant1);
    }
}

#undef EXTRA_RESET
#define EXTRA_RESET\
    extra_reset();

ONE_EXIT("room/giant_lair", "east",
	 "Giants conference of human bashing",
	 "You are at the yearly conference of human bashing,\n" +
	 "organized by the giants.\n", 1)
