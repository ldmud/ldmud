#include "room.h"

object troll;

#undef EXTRA_RESET
#define EXTRA_RESET\
    extra_reset();

void extra_reset() {
    object money;
    if (!troll || !living(troll)) {
	troll = clone_object("obj/monster");
	troll->set_name("troll");
	troll->set_level(9);
	troll->set_hp(100);
	troll->set_wc(12);
	troll->set_al(-60);
	troll->set_short("A troll");
	troll->set_long(
		   "It is a nasty troll that look very aggressive.\n");
	troll->set_aggressive(1);
	troll->set_spell_mess1(
		   "The troll says: Mumble");
	troll->set_spell_mess2(
		   "The troll says: Your mother was a bitch!");
	troll->set_chance(20);
	move_object(troll, this_object());
	money = clone_object("obj/money");
        money->set_money(random(500));
        move_object(money, troll);
    }
}

TWO_EXIT("room/clearing", "east",
	 "room/slope", "west",
        "In a forest",
        "You are in a big forest.\n", 1)
