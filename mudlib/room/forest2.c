#include "std.h"

object troll;

#undef EXTRA_RESET
#define EXTRA_RESET\
    extra_reset();

extra_reset() {
    object money;
    if (!troll || !living(troll)) {
	troll = clone_object("obj/monster");
	call_other(troll, "set_name", "troll");
	call_other(troll, "set_level", 9);
	call_other(troll, "set_hp", 100);
	call_other(troll, "set_wc", 12);
	call_other(troll, "set_al", -60);
	call_other(troll, "set_short", "A troll");
	call_other(troll, "set_long",
		   "It is a nasty troll that looks very aggressive.\n");
	call_other(troll, "set_aggressive", 1);
	call_other(troll, "set_spell_mess1",
		   "The troll says: Mumble");
	call_other(troll, "set_spell_mess2",
		   "The troll says: Your mother was a bitch!");
	call_other(troll, "set_chance", 20);
	move_object(troll, this_object());
	money = clone_object("obj/money");
        call_other(money, "set_money", random(500));
        move_object(money, troll);
    }
}

TWO_EXIT("room/clearing", "east",
	 "room/slope", "west",
        "In a forest",
        "You are in a big forest.\n", 1)
