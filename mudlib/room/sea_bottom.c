#include "std.h"

object octopus;

#undef EXTRA_RESET
#define EXTRA_RESET extra_reset();

extra_reset() {
    if (!octopus || !living(octopus)) {
	object chest;
	object money;
	octopus = clone_object("obj/monster");
	call_other(octopus, "set_name", "octopus");
	call_other(octopus, "set_level", 9);
	call_other(octopus, "set_hp", 100);
	call_other(octopus, "set_wc", 12);
	call_other(octopus, "set_al", -20);
	call_other(octopus, "set_short", "An octopus");
	call_other(octopus, "set_long",
		   "A very big octopus with long arms, reaching for you.\n");
	call_other(octopus, "set_spell_mess1",
		   "The octopus says: Mumble");
	call_other(octopus, "set_spell_mess2",
		   "The octopus says: I will convert you to a pulp!");
	call_other(octopus, "set_chance", 20);
	move_object(octopus, this_object());
	chest = clone_object("obj/chest");
	move_object(chest, octopus);
        money = clone_object("obj/money");
        call_other(money, "set_money", random(500));
        move_object(money, chest);
    }
}

ONE_EXIT("room/sea", "up",
	 "Sea bottom",
	 "You are at the bottom of the sea.\n", 1)
