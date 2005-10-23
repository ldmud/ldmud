#include "room.h"

object stethoscope;
object frog;

void extra_reset() {
    if (!stethoscope || environment(stethoscope) != this_object()) {
	stethoscope = clone_object("obj/stethoscope");
	move_object(stethoscope, this_object());
    }
    if (!frog || !living(frog)) {
	object crown;
	frog = clone_object("obj/monster");
	frog->set_name("frog");
	frog->set_short("A cute little frog");
	frog->set_wc(4);
	frog->set_level(1);
	frog->set_frog(1);
	move_object(frog, this_object());
	crown = clone_object("obj/treasure");
	crown->set_id("crown");
	crown->set_value(30);
	crown->set_short("An iron crown");
	move_object(crown, frog);
    }
}

#undef EXTRA_RESET
#define EXTRA_RESET\
    extra_reset();

ONE_EXIT("room/ruin", "west",
	 "A large open plain",
	 "A large open plain. There is a river to the east,\nand some kind of building to the west\n", 1)
