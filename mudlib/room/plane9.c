#include "std.h"

object stethoscope;
object frog;

extra_reset() {
    if (!stethoscope || environment(stethoscope) != this_object()) {
	stethoscope = clone_object("obj/stethoscope");
	move_object(stethoscope, this_object());
    }
    if (!frog || !living(frog)) {
	object crown;
	frog = clone_object("obj/monster");
	call_other(frog, "set_name", "frog");
	call_other(frog, "set_short", "A cute little frog");
	call_other(frog, "set_wc", 4);
	call_other(frog, "set_level", 1);
	call_other(frog, "set_frog", 1);
	move_object(frog, this_object());
	crown = clone_object("obj/treasure");
	call_other(crown, "set_id", "crown");
	call_other(crown, "set_value", 30);
	call_other(crown, "set_short", "An iron crown");
	move_object(crown, frog);
    }
}

#undef EXTRA_RESET
#define EXTRA_RESET\
    extra_reset();

ONE_EXIT("room/ruin", "west",
	 "A large open plain",
	 "A large open plain. A river blocks passage to the east,\nand some kind of building lies to the west\n", 1)
