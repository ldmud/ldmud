#include "../std.h"

object rat;

#undef EXTRA_RESET
#define EXTRA_RESET extra_reset();

#undef EXTRA_MOVE1
#define EXTRA_MOVE1\
    if ("room/sub/door_trap"->query_west_door() == 0) {\
	write("The door is closed.\n");\
	return 1;\
    }

void extra_reset() {
    object black_stone;
    if (!rat || !living(rat)) {
	rat = clone_object("obj/monster");
	rat->set_name("rat");
	rat->set_alias("black rat");
	rat->set_level(3);
	rat->set_short("An ugly black rat");
	rat->set_wc(5);
	rat->set_agressive(1);
	move_object(rat, this_object());
	black_stone = clone_object("obj/treasure");
	black_stone->set_id("stone");
	black_stone->set_alias("black stone");
	black_stone->set_short("A black stone");
	black_stone->set_value(60);
	move_object(black_stone, rat);
    }
}

ONE_EXIT("room/sub/door_trap", "east",
	 "Black room",
	 "This is the black room.\n", 0)
