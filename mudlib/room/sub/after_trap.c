#include "../std.h"

object rat;

#undef EXTRA_RESET
#define EXTRA_RESET extra_reset();

#undef EXTRA_MOVE1
#define EXTRA_MOVE1\
    if (call_other("room/sub/door_trap", "query_west_door") == 0) {\
	write("The door is closed.\n");\
	return 1;\
    }
ONE_EXIT("room/sub/door_trap", "east",
	 "Black room",
	 "This is the black room.\n", 0)

extra_reset() {
    object black_stone;
    if (!rat || !living(rat)) {
	rat = clone_object("obj/monster");
	call_other(rat, "set_name", "rat");
	call_other(rat, "set_alias", "black rat");
	call_other(rat, "set_level", 3);
	call_other(rat, "set_short", "An ugly black rat");
	call_other(rat, "set_wc", 5);
	call_other(rat, "set_agressive", 1);
	move_object(rat, this_object());
	black_stone = clone_object("obj/treasure");
	call_other(black_stone, "set_id", "stone");
	call_other(black_stone, "set_alias", "black stone");
	call_other(black_stone, "set_short", "A black stone");
	call_other(black_stone, "set_value", 60);
	move_object(black_stone, rat);
    }
}
