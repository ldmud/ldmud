#include "std.h"
#undef EXTRA_RESET
#define EXTRA_RESET fix_jacket();

TWO_EXIT("room/wild1", "east",
	 "room/clearing", "west",
	 "In a forest",
	 "You are in a big forest.\n", 1)

fix_jacket()
{
    object leather_jacket;

    leather_jacket = present("leather jacket");
    if (!leather_jacket) {
	leather_jacket = clone_object("obj/armor");
	call_other(leather_jacket, "set_name", "leather jacket");
	call_other(leather_jacket, "set_short", "A leather jacket");
	call_other(leather_jacket, "set_alias", "jacket");
	call_other(leather_jacket, "set_long", "A worn but sturdy leather jacket.\n" +
	  "On the back are a lot of rivets making the pattern of a star.\n");
	call_other(leather_jacket, "set_value", 50);
	call_other(leather_jacket, "set_weight", 2);
	call_other(leather_jacket, "set_ac", 1);
	call_other(leather_jacket, "set_type", "armor");
	move_object(leather_jacket, this_object());
    }
}
