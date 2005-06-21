#include "room.h"
#undef EXTRA_RESET
#define EXTRA_RESET fix_jacket();

void fix_jacket()
{
    object leather_jacket;

    leather_jacket = present("leather jacket");
    if (!leather_jacket) {
	leather_jacket = clone_object("obj/armour");
	leather_jacket->set_name("leather jacket");
	leather_jacket->set_short("A leather jacket");
	leather_jacket->set_alias("jacket");
	leather_jacket->set_long("A worn but sturdy leather jacket.\n" +
	  "On the back is a lot of rivets making the pattern of a star.\n");
	leather_jacket->set_value(50);
	leather_jacket->set_weight(2);
	leather_jacket->set_ac(2);
	leather_jacket->set_type("armour");
	move_object(leather_jacket, this_object());
    }
}

TWO_EXIT("room/wild1", "east",
	 "room/clearing", "west",
	 "In a forest",
	 "You are in a big forest.\n", 1)
