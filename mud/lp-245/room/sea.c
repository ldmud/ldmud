#include "std.h"

#undef EXTRA_INIT
#define EXTRA_INIT add_action("dive", "dive");

string bag;

ONE_EXIT("room/jetty2","west",
	 "All at sea",
	 "You are swimming out at the sea.\n", 1)

int dive() {
    object ob;

    ob = first_inventory(this_player());
    while(ob) {
	if (ob->can_put_and_get()) {
	    this_player()->move_player("down#room/sea_bottom");
	    return 1;
	}
	ob = next_inventory(ob);
    }
    write("You can't breathe under water !\n");
    write("You should try to get some portable air supply!\n");
    return 1;
}
