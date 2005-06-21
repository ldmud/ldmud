inherit "room/room";

void reset(int arg) {
    if (arg) return;

    set_light(1);
    short_desc = "Village green";
    no_castle_flag = 1;
    long_desc =
	"You are at an open green place south of the village church.\n" +
	    "You can see a road further to the east.\n";
    dest_dir = ({"room/church", "north",
		 "room/hump", "west",
		 "room/vill_track", "east"});
}

