int door_open;

void long() {
    write("This is room 2. There is an exit to the west.\n");
}

int go_west() {
    if (!door_open)
	write("The door is closed.\n");
    if (door_open)
    {
	this_player()->move_object("west#room/test");
        return 1;
    }
    return 0;
}

void init() {
    add_action("go_west", "west");
    door_open = "room/test"->door_open();
    write("You come into room 2.\n");
    if (door_open)
	write("There is an open door to the west.\n");
}
