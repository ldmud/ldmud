int door_open;

long() {
    write("This is room 2. There is an exit to the west.\n");
}

go_west() {
    if (!door_open)
	write("The door is closed.\n");
    if (door_open)
	this_player()->move_object("west#room/test");
}

init() {
    add_action("go_west", "west");
    door_open = "room/test"->door_open();
    write("You come into room 2.\n");
    if (door_open)
	write("There is an open door to the west.\n");
}
