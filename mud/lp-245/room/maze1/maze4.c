int exit_num;

string short() {
    return "A maze";
}

void long() {
    write("In a maze.\n");
    write("There are four obvious exits: north, south, west and east.\n");
}

void reset() {
    exit_num = random(4);	/* "grin" */
}

void init() {
    add_action("e0", "north");
    add_action("e1", "south");
    add_action("e2", "east");
    add_action("e3", "west");
}

int e0() {
    if (exit_num == 0)
	this_player()->move_player("north#room/maze1/maze5");
    else
	this_player()->move_player("north#room/maze1/maze3");
    return 1;
}

int e1() {
    if (exit_num == 1)
	this_player()->move_player("south#room/maze1/maze5");
    else
	this_player()->move_player("south#room/maze1/maze2");
    return 1;
}

int e2() {
    if (exit_num == 2)
	this_player()->move_player("east#room/maze1/maze5");
    else
	this_player()->move_player("east#room/well");
    return 1;
}

int e3() {
    if (exit_num == 3)
	this_player()->move_player("west#room/maze1/maze5");
    else
	this_player()->move_player("west#room/well");
    return 1;
}
