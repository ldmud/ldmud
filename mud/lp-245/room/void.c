string short() {
    return "The void";
}

void long() {
    write(short() + ".\n");
    write("You come to the void if you fall out of a room, and have nowhere to go.\n");
    write("Give the command 'church', and you will come back to village church.\n");
    write("\nYou are transfered to the church...\n");
    this_player()->move_player("X#room/church");
}

void init() {
    add_action("church", "church");
}

int church() {
    this_player()->move_player("away#room/church");
    return 1;
}

void reset(int arg)
{
    if (arg)
	return;
    set_light(1);
}

int id(string str) { return str == "void"; }
