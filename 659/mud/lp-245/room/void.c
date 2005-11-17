short() {
    return "The void";
}

long() {
    write(short() + ".\n");
    write("You come to the void if you fall out of a room, and have nowhere to go.\n");
    write("Give the command 'church', and you will come back to village church.\n");
    write("\nYou are transfered to the church...\n");
    this_player()->move_player("X#room/church");
}

init() {
    add_action("church", "church");
}

church() {
    this_player()->move_player("away#room/church");
    return 1;
}

reset(arg)
{
    if (arg)
	return;
    set_light(1);
}

id(str) { return str == "void"; }
