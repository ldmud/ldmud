void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("south", "south");
    add_action("east", "east");
    add_action("west", "west");
}

string short()
{
    return "Halfway up the hill on the Isle of the Magi";
}

void long()
{
    write("You are halfway up the hill.\n" +
	  "On top of the hill, to the south, stands the ruins of the tower of\n" +
	  "Arcanarton.\n" +
	  "The bridge to the mainland stands at the base of the hill to the north\n" +
	  "A path heads around the hill to the east, and down the hill to the shore\n" +
	  "of the island to the west.\n");
}

int north()
{
    this_player()->move_player("north#room/south/sislnd1");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sislnd18");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sislnd14");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sislnd12");
    return 1;
}
