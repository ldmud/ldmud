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
}

string short()
{
    return "The shore of Crescent Lake";
}

void long()
{
    write("You are standing on the shore of Crescent Lake, a beautiful and\n" +
	  "clear lake. Out in the centre of the lake stands the Isle\n" +
	  "of the Magi.\n" +
	  "A trail leads into the forest to the east.\n" +
	  "The shore of Crescent Lake continues north and south\n");
}

int north()
{
    this_player()->move_player("north#room/south/sshore3");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sshore5");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sforst12");
    return 1;
}
