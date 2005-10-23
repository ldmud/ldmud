void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("west", "west");
    add_action("east", "east");
    add_action("southeast", "southeast");
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
	  "The shore of Crescent Lake continues southeast and west\n");
}

int east()
{
    this_player()->move_player("east#room/south/sforst49");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sshore23");
    return 1;
}

int southeast()
{
    this_player()->move_player("southeast#room/south/sshore25");
    return 1;
}
