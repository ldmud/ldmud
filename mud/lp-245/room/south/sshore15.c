void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("northwest", "northwest");
    add_action("south", "south");
    add_action("west", "west");
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
	  "Trails lead into the forest to the south and west.\n" +
	  "The shore of Crescent Lake continues northwest and southeast\n");
}

int northwest()
{
    this_player()->move_player("northwest#room/south/sshore16");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sforst41");
    return 1;
}

int southeast()
{
    this_player()->move_player("southeast#room/south/sshore14");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sforst40");
    return 1;
}
