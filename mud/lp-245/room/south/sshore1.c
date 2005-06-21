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
    add_action("northwest", "northwest");
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
	  "Trails lead into the forest to the north and east.\n" +
	  "The shore of Crescent Lake continues south and northwest\n");
}

int north()
{
    this_player()->move_player("north#room/south/sforst8");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sshore2");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sforst9");
    return 1;
}

int northwest()
{
    this_player()->move_player("northwest#room/south/sshore30");
    return 1;
}
