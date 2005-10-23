void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("west", "west");
    add_action("northeast", "northeast");
    add_action("southwest", "southwest");
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
	  "Trails lead into the forest to the north and west.\n" +
	  "The shore of Crescent Lake continues northeast and southwest\n");
}

int north()
{
    this_player()->move_player("north#room/south/sforst47");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sforst46");
    return 1;
}

int northeast()
{
    this_player()->move_player("northeast#room/south/sshore28");
    return 1;
}

int southwest()
{
    this_player()->move_player("southwest#room/south/sshore26");
    return 1;
}
