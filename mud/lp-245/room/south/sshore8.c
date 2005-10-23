void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("northeast", "northeast");
    add_action("south", "south");
    add_action("east", "east");
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
	  "Trails lead into the forest to the south and east.\n" +
	  "The shore of Crescent Lake continues northeast and southwest\n");
}

int northeast()
{
    this_player()->move_player("northeast#room/south/sshore7");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sforst20");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sforst19");
    return 1;
}

int southwest()
{
    this_player()->move_player("southwest#room/south/sshore9");
    return 1;
}
